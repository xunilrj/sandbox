#define UNICODE

#include <io.h>
#include <stdio.h>
#include <winsock2.h>
#include <Mswsock.h>

#include <iostream>
 
#pragma comment(lib,"ws2_32.lib")

const int MAX_RIO_THREAD = 1;
const int MAX_SEND_RQ_SIZE_PER_SOCKET = 32;
const int MAX_RECV_RQ_SIZE_PER_SOCKET = 32;
const int SESSION_BUFFER_SIZE = 65536;
const int MAX_CLIENT_PER_RIO_THREAD = 2560;
const int MAX_CQ_SIZE_PER_RIO_THREAD =
    (MAX_SEND_RQ_SIZE_PER_SOCKET + MAX_RECV_RQ_SIZE_PER_SOCKET) 
    * MAX_CLIENT_PER_RIO_THREAD;
const int MAX_RIO_RESULT = 1000;

RIO_EXTENSION_FUNCTION_TABLE mRioFunctionTable = { 0, };
RIO_CQ mRioCompletionQueue[MAX_RIO_THREAD+1] = { 0, };

struct IOCPLoopData {HANDLE iocp;};
DWORD WINAPI IOCPLoop(LPVOID lpParam)
{
    auto& data = *(IOCPLoopData*)lpParam;
    std::cout << "IOCPloop " << data.iocp << std::endl;
    unsigned long bytes = 0;
    unsigned long long cKey = 0;
    OVERLAPPED* overlapped;
    std::cout << "IOCPloop waiting" << std::endl;
    auto r = GetQueuedCompletionStatus(
        data.iocp,
        &bytes,
        &cKey,
        &overlapped,
        INFINITE
    );
    if(r){
        std::cout << "IOCPloop " << bytes << " " << cKey;
    }

    return 0;
}

int main(int argc, char** argv)
{    
    int LISTEN_PORT = 5000;

    //////////////////////////////////////////// init

    WSADATA wsa;
	WSAStartup(MAKEWORD(2, 2), &wsa);

	/// create TCP socket with RIO mode
	SOCKET mListenSocket = WSASocket(AF_INET, 
        SOCK_STREAM, 
        IPPROTO_TCP, 
        NULL, 
        0, 
        WSA_FLAG_REGISTERED_IO);
    int opt = 1;
	setsockopt(mListenSocket, 
        SOL_SOCKET, 
        SO_REUSEADDR, 
        (const char*)&opt, 
        sizeof(int));

    /// bind
    SOCKADDR_IN serveraddr;
	ZeroMemory(&serveraddr, sizeof(serveraddr));
	serveraddr.sin_family = AF_INET;
	serveraddr.sin_port = htons(LISTEN_PORT);
	serveraddr.sin_addr.s_addr = htonl(INADDR_ANY);

	bind(mListenSocket, (SOCKADDR*)&serveraddr, sizeof(serveraddr));

    /// RIO function table
	GUID functionTableId = WSAID_MULTIPLE_RIO;
	DWORD dwBytes = 0;

	WSAIoctl(mListenSocket,
        SIO_GET_MULTIPLE_EXTENSION_FUNCTION_POINTER, 
        &functionTableId, 
        sizeof(GUID), 
        (void**)&mRioFunctionTable, 
        sizeof(mRioFunctionTable), 
        &dwBytes, 
        NULL, 
        NULL);

    /// RIO INITIALIZE
    SYSTEM_INFO systemInfo;
	GetSystemInfo(&systemInfo);
	const unsigned __int64 granularity = systemInfo.dwAllocationGranularity; ///< maybe 64K

    if(SESSION_BUFFER_SIZE % granularity != 0){
        std::cout << "granularity" << std::endl;
    }
	
    auto mRioBufferPointer = reinterpret_cast<char*>(
        VirtualAllocEx(GetCurrentProcess(), 
        0, 
        SESSION_BUFFER_SIZE, 
        MEM_COMMIT | MEM_RESERVE, 
        PAGE_READWRITE)
    );
    mRioBufferPointer[630] = 0;
    std::cout << "Buffer" << std::endl;
    std::cout << mRioBufferPointer << std::endl;

    auto mRioBufferId = mRioFunctionTable.RIORegisterBuffer(
        mRioBufferPointer, 
        SESSION_BUFFER_SIZE);
    std::cout << "RIO bufferId: " << mRioBufferId << std::endl;

    ////////////////////////////////////////////////// completion port

    auto iocp = 
       CreateIoCompletionPort(
           INVALID_HANDLE_VALUE, 
           NULL, 
           0, 
           0 );

    std::cout << "IOCP id " << iocp << std::endl;

    IOCPLoopData param = {0};
    param.iocp = iocp;
    HANDLE thread1;
    thread1 = CreateThread( 
        NULL, 
        0, 
        IOCPLoop, 
        &param, 
        0, 
        NULL); 
    ////////////////////////////////////////////////// completion queue

    OVERLAPPED overlapped;
    RIO_NOTIFICATION_COMPLETION notification;
    notification.Type = RIO_IOCP_COMPLETION;
    notification.Iocp.IocpHandle = iocp;
    notification.Iocp.Overlapped = &overlapped;
    notification.Iocp.CompletionKey = 0;
    mRioCompletionQueue[0] = mRioFunctionTable.RIOCreateCompletionQueue(
        MAX_CQ_SIZE_PER_RIO_THREAD, 
        &notification);
    mRioFunctionTable.RIONotify(mRioCompletionQueue[0]);
    std::cout << "RIO completionQueue: " << mRioCompletionQueue[0] << std::endl;
    
    ////////////////////////////////////////////////// accept
    
    listen(mListenSocket, SOMAXCONN);
    std::cout << "listening..." << std::endl;

    while (true)
	{
        sockaddr clientaddr;
		int addrlen = sizeof(clientaddr);
		SOCKET acceptedSock = accept(mListenSocket, &clientaddr, &addrlen);
		//getpeername(acceptedSock, (SOCKADDR*)&clientaddr, &addrlen);

        

		// /// make socket non-blocking
	    // u_long arg = 1 ;
	    // ioctlsocket(acceptedSock, FIONBIO, &arg);

        // /// turn off nagle
	    // int opt = 1 ;
	    // setsockopt(acceptedSock, 
        //     IPPROTO_TCP, 
        //     TCP_NODELAY, 
        //     (const char*)&opt, 
        //     sizeof(int));

        /// create socket RQ
	    /// SEND and RECV within one CQ (you can do with two CQs, seperately)
        auto queue = mRioFunctionTable.RIOCreateRequestQueue(acceptedSock, 
            MAX_RECV_RQ_SIZE_PER_SOCKET, 
            1, 
            MAX_SEND_RQ_SIZE_PER_SOCKET, 
            1,
		    mRioCompletionQueue[0], // ReceiveCQ
            mRioCompletionQueue[0], // SendCQ
            NULL);

        //RECEIVE 
        auto rioBuffer = RIO_BUF {0};
        rioBuffer.BufferId = mRioBufferId;
        rioBuffer.Length  = SESSION_BUFFER_SIZE;
        rioBuffer.Offset  = 0;

        
        /// start async recv
        

        // while(true)
        // {
        DWORD flags = 0;
        auto r = mRioFunctionTable.RIOReceive(queue, 
            &rioBuffer, 
            1, 
            flags, 
            &rioBuffer);
        //     std::cout << "RIOReceive: " << r << std::endl;

        //     RIORESULT results[MAX_RIO_RESULT] = {0};
        //     ULONG numResults = mRioFunctionTable.RIODequeueCompletion(
        //         mRioCompletionQueue[0], 
        //         results, 
        //         MAX_RIO_RESULT);
            
        //     while(numResults == 0) {            
        //         std::cout << "waiting..." << std::endl;
        //         Sleep(1000);

        //         numResults = mRioFunctionTable.RIODequeueCompletion(
        //             mRioCompletionQueue[0], 
        //             results, 
        //             MAX_RIO_RESULT);
        //     }

        //     std::cout << "numResults: " << numResults << std::endl;

        //     for(int i = 0;i < numResults;++i){
        //         auto &rioBuffer2 = results[i].RequestContext;
        //         std::cout << i << " bytes transferred: " << results[i].BytesTransferred << std::endl;
        //         std::cout << i << " status: " << results[i].Status << std::endl;

        //         mRioBufferPointer[results[i].BytesTransferred] = 0;
                
        //         std::cout << "Request" << std::endl;
        //         std::cout << mRioBufferPointer << std::endl;
        //     }

        //     std::cout << "Send" << std::endl;

        //     auto sendRioBuffer = RIO_BUF {0};
        //     sendRioBuffer.BufferId = mRioBufferId;
        //     sendRioBuffer.Offset  = results[0].BytesTransferred + 1;
            

        //     const char* response = "HTTP/1.1 200 OK\nContent-Length: 44\n\n<html><body><h1>It works!</h1></body></html>";
        //     memcpy(&mRioBufferPointer[sendRioBuffer.Offset], (void*)response, strlen(response));
        //     sendRioBuffer.Length = strlen(response);

        //     // std::cout << strlen(response) << std::endl;
        //     // std::cout << &mRioBufferPointer[sendRioBuffer.Offset] << std::endl;

        //     numResults = mRioFunctionTable.RIOSend(
        //         queue, 
        //         &sendRioBuffer, 
        //         1, 
        //         flags, 
        //         &sendRioBuffer);

        //     std::cout << "numResults: " << numResults << std::endl;
        // }

        Sleep(10000);
	}

    // Cleanup
    WSACleanup();
}