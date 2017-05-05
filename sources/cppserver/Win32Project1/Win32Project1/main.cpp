/*
    Bind socket to port 8888 on localhost
	*/
#include<io.h>
#include<stdio.h>
#include <string>
#include <iostream>     // std::cout, std::ios
#include <sstream> 
#include<winsock2.h>
#include "main.h"
#include <utility>

#pragma comment(lib,"ws2_32.lib") //Winsock Library

struct handle_traits
{
	static HANDLE invalid() throw()
	{
		return nullptr;
	}
	static void close(HANDLE value) throw()
	{
		CloseHandle(value);
	}
};

template <typename Type, typename Traits>
class unique_handle
{
	unique_handle(unique_handle const &) = delete;
	unique_handle & operator=(unique_handle const &) = delete;
	void close() throw()
	{
		if (*this)
		{
			Traits::close(m_value);
		}
	}
	Type m_value;
public:
	explicit unique_handle(Type value = Traits::invalid()) throw() :
		m_value(value)
	{
	}
	~unique_handle() throw()
	{
		close();
	}

private:
	struct boolean_struct { int member; };
	typedef int boolean_struct::* boolean_type;
	bool operator==(unique_handle const &) = delete;
	bool operator!=(unique_handle const &) = delete;
public:
	operator boolean_type() const throw()
	{
		return Traits::invalid() != m_value ? &boolean_struct::member : nullptr;
	}
	Type get() const throw()
	{
		return m_value;
	}
	bool reset(Type value = Traits::invalid()) throw()
	{
		if (m_value != value)
		{
			close();
			m_value = value;
		}
		return *this;
	}
	Type release() throw()
	{
		auto value = m_value;
		m_value = Traits::invalid();
		return value;
	}
	unique_handle(unique_handle && other) throw() :
		m_value(other.release())
	{
	}
	unique_handle & operator=(unique_handle && other) throw()
	{
		reset(other.release());
		return *this;
	}
};

struct wsa_trait
{
	static auto invalid() noexcept
	{
		return 0;
	}

	static auto close(int value) noexcept
	{
		WSACleanup();
	}
};

struct wsa
{
	unique_handle<int, wsa_trait> handle;

	wsa()
	{
		WSADATA wsa;
		::WSAStartup(MAKEWORD(2, 2), &wsa);
	}
};

struct socket_trait
{
	static auto invalid() noexcept
	{
		return INVALID_SOCKET;
	}

	static auto close(SOCKET value) noexcept
	{
		::shutdown(value, SD_BOTH);
		::closesocket(value);
	}
};

using socket_handle = unique_handle<SOCKET, socket_trait>;

struct socket_client
{
	socket_handle handle;

	//TODO is this right?
	socket_client(socket_handle &handle) : handle(std::move(handle))
	{
	}

	void receive()
	{
		char recvbuf[1024];
		int recvbuflen = 1024;
		auto result = recv(handle.get(), recvbuf, recvbuflen, 0);

		recvbuf[result] = 0;
		printf(recvbuf);
	}

	void send(const char * buffer, int len)
	{
		auto result = ::send(handle.get(), buffer, len, 0);
		if (result == SOCKET_ERROR) {
			printf("send failed with error: %d\n", WSAGetLastError());
		}
	}
};

struct socket_server
{
	socket_handle handle;

	auto open(unsigned int port)
	{
		auto const result = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);

		if (result == INVALID_SOCKET) {
			throw 1;
		}

		struct sockaddr_in server;
		server.sin_family = AF_INET;
		server.sin_addr.s_addr = INADDR_ANY;
		server.sin_port = htons(port);

		::bind(result, (struct sockaddr *)&server, sizeof(server));
		::listen(result, SOMAXCONN);

		auto localHandle = socket_handle(result);
		handle = std::move(localHandle);
	}

	auto accept()
	{
		auto const result = ::accept(handle.get(), NULL, NULL);

		if (result == INVALID_SOCKET) {
			printf("accept failed with error: %d\n", WSAGetLastError());
		}

		auto localHandle = socket_handle(result);
		return socket_client(localHandle);
	}
};

struct watcher_trait
{
	static auto invalid() noexcept
	{
		return INVALID_HANDLE_VALUE;
	}

	static auto close(HANDLE value) noexcept
	{
		FindCloseChangeNotification(value);
	}
};

using watcher_handle = unique_handle<HANDLE, watcher_trait>;

struct file_watcher
{
	file_watcher(std::string path) : handle(FindFirstChangeNotificationA(
		path.c_str(),
		FALSE,
		FILE_NOTIFY_CHANGE_LAST_WRITE))
	{
	}

	bool wait()
	{
		auto status = WaitForSingleObject(handle.get(), INFINITE);
		switch (status)
		{
			case WAIT_OBJECT_0:return true;
			default: return false;
		}
	}
private:
	watcher_handle handle;
};

class completion_port
{
	HANDLE h;
	HANDLE hf;
	completion_port(completion_port const &);
	completion_port & operator=(completion_port const &);
public:
	explicit completion_port(DWORD tc = 0) :
		h(CreateIoCompletionPort(INVALID_HANDLE_VALUE, nullptr, 0, tc))
	{
		//ASSERT(h);
	}
	~completion_port()
	{
		CloseHandle(h);
	}
	void add_file(HANDLE f, ULONG_PTR k = 0)
	{
		hf = f;
		auto r = CreateIoCompletionPort(f, h, k, 0);
	}
	void queue(DWORD c, ULONG_PTR k, OVERLAPPED * o)
	{
		PostQueuedCompletionStatus(h, c, k, o);
	}
	void dequeue(DWORD & c, ULONG_PTR & k, OVERLAPPED *& o)
	{
		GetQueuedCompletionStatus(h, &c, &k, &o, INFINITE);
	}
	void when()
	{
		OVERLAPPED o = {};
		char b[64];
		auto io = CreateThreadpoolIo(hf, [](PTP_CALLBACK_INSTANCE Instance,
			PVOID                 Context,
			PVOID                 Overlapped,
			ULONG                 IoResult,
			ULONG_PTR             NumberOfBytesTransferred,
			PTP_IO                Io)
		{
		}, b, nullptr);
		auto error = GetLastError();

		StartThreadpoolIo(io);
	}
};

struct namedpipe_trait
{
	static auto invalid() noexcept
	{
		return INVALID_HANDLE_VALUE;
	}

	static auto close(HANDLE value) noexcept
	{
		CloseHandle(value);
	}
};

using namedpipe_handle = unique_handle<HANDLE, namedpipe_trait>;

class namedpipe
{
public:
	namedpipe() : handle(CreateNamedPipe(
		L"\\\\.\\pipe\\my_pipe", // name of the pipe
		PIPE_ACCESS_DUPLEX |        // read/write access
		FILE_FLAG_OVERLAPPED,       // overlapped mode
		PIPE_TYPE_MESSAGE |         // message-type pipe
		PIPE_READMODE_MESSAGE |     // message read mode
		PIPE_WAIT,                  // blocking mode
		PIPE_UNLIMITED_INSTANCES,   // unlimited instances
		1024,                   // output buffer size
		1024,                   // input buffer size
		0,               // client time-out
		NULL))
	{
	}

	void associate(completion_port & port)
	{
		port.add_file(handle.get());
	}

	void connect()
	{
		ConnectNamedPipe(handle.get(), NULL);
	}

	void enqueue()
	{
		const wchar_t *data = L"*** Hello Pipe World ***";
		DWORD numBytesWritten = 0;
		WriteFile(handle.get(),
			data, // data to send
			wcslen(data) * sizeof(wchar_t), // length of data to send (bytes)
			&numBytesWritten, // will store actual amount of data sent
			NULL // not using overlapped IO
		);
	}
private:
	namedpipe_handle handle;
};

enum htmltag {
	htmltag_html,
	htmltag_head,
	htmltag_body,
	htmltag_div
};


struct hypernode
{
	hypernode(htmltag tag) : Tag(tag)
	{
	}

	htmltag Tag;
};

hypernode h(htmltag tag)
{
	return hypernode(tag);
}

void to_html(std::ostringstream& ss, hypernode * node)
{
	if (node->Tag == htmltag::htmltag_div)
	{
		ss << "<div>ok</div>";
	}
}

typedef void(*testFunction)();

#include "MemoryModule.h"

void* ReadLibrary(size_t* pSize) {
	size_t read;
	void* result;
	FILE* fp;
	fp = fopen("C:\\github\\xunilrj-sandbox\\sources\\cppserver\\Win32Project1\\Debug\\server\\server.dll", "rb");
	fseek(fp, 0, SEEK_END);
	*pSize = static_cast<size_t>(ftell(fp));
	if (*pSize == 0)
	{
		fclose(fp);
		return NULL;
	}

	result = (unsigned char *)malloc(*pSize);
	fseek(fp, 0, SEEK_SET);
	read = fread(result, 1, *pSize, fp);
	fclose(fp);
	return result;
}

void LoadFromMemory(void)
{
	void *data;
	size_t size;
	HMEMORYMODULE handle;
	testFunction f;
	HMEMORYRSRC resourceInfo;
	DWORD resourceSize;
	LPVOID resourceData;
	TCHAR buffer[100];
	data = ReadLibrary(&size);
	handle = MemoryLoadLibrary(data, size);

	f = (testFunction)MemoryGetProcAddress(handle, "start");
	f();

	resourceInfo = MemoryFindResource(handle, MAKEINTRESOURCE(VS_VERSION_INFO), RT_VERSION);
	resourceSize = MemorySizeofResource(handle, resourceInfo);
	resourceData = MemoryLoadResource(handle, resourceInfo);
	MemoryLoadString(handle, 1, buffer, sizeof(buffer));
	MemoryLoadString(handle, 20, buffer, sizeof(buffer));
	MemoryFreeLibrary(handle);

	free(data);
}

#include <chrono>
#include <thread>

int main(int argc, char *argv[])
{
	/*completion_port p;

	std::string dllpath = "C:\\github\\xunilrj-sandbox\\sources\\cppserver\\Win32Project1\\Debug\\server";
	{
		auto path = file_watcher(dllpath);
		path.wait();
		std::this_thread::sleep_for(std::chrono::milliseconds(1000));
	}
	*/
	
	completion_port port;
	namedpipe pipe;
	pipe.associate(port);
	port.when();

	TrySubmitThreadpoolCallback([](PTP_CALLBACK_INSTANCE, void *)
	{
		LoadFromMemory();
	},
		nullptr, nullptr);
	pipe.connect();
	pipe.enqueue();

	wsa wsa;

	socket_server ss;
	ss.open(8889);

	while (true) {
		auto sc = ss.accept();
		sc.receive();

		auto document = h(htmltag::htmltag_div);

		auto ss = std::ostringstream();
		to_html(ss, &document);
		auto html = ss.str();
		auto html_size = html.length();

		auto response = std::ostringstream();
		response << "HTTP/1.1 200 OK\nContent-Length: " << html_size << "\n\n";
		auto response_str = response.str();

		sc.send(response_str.c_str(), response_str.length());
		sc.send(html.c_str(), html_size);
	}

	WSACleanup();

	return 0;
}

void SendEndHeaders(const SOCKET &new_socket)
{
	auto message7 = "\n";
	send(new_socket, message7, strlen(message7), 0);
}

void SendContentTypeHtml(const SOCKET &new_socket)
{
	auto message6 = "Content-Type:text/html\n";
	send(new_socket, message6, strlen(message6), 0);
}

void SendTransferEncodingChunked(const SOCKET &new_socket)
{
	auto message6 = "Transfer-Encoding: chunked\n";
	send(new_socket, message6, strlen(message6), 0);
}

void Send200OK(const SOCKET &new_socket)
{
	auto message2 = "200 OK\n";
	send(new_socket, message2, strlen(message2), 0);
}
