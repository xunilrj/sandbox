// watch.cpp : This file contains the 'main' function. Program execution begins and ends there.
//

#include "pch.h"
#include <iostream>
#include <set>
#include <algorithm>
#include <string>

#include <Windows.h>
#include <WinHttp.h>
#pragma comment(lib, "winhttp.lib")
#include <Websocket.h>

struct noncopyable {};
struct moveable {};

struct unique_handle : noncopyable, moveable
{
	unique_handle()
		: handle {INVALID_HANDLE_VALUE}
	{

	}
	unique_handle(const unique_handle&) = delete;
	unique_handle& operator=(const unique_handle&) = delete;
	
	unique_handle(HANDLE&& other)
		: handle(other)
	{
		other = INVALID_HANDLE_VALUE;
	}

	unique_handle(unique_handle&& other)
		: handle(other.handle)
	{
		other.handle = INVALID_HANDLE_VALUE;
	}
	
	unique_handle& operator=(unique_handle&& other)
	{
		if (this != &other) {
			handle = other.handle;
			other.handle = INVALID_HANDLE_VALUE;
		}
		return *this;
	}
	
	~unique_handle()
	{
		if(handle != INVALID_HANDLE_VALUE)
			::CloseHandle(handle);
	}

	operator HANDLE () const & { return handle; }
	operator HANDLE () && { return release(); }
	[[nodiscard]] HANDLE get() & { return handle; }
	[[nodiscard]] HANDLE release()
	{
		auto h = handle;
		handle = INVALID_HANDLE_VALUE;
		return h;
	}
private:
	HANDLE handle;
};

template <typename T, typename T2>
struct property
{
	property(T2& ref) :
		pipeTo{ ref }
	{
	}

	property<T,T2>& operator=(T&& other)
	{
		if (&value != &other) {
			value = std::move(other);
			pipeTo = value;
		}
		return *this;
	}

	[[nodiscard]] operator T2& () const & { return pipeTo; }
	[[nodiscard]] operator T& () const & { return value; }
private:
	T value;
	T2& pipeTo;
};


namespace w32
{
	//events
	unique_handle CreateEventW(_In_opt_ LPSECURITY_ATTRIBUTES lpEventAttributes, _In_ BOOL bManualReset, _In_ BOOL bInitialState, _In_opt_ LPCWSTR lpName){return ::CreateEventW(lpEventAttributes, bManualReset, bInitialState, lpName);}
	
	//io
	unique_handle CreateFile(_In_ LPCSTR lpFileName, _In_ DWORD dwDesiredAccess, _In_ DWORD dwShareMode, _In_opt_ LPSECURITY_ATTRIBUTES lpSecurityAttributes, _In_ DWORD dwCreationDisposition, _In_ DWORD dwFlagsAndAttributes, _In_opt_ HANDLE hTemplateFile){return ::CreateFileA(lpFileName, dwDesiredAccess, dwShareMode, lpSecurityAttributes, dwCreationDisposition, dwFlagsAndAttributes, hTemplateFile);}

	//http
	unique_handle WinHttpOpen(_In_opt_z_ LPCWSTR pszAgentW, _In_ DWORD dwAccessType, _In_opt_z_ LPCWSTR pszProxyW, _In_opt_z_ LPCWSTR pszProxyBypassW, _In_ DWORD dwFlags){return ::WinHttpOpen(pszAgentW, dwAccessType, pszProxyW, pszProxyBypassW, dwFlags); }
	unique_handle WinHttpConnect(IN HINTERNET hSession, IN LPCWSTR pswzServerName, IN INTERNET_PORT nServerPort, IN DWORD dwReserved){return ::WinHttpConnect(hSession,pswzServerName, nServerPort, dwReserved);}
	
	//websocket
	HRESULT WINAPI WebSocketCreateServerHandle(_In_reads_(ulPropertyCount) const PWEB_SOCKET_PROPERTY pProperties, _In_ ULONG ulPropertyCount, _Out_ WEB_SOCKET_HANDLE *phWebSocket) { return WebSocketCreateServerHandle(pProperties, ulPropertyCount, phWebSocket); }
	DWORD WinHttpWebSocketReceive(_In_ HINTERNET hWebSocket, _Out_writes_bytes_to_(dwBufferLength, *pdwBytesRead) PVOID pvBuffer, _In_ DWORD dwBufferLength, _Out_range_(0, dwBufferLength) DWORD *pdwBytesRead, _Out_ WINHTTP_WEB_SOCKET_BUFFER_TYPE *peBufferType){return ::WinHttpWebSocketReceive(hWebSocket, pvBuffer, dwBufferLength, pdwBytesRead, peBufferType);}

	struct OVERLAPPED
	{
		property<unique_handle, HANDLE> hEvent;

		OVERLAPPED(int) :
			ovl { 0 },
			hEvent{ ovl.hEvent }
		{
		}

		OVERLAPPED() :
			ovl{ 0 },
			hEvent{ ovl.hEvent }
		{
		}

		operator ::OVERLAPPED () const & { return ovl; }
		::OVERLAPPED* operator& () { return &ovl; }
	private:
		::OVERLAPPED ovl;
	};
}

namespace w32x
{
	template <typename F>
	void Watch(const std::string& dir, F f)
	{
		auto dirHandle = w32::CreateFile(dir.c_str(),
			FILE_LIST_DIRECTORY,
			FILE_SHARE_WRITE | FILE_SHARE_READ | FILE_SHARE_DELETE,
			NULL,
			OPEN_EXISTING,
			FILE_FLAG_BACKUP_SEMANTICS | FILE_FLAG_OVERLAPPED,
			NULL);

		FILE_NOTIFY_INFORMATION strFileNotifyInfo[1024]{ 0 };

		w32::OVERLAPPED ovl{ 0 };
		ovl.hEvent = w32::CreateEvent(NULL, TRUE, FALSE, NULL);

		std::set<std::wstring> modifications;

		std::cout << "waiting..." << std::endl;
		::ReadDirectoryChangesW(dirHandle,
			(LPVOID)&strFileNotifyInfo,
			sizeof(strFileNotifyInfo),
			FALSE,
			FILE_NOTIFY_CHANGE_LAST_WRITE,
			NULL,
			&ovl,
			NULL);

		while (1)
		{
			DWORD result = ::WaitForSingleObject(ovl.hEvent, 1000);

			switch (result)
			{
			case WAIT_TIMEOUT:
				for (auto x : modifications)
					f(x);
				modifications.clear();
				break;
			case WAIT_OBJECT_0:
				unsigned long dw{ 0 };
				::GetOverlappedResult(dirHandle, &ovl, &dw, FALSE);

				auto& info = strFileNotifyInfo[0];
				modifications.emplace(info.FileName, info.FileNameLength / 2);

				::ResetEvent(ovl.hEvent);
				::ReadDirectoryChangesW(dirHandle,
					(LPVOID)&strFileNotifyInfo,
					sizeof(strFileNotifyInfo),
					FALSE,
					FILE_NOTIFY_CHANGE_LAST_WRITE,
					NULL,
					&ovl,
					NULL);
				break;
			}
		}
	}

	void WriteConsoleA(const char* str,
		int N,
		DWORD nStdHandle = STD_OUTPUT_HANDLE)
	{
		::HANDLE h = (::HANDLE)::GetStdHandle(STD_OUTPUT_HANDLE);
		::DWORD nWritten;
		::WriteConsoleA(h, str, N, &nWritten, NULL);
	}

	void WriteConsoleW(const wchar_t* str,
		int N,
		DWORD nStdHandle = STD_OUTPUT_HANDLE)
	{
		::HANDLE h = (::HANDLE)::GetStdHandle(STD_OUTPUT_HANDLE);
		::DWORD nWritten;
		::WriteConsoleW(h, str, N, &nWritten, NULL);
	}
	
	template <unsigned int N>
	void WriteConsole(const char (&str)[N],
		DWORD nStdHandle = STD_OUTPUT_HANDLE)
	{
		WriteConsoleA(str, N - 1, nStdHandle);
	}

	void WriteConsole(const std::string& str, 
		DWORD nStdHandle = STD_OUTPUT_HANDLE)
	{
		WriteConsoleA(str.c_str(), str.length(), nStdHandle);
	}

	void WriteConsole(const std::wstring& str,
		DWORD nStdHandle = STD_OUTPUT_HANDLE)
	{
		WriteConsoleW(str.c_str(), str.length(), nStdHandle);
	}

	void ClearScreen(DWORD nStdHandle = STD_OUTPUT_HANDLE)
	{
		COORD coordScreen = { 0, 0 };
		DWORD cCharsWritten;
		CONSOLE_SCREEN_BUFFER_INFO csbi;
		DWORD dwConSize;

		::HANDLE h = (::HANDLE)::GetStdHandle(STD_OUTPUT_HANDLE);
		::GetConsoleScreenBufferInfo(h, &csbi);
		dwConSize = csbi.dwSize.X * csbi.dwSize.Y;

		::FillConsoleOutputCharacter(h, (TCHAR)' ', dwConSize, coordScreen, &cCharsWritten );
		::GetConsoleScreenBufferInfo(h, &csbi );
		::FillConsoleOutputAttribute(h, csbi.wAttributes, dwConSize, coordScreen, &cCharsWritten );
		::SetConsoleCursorPosition(h, coordScreen );
		return;
	}
}

int main(int argc, char** argv)
{
	const WCHAR *pcwszServerName = L"localhost";
	INTERNET_PORT Port = INTERNET_DEFAULT_HTTP_PORT;

	auto session = WinHttpOpen(L"WebSocket sample",
		WINHTTP_ACCESS_TYPE_DEFAULT_PROXY,
		NULL,
		NULL,
		0);

	WEB_SOCKET_HANDLE serverHandle = NULL;
	auto hr = WebSocketCreateServerHandle(NULL, 0, &serverHandle);
	
	auto dir = argv[1];
	w32x::Watch(dir, [](auto&& file) {
		w32x::WriteConsole(file + L"\n");
	});
}