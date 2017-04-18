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
		//WSAEVENT e = WSACreateEvent();
		//WSAEventSelect(value, e, FD_CLOSE);
		::shutdown(value, SD_BOTH);
		//WSAWaitForMultipleEvents(1, &e, FALSE, WSA_INFINITE, FALSE);

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

int main(int argc, char *argv[])
{
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
