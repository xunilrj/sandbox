/*
    Bind socket to port 8888 on localhost
	*/
#include<io.h>
#include<stdio.h>
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

struct socket_trait
{
	static auto invalid() noexcept
	{
		return 0;
	}

	static auto close(SOCKET value) noexcept
	{
		closesocket(value);
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

	void send(const char * buffer, int len)
	{
		::send(handle.get(), buffer, len, 0);
	}
};

struct socket_server
{
	socket_handle handle;

	auto open(unsigned int port, unsigned int backlog)
	{
		auto const result = socket(AF_INET, SOCK_STREAM, 0);

		if (result == INVALID_SOCKET) {
			throw 1;
		}

		struct sockaddr_in server;
		server.sin_family = AF_INET;
		server.sin_addr.s_addr = INADDR_ANY;
		server.sin_port = htons(port);

		bind(result, (struct sockaddr *)&server, sizeof(server));

		listen(result, backlog);

		auto localHandle = socket_handle(result);
		handle = std::move(localHandle);
	}

	auto accept()
	{
		struct sockaddr_in client;
		int c = sizeof(struct sockaddr_in);
		auto const result = ::accept(handle.get(), (struct sockaddr *)&client, &c);

		auto localHandle = socket_handle(result);
		return socket_client(localHandle);
	}
};



int main(int argc, char *argv[])
{
	WSADATA wsa;
	printf("\nInitialising Winsock...");
	if (WSAStartup(MAKEWORD(2, 2), &wsa) != 0)
	{
		printf("Failed. Error Code : %d", WSAGetLastError());
		return 1;
	}
	printf("Initialised.\n");

	socket_server ss;
	ss.open(8889, 1);
	auto sc  = ss.accept();
	auto response = "HTTP/1.1 200 OK\nContent-Length: 2\n\nOK";
	sc.send(response, strlen(response));
			
	getchar();
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
