
#include <iostream>

#include "include/http_parser.h"

#define ACE_AS_STATIC_LIBS
#include "ace/config.h"
#include "ace/INET_Addr.h"
#include "ace/SOCK_Acceptor.h"
#include "ace/SOCK_Connector.h"
#include "ace/SOCK_Stream.h"

bool keep = true;

class http_context {
public:

	http_context()
	{
	} 

	void start(ACE_SOCK_ACCEPTOR &acceptor)
	{
		ACE_SOCK_ACCEPTOR::PEER_STREAM stream;

		acceptor.accept(stream);

		http_parser_settings settings;
		settings.on_message_begin = [](auto parser) {
			return 0;
		};
		settings.on_message_complete = [](auto parser) {
			keep = false;
			return 0;
		};
		settings.on_url = [](auto parser, auto at, auto length) {
			return 0;
		};
		settings.on_header_field = [](auto parser, auto at, auto length) {
			return 0;
		};
		settings.on_header_value = [](auto parser, auto at, auto length) {
			return 0;
		};
		settings.on_headers_complete = [](auto parser) {
			return 0;
		};
		settings.on_body = [](auto parser, auto at, auto length) {
			return 0;
		};
		http_parser parser;
		http_parser_init(&parser, HTTP_REQUEST);
		parser.data = (void*)&stream;

		while (keep)
		{
			char buf[BUFSIZ];
			ssize_t recved = stream.recv(buf, sizeof buf);
			//stream.send_n(buf, n);
			//}

			if (recved > 0)
			{
				http_parser_execute(&parser, &settings, buf, recved);
			}
			else
			{
				keep = false;
			}
		}

		char response[] = "HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nok";
		stream.send_n(response, sizeof response);
	}
private:
	ACE_SOCK_ACCEPTOR::PEER_STREAM Stream;
};

template <class ACCEPTOR>
int echo_server(u_short port)
{
	ACCEPTOR::PEER_ADDR my_addr(port);
	ACCEPTOR acceptor(my_addr);	

	while (true)
	{
		std::cout << "Waiting client..." << std::endl;

		http_context ctx;
		ctx.start(acceptor);
	}

	return 0;
}

int main(int argc, char * argv[])
{
	auto result = echo_server<ACE_SOCK_ACCEPTOR>(7000);
	return 0;
}