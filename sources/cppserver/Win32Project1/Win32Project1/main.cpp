/*
    Bind socket to port 8888 on localhost
	*/
#include<io.h>
#include<stdio.h>
#include<winsock2.h>
#include "main.h"

#pragma comment(lib,"ws2_32.lib") //Winsock Library

int main(int argc, char *argv[])
{
	WSADATA wsa;
	SOCKET s, new_socket;
	struct sockaddr_in server, client;
	int c;
	char *message;

	printf("\nInitialising Winsock...");
	if (WSAStartup(MAKEWORD(2, 2), &wsa) != 0)
	{
		printf("Failed. Error Code : %d", WSAGetLastError());
		return 1;
	}

	printf("Initialised.\n");

	//Create a socket
	if ((s = socket(AF_INET, SOCK_STREAM, 0)) == INVALID_SOCKET)
	{
		printf("Could not create socket : %d", WSAGetLastError());
	}

	printf("Socket created.\n");

	//Prepare the sockaddr_in structure
	server.sin_family = AF_INET;
	server.sin_addr.s_addr = INADDR_ANY;
	server.sin_port = htons(8889);

	//Bind
	if (bind(s, (struct sockaddr *)&server, sizeof(server)) == SOCKET_ERROR)
	{
		printf("Bind failed with error code : %d", WSAGetLastError());
	}

	puts("Bind done");

	//Listen to incoming connections
	listen(s, 3);

	//Accept and incoming connection
	puts("Waiting for incoming connections...");

	c = sizeof(struct sockaddr_in);
	new_socket = accept(s, (struct sockaddr *)&client, &c);
	if (new_socket == INVALID_SOCKET)
	{
		printf("accept failed with error code : %d", WSAGetLastError());
	}

	puts("Connection accepted");

	//Reply to client
	
	auto message1 = "HTTP/1.1 ";
	send(new_socket, message1, strlen(message1), 0);
	Send200OK(new_socket);
	
	/*auto message4 = "Content-Length: ";
	auto message5 = "44\n";
	send(new_socket, message4, strlen(message4), 0);
	send(new_socket, message5, strlen(message5), 0);*/

	SendContentTypeHtml(new_socket);
	SendTransferEncodingChunked(new_socket);
	SendEndHeaders(new_socket);
	
	auto message8 = "2c\r\n<html><body><h1>It works!</h1></body></html>\r\n0\r\n\r\n";
	send(new_socket, message8, strlen(message8), 0);

	getchar();

	closesocket(s);
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
