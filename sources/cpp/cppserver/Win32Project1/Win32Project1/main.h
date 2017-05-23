#pragma once

void Send200OK(const SOCKET &new_socket);

void SendContentTypeHtml(const SOCKET &new_socket);
void SendTransferEncodingChunked(const SOCKET &new_socket);
void SendEndHeaders(const SOCKET &new_socket);
