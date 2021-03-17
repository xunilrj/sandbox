
// #include <stdio.h>
// #include <unistd.h>
// #include <string.h>
// #include <sys/socket.h>
// #include <arpa/inet.h>
// #include <openssl/ssl.h>
// #include <openssl/err.h>
// #include <iostream>

// //https://security.stackexchange.com/questions/90422/ssl-certificates-and-cipher-suites-correspondence

// int create_socket(int port)
// {
//     int s;
//     struct sockaddr_in addr;

//     addr.sin_family = AF_INET;
//     addr.sin_port = htons(port);
//     addr.sin_addr.s_addr = htonl(INADDR_ANY);

//     s = socket(AF_INET, SOCK_STREAM, 0);
//     bind(s, (struct sockaddr *)&addr, sizeof(addr));
//     listen(s, 1);

//     return s;
// }

// void cleanup_openssl()
// {
//     EVP_cleanup();
// }

// int main(int argc, char **argv)
// {
//     SSL_load_error_strings();
//     OpenSSL_add_ssl_algorithms();

//     const SSL_METHOD *method = TLS_server_method();

//     SSL_CTX *ctx = SSL_CTX_new(method);
//     SSL_CTX_set_min_proto_version(ctx, TLS1_1_VERSION);
//     SSL_CTX_set_max_proto_version(ctx, TLS1_3_VERSION);
//     SSL_CTX_set_cipher_list(ctx, "TLS13-AES-256-GCM-SHA384");
//     SSL_CTX_set_ecdh_auto(ctx, 1);
//     SSL_CTX_use_certificate_file(ctx, "certificate.pem", SSL_FILETYPE_PEM);
//     SSL_CTX_use_PrivateKey_file(ctx, "key.pem", SSL_FILETYPE_PEM);
//     SSL_CTX_set_verify(ctx, SSL_VERIFY_PEER | SSL_VERIFY_FAIL_IF_NO_PEER_CERT, [](int, X509_STORE_CTX *) {
//         printf("Verifying.");
//         return 1;
//     });

//     const int sock = create_socket(4443);

//     const char reply[] = "test\n";
//     while (1)
//     {
//         int client = accept(sock, nullptr, nullptr);

//         SSL *ssl = SSL_new(ctx);
//         SSL_set_fd(ssl, client);

//         std::cout << "Server Ciphers" << std::endl;
//         auto ciphers = SSL_get_ciphers(ssl);
//         while (auto top = sk_SSL_CIPHER_pop(ciphers))
//         {
//             std::cout << SSL_CIPHER_get_name(top) << " " << SSL_CIPHER_get_id(top) << std::endl;
//         }

//         const auto accepted = SSL_accept(ssl);
//         ERR_print_errors_fp(stderr);
//         std::cout << "Accepted:" << accepted << std::endl;

//         std::cout << "Client Ciphers" << std::endl;
//         ciphers = SSL_get_client_ciphers(ssl);
//         while (auto top = sk_SSL_CIPHER_pop(ciphers))
//         {
//             std::cout << SSL_CIPHER_get_name(top) << " " << SSL_CIPHER_get_id(top) << std::endl;
//         }

//         SSL_write(ssl, reply, strlen(reply));

//         SSL_shutdown(ssl);
//         SSL_free(ssl);

//         close(client);

//         std::cout << "Closed" << std::endl;
//     }

//     close(sock);
//     SSL_CTX_free(ctx);
//     cleanup_openssl();
// }

#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <openssl/ssl.h>
#include <openssl/err.h>
#include <iostream>

int create_socket(int port)
{
    int s;
    struct sockaddr_in addr;

    addr.sin_family = AF_INET;
    addr.sin_port = htons(port);
    addr.sin_addr.s_addr = htonl(INADDR_ANY);

    s = socket(AF_INET, SOCK_STREAM, 0);
    if (s < 0)
    {
        perror("Unable to create socket");
        exit(EXIT_FAILURE);
    }

    if (bind(s, (struct sockaddr *)&addr, sizeof(addr)) < 0)
    {
        perror("Unable to bind");
        exit(EXIT_FAILURE);
    }

    if (listen(s, 1) < 0)
    {
        perror("Unable to listen");
        exit(EXIT_FAILURE);
    }

    return s;
}

void cleanup_openssl()
{
    EVP_cleanup();
}

int main(int argc, char **argv)
{
    int sock;

    SSL_load_error_strings();
    // OpenSSL_add_ssl_algorithms();

    const SSL_METHOD *method = TLS_server_method();
    SSL_CTX *ctx = SSL_CTX_new(method);
    SSL_CTX_set_cipher_list(ctx, "TLS13-AES-256-GCM-SHA384");

    SSL_CTX_set_ecdh_auto(ctx, 1);
    SSL_CTX_use_certificate_file(ctx, "certificate.pem", SSL_FILETYPE_PEM);
    SSL_CTX_use_PrivateKey_file(ctx, "key.pem", SSL_FILETYPE_PEM);

    sock = create_socket(4443);

    while (1)
    {
        int client = accept(sock, nullptr, nullptr);

        SSL *ssl = SSL_new(ctx);
        SSL_set_fd(ssl, client);
        SSL_accept(ssl);

        //https://www.openssl.org/docs/man1.1.0/man3/SSL_get_current_cipher.html
        // const auto ciphers = SSL_get_client_ciphers(ssl);
        // while (const auto cipher = sk_SSL_CIPHER_pop(ciphers))
        // {
        //     std::cout << "Cipher: " << SSL_CIPHER_get_name(cipher) << std::endl;
        // }
        const auto current_cipher = SSL_get_current_cipher(ssl);
        std::cout << "Current: " << SSL_CIPHER_get_name(current_cipher) << std::endl;

        const char reply[] = "test\n";
        SSL_write(ssl, reply, strlen(reply));

        SSL_shutdown(ssl);
        SSL_free(ssl);

        close(client);
    }

    close(sock);
    SSL_CTX_free(ctx);
    cleanup_openssl();
}
