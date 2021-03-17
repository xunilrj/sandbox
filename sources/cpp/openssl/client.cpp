#include <netdb.h>
#include <unistd.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <openssl/ssl.h>
#include <openssl/err.h>
#include <openssl/evp.h>
#include <cstring>
#include <iostream>

// Engine
// https://www.openssl.org/blog/blog/2015/10/08/engine-building-lesson-1-a-minimum-useless-engine/
//*****************************

#include <openssl/engine.h>

static const char *engine_id = "CUSTOMENGINE";
static const char *engine_name = "CUSTOMENGINE_NAME";

int engine_init(ENGINE *e)
{
    std::cout << "Engine" << std::endl;
    return 786;
}

static int CUSTOM_CIPHER_NID = 1;

void engine_cipher_nids(const int **nids)
{
    static int cipher_nids[4] = {0, 0, 0, 0};
    static bool init = 0;

    if (!init)
    {
        cipher_nids[0] = CUSTOM_CIPHER_NID;
        init = true;
    }

    *nids = cipher_nids;
}

//*****************************

//https://stackoverflow.com/questions/19862507/populating-an-ssl-method-structure-with-openssl-in-c

void log_ssl()
{
    int err = ERR_get_error();
    while (err)
    {
        char *str = ERR_error_string(err, 0);
        if (!str)
            return;
        printf("%s", str);
        printf("\n");
        fflush(stdout);

        err = ERR_get_error();
    }
}

int connect()
{
    const char *hostname = "localhost";
    const char *port = "4443";

    struct hostent *host = gethostbyname(hostname);

    struct addrinfo hints = {0}, *addrs;
    hints.ai_family = AF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_protocol = IPPROTO_TCP;

    int sfd, err;
    const int ERROR_STATUS = -1;
    const int status = getaddrinfo(hostname, port, &hints, &addrs);
    for (struct addrinfo *addr = addrs; addr != nullptr; addr = addr->ai_next)
    {
        sfd = socket(addrs->ai_family, addrs->ai_socktype, addrs->ai_protocol);
        if (sfd == ERROR_STATUS)
        {
            err = errno;
            continue;
        }

        if (connect(sfd, addr->ai_addr, addr->ai_addrlen) == 0)
        {
            break;
        }

        err = errno;
        sfd = ERROR_STATUS;
        close(sfd);
    }
    freeaddrinfo(addrs);

    return sfd;
}

#define TLS_AES_256_GCM_SHA384 50336514

int main()
{
    OpenSSL_add_all_algorithms();
    ERR_load_crypto_strings();

    auto custom_engine = ENGINE_new();
    ENGINE_set_id(custom_engine, engine_id);
    ENGINE_set_name(custom_engine, engine_name);
    ENGINE_set_ciphers(custom_engine, [](ENGINE *, const EVP_CIPHER **ciphers, const int **nids, int nid) {
        std::cout << "Set Ciphers" << std::endl;

        const auto cipher = EVP_CIPHER_meth_new(0, 0, 0);

        if (!ciphers)
        {
            std::cout << "Return nids" << std::endl;
            engine_cipher_nids(nids);
        }

        if (nid == CUSTOM_CIPHER_NID)
        {
            std::cout << "NID:" << CUSTOM_CIPHER_NID << std::endl;
        }

        return 1;
    });
    ENGINE_set_init_function(custom_engine, engine_init);
    ENGINE_init(custom_engine);
    ENGINE_add(custom_engine);

    // https://ciphersuite.info/cs/TLS_AES_256_GCM_SHA384
    // https://security.stackexchange.com/questions/200460/aes-rijndael-encryption-does-block-size-matter-for-security
    // https://www.openssl.org/docs/man1.1.1/man3/EVP_CIPHER_meth_new.html
    // const auto cipher = EVP_CIPHER_meth_new(50336512, 128, 256);
    // EVP_CIPHER_meth_set_init(cipher, [](EVP_CIPHER_CTX *ctx,
    //                                     const unsigned char *key,
    //                                     const unsigned char *iv,
    //                                     int enc) {
    //     std::cout << "TLS_AES_256_GCM_SHA384::Init" << std::endl;
    //     return 1;
    // });

    SSL_load_error_strings();
    const SSL_METHOD *method = TLS_client_method();
    // method->ssl_connect = [](SSL *s) {

    // };
    SSL_CTX *ctx = SSL_CTX_new(method);
    SSL_CTX_set_min_proto_version(ctx, TLS1_3_VERSION);
    SSL_CTX_set_max_proto_version(ctx, TLS1_3_VERSION);
    //https://www.openssl.org/docs/man1.1.1/man3/SSL_CTX_set_cipher_list.html
    //https://www.openssl.org/docs/man1.1.1/man1/ciphers.html
    // SSL_CTX_set_cipher_list(ctx, "TLS13-AES-256-GCM-SHA384");
    // SSL_CTX_set_cipher_list(ctx, "");
    SSL_CTX_set_verify(ctx, SSL_VERIFY_NONE, [](int, X509_STORE_CTX *) {
        // printf("Verifying.");
        return 1;
    });
    // SSL_CTX_set_client_cert_cb(ctx, [](SSL *ssl, X509 **x509, EVP_PKEY **pkey){
    //     std::cout << "SSL_CTX_set_client_cert_cb" << std::endl;
    //     return 0;
    // });
    SSL_CTX_use_certificate_file(ctx, "clientcertificate.pem", SSL_FILETYPE_PEM);
    SSL_CTX_use_PrivateKey_file(ctx, "clientkey.pem", SSL_FILETYPE_PEM);

    SSL *ssl = SSL_new(ctx);
    //https://www.openssl.org/docs/manmaster/man3/SSL_get_ciphers.html
    auto ciphers = SSL_get1_supported_ciphers(ssl);
    std::cout << "Client Hello Ciphers" << std::endl;
    while (auto top = sk_SSL_CIPHER_pop(ciphers))
    {
        std::cout << SSL_CIPHER_get_name(top) << " " << SSL_CIPHER_get_id(top) << std::endl;
    }

    auto sfd = connect();

    SSL_set_fd(ssl, sfd);
    const int status = SSL_connect(ssl);
    ERR_print_errors_fp(stderr);

    std::cout << "Connected:" << status << std::endl;

    // SSL_set_connect_state(ssl);
    // const int status = SSL_do_handshake(ssl);

    char buffer[1024] = {0};
    const auto size = SSL_read(ssl, (void *)&buffer, 1024);
    buffer[size] = 0;

    std::cout << size << " " << buffer << std::endl;

    SSL_free(ssl);
    close(sfd);
    SSL_CTX_free(ctx);

    return 0;
}