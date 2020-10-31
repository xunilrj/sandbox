#include <iostream>
#include <string>

#include <dlfcn.h>

using PublishMessage = void (*)(
    uint64_t,
    uint64_t,
    uint64_t,
    const uint8_t *);

enum Messages
{
    PrepareInstallModule = 1,
    InstallModule = 2,
    PrepareUninstallModule = 3,
    UninstallModule = 4,

    BusMessage = 5,
};

int main(int argc, char **argv)
{
    auto h = dlopen("./MySharedLib.so", RTLD_NOW | RTLD_GLOBAL);
    auto f = (PublishMessage)dlsym(h, "print_hello_from_rust");

    std::cout << h << " " << f << std::endl;

    std::string name = "Daniel";
    std::string msg_json = "{}";
    f(0, Messages::PrepareInstallModule, msg_json.length(), (const uint8_t *)(void *)msg_json.c_str());
}