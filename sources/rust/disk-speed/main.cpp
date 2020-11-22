#include <sys/types.h> 
#include <sys/stat.h> 
#include <unistd.h>
#include <fcntl.h> 
#include <cstdint>

int main(int argc, char **argv)
{
    uint8_t byte = 0;

    auto h = creat(".cpp", S_IRWXU);
    write(h, &byte, 1);
    fdatasync(h); 
    close(h);

    return 0;
}