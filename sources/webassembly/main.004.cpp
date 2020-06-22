#define EXPORT __attribute__((visibility("default")))

namespace browser::document
{
    extern void *getElementById(const char *id);
}

EXPORT
void *init()
{
    return browser::document::getElementById("screen");
}