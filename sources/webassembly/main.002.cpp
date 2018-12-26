#define EXPORT __attribute__((visibility("default")))

namespace browser::console
{
    extern void log(int i) asm("consoleLog");
}

EXPORT int add(int l, int r) asm("add");
int add(int l, int r)
{
    browser::console::log(1);
    return l + r;
}