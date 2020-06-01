namespace browser::console
{
    extern void log(int i) asm("consoleLog");
}

__attribute__((export_name("sum"))) int add(int l, int r)
{
    browser::console::log(1);
    return l + r;
}