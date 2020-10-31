#include <stdio.h>

int main()
{
    auto r = fopen("a|b.txt", "r");
    printf("%p", r);
    return 0;
}