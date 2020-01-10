# Windows C++ Programming

The bare minimum for a win32 application is the following.

```c++
#include <Windows.h>

int WINAPI WinMain(HINSTANCE hInstance,
    HINSTANCE hPrevInstance,
	LPSTR lpCmdLine, int nCmdShow)
{
}
```

We can compile this code with CMake