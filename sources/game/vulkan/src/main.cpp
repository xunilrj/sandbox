#include <vector>
#include <iostream>
#include <optional>
#include <tuple>
#include <algorithm>
#include <string>
#include <sstream>
#include <Windows.h>
#include "vulkan.h"

LRESULT CALLBACK WndProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
	switch (msg)
	{
		case WM_CLOSE: DestroyWindow(hwnd); break;
		case WM_DESTROY: PostQuitMessage(0); break;
		default: return DefWindowProcW(hwnd, msg, wParam, lParam);
	}
	return 0;
}

HWND createWindow(HINSTANCE hInstance, const std::wstring& className, int nCmdShow,
    uint32_t w = 640, uint32_t h = 480)
{
    WNDCLASSEXW wc;
	wc.cbSize = sizeof(WNDCLASSEX);
	wc.style = 0;
	wc.lpfnWndProc = WndProc;
	wc.cbClsExtra = 0;
	wc.cbWndExtra = 0;
	wc.hInstance = hInstance;
	wc.hIcon = LoadIcon(NULL, IDI_APPLICATION);
	wc.hCursor = LoadCursor(NULL, IDC_ARROW);
	wc.hbrBackground = (HBRUSH)(COLOR_WINDOW + 1);
	wc.lpszMenuName = NULL;
	wc.lpszClassName = className.c_str();
	wc.hIconSm = LoadIcon(NULL, IDI_APPLICATION);
	RegisterClassExW(&wc);
    
	HWND hwnd = CreateWindowExW(
		WS_EX_CLIENTEDGE,
		className.c_str(),
		L"",
		WS_OVERLAPPEDWINDOW,
		CW_USEDEFAULT, CW_USEDEFAULT, 
		640, 480,
		NULL, NULL, hInstance, NULL);
	ShowWindow(hwnd, nCmdShow);
	UpdateWindow(hwnd);

    auto dwStyle = (DWORD)GetWindowLongPtrW(hwnd, GWL_STYLE) ;
    auto dwExStyle = (DWORD)GetWindowLongPtrW(hwnd, GWL_EXSTYLE) ;
    auto menu = GetMenu(hwnd) ;
    auto rc = RECT{ 0, 0, (LONG)w, (LONG)h } ;

    AdjustWindowRectEx(&rc, dwStyle, menu ? TRUE : FALSE, dwExStyle);
    SetWindowPos(hwnd, NULL, 0, 0, 
        rc.right - rc.left,
        rc.bottom - rc.top,
        SWP_NOZORDER | SWP_NOMOVE) ;

    return hwnd;
}

#include <functional>
#include <chrono>
template<typename TimeT = std::chrono::milliseconds, typename F, typename ...Args>
auto duration(F&& func, Args&&... args)
{
    auto start = std::chrono::steady_clock::now();
    auto r = std::invoke(std::forward<decltype(func)>(func), forward<Args>(args)...);
    return std::make_tuple(
        std::chrono::duration_cast<TimeT>(std::chrono::steady_clock::now()-start),
        r);
}

struct LoopThreadParams
{
    HINSTANCE hInstance;
	HWND hwnd;
};

template <typename T>
void SetWindowText(HWND hwnd, T value)
{
    std::wstringstream ss;
    ss << value;
    SetWindowTextW(hwnd, ss.str().c_str());
}

DWORD WINAPI LoopThread(LPVOID lpParam)
{
    auto& p = *(LoopThreadParams*)lpParam;
    auto& hInstance = p.hInstance;
    auto& hwnd = p.hwnd;

    auto instance = createVkInstance("Vulkan Test", 
        VK_KHR_SURFACE_EXTENSION_NAME "," VK_KHR_WIN32_SURFACE_EXTENSION_NAME);
    auto surface = createVkSurface(hInstance, hwnd, instance);

    auto devices = enumeratePhysicalDevices(instance);
    auto gpu0 = devices[0];
    auto [index, queueFamily] = getQueue(gpu0, [&](auto i, auto&& f)
        { 
            return queueFamilySupports(f, VK_QUEUE_GRAPHICS_BIT) && queueFamilyPresents(gpu0, surface, i); 
        }).value();    
    auto device = createDevice(gpu0, index, VK_KHR_SWAPCHAIN_EXTENSION_NAME);

    auto queue = getQueue(device, index, 0);
    auto swapChain = createSwapChain(gpu0, device, surface);

    while(true) 
    {
        auto [dur, imgIndex] = duration([&]() { return waitAcquireNextImage(device, swapChain); });
        SetWindowText(hwnd, std::chrono::duration_cast<std::chrono::milliseconds>(dur).count());
        queue << present(swapChain, imgIndex);        
    }
}

int WINAPI WinMain(HINSTANCE hInstance,
    HINSTANCE hPrevInstance,
	LPSTR lpCmdLine, int nCmdShow)
{
    auto hwnd = createWindow(hInstance, L"vulcanWindow", nCmdShow);

	LoopThreadParams p = { hInstance, hwnd };
	DWORD   dwThreadIdArray;
	HANDLE  hThreadArray = CreateThread(
		NULL,
		0,
		LoopThread,
		&p,
		0,
		&dwThreadIdArray);

    MSG msg;
	while(GetMessage(&msg, NULL, 0, 0 ))
    { 
		TranslateMessage(&msg); 
		DispatchMessage(&msg); 
    } 
    return 0;
}