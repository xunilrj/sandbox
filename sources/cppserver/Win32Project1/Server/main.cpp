#include <Windows.h>

BOOL WINAPI DllMain(HMODULE hinstDLL, DWORD fdwReason, LPVOID lpvReserved)
{
	switch (fdwReason)
	{
		case DLL_PROCESS_ATTACH:
			break;
		case DLL_PROCESS_DETACH:
			if (lpvReserved == NULL)
			{
				//FREE RESOURCES
			}
			else
			{

			}
			break;
		case DLL_THREAD_ATTACH:
		case DLL_THREAD_DETACH:
			break;
	}

	return TRUE;
}

extern "C" __declspec (dllexport) bool example()
{
	MessageBoxA(NULL, "OK", "test8", NULL);
	return true;
}

#include <iostream>
#include <string>
#include <windows.h>

extern "C" __declspec (dllexport)  void start()
{
	std::wcout << "Connecting to pipe..." << std::endl;

	HANDLE pipe = CreateFile(
		L"\\\\.\\pipe\\my_pipe",
		GENERIC_READ, // only need read access
		FILE_SHARE_READ | FILE_SHARE_WRITE,
		NULL,
		OPEN_EXISTING,
		FILE_ATTRIBUTE_NORMAL,
		NULL
	);

	wchar_t buffer[128];
	DWORD numBytesRead = 0;
	BOOL result = ReadFile(
		pipe,
		buffer, // the data from the pipe will be put here
		127 * sizeof(wchar_t), // number of bytes allocated
		&numBytesRead, // this will store number of bytes actually read
		NULL // not using overlapped IO
	);

	std::wcout << std::wstring(buffer) << std::endl;

	CloseHandle(pipe);

	std::cout << "close";
}
