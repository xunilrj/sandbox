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
	MessageBoxA(NULL, "OK", "test4", NULL);
	return true;
}
