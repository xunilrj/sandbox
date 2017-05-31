#include <windows.h>

#define SVCNAME TEXT("SvcName")
SERVICE_STATUS          gSvcStatus; 
SERVICE_STATUS_HANDLE   gSvcStatusHandle; 
HANDLE                  ghSvcStopEvent = NULL;

VOID ReportSvcStatus( DWORD dwCurrentState,
                      DWORD dwWin32ExitCode,
                      DWORD dwWaitHint)
{
    static DWORD dwCheckPoint = 1;

    // Fill in the SERVICE_STATUS structure.

    gSvcStatus.dwCurrentState = dwCurrentState;
    gSvcStatus.dwWin32ExitCode = dwWin32ExitCode;
    gSvcStatus.dwWaitHint = dwWaitHint;

    if (dwCurrentState == SERVICE_START_PENDING)
        gSvcStatus.dwControlsAccepted = 0;
    else gSvcStatus.dwControlsAccepted = SERVICE_ACCEPT_STOP;

    if ( (dwCurrentState == SERVICE_RUNNING) ||
           (dwCurrentState == SERVICE_STOPPED) )
        gSvcStatus.dwCheckPoint = 0;
    else gSvcStatus.dwCheckPoint = dwCheckPoint++;

    // Report the status of the service to the SCM.
    SetServiceStatus( gSvcStatusHandle, &gSvcStatus );
}

VOID WINAPI SvcCtrlHandler(DWORD dwCtrl)
{
   switch(dwCtrl) 
   {  
      case SERVICE_CONTROL_STOP: 
         ReportSvcStatus(SERVICE_STOP_PENDING, NO_ERROR, 0);
         SetEvent(ghSvcStopEvent);
         ReportSvcStatus(gSvcStatus.dwCurrentState, NO_ERROR, 0);
         return;
      case SERVICE_CONTROL_INTERROGATE: 
         break; 
      default: 
         break;
   } 
}

int main(int argc, char **argv)
{
    SERVICE_TABLE_ENTRY DispatchTable[] = 
    { 
        { SVCNAME, (LPSERVICE_MAIN_FUNCTION) [](DWORD dwArgc, LPTSTR *lpszArgv){
            ghSvcStopEvent = CreateEvent(NULL, TRUE, FALSE, NULL);
            gSvcStatusHandle = RegisterServiceCtrlHandler(SVCNAME, SvcCtrlHandler);
            gSvcStatus.dwServiceType = SERVICE_WIN32_OWN_PROCESS; 
            gSvcStatus.dwServiceSpecificExitCode = 0;    
            ReportSvcStatus( SERVICE_START_PENDING, NO_ERROR, 3000 );
            ReportSvcStatus( SERVICE_RUNNING, NO_ERROR, 0 );
            while(1)
            {
                WaitForSingleObject(ghSvcStopEvent, INFINITE);
                ReportSvcStatus( SERVICE_STOPPED, NO_ERROR, 0 );
                return;
            }
        }}, 
        { NULL, NULL } 
    }; 

    StartServiceCtrlDispatcher(DispatchTable);
    
    return 0;
}


