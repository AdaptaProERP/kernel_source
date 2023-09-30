


KillProcessByName( "AdaptaPro.exe" )

************************************************************************************************
#pragma BEGINDUMP

#include <Windows.h>
#include <hbapi.h>
#include <string.h>
#include <tlhelp32.h>

void killProcessByName(const char *filename)
{
    HANDLE hSnapShot = CreateToolhelp32Snapshot(TH32CS_SNAPALL, NULL);
    PROCESSENTRY32 pEntry;
    BOOL hRes;

    pEntry.dwSize = sizeof (pEntry);
    hRes = Process32First(hSnapShot, &pEntry);

    while (hRes)
    {
        if (strcmp(pEntry.szExeFile, filename) == 0)
        {
            HANDLE hProcess = OpenProcess(PROCESS_TERMINATE, 0,
                                          (DWORD) pEntry.th32ProcessID);
            if (hProcess != NULL)
            {
                TerminateProcess(hProcess, 9);
                CloseHandle(hProcess);
            }
        }
        hRes = Process32Next(hSnapShot, &pEntry);
    }
    CloseHandle(hSnapShot);
}

HB_FUNC( KILLPROCESSBYNAME )
{
   killProcessByName( hb_parc( 1 ) );
}

#pragma ENDDUMP
******************************************************************************************************************+
