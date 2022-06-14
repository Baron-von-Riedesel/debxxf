
// helper dll for deb16fw if running on a 32-bit windows system.

#define WIN32_LEAN_AND_MEAN

#include <windows.h>
#include "wownt32.h"
#include "tlhelp32.h"

#define ID_DEBUGENTRY 0x104

extern "C" int __stdcall _DllMainCRTStartup(UINT, UINT, UINT);

HWND hwndDebugger = 0;
BOOL bConsoleAlloced = 0;

HANDLE hChildStdoutRead = 0;    // read from STDOUT of child
HANDLE hChildStdinWrite = 0;    // write to STDIN of child

STARTUPINFO sinfo;
PROCESS_INFORMATION procinfo;

typedef int (__stdcall * LPFNMESSAGEBOX)(HANDLE, char *, char *, int);

typedef HANDLE (__stdcall * LPFNCREATETOOLHELP32SNAPSHOT)(int, int);
typedef int (__stdcall * LPFNMODULE32FIRST)(HANDLE, MODULEENTRY32 *);
typedef int (__stdcall * LPFNMODULE32NEXT)(HANDLE, MODULEENTRY32 *);
typedef int (__stdcall * LPFNPROCESS32FIRST)(HANDLE, PROCESSENTRY32 *);
typedef int (__stdcall * LPFNPROCESS32NEXT)(HANDLE, PROCESSENTRY32 *);

LPFNCREATETOOLHELP32SNAPSHOT _CreateToolhelp32Snapshot = 0;
LPFNMODULE32FIRST _Module32First = 0;
LPFNMODULE32NEXT _Module32Next = 0;
LPFNPROCESS32FIRST _Process32First = 0;
LPFNPROCESS32NEXT _Process32Next = 0;

char szName[] = {"deb16fwh.dll"};

#ifdef _DEBUG
#define tprintf(x) OutputDebugStringA(x)
#else
#define tprintf(x)
#endif

//////////////////////////////////////////////////////////////
//  ListModule
//////////////////////////////////////////////////////////////
int _stdcall ListModule(MODULEENTRY32 * pME)
{
HANDLE        handle;
MODULEENTRY32 me;
int           i;

    if (!_CreateToolhelp32Snapshot)
        return 0;

    i = pME->dwSize;

    if (!(handle = _CreateToolhelp32Snapshot(TH32CS_SNAPMODULE,i)))
        return 0;

    me.dwSize = sizeof(MODULEENTRY32);
    i = _Module32First(handle,&me);

    while(i) {
          CopyMemory(pME, &me, sizeof MODULEENTRY32);
          pME++;
          i = _Module32Next(handle,&me);
    }
    pME->dwSize = 0;
    CloseHandle(handle);

    return 1;
}
//////////////////////////////////////////////////////////////
//  ListProcess
//////////////////////////////////////////////////////////////
int _stdcall ListProcess(PROCESSENTRY32 * pPE)
{
HANDLE          handle;
PROCESSENTRY32  pe;
int             i;

    if (!_CreateToolhelp32Snapshot)
        return 0;

    if (!(handle = _CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS,0)))
        return 0;

    pe.dwSize = sizeof(PROCESSENTRY32);
    i = _Process32First(handle,&pe);

    while (i) {
       CopyMemory(pPE, &pe, sizeof PROCESSENTRY32);
       pPE++;
       i = _Process32Next(handle,&pe);
    }
    pPE->dwSize = 0;
    CloseHandle(handle);
    return 1;
}
//////////////////////////////////////////////////////////////
//  ProcessInfo
//////////////////////////////////////////////////////////////
int _stdcall ProcessInfo(DWORD * pStr)
{
DWORD   dwMin,dwMax;
DWORD   PID;
int     rc = 0;
BYTE *  dwAddress;
UINT    dwInfoSiz;
HANDLE  hProcess;
MEMORY_BASIC_INFORMATION *  pInfoBuf;

    PID       = *pStr;
    if (!PID) {
        PID = GetCurrentProcessId();
        *pStr = PID;
    }
    dwAddress = (BYTE *)*(pStr+1);
    pInfoBuf  = (MEMORY_BASIC_INFORMATION *)*(pStr+2);
    hProcess  = (HANDLE)*(pStr+3);
    dwInfoSiz = sizeof(MEMORY_BASIC_INFORMATION);
    if (!hProcess) {
        hProcess = OpenProcess(PROCESS_QUERY_INFORMATION | PROCESS_VM_READ,0,PID);
        *(HANDLE *)(pStr+3) = hProcess;
    }
    if (hProcess)
        if (pInfoBuf)
            rc = VirtualQueryEx(hProcess,dwAddress,pInfoBuf,dwInfoSiz);
        else
            rc = CloseHandle(hProcess);
    return rc;
}

#if 0
//////////////////////////////////////////////////////////////
//  WriteMemory
//////////////////////////////////////////////////////////////
int _stdcall WriteMemory(BYTE * pMem, DWORD dwValue)
{
    DWORD dwOldProtect;
    if (VirtualProtect(pMem, 1, PAGE_READWRITE, &dwOldProtect)) {
        *pMem = (BYTE)dwValue;
        VirtualProtect(pMem, 1, dwOldProtect, &dwOldProtect);
        return 1;
    }
    return 0;
}
#endif
int DisplayMessage(char * pMsg)
{
    LPFNMESSAGEBOX _MessageBox;
    if (_MessageBox = (LPFNMESSAGEBOX)GetProcAddress(GetModuleHandle("USER32"),"MessageBoxA"))
        _MessageBox(0, pMsg, szName, MB_OK);
    return 1;
}

/////////////////////////////////////////////////////////////
// the console functions below will only work on NT platforms.
// on Win9x, no console would be attached if CreateProcess()
// is called from a win16 program.
/////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////
// init a child process which will do the console handling
/////////////////////////////////////////////////////////////
int InitChildProcess()
{
    HANDLE hConOut;
    HANDLE hConInp;
    HANDLE hChildStdoutReadTmp;
    HANDLE hChildStdinWriteTmp;
    HANDLE hChildStdoutWrite;   // this will be STDOUT for child
    HANDLE hChildStdinRead;     // this will be STDIN for child
    SECURITY_ATTRIBUTES sa;
    OSVERSIONINFO osvi;
    LPSTR pszFilePart;
    char szPath[MAX_PATH];


    int i;

    hConOut = GetStdHandle(STD_OUTPUT_HANDLE);
    hConInp = GetStdHandle(STD_INPUT_HANDLE);

    sa.nLength = sizeof(SECURITY_ATTRIBUTES);
    sa.bInheritHandle = TRUE;
    sa.lpSecurityDescriptor = NULL;

    tprintf("calling CreatePipe for stdout\r\n");
    i = CreatePipe(&hChildStdoutReadTmp, &hChildStdoutWrite, &sa, 0);
    if (!i) {
        DisplayMessage("CreatePipe() for child stdout failed");
        return 0;
    }

    SetStdHandle(STD_OUTPUT_HANDLE, hChildStdoutWrite);
    tprintf("calling DuplicateHandle\r\n");
    i = DuplicateHandle(GetCurrentProcess(), hChildStdoutReadTmp,
                    GetCurrentProcess(), &hChildStdoutRead,
                        0, FALSE, DUPLICATE_SAME_ACCESS);
    if (!i) {
        DisplayMessage("DuplicateHandle() failed");
        return 0;
    }

    CloseHandle(hChildStdoutReadTmp);

    tprintf("calling CreatePipe for stdin\r\n");
    i = CreatePipe(&hChildStdinRead, &hChildStdinWriteTmp, &sa, 0);
    if (!i) {
        DisplayMessage("CreatePipe() for child stdin failed");
        return 0;
    }

    SetStdHandle(STD_INPUT_HANDLE, hChildStdinRead);
    tprintf("calling DuplicateHandle\r\n");
    i = DuplicateHandle(GetCurrentProcess(), hChildStdinWriteTmp,
                    GetCurrentProcess(), &hChildStdinWrite,
                    0, FALSE, DUPLICATE_SAME_ACCESS);
    if (!i) {
        DisplayMessage("DuplicateHandle() failed");
        return 0;
    }
    CloseHandle(hChildStdinWriteTmp);

    i = SearchPath(NULL, "deb16fwp.exe", NULL, sizeof(szPath), szPath, &pszFilePart);
    if (i) {
        osvi.dwOSVersionInfoSize = sizeof( osvi );
        GetVersionEx(&osvi);
        ZeroMemory(&sinfo, sizeof(sinfo));
        sinfo.cb = sizeof(STARTUPINFO);
        tprintf("calling CreateProcess(deb16fwp)\r\n");
        //i = CreateProcess(szPath, "", 0, 0, 1, CREATE_NEW_CONSOLE, 0, 0, &sinfo, &procinfo);
        i = CreateProcess(szPath, "", 0, 0, 1, 0, 0, 0,&sinfo, &procinfo);
    }
    if (!i) {
        DisplayMessage("cannot run deb16fwp.exe");
        return 0;
    }

    SetStdHandle(STD_INPUT_HANDLE, hConInp);
    SetStdHandle(STD_OUTPUT_HANDLE, hConOut);

    CloseHandle(hChildStdoutWrite);
    CloseHandle(hChildStdinRead);


    return 1;

}
//////////////////////////////////////////////////////////////
//  WriteDbgConsole
//////////////////////////////////////////////////////////////
int _stdcall WriteDbgConsole(DWORD dwChar)
{
    DWORD dwWritten;
    int i;
    char szStr[40];

    if (!hChildStdinWrite) {
        if (!InitChildProcess())
            return 0;
    }

    WriteFile(hChildStdinWrite, &dwChar, 1, &dwWritten, 0);
    return dwWritten;
}
//////////////////////////////////////////////////////////////
//  ReadDbgConsole
//////////////////////////////////////////////////////////////
int _stdcall ReadDbgConsole(DWORD dwDmy)
{
    DWORD dwRead;
    DWORD dwTmp;


    if (ReadFile(hChildStdoutRead, &dwTmp, 4, &dwRead, 0))
        return dwTmp;
    else
        return 0;
}
//////////////////////////////////////////////////////////////
//  PeekDbgConsole
//////////////////////////////////////////////////////////////
int _stdcall PeekDbgConsole(DWORD dwDmy)
{
    DWORD dwRead;
    DWORD dwTmp;

    if ( dwDmy ) { Sleep( 0 ); return( 0 ); }
    if (PeekNamedPipe(hChildStdoutRead, &dwTmp, 4, &dwRead, NULL, NULL))
        if (dwRead == 4)
            return 1;
    return 0;
}
//////////////////////////////////////////////////////////////
//  CloseDbgConsole
//////////////////////////////////////////////////////////////
void _stdcall CloseDbgConsole(DWORD dwDmy)
{
    if (hChildStdinWrite)
        CloseHandle(hChildStdinWrite);
    if (hChildStdoutRead)
        CloseHandle(hChildStdoutRead);
}
//////////////////////////////////////////////////////////////
//  DLL Entry Point
//////////////////////////////////////////////////////////////
int _stdcall DllMain(UINT handle,UINT reason,UINT reserved)
{
    HINSTANCE hKernel;

    if (reason == DLL_PROCESS_ATTACH) {
        if (hKernel = GetModuleHandle("KERNEL32")) {
            _CreateToolhelp32Snapshot = (LPFNCREATETOOLHELP32SNAPSHOT)
                GetProcAddress(hKernel,"CreateToolhelp32Snapshot");
            _Module32First = (LPFNMODULE32FIRST)
                GetProcAddress(hKernel,"Module32First");
            _Module32Next = (LPFNMODULE32NEXT)
                GetProcAddress(hKernel,"Module32Next");
            _Process32First = (LPFNPROCESS32FIRST)
                GetProcAddress(hKernel,"Process32First");
            _Process32Next = (LPFNPROCESS32NEXT)
                GetProcAddress(hKernel,"Process32Next");
        }
    }
    return 1;
}
int __stdcall _DllMainCRTStartup(UINT handle, UINT reason, UINT reserved)
{
	return DllMain(handle,reason,reserved);
}
