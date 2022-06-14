
    pushcontext listing
    .xlist
    .xcref

cr equ 13
lf equ 10

;*** prototypes of windows functions - 16 Bit only ***

UINT      typedef WORD
HTASK     typedef WORD
HWND      typedef WORD
HMENU     typedef WORD
HINSTANCE typedef WORD
HICON     typedef WORD
HCURSOR   typedef WORD
WPARAM    typedef WORD
HDC		  typedef WORD
LPARAM    typedef DWORD
LPSTR     typedef FAR PTR BYTE

ifndef POINT
POINT   struct
        x             dw      ?
        y             dw      ?
POINT   ends
endif

ifndef RECT
RECT    struct
        left          dw      ?
        top           dw      ?
        right         dw      ?
        bottom        dw      ?
RECT    ends
endif

WINDOWPLACEMENT struct
length_		dw ?
flags		dw ?
showCmd		dw ?
ptMinPosition POINT <>
ptMaxPosition POINT <>
rcNormalPosition RECT <>
WINDOWPLACEMENT ends

DLGWINDOWEXTRA	equ 30

;*********************************************************************

;*** Kernel functions ***

AllocCStoDSAlias  proto far pascal wSel:word
AllocDStoCSAlias  proto far pascal wSel:word
AllocSelector     proto far pascal parm:word
DebugBreak        proto far pascal
DisableKernel     proto far pascal
Dos3Call          proto far pascal
FatalExit         proto far pascal iExitCode:word
FatalExitHook     proto far pascal newproc:dword
FreeLibrary       proto far pascal hModule:word
FreeProcInstance  proto far pascal lpProc:dword
FreeSelector      proto far pascal selector:word
GetCurrentPDB     proto far pascal
GetCurrentTask    proto far pascal
GetDOSEnvironment proto far pascal
GetExePtr         proto far pascal :word
GetInstanceData   proto far pascal hInstance:WORD,pAddr:WORD,wCount:WORD
GetModuleHandle   proto far pascal lpModuleName:far ptr byte
GetModuleUsage    proto far pascal hModule:WORD
GetProcAddress    proto far pascal hInstance:word,lpProcName:far ptr byte
GetSelectorBase   proto far pascal selector:word
GetSelectorLimit  proto far pascal selector:word
GetSetKernelDosProc proto far pascal dwNewProc:dword
GetVersion        proto far pascal
GlobalAlloc       proto far pascal wFlags:word,dwBytes:dword
GlobalDosAlloc    proto far pascal dwBytes:dword
GlobalDosFree     proto far pascal wSel:word
GlobalFree        proto far pascal wHandle:word
GlobalLock        proto far pascal hGlobal:word
GlobalPageLock    proto far pascal hGlobal:word
GlobalPageUnlock  proto far pascal hGlobal:word
InitTask          proto far pascal
LoadLibrary       proto far pascal lpszName:far ptr byte
LocalAlloc        proto far pascal wType:word,cbSize:word
LocalFree         proto far pascal hLocal:dword
LockSegment       proto far pascal a1:word
MakeProcInstance  proto far pascal lpProc:dword,hInstance:word
NoHookDosCall     proto far pascal
OpenFile          proto far pascal lpFilename:far ptr byte,lpOFStruct:far ptr byte,Opentype:word
OutputDebugString proto far pascal lpString:far ptr byte
PostEvent         proto far pascal hTask:word
SetHandleCount    proto far pascal handles:word
SetSelectorBase   proto far pascal selector:word,base:dword
SetSelectorLimit  proto far pascal selector:word,limit:dword
UnlockSegment     proto far pascal a1:word
WaitEvent         proto far pascal hTask:word
Yield             proto far pascal
lstrcpy           proto far pascal lpStr1:dword,lpStr2:dword
GetPrivateProfileString proto far pascal :far ptr byte, :far ptr byte, :far ptr byte, :far ptr byte, :word, :far ptr byte
WritePrivateProfileString proto far pascal :far ptr byte, :far ptr byte, :far ptr byte, :far ptr byte
GetModuleFileName proto far pascal :word, :far ptr byte, :word
lstrcat			proto far pascal :far ptr byte, :far ptr byte

wsprintf          proto far c      lpBuffer:far ptr byte,lpFormat:far ptr byte,a3:VARARG

;--- KEYBOARD functions

GetBiosKeyProc	proto far pascal
EnableKBSysreq	proto far pascal :word

;*** USER functions ***

WNDPROC typedef proto far pascal hWnd:HWND,message:UINT,wParam:WPARAM,lParam:LPARAM
DLGPROC typedef proto far pascal hWnd:HWND,message:UINT,wParam:WPARAM,lParam:LPARAM

AppendMenu        proto far pascal :HMENU, :WORD, :WORD, :far ptr byte
BeginPaint        proto far pascal hWnd:word, ps:far ptr byte
CopyRect          proto far pascal lpRC1:far ptr RECT, lpRC2:far ptr RECT
CreateCaret       proto far pascal hWnd:word, hBitmap:word, cx_:word, cy:word
CreateDialog      proto far pascal hInstance:HINSTANCE,lpRES:far ptr byte,hWnd:HWND,fpDlgProc:far ptr DLGPROC
CreateWindow      proto far pascal lpszClass:far ptr byte,lpszName:far ptr byte,style:dword,x:word,y:word,_cx:word,cy:word,parent:word,menu:word,hInst:word,lParam:dword
DefDlgProc        proto far pascal hWnd:HWND,message:UINT,wParam:WPARAM,lParam:LPARAM
DefWindowProc     proto far pascal hWnd:HWND,message:UINT,wParam:WPARAM,lParam:LPARAM
DestroyWindow     proto far pascal hWnd:word
DialogBox         proto far pascal hInstance:HINSTANCE,lpRes:far ptr byte,hWnd:HWND,fpDlgProc:DLGPROC
DisableOEMLayer   proto far pascal
DispatchMessage   proto far pascal lpMsg:dword
DlgDirList        proto far pascal hDlg:word, lpszPath:far ptr byte, wID:word, wIDPath:word, wFType:word
DlgDirSelect      proto far pascal hDlg:word, lpszFilename:far ptr byte, wID:word
DrawIcon          proto far pascal :HDC, :word, :word, :HICON
EnableOEMLayer    proto far pascal
EndDialog         proto far pascal hWnd:HWND,wResult:WORD
EndPaint          proto far pascal hWnd:word,ps:far ptr byte
GetClientRect     proto far pascal hWnd:word,lpRect:far ptr byte
GetDC             proto far pascal hWnd:word
GetDesktopHwnd    proto far pascal
GetDesktopWindow  proto far pascal
GetDlgItem        proto far pascal hWnd:HWND,wID:UINT
GetDlgItemText    proto far pascal hWnd:HWND,wID:UINT,lpStr:far ptr byte,iLength:WORD
GetKeyState       proto far pascal wKey:word
GetMessage        proto far pascal lpMsg:dword, hWnd:word, msgfrom:word, msgto:word
GetMouseEventProc proto far pascal
GetSysColor       proto far pascal wIndex:word
GetSystemMenu     proto far pascal hWnd:word,bRevert:word
GetWindowPlacement proto far pascal :HWND, :ptr WINDOWPLACEMENT
HideCaret         proto far pascal hWnd:word
InitApp           proto far pascal hInstance:word
InvalidateRect    proto far pascal hWnd:word,lpRect:far ptr byte,bool:word
IsDialogMessage   proto far pascal hWnd:word,lpMsg:dword
IsIconic		  proto far pascal hWnd:word
LoadAccelerators  proto far pascal hInstance:word,lpRes:dword
LoadCursor        proto far pascal hInstance:word,lpRes:far ptr byte
LoadIcon          proto far pascal hInstance:word,lpRes:dword
LoadMenu          proto far pascal hInstance:word,pStr:far ptr byte
LoadString        proto far pascal hInstance:word,id:word,lpStr:far ptr byte,iLength:word
MessageBeep       proto far pascal wAlert:word
MessageBox        proto far pascal hWnd:word,x2:far ptr byte,x3:far ptr byte,x4:word
ModifyMenu        proto far pascal hMenu:word,nPos:word,wFlags:word,wIDNewItem:word,lpNewItem:far ptr byte
PostMessage       proto far pascal hWnd:word,message:word,wParam:word,lParam:dword
PostQuitMessage   proto far pascal parm:word
PostAppMessage    proto far pascal hTask:word,message:UINT,wParam:WPARAM,lParam:LPARAM
PrestoChangoSelector proto far pascal wSrcSel:WORD,wDestSel:WORD
RegisterClass     proto far pascal lpWndClass:far ptr byte
ReleaseDC         proto far pascal hWnd:word,hDC:word
ScrollWindow      proto far pascal hWnd:word,xpos:word,ypos:word,lpRect1:dword,lpRect2:dword
SendDlgItemMessage proto far pascal hWnd:HWND,wID:WORD,uMsg:UINT,wParam:WPARAM,lParam:LPARAM
SendMessage       proto far pascal hWnd:word,message:word,wParam:word,lParam:dword
SetCaretPos       proto far pascal x:word, y:word
SetClassWord      proto far pascal hWnd:word,id:word, wNewValue:word
SetCursor         proto far pascal hCursor:word
SetDlgItemText    proto far pascal hWnd:word, id:word, lpszText:far ptr byte
SetFocus          proto far pascal hWnd:word
SetScrollPos      proto far pascal hWnd:word,vonw:word,bisw:word,bool:word
SetScrollRange    proto far pascal hWnd:word,typ:word,vonw:word,bisw:word,bool:word
SetWindowPlacement proto far pascal :HWND, :ptr WINDOWPLACEMENT
SetWindowPos      proto far pascal hWnd:word, hWndA:word, x:word, y:word, cx_:word, cy:word, wFlags:word
SetWindowText     proto far pascal hWnd:word,lpText:far ptr byte
ShowCaret         proto far pascal hWnd:word
ShowWindow        proto far pascal hWnd:word,swoption:word
TranslateAccelerator proto far pascal hWnd:word,hAccTab:word,lpMsg:dword
TranslateMessage  proto far pascal lpMsg:dword
UpdateWindow      proto far pascal hWnd:word
ValidateRect      proto far pascal hWnd:word, lpRC:far ptr RECT

;*** GDI Prototypes ***

BitBlt              proto far pascal hDC:word, x:word, y:word, _dx:word, dy:word, :word, :word, :word, :dword
CreateDC            proto far pascal :far ptr, :far ptr, :far ptr, :far ptr
CreateCompatibleBitmap  proto far pascal hDC:word, :word, :word
CreateCompatibleDC  proto far pascal hDC:word
Death               proto far pascal hDC:word
DeleteDC            proto far pascal hDC:word
DeleteObject        proto far pascal hGDIObject:word
Ellipse             proto far pascal hDC:word, a1:word, a2:word, a3:word, a4:word
FillRect            proto far pascal hDC:word, lpRC:far ptr RECT, hBrush:word
GetDeviceCaps       proto far pascal hDC:word, :word
GetStockObject      proto far pascal resID:word
GetTextMetrics      proto far pascal hDC:word, lpTM:far ptr TEXTMETRIC
GetWindowOrg        proto far pascal hDC:word
PatBlt              proto far pascal hDC:word, :word, :word, :word, :word, :dword
SelectObject        proto far pascal hDC:word,hGDIObject:word
SetBkColor          proto far pascal hDC:word, dwColor:dword
SetMapMode          proto far pascal hDC:word, a1:word
SetTextColor        proto far pascal hDC:word, dwColor:dword
SetWindowOrg        proto far pascal hDC:word, X:word, Y:word
Rectangle           proto far pascal hDC:word, a1:word, a2:word, a3:word, a4:word
Resurrection        proto far pascal hDC:word, w1:word, w2:word, w3:word, w4:word, w5:word, w6:word
RoundRect           proto far pascal hDC:word, a1:word, a2:word, a3:word, a4:word, a5:word, a6:word
TextOut             proto far pascal hDC:word,x:word,y:word,lpStr:far ptr byte,iLength:word

SRCCOPY equ 0CC0020h

;*** SOUND ***

OpenSound   proto far pascal
CloseSound  proto far pascal
StartSound  proto far pascal
StopSound   proto far pascal

;*** TOOLHELP prototypes ***

InterruptRegister   proto far pascal hTask:word,fpIntCallback:dword
InterruptUnRegister proto far pascal hTask:word

;*** SHELL prototypes ***

FindExecutable proto far pascal lpszFile:far ptr byte,lpszDir:far ptr byte,lpszApp:far ptr byte

    popcontext listing
