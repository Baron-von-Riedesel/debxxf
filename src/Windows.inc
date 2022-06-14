;*************************************************************************
;
;       WINDOWS.INC - Windows assembly language structures & constants
;
;*************************************************************************
;
; Conditional Block includes:   (True states)
;     NOTEXT - don't include TextMetric struc & text drawing modes & stock objs.
;     NORASTOPS - don't include binary and ternary raster ops.
;     NOVK      - don't include virtual key definitions
;     NOMB      - don't include message box definitions
;     NOWM      - don't include window messages
;
;
FALSE   =       0
TRUE    =       1
NULL    =       0

;*******************************************************************
;
;       Rectangle
;
;*******************************************************************

ifndef RECT
RECT    struc
        rcLeft          dw      ?
        rcTop           dw      ?
        rcRight         dw      ?
        rcBottom        dw      ?
RECT    ends
endif
;*******************************************************************
;
;  Window Class structure
;
;*******************************************************************

WNDCLASS struc
        style        dw      ?       ; class style
        lpfnWndProc  dd      ?
        cbClsExtra   dw      ?
        cbWndExtra   dw      ?
        hInstance    dw      ?       ; instance handle
        hIcon        dw      ?       ; class icon handle
        hCursor      dw      ?       ; class cursor handle
        hbrBackground dw     ?       ; class background brush
        lpszMenuName dd      ?       ; menu name
        lpszClassName dd     ?       ; far ptr to class name
WNDCLASS ends


;
;  Predefined cursor & icon IDs
;
IDC_ARROW       = 32512
IDC_IBEAM       = 32513
IDC_WAIT        = 32514
IDC_CROSS       = 32515
IDC_UPARROW     = 32516
IDC_SIZE        = 32640
IDC_ICON        = 32641
IDC_SIZENWSE    = 32642
IDC_SIZENESW    = 32643
IDC_SIZEWE      = 32644
IDC_SIZENS      = 32645

IDI_APPLICATION = 32512
IDI_HAND        = 32513
IDI_QUESTION    = 32514
IDI_EXCLAMATION = 32515
IDI_ASTERISK    = 32516

;
;  MessageBox type flags
;
IFNDEF                  NOMB
MB_OK                   = 0000H
MB_OKCANCEL             = 0001H
MB_ABORTRETRYIGNORE     = 0002H
MB_YESNOCANCEL          = 0003H
MB_YESNO                = 0004H
MB_RETRYCANCEL          = 0005H

MB_ICONHAND             = 0010H
MB_ICONQUESTION         = 0020H
MB_ICONEXCLAMATION      = 0030H
MB_ICONASTERISK         = 0040H

MB_DEFBUTTON1           = 0000H
MB_DEFBUTTON2           = 0100H
MB_DEFBUTTON3           = 0200H

MB_APPLMODAL            = 0000H
MB_SYSTEMMODAL          = 1000H
MB_TASKMODAL            = 2000H

MB_NOFOCUS              = 8000H

;
;  Conventional dialog box and message box command IDs
;
IDOK     =   1
IDCANCEL =   2
IDABORT  =   3
IDRETRY  =   4
IDIGNORE =   5
IDYES    =   6
IDNO     =   7

ENDIF

;
; Memory manager flags
;
LMEM_FIXED      =   0000h
LMEM_MOVEABLE   =   0002h
LMEM_NOCOMPACT  =   0010H
LMEM_NODISCARD  =   0020H
LMEM_ZEROINIT   =   0040h
LMEM_MODIFY     =   0080H
LMEM_DISCARDABLE=   0F00h
LHND    =    LMEM_MOVEABLE+LMEM_ZEROINIT
LPTR    =    LMEM_FIXED+LMEM_ZEROINIT
; Flags returned by LocalFlags (in addition to LMEM_DISCARDABLE)
LMEM_DISCARDED  =   4000H
LMEM_LOCKCOUNT  =   00FFH

GMEM_FIXED      =   0000h
GMEM_MOVEABLE   =   0002h
GMEM_NOCOMPACT  =   0010h
GMEM_NODISCARD  =   0020h
GMEM_ZEROINIT   =   0040h
GMEM_MODIFY     =   0080h
GMEM_DISCARDABLE=   0100h
GMEM_NOT_BANKED =   1000h
GMEM_DDESHARE   =   2000h
GMEM_SHARE      =   2000h
GMEM_NOTIFY     =   4000h
GMEM_LOWER      =   GMEM_NOT_BANKED
GHND            =   GMEM_MOVEABLE+GMEM_ZEROINIT
GPTR            =   GMEM_FIXED+GMEM_ZEROINIT

; Flags returned by GlobalFlags (in addition to GMEM_DISCARDABLE)
GMEM_DISCARDED  =    4000h
GMEM_LOCKCOUNT  =    00FFh


;  Virtual Keys, Standard Set

IFNDEF          NOVK
VK_LBUTTON      = 01H
VK_RBUTTON      = 02H
VK_CANCEL       = 03H
VK_BACK         = 08H
VK_TAB          = 09H
VK_CLEAR        = 0cH
VK_RETURN       = 0dH
VK_SHIFT        = 10H
VK_CONTROL      = 11H
VK_MENU         = 12H
VK_PAUSE        = 13H
VK_CAPITAL      = 14H
VK_ESCAPE       = 1bH
VK_SPACE        = 20H

VK_PRIOR        = 21H
VK_NEXT         = 22H
VK_END          = 23H
VK_HOME         = 24H
VK_LEFT         = 25H
VK_UP           = 26H
VK_RIGHT        = 27H
VK_DOWN         = 28H

;  VK_A thru VK_Z are the same as their ASCII equivalents: 'A' thru 'Z'
;  VK_0 thru VK_9 are the same as their ASCII equivalents: '0' thru '0'

VK_PRINT        = 2aH
VK_EXECUTE      = 2bH
VK_SNAPSHOT     = 2ch   ; Printscreen key..
VK_INSERT       = 2dH
VK_DELETE       = 2eH
VK_HELP         = 2fH

VK_NUMPAD0      = 60H
VK_NUMPAD1      = 61H
VK_NUMPAD2      = 62H
VK_NUMPAD3      = 63H
VK_NUMPAD4      = 64H
VK_NUMPAD5      = 65H
VK_NUMPAD6      = 66H
VK_NUMPAD7      = 67H
VK_NUMPAD8      = 68H
VK_NUMPAD9      = 69H
VK_MULTIPLY     = 6AH
VK_ADD          = 6BH
VK_SEPARATER    = 6CH
VK_SUBTRACT     = 6DH
VK_DECIMAL      = 6EH
VK_DIVIDE       = 6FH

VK_F1           = 70H
VK_F2           = 71H
VK_F3           = 72H
VK_F4           = 73H
VK_F5           = 74H
VK_F6           = 75H
VK_F7           = 76H
VK_F8           = 77H
VK_F9           = 78H
VK_F10          = 79H
VK_F11          = 7aH
VK_F12          = 7bH
VK_F13          = 7cH
VK_F14          = 7dH
VK_F15          = 7eH
VK_F16          = 7fH
VK_F17          = 80H
VK_F18          = 81H
VK_F19          = 82H
VK_F20          = 83H
VK_F21          = 84H
VK_F22          = 85H
VK_F23          = 86H
VK_F24          = 87H

VK_NUMLOCK      = 90H
VK_SCROLL       = 91H
ENDIF

;--- values returned by GetWinFlags()
WF_PMODE        =    0001h
WF_CPU286       =    0002h
WF_CPU386       =    0004h
WF_CPU486       =    0008h
WF_STANDARD     =    0010h
WF_WIN286       =    0010h
WF_ENHANCED     =    0020h
WF_WIN386       =    0020h
WF_CPU086       =    0040h
WF_CPU186       =    0080h
WF_LARGEFRAME   =    0100h
WF_SMALLFRAME   =    0200h
WF_80x87        =    0400h
WF_PAGING       =    0800h
WF_WLO          =    8000h


; Window field offsets for GetWindowLong() and GetWindowWord()
GWL_WNDPROC       =  (-4)
GWW_HINSTANCE     =  (-6)
GWW_HWNDPARENT    =  (-8)
GWW_ID            =  (-12)
GWL_STYLE         =  (-16)
GWL_EXSTYLE       =  (-20)

; Class field offsets for GetClassLong() and GetClassWord()
GCL_MENUNAME      =  (-8)
GCW_HBRBACKGROUND =  (-10)
GCW_HCURSOR       =  (-12)
GCW_HICON         =  (-14)
GCW_HMODULE       =  (-16)
GCW_CBWNDEXTRA    =  (-18)
GCW_CBCLSEXTRA    =  (-20)
GCL_WNDPROC       =  (-24)
GCW_STYLE         =  (-26)

; WinWhere() Area Codes
HTERROR           =  (-2)
HTTRANSPARENT     =  (-1)
HTNOWHERE         =  0
HTCLIENT          =  1
HTCAPTION         =  2
HTSYSMENU         =  3
HTGROWBOX         =  4
HTSIZE            =  HTGROWBOX
HTMENU            =  5
HTHSCROLL         =  6
HTVSCROLL         =  7
HTREDUCE          =  8
HTZOOM            =  9
HTLEFT            =  10
HTRIGHT           =  11
HTTOP             =  12
HTTOPLEFT         =  13
HTTOPRIGHT        =  14
HTBOTTOM          =  15
HTBOTTOMLEFT      =  16
HTBOTTOMRIGHT     =  17
HTSIZEFIRST       =  HTLEFT
HTSIZELAST        =  HTBOTTOMRIGHT



;*************************************************************************
;
;       Misc structures & constants
;
;*************************************************************************

IFNDEF  NOMST
ifndef POINT
POINT   struc
        ptX             dw      ?
        ptY             dw      ?
POINT   ends
endif

LOGPEN struc
    lopnStyle       dw ?
    lopnWidth       db (SIZE POINT) DUP(?)
    lopnColor       dd ?
LOGPEN ends


BITMAP STRUC
        bmType         DW ?
        bmWidth        DW ?
        bmHeight       DW ?
        bmWidthBytes   DW ?
        bmPlanes       DB ?
        bmBitsPixel    DB ?
        bmBits         DD ?
BITMAP ENDS

RGBTRIPLE       struc
        rgbBlue         db ?
        rgbGreen        db ?
        rgbRed          db ?
RGBTRIPLE       ends

RGBQUAD         struc
        rgbqBlue        db ?
        rgbqGreen       db ?
        rgbqRed         db ?
        rgbqReserved    db ?
RGBQUAD         ends

; structures for defining DIBs
BITMAPCOREHEADER struc
        bcSize      dd ?
        bcWidth     dw ?
        bcHeight    dw ?
        bcPlanes    dw ?
        bcBitCount  dw ?
BITMAPCOREHEADER ends

BITMAPINFOHEADER struc
        biSize           dd ?
        biWidth          dd ?
        biHeight         dd ?
        biPlanes         dw ?
        biBitCount       dw ?

        biCompression    dd ?
        biSizeImage      dd ?
        biXPelsPerMeter  dd ?
        biYPelsPerMeter  dd ?
        biClrUsed        dd ?
        biClrImportant   dd ?
BITMAPINFOHEADER ends

BITMAPINFO  struc
    bmiHeader   db (SIZE BITMAPINFOHEADER) DUP (?)
    bmiColors   db ?            ; array of RGBQUADs
BITMAPINFO  ends

BITMAPCOREINFO  struc
    bmciHeader  db (SIZE BITMAPCOREHEADER) DUP (?)
    bmciColors  db ?            ; array of RGBTRIPLEs
BITMAPCOREINFO  ends

BITMAPFILEHEADER struc
    bfType          dw ?
    bfSize          dd ?
    bfReserved1     dw ?
    bfReserved2     dw ?
    bfOffBits       dd ?
BITMAPFILEHEADER ends


WNDSTRUC struc
        WSwndStyle        dd      ?
        WSwndID           dw      ?
        WSwndText         dw      ?
        WSwndParent       dw      ?
        WSwndInstance     dw      ?
        WSwndClassProc    dd      ?
WNDSTRUC ends
;
;  Message structure
;
MSGSTRUCT       struc
hwnd            dw      ?
message         dw      ?
wParam          dw      ?
lParam          dd      ?
time            dd      ?
pt              dd      ?
MSGSTRUCT       ends

NEWPARMS struc
        nprmHwnd        dw      ?
        nprmCmd         db      ?
NEWPARMS ends
ENDIF

PAINTSTRUCT STRUC
    hdc         DW ?
    fErase      DW ?
    rcPaint     DB size RECT dup(?)
    fRestore    DW ?
    fIncUpdate  DW ?
    rgbReserved DB 16 dup(?)
PAINTSTRUCT ENDS


CREATESTRUCT struc
    cs_lpCreateParams  dd ?
    cs_hInstance       dw ?
    cs_hMenu           dw ?
    cs_hwndParent      dw ?
    cs_cy              dw ?
    cs_cx              dw ?
    cs_y               dw ?
    cs_x               dw ?
    cs_style           dd ?
    cs_lpszName        dd ?
    cs_lpszClass       dd ?
    cs_dwExStyle       dd ?
CREATESTRUCT  ends
;
;       PostError constants
;
WARNING     = 0           ; command codes
MINOR_ERROR = 1
FATAL_ERROR = 2

IGNORE      = 0           ; response codes
RETRY       = 1
ABORT       = 2


;
; Menu flags for Change/Check/Enable MenuItem
;
MF_INSERT       =   0000h
MF_CHANGE       =   0080h
MF_APPEND       =   0100h
MF_DELETE       =   0200h
MF_REMOVE       =   1000h

MF_BYCOMMAND    =   0000h
MF_BYPOSITION   =   0400h

MF_SEPARATOR    =   0800h

MF_ENABLED      =   0000h
MF_GRAYED       =   0001h
MF_DISABLED     =   0002h

MF_UNCHECKED    =   0000h
MF_CHECKED      =   0008h
MF_USECHECKBITMAPS= 0200h

MF_STRING       =   0000h
MF_BITMAP       =   0004h
MF_OWNERDRAW    =   0100h

MF_POPUP        =   0010h
MF_MENUBARBREAK =   0020h
MF_MENUBREAK    =   0040h

MF_UNHILITE     =   0000h
MF_HILITE       =   0080h

MF_SYSMENU      =   2000h
MF_HELP         =   4000h
MF_MOUSESELECT  =   8000h


;
;  System Menu Command Values
;
SC_SIZE        = 0F000h
SC_MOVE        = 0F010h
SC_MINIMIZE    = 0F020h
SC_MAXIMIZE    = 0F030h
SC_NEXTWINDOW  = 0F040h
SC_PREVWINDOW  = 0F050h
SC_CLOSE       = 0F060h
SC_VSCROLL     = 0F070h
SC_HSCROLL     = 0F080h
SC_MOUSEMENU   = 0F090h
SC_KEYMENU     = 0F100h
SC_ARRANGE     = 0F110h
SC_RESTORE     = 0F120h
SC_TASKLIST    = 0F130h
SC_SCREENSAVE  = 0F140h
SC_HOTKEY      = 0F150h

SC_ICON        = SC_MINIMIZE
SC_ZOOM        = SC_MAXIMIZE

;
;  Window State Messages
;
IFNDEF  NOWM
WM_STATE            = 0000H

WM_NULL             = 0000h
WM_CREATE           = 0001h
WM_DESTROY          = 0002h
WM_MOVE             = 0003h
WM_SIZE             = 0005h
WM_ACTIVATE         = 0006h
WM_SETFOCUS         = 0007h
WM_KILLFOCUS        = 0008h
WM_ENABLE           = 000Ah
WM_SETREDRAW        = 000Bh
WM_SETTEXT          = 000Ch
WM_GETTEXT          = 000Dh
WM_GETTEXTLENGTH    = 000Eh
WM_PAINT            = 000Fh
WM_CLOSE            = 0010h
WM_QUERYENDSESSION  = 0011h
WM_QUIT             = 0012h
WM_QUERYOPEN        = 0013h
WM_ERASEBKGND       = 0014h
WM_SYSCOLORCHANGE   = 0015h
WM_ENDSESSION       = 0016h
WM_SYSTEMERROR      = 0017h
WM_SHOWWINDOW       = 0018h
WM_CTLCOLOR         = 0019h
WM_WININICHANGE     = 001Ah
WM_DEVMODECHANGE    = 001Bh
WM_ACTIVATEAPP      = 001Ch
WM_FONTCHANGE       = 001Dh
WM_TIMECHANGE       = 001Eh
WM_CANCELMODE       = 001Fh
WM_SETCURSOR        = 0020h
WM_MOUSEACTIVATE    = 0021h
WM_CHILDACTIVATE    = 0022h
WM_QUEUESYNC        = 0023h
WM_GETMINMAXINFO    = 0024h
WM_PAINTICON        = 0026h
WM_ICONERASEBKGND   = 0027h
WM_NEXTDLGCTL       = 0028h
WM_SPOOLERSTATUS    = 002Ah
WM_DRAWITEM         = 002Bh
WM_MEASUREITEM      = 002Ch
WM_DELETEITEM       = 002Dh
WM_VKEYTOITEM       = 002Eh
WM_CHARTOITEM       = 002Fh
WM_SETFONT          = 0030h
WM_GETFONT          = 0031h
WM_QUERYDRAGICON    = 0037h
WM_COMPAREITEM      = 0039h
WM_COMPACTING       = 0041h
IFNDEF NOWIN31
WM_COMMNOTIFY       = 0044h
WM_WINDOWPOSCHANGING= 0046h
WM_WINDOWPOSCHANGED = 0047h
WM_POWER            = 0048h
ENDIF


WM_NCCREATE         = 0081h
WM_NCDESTROY        = 0082h
WM_NCCALCSIZE       = 0083h
WM_NCHITTEST        = 0084h
WM_NCPAINT          = 0085h
WM_NCACTIVATE       = 0086h
WM_GETDLGCODE       = 0087h
WM_NCMOUSEMOVE      = 00A0h
WM_NCLBUTTONDOWN    = 00A1h
WM_NCLBUTTONUP      = 00A2h
WM_NCLBUTTONDBLCLK  = 00A3h
WM_NCRBUTTONDOWN    = 00A4h
WM_NCRBUTTONUP      = 00A5h
WM_NCRBUTTONDBLCLK  = 00A6h
WM_NCMBUTTONDOWN    = 00A7h
WM_NCMBUTTONUP      = 00A8h
WM_NCMBUTTONDBLCLK  = 00A9h

WM_KEYFIRST         = 0100h
WM_KEYDOWN          = 0100h
WM_KEYUP            = 0101h
WM_CHAR             = 0102h
WM_DEADCHAR         = 0103h
WM_SYSKEYDOWN       = 0104h
WM_SYSKEYUP         = 0105h
WM_SYSCHAR          = 0106h
WM_SYSDEADCHAR      = 0107h
WM_KEYLAST          = 0108h

WM_INITDIALOG       = 0110h
WM_COMMAND          = 0111h
WM_SYSCOMMAND       = 0112h
WM_TIMER            = 0113h
WM_HSCROLL          = 0114h
WM_VSCROLL          = 0115h
WM_INITMENU         = 0116h
WM_INITMENUPOPUP    = 0117h
WM_MENUSELECT       = 011Fh
WM_MENUCHAR         = 0120h
WM_ENTERIDLE        = 0121h


WM_MOUSEFIRST       = 0200h
WM_MOUSEMOVE        = 0200h
WM_LBUTTONDOWN      = 0201h
WM_LBUTTONUP        = 0202h
WM_LBUTTONDBLCLK    = 0203h
WM_RBUTTONDOWN      = 0204h
WM_RBUTTONUP        = 0205h
WM_RBUTTONDBLCLK    = 0206h
WM_MBUTTONDOWN      = 0207h
WM_MBUTTONUP        = 0208h
WM_MBUTTONDBLCLK    = 0209h
WM_MOUSELAST        = 0209h

WM_PARENTNOTIFY     = 0210h
WM_MDICREATE        = 0220h
WM_MDIDESTROY       = 0221h
WM_MDIACTIVATE      = 0222h
WM_MDIRESTORE       = 0223h
WM_MDINEXT          = 0224h
WM_MDIMAXIMIZE      = 0225h
WM_MDITILE          = 0226h
WM_MDICASCADE       = 0227h
WM_MDIICONARRANGE   = 0228h
WM_MDIGETACTIVE     = 0229h
WM_MDISETMENU       = 0230h
WM_DROPFILES        = 0233h


WM_CUT              = 0300h
WM_COPY             = 0301h
WM_PASTE            = 0302h
WM_CLEAR            = 0303h
WM_UNDO             = 0304h
WM_RENDERFORMAT     = 0305h
WM_RENDERALLFORMATS = 0306h
WM_DESTROYCLIPBOARD = 0307h
WM_DRAWCLIPBOARD    = 0308h
WM_PAINTCLIPBOARD   = 0309h
WM_VSCROLLCLIPBOARD = 030Ah
WM_SIZECLIPBOARD    = 030Bh
WM_ASKCBFORMATNAME  = 030Ch
WM_CHANGECBCHAIN    = 030Dh
WM_HSCROLLCLIPBOARD = 030Eh
WM_QUERYNEWPALETTE  = 030Fh
WM_PALETTEISCHANGING = 0310h
WM_PALETTECHANGED   = 0311h

IFNDEF NOWIN31
WM_PENWINFIRST      equ 0380h
WM_PENWINLAST       equ 038Fh


WM_COALESCE_FIRST  equ 0390h
WM_COALESCE_LAST   equ 039Fh




ENDIF

;  private window messages start here

WM_USER             = 0400H
ENDIF           ; NOWM

; WM_MOUSEACTIVATE Return Codes
MA_ACTIVATE       =  1
MA_ACTIVATEANDEAT =  2
MA_NOACTIVATE     =  3

; Size message commands
SIZENORMAL       = 0
SIZEICONIC       = 1
SIZEFULLSCREEN   = 2
SIZEZOOMSHOW     = 3
SIZEZOOMHIDE     = 4

; ShowWindow() Commands
SW_HIDE            = 0
SW_SHOWNORMAL      = 1
SW_NORMAL          = 1
SW_SHOWMINIMIZED   = 2
SW_SHOWMAXIMIZED   = 3
SW_MAXIMIZE        = 3
SW_SHOWNOACTIVATE  = 4
SW_SHOW            = 5
SW_MINIMIZE        = 6
SW_SHOWMINNOACTIVE = 7
SW_SHOWNA          = 8
SW_RESTORE         = 9

; Old ShowWindow() Commands
HIDE_WINDOW        = 0
SHOW_OPENWINDOW    = 1
SHOW_ICONWINDOW    = 2
SHOW_FULLSCREEN    = 3
SHOW_OPENNOACTIVATE= 4

;  identifiers for the WM_SHOWWINDOW message
SW_PARENTCLOSING =  1
SW_OTHERZOOM     =  2
SW_PARENTOPENING =  3
SW_OTHERUNZOOM   =  4
;
; Key state masks for mouse messages
;
MK_LBUTTON       = 0001h
MK_RBUTTON       = 0002h
MK_SHIFT         = 0004h
MK_CONTROL       = 0008h
MK_MBUTTON       = 0010h
;
; Class styles
;
CS_VREDRAW         = 0001h
CS_HREDRAW         = 0002h
CS_KEYCVTWINDOW    = 0004H
CS_DBLCLKS         = 0008h
;                    0010h reserved
CS_OWNDC           = 0020h
CS_CLASSDC         = 0040h
CS_PARENTDC        = 0080h
CS_NOKEYCVT        = 0100h
CS_SAVEBITS        = 0800h
CS_NOCLOSE         = 0200h
CS_BYTEALIGNCLIENT = 1000h
CS_BYTEALIGNWINDOW = 2000h
CS_GLOBALCLASS     = 4000h    ; Global window class

;
; Special CreateWindow position value
;
CW_USEDEFAULT   EQU    8000h

;
; Windows styles (the high words)
;
WS_OVERLAPPED   = 00000h
WS_ICONICPOPUP  = 0C000h
WS_POPUP        = 08000h
WS_CHILD        = 04000h
WS_MINIMIZE     = 02000h
WS_VISIBLE      = 01000h
WS_DISABLED     = 00800h
WS_CLIPSIBLINGS = 00400h
WS_CLIPCHILDREN = 00200h
WS_MAXIMIZE     = 00100h
WS_CAPTION      = 000C0h     ; WS_BORDER | WS_DLGFRAME
WS_BORDER       = 00080h
WS_DLGFRAME     = 00040h
WS_VSCROLL      = 00020h
WS_HSCROLL      = 00010h
WS_SYSMENU      = 00008h
WS_THICKFRAME   = 00004h
WS_HREDRAW      = 00002h
WS_VREDRAW      = 00001h
WS_GROUP        = 00002h
WS_TABSTOP      = 00001h
WS_MINIMIZEBOX  = 00002h
WS_MAXIMIZEBOX  = 00001h

; Common Window Styles

WS_OVERLAPPEDWINDOW = WS_OVERLAPPED + WS_CAPTION + WS_SYSMENU + WS_THICKFRAME + WS_MINIMIZEBOX + WS_MAXIMIZEBOX
WS_POPUPWINDOW  = WS_POPUP + WS_BORDER + WS_SYSMENU
WS_CHILDWINDOW  = WS_CHILD
WS_TILEDWINDOW  = WS_OVERLAPPEDWINDOW

WS_TILED        = WS_OVERLAPPED
WS_ICONIC       = WS_MINIMIZE
WS_SIZEBOX      = WS_THICKFRAME

; Extended Window Styles (low words)
WS_EX_DLGMODALFRAME  = 0001
WS_EX_DRAGOBJECT     = 0002
WS_EX_NOPARENTNOTIFY = 0004
WS_EX_TOPMOST        = 0008

;  Predefined resource types
;
RT_CURSOR       = 1              ; must be passed through MAKEINTRESOURCE
RT_BITMAP       = 2
RT_ICON         = 3
RT_MENU         = 4
RT_DIALOG       = 5
RT_STRING       = 6
RT_FONTDIR      = 7
RT_FONT         = 8
RT_ACCELERATOR  = 9
RT_RCDATA       = 10

;** NOTE: if any new resource types are introduced above this point, then the
;** value of DIFFERENCE must be changed.
;** (RT_GROUP_CURSOR - RT_CURSOR) must always be equal to DIFFERENCE
;** (RT_GROUP_ICON - RT_ICON) must always be equal to DIFFERENCE

DIFFERENCE       =   11

RT_GROUP_CURSOR  =   RT_CURSOR + DIFFERENCE
RT_GROUP_ICON    =   RT_ICON + DIFFERENCE

COLOR_WINDOW equ 5

; NOMDI

; PeekMessage() Options
PM_NOREMOVE    = 0000h
PM_REMOVE      = 0001h
PM_NOYIELD     = 0002h


IFNDEF NOWINMESSAGES

; Button Control Messages
BM_GETCHECK        = (WM_USER+0)
BM_SETCHECK        = (WM_USER+1)
BM_GETSTATE        = (WM_USER+2)
BM_SETSTATE        = (WM_USER+3)
BM_SETSTYLE        = (WM_USER+4)

ENDIF
; NOWINMESSAGES

; Button Control Styles (low word)
BS_PUSHBUTTON      = 00h
BS_DEFPUSHBUTTON   = 01h
BS_CHECKBOX        = 02h
BS_AUTOCHECKBOX    = 03h
BS_RADIOBUTTON     = 04h
BS_3STATE          = 05h
BS_AUTO3STATE      = 06h
BS_GROUPBOX        = 07h
BS_USERBUTTON      = 08h
BS_AUTORADIOBUTTON = 09h
BS_OWNERDRAW       = 0Bh
BS_LEFTTEXT        = 20h

; User Button Notification Codes
BN_CLICKED         = 0
BN_PAINT           = 1
BN_HILITE          = 2
BN_UNHILITE        = 3
BN_DISABLE         = 4
BN_DOUBLECLICKED   = 5

; Dialog Styles (low words)
DS_ABSALIGN        = 01h
DS_SYSMODAL        = 02h
DS_LOCALEDIT       = 20h
DS_SETFONT         = 40h
DS_MODALFRAME      = 80h
DS_NOIDLEMSG       = 100h

IFNDEF NOWINMESSAGES

; Dialog box messages
DM_GETDEFID        = (WM_USER+0)
DM_SETDEFID        = (WM_USER+1)

ENDIF   ;NOWINMESSAGES

; Static Control styles (low word)
SS_LEFT            = 00h
SS_CENTER          = 01h
SS_RIGHT           = 02h
SS_ICON            = 03h
SS_BLACKRECT       = 04h
SS_GRAYRECT        = 05h
SS_WHITERECT       = 06h
SS_BLACKFRAME      = 07h
SS_GRAYFRAME       = 08h
SS_WHITEFRAME      = 09h
SS_SIMPLE          = 0Bh
SS_LEFTNOWORDWRAP  = 0Ch
SS_NOPREFIX        = 80h    ; Don't do "&" character translation

IFNDEF NOWIN31
IFNDEF NOWINMESSAGES

;Static Control Messages
STM_SETICON        = (WM_USER+0)
STM_GETICON        = (WM_USER+1)
ENDIF
ENDIF

; Scroll Bar Styles (low word)
SBS_HORZ                    = 0000h
SBS_VERT                    = 0001h
SBS_TOPALIGN                = 0002h
SBS_LEFTALIGN               = 0002h
SBS_BOTTOMALIGN             = 0004h
SBS_RIGHTALIGN              = 0004h
SBS_SIZEBOXTOPLEFTALIGN     = 0002h
SBS_SIZEBOXBOTTOMRIGHTALIGN = 0004h
SBS_SIZEBOX                 = 0008h

IFNDEF NOSYSMETRICS

; GetSystemMetrics() codes
SM_CXSCREEN           =  0
SM_CYSCREEN           =  1
SM_CXVSCROLL          =  2
SM_CYHSCROLL          =  3
SM_CYCAPTION          =  4
SM_CXBORDER           =  5
SM_CYBORDER           =  6
SM_CXDLGFRAME         =  7
SM_CYDLGFRAME         =  8
SM_CYVTHUMB           =  9
SM_CXHTHUMB           =  10
SM_CXICON             =  11
SM_CYICON             =  12
SM_CXCURSOR           =  13
SM_CYCURSOR           =  14
SM_CYMENU             =  15
SM_CXFULLSCREEN       =  16
SM_CYFULLSCREEN       =  17
SM_CYKANJIWINDOW      =  18
SM_MOUSEPRESENT       =  19
SM_CYVSCROLL          =  20
SM_CXHSCROLL          =  21
SM_DEBUG              =  22
SM_SWAPBUTTON         =  23
SM_RESERVED1          =  24
SM_RESERVED2          =  25
SM_RESERVED3          =  26
SM_RESERVED4          =  27
SM_CXMIN              =  28
SM_CYMIN              =  29
SM_CXSIZE             =  30
SM_CYSIZE             =  31
SM_CXFRAME            =  32
SM_CYFRAME            =  33
SM_CXMINTRACK         =  34
SM_CYMINTRACK         =  35
IFNDEF NOWIN31
SM_CXDOUBLECLK        =  36
SM_CYDOUBLECLK        =  37
SM_CXICONSPACING      =  38
SM_CYICONSPACING      =  39
SM_MENUDROPALIGNMENT  =  40
SM_PENWINDOWS         =  41
SM_DBCSENABLED        =  42
ENDIF
SM_CMETRICSMAX        =  43

ENDIF   ;NOSYSMETRICS

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
POINT struct
x     dw      ?
y     dw      ?
POINT ends
endif

ifndef RECT
RECT struct
left          dw      ?
top           dw      ?
right         dw      ?
bottom        dw      ?
RECT ends
endif

WINDOWPLACEMENT struct
length_       dw ?
flags         dw ?
showCmd       dw ?
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
lstrcat           proto far pascal :far ptr byte, :far ptr byte

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
PeekMessage       proto far pascal lpMsg:dword, hWnd:word, msgfrom:word, msgto:word, flags:word
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

SRCCOPY	equ 0CC0020h

;*** TOOLHELP prototypes ***

InterruptRegister   proto far pascal hTask:word,fpIntCallback:dword
InterruptUnRegister proto far pascal hTask:word

