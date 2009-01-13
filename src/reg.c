#define WIN32_LEAN_AND_MEAN     1
/* MinGW has a bug in windows.h */
#ifndef __MINGW32__
#define NOGDI
#endif
/* Includes for CygWin GCC: */
#ifdef __CYGWIN__
#include <w32api/windows.h>
#else
#include <windows.h>
#endif

#include "caml/mlvalues.h"
#include "caml/alloc.h"

#define SIZE    8192

CAMLprim value get_bg2main_path(void)
{
  int result;
  char buf[SIZE] = { '.', 0 } ;
#ifdef __CYGWIN__
  long size = SIZE; 
#else
  int size = SIZE; 
#endif

  result = RegQueryValueA(HKEY_LOCAL_MACHINE, "Software\\Microsoft\\Windows\\CurrentVersion\\App Paths\\BG2Main.Exe", buf, &size); 

  // "buf" now contains the path to BG2Main.exe

  return copy_string(buf); 
}

CAMLprim value get_bgmain_path(void)
{
  int result;
  char buf[SIZE] = { '.', 0 } ;
#ifdef __CYGWIN__
  long size = SIZE; 
#else
  int size = SIZE; 
#endif

  result = RegQueryValueA(HKEY_LOCAL_MACHINE, "Software\\Microsoft\\Windows\\CurrentVersion\\App Paths\\BGMain.Exe", buf, &size); 

  // "buf" now contains the path to BG2Main.exe

  return copy_string(buf); 
}

CAMLprim value get_iwdmain_path(void)
{
  int result;
  char buf[SIZE] = { '.', 0 } ;
#ifdef __CYGWIN__
  long size = SIZE; 
#else
  int size = SIZE; 
#endif

  result = RegQueryValueA(HKEY_LOCAL_MACHINE, "Software\\Microsoft\\Windows\\CurrentVersion\\App Paths\\IDMain.Exe", buf, &size); 

  // "buf" now contains the path to BG2Main.exe

  return copy_string(buf); 
}

CAMLprim value get_pstmain_path(void)
{
  int result;
  char buf[SIZE] = { '.', 0 } ;
#ifdef __CYGWIN__
  long size = SIZE; 
#else
  int size = SIZE; 
#endif

  result = RegQueryValueA(HKEY_LOCAL_MACHINE, "Software\\Microsoft\\Windows\\CurrentVersion\\App Paths\\Torment.Exe", buf, &size); 

  // "buf" now contains the path to BG2Main.exe

  return copy_string(buf); 
}

CAMLprim value get_iwd2main_path(void)
{
  int result;
  char buf[SIZE] = { '.', 0 } ;
#ifdef __CYGWIN__
  long size = SIZE; 
#else
  int size = SIZE; 
#endif

  result = RegQueryValueA(HKEY_LOCAL_MACHINE, "Software\\Microsoft\\Windows\\CurrentVersion\\App Paths\\IWD2.Exe", buf, &size); 

  // "buf" now contains the path to BG2Main.exe

  return copy_string(buf); 
}

CAMLprim value win_check_UAC(void)
{
		int isIt;
    HKEY hKey = 0;
    char buf[255] = {0};
    DWORD dwType = 0;
    DWORD dwBufSize = sizeof(buf);
    const char* subkey = "SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Policies\\System";
    RegOpenKey(HKEY_LOCAL_MACHINE,subkey,&hKey);
    dwType = REG_SZ;
    RegQueryValueEx(hKey,"EnableLUA",0, &dwType, (BYTE*)buf, &dwBufSize);
    RegCloseKey(hKey);
    return Val_bool(buf[0]);
}

// to compile:
// cl reg.c /link ADVAPI32.LIB
