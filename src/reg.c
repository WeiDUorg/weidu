// This file has been edited by Fredrik Lindgren, a.k.a. Wisp,
// starting from 18 December 2012 and WeiDU 231.06.

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

CAMLprim value get_user_personal_dir(void)
{
  HKEY hKey = 0;
  char unexpBuf[SIZE] = {'.', 0};
  DWORD unexpBufSize = sizeof(unexpBuf);
  DWORD type = REG_EXPAND_SZ;
  const char* subkey = "Software\\Microsoft\\Windows\\CurrentVersion\\Explorer\\User Shell Folders";
  char expBuf[SIZE] = {'.', 0};
  DWORD expBufSize = sizeof(expBuf);

  RegOpenKeyEx(HKEY_CURRENT_USER, subkey, 0, KEY_QUERY_VALUE, &hKey);
  RegQueryValueExA(hKey, "Personal", 0, &type, (BYTE*)unexpBuf, &unexpBufSize);
  ExpandEnvironmentStrings(&unexpBuf, (BYTE*)expBuf, &expBufSize);
  RegCloseKey(hKey);

  return copy_string(expBuf);
}
