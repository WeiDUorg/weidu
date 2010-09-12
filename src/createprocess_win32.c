/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*  Xavier Leroy and Pascal Cuoq, projet Cristal, INRIA Rocquencourt   */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

/* $Id: createprocess.c,v 1.13 2001/12/07 13:40:43 xleroy Exp $ */

#include <windows.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>

value weidu_win_create_process_native(value cmd, value cmdline, value env,
                                value fd1, value fd2, value fd3)
{
  PROCESS_INFORMATION pi;
  STARTUPINFO si;
  char * exefile, * envp;
  int flags;

  exefile = (String_val(cmd));
  if (env != Val_int(0)) {
    envp = String_val(Field(env, 0));
  } else {
    envp = NULL;
  }
  /* Prepare stdin/stdout/stderr redirection */
  GetStartupInfo(&si);
  si.dwFlags |= STARTF_USESTDHANDLES;
  si.hStdInput = Handle_val(fd1);
  si.hStdOutput = Handle_val(fd2);
  si.hStdError = Handle_val(fd3);

  // flags = CREATE_NEW_CONSOLE;
  flags = DETACHED_PROCESS;
  si.dwFlags = (STARTF_USESHOWWINDOW | STARTF_USESTDHANDLES);
  si.wShowWindow = SW_HIDE;

  /* Create the process */
  if (! CreateProcess(exefile, String_val(cmdline), NULL, NULL,
                      TRUE, flags, envp, NULL, &si, &pi)) {
    return Val_int(-1);
  }
  CloseHandle(pi.hThread);
  /* Return the process handle as pseudo-PID
     (this is consistent with the wait() emulation in the MSVC C library */
  return Val_int(pi.hProcess);
}

CAMLprim value weidu_win_create_process(value * argv, int argn)
{
  return weidu_win_create_process_native(argv[0], argv[1], argv[2],
                                   argv[3], argv[4], argv[5]);
}
