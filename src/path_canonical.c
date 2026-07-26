#include <stdlib.h>
#include <string.h>

#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN 1
#ifndef _WIN32_WINNT
#define _WIN32_WINNT 0x0600
#endif
#include <windows.h>
#endif

static value copy_empty_string(void)
{
  return caml_copy_string("");
}

CAMLprim value weidu_canonical_existing_path(value path_value)
{
  CAMLparam1(path_value);
  CAMLlocal1(result);

#ifdef _WIN32
  HANDLE handle;
  DWORD length;
  DWORD written;
  char *buffer;
  char *converted;
  const char *output;

  handle = CreateFileA(String_val(path_value), 0,
                       FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
                       NULL, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, NULL);
  if (handle == INVALID_HANDLE_VALUE)
    CAMLreturn(copy_empty_string());

  length = GetFinalPathNameByHandleA(
      handle, NULL, 0, FILE_NAME_NORMALIZED | VOLUME_NAME_DOS);
  if (length == 0) {
    CloseHandle(handle);
    CAMLreturn(copy_empty_string());
  }

  buffer = (char *) malloc((size_t) length + 1);
  if (buffer == NULL) {
    CloseHandle(handle);
    caml_raise_out_of_memory();
  }

  written = GetFinalPathNameByHandleA(
      handle, buffer, length + 1, FILE_NAME_NORMALIZED | VOLUME_NAME_DOS);
  CloseHandle(handle);
  if (written == 0 || written > length) {
    free(buffer);
    CAMLreturn(copy_empty_string());
  }

  converted = NULL;
  output = buffer;
  if (strncmp(buffer, "\\\\?\\UNC\\", 8) == 0) {
    size_t tail_length = strlen(buffer + 8);
    converted = (char *) malloc(tail_length + 3);
    if (converted == NULL) {
      free(buffer);
      caml_raise_out_of_memory();
    }
    converted[0] = '\\';
    converted[1] = '\\';
    memcpy(converted + 2, buffer + 8, tail_length + 1);
    output = converted;
  } else if (strncmp(buffer, "\\\\?\\", 4) == 0) {
    output = buffer + 4;
  }

  result = caml_copy_string(output);
  free(converted);
  free(buffer);
  CAMLreturn(result);
#else
  char *resolved = realpath(String_val(path_value), NULL);
  if (resolved == NULL)
    CAMLreturn(copy_empty_string());
  result = caml_copy_string(resolved);
  free(resolved);
  CAMLreturn(result);
#endif
}
