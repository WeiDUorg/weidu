#include "caml/mlvalues.h"
#include "caml/alloc.h"

#include <stdlib.h>
#include <stdio.h>
#include <pwd.h>
#include <unistd.h>

CAMLprim value get_user_home_dir(void)
{
  const char* homeDir;
  struct passwd* pwd = getpwuid(getuid());
  if (pwd) {
    homeDir = pwd->pw_dir;
  }

  return copy_string(homeDir);
}
