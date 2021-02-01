//----------------------------------------------------------------------------
// Module to do recursive directory file matching under windows.
//
// Tries to do pattern matching to produce similar results as Unix, but
// using the Windows _findfirst to do all the pattern matching.
//
// Also hadles recursive directories - "**" path component expands into any
// levels of subdirectores (ie c:\**\*.c matches ALL .c files on drive c:)
// 
// Matthias Wandel Nov 5 2000
//----------------------------------------------------------------------------
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <ctype.h>
#include <io.h>

#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/callback.h>


#define TRUE 1
#define FALSE 0

typedef struct
{
  char *Name;
  int attrib;
}
FileEntry;


//------------------------------------------------------------------------------
// Simple path splicing (assumes no '\' in either part)
//------------------------------------------------------------------------------
static void
CatPath (char *dest, const char *p1, const char *p2)
{
  int l;
  l = strlen (p1);
  if (!l)
    {
      strcpy (dest, p2);
    }
  else
    {
      if (l + strlen (p2) > 200)
	{
	  fprintf (stderr, "Path too long\n");
	  exit (-1);
	}
      memcpy (dest, p1, l + 1);
      if (dest[l - 1] != '\\' && dest[l - 1] != ':')
	{
	  dest[l++] = '\\';
	}
      strcpy (dest + l, p2);
    }
}

//------------------------------------------------------------------------------
// Qsort compare function
//------------------------------------------------------------------------------
int
CompareFunc (const void *f1, const void *f2)
{
  return strcmp (((FileEntry *) f1)->Name, ((FileEntry *) f2)->Name);
}

//----------------------------------------------------------------------------
// Decide how a particular pattern should be handled, and call function for
// each.
//----------------------------------------------------------------------------
void
MyGlob (const char *Pattern, value FileFuncParm)
{
  char BasePattern[_MAX_PATH];
  char MatchPattern[_MAX_PATH];
  char PatCopy[_MAX_PATH * 2];

  int a;

  int MatchFiles, MatchDirs;
  int BaseEnd, PatternEnd;
  int SawPat;
  int RecurseAt;

  CAMLparam0();
  CAMLlocal1(v_string); 

  strcpy (PatCopy, Pattern);

DoRecursion:
  MatchFiles = FALSE;
  MatchDirs = TRUE;
  BaseEnd = 0;
  PatternEnd = 0;

  SawPat = FALSE;
  RecurseAt = -1;

  // Split the path into base path and pattern to match agains using findfirst.
  for (a = 0;; a++)
    {
      if (PatCopy[a] == '*' || PatCopy[a] == '?')
	{
	  SawPat = TRUE;
	}

      if (PatCopy[a] == '*' && PatCopy[a + 1] == '*')
	{
	  if (a == 0 || PatCopy[a - 1] == '\\' || PatCopy[a - 1] == ':')
	    {
	      if (PatCopy[a + 2] == '\\' || PatCopy[a + 2] == '\0')
		{
		  // x\**\y --> x\y x\*\**\y
		  RecurseAt = a;
		  if (PatCopy[a + 2])
		    {
		      memcpy (PatCopy + a, PatCopy + a + 3,
			      strlen (PatCopy) - a - 1);
		    }
		  else
		    {
		      PatCopy[a + 1] = '\0';
		    }
		}
	    }
	}

      if (PatCopy[a] == '\\' || (PatCopy[a] == ':' && PatCopy[a + 1] != '\\'))
	{
	  PatternEnd = a;
	  if (SawPat)
	    break;		// Findfirst can only match one level of wildcard at a time.
	  BaseEnd = a + 1;
	}
      if (PatCopy[a] == '\0')
	{
	  PatternEnd = a;
	  MatchFiles = TRUE;
	  MatchDirs = FALSE;
	  break;
	}
    }

  if (!SawPat)
    {
      // No pattern.  This should refer to a file.
      v_string = copy_string(PatCopy);
      callback(FileFuncParm,v_string);
      return;
    }

  strncpy (BasePattern, PatCopy, BaseEnd);
  BasePattern[BaseEnd] = 0;

  strncpy (MatchPattern, PatCopy, PatternEnd);
  MatchPattern[PatternEnd] = 0;

  {
    FileEntry *FileList = NULL;
    int NumAllocated = 0;
    int NumHave = 0;

    struct _finddata_t finddata;
    long find_handle;

    find_handle = _findfirst (MatchPattern, &finddata);

    for (;;)
      {
	if (find_handle == -1)
	  break;

	// Eliminate the obvious patterns.
	if (!memcmp (finddata.name, ".", 2))
	  goto next_file;
	if (!memcmp (finddata.name, "..", 3))
	  goto next_file;

	if (finddata.attrib & _A_SUBDIR)
	  {
	    if (!MatchDirs)
	      goto next_file;
	  }
	else
	  {
	    if (!MatchFiles)
	      goto next_file;
	  }

	// Add it to the list.
	if (NumAllocated <= NumHave)
	  {
	    NumAllocated = NumAllocated + 10 + NumAllocated / 2;
	    FileList = realloc (FileList, NumAllocated * sizeof (FileEntry));
	    if (FileList == NULL)
	      goto nomem;
	  }
	a = strlen (finddata.name);
	FileList[NumHave].Name = malloc (a + 1);
	if (FileList[NumHave].Name == NULL)
	  {
	  nomem:
	    printf ("malloc failure\n");
	    exit (-1);
	  }
	memcpy (FileList[NumHave].Name, finddata.name, a + 1);
	FileList[NumHave].attrib = finddata.attrib;
	NumHave++;

      next_file:
	if (_findnext (find_handle, &finddata) != 0)
	  break;
      }
    _findclose (find_handle);

    // Sort the list...
    qsort (FileList, NumHave, sizeof (FileEntry), CompareFunc);


    // Use the list.
    for (a = 0; a < NumHave; a++)
      {
	char CombinedName[_MAX_PATH * 2];
	if (FileList[a].attrib & _A_SUBDIR)
	  {
	    if (MatchDirs)
	      {
		// Need more directories.
		CatPath (CombinedName, BasePattern, FileList[a].Name);
		strcat (CombinedName, PatCopy + PatternEnd);
		MyGlob (CombinedName, FileFuncParm);
	      }
	  }
	else
	  {
	    if (MatchFiles)
	      {
		// We need files at this level.
		CatPath (CombinedName, BasePattern, FileList[a].Name);
                v_string = copy_string(CombinedName);
                callback(FileFuncParm,v_string);
	      }
	  }
	free (FileList[a].Name);
      }
    free (FileList);
  }

  if (RecurseAt >= 0)
    {
      strcpy (MatchPattern, PatCopy + RecurseAt);
      PatCopy[RecurseAt] = 0;
      strcpy (PatCopy + RecurseAt, "*\\**\\");
      strcat (PatCopy, MatchPattern);

      // As this funciton context is no longer needed, we can just goto back
      // to the top of it to avoid adding another context on the stack.
      goto DoRecursion;
    }
}

value
myglob (value v_pat, value v_fun) 
{
  CAMLparam2(v_pat,v_fun);
  const char * pat = String_val(v_pat);
  MyGlob (pat, v_fun);
  CAMLreturn(Val_int(0)); 
} 
