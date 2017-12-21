unit CE_Winapi_Windows;

interface
uses System.Types, System.UITypes;
implementation
type
{ Translated from WINDEF.H }

  WCHAR = WideChar;
  {$EXTERNALSYM WCHAR}
  PWChar = MarshaledString;

  LPSTR = MarshaledAString;
  {$EXTERNALSYM LPSTR}
  PLPSTR = ^LPSTR;
  {$EXTERNALSYM PLPSTR}
  LPCSTR = MarshaledAString;
  {$EXTERNALSYM LPCSTR}
  LPCTSTR = {$IFDEF UNICODE}MarshaledString{$ELSE}MarshaledAString{$ENDIF};
  {$EXTERNALSYM LPCTSTR}
  LPTSTR = {$IFDEF UNICODE}MarshaledString{$ELSE}MarshaledAString{$ENDIF};
  {$EXTERNALSYM LPTSTR}
  PLPCTSTR = {$IFDEF UNICODE}PMarshaledString{$ELSE}PMarshaledAString{$ENDIF};
  {$EXTERNALSYM LPCTSTR}
  PLPTSTR = {$IFDEF UNICODE}PMarshaledString{$ELSE}PMarshaledAString{$ENDIF};
  {$EXTERNALSYM LPTSTR}
  LPWSTR = PWideChar;
  {$EXTERNALSYM LPWSTR}
  PLPWSTR = ^LPWSTR;
  LPCWSTR = PWideChar;
  {$EXTERNALSYM LPCWSTR}

  DWORD = System.Types.DWORD;
  {$EXTERNALSYM DWORD}
  BOOL = LongBool;
  {$EXTERNALSYM BOOL}
  PBOOL = ^BOOL;
  {$EXTERNALSYM PBOOL}
  PByte = System.Types.PByte;
  LPBYTE = PByte;
  {$EXTERNALSYM LPBYTE}
  PINT = ^Integer;
  {$EXTERNALSYM PINT}
  PSingle = ^Single;
  PWORD = ^Word;
  {$EXTERNALSYM PWORD}
  PDWORD = ^DWORD;
  {$EXTERNALSYM PDWORD 'unsigned*'}
  LPDWORD = PDWORD;
  {$EXTERNALSYM LPDWORD}
  LPVOID = Pointer;
  {$EXTERNALSYM LPVOID}
  LPCVOID = Pointer;
  {$EXTERNALSYM LPCVOID}
  ULONG32 = LongWord;
  {$EXTERNALSYM ULONG32}
  LONG = Longint;
  {$EXTERNALSYM LONG}
  PVOID = Pointer;
  {$EXTERNALSYM PVOID}
  PPVOID = ^PVOID;
  {$EXTERNALSYM PPVOID}
  LONG64 = Int64;
  {$EXTERNALSYM LONG64}
  ULONG64 = UInt64;
  {$EXTERNALSYM ULONG64}
  DWORD64 = UInt64;
  {$EXTERNALSYM DWORD64}
  PLONG64 = ^LONG64;
  {$EXTERNALSYM PLONG64}
  PULONG64 = ^ULONG64;
  {$EXTERNALSYM PULONG64}
  PDWORD64 = ^DWORD64;
  {$EXTERNALSYM PDWORD64}

  UCHAR = Byte;
  {$EXTERNALSYM UCHAR}
  PUCHAR = ^Byte;
  {$EXTERNALSYM PUCHAR}
  SHORT = Smallint;
  {$EXTERNALSYM SHORT}
  USHORT = Word;
  {$EXTERNALSYM USHORT}
  PSHORT = System.PSmallint;
  {$EXTERNALSYM PSHORT}
  PUSHORT = System.PWord;
  {$EXTERNALSYM PUSHORT}
  UINT = LongWord;
  {$EXTERNALSYM UINT}
  PUINT = ^UINT;
  {$EXTERNALSYM PUINT}
  ULONG = Cardinal;
  {$EXTERNALSYM ULONG}
  PULONG = ^ULONG;
  {$EXTERNALSYM PULONG}
  PLongint = System.PLongint;
  {$EXTERNALSYM PLongint}
  PInteger = System.PInteger;
  {$EXTERNALSYM PInteger}
  PLongWord = System.PLongWord;
  {$EXTERNALSYM PLongWord}
  PSmallInt = System.PSmallInt;
  {$EXTERNALSYM PSmallInt}
  PDouble = System.PDouble;
  {$EXTERNALSYM PDouble}
  PShortInt = System.PShortInt;
  {$EXTERNALSYM PShortInt}

  LCID = DWORD;
  {$EXTERNALSYM LCID}
  LANGID = Word;
  {$EXTERNALSYM LANGID}

  THandle = System.THandle;
  PHandle = ^THandle;

  // From BaseTsd.h
  INT_PTR = System.IntPtr;    // NativeInt;
  {$EXTERNALSYM INT_PTR}
  UINT_PTR = System.UIntPtr;  // NativeUInt;
  {$EXTERNALSYM UINT_PTR}
  LONG_PTR = NativeInt;
  {$EXTERNALSYM LONG_PTR}
  ULONG_PTR = NativeUInt;
  {$EXTERNALSYM ULONG_PTR}
  DWORD_PTR = ULONG_PTR;
  {$EXTERNALSYM DWORD_PTR}
  HANDLE_PTR = type NativeUInt;
  {$EXTERNALSYM HANDLE_PTR}
  SIZE_T = ULONG_PTR;
  {$EXTERNALSYM SIZE_T}
  SSIZE_T = LONG_PTR;
  {$EXTERNALSYM SSIZE_T}

  PINT_PTR = ^INT_PTR;
  {$EXTERNALSYM PINT_PTR}
  PUINT_PTR = ^UINT_PTR;
  {$EXTERNALSYM PUINT_PTR}
  PLONG_PTR = ^LONG_PTR;
  {$EXTERNALSYM PLONG_PTR}
  PULONG_PTR = ^ULONG_PTR;
  {$EXTERNALSYM PULONG_PTR}
  PDWORD_PTR = ^DWORD_PTR;
  {$EXTERNALSYM PDWORD_PTR}
  PSIZE_T = ^SIZE_T;
  {$EXTERNALSYM PSIZE_T}
  PSSIZE_T = ^SSIZE_T;
  {$EXTERNALSYM PSSIZE_T}

const
  MAX_PATH = 260;
  {$EXTERNALSYM MAX_PATH}
  ANYSIZE_ARRAY = 1;
  {$EXTERNALSYM ANYSIZE_ARRAY}
  MAX_HW_COUNTERS = 16;
  {$EXTERNALSYM MAX_HW_COUNTERS}
  THREAD_PROFILING_FLAG_DISPATCH = $00000001;
  {$EXTERNALSYM THREAD_PROFILING_FLAG_DISPATCH}

{ Translated from WINNT.H (only things needed for API calls) }

{line 190}
type
  LONGLONG = Int64;
  {$EXTERNALSYM LONGLONG}
  PSID = Pointer;
  {$EXTERNALSYM PSID}
  PLargeInteger = ^TLargeInteger;
  _LARGE_INTEGER = record
    case Integer of
    0: (
      LowPart: DWORD;
      HighPart: Longint);
    1: (
      QuadPart: LONGLONG);
  end;
  {$EXTERNALSYM _LARGE_INTEGER}
  {$NODEFINE TLargeInteger}
  TLargeInteger = Int64;
  LARGE_INTEGER = _LARGE_INTEGER;
  {$EXTERNALSYM LARGE_INTEGER}

  DWORDLONG = UInt64;
  {$EXTERNALSYM DWORDLONG}
  ULONGLONG = UInt64;
  {$EXTERNALSYM ULONGLONG}
  PULONGLONG = ^UInt64;
  {$EXTERNALSYM PULONGLONG}
  PULargeInteger = ^TULargeInteger;
  _ULARGE_INTEGER = record
    case Integer of
    0: (
      LowPart: DWORD;
      HighPart: DWORD);
    1: (
      QuadPart: ULONGLONG);
  end;
  {$EXTERNALSYM _ULARGE_INTEGER}
  {$NODEFINE TULargeInteger}
  TULargeInteger = UInt64;
  ULARGE_INTEGER = _ULARGE_INTEGER;
  {$EXTERNALSYM ULARGE_INTEGER}

{line 450}
  PListEntry = ^TListEntry;
  _LIST_ENTRY = record
    Flink: PListEntry;
    Blink: PListEntry;
  end;
  {$EXTERNALSYM _LIST_ENTRY}
  TListEntry = _LIST_ENTRY;
  LIST_ENTRY = _LIST_ENTRY;
  {$EXTERNALSYM LIST_ENTRY}

{line 490}
const
  MINCHAR = $80;
  {$EXTERNALSYM MINCHAR}
  MAXCHAR = 127;
  {$EXTERNALSYM MAXCHAR}
  MINSHORT = $8000;
  {$EXTERNALSYM MINSHORT}
  MAXSHORT = 32767;
  {$EXTERNALSYM MAXSHORT}
  MINLONG = DWORD($80000000);
  {$EXTERNALSYM MINLONG}
  MAXLONG = $7FFFFFFF;
  {$EXTERNALSYM MAXLONG}
  MAXBYTE = 255;
  {$EXTERNALSYM MAXBYTE}
  MAXWORD = 65535;
  {$EXTERNALSYM MAXWORD}
  MAXDWORD = DWORD($FFFFFFFF);
  {$EXTERNALSYM MAXDWORD}
end.
