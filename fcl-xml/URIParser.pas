{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team
    Original author: Sebastian Guenther

    Unit to parse complete URI in its parts or to reassemble an URI

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit URIParser;

interface

type
  TURI = record
    Protocol: AnsiString;
    Username: AnsiString;
    Password: AnsiString;
    Host: AnsiString;
    Port: Word;
    Path: AnsiString;
    Document: AnsiString;
    Params: AnsiString;
    Bookmark: AnsiString;
    HasAuthority: Boolean;
  end;

//type UTF8String = AnsiString;

function EncodeURI(const URI: TURI): AnsiString;
function ParseURI(const URI: AnsiString):  TURI; overload;
function ParseURI(const URI, DefaultProtocol: AnsiString; DefaultPort: Word):  TURI; overload;

function ResolveRelativeURI(const BaseUri, RelUri: AnsiString;
  out ResultUri: AnsiString): Boolean; overload;

function ResolveRelativeURI(const BaseUri, RelUri: UTF8String;
  out ResultUri: UTF8String): Boolean; overload;

function URIToFilename(const URI: AnsiString; out Filename: AnsiString): Boolean;
function FilenameToURI(const Filename: AnsiString): AnsiString;

function IsAbsoluteURI(const UriReference: AnsiString): Boolean;

implementation

uses SysUtils;

const
  GenDelims = [':', '/', '?', '#', '[', ']', '@'];
  SubDelims = ['!', '$', '&', '''', '(', ')', '*', '+', ',', ';', '='];
  ALPHA = ['A'..'Z', 'a'..'z'];
  DIGIT = ['0'..'9'];
  Unreserved = ALPHA + DIGIT + ['-', '.', '_', '~'];
  ValidPathChars = Unreserved + SubDelims + ['@', ':', '/'];

function Escape(const s: AnsiString; const Allowed: TSysCharSet): AnsiString;
var
  i: Integer;
begin
  SetLength(Result, 0);
  for i := 1 to Length(s) do
    if not (s[i] in Allowed) then
      Result := Result + '%' + IntToHex(ord(s[i]), 2)
    else
      Result := Result + s[i];
end;

function EncodeURI(const URI: TURI): AnsiString;
// ! if no scheme then first colon in path should be escaped
begin
  SetLength(Result, 0);
  if Length(URI.Protocol) > 0 then
    Result := LowerCase(URI.Protocol) + ':';
  if URI.HasAuthority then
  begin
    Result := Result + '//';
    if Length(URI.Username) > 0 then
    begin
      Result := Result + URI.Username;
      if Length(URI.Password) > 0 then
        Result := Result + ':' + URI.Password;
      Result := Result + '@';
    end;
    Result := Result + URI.Host;
  end;
  if URI.Port <> 0 then
    Result := Result + ':' + IntToStr(URI.Port);
  Result := Result + Escape(URI.Path, ValidPathChars);
  if Length(URI.Document) > 0 then
  begin
    if (Length(URI.Path) > 0) and ((Length(Result) = 0) or (Result[Length(Result)] <> '/')) then
      Result := Result + '/';
    Result := Result + Escape(URI.Document, ValidPathChars);
  end;
  if Length(URI.Params) > 0 then
    Result := Result + '?' + Escape(URI.Params, ValidPathChars);
  if Length(URI.Bookmark) > 0 then
    Result := Result + '#' + Escape(URI.Bookmark, ValidPathChars);
end;

function ParseURI(const URI: AnsiString):  TURI;
begin
  Result := ParseURI(URI, '', 0);
end;

function HexValue(c: AnsiChar): Integer;
begin
  case c of
    '0'..'9': Result := ord(c) - ord('0');
    'A'..'F': Result := ord(c) - (ord('A') - 10);
    'a'..'f': Result := ord(c) - (ord('a') - 10);
  else
    Result := 0;
  end;
end;

function Unescape(const s: AnsiString): AnsiString;
var
  i, RealLength: Integer;
begin
  SetLength(Result, Length(s));
  i := 1;
  RealLength := 0;
  while i <= Length(s) do
  begin
    Inc(RealLength);
    if s[i] = '%' then
    begin
      Result[RealLength] := AnsiChar(HexValue(s[i + 1]) shl 4 or HexValue(s[i + 2]));
      Inc(i, 3);
    end else
    begin
      Result[RealLength] := s[i];
      Inc(i);
    end;
  end;
  SetLength(Result, RealLength);
end;

function ParseURI(const URI, DefaultProtocol: AnsiString; DefaultPort: Word):  TURI;
var
  s, Authority: AnsiString;
  i: Integer;
begin
  Result.Protocol := LowerCase(DefaultProtocol);
  Result.Port := DefaultPort;

  s := URI;

  // Extract scheme

  for i := 1 to Length(s) do
    if s[i] = ':' then
    begin
      Result.Protocol := Copy(s, 1, i - 1);
      s := Copy(s, i + 1, MaxInt);
      break;
    end
    else
      if not (((i=1) and (s[i] in ALPHA)) or (s[i] in ALPHA + DIGIT + ['+', '-', '.'])) then
        break;

  // Extract the bookmark

  i := LastDelimiter('#', s);
  if i > 0 then
  begin
    Result.Bookmark := Unescape(Copy(s, i + 1, MaxInt));
    s := Copy(s, 1, i - 1);
  end;

  // Extract the params

  i := LastDelimiter('?', s);
  if i > 0 then
  begin
    Result.Params := Unescape(Copy(s, i + 1, MaxInt));
    s := Copy(s, 1, i - 1);
  end;

  // extract authority

  if (Length(s) > 1) and (s[1] = '/') and (s[2] = '/') then
  begin
    i := 3;
    while (i <= Length(s)) and (s[i] <> '/') do
      Inc(i);
    Authority := Copy(s, 3, i-3);
    s := Copy(s, i, MaxInt);
    Result.HasAuthority := True;    // even if Authority is empty
  end
  else
  begin
    Result.HasAuthority := False;
    Authority := '';
  end;

  // now s is 'hier-part' per RFC3986
  // Extract the document name (nasty...)

  for i := Length(s) downto 1 do
    if s[i] = '/' then
    begin
      Result.Document := Unescape(Copy(s, i + 1, Length(s)));
      if (Result.Document <> '.') and (Result.Document <> '..') then
        s := Copy(s, 1, i)
      else
        Result.Document := '';
      break;
    end else if s[i] = ':' then
      break
    else if i = 1 then
    begin
      Result.Document := Unescape(s);
      if (Result.Document <> '.') and (Result.Document <> '..') then
        s := ''
      else
        Result.Document := '';
      // break - not needed, last iteration
    end;

  // Everything left is a path

  Result.Path := Unescape(s);

  // Extract the port number

  i := LastDelimiter(':@', Authority);
  if (i > 0) and (Authority[i] = ':') then
  begin
    Result.Port := StrToInt(Copy(Authority, i + 1, MaxInt));
    Authority := Copy(Authority, 1, i - 1);
  end;

  // Extract the hostname

  i := Pos('@', Authority);
  if i > 0 then
  begin
    Result.Host := Copy(Authority, i+1, MaxInt);
    Delete(Authority, i, MaxInt);

    // Extract username and password
    if Length(Authority) > 0 then
    begin
      i := Pos(':', Authority);
      if i = 0 then
        Result.Username := Authority
      else
      begin
        Result.Username := Copy(Authority, 1, i - 1);
        Result.Password := Copy(Authority, i + 1, MaxInt);
      end;
    end;
  end
  else
    Result.Host := Authority;
end;

procedure RemoveDotSegments(var s: AnsiString);
var
  Cur, Prev: Integer;
begin
  Prev := Pos('/', s);
  while (Prev > 0) and (Prev < Length(s)) do
  begin
    Cur := Prev+1;
    while (Cur <= Length(s)) and (s[Cur] <> '/') do
      Inc(Cur);
    if (Cur - Prev = 2) and (s[Prev+1] = '.') then
      Delete(s, Prev+1, 2)
    else if (Cur - Prev = 3) and (s[Prev+1] = '.') and (s[Prev+2] = '.') then
    begin
      while (Prev > 1) and (s[Prev-1] <> '/') do
        Dec(Prev);
      if Prev > 1 then
        Dec(Prev);
      Delete(s, Prev+1, Cur-Prev);
    end
    else
      Prev := Cur;
  end;
end;

// TODO: this probably must NOT percent-encode the result...
function ResolveRelativeURI(const BaseUri, RelUri: UTF8String;
  out ResultUri: UTF8String): Boolean;
var
  Base, Rel: TUri;
begin
  Base := ParseUri(BaseUri);
  Rel := ParseUri(RelUri);

  Result := (Base.Protocol <> '') or (Rel.Protocol <> '');
  if not Result then
    Exit;
  with Rel do
  begin
    if (Path = '') and (Document = '') then
    begin
      if (Protocol = '') and (Host = '') then
      begin
        if Params <> '' then
          Base.Params := Params;
        Base.Bookmark := Bookmark;
        ResultUri := EncodeUri(Base);
        Exit;
      end;
    end;
    if (Protocol <> '') then  // RelURI is absolute - return it...
    begin
      ResultUri := RelUri;
      Exit;
    end;
    // Inherit protocol
    Protocol := Base.Protocol;
    if (Host = '') then   // TODO: or "not HasAuthority"?
    begin
      // Inherit Authority (host, port, username, password)
      Host := Base.Host;
      Port := Base.Port;
      Username := Base.Username;
      Password := Base.Password;
      HasAuthority := Base.HasAuthority;
      if (Path = '') or (Path[1] <> '/') then  // path is empty or relative
        Path := Base.Path + Path;
      RemoveDotSegments(Path);
    end;
  end; // with
  ResultUri := EncodeUri(Rel);
end;

function ResolveRelativeURI(const BaseUri, RelUri: AnsiString;
  out ResultUri: AnsiString): Boolean;
var
  rslt: UTF8String;
begin
  Result := ResolveRelativeURI(UTF8Encode(BaseUri), UTF8Encode(RelUri), rslt);
  if Result then
    ResultURI := UTF8Decode(rslt);
end;

function URIToFilename(const URI: AnsiString; out Filename: AnsiString): Boolean;
var
  U: TURI;
  I: Integer;
begin
  Result := False;
  U := ParseURI(URI);
  if SameText(U.Protocol, 'file') then
  begin
    if (Length(U.Path) > 2) and (U.Path[1] = '/') and (U.Path[3] = ':') then
      Filename := Copy(U.Path, 2, MaxInt)
    else
      Filename := U.Path;
    Filename := Filename + U.Document;
    Result := True;
  end
  else
    if U.Protocol = '' then  // fire and pray?
    begin
      Filename := U.Path + U.Document;
      Result := True;
    end;
  if PathDelim <> '/' then
  begin
    I := Pos('/', Filename);
    while I > 0 do
    begin
      Filename[I] := PathDelim;
      I := Pos('/', Filename);
    end;
  end;
end;

function FilenameToURI(const Filename: AnsiString): AnsiString;
var
  I: Integer;
begin
  // TODO: seems implemented, but not tested well
  Result := 'file://';
  if (Length(Filename) > 2) and (Filename[1] <> PathDelim) and (Filename[2] = ':') then
    Result := Result + '/';
  Result := Result + Filename;
  if PathDelim <> '/' then
  begin
    I := Pos(PathDelim, Result);
    while I <> 0 do
    begin
      Result[I] := '/';
      I := Pos(PathDelim, Result);
    end;
  end;
end;


function IsAbsoluteURI(const UriReference: AnsiString): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 1 to Length(UriReference) do
  begin
    if UriReference[I] = ':' then
      Exit
    else
      if not (((I=1) and (UriReference[I] in ALPHA)) or
         (UriReference[i] in ALPHA + DIGIT + ['+', '-', '.'])) then
      Break;
  end;
  Result := False;
end;


end.
