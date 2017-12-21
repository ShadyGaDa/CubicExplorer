//******************************************************************************
//  CubicCore
//  Version: 1.00
//
//  The Original Code is ccTypes.pas.
//
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved.
//
//******************************************************************************
unit ccTypes;

{$INCLUDE Core.inc}

{==============================================================================}
interface

uses
  {$IFDEF MSWindows}
  Windows,
  {$ENDIF}
  Types;

{==============================================================================}
type
  TWideFileName = type WideString;

  TRect = Types.TRect;
  TPoint = Types.TPoint;

  {$IFDEF MSWindows}
  HWND = Windows.HWND;
  {$ELSE}
  HWND = type LongWord;
  {$ENDIF}

  TCCInsertMode = (imBefore, imAfter, imFirstChild, imLastChild);

{==============================================================================}
implementation

end.
