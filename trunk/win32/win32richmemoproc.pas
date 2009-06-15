unit Win32RichMemoProc; 

{$mode objfpc}{$H+}

interface

uses
  // windows units
  Windows,richedit, 
  // RTL units  
  Classes, SysUtils, 
  // LCL units
  Graphics,
  // RichMemoUnits
  WSRichMemo, 
  // Win32 widgetset units  
  win32proc; 
  
type
  { TRichEditManager }

  TRichEditManager = class(TObject)
  public
    class function SetSelectedTextStyle(RichEditWnd: Handle; 
      Params: TIntFontParams): Boolean; virtual;
    class function GetSelectedTextStyle(RichEditWnd: Handle; var Params: TIntFontParams): Boolean; virtual;
    class procedure GetSelection(RichEditWnd: Handle; var TextStart, TextLen: Integer); virtual;      
    class procedure SetSelection(RichEditWnd: Handle; TextStart, TextLen: Integer); virtual;      
    class procedure SetHideSelection(RichEditWnd: Handle; AValue: Boolean); virtual;
    class function LoadRichText(RichEditWnd: Handle; ASrc: TStream): Boolean; virtual;
    class function SaveRichText(RichEditWnd: Handle; ADst: TStream): Boolean; virtual;
    
  end;
  TRichManagerClass = class of TRichEditManager;
                     
var
  RichEditManager : TRichManagerClass = nil;

function InitRichEdit: Boolean;
function GetRichEditClass: AnsiString;
procedure CopyStringToCharArray(const s: String; var Chrs: array of Char; ChrsSize: integer);
function FontStylesToEffects(Styles: TFontStyles): LongWord;
function EffectsToFontStyles(Effects: LongWord): TFontStyles;

implementation

const
  GlobalRichClass : AnsiString = '';
  
const  
  TwipsInFontSize = 20; // see MSDN for CHARFORMAT Structure CFM_SIZE
  
function GetRichEditClass: AnsiString;
begin
  Result := GlobalRichClass;
end;  
 
function InitRichEdit: Boolean;
begin
  if GlobalRichClass = '' then begin
    if LoadLibrary('RICHED20.DLL') <> 0 then begin
      if UnicodeEnabledOS then GlobalRichClass := 'RichEdit20W'
      else GlobalRichClass := 'RichEdit20A'
    end else if LoadLibrary('RICHED32.DLL') <> 0 then
      GlobalRichClass := 'RichEdit';
      
    if not Assigned(RichEditManager) then 
      RichEditManager := TRichEditManager;
      
    Result := GlobalRichClass <> '';
  end;
end;

procedure CopyStringToCharArray(const s: String; var Chrs: array of Char; ChrsSize: integer);
begin
  if length(s) < ChrsSize then ChrsSize := length(s);
  if length(s) > 0 then Move(s[1], Chrs[0], ChrsSize);
end;

function FontStylesToEffects(Styles: TFontStyles): LongWord;
begin
  Result := 0;
  if fsBold in Styles then Result := Result or CFE_BOLD;
  if fsItalic in Styles then Result := Result or CFE_ITALIC;
  if fsStrikeOut in Styles then Result := Result or CFE_STRIKEOUT;
  if fsUnderline in Styles then Result := Result or CFE_UNDERLINE;
end;

function EffectsToFontStyles(Effects: LongWord): TFontStyles;
begin
  Result := [];
  if Effects and CFE_BOLD > 0 then Include(Result, fsBold);
  if Effects and CFE_ITALIC > 0 then Include(Result, fsItalic);
  if Effects and CFE_STRIKEOUT > 0 then Include(Result, fsStrikeOut);
  if Effects and CFE_UNDERLINE > 0 then Include(Result, fsUnderline);
end;

         
procedure CharFormatToFontParams(const fmt: TCHARFORMAT; var Params: TIntFontParams);
begin
  Params.Name := fmt.szFaceName;
  Params.Size := Round(fmt.yHeight/TwipsInFontSize);
  Params.Color := fmt.crTextColor;
  Params.Style := EffectsToFontStyles(fmt.dwEffects);
end;

{ TRichEditManager }

class function TRichEditManager.SetSelectedTextStyle(RichEditWnd: Handle; 
  Params: TIntFontParams): Boolean;
var
  w : WPARAM;
  fmt : TCHARFORMAT;
  
begin
  if RichEditWnd = 0 then begin
    Result := false;
    Exit;
  end;
  
  w := SCF_SELECTION;    
    
  FillChar(fmt, sizeof(fmt), 0);
  fmt.cbSize := sizeof(fmt);
  

  fmt.dwMask := fmt.dwMask or CFM_COLOR;
  fmt.crTextColor := Params.Color;

  fmt.dwMask := fmt.dwMask or CFM_FACE ;
  // keep last char for Null-termination?
  CopyStringToCharArray(Params.Name, fmt.szFaceName, LF_FACESIZE-1); 
  
  fmt.dwMask := fmt.dwMask or CFM_SIZE;
  fmt.yHeight := Params.Size * TwipsInFontSize;
  
  fmt.dwMask := fmt.dwMask or CFM_EFFECTS;
  fmt.dwEffects := FontStylesToEffects(Params.Style);
  
  Result := SendMessage(RichEditWnd, EM_SETCHARFORMAT, w, PtrInt(@fmt))>0;
end;

class function TRichEditManager.GetSelectedTextStyle(RichEditWnd: Handle;  
  var Params: TIntFontParams): Boolean; 
var
  w     : WPARAM;
  fmt   : TCHARFORMAT;
  mask  : LongWord;
  
begin
  Result := false;
  if RichEditWnd = 0 then Exit;
  
  w := SCF_SELECTION;    
    
  FillChar(fmt, sizeof(fmt), 0);
  fmt.cbSize := sizeof(fmt);
  fmt.dwMask := CFM_COLOR or CFM_FACE or CFM_SIZE or CFM_EFFECTS;
  
  SendMessage(RichEditWnd, EM_GETCHARFORMAT, w, PtrInt(@fmt));
  
  CharFormatToFontParams(fmt, Params);
  Result := true;  
end;

class procedure TRichEditManager.GetSelection(RichEditWnd: Handle; var TextStart, TextLen: Integer); 
var
  Range  : TCHARRANGE;
begin
  Range.cpMax := 0;
  Range.cpMin := 0;
  SendMessage(RichEditWnd, EM_EXGETSEL, 0, PtrInt(@Range));
  TextStart := Range.cpMin;
  TextLen := Range.cpMax-Range.cpMin;
end;

class procedure TRichEditManager.SetSelection(RichEditWnd: Handle; TextStart, TextLen: Integer); 
var
  Range  : TCHARRANGE;
begin
  Range.cpMin := TextStart;
  Range.cpMax := TextStart + TextLen;
  SendMessage(RichEditWnd, EM_EXSETSEL, 0, PtrInt(@Range));
end;

class procedure TRichEditManager.SetHideSelection(RichEditWnd: Handle; AValue: Boolean); 
begin
  if AValue then
    SendMessage(RichEditWnd, EM_SETOPTIONS, ECOOP_AND, not ECO_NOHIDESEL)
  else
    SendMessage(RichEditWnd, EM_SETOPTIONS, ECOOP_OR, ECO_NOHIDESEL);
end;

type
  TEditStream_ = record
    dwCookie    : PDWORD;
    dwError     : DWORD;
    pfnCallback : EDITSTREAMCALLBACK;
  end;
  
function RTFLoadCallback(dwCookie:PDWORD; pbBuff:LPBYTE; cb:LONG; var pcb:LONG):DWORD; stdcall;
var
  s : TStream;  
begin
  try
    s := TStream(dwCookie);
    pcb := s.Read(pbBuff^, cb);
    Result := 0;
  except
    Result := 1;
  end;
end;

class function TRichEditManager.LoadRichText(RichEditWnd: Handle; ASrc: TStream): Boolean; 
var
  cbs : TEditStream_;
begin
  cbs.dwCookie := PDWORD(ASrc);
  cbs.dwError := 0;
  cbs.pfnCallback := @RTFLoadCallback;
  SendMessage(RichEditWnd, EM_STREAMIN, SF_RTF, LPARAM(@cbs) );
  Result := cbs.dwError = 0;
end;

function RTFSaveCallback(dwCookie:PDWORD; pbBuff:LPBYTE; cb:LONG; var pcb:LONG):DWORD; stdcall;
var
  s : TStream;  
begin
  try
    s := TStream(dwCookie);
    pcb := s.Write(pbBuff^, cb);
    Result := 0;
  except
    Result := 1;
  end;
end;

class function TRichEditManager.SaveRichText(RichEditWnd: Handle; ADst: TStream): Boolean; 
var
  cbs : TEditStream_;
begin
  cbs.dwCookie := PDWORD(ADst);
  cbs.dwError := 0;
  cbs.pfnCallback := @RTFSaveCallback;
  SendMessage(RichEditWnd, EM_STREAMOUT, SF_RTF, LPARAM(@cbs) );
  Result := cbs.dwError = 0;
end;

end.                                            

