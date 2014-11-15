{
 win32richmemo.pas 
 
 Author: Dmitry 'skalogryz' Boyarintsev 

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

unit Win32RichMemo;

{$mode objfpc}{$H+}

interface

uses
  // Win32 headers  
  Windows, RichEdit,
  // RTL headers
  Classes, SysUtils, 
  // LCL headers
  LCLType, LCLIntf, LCLProc, WSLCLClasses,
  Graphics, Controls, StdCtrls, 
  // Win32WidgetSet
  Win32WSControls, Win32Int, 
  // RichMemo headers
  WSRichMemo, Win32RichMemoProc;

type  

  { TWin32WSCustomRichMemo }

  TWin32WSCustomRichMemo = class(TWSCustomRichMemo)
  published
    class procedure SetColor(const AWinControl: TWinControl); override;
  
    class procedure SetSelStart(const ACustomEdit: TCustomEdit; NewStart: integer); override;
    class procedure SetSelLength(const ACustomEdit: TCustomEdit; NewLength: integer); override;
    
    class procedure CutToClipboard(const AWinControl: TWinControl); override;
    class procedure CopyToClipboard(const AWinControl: TWinControl); override;
    class procedure PasteFromClipboard(const AWinControl: TWinControl); override;

    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND; override;
    class function GetTextAttributes(const AWinControl: TWinControl; TextStart: Integer;
      var Params: TIntFontParams): Boolean; override;
    class procedure SetTextAttributes(const AWinControl: TWinControl; TextStart, TextLen: Integer;
      const Params: TIntFontParams); override;
    class procedure SetHideSelection(const ACustomEdit: TCustomEdit; AHideSelection: Boolean); override;      
    class function GetStyleRange(const AWinControl: TWinControl; TextStart: Integer; var RangeStart, RangeLen: Integer): Boolean; override;
    class function LoadRichText(const AWinControl: TWinControl; Source: TStream): Boolean; override;
    class function SaveRichText(const AWinControl: TWinControl; Dst: TStream): Boolean; override;

    class function GetParaAlignment(const AWinControl: TWinControl; TextStart: Integer;
      var AAlign: Integer): Boolean; override;
    class procedure SetParaAlignment(const AWinControl: TWinControl; TextStart, TextLen: Integer;
      const AAlign: Integer); override;

    class function GetParaMetric(const AWinControl: TWinControl; TextStart: Integer;
      var AMetrics: TIntParaMetric): Boolean; override;
    class procedure SetParaMetric(const AWinControl: TWinControl; TextStart, TextLength: Integer;
      const AMetrics: TIntParaMetric); override;

    class function GetParaNumbering(const AWinControl: TWinControl; TextStart: Integer;
      var ANumber: TIntParaNumbering): Boolean; override;
    class procedure SetParaNumbering(const AWinControl: TWinControl; TextStart, TextLen: Integer;
      const ANumber: TIntParaNumbering); override;

    class procedure InDelText(const AWinControl: TWinControl; const TextUTF8: String; DstStart, DstLen: Integer); override;
  end;
  
implementation

const
  AlignmentToEditFlags: array[TAlignment] of DWord =
  (
{ taLeftJustify  } ES_LEFT,
{ taRightJustify } ES_RIGHT,
{ taCenter       } ES_CENTER
  );

  
procedure LockRedraw(AHandle: HWND);
begin
  SendMessage(AHandle, WM_SETREDRAW, 0, 0);
end;

procedure UnlockRedraw(AHandle: HWND; NeedInvalidate: Boolean = true);
begin
  SendMessage(AHandle, WM_SETREDRAW, 1, 0);
  if NeedInvalidate then 
    Windows.InvalidateRect(AHandle, nil, true);
end;  

function RichEditProc(Window: HWnd; Msg: UInt; WParam: Windows.WParam;
   LParam: Windows.LParam): LResult; stdcall;
begin 
  if Msg = WM_PAINT then begin
    //todo: LCL WM_PAINT handling prevents richedit from drawing correctly
    Result := CallDefaultWindowProc(Window, Msg, WParam, LParam)
    //Result := WindowProc(Window, Msg, WParam, LParam)
  end else
    Result := WindowProc(Window, Msg, WParam, LParam);
end;  

{ TWin32WSCustomRichMemo }

class procedure TWin32WSCustomRichMemo.SetColor(const AWinControl: TWinControl);  
begin
  // this methos is implemented, because Win32RichMemo doesn't use 
  // default LCL WM_PAINT message!
  SendMessage(AWinControl.Handle, EM_SETBKGNDCOLOR, 0, ColorToRGB(AWinControl.Color));
end;

class procedure TWin32WSCustomRichMemo.SetSelStart(const ACustomEdit: TCustomEdit; NewStart: integer);  
var
  range : Tcharrange;
begin
  range.cpMin := NewStart;
  range.cpMax := NewStart;
  SendMessage(ACustomEdit.Handle, EM_EXSETSEL, 0, LPARAM(@range));
  InvalidateRect(ACustomEdit.Handle, nil, false);  
end;

class procedure TWin32WSCustomRichMemo.SetSelLength(const ACustomEdit: TCustomEdit; NewLength: integer);  
var
  range : Tcharrange;
begin
  SendMessage(ACustomEdit.Handle, EM_EXGETSEL, 0, LPARAM(@range));
  range.cpMax := range.cpMin + NewLength;
  SendMessage(ACustomEdit.Handle, EM_EXSETSEL, 0, LPARAM(@range));
  InvalidateRect(ACustomEdit.Handle, nil, false);
end;

class procedure TWin32WSCustomRichMemo.CutToClipboard(const AWinControl: TWinControl);  
begin
  SendMessage(AWinControl.Handle, WM_CUT, 0,0);  
end;

class procedure TWin32WSCustomRichMemo.CopyToClipboard(const AWinControl: TWinControl);  
begin
  SendMessage(AWinControl.Handle, WM_COPY, 0,0);  
end;

class procedure TWin32WSCustomRichMemo.PasteFromClipboard(const AWinControl: TWinControl);  
begin
  SendMessage(AWinControl.Handle, WM_PASTE, 0,0);
end;

class function TWin32WSCustomRichMemo.CreateHandle(const AWinControl: TWinControl;  
  const AParams: TCreateParams): HWND;  
var
  Params      : TCreateWindowExParams;
  RichClass   : AnsiString;
  ACustomMemo : TCustomMemo;
begin
  InitRichEdit;
  RichClass := GetRichEditClass;
  if RichClass = '' then begin
    Result := 0;
    Exit;
  end;

  // general initialization of Params

  // if you're using 0.9.28.2 compiler, uncomment the line,
  // PrepareCreateWindow(AWinControl, Params);
  // and comment the following like (it's for 0.9.30 compatiblity):
  PrepareCreateWindow(AWinControl, AParams, Params);

  Params.SubClassWndProc := @RichEditProc;

  // customization of Params
  ACustomMemo := TCustomMemo(AWinControl);
  with Params do
  begin
    Flags := Flags or ES_AUTOVSCROLL or ES_MULTILINE or ES_WANTRETURN;

    if ACustomMemo.ReadOnly then
      Flags := Flags or ES_READONLY;
    Flags := Flags or AlignmentToEditFlags[ACustomMemo.Alignment];
    case ACustomMemo.ScrollBars of
      ssHorizontal, ssAutoHorizontal:
        Flags := Flags or WS_HSCROLL;
      ssVertical, ssAutoVertical:
        Flags := Flags or WS_VSCROLL;
      ssBoth, ssAutoBoth:
        Flags := Flags or WS_HSCROLL or WS_VSCROLL;
    end;
    if ACustomMemo.WordWrap then
      Flags := Flags and not WS_HSCROLL
    else
      Flags := Flags or ES_AUTOHSCROLL;
      
    if ACustomMemo.BorderStyle=bsSingle then
      FlagsEx := FlagsEx or WS_EX_CLIENTEDGE;
      
    pClassName := @RichClass[1];
    WindowTitle := StrCaption;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  // memo is not a transparent control -> no need for parentpainting
  Params.WindowInfo^.needParentPaint := false;
  Result := Params.Window;
end;

class procedure TWin32WSCustomRichMemo.SetTextAttributes(const AWinControl: TWinControl; 
  TextStart, TextLen: Integer; const Params: TIntFontParams);  
var
  OrigStart : Integer;
  OrigLen   : Integer;
  NeedLock  : Boolean;  
begin
  if not Assigned(RichEditManager) or not Assigned(AWinControl) then Exit;
  
  RichEditManager.GetSelection(AWinControl.Handle, OrigStart, OrigLen);
  
  NeedLock := (OrigStart <> TextStart) or (OrigLen <> TextLen);
  if NeedLock then begin
    LockRedraw(AWinControl.Handle);
    RichEditManager.SetSelection(AWinControl.Handle, TextStart, TextLen);
    RichEditManager.SetSelectedTextStyle(AWinControl.Handle, Params );
    RichEditManager.SetSelection(AWinControl.Handle, OrigStart, OrigLen);
    UnlockRedraw(AWinControl.Handle);
  end else 
    RichEditManager.SetSelectedTextStyle(AWinControl.Handle, Params);
end;

class function TWin32WSCustomRichMemo.GetTextAttributes(const AWinControl: TWinControl; 
  TextStart: Integer; var Params: TIntFontParams): Boolean;  
var
  OrigStart : Integer;
  OrigLen   : Integer;
  NeedLock  : Boolean;  
  eventmask : LongWord;
begin
  if not Assigned(RichEditManager) or not Assigned(AWinControl) then begin
    Result := false;
    Exit;
  end;
  
  eventmask := SendMessage(AWinControl.Handle, EM_GETEVENTMASK, 0, 0);
  SendMessage(AWinControl.Handle, EM_SETEVENTMASK, 0, 0);
  
  RichEditManager.GetSelection(AWinControl.Handle, OrigStart, OrigLen);
  
  NeedLock := (OrigStart <> TextStart);
  if NeedLock then begin
    LockRedraw(AWinControl.Handle);
    RichEditManager.SetSelection(AWinControl.Handle, TextStart, 1);
    Result := RichEditManager.GetSelectedTextStyle(AWinControl.Handle, Params );
    RichEditManager.SetSelection(AWinControl.Handle, OrigStart, OrigLen);
    UnlockRedraw(AWinControl.Handle, false);
  end else begin
    LockRedraw(AWinControl.Handle);
    Result := RichEditManager.GetSelectedTextStyle(AWinControl.Handle, Params);
    UnlockRedraw(AWinControl.Handle, false);
  end;
    
  SendMessage(AWinControl.Handle, EM_SETEVENTMASK, 0, eventmask);
end;


class procedure TWin32WSCustomRichMemo.SetHideSelection(
  const ACustomEdit: TCustomEdit; AHideSelection: Boolean);  
begin
  if not Assigned(RichEditManager) or not Assigned(ACustomEdit) then Exit;
  RichEditManager.SetHideSelection(ACustomEdit.Handle, AHideSelection);
end;

procedure InitScrollInfo(var info: TScrollInfo);
begin
  FillChar(info, sizeof(info), 0);
  info.cbSize := sizeof(info);
  info.fMask := SIF_ALL;
end;

class function TWin32WSCustomRichMemo.GetStyleRange(
  const AWinControl: TWinControl; TextStart: Integer; var RangeStart, 
  RangeLen: Integer): Boolean;  
var
  OrigStart : Integer;
  OrigLen   : Integer;
  hInfo     : TScrollInfo;
  vInfo     : TScrollInfo;
  hVisible  : Boolean;
  vVisible  : Boolean;
  eventmask : longword;
begin
  if not Assigned(RichEditManager) or not Assigned(AWinControl) then begin
    Result := false;
    Exit;
  end;
  
  eventmask := SendMessage(AWinControl.Handle, EM_GETEVENTMASK, 0, 0);
  SendMessage(AWinControl.Handle, EM_SETEVENTMASK, 0, 0);
  
  RichEditManager.GetSelection(AWinControl.Handle, OrigStart, OrigLen);
  LockRedraw(AWinControl.Handle);
  InitScrollInfo(hInfo);
  InitScrollInfo(vInfo);  
  hVisible:=GetScrollbarVisible(AWinControl.Handle, SB_Horz);
  vVisible:=GetScrollbarVisible(AWinControl.Handle, SB_Vert);
  GetScrollInfo(AWinControl.Handle, SB_Horz, hInfo);
  GetScrollInfo(AWinControl.Handle, SB_Vert, vInfo);
  
  RichEditManager.SetSelection(AWinControl.Handle, TextStart, 1);
  try
    Result := RichEditManager.GetStyleRange(AWinControl.Handle, TextStart, RangeStart, RangeLen);
  except
  end;
  
  if hVisible then SetScrollInfo(AWinControl.Handle, SB_Horz, hInfo, false);
  if vVisible then SetScrollInfo(AWinControl.Handle, SB_Vert, vInfo, false);
  RichEditManager.SetSelection(AWinControl.Handle, OrigStart, OrigLen);
  UnlockRedraw(AWinControl.Handle, false);
  
  SendMessage(AWinControl.Handle, EM_SETEVENTMASK, 0, eventmask);
end;

class function TWin32WSCustomRichMemo.LoadRichText(
  const AWinControl: TWinControl; Source: TStream): Boolean;  
begin
  Result := False;
  if not Assigned(RichEditManager) or not Assigned(AWinControl) then Exit;
  Result := RichEditManager.LoadRichText(AWinControl.Handle, Source);
end;

class function TWin32WSCustomRichMemo.SaveRichText(
  const AWinControl: TWinControl; Dst: TStream): Boolean;  
begin
  Result := false;
  if not Assigned(RichEditManager) or not Assigned(AWinControl) then Exit;
  Result := RichEditManager.SaveRichText(AWinControl.Handle, Dst);
end;

class function TWin32WSCustomRichMemo.GetParaAlignment(
  const AWinControl: TWinControl; TextStart: Integer; var AAlign: Integer
  ): Boolean;
var
  para : PARAFORMAT2;
begin
  Result:=false;
  if not Assigned(RichEditManager) or not Assigned(AWinControl) then Exit;
  RichEditManager.GetPara2(AWinControl.Handle, TextStart, para);
  case para.wAlignment of
    PFA_CENTER:  AAlign:=AL_CENTER;
    PFA_RIGHT:   AAlign:=AL_RIGHT;
    PFA_JUSTIFY: AAlign:=AL_JUSTIFY;
  else
    AAlign:=AL_LEFT;
  end;
  Result:=true;
end;

class procedure TWin32WSCustomRichMemo.SetParaAlignment(
  const AWinControl: TWinControl; TextStart, TextLen: Integer; const AAlign: Integer);
var
  para : PARAFORMAT2;
const
  WinPara : array [AL_LEFT..AL_JUSTIFY] of word = (PFA_LEFT, PFA_RIGHT, PFA_CENTER, PFA_JUSTIFY);
begin
  if not Assigned(RichEditManager) or not Assigned(AWinControl)
    or (AAlign<AL_LEFT) or (AAlign>AL_JUSTIFY) then Exit;
  FillChar(para, sizeof(para), 0);
  para.cbSize:=sizeof(para);
  para.dwMask:=PFM_ALIGNMENT;
  para.wAlignment:=WinPara[byte(AAlign)];
  RichEditManager.SetPara2(AWinControl.Handle, TextStart, TextLen, para);
end;

class function TWin32WSCustomRichMemo.GetParaMetric(
  const AWinControl: TWinControl; TextStart: Integer;
  var AMetrics: TIntParaMetric): Boolean;
var
  para : PARAFORMAT2;
begin
  Result:=false;
  if not Assigned(RichEditManager) or not Assigned(AWinControl) then Exit;
  RichEditManager.GetPara2(AWinControl.Handle, TextStart, para);

  AMetrics.FirstLine:=para.dxStartIndent/20;
  AMetrics.TailIndent:=para.dxRightIndent/20;
  AMetrics.HeadIndent:=(para.dxStartIndent+para.dxOffset)/20;
  AMetrics.SpaceAfter:=para.dySpaceAfter/20;
  AMetrics.SpaceBefore:=para.dySpaceBefore/20;
  AMetrics.LineSpacing:=para.dyLineSpacing/20;
end;

class procedure TWin32WSCustomRichMemo.SetParaMetric(
  const AWinControl: TWinControl; TextStart, TextLength: Integer;
  const AMetrics: TIntParaMetric);
var
  para : PARAFORMAT2;
begin
  if not Assigned(RichEditManager) or not Assigned(AWinControl) then Exit;
  FillChar(para, SizeOf(para), 0);

  para.cbSize:=sizeof(para);
  para.dwMask:=
     PFM_STARTINDENT or PFM_RIGHTINDENT
     or PFM_OFFSET
     or PFM_SPACEAFTER or PFM_SPACEBEFORE
     or PFM_LINESPACING;
  para.dxStartIndent:=round(AMetrics.FirstLine*20);
  para.dxRightIndent:=round(AMetrics.TailIndent*20);
  para.dxOffset:=round((AMetrics.HeadIndent-AMetrics.FirstLine)*20);
    //round(AMetrics.HeadIndent*20);
  para.dySpaceAfter:=round(AMetrics.SpaceAfter*20);
  para.dySpaceBefore:=round(AMetrics.SpaceBefore*20);
  para.dyLineSpacing:=round(AMetrics.LineSpacing*20);
  RichEditManager.SetPara2(AWinControl.Handle, TextStart, TextLength, para);
end;

const
  PFN_ARABIC   = 2;
  PFN_LCLETTER = 3;
  PFN_LCROMAN  = 4;
  PFN_UCLETTER = 5;
  PFN_UCROMAN  = 6;
  PFN_CUSTOM   = 7;

class function TWin32WSCustomRichMemo.GetParaNumbering(
  const AWinControl: TWinControl; TextStart: Integer;
  var ANumber: TIntParaNumbering): Boolean;
var
  para : PARAFORMAT2;
begin
  Result:=False;
  if not Assigned(RichEditManager) or not Assigned(AWinControl) then Exit;

  RichEditManager.GetPara2(AWinControl.Handle, TextStart, para);
  case para.wNumbering of
    PFN_BULLET:   ANumber.Numbering:=pnBullet;
    PFN_ARABIC:   ANumber.Numbering:=pnNumber;
    PFN_LCLETTER: ANumber.Numbering:=pnLowLetter;
    PFN_LCROMAN:  ANumber.Numbering:=pnLowRoman;
    PFN_UCLETTER: ANumber.Numbering:=pnUpLetter;
    PFN_UCROMAN:  ANumber.Numbering:=pnUpRoman;
    PFN_CUSTOM:   begin
      ANumber.Numbering:=pnCustomChar;
      ANumber.NumCustom:=WideChar(para.wNumberingStart);
    end;
  else
    ANumber.Numbering:=pnNone;
  end;
  ANumber.NumIndent:=para.wNumberingTab/20;
  Result:=true
end;

class procedure TWin32WSCustomRichMemo.SetParaNumbering(
  const AWinControl: TWinControl; TextStart, TextLen: Integer;
  const ANumber: TIntParaNumbering);
var
  para : PARAFORMAT2;
begin
  if not Assigned(RichEditManager) or not Assigned(AWinControl) then Exit;
  FillChar(para, SizeOf(para), 0);

  para.cbSize:=sizeof(para);
  para.dwMask:=
     PFM_NUMBERING or PFM_NUMBERINGTAB;
  case ANumber.Numbering of
    pnNone:       para.wNumbering:=0;
    pnBullet:     para.wNumbering:=PFN_BULLET;
    pnNumber:     para.wNumbering:=PFN_ARABIC;
    pnLowLetter:  para.wNumbering:=PFN_LCLETTER;
    pnLowRoman:   para.wNumbering:=PFN_LCROMAN;
    pnUpLetter:   para.wNumbering:=PFN_UCLETTER;
    pnUpRoman:    para.wNumbering:=PFN_UCROMAN;
    pnCustomChar: begin
      para.wNumbering:=PFN_CUSTOM;
      para.wNumberingStart:=Word(ANumber.NumCustom);
      para.dwMask:=para.dwMask or PFM_NUMBERINGSTART;
    end;
  end;

  para.wNumberingTab:=round(ANumber.NumIndent*20);
  RichEditManager.SetPara2(AWinControl.Handle, TextStart, TextLen, para);
end;

class procedure TWin32WSCustomRichMemo.InDelText(const AWinControl:TWinControl;
  const TextUTF8:String;DstStart,DstLen:Integer);
begin
  RichEditManager.SetText(AWinControl.Handle,UTF8Decode(TextUTF8),DstStart,DstLen);
end;
 
end.

