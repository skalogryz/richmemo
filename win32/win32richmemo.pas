unit Win32RichMemo;

{$mode objfpc}{$H+}

interface

uses
  // Win32 headers  
  Windows, 
  // RTL headers
  Classes, SysUtils, 
  // LCL headers
  LCLType, LCLIntf, LCLProc, WSLCLClasses,
  Controls, StdCtrls,
  // Win32WidgetSet 
  Win32WSControls, Win32Int, 
  // RichMemo headers
  WSRichMemo, Win32RichMemoProc;

type  

  { TWin32WSCustomRichMemo }

  TWin32WSCustomRichMemo = class(TWSCustomRichMemo)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND; override;
    class function GetTextAttributes(const AWinControl: TWinControl; TextStart: Integer;
      var Params: TIntFontParams): Boolean; override;
    class procedure SetTextAttributes(const AWinControl: TWinControl; TextStart, TextLen: Integer; 
      const Params: TIntFontParams); override;
    class procedure SetHideSelection(const AWinControl: TWinControl; AHideSelection: Boolean); override;      
    class function GetStyleRange(const AWinControl: TWinControl; TextStart: Integer; var RangeStart, RangeLen: Integer): Boolean; override;
    class function LoadRichText(const AWinControl: TWinControl; Source: TStream): Boolean; override;
    class function SaveRichText(const AWinControl: TWinControl; Dst: TStream): Boolean; override;
  end;
  
implementation

const
  AlignmentToEditFlags: array[TAlignment] of DWord =
  (
{ taLeftJustify  } ES_LEFT,
{ taRightJustify } ES_RIGHT,
{ taCenter       } ES_CENTER
  );

  
procedure LockRedraw(AHandle: Integer);
begin
  SendMessage(AHandle, WM_SETREDRAW, 0, 0);
end;

procedure UnlockRedraw(AHandle: Integer; Invalidate: Boolean = true);
begin
  SendMessage(AHandle, WM_SETREDRAW, 1, 0);
  if Invalidate then InvalidateRect(AHandle, nil, false);
end;  

function RichEditProc(Window: HWnd; Msg: UInt; WParam: Windows.WParam;
   LParam: Windows.LParam): LResult; stdcall;
begin 
  if Msg = WM_PAINT then
    //todo: LCL WM_PAINT handling prevents richedit from drawing correctly
    Result := CallDefaultWindowProc(Window, Msg, WParam, LParam)
  else
    Result := WindowProc(Window, Msg, WParam, LParam);
end;  

{ TWin32WSCustomRichMemo }

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
  PrepareCreateWindow(AWinControl, Params);
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

class function TWin32WSCustomRichMemo.GetTextAttributes(
  const AWinControl: TWinControl; TextStart: Integer; var Params: TIntFontParams
  ): Boolean;  
var
  OrigStart : Integer;
  OrigLen   : Integer;
  NeedLock  : Boolean;  
begin
  if not Assigned(RichEditManager) or not Assigned(AWinControl) then begin
    Result := false;
    Exit;
  end;
  
  RichEditManager.GetSelection(AWinControl.Handle, OrigStart, OrigLen);
  
  NeedLock := (OrigStart <> TextStart);
  if NeedLock then begin
    LockRedraw(AWinControl.Handle);
    RichEditManager.SetSelection(AWinControl.Handle, TextStart, 1);
    Result := RichEditManager.GetSelectedTextStyle(AWinControl.Handle, Params );
    RichEditManager.SetSelection(AWinControl.Handle, OrigStart, OrigLen);
    UnlockRedraw(AWinControl.Handle);
  end else 
    Result := RichEditManager.GetSelectedTextStyle(AWinControl.Handle, Params);
end;


class procedure TWin32WSCustomRichMemo.SetHideSelection(
  const AWinControl: TWinControl; AHideSelection: Boolean);  
begin
  if not Assigned(RichEditManager) or not Assigned(AWinControl) then Exit;
  RichEditManager.SetHideSelection(AWinControl.Handle, AHideSelection);
end;

class function TWin32WSCustomRichMemo.GetStyleRange(
  const AWinControl: TWinControl; TextStart: Integer; var RangeStart, 
  RangeLen: Integer): Boolean;  
var
  OrigStart : Integer;
  OrigLen   : Integer;
begin
  if not Assigned(RichEditManager) or not Assigned(AWinControl) then begin
    Result := false;
    Exit;
  end;
  RichEditManager.GetSelection(AWinControl.Handle, OrigStart, OrigLen);
  LockRedraw(AWinControl.Handle);
  RichEditManager.SetSelection(AWinControl.Handle, TextStart, 1);
  Result := RichEditManager.GetStyleRange(AWinControl.Handle, TextStart, RangeStart, RangeLen);
  RichEditManager.SetSelection(AWinControl.Handle, OrigStart, OrigLen);
  UnlockRedraw(AWinControl.Handle);
end;

class function TWin32WSCustomRichMemo.LoadRichText(
  const AWinControl: TWinControl; Source: TStream): Boolean;  
begin
  Result := false;
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
 
end.

