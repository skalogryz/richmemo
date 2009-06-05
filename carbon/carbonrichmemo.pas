unit carbonrichmemo; 

{$mode objfpc}{$H+}

interface

uses
  LCLType, Classes, SysUtils,

  Controls,

  RichMemoTypes, WSRichMemo,

  CarbonEdits;

type
  { TCarbonWSCustomRichMemo }

  TCarbonWSCustomRichMemo = class(TWSCustomRichMemo)
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND; override;
    class function GetTextAttributes(const AWinControl: TWinControl; TextStart: Integer;
      var Params: TFontParams): Boolean; override;
    class procedure SetTextAttributes(const AWinControl: TWinControl; TextStart, TextLen: Integer;
      Mask: TTextStyleMask; const Params: TFontParams); override;
    class procedure SetHideSelection(const AWinControl: TWinControl; AHideSelection: Boolean); override;
  end;

implementation

{ TCarbonWSCustomRichMemo }

class function TCarbonWSCustomRichMemo.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
begin
  Result := TLCLIntfHandle(TCarbonMemo.Create(AWinControl, AParams));
end;

class function TCarbonWSCustomRichMemo.GetTextAttributes(const AWinControl: TWinControl;
  TextStart: Integer; var Params: TFontParams): Boolean;
begin
  Result:=false;
end;

class procedure TCarbonWSCustomRichMemo.SetTextAttributes(const AWinControl: TWinControl;
  TextStart, TextLen: Integer;
  Mask: TTextStyleMask; const Params: TFontParams);
var
  memo  : TCarbonMemo;
begin
  if not Assigned(AWinControl) or not Assigned(AWinControl.Handle) then Exit;
  memo := TCarbonMemo.Create(AWinControl.Handle);
end;

class procedure TCarbonWSCustomRichMemo.SetHideSelection(
  const AWinControl: TWinControl; AHideSelection: Boolean);
begin

end;

end.

