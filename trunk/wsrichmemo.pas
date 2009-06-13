unit WSRichMemo; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, 

  Graphics, Controls, 
  
  WSStdCtrls; 
  
type

  TIntFontParams = record
    Name    : String;
    Size    : Integer;
    Color   : TColor;
    Style   : TFontStyles;
  end;


  { TWSCustomRichMemo }

  TWSCustomRichMemo = class(TWSCustomMemo)
  published
    class function GetStyleRange(const AWinControl: TWinControl; TextStart: Integer; var RangeStart, RangeLen: Integer): Boolean; virtual;
    class function GetTextAttributes(const AWinControl: TWinControl; TextStart: Integer;
      var Params: TIntFontParams): Boolean; virtual;
    class procedure SetTextAttributes(const AWinControl: TWinControl; TextStart, TextLen: Integer; 
      {Mask: TTextStyleMask;} const Params: TIntFontParams); virtual;
    class procedure SetHideSelection(const AWinControl: TWinControl; AHideSelection: Boolean); virtual;
    class function LoadRichText(const AWinControl: TWinControl; Source: TStream): Boolean; virtual;
    class function SaveRichText(const AWinControl: TWinControl; Dest: TStream): Boolean; virtual;
  end;
  TWSCustomRichMemoClass = class of TWSCustomRichMemo;

  
function WSRegisterCustomRichMemo: Boolean; external name 'WSRegisterCustomRichMemo';

implementation

{ TWSCustomRichMemo }

class function TWSCustomRichMemo.GetStyleRange(const AWinControl: TWinControl;
  TextStart: Integer; var RangeStart, RangeLen: Integer): Boolean;
begin
  RangeStart :=-1;
  RangeLen := -1;
  Result := false;
end;

class function TWSCustomRichMemo.GetTextAttributes(const AWinControl: TWinControl; 
  TextStart: Integer; var Params: TIntFontParams): Boolean;
begin
  Result := false;
end;

class procedure TWSCustomRichMemo.SetTextAttributes(const AWinControl: TWinControl; 
  TextStart, TextLen: Integer;  
  {Mask: TTextStyleMask;} const Params: TIntFontParams);
begin
end;

class procedure TWSCustomRichMemo.SetHideSelection(const AWinControl: TWinControl; AHideSelection: Boolean); 
begin

end;

class function TWSCustomRichMemo.LoadRichText(const AWinControl: TWinControl; Source: TStream): Boolean;
begin
  Result := false;
end;

class function TWSCustomRichMemo.SaveRichText(const AWinControl: TWinControl; Dest: TStream): Boolean;
begin
  Result := false;
end;

end.

