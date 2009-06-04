unit WSRichMemo; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, 

  Graphics, Controls, 
  
  RichMemoTypes, 
  
  WSStdCtrls; 
  
type
  { TWSCustomRichMemo }

  TWSCustomRichMemo = class(TWSCustomMemo)
  published
    class function GetTextAttributes(const AWinControl: TWinControl; TextStart: Integer; 
      var Params: TFontParams): Boolean; virtual;
    class procedure SetTextAttributes(const AWinControl: TWinControl; TextStart, TextLen: Integer; 
      Mask: TTextStyleMask; const Params: TFontParams); virtual;
    class procedure SetHideSelection(const AWinControl: TWinControl; AHideSelection: Boolean); virtual;
  end;
  TWSCustomRichMemoClass = class of TWSCustomRichMemo;
  
function WSRegisterCustomRichMemo: Boolean; external name 'WSRegisterCustomRichMemo';

implementation

{ TWSCustomRichMemo }

class function TWSCustomRichMemo.GetTextAttributes(const AWinControl: TWinControl; 
  TextStart: Integer; var Params: TFontParams): Boolean; 
begin
  Result := false;
end;

class procedure TWSCustomRichMemo.SetTextAttributes(const AWinControl: TWinControl; 
  TextStart, TextLen: Integer;  
  Mask: TTextStyleMask; const Params: TFontParams); 
begin
end;

class procedure TWSCustomRichMemo.SetHideSelection(const AWinControl: TWinControl; AHideSelection: Boolean); 
begin

end;

end.

