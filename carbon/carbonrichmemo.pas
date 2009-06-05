unit carbonrichmemo; 

{$mode objfpc}{$H+}

interface

uses
  MacOSAll,

  LCLType, Classes, SysUtils,

  Controls, Graphics,

  RichMemoTypes, WSRichMemo,

  CarbonProc, CarbonEdits;

type

  { TCarbonRichEdit }

  TCarbonRichEdit = class(TCarbonMemo)
  protected
    function GetCreationOptions: TXNFrameOptions; override;
  end;

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

const
  TXNAttributesMax = 10;

procedure AttrSetFontName(const FontName: String; var Attr: TXNTypeAttributes);
begin
  Attr.tag := kATSUFontTag;
  Attr.size := SizeOf(ATSUFontID);
  Attr.data.dataValue := FindCarbonFontID(FontName);
end;

procedure AttrSetColor(var MacColor: RGBColor; var Attr: TXNTypeAttributes);
begin
  Attr.tag := kTXNQDFontColorAttribute;
  Attr.size := kTXNQDFontColorAttributeSize;
  Attr.data.dataPtr := @MacColor;
end;

procedure AttrSetSize(FontSize: Integer; var Attr: TXNTypeAttributes);
begin
  Attr.tag := kTXNQDFontSizeAttribute;
  Attr.size := kTXNQDFontSizeAttributeSize;
  Attr.data.dataValue := FontSize;
end;

procedure AttrSetStyle(FontStyle: TFontStyles; var Attr: TXNTypeAttributes);
begin
  Attr.tag := kTXNQDFontStyleAttribute;
  Attr.size := kTXNQDFontStyleAttributeSize;
  Attr.data.dataValue := normal;

  if fsBold in FontStyle then Attr.data.dataValue := Attr.data.dataValue or bold;
  if fsItalic in FontStyle then Attr.data.dataValue := Attr.data.dataValue or italic;
  if fsUnderline in FontStyle then Attr.data.dataValue := Attr.data.dataValue or underline;
  // if fsStrikeOut in FontStyle then ... can be implemented only by using ATSU
end;

procedure ParamsToTXNAttribs(ParamsMask: TTextStyleMask; const Params: TFontParams;
  var Attr: array of TXNTypeAttributes; var AttrCount: Integer; var MacColor: RGBColor);
begin
  AttrCount := 0;
  //todo: replace QuickDraw style by ATSU style

  if tsm_Color in ParamsMask then begin
    MacColor := ColorToRGBColor(Params.Color);
    AttrSetColor(MacColor, Attr[AttrCount] );
    inc(AttrCount);
  end;

  if tsm_Name in ParamsMask then begin
    AttrSetFontName(Params.Name, Attr[AttrCount] );
    inc(AttrCount);
  end;

  if tsm_Size in ParamsMask then begin
    AttrSetSize(Params.Size, Attr[AttrCount] );
    inc(AttrCount);
  end;

  if tsm_Styles in ParamsMask then begin
    AttrSetStyle(Params.Style, Attr[AttrCount]);
    inc(AttrCount);
  end;
end;

{ TCarbonWSCustomRichMemo }

class function TCarbonWSCustomRichMemo.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
begin
  Result := TLCLIntfHandle(TCarbonRichEdit.Create(AWinControl, AParams));
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
  attr  : array [0..TXNAttributesMax-1] of TXNTypeAttributes;
  attrcount : Integer;
  maccolor  : RGBColor;
begin
  if not Assigned(AWinControl) or (AWinControl.Handle = 0) then Exit;
  memo := TCarbonMemo(AWinControl.Handle);

  ParamsToTXNAttribs(Mask, Params, attr, attrcount, maccolor);

  TXNSetTypeAttributes(HITextViewGetTXNObject(ControlRef(memo.Widget)), attrcount, @Attr[0],
    TextStart, TextStart+TextLen);
end;

class procedure TCarbonWSCustomRichMemo.SetHideSelection(const AWinControl: TWinControl;
  AHideSelection: Boolean);
begin

end;

{ TCarbonRichEdit }

function TCarbonRichEdit.GetCreationOptions: TXNFrameOptions;
begin
  Result := kOutputTextInUnicodeEncodingMask;
end;

end.

