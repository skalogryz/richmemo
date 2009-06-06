unit CarbonRichMemo;

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
  public
    function GetContinuousTypeAttributes(var oContinuousFlags: TXNContinuousFlags;
      iCount: ItemCount; var ioTypeAttributes: array of TXNTypeAttributes): Boolean;
    function SetTypeAttributes(iCount: ItemCount; const iTypeAttributes: array of TXNTypeAttributes;
      StartOffset, EndOffset: Integer): Boolean;
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

function GetATSUFontName(AStyle: ATSUStyle): String;
var
  fontid: ATSUFontID;
begin
  ATSUGetAttribute(AStyle, kATSUFontTag, sizeof(ATSUFontID), @fontid, nil);
  Result := CarbonFontIDToFontName(fontid);
end;

function GetATSUFontSize(ASTyle: ATSUStyle): Integer;
var
  sz  : fixed;
begin
  ATSUGetAttribute(AStyle, kATSUSizeTag, sizeof(fixed), @sz, nil);
  Result := Fix2Long(sz);
end;

procedure GetATSUFontRGBAColor(AStyle: ATSUStyle; var r,g,b,a: Byte);
var
  rgba : ATSURGBAlphaColor;
begin
  ATSUGetAttribute(AStyle, kATSURGBAlphaColorTag, sizeof(Boolean), @rgba, nil);
  r := Round(rgba.red*255);
  g := Round(rgba.green*255);
  b := Round(rgba.blue*255);
  a := Round(rgba.alpha*255);
end;

function GetATSUFontColor(AStyle: ATSUStyle): TColor;
var
  r,g,b,a: Byte;
begin
  GetATSUFontRGBAColor(AStyle, r,g,b,a);
  Result := (b shl 16) or (g shl 8) or r;
end;

function GetATSUFontStyles(AStyle: ATSUStyle): TFontStyles;
var
  b : Boolean;
begin
  b:=false;
  Result := [];
  ATSUGetAttribute(AStyle, kATSUQDBoldfaceTag, sizeof(Boolean), @b, nil);
  if b then Include(Result, fsBold);
  ATSUGetAttribute(AStyle, kATSUQDItalicTag, sizeof(Boolean), @b, nil);
  if b then Include(Result, fsItalic);
  ATSUGetAttribute(AStyle, kATSUQDUnderlineTag, sizeof(Boolean), @b, nil);
  if b then Include(Result, fsUnderline);
  ATSUGetAttribute(AStyle, kATSUStyleStrikeThroughTag , sizeof(Boolean), @b, nil);
  if b then Include(Result, fsStrikeOut);
end;

function IsValidControlHandle(AWinControl: TWinControl): Boolean;
begin
  Result := Assigned(AWinControl) and (AWinControl.Handle<>0);
end;

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
  Attr.data.dataValue := FontStyleToQDStyle(FontStyle)
end;

procedure AttrSetATSUStyle(AStyle: ATSUStyle; var Attr: TXNTypeAttributes);
begin
  Attr.tag := kTXNATSUIStyle;
  Attr.size := kTXNATSUIStyleSize;
  Attr.data.dataPtr := astyle;
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
var
  memo  : TCarbonRichEdit;
  attr  : array [0..1] of TXNTypeAttributes;
  sstart  : Integer;
  slen    : Integer;
  flags   : TXNContinuousFlags;

  astyle  : ATSUStyle;

begin
  Result := IsValidControlHandle(AWinControl);
  if not Result then Exit;
  memo := TCarbonRichEdit(AWinControl.Handle);

  memo.GetSelStart(sstart);
  memo.GetSelLength(slen);

  memo.SetSelStart(TextStart);
  memo.SetSelLength(1);

  ATSUCreateStyle(astyle);
  AttrSetATSUStyle(astyle, attr[0]);
  AttrSetStyle([], attr[1]);

  Result := memo.GetContinuousTypeAttributes(flags, 2, attr);
  Params.Name := GetATSUFontName(astyle);
  Params.Color := GetATSUFontColor(astyle);
  Params.Style := GetATSUFontStyles(astyle) + QDStyleToFontStyle(attr[1].data.dataValue);
  Params.Size := GetATSUFontSize(astyle);

  ATSUDisposeStyle(astyle);

  memo.SetSelLength(sstart);
  memo.SetSelLength(slen);
end;

class procedure TCarbonWSCustomRichMemo.SetTextAttributes(const AWinControl: TWinControl;
  TextStart, TextLen: Integer;
  Mask: TTextStyleMask; const Params: TFontParams);
var
  memo      : TCarbonRichEdit;
  Attr      : array [0..TXNAttributesMax-1] of TXNTypeAttributes;
  Count     : Integer;
  maccolor  : RGBColor;
begin
  if not IsValidControlHandle(AWinControl) then Exit;
  memo := TCarbonRichEdit(AWinControl.Handle);

  ParamsToTXNAttribs(Mask, Params, attr, Count, maccolor);

  memo.SetTypeAttributes(Count, Attr, TextStart, TextStart+TextLen);
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

function TCarbonRichEdit.GetContinuousTypeAttributes(
  var oContinuousFlags: TXNContinuousFlags; iCount: ItemCount;
  var ioTypeAttributes: array of TXNTypeAttributes): Boolean;
begin
  Result := TXNGetContinuousTypeAttributes(HITextViewGetTXNObject(Widget),
              oContinuousFlags,  iCount, @ioTypeAttributes[0]) = noErr;
end;

function TCarbonRichEdit.SetTypeAttributes(iCount: ItemCount;
  const iTypeAttributes: array of TXNTypeAttributes; StartOffset,
  EndOffset: Integer): Boolean;
begin
  Result := TXNSetTypeAttributes(HITextViewGetTXNObject(Widget), iCount,
              @iTypeAttributes[0], StartOffset, EndOffset) = noErr;
end;

end.

