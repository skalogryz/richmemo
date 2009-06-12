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
    function GetIndexedRunInfoFromRange(iIndex: ItemCount;   iStartOffset, iEndOffset: TXNOffset;
      var oRunStartOffset, oRunEndOffset: TXNOffset;
      oRunDataType: TXNDataTypePtr; iTypeAttributeCount: ItemCount;
      ioTypeAttributes: TXNTypeAttributesPtr): Boolean;
    function GetContinuousTypeAttributes(var oContinuousFlags: TXNContinuousFlags;
      iCount: ItemCount; var ioTypeAttributes: array of TXNTypeAttributes): Boolean;
    function SetTypeAttributes(iCount: ItemCount; const iTypeAttributes: array of TXNTypeAttributes;
      StartOffset, EndOffset: Integer): Boolean;
  end;

  { TCarbonWSCustomRichMemo }

  TCarbonWSCustomRichMemo = class(TWSCustomRichMemo)
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND; override;
    class function GetStyleRange(const AWinControl: TWinControl; TextStart: Integer;
      var RangeStart, RangeLen: Integer): Boolean; override;
    class function GetTextAttributes(const AWinControl: TWinControl; TextStart: Integer;
      var Params: TFontParams): Boolean; override;
    class procedure SetTextAttributes(const AWinControl: TWinControl; TextStart, TextLen: Integer;
      Mask: TTextStyleMask; const Params: TFontParams); override;
    class procedure SetHideSelection(const AWinControl: TWinControl; AHideSelection: Boolean); override;
    class function LoadRichText(const AWinControl: TWinControl; Src: TStream): Boolean; override;
    class function SaveRichText(const AWinControl: TWinControl; Dst: TStream): Boolean; override;
  end;

implementation

// Notes:

// http://developer.apple.com/DOCUMENTATION/Carbon/Reference/Multilingual_Text_Engine/Reference/reference.html
// TXNFlattenObjectToCFDataRef
//
//oDataRef
//   On input, points to a structure of type CFDataRef. On output, points to a flattened
//   version of the text object in the format specified by the iTXNDataType parameter.
//   You are responsible to retain the returned CFDataRef.
//
// It's unclear (though expected), if a user is responsible to release returned CFData object.
//
// Releasing is necessary, as noted this mailling list discussion
// http://lists.apple.com/archives/carbon-dev/2005/Feb/msg00657.html


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

function GetValidRichEdit(AWinControl: TWinControl): TCarbonRichEdit;
begin
  if Assigned(AWinControl) and (AWinControl.Handle<>0) and (TObject(AWinControl.Handle) is TCarbonRichEdit) then
    Result := TCarbonRichEdit(AWinControl.Handle)
  else
    Result := nil;
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

class function TCarbonWSCustomRichMemo.GetStyleRange(const AWinControl: TWinControl;
  TextStart: Integer; var RangeStart, RangeLen: Integer): Boolean;
var
  edit      : TCarbonRichEdit;
  obj       : TXNObject;
  sst, slen : Integer;
  st, len   : Integer;
  send      : Integer;
  fndstyle  : Boolean;
  wattr     : array [0..1] of TXNTypeAttributes;
  attr      : array [0..1] of TXNTypeAttributes;
  astyle    : ATSUStyle;
  flags     : TXNContinuousFlags;
  d         : Integer;
  macrgb    : RGBColor;
  RngStart  : TXNOffset;
  RngEnd    : TXNOffset;
begin
  Result := false;
  edit := GetValidRichEdit(AWinControl);
  if not Assigned(edit) then Exit;

  Result := edit.GetIndexedRunInfoFromRange(0, TextStart, TextStart+1, RngStart, RngEnd, nil, 0, nil);
  if Result then begin
    RangeStart := RngStart;
    RangeLen := RngEnd - RngStart;
  end;

{  edit.GetSelStart(sst);
  edit.GetSelLength(slen);

  edit.SetSelStart(TextStart);
  edit.SetSelLength(1);

  ATSUCreateStyle(astyle);
  AttrSetATSUStyle(nil, wattr[0]);
  AttrSetColor(macrgb, wattr[1]);
  edit.GetContinuousTypeAttributes(flags, 2, wattr[0]);

  GetTextLen(AWinControl, len);
  dec(len, TextStart);
  st:=TextStart;

  writeln('TextStart  = ', TextStart);
  writeln('TextLength = ', 2);
  edit.SetSelStart(TextStart);
  edit.SetSelLength(2);//send-TextStart);
  attr := wattr;
  edit.GetContinuousTypeAttributes(flags, 2, attr);

  writeln('contflags ',flags);

  Result := true;}

{  fndstyle := false;
  while not fndstyle do begin
    edit.SetSelStart(st);
    edit.SetSelLength(len);
    attr := wattr;

    send := st + len;
    repeat
      writeln(st,' ', send);
      d := (st+send) div 2; {разделить пополам интервал просмотра}
      edit.SetSelStart(TextStart);
      edit.SetSelLength(send-TextStart);
      attr := wattr;
      edit.GetContinuousTypeAttributes(flags, 2, attr);
      if flags = (kTXNColorContinuousMask or kTXNATSUIStyleContinuousMask) then
        st := st+1
      else
        send := d-1;
    until (st>send);}

{    while send > st do begin
      writeln(st, ' ', send, ' ', send - st, ' ', (send - st) div 2);
      attr := wattr;
      edit.SetSelStart(st);
      edit.SetSelLength(st+len);
      edit.GetContinuousTypeAttributes(flags, 2, attr);
      writeln('   flags = ', flags);
      {d := (send - st) div 2;
      if d = 0 then d := 1;}
      if flags = (kTXNColorContinuousMask or kTXNATSUIStyleContinuousMask)
        then st := send
        else dec(send, d);
    end;
    Result := send - TextStart;}
 // end;

{  edit.SetSelStart(sst);
  edit.SetSelLength(slen);
  ATSUDisposeStyle(astyle);}
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
  Result := false;
  memo := GetValidRichEdit(AWinControl);
  if not Assigned(memo) then Exit;

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
  memo := GetValidRichEdit(AWinControl);
  if not Assigned(memo) then Exit;

  ParamsToTXNAttribs(Mask, Params, attr, Count, maccolor);

  memo.SetTypeAttributes(Count, Attr, TextStart, TextStart+TextLen);
end;

class procedure TCarbonWSCustomRichMemo.SetHideSelection(const AWinControl: TWinControl;
  AHideSelection: Boolean);
begin

end;

function GetTempFileUniqueName(forcedir: Boolean=true): String;
var
  g : TGUID;
  d : String;
begin
  repeat
    CreateGUID(g);
    Result := GetTempFileName +  GUIDToString(g) +'.rtf';
  until not FileExists(Result);
  if forcedir then begin
    d := ExtractFileDir(Result);
    if not DirectoryExists(d) then ForceDirectories(d);
  end;
end;

class function TCarbonWSCustomRichMemo.LoadRichText(const AWinControl: TWinControl;
  Src: TStream): Boolean;
var
  edit  : TCarbonRichEdit;
  filename: String;
  fs  : TFileStream;
  cf  : CFStringRef;
  url : CFURLRef;
  res : integer;
begin
  Result := false;
  edit := GetValidRichEdit(AWinControl);
  if not Assigned(edit) then Exit;

  Result := false;
  filename := GetTempFileUniqueName;

  try
    fs := TFileStream.Create(filename, fmCreate);
    try
      fs.CopyFrom(Src, Src.Size - Src.Position);
    finally
      fs.Free;
    end;

    CreateCFString(filename, cf);
    url := CFURLCreateWithFileSystemPath (kCFAllocatorDefault, cf, kCFURLPOSIXPathStyle, false);
    try
      TXNSelectAll(HITextViewGetTXNObject(edit.Widget));
      Result := TXNSetDataFromCFURLRef( HITextViewGetTXNObject(edit.Widget), url, kTXNStartOffset, kTXNEndOffset) = noErr;
    finally
      CFRelease(url);
      FreeCFString(cf);
    end;

  except
    Result := false;
  end;

  if FileExists(filename) then DeleteFile(filename);
end;

class function TCarbonWSCustomRichMemo.SaveRichText(const AWinControl: TWinControl;
  Dst: TStream): Boolean;
var
  edit  : TCarbonRichEdit;
  data  : CFDataRef;
  sz    : Integer;
  ptr   : PByteArray;
begin
  Result := false;
  edit := GetValidRichEdit(AWinControl);
  if not Assigned(edit) or not Assigned(Dst) then Exit;

  Result := TXNFlattenObjectToCFDataRef(HITextViewGetTXNObject(edit.Widget), kTXNRichTextFormatData, data) = noErr;
  if not Result and Assigned(data) then Exit;

  try
    sz := CFDataGetLength(data);
    ptr := CFDataGetBytePtr(data);
    if Assigned(ptr) and (sz > 0) then Dst.Write(ptr^, sz);
    Result := false;
  finally
    // see TXNFlattenObjectToCFDataRef notes
    CFRelease(data);
  end;

end;

{ TCarbonRichEdit }

function TCarbonRichEdit.GetCreationOptions: TXNFrameOptions;
begin
  Result := kOutputTextInUnicodeEncodingMask;
end;

function TCarbonRichEdit.GetIndexedRunInfoFromRange(iIndex: ItemCount;
  iStartOffset, iEndOffset: TXNOffset;
  var oRunStartOffset, oRunEndOffset: TXNOffset;
  oRunDataType: TXNDataTypePtr; iTypeAttributeCount: ItemCount;
  ioTypeAttributes: TXNTypeAttributesPtr): Boolean;
begin
  Result := TXNGetIndexedRunInfoFromRange( HITextViewGetTXNObject(Widget),
    iIndex, iStartOffset, iEndOffset, @oRunStartOffset, @oRunEndOffset,
    oRunDataType, iTypeAttributeCount, ioTypeAttributes ) = noErr;
end;

function TCarbonRichEdit.GetContinuousTypeAttributes(
  var oContinuousFlags: TXNContinuousFlags; iCount: ItemCount;
  var ioTypeAttributes: array of TXNTypeAttributes): Boolean;
var
  res : OSStatus;
begin
  Result := TXNGetContinuousTypeAttributes(HITextViewGetTXNObject(Widget),
              oContinuousFlags, iCount, @ioTypeAttributes[0]) = noErr;
end;

function TCarbonRichEdit.SetTypeAttributes(iCount: ItemCount;
  const iTypeAttributes: array of TXNTypeAttributes; StartOffset,
  EndOffset: Integer): Boolean;
begin
  Result := TXNSetTypeAttributes(HITextViewGetTXNObject(Widget), iCount,
              @iTypeAttributes[0], StartOffset, EndOffset) = noErr;
end;




end.

