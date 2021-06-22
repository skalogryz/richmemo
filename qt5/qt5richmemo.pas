unit qt5richmemo;

interface

{$mode delphi}

{$define RMQT5_TEXTFORMATS} // the feature is available in Qt5 Trunk
                            // it allows to query Qt5TextEdit for character formats array
{$ifdef RMQT5_NOTEXTFORMATS}{$undef RMQT5_TEXTFORMATS}{$endif}

//
// Following class methods are need for the implementation
//  QTextCharFormatH
//  QTextBlockFormatH
uses
  SysUtils, Math, LCLType, Controls, StdCtrls, Graphics,
  qt5, qtobjects, qtwidgets, qtprivate,
  WSProc,
  RichMemo, WSRichMemo;

type
  TCustomRichMemoInt   = class(TCustomRichMemo);

  { TQtRichTextEdit }

  TQtRichTextEdit = class(TQtTextEdit)
  private
    anchor: Widestring;
    FSelectionChangedHook: QTextEdit_hookH;
    FCursorPositionChangedHook: QTextEdit_hookH;
  public
    procedure AttachEvents; override;
    procedure DetachEvents; override;
    function SlotMouse(Sender: QObjectH; Event: QEventH): Boolean; override; cdecl;
    procedure SignalSelectionChanged(); cdecl;
    procedure SignalCursorPositionChanged(); cdecl;
  end;

  { TQtWSCustomRichMemo }

  TQtWSCustomRichMemo = class(TWSCustomRichMemo)
  published
    class function CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class function GetParaAlignment(const AWinControl: TWinControl; TextStart: Integer;
      var AAlign: TIntParaAlignment): Boolean; override;
    class procedure SetParaAlignment(const AWinControl: TWinControl; TextStart, TextLen: Integer;
      const AAlign: TIntParaAlignment); override;
    class function GetTextAttributes(const AWinControl: TWinControl; TextStart: Integer;
      var Params: TIntFontParams): Boolean; override;
    class procedure SetTextAttributes(const AWinControl: TWinControl; TextStart, TextLen: Integer;
      const Params: TIntFontParams); override;
    class function HandleKeyDown(const AWinControl: TWinControl; key:word): boolean; override;

    class function GetParaMetric(const AWinControl: TWinControl; TextStart: Integer;
      var AMetric: TIntParaMetric): Boolean; override;
    class procedure SetParaMetric(const AWinControl: TWinControl; TextStart, TextLen: Integer;
      const AMetric: TIntParaMetric); override;

    class function GetParaNumbering(const AWinControl: TWinControl; TextStart: Integer;
      var ANumber: TIntParaNumbering): Boolean; override;
    class procedure SetParaNumbering(const AWinControl: TWinControl; TextStart, TextLen: Integer;
      const ANumber: TIntParaNumbering); override;

    class function GetParaRange(const AWinControl: TWinControl; TextStart: Integer; var rng: TParaRange): Boolean; override;
    class procedure InDelText(const AWinControl: TWinControl; const TextUTF8: String; DstStart, DstLen: Integer); override;
    class procedure SetTextUIParams(const AWinControl: TWinControl; TextStart, TextLen: Integer; const ui: TTextUIParam); override;
    class function GetTextUIParams(const AWinControl: TWinControl; TextStart: Integer; var ui: TTextUIParam): Boolean; override;

    class function Search(const AWinControl: TWinControl; const ANiddle: string; const SearchOpts: TIntSearchOpt): Integer; override;

    class function isInternalChange(const AWinControl: TWinControl; Params: TTextModifyMask
      ): Boolean; override;
    class procedure SetTextAttributesInternal(const AWinControl: TWinControl; TextStart, TextLen: Integer;
      const AModifyMask: TTextModifyMask; const Params: TIntFontParams); override;

    class function GetStyleRange(const AWinControl: TWinControl; TextStart: Integer; var RangeStart, RangeLen: Integer): Boolean; override;
  end;

type
  TEditorState = record
    selst  : integer; // selection start
    sellen : integer; // selection length
  end;

// no sanity check is done in these functions
procedure MakeBackup(te: TQtTextEdit; out backup: TEditorState);
procedure ApplyBackup(te: TQtTextEdit; const backup: TEditorState);

implementation

const
  WordWrapMap: array[Boolean] of QTextEditLineWrapMode =
  (
    QTextEditNoWrap,
    QTextEditWidgetWidth
  );

  AlignmentMap: array[TIntParaAlignment] of QtAlignment =
  (
    QtAlignLeft,
    QtAlignRight,
    QtAlignHCenter,
    QtAlignJustify
  );

const
  QNormal = 50;
  QBold   = 75;

function QBrushToColor(brush: QBrushH): TQColor; overload;
var
  aColor: PQColor;
begin
  aColor := QBrush_color(brush);
  if aColor<>nil then result := aColor^
  else                fillChar(result, sizeOf(TQColor), 0);
end;

function QBrushToColor(brush: QBrushH; out color: TQColor): boolean; overload;
var
  aColor: PQColor;
begin
  aColor := QBrush_color(brush);
  result := aColor<>nil;
  if result then
    color := aColor^;
end;

function SameColor(A, B: TQColor): boolean;
begin
  result := (a.r=b.r) and (a.g=b.g) and (a.b=b.b) and (a.Alpha=b.Alpha) and
            (a.ColorSpec=b.ColorSpec);
end;

function PrivateGetListItemNumber(cursor: QTextCursorH): Integer;
var
  block: QTextBlockH;
  list, lookList: QTextListH;
begin

  // QTextList functions are not available in the current Qt5pas
  // Library/Bindings. QTextList_itemNumber would return the bullet num.
  // and QTextList_itemText would return the bullet text
  result := 0;
  block := QTextBlock_create();
  QTextCursor_block(cursor, block);
  list := QTextBlock_textList(block);
  while QTextBlock_isValid(block) do begin
    lookList := QTextBlock_textList(block);
    if lookList<>list then
      break;
    Inc(result);
    QTextBlock_previous(block, block);
  end;
  QTextBlock_Destroy(block);

end;

function PrivateGetFormatRange(cursor: QTextCursorH; const start: Integer;
  out rangeStart, rangeEnd: Integer): boolean;
var
  fmtRef, fmtCur: QTextCharFormatH;
  refIsAnchor: Boolean;
  refHRef, refFont: WideString;
  font: QFontH;
  refBackBrushStyle: QtBrushStyle;
  refHasBackColor: boolean;
  refForeColor, refBackColor: TQColor;

  refList: QTextListH;
  refListItem: Integer;

  brush: QBrushH;
  doc: QTextDocumentH;


  function SameFormats: boolean;
  var
    tmpStr: wideString;
    hasBackColor: boolean;
    backColor: TQColor;
    list: QTextListH;
    listItem: Integer;
  begin

    // deal with list items
    list := QTextCursor_currentList(cursor);
    result := (list=refList);
    if not result then exit;
    if list<>nil then begin
      listItem := PrivateGetListItemNumber(cursor);
      result := listItem=refListItem;
      if not result then exit;
    end;

    // deal with hyperlinks
    result := QTextCharFormat_isAnchor(fmtCur)=refIsAnchor;
    if not result then exit;
    if refIsAnchor then begin
      QTextCharFormat_anchorHref(fmtCur, @tmpStr);
      result := tmpStr=refHRef;
      exit;
    end;

    // text color
    QTextFormat_foreground(QTextFormatH(fmtCur), brush);
    result := SameColor(QBrushToColor(brush), refForeColor);
    if not result then exit;

    // background color
    QTextFormat_background(QTextFormatH(fmtCur), brush);
    result := QBrush_style(brush)=refBackBrushStyle;
    if not result then exit;
    hasBackColor := QBrushToColor(brush, backColor);
    result := refHasBackColor=hasBackColor;
    if not result then exit;
    if hasBackColor then begin
      result := SameColor(backColor, refBackColor);
      if not result then exit;
    end;

    // deal with formats
    QTextCharFormat_font(fmtCur, font);
    QFont_toString(font, @tmpStr);
    result := tmpStr=refFont;
  end;

begin
  result := false;

  doc := QTextCursor_document(cursor);
  if start + 1 >= QTextDocument_characterCount(doc) then
    exit;

  QTextCursor_setPosition(cursor, start + 1);

  rangeStart := start;
  rangeEnd   := rangeStart;

  refList := QTextCursor_currentList(cursor);
  if refList<>nil then
    refListItem := PrivateGetListItemNumber(cursor);

  fmtRef := QTextCharFormat_Create();
  fmtCur := QTextCharFormat_Create();

  QTextCursor_charFormat(cursor, fmtRef);

  QTextCharFormat_anchorHref(fmtRef, @refHRef);
  refIsAnchor := QTextCharFormat_isAnchor(fmtRef);
  if not refIsAnchor then begin
    font := QFont_Create();
    brush := QBrush_Create();
    QTextCharFormat_font(fmtRef, font);
    QFont_toString(font, @refFont);

    QTextFormat_foreground(QTextFormatH(fmtRef), brush);
    refForeColor := QBrushToColor(brush);
    QTextFormat_background(QTextFormatH(fmtRef), brush);
    refBackBrushStyle := QBrush_style(brush);
    refHasBackColor := QBrushToColor(brush, refBackColor);
  end;

  // find left limit
  while QTextCursor_movePosition(cursor, QTextCursorPreviousCharacter) do begin
    QTextCursor_charFormat(cursor, fmtCur);
    if not SameFormats then
      break;
    if rangeStart>0 then
      Dec(rangeStart);
  end;

  // find right limit
  QTextCursor_setPosition(cursor, start + 1);
  while QTextCursor_movePosition(cursor, QTextCursorNextCharacter) do begin
    QTextCursor_charFormat(cursor, fmtCur);
    if not SameFormats then
      break;
    inc(rangeEnd);
  end;

  if not refIsAnchor then begin
    QFont_Destroy(font);
    QBrush_Destroy(brush);
  end;
  QTextCharFormat_Destroy(fmtRef);
  QTextCharFormat_Destroy(fmtCur);

  result := true;
end;

function PrivateGetTextUIParams(w: QTextEditH; textStart: Integer; out UI: TTextUIParam): boolean;
var
  tc: QTextCursorH;
  address: UnicodeString;
  fmt: QTextCharFormatH;
  doc: QTextDocumentH;
begin
  result := false;
  tc := QTextCursor_create();

  QTextEdit_textCursor(w, tc);
  doc := QTextCursor_document(tc);

  if TextStart + 1 < QTextDocument_characterCount(doc) then
  begin
    QTextCursor_setPosition(tc, TextStart + 1);
    fmt := QTextCharFormat_Create();
    QTextCursor_charFormat(tc, fmt);
    if QTextCharFormat_isAnchor(fmt) then
    begin
      QTextCharFormat_anchorHref(fmt, @address);
      ui.linkref := UTF8Encode(address);
      include(ui.features, uiLink);
      result := true;
    end;
    QTextCharFormat_Destroy(fmt);
  end;

  QTextCursor_Destroy(tc);
end;

{$IFDEF HASDUMP}
procedure DumpRichmemo(const AWincontrol: TWinControl);
var
  te: TQtTextEdit;
  w: QTextEditH;
  str: widestring;
  tc: QTextCursorH;
  aPos: Integer;
  fmt: string;

  function ToStrSpecial(s:string): string;
  var
    i: Integer;
  begin
    result := '';
    for i:=1 to Length(s) do begin
      if s[i] in [#0..#31,#128..#255] then
        result += format('#%d(%:0.2x)',[ord(s[i])])
      else
        result += s[i];
    end;
  end;

  procedure DescribeFormat;
  var
    aList: QTextListH;
    ws: Widestring;
    item: Integer;
    fmtRef: QTextCharFormatH;
  begin
    fmt := '';
    aList := QTextCursor_currentList(tc);
    if aList<>nil then begin
      item := PrivateGetListItemNumber(tc);
      fmt += 'Item #'+IntToStr(item)+':';
    end;

    fmtRef := QTextCharFormat_Create();
    QTextCursor_charFormat(tc, fmtRef);
    if QTextCharFormat_isAnchor(fmtRef) then begin
      QTextCharFormat_anchorHref(fmtRef, @ws);
      fmt += 'link: '+ UTF8Encode(ws);
    end;
    QTextCharFormat_Destroy(fmtRef);
    //

    ws:='';
    QTextEdit_fontFamily(w, @ws);
    fmt += 'Font:"';
    if ws<>'' then
      fmt += UTF8Encode(ws);
    fmt += '"';
    fmt += format(',%f',[QTextEdit_fontPointSize(w)]);
    if QTextEdit_fontWeight(w)>=QBold then fmt += ',b';
    if QTextEdit_fontItalic(w) then fmt += ',i';
    if QTextEdit_fontUnderline(w) then fmt += ',u';
  end;

begin
  if not WSCheckHandleAllocated(AWinControl, 'Dump') then
    Exit;

  te:=TQtTextEdit(AWinControl.Handle);
  w:=QTextEditH(te.Widget);
  tc := QTextCursor_Create();
  QTextEdit_textCursor(w, tc);

  QTextCursor_movePosition(tc, QTextCursorStart);
  DescribeFormat;
  aPos := QTextCursor_position(tc);
  while QTextCursor_movePosition(tc, QTextCursorNextCharacter, QTextCursorKeepAnchor) do begin
    QTextCursor_selectedText(tc, @str);
    WriteLn(format('%d: %s -> %s',[aPos, ToStrSpecial(UTF8Encode(str)), fmt]));
    aPos := QTextCursor_position(tc);
    QTextCursor_setPosition(tc, aPos);
    DescribeFormat;
  end;

  QTextCursor_destroy(tc);
end;
{$ENDIF}

{ TQtRichTextEdit }

procedure TQtRichTextEdit.AttachEvents;
begin
  inherited AttachEvents;

  FSelectionChangedHook := QTextEdit_hook_create(Widget);
  QTextEdit_hook_hook_selectionChanged(FSelectionChangedHook, SignalSelectionChanged);

  FCursorPositionChangedHook := QTextEdit_hook_create(Widget);
  QTextEdit_hook_hook_cursorPositionChanged(FCursorPositionChangedHook, SignalCursorPositionChanged);
end;

procedure TQtRichTextEdit.DetachEvents;
begin

  if FSelectionChangedHook <> nil then
  begin
    QObject_hook_Destroy(FSelectionChangedHook);
    FSelectionChangedHook := nil;
  end;

  if FCursorPositionChangedHook <> nil then
  begin
    QObject_hook_Destroy(FCursorPositionChangedHook);
    FCursorPositionChangedHook := nil;
  end;

  inherited DetachEvents;
end;

function TQtRichTextEdit.SlotMouse(Sender: QObjectH; Event: QEventH): Boolean;
  cdecl;
var
  Pos: TQtPoint;
  tc: QTextCursorH;
  rangeStart, rangeEnd: Integer;
  mi: TLinkMouseInfo;
  ui: TTextUIParam;
  w: QTextEditH;
begin
  if (LCLObject is TCustomRichMemo) then begin
    QMouseEvent_pos(QMouseEventH(Event), @Pos);
    case QEvent_type(Event) of
      QEventMouseButtonPress:
        begin
          Anchor := '';
          QTextEdit_anchorAt(QTextEditH(widget), @Anchor, @Pos);
        end;
      QEventMouseButtonRelease:
        if Anchor<>'' then begin
          w := QTextEditH(widget);
          tc := QTextCursor_Create();
          QTextEdit_cursorForPosition(w, tc, @pos);
          rangeStart := QTextCursor_position(tc);
          mi.LinkRef:=UTF8Encode(Anchor);
          mi.Button:=mbLeft;
          // Qt reports there is an anchor at the extreme right pixel of the link
          // but sometimes this corresponds to an offset out of the style range
          // report clicks only for offsets that are within style range.
          if PrivateGetTextUIParams(w, rangeStart, ui) and (uiLink in ui.features) then begin
            if PrivateGetFormatRange(tc, rangeStart, rangeStart, rangeEnd) then
              TCustomRichMemoInt(LCLObject).doLinkAction(laClick, mi, rangeStart, rangeEnd-rangeStart+1);
          end;
          QTextCursor_Destroy(tc);
          Anchor := '';
        end;
    end;
  end;
  Result:=inherited SlotMouse(Sender, Event);
end;

procedure TQtRichTextEdit.SignalSelectionChanged(); cdecl;
begin
  if (LCLObject is TCustomRichMemo) then
    TCustomRichMemoInt(LCLObject).DoSelectionChange();
end;

procedure TQtRichTextEdit.SignalCursorPositionChanged(); cdecl;
begin
  if (LCLObject is TCustomRichMemo) then
    TCustomRichMemoInt(LCLObject).DoSelectionChange();
end;

{ TQtWSCustomRichMemo }

class function TQtWSCustomRichMemo.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  QtTextEdit: TQtRichTextEdit;
begin
  QtTextEdit := TQtRichTextEdit.Create(AWinControl, AParams);
  QtTextEdit.AcceptRichText := True;
  QtTextEdit.ClearText;
  QtTextEdit.setBorder(TCustomMemo(AWinControl).BorderStyle = bsSingle);
  QtTextEdit.setReadOnly(TCustomMemo(AWinControl).ReadOnly);
  QtTextEdit.setLineWrapMode(WordWrapMap[TCustomMemo(AWinControl).WordWrap]);
  // create our FList helper
  QtTextEdit.FList := TQtMemoStrings.Create(TCustomMemo(AWinControl));
  QtTextEdit.setScrollStyle(TCustomMemo(AWinControl).ScrollBars);
  QtTextEdit.setTabChangesFocus(not TCustomMemo(AWinControl).WantTabs);
  QtTextEdit.AttachEvents;
  QTextEdit_setTabStopWidth(QTextEditH(QtTextEdit.Widget), 55);

  Result := TLCLIntfHandle(QtTextEdit);
end;

class procedure TQtWSCustomRichMemo.SetParaAlignment(
  const AWinControl: TWinControl; TextStart, TextLen: Integer;
  const AAlign: TIntParaAlignment);
var
  w : QTextEditH;
  te : TQtTextEdit;
  ss, sl :  Integer;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetParaAlignment') then
    Exit;
  te:=TQtTextEdit(AWinControl.Handle);
  w:=QTextEditH(te.Widget);

  ss:=te.getSelectionStart;
  sl:=te.getSelectionLength;

  te.setSelection(TextStart, TextLen);

  // alignment
  QTextEdit_setAlignment(w, AlignmentMap[AAlign]);

  te.setSelection(ss, sl);
end;

class function TQtWSCustomRichMemo.GetTextAttributes(
  const AWinControl: TWinControl; TextStart: Integer; var Params: TIntFontParams
  ): Boolean;
var
  w : QTextEditH;
  te : TQtTextEdit;
  ws : WideString;
  clr: TQColor;
  bck : TEditorState;
  tc: QTextCursorH;
  fmt: QTextCharFormatH;
  brush: QBrushH;
  brushStyle: QtBrushStyle;
  font: QFontH;
begin
  InitFontParams(Params);
  if not WSCheckHandleAllocated(AWinControl, 'GetTextAttributes') then begin
    Result:=false;
    Exit;
  end;

  te:=TQtTextEdit(AWinControl.Handle);
  w:=QTextEditH(te.Widget);

  MakeBackup(te, bck);

  // If selstart is at the end of the line (before the EOL char) and we set Len=1
  // it would move the cursor at the begining of the next block which would make
  // that the retrieved attibutes do not match the previous block attributes
  // (the expected behaviour) but the attributes of the next block which is wrong

  tc := QTextCursor_Create();
  QTextEdit_textCursor(w, tc);

  QTextCursor_setPosition(tc, TextStart);
  if not QTextCursor_atBlockEnd(tc) then
    QTextCursor_setPosition(tc, TextStart + 1, QTextCursorKeepAnchor);

  fmt := QTextCharFormat_Create();
  QTextCursor_charFormat(tc, fmt);

  font := QFont_Create();
  QTextCharFormat_font(fmt, font);

  ws:='';
  QFont_family(font, @ws);
  if ws<>'' then Params.Name:=UTF8Encode(ws);
  Params.Size := QFont_pointSize(font);
  if QFont_bold(font) then Include(Params.Style, fsBold);
  if QFont_italic(font) then Include(Params.Style, fsItalic);
  if QFont_underline(font) then Include(Params.Style, fsUnderline);
  QFont_Destroy(font);

  brush := QBrush_Create();
  QTextFormat_foreground(QTextFormatH(fmt), brush);
  QBrushToColor(brush, clr);
  TQColorToColorRef(clr, TColorRef(params.Color));

  QTextFormat_background(QTextFormatH(fmt), brush);
  brushStyle := QBrush_style(brush);
  params.HasBkClr := (brushStyle=QtSolidPattern) and QBrushToColor(brush, clr);
  if params.HasBkClr then
    TQColorToColorRef(clr, TColorRef(params.BkColor));

  QBrush_Destroy(brush);
  QTextCharFormat_Destroy(fmt);
  QTextCursor_Destroy(tc);

  Result:=true;
end;

class procedure TQtWSCustomRichMemo.SetTextAttributes(
  const AWinControl: TWinControl; TextStart, TextLen: Integer;
  const Params: TIntFontParams);
var
  w : QTextEditH;
  te : TQtTextEdit;
  ss, sl :  Integer;
  ws : WideString;
  clr: TQColor;
  tc: QTextCursorH;
  fmt: QTextCharFormatH;
  Brush: QBrushH;
const
  QIsBold: array [Boolean] of integer = (QNormal, QBold);
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetTextAttributes') then
    Exit;
  te:=TQtTextEdit(AWinControl.Handle);
  w:=QTextEditH(te.Widget);

  ss:=te.getSelectionStart;
  sl:=te.getSelectionLength;

  te.setSelection(TextStart, TextLen);

  tc := QTextCursor_Create();
  fmt := QTextCharFormat_Create();

  QTextEdit_textCursor(w, tc);
  QTextCursor_charFormat(tc, fmt);

  ws:=UTF8Decode(Params.Name);
  if ws<>'' then QTextCharFormat_setFontFamily(fmt, @ws);
  if Params.Size>0 then QTextCharFormat_setFontPointSize(fmt, Params.Size);
  QTextCharFormat_setFontUnderline(fmt, fsUnderline in Params.Style);
  QTextCharFormat_setFontWeight(fmt, QisBold[fsBold in Params.Style]);
  QTextCharFormat_setFontItalic(fmt, fsItalic in Params.Style);

  ColorRefToTQColor(ColorToRGB(Params.Color), clr);
  Brush := QBrush_create(@clr, QtSolidPattern);
  QTextFormat_setForeground(QTextFormatH(fmt), brush);

  if Params.HasBkClr then begin
    ColorRefToTQColor(ColorToRGB(Params.BkColor), clr);
    QBrush_setColor(brush, @clr);
  end else
    QBrush_setStyle(brush, QtNoBrush);
  QTextFormat_setBackground(QTextFormatH(fmt), brush);
  QBrush_Destroy(Brush);

  QTextCharFormat_setAnchor(fmt, false);
  ws := '';
  QTextCharFormat_setAnchorHref(fmt, @ws);

  QTextCursor_setCharFormat(tc, fmt);
  QTextEdit_setTextCursor(w, tc);

  QTextCharFormat_Destroy(fmt);
  QTextCursor_Destroy(tc);

  te.setSelection(ss, sl);
end;

class function TQtWSCustomRichMemo.HandleKeyDown(
  const AWinControl: TWinControl; key: word): boolean;
var
  te: TQtTextEdit;
  w: QTextEditH;
  tc: QTextCursorH;
  block: QTextBlockH;
  fmt: QTextBlockFormatH;
  itemText: widestring;
begin
  Result:=false; // all key is handled by qt by default
  if key=VK_RETURN then begin
    te:=TQtTextEdit(AWinControl.Handle);
    w:=QTextEditH(te.Widget);
    tc := QTextCursor_Create();
    QTextEdit_textCursor(w, tc);
    if QTextCursor_currentList(tc)<>nil then begin
      block := QTextBlock_create();
      QTextCursor_block(tc, block);
      QTextBlock_text(block, @itemText);
      if itemText='' then begin
        fmt := QTextBlockFormat_Create();
        QTextBlock_blockFormat(block, fmt);
        QTextFormat_setObjectIndex(QTextFormatH(fmt), -1);
        QTextCursor_setBlockFormat(tc, fmt);
        QTextBlockFormat_Destroy(fmt);
        Result := true;
      end;
      QTextBlock_Destroy(block);
    end;
    QTextCursor_Destroy(tc);
  end;
end;

class function TQtWSCustomRichMemo.GetParaMetric(
  const AWinControl: TWinControl; TextStart: Integer;
  var AMetric: TIntParaMetric): Boolean;
var
  te: TQtTextEdit;
  w: QTextEditH;
  fmt: QTextBlockFormatH;
  tc: QTextCursorH;
  pixToPt: double;
begin
  result := WSCheckHandleAllocated(AWinControl, 'GetParaMetric');
  if not result then
    exit;

  te:=TQtTextEdit(AWinControl.Handle);
  w:=QTextEditH(te.Widget);

  tc := QTextCursor_Create();
  fmt := QTextBlockFormat_Create();

  QTextEdit_textCursor(w, tc);

  QTextCursor_setPosition(tc, TextStart);
  QTextCursor_blockFormat(tc, fmt);

  pixToPt := ifThen(ScreenDPI>0, PageDPI/ScreenDPI, 1.0);
  AMetric.FirstLine   := QTextBlockFormat_textIndent(fmt) * pixToPt;
  AMetric.HeadIndent  := QTextBlockFormat_leftMargin(fmt) * pixToPt;
  AMetric.TailIndent  := QTextBlockFormat_rightMargin(fmt) * pixToPt;
  AMetric.SpaceBefore := QTextBlockFormat_topMargin(fmt) * pixToPt;
  AMetric.SpaceAfter  := QTextBlockFormat_bottomMargin(fmt) * pixToPt;
  if QTextBlockFormat_lineHeightType(fmt)=ord(QTextBlockFormatSingleHeight) then
    AMetric.LineSpacing := SingleLineSpacing
  else if QTextBlockFormat_lineHeightType(fmt)=ord(QTextBlockFormatProportionalHeight) then
    AMetric.LineSpacing := QTextBlockFormat_lineHeight(fmt)/100*DefLineSpacing
  else
    AMetric.LineSpacing := DefLineSpacing;
  QTextBlockFormat_Destroy(fmt);
  QTextCursor_Destroy(tc);
end;

class procedure TQtWSCustomRichMemo.SetParaMetric(
  const AWinControl: TWinControl; TextStart, TextLen: Integer;
  const AMetric: TIntParaMetric);
var
  te: TQtTextEdit;
  w: QTextEditH;
  fmt: QTextBlockFormatH;
  tc: QTextCursorH;
  doc: QTextDocumentH;
  maxPosition: Integer;
  PtToPix: double;
begin

  if not WSCheckHandleAllocated(AWinControl, 'SetParaMetric') then
    exit;

  te:=TQtTextEdit(AWinControl.Handle);
  w:=QTextEditH(te.Widget);

  tc := QTextCursor_Create();
  fmt := QTextBlockFormat_Create();

  QTextEdit_textCursor(w, tc);

  doc := QTextCursor_document(tc);
  maxPosition := QTextDocument_characterCount(doc) - 1;
  QTextCursor_setPosition(tc, Min(TextStart, maxPosition));
  QTextCursor_blockFormat(tc, fmt);

  ptToPix := ifThen(ScreenDPI>0, ScreenDPI/PageDPI, 1.0);
  with AMetric do begin
    QTextBlockFormat_setTextIndent(fmt, (FirstLine-HeadIndent)*ptToPix);
    QTextBlockFormat_setLeftMargin(fmt, HeadIndent*ptToPix);
    QTextBlockFormat_setRightMargin(fmt, TailIndent*ptToPix);
    QTextBlockFormat_setTopMargin(fmt, SpaceBefore*ptToPix);
    QTextBlockFormat_setBottomMargin(fmt, SpaceAfter*ptToPix);
    if LineSpacing=SingleLineSpacing then
      QTextBlockFormat_setLineHeight(fmt, 0, ord(QTextBlockFormatSingleHeight))
    else
      QTextBlockFormat_setLineHeight(fmt, LineSpacing/DefLineSpacing*100,
                                          ord(QTextBlockFormatProportionalHeight));
  end;

  QTextCursor_setBlockFormat(tc, fmt);

  QTextBlockFormat_Destroy(fmt);
  QTextCursor_Destroy(tc);
end;

class function TQtWSCustomRichMemo.GetParaNumbering(
  const AWinControl: TWinControl; TextStart: Integer;
  var ANumber: TIntParaNumbering): Boolean;
var
  te: TQtTextEdit;
  w: QTextEditH;
  tc: QTextCursorH;
  list: QTextListH;
  listFmt: QTextListFormatH;
  style: QTextListFormatStyle;
  suffix: Widestring;
begin
  result := false;
  if not WSCheckHandleAllocated(AWinControl, 'GetParaNumbering') then
    Exit;

  te:=TQtTextEdit(AWinControl.Handle);
  w:=QTextEditH(te.Widget);
  tc := QTextCursor_Create();
  QTextEdit_textCursor(w, tc);
  QTextCursor_setPosition(tc, TextStart);

  InitParaNumbering(ANumber);
  result := true;

  list := QTextCursor_currentList(tc);
  if list=nil then exit;

  listFmt := QTextListFormat_Create();
  QTextObject_Format(QTextObjectH(list), QTextFormatH(listFmt));

  style := QTextListFormat_style(listFmt);
  case style of
    QTextListFormatListUpperRoman..QTextListFormatListDecimal:
      begin
        case style of
          QTextListFormatListUpperRoman: ANumber.Style := pnUpRoman;
          QTextListFormatListLowerRoman: ANumber.Style := pnLowRoman;
          QTextListFormatListUpperAlpha: ANumber.Style := pnUpLetter;
          QTextListFormatListLowerAlpha: ANumber.Style := pnLowLetter;
          QTextListFormatListDecimal: ANumber.Style := pnNumber;
        end;
        QTextListFormat_numberSuffix(listFmt, @Suffix);
        ANumber.SepChar := suffix[1];
        ANumber.NumberStart := PrivateGetlistItemNumber(tc);
        //WriteLn('ANumber.NumberStart=', ANumber.NumberStart);
      end;
    QTextListFormatListSquare,
    QTextListFormatListCircle,
    QTextListFormatListDisc: ANumber.Style := pnBullet;
    else
      ANumber.Style := pnNone;  //QTextListFormatListStyleUndefined;
  end;

  if ANumber.Style<>pnNone then begin
    ANumber.Indent := QTextListFormat_indent(listFmt);
    // TODO:
    // Qt indents are integer multiple of some default value in pixels and
    // this value basically indicates the list item level
    // while the rtfSpec indent means the distance between the list item number
    // or bullet and the list item text.
    // Qt indentation could be emulated with paragraph left-margin
    ANumber.Indent :=  ANumber.Indent * 280;
  end;

  QTextListFormat_Destroy(listFmt);
  QTextCursor_Destroy(tc);
end;

class procedure TQtWSCustomRichMemo.SetParaNumbering(
  const AWinControl: TWinControl; TextStart, TextLen: Integer;
  const ANumber: TIntParaNumbering);
var
  te: TQtTextEdit;
  w: QTextEditH;
  tc: QTextCursorH;
  list: QTextListH;
  listFmt: QTextListFormatH;
  style: QTextListFormatStyle;
  suffix: widestring;
  block: QTextBlockH;
  fmt: QTextBlockFormatH;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetParaNumbering') then
    Exit;

  case ANumber.Style of
    pnBullet:     style := QTextListFormatListDisc;
    pnNumber:     style := QTextListFormatListDecimal;
    pnLowLetter:  style := QTextListFormatListLowerAlpha;
    pnUpLetter:   style := QTextListFormatListUpperAlpha;
    pnLowRoman:   style := QTextListFormatListLowerRoman;
    pnUpRoman:    style := QTextListFormatListUpperRoman;
    pnCustomChar: style := QTextListFormatListStyleUndefined;
  end;

  te:=TQtTextEdit(AWinControl.Handle);
  w:=QTextEditH(te.Widget);
  tc := QTextCursor_Create();
  try
    QTextEdit_textCursor(w, tc);
    QTextCursor_setPosition(tc, TextStart);

    if ANumber.Style=pnNone then begin
      list := QTextCursor_currentList(tc);
      if (list<>nil) then begin
        // this is a list item that should converted to a no-list item.
        block := QTextBlock_Create();
        fmt := QTextBlockFormat_Create();
        QTextCursor_block(tc, block);
        QTextBlock_blockFormat(block, fmt);
        QTextFormat_setObjectIndex(QTextFormatH(fmt), -1);
        QTextCursor_setBlockFormat(tc, fmt);
        QTextBlockFormat_Destroy(fmt);
        QTextBlock_Destroy(block);
      end;
      exit;
    end;

    listFmt := QTextListFormat_Create();
    QTextListFormat_setStyle(listFmt, style);
    case ANumber.Style of
      pnBullet:;
      pnCustomChar:
        begin
          Suffix := ' ';
          QTextListFormat_setNumberSuffix(listFmt, @Suffix);
          Suffix := ANumber.CustomChar;
          QTextListFormat_setNumberPrefix(listFmt, @Suffix);
        end;
      else
        begin
          Suffix := ANumber.SepChar;
          QTextListFormat_setNumberSuffix(listFmt, @Suffix);
        end;
    end;

    QTextListFormat_setIndent(listFmt, 1);

    if TextLen>0 then
      QTextCursor_SetPosition(tc, TextStart+TextLen, QTextCursorKeepAnchor);

    list := QTextCursor_createList(tc, listFmt);

    QTextListFormat_Destroy(listFmt);

  finally
    QTextCursor_Destroy(tc);
  end;
end;

class function TQtWSCustomRichMemo.GetParaRange(const AWinControl: TWinControl;
  TextStart: Integer; var rng: TParaRange): Boolean;
var
  te : TQtTextEdit;
  tc : QTextCursorH;
begin
  result := false;
  rng.start := TextStart;
  rng.length := 0;
  rng.lengthNoBr := 0;

  if not WSCheckHandleAllocated(AWinControl, 'GetParaRange') then
    Exit;

  te:=TQtTextEdit(AWinControl.Handle);

  tc := QTextCursor_create();
  QTextEdit_textCursor(QTextEditH(te.Widget), tc);

  QTextCursor_setPosition(tc, TextStart);
  QTextCursor_movePosition(tc, QTextCursorStartOfBlock);
  rng.start:= QTextCursor_Position(tc);
  if QTextCursor_movePosition(tc, QTextCursorEndOfBlock) then
  begin
    rng.lengthNoBr := QTextCursor_position(tc) -  rng.start;
    if QTextCursor_movePosition(tc, QTextCursorNextCharacter) then
      rng.length := QTextCursor_position(tc) -  rng.start
    else
      rng.length := rng.lengthNoBr;
  end;

  QTextCursor_destroy(tc);
  result := true;
end;

class procedure TQtWSCustomRichMemo.InDelText(const AWinControl: TWinControl;
  const TextUTF8: String; DstStart, DstLen: Integer);
var
  te: TQtTextEdit;
  w: QTextEditH;
  tc: QTextCursorH;
  AText: UnicodeString;
  doc: QTextDocumentH;
  maxPosition: Integer;
begin
  if not WSCheckHandleAllocated(AWinControl, 'InDelText') then
    Exit;
  te:=TQtTextEdit(AWinControl.Handle);
  w:=QTextEditH(te.Widget);
  tc := QTextCursor_create();
  QTextEdit_textCursor(w, tc);

  doc := QTextCursor_document(tc);
  maxPosition := QTextDocument_characterCount(doc) - 1;

  QTextCursor_clearSelection(tc);
  QTextCursor_setPosition(tc, Min(DstStart, maxPosition));
  QTextCursor_setPosition(tc, Min(DstStart + DstLen, maxPosition), QTextCursorKeepAnchor);
  QTextCursor_removeSelectedText(tc);

  AText := UTF8Decode(TextUTF8);
  QTextCursor_insertText(tc, @AText);
  QTextCursor_Destroy(tc);
end;

class procedure TQtWSCustomRichMemo.SetTextUIParams(
  const AWinControl: TWinControl; TextStart, TextLen: Integer;
  const ui: TTextUIParam);
var
  te: TQtTextEdit;
  w: QTextEditH;
  tc: QTextCursorH;
  address: UnicodeString;
  fmt: QTextCharFormatH;
  Color: TQColor;
  Brush: QBrushH;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetTextUIParams') then
    Exit;
  te:=TQtTextEdit(AWinControl.Handle);
  w:=QTextEditH(te.Widget);
  tc := QTextCursor_create();
  QTextEdit_textCursor(w, tc);

  fmt := QTextCharFormat_Create();
  QTextCursor_charFormat(tc, fmt);

  ColorRefToTQColor(ColorToRGB(clGreen), Color);
  Brush := QBrush_create(@Color, QtSolidPattern);
  QTextFormat_setForeground(QTextFormatH(fmt), brush);
  QBrush_Destroy(Brush);
  QTextCharFormat_setAnchor(fmt, true);
  address := UTF8Decode(ui.linkref);
  QTextCharFormat_setAnchorHref(fmt, @address);

  QTextCursor_setPosition(tc, TextStart);
  QTextCursor_setPosition(tc, TextStart + TextLen, QTextCursorKeepAnchor);
  QTextCursor_setCharFormat(tc, fmt);

  QTextCharFormat_Destroy(fmt);
  QTextCursor_Destroy(tc);
end;

class function TQtWSCustomRichMemo.GetTextUIParams(
  const AWinControl: TWinControl; TextStart: Integer; var ui: TTextUIParam
  ): Boolean;
var
  te: TQtTextEdit;
  w: QTextEditH;
begin
  result := false;
  if not WSCheckHandleAllocated(AWinControl, 'SetTextUIParams') then
    Exit;

  te:=TQtTextEdit(AWinControl.Handle);
  w:=QTextEditH(te.Widget);

  result := PrivateGetTextUIParams(w, TextStart, ui);
end;

class function TQtWSCustomRichMemo.Search(const AWinControl: TWinControl;
  const ANiddle: string; const SearchOpts: TIntSearchOpt): Integer;
var
  w : QTextEditH;
  te : TQtTextEdit;
  ws : Widestring;
  fl : QTextDocumentFindFlags;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetParaAlignment') then
    Exit;
  te:=TQtTextEdit(AWinControl.Handle);
  w:=QTextEditH(te.Widget);

  fl:=0;
  if soMatchCase in SearchOpts.Options then fl:=fl or QTextDocumentFindCaseSensitively;
  if soWholeWord in SearchOpts.Options then fl:=fl or QTextDocumentFindWholeWords;
  if soBackward in SearchOpts.Options then fl:=fl or QTextDocumentFindBackward;

  //todo: range filtering in Serach Opts
  ws:=UTF8Decode(ANiddle);
  if QTextEdit_find(w, @ws, fl) then  Result:=te.getSelectionStart
  else Result:=-1;
end;

class function TQtWSCustomRichMemo.isInternalChange(
  const AWinControl: TWinControl; Params: TTextModifyMask): Boolean;
begin
  Result := false;

  //Result:=inherited isInternalChange(AWinControl, Params);
end;

class procedure TQtWSCustomRichMemo.SetTextAttributesInternal(
  const AWinControl: TWinControl; TextStart, TextLen: Integer;
  const AModifyMask: TTextModifyMask; const Params: TIntFontParams);
begin
  SetTextAttributes(AWinControl, TextStart, TextLen, Params);
end;

class function TQtWSCustomRichMemo.GetStyleRange(
  const AWinControl: TWinControl; TextStart: Integer; var RangeStart,
  RangeLen: Integer): Boolean;
var
  qcur : QTextCursorH;
  te : TQtTextEdit;
  {$ifdef RMQT5_TEXTFORMATS}
  bck : TEditorState;
  al : QtAlignment;
  qblck : QTextBlockH;
  qbfmt : QTextBlockFormatH;
  i   : integer;
  cnt : integer;
  rng : array of TTextRange;
  blckofs: integer;
  {$endif}
begin
  if not WSCheckHandleAllocated(AWinControl, 'GetStyleRange') then begin
    Result:=false;
    Exit;
  end;

  te:=TQtTextEdit(AWinControl.Handle);
  qcur := QTextCursor_Create();

  {$ifndef RMQT5_TEXTFORMATS}
  QTextEdit_textCursor(QTextEditH(te.Widget), qcur);
  result := PrivateGetFormatRange(qCur, TextStart, RangeStart, RangeLen);
  if result then
    RangeLen := RangeLen - RangeStart + 1;
  QTextCursor_Destroy(qcur);
  {$else}
  MakeBackup(te, bck);
  qblck := QTextBlock_Create();
  try
    te.setSelection(TextStart, 0);
    QTextEdit_textCursor(QTextEditH(te.Widget), qcur);
    QTextCursor_block(qcur, qblck);

    cnt := QTextBlock_textFormatsCount(qblck);
    SetLength(rng, cnt);
    if cnt>0 then begin
      blckofs := QTextBlock_position(qblck);
      textStart := textStart - blckofs;
      for i:=0 to cnt-1 do begin
        if (textStart >= rng[i].start) and (textStart<rng[i].start+rng[i].length) then
        begin
          RangeStart := rng[i].start + blckofs;
          RangeLen := rng[i].length;
          break;
        end;
      end;
    end;
  finally
    QTextBlock_Destroy(qblck);
    QTextCursor_Destroy(qcur);
    ApplyBackup(te, bck);
  end;
  {$endif}
end;

class function TQtWSCustomRichMemo.GetParaAlignment(
  const AWinControl: TWinControl; TextStart: Integer;
  var AAlign: TIntParaAlignment): Boolean;
var
  te : TQtTextEdit;
  al : QtAlignment;
begin
  if not WSCheckHandleAllocated(AWinControl, 'GetParaAlignment') then begin
    Result:=false;
    Exit;
  end;
  te:=TQtTextEdit(AWinControl.Handle);
  al:=QTextEdit_alignment(QTextEditH(te.Widget));
  if QtAlignLeading and al > 0 then AAlign:=paLeft
  else if QtAlignTrailing and al > 0 then AAlign:=paRight
  else if QtAlignCenter and al > 0 then AAlign:=paCenter
  else if QtAlignJustify and al > 0 then AAlign:=paJustify
  else AAlign:=paLeft;
  Result:=true;
end;

procedure MakeBackup(te: TQtTextEdit; out backup: TEditorState);
begin
  backup.selst:=te.getSelectionStart;
  backup.sellen:=te.getSelectionLength;
end;

procedure ApplyBackup(te: TQtTextEdit; const backup: TEditorState);
begin
  te.setSelection(backup.selst, backup.sellen);
end;

end.
