{
 gtk2richmemo.pas

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

unit Gtk2RichMemo;

{$mode objfpc}{$H+}

{.$define DEBUG}

interface

uses
  // Bindings
  gtk2, glib2, gdk2, pango,
  // RTL/FCL
  Types, Classes, SysUtils, StrUtils,
  // LCL
  LCLType, Controls, Graphics, LazUTF8, StdCtrls, LCLProc, {$IFDEF Debug} LazLogger,{$ENDIF}
  // Gtk2 widget
  Gtk2Int, Gtk2Def,
  GTK2WinApiWindow, Gtk2Globals, Gtk2Proc,
  gdk2pixbuf, Gtk2WSStdCtrls,
  // RichMemo
  RichMemo, WSRichMemo, RichMemoUtils;

const
  TagNameNumeric = 'numeric';
  TagNameSubOrSuper = 'suborsuper';
  TagNameLink       = 'linkref';
  BulletChar     = #$E2#$80#$A2;
  TabChar        = #$09;

  { TGtk2WSCustomRichMemo }
type
  TGtk2WSCustomRichMemo = class(TWSCustomRichMemo)
  protected
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo);
    class procedure GetWidgetBuffer(const AWinControl: TWinControl; var TextWidget: PGtkWidget; var Buffer: PGtkTextBuffer);

    class function GetAttrAtIter(view: PGtkTextView; const start: TGtkTextIter): PGtkTextAttributes;
    class function GetAttrAtPos(const AWinControl: TWinControl; TextStart: Integer; APara: Boolean = false): PGtkTextAttributes;
    class procedure GetAttributesAt(const AWinControl: TWinControl; TextStart: Integer; APara: Boolean;
      var attr: PGtkTextAttributes; var fp: TFontParams);

    class procedure ApplyTag(abuffer: PGtkTextBuffer; tag: PGtkTextTag; TextStart, TextLen: Integer; ToParagraphs: Boolean = False; isNewTag: boolean = false); overload;
    class procedure ApplyTag(abuffer: PGtkTextBuffer; tag: PGtkTextTag; istart, iend: TGtkTextIter; ToParagraphs: Boolean = False; isNewTag: boolean = false); overload;
    class procedure FormatSubSuperScript(buffer: PGtkTextBuffer; vs: TVScriptPos; fontSizePts: Double; TextStart, TextLen: Integer);
    class procedure ClearParagraph(rich: TCustomRichMemo; TextPos: Integer); override;

  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;

    class function GetSelLength(const ACustomEdit: TCustomEdit): integer; override;
    class function GetStrings(const ACustomMemo: TCustomMemo): TStrings; override;

    class function GetStyleRange(const AWinControl: TWinControl; TextStart: Integer;
      var RangeStart, RangeLen: Integer): Boolean; override;
    class function GetTextAttributes(const AWinControl: TWinControl; TextStart: Integer;
      var Params: TIntFontParams): Boolean; override;
    class procedure SetTextAttributes(const AWinControl: TWinControl; TextStart, TextLen: Integer;
      const Params: TIntFontParams); override;
    class function GetParaAlignment(const AWinControl: TWinControl; TextStart: Integer;
      var AAlign: TIntParaAlignment): Boolean; override;
    class procedure SetParaAlignment(const AWinControl: TWinControl; TextStart, TextLen: Integer;
      const AAlign: TIntParaAlignment); override;

    class function GetParaMetric(const AWinControl: TWinControl; TextStart: Integer;
      var AMetric: TIntParaMetric): Boolean; override;
    class procedure SetParaMetric(const AWinControl: TWinControl; TextStart, TextLen: Integer;
      const AMetric: TIntParaMetric); override;

    class function GetParaNumbering(const AWinControl: TWinControl; TextStart: Integer;
      var ANumber: TIntParaNumbering): Boolean; override;
    class procedure SetParaNumbering(const AWinControl: TWinControl; TextStart,
      TextLen: Integer; const ANumber: TIntParaNumbering); override;

    class procedure SetParaTabs(const AWinControl: TWinControl; TextStart, TextLen: Integer;
      const AStopList: TTabStopList); override;
    class function GetParaTabs(const AWinControl: TWinControl; TextStart: integer;
      var AStopList: TTabStopList): Boolean; override;

    class function GetParaRange(const AWinControl: TWinControl; TextStart: Integer; var rng: TParaRange): Boolean; override;

    class procedure SetTextUIParams(const AWinControl: TWinControl; TextStart, TextLen: Integer;
      const ui: TTextUIParam); override;
    class function GetTextUIParams(const AWinControl: TWinControl; TextStart: Integer;
      var ui: TTextUIParam): Boolean; override;

    class procedure InDelText(const AWinControl: TWinControl; const TextUTF8: String; DstStart, DstLen: Integer); override;
    class function CharAtPos(const AWinControl: TWinControl; x,y: Integer): Integer; override;

    class function Search(const AWinControl: TWinControl; const ANiddle: string; const SearchOpts: TIntSearchOpt): Integer; override;

    class function ImageFromFile(const ARichMemo: TCustomRichMemo; APos: Integer;
         const FileNameUTF8: string;
         const AImgSize: TSize
      ): Boolean;
    class procedure SetSelStart(const ACustomEdit: TCustomEdit; NewStart: integer); override;
    class procedure SetSelLength(const ACustomEdit: TCustomEdit; NewLength: integer); override;

    class procedure SetZoomFactor(const AWinControl: TWinControl; AZoomFactor: Double); override;

    // inline handler
    class function InlineInsert(const AWinControl: TWinControl; ATextStart, ATextLength: Integer;
      const ASize: TSize; AHandler: TRichMemoInline; var wsObj: TRichMemoInlineWSObject): Boolean; override;
    class procedure InlineInvalidate(const AWinControl: TWinControl;
       AHandler: TRichMemoInline; wsObj: TRichMemoInlineWSObject); override;
  end;

  { TGtk2InlineObject }

  TGtk2InlineObject = class(TRichMemoInlineWSObject)
  public
    anch   : PGtkTextChildAnchor;
    wgt    : PGtkWidget;
    il     : TRichMemoInline;
    size   : TSize;
    cnv    : TCanvas;
    constructor Create;
    destructor Destroy; override;
  end;

type

  { TGtk2RichMemoStrings }

  TGtk2RichMemoStrings = class(TGtk2MemoStrings)
  protected
    FGtkText : PGtkTextView;
    FGtkBuf: PGtkTextBuffer;
    FOwner: TWinControl;
    function GetTextStr: string; override;
    function GetCount: integer; override;
    function Get(Index : Integer) : string; override;
    procedure SetUpdateState(Updating: Boolean); override;
  public
    constructor Create(TextView : PGtkTextView; TheOwner: TWinControl);
    destructor Destroy; override;
    procedure Assign(Source : TPersistent); override;
    procedure AddStrings(TheStrings: TStrings); override;
    procedure Clear; override;
    procedure Delete(Index : integer); override;
    procedure Insert(Index : integer; const S: string); override;
    procedure SetTextStr(const Value: string); override;
    property Owner: TWinControl read FOwner;
  end;


const
  SubSuperFontKoef = 0.583; // the koef for the font size of sub/super scripts matching Adover Illustrator and OpenOffice
  SuperRiseKoef =  0.58;
  SubRiseKoef   = -0.08;

implementation

type
  TRichMemoData = record
    link    : Boolean;
    link_li : integer;
    link_le : integer;
    link_act: TGdkEventType;
    link_btn: guint;
  end;
  PRichMemoData = ^TRichMemoData;

var
  linkCursor: PGdkCursor = nil;// gdk_cursor_new(GDK_DRAFT_LARGE)

var
  //tagAttrCounter: Integer = 0;
  tagScaleCounter: Integer = 0;
  tagSubSuperCounter: Integer = 0;
  tagJustCounter: Integer = 0;
  tagTabCounter: Integer = 0;
  tagLinkCounter: Integer = 0;
  tagZoomCounter: Integer = 0;
  //tagListCounter: Integer = 0;

function GetTagName(baseName: string; var counter: Integer): string;
begin
  inc(counter);
  result := baseName+':'+IntToStr(counter);
end;

{$IFDEF DEBUG}
procedure DumpTag(tag: PGtkTextTag; data:gpointer); cdecl;
var
  indent, left_margin: gint;
  left_margin_set: gboolean;
  name: pgchar;
begin
  g_object_get(G_OBJECT(tag),
    pchar('indent'), [ @indent,
    'left-margin', @left_margin,
    'left-margin-set', @left_margin_set,
    'name', @name,
    nil
    ]);
  dbgOut('  pri=%d indent=%d left_margin=',[tag^.priority, indent]);
  if left_margin_set then dbgOut('SET %d px', [left_margin])
  else                    dbgOut('UNSET ', [left_margin]);
  dbgOut(' name=%s', [name]);
  {%H-}DebugLn;
end;

procedure DumpTagsAt(msg: string; buffer: PGtkTextBuffer; offset:Integer; len:Integer=5);
var
  iter, iend: TGtkTextIter;
  tags, tagItem: PGSList;
  p: pchar;
  txt: string;
begin

  if len<1 then
    exit;

  gtk_text_buffer_get_iter_at_offset (buffer, @iter, offset);
  gtk_text_buffer_get_iter_at_offset (buffer, @iend, offset+1);

  p := gtk_text_buffer_get_text(buffer, @iter, @iend, true);
  if p=nil then txt := 'nil'
  else begin
    if p^ in [#0..#32,#128..#255] then  txt := '#' + IntToStr(ord(p^))
    else                                txt := p^;
  end;
  DebugLn('%s: tags at offset=%d [%s]', [msg, offset, txt]);
  tags := gtk_text_iter_get_tags(@iter);

  tagItem := tags;
  while (tagItem<>nil) do begin
    if tagItem^.data<>nil then
      DumpTag(PGtkTextTag(tagItem^.data), nil);
    tagItem := tagItem^.next;
  end;

  if tags<>nil then
    g_slist_free(tags);

  DumpTagsAt(msg, buffer, offset + 1, len - 1);
end;

function CompareTags(Item1, Item2: Pointer): Integer;
begin
  result := PGtkTextTag(Item1)^.priority - PGtkTextTag(Item2)^.priority;
end;

procedure CollectTags(tag: PGtkTextTag; data:gpointer); cdecl;
begin
  TList(Data).Add(tag);
end;

procedure DumpTagTable(buffer: PGtkTextBuffer; msg: string);
var
  table: PGtkTextTagTable;
  List: TList;
  tag: Pointer;
begin
  table := gtk_text_buffer_get_tag_table(buffer);
  DebugLn('%s Count=%d:', [msg, gtk_text_tag_table_get_size(table)]);

  List := TList.Create;
  gtk_text_tag_table_foreach(table, @CollectTags, List);
  List.Sort(@CompareTags);
  for tag in List do
    DumpTag(PGtkTextTag(tag), nil);
  List.Free;
end;
{$ENDIF}

procedure gtk_text_buffer_copy_tags_from_offset(buffer: PGtkTextBuffer;
  srcIter: PGtkTextIter; dstOffset, dstLen: Integer);
var
  tags, tagItem: PGSList;
  iStart, iEnd: TGtkTextIter;
begin
  tags := gtk_text_iter_get_tags(srcIter);
  gtk_text_buffer_get_iter_at_offset (buffer, @iStart, dstOffset);
  gtk_text_buffer_get_iter_at_offset (buffer, @iEnd, dstOffset+dstLen);

  tagItem := tags;
  while (tagItem<>nil) do begin
    if tagItem^.data<>nil then
       gtk_text_buffer_apply_tag(buffer, PGtkTextTag(tagItem^.data), @iStart, @iEnd);
    tagItem := tagItem^.next;
  end;

  if tags<>nil then
    g_slist_free(tags);
end;

function gtk_text_buffer_get_linkref_tag_at_offset(buffer: PGtkTextBuffer;
  offset: Integer): PGtkTextTag;
var
  iter: TGtkTextIter;
  tags, tagItem: PGSList;
  data: pointer;
begin
  result := nil;

  gtk_text_buffer_get_iter_at_offset (buffer, @iter, offset);
  tags := gtk_text_iter_get_tags(@iter);

  tagItem := tags;
  while (tagItem<>nil) do begin
    data := g_object_get_data(G_OBJECT(tagItem^.data), TagNameLink);
    if data<>nil then begin
      result := PGtkTextTag(tagItem^.data);
      break;
    end;
    tagItem := tagItem^.next;
  end;

  if tags<>nil then
    g_slist_free(tags);
end;

function NextField(const text:string; sep:char; var index:Integer): string; overload;
begin
  result := '';
  while index<=length(text) do begin
    inc(index);
    if text[index-1]=sep then
      break;
    result := result + text[index-1];
  end;
end;

function NextField(const text:string; sep:char; var index:Integer; def:Integer): Integer; overload;
begin
  result := StrToIntDef(NextField(text, sep, index), def);
end;

function gtk_text_buffer_offset_is_list_item(buffer: PGtkTextBuffer; offset: Integer): boolean;
var
  tag: PGtkTextTag;
  iter: TGtkTextIter;
begin
  result := false;
  tag := gtk_text_tag_table_lookup( gtk_text_buffer_get_tag_table(buffer), TagNameNumeric);
  if tag=nil then exit;

  gtk_text_buffer_get_iter_at_offset(buffer, @iter, offset);
  gtk_text_iter_set_line_offset(@iter, 0);

  result := gtk_text_iter_begins_tag(@iter, tag)
end;

function gtk_text_iter_get_numbering(iStart, iEnd: PGtkTextIter; out pn: TParaNumbering): boolean;
var
  tags, tagItem: PGSList;
  tagText: pgchar;
  tag: PGtkTextTag;
  tagName: string;
  i, sepPos: Integer;
begin

  result := false;
  tags := gtk_text_iter_get_tags(iStart);

  tagItem := tags;
  while (tagItem<>nil) do begin
    tag := PGtkTextTag(tagItem^.data);
    if pos('list:', tag^.name)=1 then begin
      result := true;
      tagName := copy(tag^.name, 6, 255);
      i := 1;
      pn.Style := TParaNumStyle(NextField(tagName, '_', i, 0));
      sepPos := NextField(tagName, '_', i, 0);
      tagText := gtk_text_iter_get_text(iStart, iEnd);
      dec(sepPos);
      if SepPos>0 then
        pn.SepChar := tagText[sepPos]; // tagText is pchar
      case pn.Style of
        pnNumber:
          pn.NumberStart := StrToIntDef(copy(tagText, 1, sepPos), 1);
        pnLowLetter:
          pn.NumberStart := ord(tagText[0]) - ord('a') + 1;
        pnUpLetter:
          pn.NumberStart := ord(tagText[0]) - ord('A') + 1;
        pnLowRoman, pnUpRoman:
          pn.NumberStart := RomanToIntDef(copy(tagText, 1, sepPos), 1);
      end;
      break;
    end;
    tagItem := tagItem^.next;
  end;

  if tags<>nil then
    g_slist_free(tags);
end;

procedure gtk_text_buffer_split_tags_at_offset(widget: PGtkWidget; buffer: PGtkTextBuffer;
  offset, splitLen: Integer);
var
  iStart, iEnd: TGtkTextIter;
  item: PGSList;
  tag: PGtkTextTag;
  attr: PGtkTextAttributes;
  hasbg: Boolean;
begin

  attr := gtk_text_view_get_default_attributes(PGtkTextView(widget));
  if not Assigned(attr) then Exit;
  gtk_text_buffer_get_iter_at_offset (buffer, @iStart, offset);
  gtk_text_iter_get_attributes(@iStart, attr);
  hasbg := draw_bg(attr^.appearance)=1;
  gtk_text_attributes_unref(attr);

  if not hasbg then
    exit;

  gtk_text_buffer_get_iter_at_offset (buffer, @iEnd, offset+splitLen);

  item := gtk_text_iter_get_tags(@iStart);
  while (item<>nil) do begin
    tag := PGtkTextTag(item^.data);
    gtk_text_buffer_remove_tag(buffer, tag, @iStart, @iEnd);
    item := item^.next;
  end;
end;

procedure gtk_texttag_free_linkref(linkref: gpointer); cdecl;
begin
  StrDispose(pchar(linkref));
end;

function gtk_texttag_get_linkref(tag: PGtkTextTag): string;
var
  data: pointer;
begin
  data := g_object_get_data(G_OBJECT(tag), TagNameLink);
  if data<>nil then
    result := pchar(data)
  else
    result := '';
end;

procedure gtk_texttag_set_linkref(tag:PGtkTextTag; linkRef: string);
begin
  g_object_set_data_full(G_OBJECT(tag), TagNameLink, StrNew(pchar(linkRef)),
                         @gtk_texttag_free_linkref);
end;

  // todo: why "shr" on each of this flag test?
function gtktextattr_underline(const a : TGtkTextAppearance) : Boolean;
begin
  Result:=((a.flag0 and bm_TGtkTextAppearance_underline) shr bp_TGtkTextAppearance_underline) > 0;
end;

function gtktextattr_strikethrough(const a : TGtkTextAppearance) : Boolean;
begin
  Result:=((a.flag0 and bm_TGtkTextAppearance_strikethrough) shr bp_TGtkTextAppearance_strikethrough) > 0;
end;

function gtktextattr_bkcolor(const a : TGtkTextAppearance) : Boolean;
begin
  Result:=((a.flag0 and bm_TGtkTextAppearance_draw_bg ) shr bp_TGtkTextAppearance_draw_bg) > 0;
end;


function GtkTextAttrToFontParams(const textAttr: TGtkTextAttributes; var FontParams: TIntFontParams; const FontKoef : Double = 1.0): Boolean;
var
  w   : integer;
  st  : TPangoStyle;
  pf  : PPangoFontDescription;
  sz  : double;
begin
  InitFontParams(FontParams);

  pf := textAttr.font;
  Result := Assigned(pf);
  if not Result then Exit;

  if Assigned(pf) then begin
    FontParams.Name := pango_font_description_get_family(pf);
    FontParams.Size := pango_font_description_get_size(pf);
    sz:=FontParams.Size / PANGO_SCALE;
    if pango_font_description_get_size_is_absolute(pf) then
      sz:=sz/(ScreenDPI/PageDPI);
    FontParams.Size:=round(sz*FontKoef);
    w := pango_font_description_get_weight(pf);
    if w > PANGO_WEIGHT_NORMAL then Include(FontParams.Style, fsBold);

    st := pango_font_description_get_style(pf);
    if st and PANGO_STYLE_ITALIC > 0 then  Include(FontParams.Style, fsItalic);
  end;

  FontParams.Color := TGDKColorToTColor(textAttr.appearance.fg_color);
  if gtktextattr_underline(textAttr.appearance) then  Include(FontParams.Style, fsUnderline);
  if gtktextattr_strikethrough(textAttr.appearance) then Include(FontParams.Style, fsStrikeOut);
  FontParams.HasBkClr := gtktextattr_bkcolor(textAttr.appearance);
  if FontParams.HasBkClr then
    FontParams.BkColor := TGDKColorToTColor(textAttr.appearance.bg_color);

end;

type
  TGtk2WSCustomMemoInt = class(TGtk2WSCustomMemo);
  TCustomRichMemoInt   = class(TCustomRichMemo);

procedure Gtk2WS_MemoSelChanged_Before(Textbuffer: PGtkTextBuffer;
   StartIter: PGtkTextIter; mark: PGtkTextMark; WidgetInfo: PWidgetInfo); cdecl;
var
  tag : PGtkTextTag;
begin
  tag := gtk_text_tag_table_lookup( gtk_text_buffer_get_tag_table(TextBuffer)
    , TagNameNumeric);

  // navigate "through" numbering characters
  if gtk_text_iter_has_tag( StartIter, tag) then begin
    // if tried to move at the "endo
    if gtk_text_iter_begins_tag(StartIter, tag) then begin
      gtk_text_iter_forward_to_tag_toggle(StartIter, nil);
      gtk_text_buffer_move_mark(TextBuffer, mark, StartIter);
    end else begin
      gtk_text_iter_forward_char(StartIter);
      if gtk_text_iter_ends_tag(StartIter, tag) then begin
        gtk_text_iter_backward_to_tag_toggle(StartIter, nil);
        gtk_text_iter_backward_char(StartIter);
        gtk_text_buffer_move_mark(TextBuffer, mark, StartIter);
      end;
    end;
  end;
end;

procedure Gtk2WS_MemoSelChanged (Textbuffer: PGtkTextBuffer;
   StartIter: PGtkTextIter; mark: PGtkTextMark; WidgetInfo: PWidgetInfo); cdecl;
begin
  if TControl(WidgetInfo^.LCLObject) is TCustomRichMemo then
  begin
    TCustomRichMemoInt(WidgetInfo^.LCLObject).DoSelectionChange;
  end;
end;

procedure Gtk2WS_RichMemoInsert(Textbuffer: PGtkTextBuffer;
   StartIter: PGtkTextIter; text: PChar; len: gint; WidgetInfo: PWidgetInfo); cdecl;
var
  rm : TCustomRichMemo;
  iter : PGtkTextIter;
  tag  : PGtkTextTag;
  w    : PGtkWidget;
  b    : PGtkTextBuffer;
  attr : PGtkTextAttributes;
  tagName: String;
begin
  if TControl(WidgetInfo^.LCLObject) is TCustomRichMemo then
  begin
    rm := TCustomRichMemo(WidgetInfo^.LCLObject);
    // re-zooming any newly entered (pasted, manually inserted text)
    if (rm.ZoomFactor<>1) then begin
      TGtk2WSCustomRichMemo.GetWidgetBuffer(rm, w, b);
      iter:=gtk_text_iter_copy(StartIter);
      gtk_text_iter_backward_chars(iter, len);
      attr := gtk_text_view_get_default_attributes(PGtkTextView(w));
      gtk_text_iter_get_attributes(iter, attr);

      if attr^.font_scale<>rm.ZoomFactor then begin
        tagName := GetTagName('scale', tagScaleCounter);
        tag := gtk_text_buffer_create_tag(b, pchar(tagName),
            'scale', [   gdouble(rm.ZoomFactor),
            'scale-set', gboolean(gTRUE),
            nil]);
        gtk_text_buffer_apply_tag(b, tag, iter, StartIter);
        {$IFDEF DEBUG}
        {%H-}DebugLn('Applied ScaleTag');
        {$ENDIF}
      end;
      gtk_text_attributes_unref(attr);
    end;
  end;
end;

procedure Gtk2WS_Backspace(view: PGtkTextView; WidgetInfo: PWidgetInfo); cdecl;
var
  buf    : PGtkTextBuffer;
  mark   : PGtkTextMark;
  iend   : TGtkTextIter;
  istart : TGtkTextIter;
  tag    : PGtkTextTag;
begin
  // this handler checks, if the "numbering" should be erarsed
  buf:=gtk_text_view_get_buffer(view);
  if not Assigned(buf) then Exit;
  mark := gtk_text_buffer_get_mark(buf, 'insert');
  if not Assigned(mark) then Exit;
  tag := gtk_text_tag_table_lookup( gtk_text_buffer_get_tag_table(buf)
    , TagNameNumeric);
  if not Assigned(tag) then Exit;

  // first, check if cursor is right "after" the "numbering characters"
  gtk_text_buffer_get_iter_at_mark(buf, @iend, mark);

  if gtk_text_iter_ends_tag(@iend, tag) then begin
    // cursor position is at the beginning of the line - erase all
    // characters that belong to the numbering.
    istart:=iend;
    gtk_text_iter_backward_to_tag_toggle(@istart, tag);
    gtk_text_buffer_delete(buf, @istart, @iend);
    // prevent default backspace
    g_signal_stop_emission_by_name(view, 'backspace');
  end;
end;

function GatherNumeirc(buf: PGtkTextBuffer; const istart: TGtkTextIter; var num: TIntParaNumbering): Boolean;
var
  iend : TGtkTextIter;
  ch   : Pgchar;
  s    : string;
  i    : Integer;
  {%H-}err  : Integer;
begin
  iend:=istart;
  gtk_text_iter_forward_to_tag_toggle(@iend, nil);
  ch:=gtk_text_iter_get_text(@istart, @iend);
  Result:=Assigned(ch);
  if not Result then Exit;
  s:=ch;
  g_free(ch);
  Result:=length(s)>1;
  if not Result then Exit;

  if Pos(BulletChar, s[1]) = 1 then num.Style:=pnBullet
  else if s[1] in ['0'..'9'] then begin
    num.Style:=pnNumber;
    i:=1;
    while (i<=length(s)) and (s[i] in ['0'..'9']) do inc(i);
    Val( Copy(s, 1, i-1), num.NumberStart, err);
  end else begin
    num.Style:=pnCustomChar;

  end;

  //ch.
  //gtk_text_buffer_insert_range
end;

function Gtk2_RichMemoKeyPress(view: PGtkTextView; Event: PGdkEventKey;
  WidgetInfo: PWidgetInfo): gboolean; cdecl;
var
  buf    : PGtkTextBuffer;
  mark   : PGtkTextMark;
  istart : TGtkTextIter;
  tag    : PGtkTextTag;
begin
  if Event^.keyval= GDK_KEY_Return then begin
    //writeln('return !');
    buf:=gtk_text_view_get_buffer(view);
    if not Assigned(buf) then Exit;
    mark := gtk_text_buffer_get_mark(buf, 'insert');
    if not Assigned(mark) then Exit;
    tag := gtk_text_tag_table_lookup( gtk_text_buffer_get_tag_table(buf)
      , TagNameNumeric);
    if not Assigned(tag) then Exit;
    gtk_text_buffer_get_iter_at_mark(buf, @istart, mark);

    gtk_text_iter_set_line_offset(@istart, 0);
    if gtk_text_iter_begins_tag(@istart, tag) then begin
      //writeln('apply!');
      //writeln( 'ofs: ', gtk_text_iter_get_offset(@istart));
    end;
  end;
  Result:=false;
end;


function Gtk2_RichMemoMotion(view: PGtkTextView; Event: PGdkEventKey;
  WidgetInfo: PWidgetInfo): gboolean; cdecl;
var
  mt : PGdkEventMotion;
  i  : TGtkTextIter;
  tag : PGtkTextTag;
  buf : PGtkTextBuffer;
  w   : PGdkWindow;
  bx,by: gint;
  offset: gint;
begin
  buf:=gtk_text_view_get_buffer(view);
  if not Assigned(buf) then Exit;

  mt:=PGdkEventMotion(Event);
  gtk_text_view_window_to_buffer_coords(view, GTK_TEXT_WINDOW_TEXT,
    round(mt^.x), round(mt^.y), @bx, @by);

  gtk_text_view_get_iter_at_location(view, @i, bx,by);

  offset := gtk_text_iter_get_offset(@i);
  tag := gtk_text_buffer_get_linkref_tag_at_offset(buf, offset);

  gdk_cursor_new(GDK_DRAFT_LARGE);
  w:= gtk_text_view_get_window(view, GTK_TEXT_WINDOW_TEXT);
  if tag<>nil then begin
    if not Assigned(linkCursor) then
      linkCursor:=gdk_cursor_new(GDK_HAND1);
    gdk_window_set_cursor(w, linkCursor);
  end else
    gdk_window_set_cursor(w, nil);

  Result:=false;
end;

function Gtk2_RichMemoMouseButton(view: PGtkTextView; Event: PGdkEventKey;
  WidgetInfo: PWidgetInfo): gboolean; cdecl;
var
  mt : PGdkEventButton;
  i  : TGtkTextIter;
  tag : PGtkTextTag;
  buf : PGtkTextBuffer;
  bx,by,offset: gint;
  mi  : TLinkMouseInfo;
  li,le: integer;
  act : TLinkAction;
  data : PRichMemoData;
begin
  buf:=gtk_text_view_get_buffer(view);
  data:=PRichMemoData(WidgetInfo^.UserData);
  if not Assigned(buf) then Exit;

  mt:=PGdkEventButton(Event);
  gtk_text_view_window_to_buffer_coords(view, GTK_TEXT_WINDOW_TEXT,
    round(mt^.x), round(mt^.y), @bx, @by);

  gtk_text_view_get_iter_at_location(view, @i, bx,by);
  offset := gtk_text_iter_get_offset(@i);
  tag := gtk_text_buffer_get_linkref_tag_at_offset(buf, offset);

  if tag<>nil then begin
    if TControl(WidgetInfo^.LCLObject) is TCustomRichMemo then
    begin
      gtk_text_iter_backward_to_tag_toggle(@i, tag);
      li:=gtk_text_iter_get_offset(@i);
      gtk_text_iter_forward_to_tag_toggle(@i, tag);
      le:=gtk_text_iter_get_offset(@i)-li;

      case mt^._type of
        GDK_BUTTON_PRESS, GDK_2BUTTON_PRESS, GDK_3BUTTON_PRESS: begin
          data^.link:=true;
          data^.link_li:=li;
          data^.link_le:=le;
          data^.link_act:=mt^._type;
          data^.link_btn:=mt^.button;
        end;

        GDK_BUTTON_RELEASE: begin
          act:=laClick;
          if (data^.link) and (data^.link_btn=mt^.button) and (data^.link_li=li) and (le=data^.link_le) then
          begin
            data^.link:=false;
            case mt^.button of
              2: mi.button:=mbMiddle;
              3: mi.button:=mbRight;
            else
              mi.button:=mbLeft;
            end;
            mi.LinkRef := gtk_texttag_get_linkref(tag);
            TCustomRichMemoInt(WidgetInfo^.LCLObject).DoLinkAction(act, mi, li, le);
          end;
        end;
      end;
    end;
  end;

  Result:=false;
end;


{ TGtk2InlineObject }

constructor TGtk2InlineObject.Create;
begin
  inherited Create;

end;

destructor TGtk2InlineObject.Destroy;
begin
  cnv.Free;
  inherited Destroy;
end;

{ TGtk2RichMemoStrings }

function TGtk2RichMemoStrings.GetTextStr: string;
var
  StartIter, EndIter: TGtkTextIter;
  AText: PgChar;
begin
  Result := '';
  gtk_text_buffer_get_start_iter(FGtkBuf, @StartIter);
  gtk_text_buffer_get_end_iter(FGtkBuf, @EndIter);

  AText := gtk_text_iter_get_text(@StartIter, @EndIter);
  Result := StrPas(AText);
  if AText <> nil then
    g_free(AText);
end;

function TGtk2RichMemoStrings.GetCount: integer;
begin
  Result := gtk_text_buffer_get_line_count(FGtkBuf);
  if Get(Result-1) = '' then Dec(Result);
end;

function TGtk2RichMemoStrings.Get(Index: Integer): string;
var
  StartIter, EndIter: TGtkTextIter;
  AText: PgChar;
begin
  gtk_text_buffer_get_iter_at_line(FGtkBuf, @StartIter, Index);
  if Index = gtk_text_buffer_get_line_count(FGtkBuf) then
    gtk_text_buffer_get_end_iter(FGtkBuf, @EndIter)
  else begin
    gtk_text_buffer_get_iter_at_line(FGtkBuf, @EndIter, Index);
    gtk_text_iter_forward_to_line_end(@EndIter);
  end;
  // if a row is blank gtk_text_iter_forward_to_line_end will goto the row ahead
  // this is not desired. so if it jumped ahead a row then the row we want is blank
  if gtk_text_iter_get_line(@StartIter) = gtk_text_iter_get_line(@EndIter) then
  begin
    AText := gtk_text_iter_get_text(@StartIter, @EndIter);
    Result := StrPas(AText);
    g_free(AText);
  end
  else
    Result := '';
end;

procedure TGtk2RichMemoStrings.SetUpdateState(Updating: Boolean);
var
  id: guint;
  hid: gulong;
begin
  inherited SetUpdateState(Updating);

  id := g_signal_lookup('changed', GTK_TYPE_TEXT_BUFFER);
  hid := g_signal_handler_find(FGtkBuf, G_SIGNAL_MATCH_ID, id, 0, nil, nil, nil);
  if Updating then
    g_signal_handler_block(FGtkBuf, hid)
  else begin
    g_signal_handler_unblock(FGtkBuf, hid);
    {$IFDEF DEBUG}
    {%H-}DebugLn;
    DumpTagTable(FGtkBuf, 'Dump Tag Table');
    {$ENDIF}
  end;
end;

constructor TGtk2RichMemoStrings.Create(TextView: PGtkTextView;
  TheOwner: TWinControl);
begin
  inherited Create(TextView, TheOwner);
  if TextView = nil then RaiseGDBException(
    'TGtk2RichMemoStrings.Create Unspecified Text widget');
  FGtkText:= TextView;
  FGtkBuf := gtk_text_view_get_buffer(FGtkText);
  if TheOwner = nil then RaiseGDBException(
    'TGtk2RichMemoStrings.Create Unspecified owner');
  FOwner:=TheOwner;
end;

destructor TGtk2RichMemoStrings.Destroy;
begin

  inherited Destroy;
end;

procedure TGtk2RichMemoStrings.Assign(Source: TPersistent);
var
  S: TStrings;
begin
  if Source is TStrings then
  begin
    S:=TStrings(Source);
    // to prevent Clear and then SetText we need to use our own Assign
    QuoteChar := S.QuoteChar;
    Delimiter := S.Delimiter;
    NameValueSeparator := S.NameValueSeparator;
    TextLineBreakStyle := S.TextLineBreakStyle;
    Text := S.Text;
  end
  else
    inherited Assign(Source);
end;

procedure TGtk2RichMemoStrings.AddStrings(TheStrings: TStrings);
begin
  SetTextStr(GetTextStr + TStrings(TheStrings).Text);
end;

procedure TGtk2RichMemoStrings.Clear;
begin
  SetText('');
end;

procedure TGtk2RichMemoStrings.Delete(Index: integer);
var
StartIter,
EndIter: TGtkTextIter;
begin
  gtk_text_buffer_get_iter_at_line(FGtkBuf, @StartIter, Index);
  if Index = Count-1 then begin
    gtk_text_iter_backward_char(@StartIter);
    gtk_text_buffer_get_end_iter(FGtkBuf, @EndIter)
  end
  else
    gtk_text_buffer_get_iter_at_line(FGtkBuf, @EndIter, Index+1);
  gtk_text_buffer_delete(FGtkBuf, @StartIter, @EndIter);
end;

procedure TGtk2RichMemoStrings.Insert(Index: integer; const S: string);
var
  StartIter,
  CursorIter: TGtkTextIter;
  NewLine: String;
  TextMark: PGtkTextMark;
begin
  if Index < gtk_text_buffer_get_line_count(FGtkBuf) then begin
    //insert with LineEnding
    NewLine := S+LineEnding;
    gtk_text_buffer_get_iter_at_line(FGtkBuf, @StartIter, Index);
  end
  else begin
    //append with a preceding LineEnding
    gtk_text_buffer_get_end_iter(FGtkBuf, @StartIter);
    if gtk_text_buffer_get_line_count(FGtkBuf) = Count then
      NewLine := LineEnding+S+LineEnding
    else
      NewLine := S+LineEnding;
  end;

  {if FQueueCursorMove = 0 then
  begin}
    TextMark := gtk_text_buffer_get_insert(FGtkBuf);
    gtk_text_buffer_get_iter_at_mark(FGtkBuf, @CursorIter, TextMark);
{    if gtk_text_iter_equal(@StartIter, @CursorIter) then
      QueueCursorMove(-1);
  end;}

  // and finally insert the new text
  gtk_text_buffer_insert(FGtkBuf, @StartIter, PChar(NewLine) ,-1);
end;

procedure TGtk2RichMemoStrings.SetTextStr(const Value: string);
begin
  if (Value <> Text) then
  begin
    LockOnChange({%H-}PGtkObject(Owner.Handle), 1);
    gtk_text_buffer_set_text(FGtkBuf, PChar(Value), -1);
  end;
end;

class procedure TGtk2WSCustomRichMemo.SetCallbacks(
  const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo);
var
  TextBuf: PGtkTextBuffer;
  view   : PGtkTextView;
begin
  TGtk2WSCustomMemoInt.SetCallbacks(AGtkWidget, AWidgetInfo);

  view:=PGtkTextView(AWidgetInfo^.CoreWidget);
  TextBuf := gtk_text_view_get_buffer(view);
  SignalConnect(PGtkWidget(view), 'backspace', @Gtk2WS_Backspace, AWidgetInfo);
  SignalConnect(PGtkWidget(TextBuf), 'mark-set', @Gtk2WS_MemoSelChanged_Before, AWidgetInfo);
  SignalConnectAfter(PGtkWidget(TextBuf), 'mark-set', @Gtk2WS_MemoSelChanged, AWidgetInfo);
  SignalConnectAfter(PGtkWidget(TextBuf), 'insert-text', @Gtk2WS_RichMemoInsert, AWidgetInfo);
  SignalConnect(PGtkWidget(view), 'key-press-event',  @Gtk2_RichMemoKeyPress, AWidgetInfo);
  SignalConnect(PGtkWidget(view), 'motion-notify-event', @Gtk2_RichMemoMotion, AWidgetInfo);
  SignalConnect(PGtkWidget(view), 'button-press-event', @Gtk2_RichMemoMouseButton, AWidgetInfo);
  SignalConnect(PGtkWidget(view), 'button-release-event', @Gtk2_RichMemoMouseButton, AWidgetInfo);
end;

class procedure TGtk2WSCustomRichMemo.GetWidgetBuffer(const AWinControl: TWinControl;
    var TextWidget: PGtkWidget; var Buffer: PGtkTextBuffer);
var
  Widget     : PGtkWidget;
  list       : PGList;
begin
  TextWidget:=nil;
  Buffer:=nil;
  // todo: cache values?
  Widget := PGtkWidget(PtrUInt(AWinControl.Handle));

  list := gtk_container_get_children(PGtkContainer(Widget));
  if not Assigned(list) then Exit;

  TextWidget := PGtkWidget(list^.data);
  if not Assigned(TextWidget) then Exit;

  buffer := gtk_text_view_get_buffer (PGtkTextView(TextWidget));
end;

class function TGtk2WSCustomRichMemo.GetAttrAtIter(view: PGtkTextView; const start: TGtkTextIter): PGtkTextAttributes;
var
  attr       : PGtkTextAttributes;
begin
  Result:=nil;
  attr := gtk_text_view_get_default_attributes(view);
  if not Assigned(attr) then Exit;

  gtk_text_iter_get_attributes(@start, attr);
  Result:=attr;
end;

class function TGtk2WSCustomRichMemo.GetAttrAtPos(
  const AWinControl: TWinControl; TextStart: Integer; APara: Boolean ): PGtkTextAttributes;
var
  TextWidget : PGtkWidget;
  buffer     : PGtkTextBuffer;
  iter       : TGtkTextIter;
begin
  Result:=nil;
  GetWidgetBuffer(AWinControl, TextWidget, buffer);
  if not Assigned(buffer) then Exit;

  gtk_text_buffer_get_iter_at_offset(buffer, @iter, TextStart);
  if APara then gtk_text_iter_set_line_offset(@iter, 0);

  Result := GetAttrAtIter(PGtkTextView(TextWidget), iter);
end;

class procedure TGtk2WSCustomRichMemo.ApplyTag(abuffer: PGtkTextBuffer;
  tag: PGtkTextTag; TextStart, TextLen: Integer; ToParagraphs: Boolean;
  isNewTag: boolean);
var
  istart : TGtkTextIter;
  iend   : TGtkTextIter;
begin
  gtk_text_buffer_get_iter_at_offset (abuffer, @istart, TextStart);
  gtk_text_buffer_get_iter_at_offset (abuffer, @iend, TextStart+TextLen);
  ApplyTag(abuffer, tag, istart, iend, toParagraphs, isNewTag);
end;

class procedure TGtk2WSCustomRichMemo.ApplyTag(abuffer: PGtkTextBuffer;
  tag: PGtkTextTag; istart, iend: TGtkTextIter; ToParagraphs: Boolean;
  isNewTag: boolean);
var
  count  : gint;
  table  : PGtkTextTagTable;
begin
  if ToParagraphs then begin
    gtk_text_iter_set_line_offset(@istart, 0);
    gtk_text_iter_forward_to_line_end(@iend);
  end;
  gtk_text_buffer_apply_tag(abuffer, tag, @istart, @iend);
  if not isNewTag then begin
    table := gtk_text_buffer_get_tag_table(abuffer);
    count := gtk_text_tag_table_get_size(table);
    gtk_text_tag_set_priority(tag, count-1);
  end;
end;

class function TGtk2WSCustomRichMemo.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
var
  Widget,
  TempWidget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
  buffer: PGtkTextBuffer;
  SS:TPoint;
  data: PRichMemoData;
begin
  Widget := gtk_scrolled_window_new(nil, nil);
  Result := TLCLIntfHandle(PtrUInt(Widget));
  if Result = 0 then Exit;
  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(Widget,dbgsName(AWinControl));
  {$ENDIF}

  New(data);
  FillChar(data^, sizeof(TRichMemoData), 0);
  WidgetInfo := CreateWidgetInfo(Pointer(Result), AWinControl, AParams);
  WidgetInfo^.DataOwner := True;
  WidgetInfo^.UserData := data;

  TempWidget := gtk_text_view_new();
  gtk_container_add(PGtkContainer(Widget), TempWidget);

  GTK_WIDGET_UNSET_FLAGS(PGtkScrolledWindow(Widget)^.hscrollbar, GTK_CAN_FOCUS);
  GTK_WIDGET_UNSET_FLAGS(PGtkScrolledWindow(Widget)^.vscrollbar, GTK_CAN_FOCUS);

  SS:=Gtk2TranslateScrollStyle(TCustomMemo(AWinControl).ScrollBars);
  gtk_scrolled_window_set_policy(PGtkScrolledWindow(Widget),SS.X, SS.Y);

  // add border for memo
  gtk_scrolled_window_set_shadow_type(PGtkScrolledWindow(Widget),
    BorderStyleShadowMap[TCustomMemo(AWinControl).BorderStyle]);

  SetMainWidget(Widget, TempWidget);
  {$ifdef RMLCLTRUNK}
  GetWidgetInfo(Widget)^.CoreWidget := TempWidget;
  {$else}
  GetWidgetInfo(Widget, True)^.CoreWidget := TempWidget;
  {$endif}

  gtk_text_view_set_editable(PGtkTextView(TempWidget), not TCustomMemo(AWinControl).ReadOnly);
  gtk_text_view_set_justification(PGtkTextView(TempWidget), aGtkJustification[TCustomMemo(AWinControl).Alignment]);
  if TCustomMemo(AWinControl).WordWrap then
    gtk_text_view_set_wrap_mode(PGtkTextView(TempWidget), GTK_WRAP_WORD)
  else
    gtk_text_view_set_wrap_mode(PGtkTextView(TempWidget), GTK_WRAP_NONE);

  gtk_text_view_set_accepts_tab(PGtkTextView(TempWidget), TCustomMemo(AWinControl).WantTabs);

  gtk_widget_show_all(Widget);

  buffer := gtk_text_view_get_buffer (PGtkTextView(TempWidget));
  //tag:=gtk_text_tag_new(TagNameNumeric);
  gtk_text_buffer_create_tag (buffer, TagNameNumeric,
      'editable',   [ gboolean(gFALSE),
      'editable-set', gboolean(gTRUE),
      nil]);

  gtk_text_buffer_create_tag (buffer, TagNameSubOrSuper, nil);

  Set_RC_Name(AWinControl, Widget);
  SetCallbacks(Widget, WidgetInfo);
end;

class procedure TGtk2WSCustomRichMemo.DestroyHandle(
  const AWinControl: TWinControl);
var
  w : PGtkWidget;
  b : PGtkTextBuffer;
  handlerid: gulong;
begin
  GetWidgetBuffer(AWinControl, w, b);

  // uninstall hanlder, to prevent crashes
  handlerid := g_signal_handler_find(b
    , G_SIGNAL_MATCH_FUNC or G_SIGNAL_MATCH_DATA
    , 0, 0, nil
    , @Gtk2WS_MemoSelChanged, GetWidgetInfo(w));
  g_signal_handler_disconnect (b, handlerid);

  // the proper way of destroying a widgetset
  Gtk2WidgetSet.DestroyLCLComponent(AWinControl);
  // or actually - TGtk2WSWinControl.DestroyHandle(AWinControl)
  // the following call
  //   TWSWinControlClass(Classparent).DestroyHandle(AWinControl);
  // won't work, because TGtk2CustomMemo doesn't have DestoryHandle assigned
end;

class function TGtk2WSCustomRichMemo.GetSelLength(const ACustomEdit: TCustomEdit
  ): integer;
var
  w : PGtkWidget;
  b : PGtkTextBuffer;
  istart : TGtkTextIter;
  iend   : TGtkTextIter;
begin
  GetWidgetBuffer(ACustomEdit, w, b);
  if not Assigned(b) then begin
    Result:=-1;
    Exit;
  end;
  Result := 0;
  if not gtk_text_buffer_get_selection_bounds(b, @istart, @iend) then Exit;

  Result := Abs(gtk_text_iter_get_offset(@iend) - gtk_text_iter_get_offset(@istart));
end;

class function TGtk2WSCustomRichMemo.GetStrings(const ACustomMemo: TCustomMemo
  ): TStrings;
var
  TextView: PGtkTextView;
begin
  {$ifdef RMLCLTRUNK}
  TextView := PGtkTextView(GetWidgetInfo({%H-}Pointer(ACustomMemo.Handle))^.CoreWidget);
  {$else}
  TextView := PGtkTextView(GetWidgetInfo({%H-}Pointer(ACustomMemo.Handle), False)^.CoreWidget);
  {$endif}
  Result := TGtk2RichMemoStrings.Create(TextView, ACustomMemo);
end;

class function TGtk2WSCustomRichMemo.GetStyleRange(
  const AWinControl: TWinControl; TextStart: Integer; var RangeStart,
  RangeLen: Integer): Boolean;
var
  w : PGtkWidget;
  b : PGtkTextBuffer;
  istart : TGtkTextIter;
  iend   : TGtkTextIter;
  iter   : TGtkTextIter;
  tag    : PGtkTextTag;
begin
  GetWidgetBuffer(AWinControl, w, b);
  if not Assigned(b) then begin
    Result:=false;
    Exit;
  end;

  gtk_text_buffer_get_iter_at_offset (b, @istart, TextStart+1);
  if gtk_text_iter_get_offset(@istart)<>TextStart+1 then begin
    Result:=false; // TextStart is beyoned the end of text
    Exit;
  end;

  // check if offset is at a numeric tag, skip it if yes.
  tag := gtk_text_tag_table_lookup( gtk_text_buffer_get_tag_table(b), TagNameNumeric);
  gtk_text_buffer_get_iter_at_offset(b, @iter, TextStart);
  if gtk_text_iter_has_tag(@iter, tag) then begin
    gtk_text_iter_forward_to_tag_toggle(@iter, tag);
    RangeStart:=gtk_text_iter_get_offset(@iter);

    gtk_text_buffer_get_iter_at_offset (b, @iend, RangeStart);
    gtk_text_iter_forward_to_tag_toggle(@iend, nil);

    RangeLen:=gtk_text_iter_get_offset(@iend)-RangeStart;
  end else begin
    gtk_text_iter_backward_to_tag_toggle(@istart, nil);
    RangeStart:=gtk_text_iter_get_offset(@istart);

    gtk_text_buffer_get_iter_at_offset (b, @iend, RangeStart);
    gtk_text_iter_forward_to_tag_toggle(@iend, nil);

    RangeLen:=gtk_text_iter_get_offset(@iend)-RangeStart;
  end;


  Result:=true;
end;

class procedure TGtk2WSCustomRichMemo.FormatSubSuperScript(buffer: PGtkTextBuffer; vs: TVScriptPos; fontSizePts: Double; TextStart, TextLen: Integer);
var
  sz     : gint;
  istart : TGtkTextIter;
  iend   : TGtkTextIter;
  tag     : PGtkTextTag;
  scrtag   : PGtkTextTag;
  hasscript : Boolean;
  k         : Double;
  tagName: String;
  tagTable: PGtkTextTagTable;
begin
  gtk_text_buffer_get_iter_at_offset (buffer, @istart, TextStart);
  tagTable := gtk_text_buffer_get_tag_table(buffer);
  scrtag := gtk_text_tag_table_lookup(tagTable, TagNameSubOrSuper);
  hasscript := gtk_text_iter_has_tag(@istart, scrtag);

  if not hasscript then begin
    iend:=istart;
    gtk_text_iter_forward_to_tag_toggle(@iend, scrtag);
    hasscript:=gtk_text_iter_get_offset(@iend)<TextStart+TextLen;
  end;

  if vs = vpNormal then begin
    // no need to do anything, since no modifications were applied;
    if not hasscript then Exit;
    gtk_text_buffer_get_iter_at_offset (buffer, @iend, TextStart+TextLen);
    gtk_text_buffer_remove_tag(buffer, scrtag, @istart, @iend);
    tagName := GetTagName('SSNormal', tagSubSuperCounter);
    tag := gtk_text_buffer_create_tag (buffer, pchar(tagName),
        'rise',     [0,
        'rise-set',  gboolean(gTRUE),
        nil]);
    gtk_text_buffer_apply_tag(buffer, tag, @istart, @iend);
    {$IFDEF DEBUG}
    {%H-}DebugLn('Applied vpNormal Tag');
    {$ENDIF}
  end else begin
    gtk_text_buffer_get_iter_at_offset (buffer, @iend, TextStart+TextLen);

    if vs = vpSubScript then begin
      k := SubRiseKoef;
      tagName := 'Sub';
    end else begin
      k := SuperRiseKoef;
      tagName := 'Super';
    end;
    sz := round(fontSizePts * k * PANGO_SCALE);

    tagName := GetTagName('SS'+TagName, tagSubSuperCounter);
    tag := gtk_text_buffer_create_tag (buffer, pchar(tagName),
        'rise',     [sz,
        'rise-set',  gboolean(gTRUE),
        'size-set',       gboolean(gTRUE),
        'size-points',    gdouble(fontSizePts*SubSuperFontKoef),
        nil]);
    gtk_text_buffer_apply_tag(buffer, tag, @istart, @iend);
    gtk_text_buffer_apply_tag(buffer, scrtag, @istart, @iend);
    {$IFDEF DEBUG}
    {%H-}DebugLn('Applied Sub or Super Script Tags');
    {$ENDIF}
  end;
end;

class procedure TGtk2WSCustomRichMemo.ClearParagraph(rich: TCustomRichMemo;
  TextPos: Integer);
var
  v       : PGtkTextView;
  b       : PGtkTextBuffer;
  istart  : TGtkTextIter;
  iend    : TGtkTextIter;
begin
  GetWidgetBuffer(Rich, PGtkWidget(v), b);
  gtk_text_buffer_get_iter_at_offset(b, @iStart, TextPos);
  gtk_text_iter_set_line_offset(@istart, 0);
  iend := istart;
  gtk_text_iter_forward_to_line_end(@iend);
  gtk_text_buffer_delete(b, @istart, @iend);
end;

class procedure TGtk2WSCustomRichMemo.GetAttributesAt(
  const AWinControl: TWinControl; TextStart: Integer; APara: Boolean;
  var attr: PGtkTextAttributes; var fp: TFontParams);
var
  iter : TGtkTextIter;
  v    : PGtkTextView;
  b    : PGtkTextBuffer;
  tag  : PGtkTextTag;
begin
  InitFontParams(fp);
  GetWidgetBuffer(AWinControl, PGtkWidget(v), b);

  gtk_text_buffer_get_iter_at_offset(b, @iter, TextStart);
  if APara then gtk_text_iter_set_line_offset(@iter, 0);

  attr:=GetAttrAtIter(v, iter);
  if not Assigned(attr) then Exit;

  tag := gtk_text_tag_table_lookup( gtk_text_buffer_get_tag_table(b), TagNameSubOrSuper );

  if gtk_text_iter_has_tag(@iter, tag) then begin
    GtkTextAttrToFontParams(attr^, fp, 1 / SubSuperFontKoef );
    if attr^.appearance.rise < 0 then fp.VScriptPos:=vpSubScript
    else if attr^.appearance.rise > 0 then fp.VScriptPos:=vpSuperScript;
  end else
    GtkTextAttrToFontParams(attr^, fp);

end;

class procedure TGtk2WSCustomRichMemo.SetTextAttributes(const AWinControl: TWinControl; TextStart, TextLen: Integer; const Params: TIntFontParams);
var
  TextWidget: PGtkWidget;
  buffer  : PGtkTextBuffer;
  tag     : Pointer;
  gcolor  : TGdkColor;
  bgcolor : TGdkColor;
  nm      : string;
  tagName : string;
  newTag  : boolean;
const
  pu: array [Boolean] of gint = (PANGO_UNDERLINE_NONE, PANGO_UNDERLINE_SINGLE);
  pb: array [Boolean] of gint = (PANGO_WEIGHT_NORMAL, PANGO_WEIGHT_BOLD);
  pi: array [Boolean] of gint = (PANGO_STYLE_NORMAL, PANGO_STYLE_ITALIC);
begin
  GetWidgetBuffer(AWinControl, TextWidget, buffer);
  if not Assigned(buffer) then Exit;

  gcolor := TColortoTGDKColor(Params.Color);

  bgcolor := TColortoTGDKColor(Params.BkColor);
  if not Params.HasBkClr then begin
    // applying a no background style over a tag with background
    // will expose the underlying background
    gtk_text_buffer_split_tags_at_offset(TextWidget, buffer, TextStart, TextLen);
  end;

  tagName := 'attr:' + StringReplace(Params.Name, ' ', '_', [rfReplaceAll]);
  tagName += '-' + IntToStr(Params.Size);
  tagName += '-' + ColorToString(Params.Color);
  if Params.HasBkClr then
    tagName += '-' + ColorToString(Params.BkColor);
  tagName += '-';
  if fsBold in Params.Style then tagName += 'B';
  if fsItalic in Params.Style then tagName += 'I';
  if fsUnderline in Params.Style then tagName += 'U';
  if fsStrikeOut in Params.Style then tagName += 'S';

  tag := gtk_text_tag_table_lookup( gtk_text_buffer_get_tag_table(buffer), pchar(tagName));
  newTag := tag=nil;
  if newTag then begin
    nm := Params.Name;
    if nm = '' then nm := #0;
    {$IFDEF DEBUG}
    {%H-}DebugLn('New Attr tag: ', tagName);
    {$ENDIF}
    tag := gtk_text_buffer_create_tag (buffer, pchar(tagName),
        'family-set',     [gboolean(gTRUE),
        'family',         @nm[1],
        'foreground-gdk', @gcolor,
        'foreground-set', gboolean(gTRUE),
        'background-gdk', @bgcolor,
        'background-set', gboolean(Params.HasBkClr),
        'size-set',       gboolean(gTRUE),
        'size-points',    gdouble(Params.Size),
        'underline-set',  gboolean(gTRUE),
        'underline',      gint(pu[fsUnderline in Params.Style]),
        'weight-set',     gboolean(gTRUE),
        'weight',         gint(pb[fsBold in Params.Style]),
        'style-set',      gboolean(gTRUE),
        'style',          gint(pi[fsItalic in Params.Style]),
        'strikethrough-set', gboolean(gTRUE),
        'strikethrough',    gboolean(fsStrikeOut in Params.Style),
        nil]);
  end
  {$IFDEF DEBUG}
  else {%H-}DebugLn('Reusing Attr tag ', tagName)
  {$ENDIF}
  ;

  ApplyTag(buffer, tag, TextStart, TextLen, false, newTag);
  FormatSubSuperScript(buffer, Params.VScriptPos, Params.Size, TextStart, TextLen);
end;

class function TGtk2WSCustomRichMemo.GetParaAlignment(
  const AWinControl: TWinControl; TextStart: Integer; var AAlign: TIntParaAlignment
  ): Boolean;
var
  attr       : PGtkTextAttributes;
begin
  attr:=GetAttrAtPos(AWinControl, TextStart, true);
  Result := Assigned(attr);
  if Result then begin
    case attr^.justification of
      GTK_JUSTIFY_LEFT:   AAlign:=paLeft;
      GTK_JUSTIFY_RIGHT:  AAlign:=paRIGHT;
      GTK_JUSTIFY_CENTER: AAlign:=paCenter;
      GTK_JUSTIFY_FILL:   AAlign:=paJustify;
    else
      AAlign:=paLeft;
    end;
    gtk_text_attributes_unref(attr);
  end;
end;

class procedure TGtk2WSCustomRichMemo.SetParaAlignment(
  const AWinControl: TWinControl; TextStart, TextLen: Integer;
  const AAlign: TIntParaAlignment);
var
  w      : PGtkWidget;
  buffer : PGtkTextBuffer;
  tag    : PGtkTextTag;
  val    : Integer;
  tagName: string;
begin

  GetWidgetBuffer(AWinControl, w, buffer);

  // check if this is a line item, skip if so.
  // TODO: Alternative: proceed and apply tag without adjusting priority
  if gtk_text_buffer_offset_is_list_item(buffer, TextStart) then begin
    {$IFDEF DEBUG}
    DebugLn('Tried to set ParaAlignment on list item, skipping');
    {$ENDIF}
    exit;
  end;

  case AAlign of
    paRight:   begin val:=GTK_JUSTIFY_RIGHT; tagName:='R'; end;
    paCenter:  begin val:=GTK_JUSTIFY_CENTER; tagName:='C'; end;
    paJustify: begin val:=GTK_JUSTIFY_FILL; tagName:='J'; end;
  else
    begin
      val := GTK_JUSTIFY_LEFT;
      tagName := 'L';
    end;
  end;

  tagName := GetTagName('just:'+TagName, tagJustCounter);
  tag := gtk_text_buffer_create_tag (buffer, pchar(tagName),
      'justification', [   gint(val),
      'justification-set', gboolean(gTRUE),
      nil]);
  ApplyTag(buffer, tag, TextStart, TextLen, true);

  {$IFDEF DEBUG}
  DumpTagTable(buffer, 'SetParaAlignment');
  {$ENDIF}
end;

class function TGtk2WSCustomRichMemo.GetParaMetric(
  const AWinControl: TWinControl; TextStart: Integer;
  var AMetric: TIntParaMetric): Boolean;
var
  attr    : PGtkTextAttributes;
  fp      : TFontParams;
  PixToPt : double;
begin
  GetAttributesAt(AWinControl, TextStart, true, attr, fp{%H-});
  Result := Assigned(attr);
  if Result then begin
    PixToPt := PageDPI / ScreenDPI;
    if attr^.indent<0 then begin
      AMetric.FirstLine:=(attr^.left_margin)*PixToPt;
      AMetric.HeadIndent:=(-attr^.indent+attr^.left_margin)*PixToPt;
    end else begin
      AMetric.FirstLine:=(attr^.left_margin+attr^.indent)*PixToPt;
      AMetric.HeadIndent:=attr^.left_margin*PixToPt;
    end;
    AMetric.TailIndent:=attr^.right_margin*PixToPt;

    AMetric.SpaceAfter:=attr^.pixels_above_lines*PixToPt;
    AMetric.SpaceBefore:=attr^.pixels_below_lines*PixToPt;
    AMetric.LineSpacing:=(attr^.pixels_inside_wrap*PixToPt+fp.Size)/(fp.Size);
    gtk_text_attributes_unref(attr);
  end;
end;

class procedure TGtk2WSCustomRichMemo.SetParaMetric(
  const AWinControl: TWinControl; TextStart, TextLen: Integer;
  const AMetric: TIntParaMetric);
var
  w      : PGtkWidget;
  buffer : PGtkTextBuffer;
  tag    : PGtkTextTag;
  h      : double;
  fl     : double;
  t      : double;
  ls     : double;
  attr   : PGtkTextAttributes;
  fp     : TFontParams;
  DPIFactor: Double;

  left_margin, indent, right_margin, pixels_above, pixels_below,
  pixels_inside: gint;
  {$IFDEF DEBUG}
  count: gint;
  {$ENDIF}
  tagName: String;
  tagTable: PGtkTextTagTable;
  isNewTag: Boolean;
begin

  GetWidgetBuffer(AWinControl, w, buffer);

  // check if this is a line item, skip if so.
  // TODO: Alternative: proceed and apply tag without adjusting priority
  if gtk_text_buffer_offset_is_list_item(buffer, TextStart) then begin
    {$IFDEF DEBUG}
    DebugLn('Tried to set ParaMetrics on list item, skipping');
    {$ENDIF}
    exit;
  end;

  h:=AMetric.HeadIndent;
  if h<0 then h:=0;
  fl:=AMetric.FirstLine;
  if fl<0 then fl:=0;

  if fl<h then begin
    t:=h;
    h:=fl;
    fl:=fl-t;
  end else
    fl:=fl-h;

  ls := AMetric.LineSpacing;
  if ls<DefLineSpacing then
    ls := DefLineSpacing;

  GetAttributesAt(AWinControl, TextStart, true, attr, fp{%H-});
  gtk_text_attributes_unref(attr);
  DPIFactor := ScreenDPI / PageDPI;

  left_margin   := gint(round(h*DPIFactor));
  indent        := gint(round(fl*DPIFactor));
  right_margin  := gint(round(AMetric.TailIndent*DPIFactor));
  pixels_above  := gint(round(AMetric.SpaceBefore*DPIFactor));
  pixels_below  := gint(round(AMetric.SpaceAfter*DPIFactor));
  pixels_inside := gint((round(fp.Size*(ls-DefLineSpacing)*DPIFactor)));

  tagName := format('pm:%d-%d-%d_%d-%d-%d',
    [left_margin, indent, right_margin, pixels_above, pixels_inside, pixels_below]);

  tagTable := gtk_text_buffer_get_tag_table(buffer);
  {$IFDEF DEBUG}
  count := gtk_text_tag_table_get_size(tagTable);
  {$ENDIF}

  tag := gtk_text_tag_table_lookup( tagTable, pchar(tagName));
  isNewTag := tag=nil;
  if isNewTag then begin
    {$IFDEF DEBUG}
    DebugLn('FirstTime Tag: %s Previous tag count=%d', [tagname, count]);
    {$ENDIF}
    tag := gtk_text_buffer_create_tag (buffer, pchar(tagName),
        'pixels-above-lines',   [ pixels_above,
        'pixels-above-lines-set', gboolean(gTRUE),
        'pixels-below-lines',     pixels_below,
        'pixels-below-lines-set', gboolean(gTRUE),
        'left-margin',            left_margin,
        'left-margin-set',        gboolean(gTRUE),
        'right-margin',           right_margin,
        'right-margin-set',       gboolean(gTRUE),
        'indent',                 indent,
        'indent-set',             gboolean(gTRUE),
        'pixels-inside-wrap',     pixels_inside,
        'pixels-inside_wrap-set', gboolean(gTRUE),
        nil]);
  end
  {$IFDEF DEBUG}
  //else DebugLn('reused tag: %s there are %d tags in table', [tagname, count])
  {$ENDIF}
  ;
  ApplyTag(buffer, tag, TextStart, TextLen, true, isNewTag);
end;

class function TGtk2WSCustomRichMemo.GetParaNumbering(
  const AWinControl: TWinControl; TextStart: Integer;
  var ANumber: TIntParaNumbering): Boolean;
var
  w: PGtkWidget;
  b: PGtkTextBuffer;
  tag: PGtkTextTag;
  istart, iend: TGtkTextIter;
  tagText: pgchar;
begin

  result := false;
  GetWidgetBuffer(AWinControl, w, b);
  if not Assigned(w) or not Assigned(b) then Exit;

  InitParaNumbering(ANumber);
  result := true;

  tag := gtk_text_tag_table_lookup( gtk_text_buffer_get_tag_table(b), TagNameNumeric);

  gtk_text_buffer_get_iter_at_offset (b, @istart, TextStart);
  gtk_text_iter_set_line_offset(@istart, 0);

  if gtk_text_iter_begins_tag(@istart, tag) then begin

    iend := iStart;
    gtk_text_iter_forward_to_tag_toggle(@iend, tag);

    tagText := gtk_text_iter_get_text(@istart, @iend);
    result := tagText<>nil;
    if result then
      result := gtk_text_iter_get_numbering(@istart, @iend, ANumber);
  end;
end;

class procedure TGtk2WSCustomRichMemo.SetParaNumbering(
  const AWinControl: TWinControl; TextStart, TextLen: Integer;
  const ANumber: TIntParaNumbering);
var
  w      : PGtkWidget;
  b      : PGtkTextBuffer;
  istart : TGtkTextIter;
  iend   : TGtkTextIter;
  txt    , tagName: String;
  len    : Integer;
  ln     : Integer;
  ls     : Integer;
  ofs    : Integer;
  numidx : Integer;
  tag    : PGtkTextTag;
  attr   : PGtkTextAttributes;
  leftMargin, sepPos: gint;
  isNewTag: Boolean;
  parr: PPangoTabArray;
begin
  inherited SetParaNumbering(AWinControl, TextStart, TextLen, ANumber);
  GetWidgetBuffer(AWinControl, w, b);
  if not Assigned(w) or not Assigned(b) then Exit;

  tag := gtk_text_tag_table_lookup( gtk_text_buffer_get_tag_table(b), TagNameNumeric);

  gtk_text_buffer_get_iter_at_offset (b, @istart, TextStart);

  iend:=istart;
  gtk_text_iter_forward_chars(@iend, TextLen);
  ln:=gtk_text_iter_get_line(@istart);
  ls:=gtk_text_iter_get_line(@iend);

  numidx:=1;
  if ANumber.Style in [pnNumber..pnUpRoman] then
    numidx:=ANumber.NumberStart;

  repeat
    gtk_text_iter_set_line_offset(@istart, 0);
    case ANumber.Style of
      pnBullet: txt := BulletChar;
      pnNumber: txt := IntToStr(numidx);
      pnLowLetter: txt := chr( ord('a') + numidx - 1);
      pnLowRoman:  txt := lowercase(IntToRoman(numidx));
      pnUpLetter:  txt := chr( ord('A') + numidx - 1);
      pnUpRoman:   txt := IntToRoman(numidx);
      pnCustomChar: txt:= UTF8Encode(ANumber.CustomChar);
    end;
    sepPos := 0;
    if not (ANumber.Style in [pnBullet, pnCustomChar]) and (ANumber.SepChar<>#0) then begin
      sepPos := Length(txt)+1; // byte Pos
      txt:=txt+UTF8Encode(ANumber.SepChar);
    end else
      sepPos := Length(txt)+1;
    txt:=txt+TabChar;

    //gtk_text_buffer_get_iter_at_offset (b, @iend, TextStart);
    //gtk_text_buffer_move
    ofs:=gtk_text_iter_get_offset(@istart);

    // remove existing number
    if gtk_text_iter_begins_tag(@istart, tag) then begin
      iend:=istart;
      gtk_text_iter_forward_to_tag_toggle(@iend, nil);
      gtk_text_buffer_delete(b, @istart, @iend);
      gtk_text_buffer_get_iter_at_offset (b, @istart, ofs);
    end;

    if ANumber.Style<>pnNone then begin
      // insert new number
      gtk_text_buffer_insert(b, @istart, @txt[1], length(txt));

      // apply original tags to new text
      gtk_text_buffer_copy_tags_from_offset(b, @istart, ofs, length(txt));

      // restoring iterators
      gtk_text_buffer_get_iter_at_offset (b, @istart, ofs);
      gtk_text_iter_set_line_offset(@istart, 0);
      iend := istart;

      len:=UTF8Length(txt);
      gtk_text_iter_forward_chars(@iend, len);

      // applying tag
      gtk_text_buffer_apply_tag_by_name(b, TagNameNumeric, @istart, @iend);

      // apply nice indentation
      attr:=GetAttrAtIter(PGtkTextView(w), istart);
      if Assigned(attr) then begin

        leftMargin := attr^.left_margin;
        gtk_text_attributes_unref(attr);

        tagName := format('list:%d_%d_%d_%d_%d_%d',
          [ord(ANumber.Style), sepPos, leftMargin, RichListMargin, RichListIndent, RichListTabWidth]);
        tag := gtk_text_tag_table_lookup( gtk_text_buffer_get_tag_table(b), pchar(tagName));
        isNewTag := tag=nil;
        if isNewTag then begin
          {$IFDEF DEBUG}
          DebugLn('FirstTime Tag: %s', [tagname]);
          {$ENDIF}
          // ref: https://stackoverflow.com/a/63291090
          parr:=pango_tab_array_new(2, true);
          pango_tab_array_set_tab(parr, 0, PANGO_TAB_LEFT, 0);
          pango_tab_array_set_tab(parr, 1, PANGO_TAB_LEFT, RichListTabWidth);
          tag := gtk_text_buffer_create_tag(b, pchar(tagName),
              'left_margin', [ gint(leftMargin + RichListMargin),
              'indent',  gInt(-RichListIndent),
              'wrap_mode', gint(GTK_WRAP_WORD),
              'tabs', parr,
              nil]);
        end
        {$IFDEF DEBUG}
        else DebugLn('Reusing tag %s',[tagName])
        {$ENDIF}
        ;

        ApplyTag(b, tag, istart, iend, false, isNewTag);
        {$IFDEF DEBUG}
        DumpTagTable(b, 'SetParaNumbering');
        {$ENDIF}
      end;

    end;

    // next line!
    gtk_text_iter_forward_line(@istart);

    inc(ln);
    inc(numidx);
  until ln>ls;
end;

class procedure TGtk2WSCustomRichMemo.SetParaTabs(
  const AWinControl: TWinControl; TextStart, TextLen: Integer;
  const AStopList: TTabStopList);
var
  w      : PGtkWidget;
  buffer : PGtkTextBuffer;
  tag    : PGtkTextTag;
  parr   : PPangoTabArray;
  i      : Integer;
  tagName: String;
  DPIFactor: double;
begin
  GetWidgetBuffer(AWinControl, w, buffer);
  if not Assigned(w) or not Assigned(buffer) then Exit;

  GetWidgetBuffer(AWinControl, w, buffer);

  if AStopList.Count=0 then
    parr:=nil
  else begin
    DPIFactor := ScreenDPI / PageDPI;
    parr:=pango_tab_array_new(AStopList.Count, true);
    for i:=0 to AStopList.Count-1 do begin
      pango_tab_array_set_tab(parr, i, PANGO_TAB_LEFT, round(AStopList.Tabs[i].Offset * DPIFactor) );
    end;
  end;

  tagName := GetTagName('tab', tagTabCounter);
  tag := gtk_text_buffer_create_tag (buffer, pchar(tagName),
      'tabs',   [ parr,
      'tabs-set', gboolean(AStopList.Count>0),
      nil]);
  ApplyTag(buffer, tag, TextStart, TextLen, true);
  {$IFDEF DEBUG}
  {%H-}DebugLn('Applied TABs tag');
  {$ENDIF}
  if Assigned(parr) then pango_tab_array_free(parr);
end;

class function TGtk2WSCustomRichMemo.GetParaTabs(
  const AWinControl: TWinControl; TextStart: integer;
  var AStopList: TTabStopList): Boolean;
var
  i      : Integer;
  attr   : PGtkTextAttributes;
  loc    : gint;
  al     : TPangoTabAlign;
  f      : Double;
begin
  InitTabStopList(AStopList);
  attr:=GetAttrAtPos(AWinControl, TextStart, true);
  Result:=Assigned(attr);
  if not Result then Exit;
  if not Assigned(attr^.tabs) then Exit;

  AStopList.Count:=pango_tab_array_get_size(attr^.tabs);
  if AStopList.Count=0 then Exit;

  f := PageDPI / ScreenDPI;
  if not pango_tab_array_get_positions_in_pixels(attr^.tabs) then
    f:= f / PANGO_SCALE;

  SetLength(AStopList.Tabs, AStopList.Count);
  for i:=0 to AStopList.Count-1 do begin
    pango_tab_array_get_tab(attr^.tabs, i, @al, @loc);
    AStopList.Tabs[i].Offset:=loc*f;
    AStopList.Tabs[i].Align:=tabLeft;
  end;
  gtk_text_attributes_unref(attr);
end;

class function TGtk2WSCustomRichMemo.GetParaRange(
  const AWinControl: TWinControl; TextStart: Integer; var rng: TParaRange
  ): Boolean;
var
  w : PGtkWidget;
  b : PGtkTextBuffer;
  istart : TGtkTextIter;
  iend   : TGtkTextIter;
begin
  GetWidgetBuffer(AWinControl, w, b);
  if not Assigned(b) then begin
    Result:=false;
    Exit;
  end;
  gtk_text_buffer_get_iter_at_offset (b, @istart, TextStart);
  gtk_text_iter_set_line_offset(@istart, 0);
  iend := istart;
  gtk_text_iter_forward_to_line_end(@iend);
  rng.start:=gtk_text_iter_get_offset(@istart);
  rng.lengthNoBr:=gtk_text_iter_get_offset(@iend)-rng.start;

  // if there's a character to move, then it's end of line, if not then it won't change!
  gtk_text_iter_forward_char(@iend);
  rng.length:=gtk_text_iter_get_offset(@iend)-rng.start;
  Result:=true;
end;

class procedure TGtk2WSCustomRichMemo.SetTextUIParams(
  const AWinControl: TWinControl; TextStart, TextLen: Integer;
  const ui: TTextUIParam);
var
  TextWidget: PGtkWidget;
  buffer  : PGtkTextBuffer;
  tag     : PGtkTextTag;
  istart  : TGtkTextIter;
  iend    : TGtkTextIter;
  gcolor  : TGdkColor;
  tagName: String;
begin
  GetWidgetBuffer(AWinControl, TextWidget, buffer);
  if not Assigned(buffer) then Exit;

  gtk_text_buffer_get_iter_at_offset (buffer, @istart, TextStart);
  gtk_text_buffer_get_iter_at_offset (buffer, @iend, TextStart+TextLen);
  if uiLink in ui.features then begin
    gColor := TColortoTGDKColor(clGreen); // todo: make an option
    tagName := GetTagName('link', tagLinkCounter);
    tag := gtk_text_buffer_create_tag(buffer, pchar(tagName),
        'foreground-gdk', [ @gcolor,
        'underline',      PANGO_UNDERLINE_SINGLE,
        NULL ] );
    gtk_texttag_set_linkref(tag, ui.linkref);
    gtk_text_buffer_apply_tag(buffer, tag, @istart, @iend);
  end else begin
    tag := gtk_text_buffer_get_linkref_tag_at_offset(buffer, TextStart);
    if Assigned(tag) then
      gtk_text_buffer_remove_tag(buffer, tag, @istart, @iend);
  end;
end;

class function TGtk2WSCustomRichMemo.GetTextUIParams(
  const AWinControl: TWinControl; TextStart: Integer;
  var ui: TTextUIParam): Boolean;
var
  TextWidget: PGtkWidget;
  buffer  : PGtkTextBuffer;
  tag     : PGtkTextTag;
begin
  ui.features:=[];
  GetWidgetBuffer(AWinControl, TextWidget, buffer);
  Result:=Assigned(buffer);
  if not Result then Exit;

  tag := gtk_text_buffer_get_linkref_tag_at_offset(buffer, TextStart);
  result := Assigned(tag);
  if result then begin
    Include(ui.features, uiLink);
    ui.linkref := gtk_texttag_get_linkref(tag);
  end;
end;

class procedure TGtk2WSCustomRichMemo.InDelText(const AWinControl: TWinControl;
  const TextUTF8: String; DstStart, DstLen: Integer);
var
  w : PGtkWidget;
  b : PGtkTextBuffer;
  istart : TGtkTextIter;
  iend   : TGtkTextIter;
  aPos: SizeInt;
  ANumber: TParaNumbering;
  offset, endOffset: gint;
begin
  GetWidgetBuffer(AWinControl, w, b);
  if not Assigned(b) then Exit;

  // Select 'dst' range and delete it
  // todo: Check deleting of numbered list items...
  gtk_text_buffer_get_iter_at_offset (b, @istart, DstStart);
  gtk_text_buffer_get_iter_at_offset (b, @iend, DstStart+DstLen);
  gtk_text_buffer_delete(b, @istart, @iend);

  if length(TextUTF8)>0 then begin

    gtk_text_buffer_insert(b, @iend, @textUTF8[1], length(TextUTF8));

    aPos := pos(#10, textUTF8);
    if aPos=0 then begin
      aPos := pos(#13, textUTF8);
      if aPos=0 then
        exit;
    end;

    // now istart points to original insert point and iend to past-inserted text
    endOffset := gtk_text_iter_get_offset(@iend);

    // find the start of the line containing the insert point
    gtk_text_buffer_get_iter_at_offset (b, @istart, DstStart);
    gtk_text_iter_set_line_offset(@istart, 0);
    offset := gtk_text_iter_get_offset(@istart);
    // check if this line is a numbered paragraph
    GetParaNumbering(AWinControl, offset, ANumber);
    if ANumber.Style = pnNone then exit;
    // advance number
    Inc(ANumber.NumberStart);
    // advance iter to start of next line
    gtk_text_iter_forward_line(@istart);
    offset := gtk_text_iter_get_offset(@istart);
    // renumber list items from offset to endoffset
    SetParaNumbering(AWinControl, offset, endOffset - offset, ANumber);
  end;
end;

class function TGtk2WSCustomRichMemo.CharAtPos(const AWinControl: TWinControl;
  x, y: Integer): Integer;
var
  w : PGtkWidget;
  b : PGtkTextBuffer;
  istart : TGtkTextIter;
  gx, gy : gint;
  trailing: gint;
begin
  GetWidgetBuffer(AWinControl, w, b);
  if not Assigned(w) then Exit;

  gtk_text_view_window_to_buffer_coords(PGtkTextView(w), GTK_TEXT_WINDOW_WIDGET, x, y, @gx, @gy);
  gtk_text_view_get_iter_at_position(PGtkTextView(w), @istart, @trailing, gx,gy);
  Result:=gtk_text_iter_get_offset(@istart)+trailing;
end;

procedure UTF8CharsToWideString(const p: Pchar; out w: WideString);
var
  slen : Integer;
  cnt: Integer;
  sz: SizeUInt;
begin
  if not Assigned(p) then begin
    w:='';
    Exit;
  end;
  slen:=strlen(p);
  if slen=0 then begin
    w:='';
    Exit;
  end;
  cnt:=UTF8Length(p, slen);
  SetLength(w, cnt);
  if cnt>0 then
   ConvertUTF8ToUTF16( @w[1], length(w), p, slen, [toInvalidCharToSymbol], sz);
  SetLength(w, sz);
end;

class function TGtk2WSCustomRichMemo.Search(const AWinControl: TWinControl;
  const ANiddle: string; const SearchOpts: TIntSearchOpt): Integer;
var
  TextWidget   : PGtkWidget;
  buffer       : PGtkTextBuffer;
  istart       : TGtkTextIter;
  iend         : TGtkTextIter;
  start_match  : TGtkTextIter;
  end_match    : TGtkTextIter;
  Found        : Boolean;
  opt          : TGtkTextSearchFlags;
  gstr         : PChar;
  txt          : WideString;
  sub          : WIdeString;
const
  GTK_TEXT_SEARCH_VISIBLE_ONLY     = 1 shl 0;  (* values of TGtkTextSearchFlags *)
  {%H-}GTK_TEXT_SEARCH_TEXT_ONLY        = 1 shl 1;
  GTK_TEXT_SEARCH_CASE_INSENSITIVE = 1 shl 2;
begin
  Result := -1;
  GetWidgetBuffer(AWinControl, TextWidget, buffer);
  if not Assigned(buffer) then Exit;

  opt:=GTK_TEXT_SEARCH_VISIBLE_ONLY;
  if not (soMatchCase in SearchOpts.Options) then begin
    opt:=opt or GTK_TEXT_SEARCH_CASE_INSENSITIVE; // doesn't work anyway! it works in gtk3 only

    gtk_text_buffer_get_iter_at_offset (buffer, @istart, SearchOpts.start);
    gtk_text_buffer_get_iter_at_offset (buffer, @iend, SearchOpts.start+SearchOpts.len);

    gtk_text_buffer_get_text(buffer, @istart, @iend, false);
    gstr := gtk_text_buffer_get_text(Buffer, @istart, @iend, False);
    if Assigned(gstr) then begin
      UTF8CharsToWideString(gstr, txt);
      g_free(gstr);
      txt:=WideUpperCase(txt);
      sub:=WideUpperCase(UTF8Decode(ANiddle));
      Result:=Pos(sub,txt);
      if Result>0 then
        Result:=Result-1+SearchOpts.start
      else
        Result:=-1;
    end else
      Result:=-1;
  end else begin
    gtk_text_buffer_get_iter_at_offset(buffer, @istart, SearchOpts.start );
    if not (soBackward in SearchOpts.Options) then
    begin
      gtk_text_buffer_get_iter_at_offset(buffer, @iend, SearchOpts.start+SearchOpts.len );
      Found := gtk_text_iter_forward_search(@istart, PgChar(ANiddle), opt,
          @start_match, @end_match, @iend)
    end else begin
      gtk_text_buffer_get_iter_at_offset(buffer, @iend, SearchOpts.start-SearchOpts.len);
      Found := gtk_text_iter_backward_search(@istart, PgChar(ANiddle), opt,
          @start_match, @end_match, @iend)
    end;

    if Found
      then Result := gtk_text_iter_get_offset(@start_match)
      else Result := -1;
  end;
end;

class function TGtk2WSCustomRichMemo.ImageFromFile(
  const ARichMemo: TCustomRichMemo; APos: Integer; const FileNameUTF8: string;
  const AImgSize: TSize): Boolean;
var
  t: PGtkWidget;
  b: PGtkTextBuffer;
  istart: TGtkTextIter;
  pix: PGdkPixbuf;
  err: PGError;
  DPIFactor: Double;
begin
  Result:=false;
  GetWidgetBuffer(ARichMemo, t, b);
  if not Assigned(b) then Exit;

  err:=nil;
  DPIFactor := ScreenDPI / PageDPI;

  if (AImgSize.cx=0) and (AImgSize.cy=0) then
    pix := gdk_pixbuf_new_from_file(PChar(FileNameUTF8), @err)
  else
    pix := gdk_pixbuf_new_from_file_at_size(PChar(FileNameUTF8),
      round(AImgSize.cx * DPIFactor),  round(AImgSize.cy * DPIFactor), @err);

  Result:=Assigned(pix);
  if Result then begin
    gtk_text_buffer_get_iter_at_offset(b, @istart, APos);
    gtk_text_buffer_insert_pixbuf(b, @istart, pix);
  end else
    writeln(err^.message);
end;

class procedure TGtk2WSCustomRichMemo.SetSelStart(const ACustomEdit: TCustomEdit; NewStart: integer);
var
  TextMark: PGtkTextMark;
  CursorIter: TGtkTextIter;
  w      : PGtkWidget;
  b      : PGtkTextBuffer;
begin
  GetWidgetBuffer(ACustomEdit, w, b);
  if not Assigned(b) then Exit;

  if NewStart = -1 then
  begin
    // always scroll so the cursor is visible
    TextMark := gtk_text_buffer_get_insert(b);
    gtk_text_buffer_get_iter_at_mark(b, @CursorIter, TextMark);
  end
  else begin
    // SelStart was used and we should move to that location
    gtk_text_buffer_get_iter_at_offset(b, @CursorIter, NewStart);
    gtk_text_buffer_place_cursor(b, @CursorIter); // needed to move the cursor
    TextMark := gtk_text_buffer_get_insert(b);
  end;
  gtk_text_view_scroll_to_mark(PGtkTextView(w), TextMark, 0, True, 0, 1);
end;

class procedure TGtk2WSCustomRichMemo.SetSelLength(
  const ACustomEdit: TCustomEdit; NewLength: integer);
var
  TextMark: PGtkTextMark;
  StartIter,
  EndIter: TGtkTextIter;
  Offset: Integer;
  w      : PGtkWidget;
  b      : PGtkTextBuffer;
begin
  GetWidgetBuffer(ACustomEdit, w, b);
  if not Assigned(b) then Exit;

  TextMark := gtk_text_buffer_get_insert(b);
  gtk_text_buffer_get_iter_at_mark(b, @StartIter, TextMark);

  Offset := gtk_text_iter_get_offset(@StartIter);

  gtk_text_buffer_get_iter_at_offset(b, @EndIter, Offset+NewLength);

  gtk_text_buffer_select_range(b, @StartIter, @EndIter);
end;

class procedure TGtk2WSCustomRichMemo.SetZoomFactor(
  const AWinControl: TWinControl; AZoomFactor: Double);
var
  w      : PGtkWidget;
  b      : PGtkTextBuffer;
  tag    : PGtkTextTag;
  istart : TGtkTextIter;
  iend   : TGtkTextIter;
  p      : PGtkTextAttributes;
  sc     : gdouble;
  tagName: String;
begin
  GetWidgetBuffer(AWinControl, w, b);
  if not Assigned(b) then Exit;

  p:=GetAttrAtPos(AWinControl, 0);
  sc:=p^.font_scale;
  if sc=0 then sc:=1;
  gtk_text_attributes_unref(p);
  // restore the scale.
  // for whatever reason, scale is always assumed as a multiplier!
  // thus it is necessary to "unscale" the previous value as well
  sc:=1/sc*AZoomFactor;

  tagName := GetTagName('zoom', tagZoomCounter);
  tag := gtk_text_buffer_create_tag(b, pchar(tagName),
      'scale', [   gdouble(sc),
      'scale-set', gboolean(gTRUE),
      nil]);

  //gtk_text_buffer_get_start_iter(b, @istart);
  gtk_text_buffer_get_iter_at_offset(b, @istart, 0);
  gtk_text_buffer_get_end_iter(b, @iend);
  gtk_text_buffer_apply_tag(b, tag, @istart, @iend);

  //todo: set default font with scale
end;

function GtkDrawableDraw(widget: PGtkWidget;
  event : PGdkEventExpose; gi: TGtk2InlineObject): gboolean; cdecl;
begin
  if not Assigned(widget) then Exit;
  if not gi.cnv.HandleAllocated then
    gi.cnv.Handle:=GTK2WidgetSet.CreateDCForWidget(widget, event^.Window,false);
  gi.il.Draw( gi.cnv, gi.Size);
  Result:=gTRUE;
end;

class function TGtk2WSCustomRichMemo.InlineInsert(
  const AWinControl: TWinControl; ATextStart, ATextLength: Integer;
  const ASize: TSize; AHandler: TRichMemoInline;
  var wsObj: TRichMemoInlineWSObject): Boolean;
var
  w      : PGtkWidget;
  b      : PGtkTextBuffer;
  istart : TGtkTextIter;
  anch   : PGtkTextChildAnchor;
  gi     : TGtk2InlineObject;
  draw   : PGtkWidget;
  sz     : TSize;
  DPIFactor: double;
begin
  Result:=false;
  GetWidgetBuffer(AWinControl, w, b);
  if not Assigned(b) then Exit;

  gi:=TGtk2InlineObject.Create;

  gtk_text_buffer_get_iter_at_offset(b, @istart, ATextStart);
  anch:=gtk_text_buffer_create_child_anchor(b, @istart);

  draw:=gtk_drawing_area_new;
  ConnectSignal( PGtkObject(draw), 'expose-event', @GtkDrawableDraw, gi);

  DPIFactor := ScreenDPI / PageDPI;
  sz.cx:=round(ASize.cx * DPIFactor);
  sz.cy:=round(ASize.cy * DPIFactor);
  gtk_widget_set_size_request(draw, sz.cx, sz.cy);
  gtk_text_view_add_child_at_anchor(PGtkTextView(w), draw, anch);

  gi.il:=AHandler;
  gi.anch:=anch;
  gi.wgt:=draw;
  gi.size:=sz;
  gi.cnv:=TCanvas.Create;

  gtk_widget_show(draw);
  gi.il.SetVisible(true);

  wsObj:=gi;
  Result:=true;
end;

class procedure TGtk2WSCustomRichMemo.InlineInvalidate(
  const AWinControl: TWinControl; AHandler: TRichMemoInline;
  wsObj: TRichMemoInlineWSObject);
var
  gi : TGtk2InlineObject;
begin
  if not Assigned(wsObj) or not (wsObj is TGtk2InlineObject) then Exit;
  gi := TGtk2InlineObject(wsObj);
  if Assigned(gi.wgt) then
    gtk_widget_queue_draw(gi.wgt);
end;


class function TGtk2WSCustomRichMemo.GetTextAttributes(const AWinControl: TWinControl;
   TextStart: Integer; var Params: TIntFontParams): Boolean;
var
  attr : PGtkTextAttributes;
begin
  GetAttributesAt(AWinControl, TextStart, false, attr, params);
  Result := Assigned(attr);
  if Result then gtk_text_attributes_unref(attr);
end;

function GtkInsertImageFromFile(const ARichMemo: TCustomRichMemo; APos: Integer;
     const FileNameUTF8: string;
     const AImgSize: TSize
  ): Boolean;
begin
  Result:=TGtk2WSCustomRichMemo.ImageFromFile(ARichMemo, APos, FileNameUTF8, AImgSize);
end;

function GtkWSGetFontParams(fontref: HFONT; var params: TFontParams): Boolean;
var
  gtkobj: PGDIObject;
  pangofont: PPangoLayout;
  PangoDesc: PPangoFontDescription;
  sz       : Integer;
  isSzAbs  : Boolean;
begin
  gtkobj:=PGDIObject(fontref);
  Result:=Assigned(gtkobj) and (gtkobj^.GDIType=gdiFont);
  if not Result then Exit;

  if gtkobj^.LogFont.lfFaceName = 'default' then begin
    pangofont:=GTK2WidgetSet.GetDefaultGtkFont(False);
    Result:=PANGO_IS_LAYOUT(pangofont);
    if Result then
    begin
      // default font name
      PangoDesc := pango_layout_get_font_description(pangofont);
      if not Assigned(PangoDesc) then
        PangoDesc := pango_context_get_font_description(pango_layout_get_context(pangofont));
      params.Name := StrPas(pango_font_description_get_family(PangoDesc));

      // default font size
      if gtkobj^.LogFont.lfHeight = 0 then begin
        isSzAbs := pango_font_description_get_size_is_absolute(PangoDesc);
        sz := pango_font_description_get_size(PangoDesc);
        if not isSzAbs then
          params.Size := round(sz / PANGO_SCALE)
        else
          params.Size := round(sz/ScreenInfo.PixelsPerInchY*72);
      end;

      // rely on LogFont structure to be initialiazed (as it seems to be)
      if gtkobj^.LogFont.lfItalic > 0 then Include(params.Style, fsItalic);
      if gtkobj^.LogFont.lfWeight >= FW_BOLD then Include(params.Style, fsBold);
      if gtkobj^.LogFont.lfUnderline > 0 then Include(params.Style, fsUnderline);
      if gtkobj^.LogFont.lfStrikeOut > 0 then Include(params.Style, fsStrikeOut);
    end;
  end else
    Result:=false;
end;

initialization
  InsertImageFromFile := @GtkInsertImageFromFile;
  WSGetFontParams:=@GtkWSGetFontParams;

end.

