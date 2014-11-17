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

interface

uses
  // Bindings
  gtk2, glib2, gdk2, pango,
  // FCL
  Classes, SysUtils,
  // LCL
  LCLType, Controls, Graphics,
  // Gtk2 widget
  Gtk2Def,
  GTK2WinApiWindow, Gtk2Globals, Gtk2Proc, InterfaceBase,
  Gtk2WSControls,
  // RichMemo
  WSRichMemo;

  { TGtk2WSCustomRichMemo }
type
  TGtk2WSCustomRichMemo = class(TWSCustomRichMemo)
  protected
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo);
    class procedure GetWidgetBuffer(const AWinControl: TWinControl; var TextWidget: PGtkWidget; var Buffer: PGtkTextBuffer);
    class function GetAttrAtPos(const AWinControl: TWinControl; TextStart: Integer): PGtkTextAttributes;
    class procedure ApplyTag(abuffer: PGtkTextBuffer; tag: PGtkTextTag; TextStart, TextLen: Integer; ToParagraphs: Boolean = False);
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;

    class function GetTextAttributes(const AWinControl: TWinControl; TextStart: Integer; var Params: TIntFontParams): Boolean; override;
    class procedure SetTextAttributes(const AWinControl: TWinControl; TextStart, TextLen: Integer; const Params: TIntFontParams); override;

    class function GetParaAlignment(const AWinControl: TWinControl; TextStart: Integer;
      var AAlign: Integer): Boolean; override;
    class procedure SetParaAlignment(const AWinControl: TWinControl; TextStart, TextLen: Integer;
      const AAlign: Integer); override;

    class procedure SetParaMetric(const AWinControl: TWinControl; TextStart, TextLen: Integer;
      const AMetric: TIntParaMetric); override;

    class procedure InDelText(const AWinControl: TWinControl; const TextUTF8: String; DstStart, DstLen: Integer); override;
  end;

implementation


function gtktextattr_underline(const a : TGtkTextAppearance) : Boolean;
begin
  Result:=((a.flag0 and bm_TGtkTextAppearance_underline) shr bp_TGtkTextAppearance_underline) > 0;
end;

function gtktextattr_strikethrough(const a : TGtkTextAppearance) : Boolean;
begin
  Result:=((a.flag0 and bm_TGtkTextAppearance_strikethrough) shr bp_TGtkTextAppearance_strikethrough) > 0;
end;

function GtkTextAttrToFontParams(const textAttr: TGtkTextAttributes; var FontParams: TIntFontParams): Boolean;
var
  w   : integer;
  st  : TPangoStyle;
  pf  : PPangoFontDescription;
  sz  : double;
const
  ScreenDPI = 96; // todo: might change, should be received dynamically
  PageDPI   = 72; // not expected to be changed
begin
  FontParams.Style := [];
  FontParams.Name := '';
  FontParams.Size := 0;
  FontParams.Color := 0;

  pf := textAttr.font;
  Result := Assigned(pf);
  if not Result then Exit;

  if Assigned(pf) then begin
    FontParams.Name := pango_font_description_get_family(pf);
    FontParams.Size := pango_font_description_get_size(pf);
    sz:=FontParams.Size / PANGO_SCALE;
    if pango_font_description_get_size_is_absolute(pf) then
      sz:=sz/(ScreenDPI/PageDPI);
    FontParams.Size:=round(sz);
    w := pango_font_description_get_weight(pf);
    if w > PANGO_WEIGHT_NORMAL then Include(FontParams.Style, fsBold);

    st := pango_font_description_get_style(pf);
    if st and PANGO_STYLE_ITALIC > 0 then  Include(FontParams.Style, fsItalic);
  end;

  FontParams.Color := TGDKColorToTColor(textAttr.appearance.fg_color);
  if gtktextattr_underline(textAttr.appearance) then  Include(FontParams.Style, fsUnderline);
  if gtktextattr_strikethrough(textAttr.appearance) then Include(FontParams.Style, fsStrikeOut);
end;


class procedure TGtk2WSCustomRichMemo.SetCallbacks(
  const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo);
begin
   TGtk2WSWinControl.SetCallbacks(PGtkObject(AGtkWidget), TComponent(AWidgetInfo^.LCLObject));
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

class function TGtk2WSCustomRichMemo.GetAttrAtPos(
  const AWinControl: TWinControl; TextStart: Integer): PGtkTextAttributes;
var
  TextWidget : PGtkWidget;
  buffer     : PGtkTextBuffer;
  iter       : TGtkTextIter;
  attr       : PGtkTextAttributes;
begin
  Result:=nil;
  GetWidgetBuffer(AWinControl, TextWidget, buffer);

  attr := gtk_text_view_get_default_attributes(PGtkTextView(TextWidget));
  if not Assigned(attr) then Exit;

  gtk_text_buffer_get_iter_at_offset(buffer, @iter, TextStart);
  gtk_text_iter_get_attributes(@iter, attr);
  Result:=attr;
end;

class procedure TGtk2WSCustomRichMemo.ApplyTag(abuffer: PGtkTextBuffer;
  tag: PGtkTextTag; TextStart, TextLen: Integer; ToParagraphs: Boolean = False);
var
  istart : TGtkTextIter;
  iend   : TGtkTextIter;
begin
  gtk_text_buffer_get_iter_at_offset (abuffer, @istart, TextStart);
  gtk_text_buffer_get_iter_at_offset (abuffer, @iend, TextStart+TextLen);
  if ToParagraphs then begin
    gtk_text_iter_set_line_offset(@istart, 0);
    gtk_text_iter_forward_to_line_end(@iend);
  end;
  gtk_text_buffer_apply_tag(abuffer, tag, @istart, @iend);
end;

class function TGtk2WSCustomRichMemo.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
var
  Widget,
  TempWidget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
begin
  Widget := gtk_scrolled_window_new(nil, nil);
  Result := TLCLIntfHandle(PtrUInt(Widget));
  if Result = 0 then Exit;
  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(Widget,dbgsName(AWinControl));
  {$ENDIF}

  WidgetInfo := CreateWidgetInfo(Pointer(Result), AWinControl, AParams);

  TempWidget := gtk_text_view_new();
  gtk_container_add(PGtkContainer(Widget), TempWidget);

  GTK_WIDGET_UNSET_FLAGS(PGtkScrolledWindow(Widget)^.hscrollbar, GTK_CAN_FOCUS);
  GTK_WIDGET_UNSET_FLAGS(PGtkScrolledWindow(Widget)^.vscrollbar, GTK_CAN_FOCUS);
  gtk_scrolled_window_set_policy(PGtkScrolledWindow(Widget),
                                     GTK_POLICY_AUTOMATIC,
                                     GTK_POLICY_AUTOMATIC);
  // add border for memo
  gtk_scrolled_window_set_shadow_type(PGtkScrolledWindow(Widget),
    BorderStyleShadowMap[TCustomControl(AWinControl).BorderStyle]);

  SetMainWidget(Widget, TempWidget);
  GetWidgetInfo(Widget, True)^.CoreWidget := TempWidget;

  // gtk_text_buffer_set_text(gtk_text_view_get_buffer(PGtkTextView(TempWidget)), PChar(TCustomMemo(AWinControl).Text), -1);
  gtk_text_view_set_editable(PGtkTextView(TempWidget), True);
{  //gtk_text_view_set_editable(PGtkTextView(TempWidget), not TCustomMemo(AWinControl).ReadOnly);
  //gtk_text_view_set_justification(PGtkTextView(TempWidget), aGtkJustification[TCustomMemo(AWinControl).Alignment]);
  if TCustomMemo(AWinControl).WordWrap then
    gtk_text_view_set_wrap_mode(PGtkTextView(TempWidget), GTK_WRAP_WORD)
  else
    gtk_text_view_set_wrap_mode(PGtkTextView(TempWidget), GTK_WRAP_NONE);}
  gtk_text_view_set_wrap_mode(PGtkTextView(TempWidget), GTK_WRAP_WORD);

  //gtk_text_view_set_accepts_tab(PGtkTextView(TempWidget), TCustomMemo(AWinControl).WantTabs);
  gtk_text_view_set_accepts_tab(PGtkTextView(TempWidget), True);

  gtk_widget_show_all(Widget);

  Set_RC_Name(AWinControl, Widget);
  SetCallbacks(Widget, WidgetInfo);
end;

class procedure TGtk2WSCustomRichMemo.SetTextAttributes(const AWinControl: TWinControl; TextStart, TextLen: Integer; const Params: TIntFontParams);
var
  TextWidget: PGtkWidget;
  buffer  : PGtkTextBuffer;
  tag     : Pointer;
  gcolor  : TGdkColor;
  nm      : string;
const
  PangoUnderline : array [Boolean] of Integer = (PANGO_UNDERLINE_NONE, PANGO_UNDERLINE_SINGLE);
  PangoBold      : array [Boolean] of Integer = (PANGO_WEIGHT_NORMAL, PANGO_WEIGHT_BOLD);
  PangoItalic    : array [Boolean] of Integer = (PANGO_STYLE_NORMAL, PANGO_STYLE_ITALIC);
begin
  GetWidgetBuffer(AWinControl, TextWidget, buffer);
  if not Assigned(buffer) then Exit;

  gcolor := TColortoTGDKColor(Params.Color);
  nm := Params.Name;
  if nm = '' then nm := #0;
  tag := gtk_text_buffer_create_tag (buffer, nil,
      'family-set',     [gboolean(gTRUE),
      'family',         @nm[1],
      'foreground-gdk', @gcolor,
      'size-set',       gboolean(gTRUE),
      'size-points',    Double(Params.Size),
      'underline-set',  gboolean(gTRUE),
      'underline',      PangoUnderline[fsUnderline in Params.Style],
      'weight-set',     gboolean(gTRUE),
      'weight',         PangoBold[fsBold in Params.Style],
      'style-set',      gboolean(gTRUE),
      'style',          PangoItalic[fsItalic in Params.Style],
      'strikethrough-set', gboolean(gTRUE),
      'strikethrough',    gboolean(fsStrikeOut in Params.Style),
      nil]);
  ApplyTag(buffer, tag, TextStart, TextLen);

end;

class function TGtk2WSCustomRichMemo.GetParaAlignment(
  const AWinControl: TWinControl; TextStart: Integer; var AAlign: Integer
  ): Boolean;
var
  attr       : PGtkTextAttributes;
begin
  attr:=GetAttrAtPos(AWinControl, TextStart);
  Result := Assigned(attr);
  if Result then begin
    case attr^.justification of
      GTK_JUSTIFY_LEFT:   AAlign:=AL_LEFT;
      GTK_JUSTIFY_RIGHT:  AAlign:=AL_RIGHT;
      GTK_JUSTIFY_CENTER: AAlign:=AL_CENTER;
      GTK_JUSTIFY_FILL:   AAlign:=AL_JUSTIFY;
    else
      AAlign:=AL_LEFT;
    end;
    gtk_text_attributes_unref(attr);
  end;
end;

class procedure TGtk2WSCustomRichMemo.SetParaAlignment(
  const AWinControl: TWinControl; TextStart, TextLen: Integer;
  const AAlign: Integer);
var
  w      : PGtkWidget;
  buffer : PGtkTextBuffer;
  tag    : PGtkTextTag;
  val    : Integer;
begin
  val := GTK_JUSTIFY_LEFT;
  case AAlign of
    AL_RIGHT:   val:=GTK_JUSTIFY_RIGHT;
    AL_CENTER:  val:=GTK_JUSTIFY_CENTER;
    AL_JUSTIFY: val:=GTK_JUSTIFY_FILL;
  end;
  GetWidgetBuffer(AWinControl, w, buffer);
  tag := gtk_text_buffer_create_tag (buffer, nil,
      'justification', [  val,
      'justification-set', gboolean(gTRUE),
      nil]);
  ApplyTag(buffer, tag, TextStart, TextLen, true);
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
const
  ScreenDPI = 96; // todo: might change, should be received dynamically
  PageDPI   = 72; // not expected to be changed
  DPIFactor = ScreenDPI / PageDPI;
begin
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

  GetWidgetBuffer(AWinControl, w, buffer);
  tag := gtk_text_buffer_create_tag (buffer, nil,
      'pixels-above-lines',   [round(AMetric.SpaceBefore*DPIFactor) ,
      'pixels-above-lines-set', gboolean(gTRUE),
      'pixels-below-lines',   round(AMetric.SpaceAfter*DPIFactor),
      'pixels-below-lines-set', gboolean(gTRUE),
      'left-margin', round(h*DPIFactor),
      'left-margin-set', gboolean(gTRUE),
      'right-margin', round(AMetric.TailIndent*DPIFactor),
      'right-margin-set', gboolean(gTRUE),
      'indent', round(fl*DPIFactor),
      'indent-set', gboolean(gTRUE),
      nil]);
  ApplyTag(buffer, tag, TextStart, TextLen, true);

end;

class procedure TGtk2WSCustomRichMemo.InDelText(const AWinControl: TWinControl;
  const TextUTF8: String; DstStart, DstLen: Integer);
var
  w : PGtkWidget;
  b : PGtkTextBuffer;
  istart : TGtkTextIter;
  iend   : TGtkTextIter;
begin
  GetWidgetBuffer(AWinControl, w, b);
  if not Assigned(b) then Exit;
  gtk_text_buffer_get_iter_at_offset (b, @istart, DstStart);
  gtk_text_buffer_get_iter_at_offset (b, @iend, DstStart+DstLen);
  gtk_text_buffer_delete(b, @istart, @iend);
  if length(TextUTF8)>0 then
    gtk_text_buffer_insert(b, @istart, @textUTF8[1], length(TextUTF8));
end;


class function TGtk2WSCustomRichMemo.GetTextAttributes(const AWinControl: TWinControl; TextStart: Integer; var Params: TIntFontParams): Boolean;
var
  attr       : PGtkTextAttributes;
begin
  attr:=GetAttrAtPos(AWinControl, TextStart);
  Result := Assigned(attr);
  if Result then begin
    GtkTextAttrToFontParams(attr^, Params);
    gtk_text_attributes_unref(attr);
  end;
end;

end.

