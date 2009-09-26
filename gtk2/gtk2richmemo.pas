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
  GtkDef,
  GTKWinApiWindow, GtkGlobals, GtkProc, InterfaceBase,
  GtkWSControls,
  // RichMemo
  WSRichMemo;

  { TGtk2WSCustomRichMemo }
type
  TGtk2WSCustomRichMemo = class(TWSCustomRichMemo)
  protected
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo);
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure SetTextAttributes(const AWinControl: TWinControl; TextStart, TextLen: Integer; const Params: TIntFontParams); override;
  end;

implementation

class procedure TGtk2WSCustomRichMemo.SetCallbacks(
  const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo);
begin
   TGtkWSWinControl.SetCallbacks(PGtkObject(AGtkWidget), TComponent(AWidgetInfo^.LCLObject));
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
  Widget, TextWidget: PGtkWidget;
  list    : PGList;
  buffer  : PGtkTextBuffer;
  tag     : Pointer;
  istart  : TGtkTextIter;
  iend    : TGtkTextIter;
  gcolor  : TGdkColor;
  nm      : string;
const
  PangoUnderline : array [Boolean] of Integer = (PANGO_UNDERLINE_NONE, PANGO_UNDERLINE_SINGLE);
  PangoBold      : array [Boolean] of Integer = (PANGO_WEIGHT_NORMAL, PANGO_WEIGHT_BOLD);
  PangoItalic    : array [Boolean] of Integer = (PANGO_STYLE_NORMAL, PANGO_STYLE_ITALIC);
begin
  Widget := PGtkWidget(PtrUInt(AWinControl.Handle));

  list := gtk_container_get_children(PGtkContainer(Widget));
  if not Assigned(list) then Exit;

  TextWidget := PGtkWidget(list^.data);
  if not Assigned(TextWidget) then Exit;

  buffer := gtk_text_view_get_buffer (PGtkTextView(TextWidget));
  if not Assigned(buffer) then Exit;

  gcolor := TColortoTGDKColor(Params.Color);
  nm := Params.Name;
  if nm = '' then nm := #0;

  tag := gtk_text_buffer_create_tag (buffer, nil,
      'foreground-gdk', [@gcolor,
      'size-set',       gboolean(true),
      'size-points',    Double(Params.Size),
      'underline-set',  gboolean(True),
      'underline',      PangoUnderline[fsUnderline in Params.Style],
      'weight-set',     gboolean(True),
      'weight',         PangoBold[fsBold in Params.Style],
      'style-set',      gboolean(True),
      'style',          PangoItalic[fsItalic in Params.Style],
      'strikethrough-set', gboolean(True),
      'strikethrough ',    gboolean(fsStrikeOut in Params.Style),
      'familty-set',    gboolean(true),
      'family',         @nm[1],
      nil]);

  gtk_text_buffer_get_iter_at_offset (buffer, @istart, TextStart);
  gtk_text_buffer_get_iter_at_offset (buffer, @iend, TextStart+TextLen);
  gtk_text_buffer_apply_tag(buffer, tag, @istart, @iend);
end;

end.

