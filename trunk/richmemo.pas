unit RichMemo; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, StdCtrls, 
  RichMemoTypes, WSRichMemo; 

type
  { TCustomRichMemo }

  TCustomRichMemo = class(TCustomMemo)
  private
    fHideSelection  : Boolean;
  protected
    class procedure WSRegisterClass; override;
    procedure CreateWnd; override;    
    procedure UpdateRichMemo; virtual;
    procedure SetHideSelection(AValue: Boolean);
  public
    procedure SetTextAttributes(TextStart, TextLen: Integer; AFont: TFont); 
    procedure SetTextAttributes(TextStart, TextLen: Integer; SetMask: TTextStyleMask; const TextParams: TFontParams); virtual; 
    function GetTextAttributes(TextStart: Integer; var TextParams: TFontParams): Boolean; virtual;
    property HideSelection : Boolean read fHideSelection write SetHideSelection;
  end;
  
  TRichMemo = class(TCustomRichMemo)
  published
    property Align;
    property Alignment;
    property Anchors;
    property BidiMode;
    property BorderSpacing;
    property BorderStyle;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property Lines;
    property MaxLength;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditingDone;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
    property PopupMenu;
    property ParentShowHint;
    property ReadOnly;
    property ScrollBars;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property WantReturns;
    property WantTabs;
    property WordWrap;
  end;
  
const
  TextStyleAll : TTextStyleMask = [tsm_Color, tsm_Name, tsm_Size, tsm_Styles];
  
function GetFontParams(styles: TFontStyles): TFontParams; overload;
function GetFontParams(color: TColor; styles: TFontStyles): TFontParams; overload;
function GetFontParams(const Name: String; color: TColor; styles: TFontStyles): TFontParams; overload;
function GetFontParams(const Name: String; Size: Integer; color: TColor; styles: TFontStyles): TFontParams; overload;
  
implementation

function GetFontParams(styles: TFontStyles): TFontParams; overload;
begin 
  Result := GetFontParams('', 0, 0, styles);
end;

function GetFontParams(color: TColor; styles: TFontStyles): TFontParams; overload;
begin
  Result := GetFontParams('', 0, color, styles);
end;

function GetFontParams(const Name: String; color: TColor; styles: TFontStyles): TFontParams; overload;
begin
  Result := GetFontParams(Name, 0, color, styles);
end;

function GetFontParams(const Name: String; Size: Integer; color: TColor; styles: TFontStyles): TFontParams; overload;
begin
  Result.Name := Name;
  Result.Size := Size;
  Result.Color := color;
  Result.Style := styles;
end;

{ TCustomRichMemo }

procedure TCustomRichMemo.SetHideSelection(AValue: Boolean);
begin
  if HandleAllocated then 
    TWSCustomRichMemoClass(WidgetSetClass).SetHideSelection(Self, AValue);
  fHideSelection := AValue;   
end;

class procedure TCustomRichMemo.WSRegisterClass;  
begin
  inherited;
  WSRegisterCustomRichMemo;
end;

procedure TCustomRichMemo.CreateWnd;  
begin
  inherited CreateWnd;  
  UpdateRichMemo;
end;

procedure TCustomRichMemo.UpdateRichMemo; 
begin
  if not HandleAllocated then Exit;
  TWSCustomRichMemoClass(WidgetSetClass).SetHideSelection(Self, fHideSelection);
end;

procedure TCustomRichMemo.SetTextAttributes(TextStart, TextLen: Integer;  
  AFont: TFont); 
var
  params  : TFontParams;
begin
  params.Name := AFont.Name;
  params.Color := AFont.Color;
  params.Size := AFont.Size;
  params.Style := AFont.Style;
  SetTextAttributes(TextStart, TextLen, TextStyleAll, params);
end;

procedure TCustomRichMemo.SetTextAttributes(TextStart, TextLen: Integer;  
  SetMask: TTextStyleMask; const TextParams: TFontParams); 
begin
  if HandleAllocated then  
    TWSCustomRichMemoClass(WidgetSetClass).SetTextAttributes(Self, TextStart, TextLen, SetMask, TextParams);
end;

function TCustomRichMemo.GetTextAttributes(TextStart: Integer; var TextParams: TFontParams): Boolean; 
begin
  if HandleAllocated then  
    Result := TWSCustomRichMemoClass(WidgetSetClass).GetTextAttributes(Self, TextStart, TextParams)
  else
    Result := false;
end;


end.

