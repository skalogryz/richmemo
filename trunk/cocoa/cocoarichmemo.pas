unit CocoaRichMemo;

interface

{$mode delphi}
{$modeswitch objectivec1}
{$modeswitch objectivec2}

uses
  CocoaAll, Types, Classes, SysUtils,
  LCLType, Controls, StdCtrls,
  CocoaPrivate, CocoaUtils,
  CocoaWSCommon, CocoaWSStdCtrls,
  WSRichMemo, RichMemo;

type

  { TCocoaWSCustomRichMemo }

  TCocoaWSCustomRichMemo = class(TWSCustomRichMemo)
  public
    // assumption is made that LCL creates NSTextView
    class procedure SetParaAlignment(const AWinControl: TWinControl; TextStart, TextLen: Integer;
      const AAlign: TIntParaAlignment); override;
    class function GetParaAlignment(const AWinControl: TWinControl; TextStart: Integer;
      var AAlign: TIntParaAlignment): Boolean; override;

    class procedure SetParaTabs(const AWinControl: TWinControl; TextStart, TextLen: Integer;
      const AStopList: TTabStopList); override;
    class function GetParaTabs(const AWinControl: TWinControl; TextStart: integer;
      var AStopList: TTabStopList): Boolean; override;


    class procedure InDelText(const AWinControl: TWinControl;
      const TextUTF8: String; DstStart, DstLen: Integer); override;
    class function LoadRichText(const AWinControl: TWinControl; Source: TStream): Boolean; override;
  end;

implementation

function MemoTextView(AWinControl: TWinControl): TCocoaTextView;
begin
  if not Assigned(AWinControl) or (AWinControl.Handle=0) then
    Result := nil
  else
    Result := TCocoaTextView(NSScrollView(AWinControl.Handle).documentView);
end;

function ParaRange(txt: NSString; textOffset, TextLen: Integer): NSRange; overload;
begin
  Result.location:=textOffset;
  if textOffset+TextLen>txt.length then
    Result.length:=txt.length-textOffset
  else
    Result.length:=TextLen;
  Result:=txt.paragraphRangeForRange(Result);
end;

function ParaRange(txt: NSTextStorage; textOffset, TextLen: Integer): NSRange; inline; overload;
begin
  Result:=ParaRange(txt.string_, textOffset, textLen);
end;

{ TCocoaWSCustomRichMemo }

class procedure TCocoaWSCustomRichMemo.SetParaAlignment(
  const AWinControl: TWinControl; TextStart, TextLen: Integer;
  const AAlign: TIntParaAlignment);
var
  txt : TCocoaTextView;
  rng : NSRange;
const
  TxtAlign : array [TIntParaAlignment] of integer = (
   NSLeftTextAlignment,  NSRightTextAlignment, NSCenterTextAlignment, NSJustifiedTextAlignment
  );
begin
  txt:=MemoTextView(AWinControl);
  if not Assigned(txt) then Exit;

  rng.location:=TextStart;
  rng.length:=TextLen;
  if TextStart+TextLen>txt.textStorage.string_.length then
    rng.length:=txt.textStorage.string_.length-TextStart;

  rng:=txt.textStorage.string_.paragraphRangeForRange(rng);
  txt.setAlignment_range(TxtAlign[AAlign], rng);
end;

class function TCocoaWSCustomRichMemo.GetParaAlignment(
  const AWinControl: TWinControl; TextStart: Integer;
  var AAlign: TIntParaAlignment): Boolean;
var
  txt : TCocoaTextView;
  rng : NSRange;
  cur : NSRange;
  al  : NSTextAlignment;
const
  TxtAlign : array [TIntParaAlignment] of integer = (
   NSLeftTextAlignment,  NSRightTextAlignment, NSCenterTextAlignment, NSJustifiedTextAlignment
  );
begin
  txt:=MemoTextView(AWinControl);
  if not Assigned(txt) then begin
    Result:=false;
    Exit;
  end;

  cur:=txt.selectedRange;
  rng.location:=TextStart;
  rng.length:=1;
  if TextStart+1>txt.textStorage.string_.length then
    rng.length:=txt.textStorage.string_.length-TextStart;


  rng:=txt.textStorage.string_.paragraphRangeForRange(rng);
  txt.setSelectedRange(rng);
  al:=txt.alignment;
  case al of
    NSRightTextAlignment: AAlign:=paRight;
    NSCenterTextAlignment: AAlign:=paCenter;
    NSJustifiedTextAlignment: AAlign:=paJustify;
  else
    AAlign:=paLeft;
  end;
  txt.setSelectedRange(cur);
  Result:=true;
end;

class procedure TCocoaWSCustomRichMemo.SetParaTabs(
  const AWinControl: TWinControl; TextStart, TextLen: Integer;
  const AStopList: TTabStopList);
var
  view  : TCocoaTextView;
  txt   : NSTextStorage;
  par   : NSMutableParagraphStyle;
  tabs  : NSMutableArray;
  tab   : NSTextTab;
  dict  : NSDictionary;
  i     : Integer;
  rng   : NSRange;
const
  TabAlignMap : array [TTabAlignment] of NSTextTabType = (
    NSLeftTabStopType,    // taLeft,
    NSCenterTabStopType,  // taCenter,
    NSRightTabStopType,   // taRight,
    NSDecimalTabStopType, // taDecimal
    NSLeftTabStopType     // taWordBar - not supported
  );
begin
  view:=MemoTextView(AWinControl);
  if not Assigned(view) then Exit;

  par := NSParagraphStyle.defaultParagraphStyle.mutableCopyWithZone(nil);
  if AStopList.Count>0 then begin
    tabs := NSMutableArray.alloc.init;
    for i:=0 to AStopList.Count-1 do begin
      tab := NSTextTab.alloc.initWithType_location( TabAlignMap[AStopList.Tabs[i].Align], AStopList.Tabs[i].Offset );
      tabs.addObject( tab );
      tab.release;
    end;
  end;
  par.setTabStops(tabs);
  txt := view.textStorage;
  txt.addAttribute_value_range( NSParagraphStyleAttributeName, par, ParaRange(txt, TextStart, textLen));
  par.release;
end;

class function TCocoaWSCustomRichMemo.GetParaTabs(
  const AWinControl: TWinControl; TextStart: integer;
  var AStopList: TTabStopList): Boolean;
var
  view  : TCocoaTextView;
  txt   : NSTextStorage;
  par   : NSParagraphStyle;
  tabs  : NSArray;
  dict  : NSDictionary;
  tab   : NSTextTab;
  i     : Integer;
begin
  InitTabStopList(AStopList);
  view:=MemoTextView(AWinControl);
  Result:=false;
  if not Assigned(view) then Exit;

  txt:=view.textStorage;
  dict := txt.attributesAtIndex_effectiveRange(textStart, nil);
  if not Assigned(dict) then Exit;

  par:=NSParagraphStyle(  dict.objectForKey(NSParagraphStyleAttributeName) );
  if not Assigned(par) then
    par:=NSParagraphStyle.defaultParagraphStyle;

  tabs:=par.tabStops;
  if not Assigned(par) then Exit;
  AStopList.Count:=tabs.count;
  SetLength(AStopList.Tabs, AStopList.Count);
  for i:=0 to tabs.count-1 do begin
    tab:=NSTextTab(tabs.objectAtIndex(i));
    AStopList.Tabs[i].Offset:=tab.location;
    case tab.tabStopType of
      NSCenterTabStopType: AStopList.Tabs[i].Align:= taCenter;
      NSRightTabStopType:  AStopList.Tabs[i].Align:= taRight;
      NSDecimalTabStopType: AStopList.Tabs[i].Align:= taDecimal;
    else
      AStopList.Tabs[i].Align:=taLeft;
    end;
  end;
end;

class procedure TCocoaWSCustomRichMemo.InDelText(
  const AWinControl: TWinControl; const TextUTF8: String; DstStart,
  DstLen: Integer);
var
  txt : TCocoaTextView;
  str : NSString;
begin
  txt:=MemoTextView(AWinControl);
  if not Assigned(txt) then Exit;

  str := NSStringUtf8(TextUtf8);
  txt.textStorage.replaceCharactersInRange_withString(NSMakeRange(DstStart, DstLen), str);
  str.release;
end;

class function TCocoaWSCustomRichMemo.LoadRichText(
  const AWinControl: TWinControl; Source: TStream): Boolean;
var
  data: NSMutableData;
  rng : NSRange;
  txt : TCocoaTextView;
begin
  //todo: avoid copying data.
  Result:=false;
  if not Assigned(Source) or not Assigned(AWinControl) or (AWinControl.Handle=0) then Exit;

  txt:=MemoTextView(AWinControl);
  if Source.size>0 then begin
    data:=NSMutableData(NSMutableData.alloc).initWithLength(Source.size);
    Source.Read(data.mutableBytes^, Source.Size);
    rng.length:=txt.textStorage.string_.length;
    rng.location:=0;
    txt.replaceCharactersInRange_withRTF(rng, data);
    data.release;
  end;
  Result:=true;
end;

end.
