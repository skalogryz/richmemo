unit RichMemoRTF;

interface

uses
  Classes, SysUtils, LCLProc, LCLIntf,
  RichMemo, RTFParsPre211, Graphics;

function MVCParserLoadStream(ARich: TCustomRichMemo; Source: TStream): Boolean;

implementation

type
  { TRTFMemoParser }

  TRTFMemoParser = class(TRTFParser)
  private
    txtbuf   : String; // keep it UTF8 encoded!

    fcolor    : TColor;      // Foreground color
    txtlen    : Integer;
    pm : TParaMetric;
    pa : TParaAlignment;
    fnum: Integer;
    fsz : double;
    fst : TFontStyles;
  protected
    procedure classUnk;
    procedure classText;
    procedure classControl;
    procedure classGroup;
    procedure classEof;
    procedure doChangePara(aminor, aparam: Integer);

    procedure doSpecialChar;
    procedure doChangeCharAttr(aminor, aparam: Integer);

    function DefaultTextColor: TColor;
    procedure PushText;
  public
    Memo  : TCustomRichMemo;
    constructor Create(AMemo: TCustomRichMemo; AStream: TStream);
    procedure StartReading;
  end;

{ TRTFMemoParserr }

procedure TRTFMemoParser.classUnk;
begin
  //writelN('unk: ', rtfMajor, ' ',rtfMinor,' ', rtfParam,' ', GetRtfText);
end;

procedure TRTFMemoParser.classText;
begin
  //writeln('txt:  ', rtfMajor, ' ',rtfMinor,' ', rtfParam,' ',Self.GetRtfText);
  case rtfMinor of
    rtfOptDest: {skipping option generator};
  else
    txtbuf:=txtbuf+Self.GetRtfText;
    txtlen:=length(txtbuf);

  end;
end;

procedure TRTFMemoParser.classControl;
begin
  if txtbuf<>'' then PushText;
  //writeln('ctrl: ', rtfClass,' ', rtfMajor, ' ', Self.GetRtfText, ' ',rtfMinor,' ', rtfParam);
  case rtfMajor of
    rtfSpecialChar: doSpecialChar;
    rtfCharAttr: doChangeCharAttr(rtfMinor, rtfParam);
    rtfParAttr: doChangePara(rtfMinor, rtfParam);
  end;
end;

procedure TRTFMemoParser.classGroup;
begin
  //writeln('group:  ', rtfMajor, ' ',rtfMinor,' ', rtfParam, ' ', GetRtfText);
end;

procedure TRTFMemoParser.classEof;
begin
  PushText;
end;

procedure TRTFMemoParser.doChangePara(aminor, aparam: Integer);
begin
  case aminor of
    rtfParDef:begin
      FillChar(pm, sizeof(pm), 0);
      pa:=paLeft;
    end;
    rtfQuadLeft:    pa:=paLeft;
    rtfQuadRight:   pa:=paRight;
    rtfQuadJust:    pa:=paJustify;
    rtfQuadCenter:  pa:=paCenter;
    rtfFirstIndent: begin
      pm.FirstLine:=aparam / 20;
      pm.FirstLine:=pm.FirstLine+pm.HeadIndent;
    end;
    rtfLeftIndent: begin
      pm.HeadIndent:=aparam / 20;
      pm.FirstLine:=pm.FirstLine+pm.HeadIndent;
    end;
    rtfRightIndent:  pm.TailIndent  := aparam / 20;
    rtfSpaceBefore:  pm.SpaceBefore := aparam / 20;
    rtfSpaceAfter:   pm.SpaceAfter  := aparam / 20;
    rtfSpaceBetween: pm.LineSpacing := aparam / 240;
  end;
end;

procedure TRTFMemoParser.doSpecialChar;
const
  {$ifdef MSWINDOWS}
  CharPara = #13#10;
  {$else}
  CharPara = #10;
  {$endif}
  CharTab  = #9;
  CharLine = #13;
begin
  case rtfMinor of
    rtfLine: txtbuf:=txtbuf+CharLine;
    rtfPar:  txtbuf:=txtbuf+CharPara;
    rtfTab:  txtbuf:=txtbuf+CharTab;
  end;
end;

procedure TRTFMemoParser.doChangeCharAttr(aminor, aparam: Integer);
var
  p : PRTFColor;
begin
  if txtbuf<>'' then PushText;

  case aminor of
    rtfPlain: fst:=[];
    rtfBold: if aparam=0 then Exclude(fst,fsBold)  else Include(fst, fsBold);
    rtfItalic: if aparam=0 then Exclude(fst,fsItalic)  else Include(fst, fsItalic);
    rtfStrikeThru: if aparam=0 then Exclude(fst,fsStrikeOut)  else Include(fst, fsStrikeOut);
    rtfFontNum: fnum:=aparam;
    rtfFontSize: fsz:=aparam/2;
    rtfUnderline: if aparam=0 then Exclude(fst,fsUnderline)  else Include(fst, fsUnderline);
    rtfNoUnderline: Exclude(fst, fsUnderline);
    rtfForeColor: begin
      if rtfParam<>0 then p:=Colors[rtfParam]
      else p:=nil;
      if not Assigned(p) then
        fcolor:=DefaultTextColor
      else
        fcolor:=RGBToColor(p^.rtfCRed, p^.rtfCGreen, p^.rtfCBlue);
    end;
  end;
end;

function TRTFMemoParser.DefaultTextColor:TColor;
begin
  Result:=ColorToRGB(Memo.Font.Color);
end;

procedure TRTFMemoParser.PushText;
var
  len   : Integer;
  font  : TFontParams;
  pf    : PRTFFONT;
  selst : Integer;
begin
  len:=UTF8Length(txtbuf);
  if len=0 then Exit;

  Memo.SelStart:=MaxInt;
  selst:=Memo.SelStart;
  // in order to get the start selection, we need to switch to the last character
  // and then get the value. SelStart doesn't match GetTextLen, since
  // "StartSel" is based on number of visible characters (i.e. line break is 1 character)
  // while GetTextLen is based on number of actual string characters
  // selst:=Memo.GetTextLen;

  Memo.SelStart:=selst;
  Memo.SelLength:=0;
  Memo.SelText:=txtbuf;

  Memo.SetParaMetric(selst, 1, pm);
  Memo.SetParaAlignment(selst, 1, pa);

  Memo.GetTextAttributes(selst, font);
  pf:=Fonts[fnum];
  if Assigned(pf) then
    font.Name:=pf^.rtfFName;
  font.Size:=round(fsz);
  font.Style:=fst;
  font.Color:=ColorToRGB(fColor);
  Memo.SetTextAttributes(selst, len, font);
  txtbuf:='';
end;

constructor TRTFMemoParser.Create(AMemo:TCustomRichMemo;AStream:TStream);
begin
  inherited Create(AStream);
  Memo:=AMemo;
  ClassCallBacks[rtfText]:=@classText;
  ClassCallBacks[rtfControl]:=@classControl;
  ClassCallBacks[rtfGroup]:=@classGroup;
  ClassCallBacks[rtfUnknown]:=@classUnk;
  ClassCallBacks[rtfEof]:=@classEof;
end;

procedure TRTFMemoParser.StartReading;
begin
  Memo.Lines.BeginUpdate;
  try
    fsz:=12;//\fsN Font size in half-points (the default is 24).
    fnum:=0;

    inherited StartReading;
    PushText;
    Memo.SelStart:=0;
    Memo.SelLength:=0;
  finally
    Memo.Lines.EndUpdate;
  end;
end;

function MVCParserLoadStream(ARich: TCustomRichMemo; Source: TStream): Boolean;
var
  p   : TRTFMemoParser;
begin
  Result:=Assigned(ARich) and Assigned(Source);
  if not Result then Exit;

  p:=TRTFMemoParser.Create(ARich, Source);
  try
    p.StartReading;
  finally
    p.Free;
  end;
  Result:=True;
end;

initialization
  RTFLoadStream:=@MVCParserLoadStream;

end.
