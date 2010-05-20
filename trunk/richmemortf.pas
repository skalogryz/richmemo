unit RichMemoRTF;

interface

uses
  Classes, SysUtils, LCLProc, LCLIntf,
  RichMemo, RTFParsPre211;

//todo: formatting support!

function MVCParserLoadStream(ARich: TCustomRichMemo; Source: TStream): Boolean;

implementation

type
  { TRTFMemoParser }

  TRTFMemoParser = class(TRTFParser)
  protected
    procedure classText;
    procedure classControl;

    procedure doSpecialChar;
  public
    txt   : String;
    Memo  : TCustomRichMemo;
    constructor Create(AMemo: TCustomRichMemo; AStream: TStream);
  end;


{ TRTFMemoParserr }
procedure TRTFMemoParser.classText;
begin
  //writeln('txt:  ', rtfMajor, ' ',rtfMinor,' ', rtfParam);
  case rtfMinor of
    rtfOptDest:;
  else
    txt:=txt+Self.GetRtfText;
  end;
end;

procedure TRTFMemoParser.classControl;
begin
  //writeln('ctrl: ', rtfClass,' ', rtfMajor, ' ', Self.GetRtfText, ' ',rtfMinor,' ', rtfParam);
  case rtfMajor of
    rtfSpecialChar: doSpecialChar;
  end;
end;

procedure TRTFMemoParser.doSpecialChar;
const
  CharPara = #10;
  CharTab  = #9;
  CharLine = #13;
begin
  case rtfMinor of
    rtfLine: txt:=txt+CharLine;
    rtfPar:  txt:=txt+CharPara;
    rtfTab:  txt:=txt+CharTab;
  end;
end;

constructor TRTFMemoParser.Create(AMemo:TCustomRichMemo;AStream:TStream);
begin
  inherited Create(AStream);
  Memo:=AMemo;
  ClassCallBacks[rtfText]:=@classText;
  ClassCallBacks[rtfControl]:=@classControl;
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
    ARich.SelStart:=0;
    ARich.SelLength:=0;
    ARich.Text:=p.txt;
  finally
    p.Free;
  end;
  Result:=True;
end;

initialization
  RTFLoadStream:=@MVCParserLoadStream;


end.
