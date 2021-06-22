unit RichMemoRTF;

interface

{$mode objfpc}{$h+}

uses
  Classes, SysUtils, LCLProc, LCLIntf, LConvEncoding, Graphics,
  RichMemo, RTFParsPre211;

function MVCParserLoadStream(ARich: TCustomRichMemo; Source: TStream; fastLoad:boolean=false): Boolean;
procedure RegisterRTFLoader;

type
  TEncConvProc = function (const s: string): string;

//todo: rewrite! it's not language based but fontchar-set based
procedure LangConvAdd(lang: Integer; convproc: TEncConvProc);
function LangConvGet(lang: Integer; var convproc: TEncConvProc): Boolean;

type
  TSaveParams = record // reserved
    start  : Integer; // the first character for the extract
    len    : Integer; // the number of characters to extract
  end;

// the function depends on GetStyleRange and to be implemented properly
// if GetStyleRange, GetParaMetric, GetParaAlignment is not working properly
// the resulting RTF would not contain any styles or the text styles would be wrong
procedure IntSaveStream(ARich: TcustomRichMemo; SaveParams: TSaveParams; Dst: TStream);
function SaveStream(ARich: TCustomRichMemo; Dst: TStream): Boolean;
procedure RegisterRTFSaver;

implementation
uses LazUTF8;

type

  { TRTFParams }

  TRTFParams = class(TObject)
  public
    fnt  : TFontParams;
    pm   : TParaMetric;
    pa   : TParaAlignment;
    fnum : Integer; // font index in the font table
    pn   : TParaNumbering;

    prev : TRTFParams;
    tabs : TTabStopList;
    constructor Create(aprev: TRTFParams);
    procedure ResetDefault;
    procedure AddTab(AOffset: double; ta: TTabAlignment);
    function  Equal(prm: TRTFParams): boolean;
  end;

  TParaNumberingType = (pntNone, pntNumeric, pntBullet);
  TParaNumberingStyle = ( pnsCard, pnsDec, pnsUcltr, pnsUcrm, pnsLcltr, pnsLcrm,
                          pnsOrd, pnsOrdt, pnsBidia, pnsBidib, pnsAiu, pnsAiud,
                          pnsAiueo, pnsAiueod, pnsChosung, pnsCnum, pnsDbnum,
                          pnsDbnumd, pnsDbnumk, pnsDbnuml, pnsDbnumt, pnsDecd,
                          pnsGanada, pnsGbnum, pnsGbnumd, pnsGbnumk, pnsGbnuml,
                          pnsIroha, pnsIrohad, pnsZodiac, pnsZodiacd, pnsZodiacl );
  TParaNumberingUnderlineStyle = (
                          pnulNone, pnulSimple, pnulThick, pnulHair, pnulDouble, pnulWave,
                          pnulWord, pnulDot, pnulDash, pnulDashDot, pnulDashDotDash );
  TParaNumberingJust = (pnjl, pnjc, pnjr);

  TRTFInteger = record
    modified: boolean;
    Value: Integer;
  end;

  TRTFBoolean = record
    modified: boolean;
    value: Boolean;
  end;

  TRTFParaNumbering = record
    valid: boolean;
    level: Integer;
    aType: TParaNumberingType;
    style: TParaNumberingStyle;
    just: TParaNumberingJust;
    underlineStyle: TParaNumberingUnderlineStyle;
    hang: boolean;
    aIndex: Integer;
    BeforeText: string;
    AfterText: string;
    skipNumbering: boolean;
    indent: TRTFInteger;
    fontNum: TRTFInteger;
    fontSize: TRTFInteger;
    fontColor: TRTFInteger;
    bold: TRTFBoolean;
    italic: TRTFBoolean;
    caps: TRTFBoolean;
    smallcaps: TRTFBoolean;
    strike: TRTFBoolean;
  end;

  TChunkType = (ctEmpty, ctText, ctPnStart, ctPnItem, ctPnEnd);

  TChunk = record
    typ: TChunkType;
    prm: TRTFParams;
    Text: string;
    Link: string;
  end;

  TRTFChunkArray = array of TChunk;

  { TRTFMemoParser }

  TRTFMemoParser = class(TRTFParser)
  private
    defColor : TColor;
    txtbuf   : String; // keep it UTF8 encoded!
    txtlen    : Integer;

    HLFromCTable : Boolean;

    prm       : TRTFParams;
    lang      : Integer;
    langproc  : TEncConvProc;
    deflang   : integer;

    skipNextCh: Boolean; // For a Unicode escape the control word \u is used,
                         // followed by a 16-bit signed decimal integer giving
                         // the Unicode UTF-16 code unit number. For the benefit
                         // of programs without Unicode support, this must be followed
                         // by the nearest representation of this character in the specified code page.
                         // For example, \u1576? would give the Arabic letter bāʼ ب, specifying that
                         // older programs which do not have Unicode support should render it as a
                         // question mark instead.
    procedure AddText(const atext: string);
  protected
    procedure classUnk;
    procedure classText;
    procedure classControl;
    procedure classGroup;
    procedure classEof;
    procedure destinationPn;
    procedure destinationField;
    procedure doChangePara(aminor, aparam: Integer);

    procedure doDestination(aminor, aparam: Integer);
    procedure doSpecialChar;
    procedure doChangeCharAttr(aminor, aparam: Integer);
    procedure SetLanguage(AlangCode: integer);

    function DefaultTextColor: TColor;
    procedure PushText;
  private
    fFastLoad: boolean;
    fLastParaNumStyle: TParaNumStyle;
    paraNum   : TRTFParaNumbering;
    pnListStarted, pnItemStarted: boolean;
    chunks: TRTFChunkArray;
    Field     : TRTFField;
    procedure LoadFromChunks;
    procedure Consolidate;
    {$IFDEF DEBUG}
    procedure DumpChunks(msg:string);
    {$ENDIF}
    function  GetHyperlink(out alinkref: string): boolean;
    function  GetParaNumbering: TParaNumbering;
    function  NewChunk: Integer;
    procedure ParaNumberingStart(newList:boolean);
  protected
    procedure doParagraph; virtual;
    procedure doPnItemStart(newList:boolean);
    procedure doPnListEnded;
  public
    Memo  : TCustomRichMemo;
    constructor Create(AMemo: TCustomRichMemo; AStream: TStream; withFastLoad:boolean=false);
    destructor Destroy; override;

    procedure StartReading; override;
  end;

var
  LangConvTable : array of record lang: integer; proc: TEncConvProc end;
  LangCount     : Integer = 0;

procedure LangConvAdd(lang: Integer; convproc: TEncConvProc);
var
  i  : integer;
begin
  for i:=0 to LangCount-1 do
    if LangConvTable[i].lang=lang then begin
      LangConvTable[i].proc:=convproc;
      Exit;
    end;
  if LangCount=length(LangConvTable) then begin
    if LangCount=0 then SetLength(LangConvTable, 64)
    else SetLength(LangConvTable, LangCount*2);
  end;
  LangConvTable[LangCount].lang:=lang;
  LangConvTable[LangCount].proc:=convproc;
  inc(LangCount);
end;

function LangConvGet(lang: Integer; var convproc: TEncConvProc): Boolean;
var
  i  : integer;
begin
  for i:=0 to LangCount-1 do
    if LangConvTable[i].lang=lang then begin
      convproc:=LangConvTable[i].proc;
      Result:=true;
      Exit;
    end;
  Result:=false;
end;

procedure LangConvInit;
begin

  if LangCount>0 then
    exit;

  LangConvAdd(1052, @CP1250ToUTF8); // Albanian
  LangConvAdd(1050, @CP1250ToUTF8); // Croatian
  LangConvAdd(1029, @CP1250ToUTF8); // Czech
  LangConvAdd(1038, @CP1250ToUTF8); // Hungarian
  LangConvAdd(1045, @CP1250ToUTF8); // Polish
  LangConvAdd(1048, @CP1250ToUTF8); // Romanian
  LangConvAdd(2074, @CP1250ToUTF8); // Serbian - Latin
  LangConvAdd(1051, @CP1250ToUTF8); // Slovak
  LangConvAdd(1060, @CP1250ToUTF8); // Slovenian

  LangConvAdd(2092, @CP1251ToUTF8); // Azeri - Cyrillic
  LangConvAdd(1059, @CP1251ToUTF8); // Belarusian
  LangConvAdd(1026, @CP1251ToUTF8); // Bulgarian
  LangConvAdd(1071, @CP1251ToUTF8); // FYRO Macedonia
  LangConvAdd(1087, @CP1251ToUTF8); // Kazakh
  LangConvAdd(1088, @CP1251ToUTF8); // Kyrgyz - Cyrillic
  LangConvAdd(1104, @CP1251ToUTF8); // Mongolian
  LangConvAdd(1049, @CP1251ToUTF8); // Russian
  LangConvAdd(3098, @CP1251ToUTF8); // Serbian - Cyrillic
  LangConvAdd(1092, @CP1251ToUTF8); // Tatar
  LangConvAdd(1058, @CP1251ToUTF8); // Ukrainian
  LangConvAdd(2115, @CP1251ToUTF8); // Uzbek - Cyrillic

  LangConvAdd(1078, @CP1252ToUTF8); // Afrikaans
  LangConvAdd(1069, @CP1252ToUTF8); // Basque
  LangConvAdd(1027, @CP1252ToUTF8); // Catalan
  LangConvAdd(1030, @CP1252ToUTF8); // Danish
  LangConvAdd(2067, @CP1252ToUTF8); // Dutch - Belgium
  LangConvAdd(1043, @CP1252ToUTF8); // Dutch - Netherlands
  LangConvAdd(3081, @CP1252ToUTF8); // English - Australia
  LangConvAdd(10249,@CP1252ToUTF8); // English - Belize
  LangConvAdd(4105, @CP1252ToUTF8); // English - Canada
  LangConvAdd(9225, @CP1252ToUTF8); // English - Caribbean
  LangConvAdd(2057, @CP1252ToUTF8); // English - Great Britain
  LangConvAdd(6153, @CP1252ToUTF8); // English - Ireland
  LangConvAdd(8201, @CP1252ToUTF8); // English - Jamaica
  LangConvAdd(5129, @CP1252ToUTF8); // English - New Zealand
  LangConvAdd(13321,@CP1252ToUTF8); // English - Phillippines
  LangConvAdd(7177, @CP1252ToUTF8); // English - Southern Africa
  LangConvAdd(11273,@CP1252ToUTF8); // English - Trinidad
  LangConvAdd(1033, @CP1252ToUTF8); // English - United States
  LangConvAdd(12297,@CP1252ToUTF8); // English - Zimbabwe
  LangConvAdd(1080, @CP1252ToUTF8); // Faroese
  LangConvAdd(1035, @CP1252ToUTF8); // Finnish
  LangConvAdd(2060, @CP1252ToUTF8); // French - Belgium
  LangConvAdd(3084, @CP1252ToUTF8); // French - Canada
  LangConvAdd(1036, @CP1252ToUTF8); // French - France
  LangConvAdd(5132, @CP1252ToUTF8); // French - Luxembourg
  LangConvAdd(6156, @CP1252ToUTF8); // French - Monaco
  LangConvAdd(4108, @CP1252ToUTF8); // French - Switzerland
  LangConvAdd(1110, @CP1252ToUTF8); // Galician
  LangConvAdd(3079, @CP1252ToUTF8); // German - Austria
  LangConvAdd(1031, @CP1252ToUTF8); // German - Germany
  LangConvAdd(5127, @CP1252ToUTF8); // German - Liechtenstein
  LangConvAdd(4103, @CP1252ToUTF8); // German - Luxembourg
  LangConvAdd(2055, @CP1252ToUTF8); // German - Switzerland
  LangConvAdd(1039, @CP1252ToUTF8); // Icelandic
  LangConvAdd(1057, @CP1252ToUTF8); // Indonesian
  LangConvAdd(1040, @CP1252ToUTF8); // Italian - Italy
  LangConvAdd(2064, @CP1252ToUTF8); // Italian - Switzerland
  LangConvAdd(2110, @CP1252ToUTF8); // Malay - Brunei
  LangConvAdd(1086, @CP1252ToUTF8); // Malay - Malaysia
  LangConvAdd(1044, @CP1252ToUTF8); // Norwegian - Bokml
  LangConvAdd(2068, @CP1252ToUTF8); // Norwegian - Nynorsk
  LangConvAdd(1046, @CP1252ToUTF8); // Portuguese - Brazil
  LangConvAdd(2070, @CP1252ToUTF8); // Portuguese - Portugal
  LangConvAdd(1274, @CP1252ToUTF8); // Spanish - Argentina
  LangConvAdd(16394,@CP1252ToUTF8); // Spanish - Bolivia
  LangConvAdd(13322,@CP1252ToUTF8); // Spanish - Chile
  LangConvAdd(9226, @CP1252ToUTF8); // Spanish - Colombia
  LangConvAdd(5130, @CP1252ToUTF8); // Spanish - Costa Rica
  LangConvAdd(7178, @CP1252ToUTF8); // Spanish - Dominican Republic
  LangConvAdd(12298,@CP1252ToUTF8); // Spanish - Ecuador
  LangConvAdd(17418,@CP1252ToUTF8); // Spanish - El Salvador
  LangConvAdd(4106, @CP1252ToUTF8); // Spanish - Guatemala
  LangConvAdd(18442,@CP1252ToUTF8); // Spanish - Honduras
  LangConvAdd(2058, @CP1252ToUTF8); // Spanish - Mexico
  LangConvAdd(19466,@CP1252ToUTF8); // Spanish - Nicaragua
  LangConvAdd(6154, @CP1252ToUTF8); // Spanish - Panama
  LangConvAdd(15370,@CP1252ToUTF8); // Spanish - Paraguay
  LangConvAdd(10250,@CP1252ToUTF8); // Spanish - Peru
  LangConvAdd(20490,@CP1252ToUTF8); // Spanish - Puerto Rico
  LangConvAdd(1034, @CP1252ToUTF8); // Spanish - Spain (Traditional)
  LangConvAdd(14346,@CP1252ToUTF8); // Spanish - Uruguay
  LangConvAdd(8202, @CP1252ToUTF8); // Spanish - Venezuela
  LangConvAdd(1089, @CP1252ToUTF8); // Swahili
  LangConvAdd(2077, @CP1252ToUTF8); // Swedish - Finland
  LangConvAdd(1053, @CP1252ToUTF8); // Swedish - Sweden

  LangConvAdd(1032, @CP1253ToUTF8); // greek

  LangConvAdd(1068, @CP1254ToUTF8); // Azeri - Latin
  LangConvAdd(1055, @CP1254ToUTF8); // turkish
  LangConvAdd(1091, @CP1254ToUTF8); // Uzbek - Latin

  LangConvAdd(1037, @CP1255ToUTF8); // hebrew

  LangConvAdd(5121, @CP1256ToUTF8); // Arabic - Algeria
  LangConvAdd(15361,@CP1256ToUTF8); // Arabic - Bahrain
  LangConvAdd(3073, @CP1256ToUTF8); // Arabic - Egypt
  LangConvAdd(2049, @CP1256ToUTF8); // Arabic - Iraq
  LangConvAdd(11265,@CP1256ToUTF8); // Arabic - Jordan
  LangConvAdd(13313,@CP1256ToUTF8); // Arabic - Kuwait
  LangConvAdd(12289,@CP1256ToUTF8); // Arabic - Lebanon
  LangConvAdd(4097, @CP1256ToUTF8); // Arabic - Libya
  LangConvAdd(6145, @CP1256ToUTF8); // Arabic - Morocco
  LangConvAdd(8193, @CP1256ToUTF8); // Arabic - Oman
  LangConvAdd(16385,@CP1256ToUTF8); // Arabic - Qatar
  LangConvAdd(1025, @CP1256ToUTF8); // Arabic - Saudi Arabia
  LangConvAdd(10241,@CP1256ToUTF8); // Arabic - Syria
  LangConvAdd(7169, @CP1256ToUTF8); // Arabic - Tunisia
  LangConvAdd(14337,@CP1256ToUTF8); // Arabic - United Arab Emirates
  LangConvAdd(9217, @CP1256ToUTF8); // Arabic - Yemen
  LangConvAdd(1065, @CP1256ToUTF8); // Farsi - Persian
  LangConvAdd(1056, @CP1256ToUTF8); // Urdu

  LangConvAdd(1061, @CP1257ToUTF8); // Estonian
  LangConvAdd(1062, @CP1257ToUTF8); // Latvian
  LangConvAdd(1063, @CP1257ToUTF8); // Lithuanian

  LangConvAdd(1066, @CP1258ToUTF8); // vietnam
end;

function CharToByte(const ch: AnsiChar): Byte;
begin
  Result:=0;
  if ch in ['0'..'9'] then Result:=byte(ch)-byte('0')
  else if ch in ['a'..'f'] then Result:=byte(ch)-byte('a')+10
  else if ch in ['A'..'F'] then Result:=byte(ch)-byte('A')+10
end;

function RTFCharToByte(const s: string): byte; inline;
begin
  // \'hh 	A hexadecimal value, based on the specified character set (may be used to identify 8-bit values).
  Result:=(CharToByte(s[3]) shl 4) or (CharToByte(s[4]));
end;

operator := (right: Integer) res: TRTFInteger;
begin
  res.modified := true;
  res.value := right;
end;

operator := (right: boolean) res: TRTFBoolean;
begin
  res.modified := true;
  res.value := right;
end;

constructor TRTFParams.Create(aprev: TRTFParams);
begin
  prev:=aprev;
  if Assigned(prev) then begin
    fnt:=prev.fnt;
    pm:=prev.pm;
    pa:=prev.pa;
    fnum:=prev.fnum;
  end else begin
    FillChar(fnt, SizeOf(fnt), 0);
    FillChar(pm, sizeof(pm), 0);
    pm.LineSpacing:=DefLineSpacing;
  end;
end;

procedure TRTFParams.ResetDefault;
begin
  // default values are taken from RTF specs
  // see section "Paragraph Formatting Properties"
  pa:=paLeft;
  pm.FirstLine:=0;
  pm.HeadIndent:=0;
  pm.TailIndent:=0;
  pm.SpaceBefore:=0;
  pm.SpaceAfter:=0;
  pm.LineSpacing:=0;
  tabs.Count:=0;
  pn.Style:=pnNone;
end;

procedure TRTFParams.AddTab(AOffset: double; ta: TTabAlignment);
begin
  if tabs.Count=length(tabs.Tabs) then begin
    if tabs.Count=0 then SetLength(tabs.Tabs, 4)
    else SetLength(tabs.Tabs, tabs.Count*2);
  end;
  tabs.Tabs[tabs.Count].Offset:=AOffset;
  tabs.Tabs[tabs.Count].Align:=ta;
  inc(tabs.Count);
end;

function TRTFParams.Equal(prm: TRTFParams): boolean;
begin
  result := false;

  if pn.Style<>prm.pn.Style then
    exit;

  if pa<>prm.pa then
    exit;

  if (fnt.Name<>prm.fnt.Name) or
     (fnt.Size<>prm.fnt.Size) or
     (fnt.Color<>prm.fnt.Color) or
     (fnt.Style<>prm.fnt.Style) or
     (fnt.VScriptPos<>prm.fnt.VScriptPos) or
     (fnt.HasBkClr<>prm.fnt.HasBkClr) or
     (fnt.BkColor<>prm.fnt.BkColor)
  then
    exit;

  if (pm.LineSpacing<>prm.pm.LineSpacing) or
     (pm.FirstLine<>prm.pm.FirstLine) or
     (pm.HeadIndent<>prm.pm.HeadIndent) or
     (pm.SpaceAfter<>prm.pm.SpaceAfter) or
     (pm.SpaceBefore<>prm.pm.SpaceBefore) or
     (pm.TailIndent<>prm.pm.TailIndent)
  then
    exit;

  result := true;
end;

{ TRTFMemoParserr }

procedure TRTFMemoParser.AddText(const atext: string);
var
  nl : Integer;
  l  : Integer;
begin
  nl:=txtlen+length(atext);
  if nl>length(txtbuf) then begin
    l:=length(txtbuf);
    while l<nl do
      if l=0 then l:=256
      else l:=l*2;
    SetLength(txtbuf, l);
  end;
  Move(atext[1], txtbuf[txtlen+1], length(atext));
  inc(txtlen, length(atext));
end;

procedure TRTFMemoParser.classUnk;
var
  txt : string;
  ws : UnicodeString;
begin
  if not Assigned(prm) then exit;

  txt:=GetRtfText;
  if (txt='\object') then begin
    SkipGroup;
    Exit;
  end;
  if (length(txt)>2) and (txt[1]='\') and (txt[2]='u') and (txt[3] in ['0'..'9']) then begin
    SetLength(Ws{%H-},1);
    ws[1]:=UnicodeChar(rtfParam);
    AddText( UTF8Encode(ws) );
    skipNextCh:=true;
  end;
end;

procedure TRTFMemoParser.classText;
var
  txt : string;
  bt  : Char;
begin
  if not Assigned(prm) then exit;
  if skipNextCh then begin
    skipNextCh:=false;
    Exit;
  end;

  txt:=Self.GetRtfText;

  if (length(txt)=4) and (txt[1]='\') and (txt[2]=#39) then begin
    if Assigned(langproc) then begin
      bt:=char(RTFCharToByte(txt));

      AddText( langproc(bt) );
    end;
  end else if (length(txt)=2) and (txt[1]='\') and (txt[2] in ['\','{','}']) then begin
    AddText(txt[2]);
  end else begin
    AddText(txt);
  end;
end;

procedure TRTFMemoParser.classControl;
begin
  if not Assigned(prm) then exit;

  if txtlen>0 then begin
    PushText;
  end;
  //writeln('ctrl: ', rtfClass,' ', rtfMajor, ' ', Self.GetRtfText, ' ',rtfMinor,' ', rtfParam);
  case rtfMajor of
    rtfDestination: doDestination(rtfMinor, rtfParam);
    rtfSpecialChar: doSpecialChar;
    rtfCharAttr: doChangeCharAttr(rtfMinor, rtfParam);
    rtfParAttr: doChangePara(rtfMinor, rtfParam);
  end;
end;

procedure TRTFMemoParser.classGroup;
var
  t : TRTFParams;
begin
  if not Assigned(prm) then exit;

  case rtfMajor of
    rtfBeginGroup: begin
      t:=TRTFParams.Create(prm);
      prm:=t;
    end;
    rtfEndGroup: begin
      if Assigned(prm) then begin
        t:=prm.prev;
        prm.Free;
        prm:=t;
      end;
    end;
  end;
end;

procedure TRTFMemoParser.classEof;
begin
  PushText;
end;

procedure TRTFMemoParser.destinationPn;
var
  level: Integer;
  gotPnTextA, gotPnTextB: boolean;
  pn: TRTFParaNumbering;

  procedure NotifyPnItem(firstItem: boolean);
  begin
    if paraNum.valid then begin
      Inc(paraNum.aIndex);
      ParaNumberingStart(firstItem);
    end;
  end;

begin

  (*
  <pn>          '{\*' \pnseclvlN <pndesc> '}' |
                '{' \pntext <char> '}' '{\*' \pn <pnlevel> <pndesc> '}'

  <pnlevel>     \pnlvlN | \pnlvlblt | \pnlvlbody | \pnlvlcont

  <pndesc>      <pnnstyle> & <pnchrfmt> & <pntxtb> & <pntxta> & <pnfmt>

  <pnnstyle>    \pncard | \pndec | \pnucltr | \pnucrm | \pnlcltr | \pnlcrm | \pnord | \pnordt |
                \pnbidia | \pnbidib | \pnaiu | \pnaiud | \pnaiueo | \pnaiueod | \pnchosung |
                \pncnum | \pndbnum | \pndbnumd | \pndbnumk | \pndbnuml | \pndbnumt |
                \pndecd | \pnganada | \pngbnum | \pngbnumd | \pngbnumk | \pngbnuml |
                \pniroha | \pnirohad | \pnuldash | \pnuldashd | \pnuldashdd | \pnulhair |
                \pnulth | \pnulwave | \pnzodiac | \pnzodiacd | \pnzodiacl

  <pnchrfmt>    \pnf? & \pnfs? & \pnb? & \pni? & \pncaps? & \pnscaps? & <pnul>? & \pnstrike? & \pncf?

  <pnul>        \pnul | \pnuld | \pnuldb | \pnulnone | \pnulw

  <pnfmt>       \pnnumonce? & \pnacross? & \pnindent? & \pnsp? & \pnprev? & <pnjust>? &
                \pnstart? & \pnhang? & \pnrestart?

  <pnjust>      \pnqc | \pnql | \pnqr

  <pntxtb>      '{' \pntxtb #PCDATA '}'
  <pntxta>      '{' \pntxta #PCDATA '}'
  *)

  // simply discard 'pntext' group, as the spect states: "it is the responsibility
  // of RTF readers that understand the '{\*' \pn ... '}' destination to ignore
  // the \pntext group".
  SkipGroup;

  // now we are within a {\*\pn paragraph numeric definition group
  level := 0;

  Fillchar(pn{%H-}, sizeof(pn), 0);

  gotPnTextA := false;
  gotPnTextB := false;

  // now process {\*\pn
  while GetToken<>rtfEOF do begin

    case rtfClass of

      rtfGroup:
        case rtfMajor of
          rtfEndGroup:
            begin
              dec(level);
              if level=0 then begin
                RouteToken;
                break;
              end;
            end;
          rtfBeginGroup: inc(level);
        end;

      rtfControl:
        case rtfMajor of
          rtfSpecialChar:
            case rtfMinor of
              rtfOptDest:
                continue;
              rtfPar:
                begin
                  if level=0 then
                    NotifyPnItem(false);
                  RouteToken;
                  exit;
                end;
            end;

          rtfDestination:
            case rtfMinor of
              rtfPN:
                begin
                  pn.valid:=true;
                  continue;
                end;
              rtfPNTextB:
                begin
                  gotPnTextB:=true;
                  gotPnTextA:=false;
                  continue;
                end;
              rtfPNTextA:
                begin
                  gotPnTextB:=false;
                  gotPnTextA:=true;
                  continue;
                end;
            end;

          rtfPnLevelAttr:
            begin
              case rtfMinor of
                rtfPnLvlN:
                  begin
                    pn.aType := pntNumeric;
                    pn.level := rtfParam;
                  end;
                rtfPnLvlBlt:
                  begin
                    pn.aType := pntBullet;
                    pn.level := 9;
                  end;
                rtfPnLvlBody:
                  begin
                    pn.aType := pntNumeric;
                    pn.level := 10;
                  end;
                rtfPnLvlCont: pn.skipNumbering := not pn.skipNumbering;
              end;
              continue;
            end;

          rtfPnStyleAttr:
            begin
              case rtfMinor of
                rtfPnStyleCard:     pn.style := pnsCard;
                rtfPnStyleDec:      pn.style := pnsDec;
                rtfPnStyleUcltr:    pn.style := pnsUcltr;
                rtfPnStyleUcrm:     pn.style := pnsUcrm;
                rtfPnStyleLcltr:    pn.style := pnsLcltr;
                rtfPnStyleLcrm:     pn.style := pnsLcrm;
                rtfPnStyleOrd:      pn.style := pnsOrd;
                rtfPnStyleOrdT:     pn.style := pnsOrdt;
                rtfPnStyleBidiA:    pn.style := pnsBidia;
                rtfPnStyleBidiB:    pn.style := pnsBidib;
                rtfPnStyleAiu:      pn.style := pnsAiu;
                rtfPnStyleAiuD:     pn.style := pnsAiud;
                rtfPnStyleAiuEo:    pn.style := pnsAiueo;
                rtfPnStyleAiuEoD:   pn.style := pnsAiueod;
                rtfPnStyleChoSung:  pn.style := pnsChosung;
                rtfPnStyleCNum:     pn.style := pnsCnum;
                rtfPnStyleDBnum:    pn.style := pnsDbnum;
                rtfPnStyleDBnumD:   pn.style := pnsDbnumd;
                rtfPnStyleDBnumK:   pn.style := pnsDbnumk;
                rtfPnStyleDBnumL:   pn.style := pnsDbnuml;
                rtfPnStyleDBnumT:   pn.style := pnsDbnumt;
                rtfPnStyleDecD:     pn.style := pnsDecd;
                rtfPnStyleGanada:   pn.style := pnsGanada;
                rtfPnStyleGbNum:    pn.style := pnsGbnum;
                rtfPnStyleGbNumD:   pn.style := pnsGbnumd;
                rtfPnStyleGbNumK:   pn.style := pnsGbnumk;
                rtfPnStyleGbNumL:   pn.style := pnsGbnuml;
                rtfPnStyleIroha:    pn.style := pnsIroha;
                rtfPnStyleIrohaD:   pn.style := pnsIrohad;
                rtfPnStyleZodiac:   pn.style := pnsZodiac;
                rtfPnStyleZodiacd:  pn.style := pnsZodiacd;
                rtfPnStyleZodiacl:  pn.style := pnsZodiacl;

                rtfPnStyleUlDash:   pn.underlineStyle := pnulDash;
                rtfPnStyleUlDashd:  pn.underlineStyle := pnulDashDot;
                rtfPnStyleUlDashdD: pn.underlineStyle := pnulDashDotDash;
                rtfPnStyleUlHair:   pn.underlineStyle := pnulHair;
                rtfPnStyleUlTh:     pn.underlineStyle := pnulThick;
                rtfPnStyleUlwave:   pn.underlineStyle := pnulWave;
              end;
              continue;
            end;

          rtfPnChrfmtAttr:
            begin
              case rtfMinor of
                rtfPnChrFmtf:       pn.fontNum := rtfParam;
                rtfPnChrFmtfs:      pn.fontSize := round(rtfParam/2);
                rtfPnChrFmtb:       pn.bold := rtfParam=1;
                rtfPnChrFmti:       pn.italic := rtfParam=1;
                rtfPnChrFmtcaps:    pn.caps := rtfParam=1;
                rtfPnChrFmtscaps:   pn.smallcaps := rtfParam=1;
                rtfPnChrFmtstrike:  pn.strike := rtfParam=1;
                rtfPnChrFmtcf:      pn.fontColor := rtfParam;
              end;
              continue;
            end;

          rtfPnulAttr:
            begin
              case rtfMinor of
                rtfPnUl:      pn.underlineStyle := pnulSimple;
                rtfPnUlD:     pn.underlineStyle := pnulDot;
                rtfPnUlDb:    pn.underlineStyle := pnulDouble;
                rtfPnUlNone:  pn.underlineStyle := pnulNone;
                rtfPnUlW:     pn.underlineStyle := pnulWord;
              end;
              continue;
            end;

          rtfPnFmtAttr:
            begin
              case rtfMinor of
                rtfPnIndent:  pn.indent := rtfParam;
                rtfPnStart:   pn.aIndex := rtfParam - 1;
                rtfPnHang:    pn.hang   := true;
                //rtfPnNumonce: WriteLn('pnnumonce');
                //rtfPnAcross:  WriteLn('pnacross');
                //rtfPnSp:      WriteLn('pnsp');
                //rtfPnPrev:    WriteLn('pnprev');
                //rtfPnRestart: WriteLn('pnrestart');
              end;
              continue;
            end;

          rtfPnJustAttr:
            begin
              case rtfMinor of
                rtfPnQc: pn.just := pnjc;
                rtfPnQl: pn.just := pnjl;
                rtfPnQr: pn.just := pnjr;
              end;
              continue;
            end;

        end;  // rtfControl: case rtfMajor of

      rtfText:
        begin
          if level=0 then begin
            // found text without {\*\pn, list item has started
            NotifyPnItem(false);
            RouteToken;
            exit;
          end;
          if gotPnTextA or gotPnTextB then begin
            if gotPnTextA then pn.AfterText := pn.AfterText + chr(rtfMajor);
            if gotPnTextB then pn.BeforeText := pn.BeforeText + chr(rtfMajor);
            continue;
          end;
        end;

    end;  // case rtfclass of

    // route everything else
    RouteToken;
  end;

  if pn.valid then begin
    if paraNum.valid and (pn.aType=paraNum.aType) and (pn.style=paraNum.style) then
      pn.valid := false // same type and style than existing pn, notify item
    else
      paraNum := pn;
  end;

  NotifyPnItem(pn.valid);
end;

procedure TRTFMemoParser.destinationField;
var
  level: Integer = 1;
  gotFieldinst: boolean = false;
  gotFieldrslt: Boolean = false;
  content: string;
begin
  (*
  <field>      '{' \field <fieldmod>? <fieldinst> <fieldrslt> '}'
  <fieldmod>    \flddirty? & \fldedit? & \fldlock? & \fldpriv?
  <fieldinst>   '{\*' \fldinst <para>+ <fldalt>? '}'
  <fldalt>      \fldalt
  <fieldrslt>   '{' \fldrslt <para>+ '}'
  *)

  content := '';
  Field.valid:= true;
  Initialize(Field);
  while true do begin
    GetToken;
    if CheckCM(rtfGroup, rtfEndGroup) then begin
      if gotFieldinst then begin
        gotFieldinst := false;
        Field.rtfFieldInst := content;
        content := '';
      end;
      if gotFieldrslt then begin
        gotFieldrslt := false;
        Field.rtfFieldRslt := content;
        content := '';
      end;
      dec(level);
      if level=0 then begin
        RouteToken;
        break;
      end;
    end else
    if CheckCM(rtfGroup, rtfBeginGroup) then begin
      inc(level);
    end else
    if CheckCMM(rtfControl, rtfSpecialChar, rtfOptDest) then begin
      continue;
    end else
    if CheckCMM(rtfControl, rtfDestination, rtfFieldInst) then begin
      gotFieldinst := true;
    end else
    if CheckCMM(rtfControl, rtfDestination, rtfFieldResult) then begin
      gotFieldrslt := true;
    end else
    if rtfClass=rtfText then begin
      content := content + chr(rtfMajor);
      if gotFieldInst or not gotFieldRslt then
        continue;
    end;
    RouteToken;
  end;

  // syntetize a token which would triggger field result expression
  SetToken(rtfControl, rtfUnknown, rtfUnknown, rtfNoParam, '');
  RouteToken;
  Field.valid:= false;
end;

procedure TRTFMemoParser.doChangePara(aminor, aparam: Integer);
const
  TabAl : array [rtfTabPos..rtfTabDecimal] of TTabAlignment = (
    tabLeft, tabRight, tabCenter, tabDecimal);
begin
  case aminor of
    rtfParDef:      prm.ResetDefault; // reset clear formatting
    rtfQuadLeft:    prm.pa:=paLeft;
    rtfQuadRight:   prm.pa:=paRight;
    rtfQuadJust:    prm.pa:=paJustify;
    rtfQuadCenter:  prm.pa:=paCenter;
    rtfFirstIndent: begin
      prm.pm.FirstLine:=aparam / 20;
    end;
    rtfLeftIndent: begin
      prm.pm.HeadIndent:=aparam / 20;
    end;
    rtfRightIndent:  prm.pm.TailIndent  := aparam / 20;
    rtfSpaceBefore:  prm.pm.SpaceBefore := aparam / 20;
    rtfSpaceAfter:   prm.pm.SpaceAfter  := aparam / 20;
    rtfSpaceBetween: prm.pm.LineSpacing := aparam / 200;
      // \slN - surprise! the "line spacing" is actually a multiplier based on the FONT size, not linesize
      // where linesize = fontsize * 1.2
    rtfLanguage: begin
      SetLanguage(rtfParam);
    end;
    rtfTabPos,//; rtfKstr : 'tx'; rtfkHash : 0),
    rtfTabRight, // rtfKstr : 'tqr'; rtfkHash : 0),
    rtfTabCenter, //; rtfKstr : 'tqc'; rtfkHash : 0),
    rtfTabDecimal: //; rtfKstr : 'tqdec'; rtfkHash : 0),
      prm.AddTab(aparam / 20, TabAl[aminor]);
  end;
end;

procedure TRTFMemoParser.doDestination(aminor, aparam: Integer);
begin
  case aminor of
    rtfDefaultLanguage:
      deflang:=aparam;
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
    rtfOptDest: SkipGroup;
    rtfLine: AddText(CharLine);
    rtfPar:
      begin
        AddText(CharPara);
        doParagraph;
        if deflang<>0 then
          SetLanguage(deflang);
      end;
    rtfTab:  AddText(CharTab);
  end;
end;

procedure TRTFMemoParser.doChangeCharAttr(aminor, aparam: Integer);
var
  p : PRTFColor;
const
  HColor : array [1..16] of TColor = (
    clBlack
    ,clBlue
    ,clAqua // Cyan
    ,clLime // Green
    ,clFuchsia  //Magenta
    ,clRed
    ,clYellow
    ,clGray // unused!
    ,clNavy // DarkBlue
    ,clTeal // DarkCyan
    ,clGreen  // DarkGreen
    ,clPurple // clDarkMagenta
    ,clMaroon // clDarkRed
    ,clOlive // clDarkYellow
    ,clGray  //clDarkGray
    ,clSilver //clLightGray
  );
begin
  case aminor of
    rtfPlain: prm.fnt.Style:=[];
    rtfBold: if aparam=0 then Exclude(prm.fnt.Style,fsBold)  else Include(prm.fnt.Style, fsBold);
    rtfItalic: if aparam=0 then Exclude(prm.fnt.Style,fsItalic)  else Include(prm.fnt.Style, fsItalic);
    rtfStrikeThru: if aparam=0 then Exclude(prm.fnt.Style,fsStrikeOut)  else Include(prm.fnt.Style, fsStrikeOut);
    rtfFontNum: prm.fnum:=aparam;
    rtfFontSize: prm.fnt.Size:=round(aparam/2);
    rtfUnderline: if aparam=0 then Exclude(prm.fnt.Style,fsUnderline)  else Include(prm.fnt.Style, fsUnderline);
    rtfNoUnderline: Exclude(prm.fnt.Style, fsUnderline);

    rtfSuperScript: prm.fnt.VScriptPos:=vpSuperscript;
    rtfSubScript  : prm.fnt.VScriptPos:=vpSubScript;
    rtfNoSuperSub : prm.fnt.VScriptPos:=vpNormal;

    rtfHighlight: begin
      prm.fnt.HasBkClr := (aparam>0) and (aparam<=high(HColor));
      if prm.fnt.HasBkClr then begin
        if HLFromCTable then prm.fnt.BkColor:=HColor[aparam]
        else begin
          p:=Colors[aparam];
          if Assigned(p) then prm.fnt.BkColor:=RGBToColor(p^.rtfCRed, p^.rtfCGreen, p^.rtfCBlue)
          // fallback?
          else prm.fnt.BkColor:=HColor[aparam];
        end;
      end;
    end;
    rtfForeColor: begin
      if rtfParam<>0 then p:=Colors[rtfParam]
      else p:=nil;
      if not Assigned(p) then
        prm.fnt.Color:=DefaultTextColor
      else
        prm.fnt.Color:=RGBToColor(p^.rtfCRed, p^.rtfCGreen, p^.rtfCBlue);
    end;
  end;
end;

procedure TRTFMemoParser.SetLanguage(AlangCode: integer);
begin
  lang:=AlangCode;
  langproc:=nil;
  LangConvGet(lang, langproc);
end;

function TRTFMemoParser.DefaultTextColor: TColor;
begin
  Result:=ColorToRGB(defColor);
end;

procedure TRTFMemoParser.LoadFromChunks;
var
  offset, i, len: Integer;
begin
  offset := 0;
  for i:=0 to Length(Chunks)-1 do begin

    case Chunks[i].typ of
      ctpnItem:;
      ctPnStart, ctPnEnd:
        begin
          Memo.SelStart := MaxInt;
          Memo.SetParaNumbering(Memo.SelStart, 0, Chunks[i].prm.pn);
        end;
      else
        begin
          len := UTF8Length(Chunks[i].Text);
          memo.SelStart := MaxInt;
          offset := memo.SelStart;
          Memo.InDelText(Chunks[i].Text, offset, 0);
          Memo.SetParaMetric(offset, len, Chunks[i].prm.pm);
          if Chunks[i].prm.tabs.Count>0 then
            Memo.SetParaTabs(offset, len, Chunks[i].prm.tabs);
          Memo.SetTextAttributes(offset, len, Chunks[i].prm.fnt);
          if Chunks[i].Link<>'' then
            Memo.SetLink(offset, len, true, Chunks[i].Link);
        end;
    end;

  end;
end;

procedure TRTFMemoParser.Consolidate;
var
  track: array of boolean;
  i, last: Integer;
begin

  {$IFDEF DEBUG}
  DumpChunks('Before consolidate');
  {$ENDIF}

  last := 0;
  SetLength(track{%H-}, length(chunks));
  track[0] := false;
  for i:=1 to Length(chunks)-1 do begin
    // todo: this comparison should be made in the type of chunk[]
    if (Chunks[i].typ=Chunks[last].typ) and
       (Chunks[i].Link=Chunks[last].Link) and
       (Chunks[i].prm.Equal(Chunks[Last].prm)) then
    begin
      Chunks[last].Text := Chunks[last].Text + Chunks[i].Text;
      Chunks[i].prm.Free;
      Track[i] := true
    end else begin
      last := i;
      Track[i] := false;
    end;
  end;

  last := 0;
  i := length(track)-1;
  while i>=0 do begin
    if Track[i] then begin
      //if i=0 then Error('Error, first element should not be deleted');
      inc(last)
    end else if (last>0) then begin
      delete(chunks, i+1, last);
      last := 0;
    end;
    dec(i);
  end;

  {$IFDEF DEBUG}
  DumpChunks('After consolidate');
  {$ENDIF}

end;

{$IFDEF DEBUG}
procedure TRTFMemoParser.DumpChunks(msg: string);
var
  i, Offset: Integer;
  len: PtrInt;
  s: string;
begin
  Offset := 0;
  WriteLn;
  WriteLn(msg,' found: ', Length(Chunks),' Chunks');
  for i:=0 to Length(Chunks)-1 do begin
    with Chunks[i] do begin
      WriteStr(s, Typ);
      len := UTF8Length(Text);
      WriteLn;
      WriteLn(' Chunk: ', i);
      WriteLn('  Type: ', s);
      WriteLn('Offset: ', offset);
      WriteLn('   Len: ', len);
      WriteLn('  Text: ', DbgStr(Text));
      WriteLn('  Link: ', Link);
      Write  ('  Font: ', prm.fnt.Name,',',prm.fnt.Size,',',ColorToString(prm.fnt.Color));
      if fsBold in prm.fnt.Style then Write(' bold');
      if fsItalic in prm.fnt.Style then Write(' italic');
      if fsUnderline in prm.fnt.Style then Write(' underline');
      WriteLn;
      Write('Alignment: ');
      case prm.pa of
        paLeft: WriteLn('Left');
        paRight: WriteLn('Right');
        paCenter: WriteLn('Center');
        paJustify: WriteLn('Justify');
      end;
      WriteLn('ParMetric:');
      WriteLn('  FirstLine: ', FloatToStr(prm.pm.FirstLine));
      WriteLn(' TailIndent: ', FloatToStr(prm.pm.TailIndent));
      WriteLn(' HeadIndent: ', FloatToStr(prm.pm.HeadIndent));
      WriteLn('SpaceBefore: ', FloatToStr(prm.pm.SpaceBefore));
      WriteLn(' SpaceAfter: ', FloatToStr(prm.pm.SpaceAfter));
      WriteLn('LineSpacing: ', FloatToStr(prm.pm.LineSpacing));
      WriteStr(s, prm.pn.Style);
      WriteLn('Para Numbering: ',s);
      WriteLn('     indent: ', FloatToStr(prm.pn.Indent));
      WriteLn(' CustomChar: ', prm.pn.CustomChar);
      WriteLn('NumberStart: ', prm.pn.NumberStart);
      WriteLn('    SepChar: ', prm.pn.SepChar);
      WriteLn('ForceNewNum: ', prm.pn.ForceNewNum);
      offset := offset + len;
    end;
  end;
end;
{$ENDIF}

function TRTFMemoParser.GetHyperlink(out alinkref: string): boolean;
var
  p: SizeInt;
begin
  result := (Field.valid);
  if result then begin
    p := pos('HYPERLINK', Field.rtfFieldInst);
    result := p>0;
    if result then begin
      aLinkref := Trim(copy(Field.rtfFieldInst, p + 9, Length(Field.rtfFieldInst)));
      p := 1;
      if (aLinkRef<>'') and (aLinkref[p] in ['"','''']) then Delete(aLinkRef, p, 1);
      p := Length(aLinkref);
      if (aLinkRef<>'') and (aLinkref[p] in ['"','''']) then Delete(aLinkRef, p, 1);
    end;
  end;
end;

// Converts parsed record paraNum into hi level TParaNumbering
function TRTFMemoParser.GetParaNumbering: TParaNumbering;
begin
  InitParaNumbering(result);
  case paraNum.aType of
    pntNumeric:
      begin
        case paraNum.style of
          pnsLcltr: result.Style := pnLowLetter;  // Lowercase alphabetical numbering (a, b, c).
          pnsUcltr: result.Style := pnUpLetter;   // Uppercase alphabetical numbering (A, B, C).
          pnsLcrm:  result.Style := pnLowRoman;   // Lowercase Roman numbering (i, ii, iii).
          pnsUcrm:  result.Style := pnUpRoman;    // Uppercase Roman numbering (I, II, III).
          else      result.Style := pnNumber;     // Decimal numbering (1, 2, 3).
        end;
        result.NumberStart := paraNum.aIndex;
        if paraNum.AfterText<>'' then
          result.SepChar := paraNum.AfterText[1];
      end
    else
      result.Style := pnBullet;
  end;
  result.Indent:= paraNum.indent.Value;
end;

function TRTFMemoParser.NewChunk: Integer;
begin
  result := Length(chunks);
  SetLength(Chunks, result + 1);
  Chunks[result].typ := ctEmpty;
  Chunks[result].prm := TRTFParams.Create(nil);
end;

procedure TRTFMemoParser.ParaNumberingStart(newList: boolean);
begin
  if txtlen>0 then
    pushText;

  doPnItemStart(newList);
end;

procedure TRTFMemoParser.doParagraph;
begin
  if pnListStarted and not pnItemStarted then begin
    doPnListEnded;
    pnListStarted := false;
  end;
  pnItemStarted := false;
end;

procedure TRTFMemoParser.PushText;
var
  len   : Integer;
  pf    : PRTFFONT;
  selst : Integer;
  b     : string;
  i     : Integer;
begin

  if not Assigned(prm) then exit;

  if txtlen=0 then Exit;

  b:=Copy(txtbuf, 1, txtlen);

  len:=UTF8Length(b);

  txtLen := 0;
  txtBuf := '';
  if Length(b)=0 then Exit;

  if fFastLoad then begin

    i := NewChunk;
    Chunks[i].typ := ctText;
    Chunks[i].Text := b;

    if Assigned(prm) then begin
      Chunks[i].prm.fnt := prm.fnt;
      Chunks[i].prm.pm := prm.pm;
      Chunks[i].prm.pm.FirstLine += Chunks[i].prm.pm.HeadIndent;
      Chunks[i].prm.pa := prm.pa;
      Chunks[i].prm.tabs := prm.tabs;
    end;

    pf:=Fonts[prm.fnum];
    if Assigned(pf) then
      Chunks[i].prm.fnt.Name:=pf^.rtfFName;

    if Field.valid then begin
      if GetHyperlink(b) then
        Chunks[i].Link := b;
    end;

    exit;
  end;

  Memo.SelStart:=MaxInt;
  selst:=Memo.SelStart;
  // in order to get the start selection, we need to switch to the last character
  // and then get the value. SelStart doesn't match GetTextLen, since
  // "StartSel" is based on number of visible characters (i.e. line break is 1 character)
  // while GetTextLen is based on number of actual string characters
  // selst:=Memo.GetTextLen;

  Memo.SelStart:=selst;
  Memo.SelLength:=0;
  Memo.SelText:=b;

  if Assigned(prm) then begin
    prm.pm.FirstLine:=prm.pm.HeadIndent+prm.pm.FirstLine;
    Memo.SetParaMetric(selst, 1, prm.pm );
    prm.pm.FirstLine:=prm.pm.FirstLine-prm.pm.HeadIndent;

    Memo.SetParaAlignment(selst, 1, prm.pa );

    if prm.tabs.Count>0 then
      Memo.SetParaTabs(selst, 1, prm.tabs);
  end;

//  Memo.GetTextAttributes(selst, font);
  pf:=Fonts[prm.fnum];
  if Assigned(pf) then prm.fnt.Name:=pf^.rtfFName;
  //prm.fnt.Size:=round(fsz);
  //prm.fnt.Style:=fst;
  //prm.fnt.Color:=ColorToRGB(fColor);
  //prm.fnt.HasBkClr:=hasbk;
  //prm.fnt.BkColor:=bcolor;
  Memo.SetTextAttributes(selst, len, prm.fnt);

  if Field.valid then begin
    if GetHyperlink(b) then
      Memo.SetLink(selst, len, true, b);
  end;
end;

procedure TRTFMemoParser.doPnItemStart(newList: boolean);
var
  n: TParaNumbering;
  i: Integer;
begin
  if not fFastLoad then begin
    if newList then begin
      n := GetParaNumbering;
      Memo.SelStart := MaxInt;
      Memo.SetParaNumbering(Memo.SelStart, 0, n);
      fLastParaNumStyle := n.Style;
    end;
  end else begin
    n := GetParaNumbering;
    i := NewChunk;
    Chunks[i].prm.pn := n;
    if newList then begin
      Chunks[i].typ := ctPnStart;
      pnListStarted := true;
    end else
      Chunks[i].typ := ctPnItem;
    pnItemStarted := true;
  end;
end;

procedure TRTFMemoParser.doPnListEnded;
var
  n: TParaNumbering;
  i: Integer;
begin
  if not fFastLoad then begin;
    if paraNum.valid then begin
      InitParaNumbering(n);
      n.Style := pnNone;
      Memo.SelStart := MaxInt;
      Memo.SetParaNumbering(Memo.SelStart, 0, n);
    end;
  end else begin
    paraNum.valid := false;
    InitParaNumbering(n);
    n.Style:=pnNone;
    i := NewChunk;
    Chunks[i].typ := ctPnEnd;
    Chunks[i].prm.pn := n;
  end;
end;

constructor TRTFMemoParser.Create(AMemo: TCustomRichMemo; AStream: TStream;
  withFastLoad: boolean);
begin
  inherited create(AStream);
  defColor := clBlack;
  fFastLoad := withFastLoad;

  ClassCallBacks[rtfText]:=@classText;
  ClassCallBacks[rtfControl]:=@classControl;
  ClassCallBacks[rtfGroup]:=@classGroup;
  ClassCallBacks[rtfUnknown]:=@classUnk;
  ClassCallBacks[rtfEof]:=@classEof;

  DestinationCallBacks[rtfField ] := @destinationField;
  DestinationCallBacks[rtfPNText] := @destinationPn;
  Memo:=AMemo;
end;

destructor TRTFMemoParser.Destroy;
var
  t: TRTFParams;
  i: Integer;
begin
  // cleanup
  while Assigned(prm) do begin
    t:=prm;
    prm:=prm.prev;
    t.Free;
  end;
  for i:=0 to Length(chunks)-1 do begin
    chunks[i].prm.Free;
    chunks[i].Text:='';
    chunks[i].Link:='';
  end;
  inherited Destroy;
end;

procedure TRTFMemoParser.StartReading;
var
  t : TRTFParams;
begin
  Memo.Lines.BeginUpdate;
  try

    prm:=TRTFParams.Create(nil);
    prm.fnt.Size:=12; //\fsN Font size in half-points (the default is 24).
    prm.fnum:=0;
    prm.ResetDefault;

    inherited StartReading;
    PushText;

    // clear the stack, if overflow
    while Assigned(prm) do begin
      t:=prm.prev;
      prm.Free;
      prm:=t;
    end;

    if fFastLoad then begin
      Consolidate;
      LoadFromChunks;
    end;

    Memo.SelStart:=0;
    Memo.SelLength:=0;
  finally
    Memo.Lines.EndUpdate;
  end;
end;

function MVCParserLoadStream(ARich: TCustomRichMemo; Source: TStream;
  fastLoad: boolean): Boolean;
var
  p   : TRTFMemoParser;
begin
  Result:=Assigned(ARich) and Assigned(Source);
  if not Result then Exit;

  p:=TRTFMemoParser.Create(ARich, Source, fastLoad);
  try
    p.StartReading;
  finally
    p.Free;
  end;
  Result:=True;
end;

procedure RegisterRTFLoader;
begin
  RTFLoadStream:=@MVCParserLoadStream;
  LangConvInit;
end;

function SaveStream(ARich: TCustomRichMemo; Dst: TStream): Boolean;
var
  p : TSaveParams;
begin
  FillChar(p, sizeof(p), 0);
  p.start:=-1;
  p.len:=-1;
  IntSaveStream(ARich, p, Dst);
  Result:=True;
end;

procedure RegisterRTFSaver;
begin
  RTFSaveStream:=@SaveStream;
end;

type
  TStyleRange = class(TObject)
    font       : TFontParams;
    fontId     : Integer; // assigned font ID
    colorId    : Integer;
    textStart  : Integer;
    textLength : Integer;
    paraNum    : TParaNumbering;
    linkRef    : string;
    next       : TStyleRange;
  end;

procedure FreeStyleList(var root: TStyleRange);
var
  t: TStyleRange;
begin
  while Assigned(root) do begin
    t:=root.next;
    root.Free;
    root:=t;
  end;
end;

procedure PrepareFontTable(styleslist: TStyleRange; afontTable: TStringList);
var
  rng : TStyleRange;
  i   : integer;
begin
  rng:=styleslist;
  while Assigned(rng) do begin
    i:=afontTable.IndexOf(rng.font.Name);
    if i<0 then
      i:=afontTable.Add(rng.font.Name);
    rng.fontId:=i;
    rng:=rng.next;
  end;
  // {\f0\fswiss\fcharset0 Arial;}
end;

function ColorToRtfText(const cl: TColor): string;
var
  r: integer;
begin
  r:=ColorToRGB(cl);
  Result:=
    '\red'+IntToStR( byte( (r and clRed) shr 0) )
    +'\green'+IntToStR( byte( (r and clLime) shr 8) )
    +'\blue'+IntToStR( byte( (r and clBlue) shr 16) );
end;

procedure PrepareColorTable(styleslist: TStyleRange; acolorTable: TStringList);
var
  rng : TStyleRange;
  i   : integer;
  t   : string;
begin
  rng:=styleslist;
  acolorTable.Add('');
  while Assigned(rng) do begin
    if rng.font.Color=clBlack then
      rng.colorId:=0
    else begin
      t:=ColorToRtfText(rng.font.Color);
      i:=acolorTable.IndexOf(t);
      if i<0 then i:=acolorTable.Add(t);
      rng.colorId:=i;
    end;
    rng:=rng.next;
  end;
  // {\f0\fswiss\fcharset0 Arial;}
end;

function GetRTFWriteText(const u: UnicodeString; var idx : integer; var isNewPara: Boolean;
  addpar:boolean=true): string;
var
  i : integer;
begin
  Result:='';
  i:=idx;
  isNewPara:=false;
  while i<=length(u) do begin
    if u[i]='\' then Result:=Result+'\\'
    else if u[i]='{' then Result:=Result+'\{'
    else if u[i]='}' then Result:=Result+'\}'
    else if u[i]=#10 then begin
      if addpar then Result:=Result+'\par';
      isNewPara:=true;
      inc(i);
      Break;
    end else if u[i]=#13 then begin
      if addpar then Result:=Result+'\par';
      isNewPara:=true;
      inc(i);
      Break;
    end else if u[i]<#127 then Result:=Result+char(byte(u[i]))
    else Result:=Result+'\u'+IntToStr(word(u[i]))+'  '; // adding a blank "space" character replacement
    inc(i);
  end;
  idx:=i;
end;

procedure IntSaveStream(ARich: TcustomRichMemo; SaveParams: TSaveParams;
  Dst: TStream);
type
  TOutType=(otIgnore, otText, otControl, otGroup);

var
  ofs     : Integer;
  needlen : Integer;
  endless : Boolean;
  root    : TStyleRange; // first in the list
  last    : TStyleRange; // last in the list
  rng     : TStyleRange; // temporary
  st, len : Integer;
  u       : UnicodeString;
  fontTable  : TStringList;
  colorTable : TStringList;
  i         : Integer;
  isnewpara : Boolean;
  s         : string;
  aLinkRef  : string;
  ispn      : boolean;
  paraIndex : Integer;

  isbold    : Boolean;
  isitalic  : Boolean;
  isuline   : Boolean;
  issupersub: Boolean;
  isColor   : integer;
  outType   : TOutType;

  pm: TParaMetric;
  pn: TParaNumbering;

  procedure RtfOut(s: string; outt:TOutType=otControl);
  begin
    if (outType=otControl) and (outt=otText) then
      s := ' '+s;
    Dst.Write(s[1], length(s));
    outType := outt;
  end;

  procedure AddRange;
  begin
    if not Assigned(root) then root:=rng;
    if Assigned(last) then last.next:=rng;
    last:=rng;
  end;

  procedure doPnText;
  begin
    case pn.style of
      pnNumber:
        RtfOut(format('{\pntext\f%d %d%s\tab}',[1, pn.NumberStart, UTF8Encode(pn.SepChar)[1]]), otGroup);
      pnBullet:
        RtfOut(format('{\pntext\f%d%s\tab}',[2, '\''B7']), otGroup);
      pnLowLetter, pnLowRoman, pnUpLetter, pnUpRoman:
        begin
        end;
    end;
    if not ispn then begin
      case pn.Style of
        pnNumber:
          RtfOut(format('{\*\pn\pnlvlbody\pnf%d\pnindent%d\pnstart%d\pndec{\pntxta%s}}',[1,180,pn.NumberStart,pn.SepChar]), otGroup);
        pnBullet:
          RtfOut(format('{\*\pn\pnlvlblt\pnf%d\pnindent%d{\pntxtb%s}}',[2,180, '\''B7']), otGroup);
      end;
      ispn := true;
    end;
  end;

  procedure doFormatting;
  begin
    rtfOut('\f'+IntToStr(rng.fontId));
    rtfOut('\fs'+IntToStr(rng.font.Size*2));
    if (fsBold in rng.font.Style) then begin
      RtfOut('\b');
      isbold:=true;
    end else begin
      if isbold then rtfOut('\b0');
      isbold:=false;
    end;
    if (fsUnderline in rng.font.Style) then begin
      rtfOut('\ul');
      isuline:=true
    end else begin
      if isuline then rtfOut('\ulnone');
      isuline:=false;
    end;
    if isColor<>rng.colorId then begin
      rtfOut('\cf'+IntToStR(rng.colorId));
      isColor:=rng.ColorId;
    end;
    if (fsItalic in rng.font.Style) then begin
      rtfOut('\i');
      isitalic:=true;
    end else begin
      if isitalic then rtfOut('\i0');
      isitalic:=false;
    end;
    if rng.font.VScriptPos=vpSuperScript then begin
      rtfOut('\super');
      issupersub:=true;
    end;
    if rng.font.VScriptPos=vpSubScript then begin
      rtfOut('\sub');
      issupersub:=true;
    end;
    if rng.font.VScriptPos=vpNormal then begin
      if issupersub then rtfOut('\nosupersub');
      issupersub:=false;
    end;
  end;

  procedure doHeader;
  var
    j: Integer;
  begin
    PrepareFontTable(root, fontTable);
    PrepareColorTable(root, colorTable);

    RtfOut('{\rtf1\ansi\ansicp1252\deff0\deflan1033');

    // start of RTF
    if fontTable.Count>0 then begin
      // at least on font should be present anyway.
      RtfOut('{\fonttbl');
      for j:=0 to fontTable.Count-1 do begin
        // setting font id, charset to 0 and name
        RtfOut('{\f'+IntToStR(j)+'\fcharset0 '+fontTable[j]+';}');
      end;
      RtfOut('}'+lineending);
    end;
    if colorTable.Count>1 then begin
      RtfOut('{\colortbl');
      for j:=0 to colorTable.Count-1 do begin
        RtfOut( colortable[j] );
        RtfOut( ';');
      end;
      RtfOut('}'+lineending);
    end;
    RtfOut('{\*\generator TRichMemo 1.0.0}'+Lineending);
  end;

  procedure doEndParagraph;
  begin
    if ispn and (paraIndex>0) then begin
      rtfOut('\pard');
      ispn := false;
    end;
    rtfOut('\par');
    RtfOut(lineEnding, otIgnore); // prettifier
  end;

begin
  if SaveParams.start<0 then ofs:=0
  else ofs:=SaveParams.start;
  root:=nil;
  last:=nil;
  needlen:=SaveParams.len;
  endless:=needlen<0;

  while ARich.GetStyleRange(ofs, st, len) do begin

    if ARich.GetParaNumbering(ofs, pn) and (pn.Style<>pnNone) then begin
      rng := TStyleRange.Create;
      rng.paraNum := pn;
      if pn.Style=pnBullet then
        rng.font.Name:='Symbol'
      else
        rng.font.Name:='Arial'; // TODO: make an option in richmemo
      AddRange;
    end;

    rng:=TStyleRange.Create;
    rng.textStart:=st;
    if not endless then begin
      if needlen<len then rng.textLength:=needlen
      else rng.textLength:=len;
      dec(needLen, len);
    end else
      rng.textLength:=len;

    ARich.GetTextAttributes(st, rng.font);

    if ARich.GetLink(st, aLinkRef) then
      rng.linkRef := aLinkRef;

    AddRange;

    ofs := st + len;
    if not endless and (needLen<=0) then break;
  end;

  if root=nil then begin
    // GetStyleRange failed - fallback to simple style export!
    root:=TStyleRange.Create;
    root.textStart:=0;
    root.textLength:=MaxInt;
    root.font.Name:=ARich.Font.Name;
    root.font.Size:=ARich.Font.Size;
  end;

  fontTable:=TStringList.Create;
  colorTable:=TStringList.Create;
  try

    doHeader;

    isnewpara := true;
    rng:=root;
    isbold:=false;
    isitalic:=false;
    issupersub:=false;
    isuline:=false;
    iscolor:=0;
    ispn:=false;
    paraIndex := 0;

    while Assigned(rng) do begin

      if rng.paraNum.Style<>pnNone then begin
        if ispn and (pn.Style<>rng.paraNum.Style) then
          ispn := false; // pn style changed, new pn header needed
        pn := rng.paraNum;
        doPnText;
        rng:=rng.next;
        paraIndex := 0;
        continue;
      end;

      u := ARich.GetUText(rng.textStart, rng.textLength);

      i := 1;
      if rng.linkRef<>'' then begin
        RtfOut('{\field ');
        RtfOut(format('{\*\fldinst HYPERLINK "%s"}',[rng.linkRef]));
        RtfOut(format('{\fldrslt{%s}}}',[GetRTFWriteText(u, i, isnewpara)]));
        rng:=rng.next;
        continue;
      end;

      doFormatting;

      while i<=length(u) do begin

        if isNewPara then begin

          case ARich.GetParaAlignment(i-1+rng.TextStart) of
            paRight:   rtfOut('\qr');
            paCenter:  rtfOut('\qc');
            paJustify: rtfOut('\qj');
          else
          end;

          if not ispn then begin
            ARich.GetParaMetric(i-1+rng.textStart, pm);
            if pm.HeadIndent<>0 then rtfOut('\li'+IntToStr(round(pm.HeadIndent*20)));
            if pm.FirstLine-pm.HeadIndent<>0 then
              rtfOut('\fi'+IntToStr(round((pm.FirstLine-pm.HeadIndent)*20)));
            if pm.TailIndent<>0 then rtfOut('\ri'+IntToStr(round(pm.TailIndent*20)));
            if pm.SpaceAfter<>0 then rtfOut('\sa'+IntToStr(round(pm.SpaceAfter*20)));
            if pm.SpaceBefore<>0 then rtfOut('\sb'+IntToStr(round(pm.SpaceBefore*20)));
            if pm.LineSpacing<>0 then rtfOut('\sl'+IntToStr(round(pm.LineSpacing*200))+'\slmult1');
          end;

        end;

        s:=GetRTFWriteText(u, i, isnewpara, false);
        RtfOut(s, otText);

        if isnewpara then begin
          doEndParagraph;
          inc(paraIndex);
        end;

      end;

      rng:=rng.next;
    end;

    // end of RTF
    RtfOut('}');
  finally
    fontTable.Free;
    colorTable.Free;
  end;
  FreeStyleList(root);
end;

initialization

end.
