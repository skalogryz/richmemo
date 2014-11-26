unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ComCtrls, RichMemo, RichMemoUtils, Win32RichMemo;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ImageList1: TImageList;
    OpenDialog1: TOpenDialog;
    RichMemo1: TRichMemo;
    btnLA: TSpeedButton;
    btnCA: TSpeedButton;
    btnRA: TSpeedButton;
    btnJA: TSpeedButton;
    procedure btnCAClick(Sender: TObject);
    procedure btnJAClick(Sender: TObject);
    procedure btnLAClick(Sender: TObject);
    procedure btnRAClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RichMemo1Click(Sender: TObject);
    procedure RichMemo1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { private declarations }
  public
    { public declarations }
    procedure SelectionChanged;
    procedure SetRichAlign(a: TParaAlignment);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then begin
    LoadRTFFile( RichMemo1, OpenDialog1.FileName );
    SelectionChanged;
  end;
end;

procedure TForm1.btnLAClick(Sender: TObject);
begin
  SetRichAlign(paLeft);
end;

procedure TForm1.btnRAClick(Sender: TObject);
begin
  SetRichAlign(paRight);
end;

procedure TForm1.btnCAClick(Sender: TObject);
begin
  SetRichAlign(paCenter);
end;

procedure TForm1.btnJAClick(Sender: TObject);
begin
  SetRichAlign(paJustify);
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  st, ln: Integer;
begin
  RichMemo1.GetParaRange(RichMemo1.SelStart+RichMemo1.SelLength, st, ln);
  Caption:=Format(' start: %d, len %d', [st, ln]);
  RichMemo1.SelStart:=st;
  RichMemo1.SelLength:=ln;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  SelectionChanged;
end;

procedure TForm1.RichMemo1Click(Sender: TObject);
begin
  SelectionChanged;
end;

procedure TForm1.RichMemo1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  SelectionChanged;
end;

procedure TForm1.SelectionChanged;
var
  pa : TParaAlignment;
begin
  pa:=RichMemo1.GetParaAlignment(RichMemo1.SelStart);
  case pa of
    paLeft:  btnLA.Down:=true;
    paRight:  btnRA.Down:=true;
    paCenter:  btnCA.Down:=true;
    paJustify:  btnJA.Down:=true;
  end;
end;

procedure TForm1.SetRichAlign(a: TParaAlignment);
begin
  RichMemo1.SetParaAlignment(RichMemo1.SelStart, RichMemo1.SelLength, a);
end;

end.

