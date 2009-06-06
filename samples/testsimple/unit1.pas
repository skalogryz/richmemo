unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls,
  RichMemoTypes, RichMemo;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    FontDialog1: TFontDialog;
    RichMemo1: TRichMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  Caption := Format('sel start %d,  sel length %d', [RichMemo1.SelStart, RichMemo1.SelLength]);
  RichMemo1.SetTextAttributes(
    RichMemo1.SelStart, RichMemo1.SelLength,
    [tsm_Color, tsm_Styles], GetFontParams(clRed, [fsBold]) );
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  prm : TFontParams;
begin
  if RichMemo1.GetTextAttributes( RichMemo1.SelStart, prm) then begin
    RichMemo1.Lines.Add('name   '+ prm.Name);
    RichMemo1.Lines.Add('size   '+ IntToStr(prm.Size));
    RichMemo1.Lines.Add('color  '+ IntToHex(Integer(prm.Color), 8));
    RichMemo1.Lines.Add('style  '+ IntToHex(Integer(prm.Style), 8));
  end else
    RichMemo1.Lines.Add('failed');
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  if Fontdialog1.Execute then begin
    RichMemo1.SetTextAttributes(
      RichMemo1.SelStart, RichMemo1.SelLength, FontDialog1.Font);
  end;
end;

initialization
  {$I unit1.lrs}

end.

