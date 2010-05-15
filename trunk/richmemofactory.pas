unit RichMemoFactory; 

{$mode objfpc}{$H+}

interface


uses
  WSLCLClasses,
  RichMemo,
  WSRichMemo
  {$ifdef LCLWin32},Win32RichMemo{$endif}
  {$ifdef LCLCarbon},CarbonRichMemo{$endif}
  {$ifdef LCLGtk2},Gtk2RichMemo{$endif}
  ;

function RegisterCustomRichMemo: Boolean;

implementation

function RegisterCustomRichMemo: Boolean; alias : 'WSRegisterCustomRichMemo';
var
  cls : TWSLCLComponentClass;
begin
  Result := True;
  {$ifdef LCLWin32}RegisterWSComponent(TCustomRichMemo, TWin32WSCustomRichMemo);
  {$elif LCLCarbon}RegisterWSComponent(TCustomRichMemo, TCarbonWSCustomRichMemo);
  {$elif LCLGtk2}RegisterWSComponent(TCustomRichMemo, TGtk2WSCustomRichMemo);
  {$else}RegisterWSComponent(TCustomRichMemo, TWSCustomRichMemo);
  {$endif}
  cls:=FindWSComponentClass(TCustomRichMemo);
  if not Assigned(cls) then RegisterWSComponent(TCustomRichMemo, TWSCustomRichMemo);
end;


end.

