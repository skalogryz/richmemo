unit RichMemoFactory; 

{$mode objfpc}{$H+}

interface


uses
  WSLCLClasses,
  RichMemo
  {$ifdef LCLWin32},Win32RichMemo{$endif}
  {$ifdef LCLCarbon},CarbonRichMemo{$endif}
  ;

function RegisterCustomRichMemo: Boolean;
  
implementation

function RegisterCustomRichMemo: Boolean; alias : 'WSRegisterCustomRichMemo';
begin
  {$ifdef LCLWin32}RegisterWSComponent(TCustomRichMemo, TWin32WSCustomRichMemo);{$endif}
  {$ifdef LCLCarbon}RegisterWSComponent(TCustomRichMemo, TCarbonWSCustomRichMemo);{$endif}
  Result := False;
end;


end.

