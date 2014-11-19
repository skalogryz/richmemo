unit RichMemoFactory; 

{$mode objfpc}{$H+}

interface

{$define NoRichMemo}
{$ifdef LCLWin32}{$undef NoRichMemo}{$endif}
{$ifdef LCLCarbon}{$undef NoRichMemo}{$endif}
{$ifdef LCLGtk2}{$undef NoRichMemo}{$endif}
{$ifdef LCLCocoa}{$undef NoRichMemo}{$endif}

uses
  WSLCLClasses,
  RichMemo
  {$ifdef NoRichMemo},WSRichMemo{$endif}
  {$ifdef LCLWin32},Win32RichMemo{$endif}
  {$ifdef LCLCarbon},CarbonRichMemo{$endif}

  {$ifdef LCLGtk2}
  //Since Gtk doesn't have a native RTF loader, RichMemo provides
  //a widgetset independent loader. It's registered by default for
  //Gtk. The registration can be prevented by compiling
  //with RichMemoNoDefaultRTFLoader defined.
  //or setting RTFLoadStream to nil (or any other routine) in runtime.
  ,RichMemoRTF, Gtk2RichMemo
  {$endif}
  {$ifdef LCLCocoa},CocoaRichMemo{$endif}
  ;

function RegisterCustomRichMemo: Boolean;

implementation

function RegisterCustomRichMemo: Boolean; alias : 'WSRegisterCustomRichMemo';
begin
  Result := True;
  {$ifdef LCLWin32}RegisterWSComponent(TCustomRichMemo, TWin32WSCustomRichMemo);{$endif}
  {$ifdef LCLCarbon}RegisterWSComponent(TCustomRichMemo, TCarbonWSCustomRichMemo);{$endif}
  {$ifdef LCLGtk2}
  RegisterWSComponent(TCustomRichMemo, TGtk2WSCustomRichMemo);
  {$ifndef RichMemoNoDefaultRTFLoader}
  RegisterRTFLoader;
  {$endif}
  {$endif}
  {$ifdef LCLCocoa}RegisterWSComponent(TCustomRichMemo, TCocoaWSCustomRichMemo);{$endif}
  {$ifdef NoRichMemo}RegisterWSComponent(TCustomRichMemo, TWSCustomRichMemo);{$endif}
end;


end.

