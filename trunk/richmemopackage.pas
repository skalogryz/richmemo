{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit richmemopackage;

{$warn 5023 off : no warning about unused units}
interface

uses
  RichMemoFactory, richmemoregister, RichMemoRTF, RichMemoUtils, 
  RichMemoHelpers, RTFParsPre211, RtfEditPropDialog, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('richmemoregister', @richmemoregister.Register);
end;

initialization
  RegisterPackage('richmemopackage', @Register);
end.
