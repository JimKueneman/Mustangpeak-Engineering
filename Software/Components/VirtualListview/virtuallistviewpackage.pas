{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit VirtualListviewPackage;

interface

uses
  virtuallistview, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('virtuallistview', @virtuallistview.Register);
end;

initialization
  RegisterPackage('VirtualListviewPackage', @Register);
end.
