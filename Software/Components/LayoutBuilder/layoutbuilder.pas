{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LayoutBuilder;

interface

uses
  layoutbuilderframe, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('layoutbuilderframe', @layoutbuilderframe.Register);
end;

initialization
  RegisterPackage('LayoutBuilder', @Register);
end.
