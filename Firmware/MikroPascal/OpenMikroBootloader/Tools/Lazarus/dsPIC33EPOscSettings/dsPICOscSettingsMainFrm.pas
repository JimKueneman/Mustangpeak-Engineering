unit dsPICOscSettingsMainFrm;

// Version 0.1
//
// The contents of this file are subject to the Mozilla Public License
// Version 1.1 (the "License"); you may not use this file except in compliance
// with the License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
//
// Alternatively, you may redistribute this library, use and/or modify it under the terms of the
// GNU Lesser General Public License as published by the Free Software Foundation;
// either version 2.1 of the License, or (at your option) any later version.
// You may obtain a copy of the LGPL at http://www.gnu.org/copyleft/.
//
// Software distributed under the License is distributed on an "AS IS" basis,
// WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for the
// specific language governing rights and limitations under the License.
//
// The initial developer of this code is Jim Kueneman <jimkueneman@yahoo.com> and Vcc
//

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls;

type

  { TfrmdsPICOscSettingsMain }

  TfrmdsPICOscSettingsMain = class(TForm)
    chkShowCodeExample: TCheckBox;
    Image1: TImage;
    lbeCrystal: TLabeledEdit;
    lbePLLIn: TLabeledEdit;
    lbePLLOut: TLabeledEdit;
    lbeFosc: TLabeledEdit;
    lbeFcy: TLabeledEdit;
    lbeFp: TLabeledEdit;
    lbeDozeSettings: TLabeledEdit;
    lbePOSCLK: TLabeledEdit;
    btnCompute: TButton;
    memCodeExample: TMemo;
    pnlPLL: TPanel;
    Image2: TImage;
    chkDisplayPLL: TCheckBox;
    lbePLLDIV: TLabeledEdit;
    lbePLLPOST: TLabeledEdit;
    lbePLLPRE: TLabeledEdit;
    lbeFPLLI: TLabeledEdit;
    lbeFSYS: TLabeledEdit;
    lbeFOSC_lbe2: TLabeledEdit;
    tmrStartup: TTimer;
    lbePOSCLK_lbe2: TLabeledEdit;
    pnlAPLL: TPanel;
    LabeledEdit1: TLabeledEdit;
    LabeledEdit2: TLabeledEdit;
    LabeledEdit3: TLabeledEdit;
    LabeledEdit4: TLabeledEdit;
    LabeledEdit5: TLabeledEdit;
    LabeledEdit6: TLabeledEdit;
    Image3: TImage;
    chkDisplayAPLL: TCheckBox;
    procedure btnComputeClick(Sender: TObject);
    procedure chkDisplayPLLClick(Sender: TObject);
    procedure chkShowCodeExampleChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tmrStartupTimer(Sender: TObject);
    procedure chkDisplayAPLLClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmdsPICOscSettingsMain: TfrmdsPICOscSettingsMain;

implementation

{$R *.lfm}

function TwoPowered(x: Cardinal): Cardinal;
var
  i: Cardinal;
begin
  Result := 1;
  for i := 1 to x do
    Result := Result * 2;
end;

procedure TfrmdsPICOscSettingsMain.btnComputeClick(Sender: TObject);
var
  Fcrystal, PLLIn, POSCLK: Extended;
  PLLPRE, PLLDIV, PLLPOST, N1, N2, M: Integer;
  FPLLI, FSYS, FOSC: Extended;
  Fp, Fcy: Extended;
  Doze: Integer;
begin
  Fcrystal := StrToFloatDef(lbeCrystal.Text, 8000000);
  PLLIn := Fcrystal;
  POSCLK := Fcrystal;

  PLLPRE := StrToIntDef(lbePLLPRE.Text, 0) mod 32;
  PLLDIV := StrToIntDef(lbePLLDIV.Text, 0) mod 8192;
  PLLPOST := StrToIntDef(lbePLLPOST.Text, 0) mod 4;
  Doze := StrToIntDef(lbeDozeSettings.Text, 0) mod 8;

  N1 := PLLPRE + 2;
  N2 := 2 * (PLLPOST + 1);
  M := PLLDIV + 2;

  FPLLI := PLLIn / N1;
  FSYS := PLLIn * M / N1;
  FOSC := PLLIn * M / (N1 * N2);

  Fp := FOSC / 2;
  Fcy := Fp / TwoPowered(Doze mod 8);  /////////////////////////////// not sure abut + 1 or  + 2

  
  lbePLLIn.Text := FloatToStr(PLLIn);
  lbePOSCLK.Text := FloatToStr(POSCLK);
  lbePOSCLK_lbe2.Text := FloatToStr(POSCLK);
  lbeFPLLI.Text := FloatToStr(FPLLI);
  lbeFSYS.Text := FloatToStr(FSYS);
  lbeFosc.Text := FloatToStr(FOSC);
  lbeFOSC_lbe2.Text := lbeFosc.Text;
  lbePLLOut.Text := lbeFosc.Text;
  lbeFp.Text := FloatToStr(Fp);
  lbeFcy.Text := FloatToStr(Fcy);
end;

procedure TfrmdsPICOscSettingsMain.chkDisplayAPLLClick(Sender: TObject);
begin
  pnlAPLL.Visible := chkDisplayAPLL.Checked;
end;

procedure TfrmdsPICOscSettingsMain.chkDisplayPLLClick(Sender: TObject);
begin
  pnlPLL.Visible := chkDisplayPLL.Checked;
end;

procedure TfrmdsPICOscSettingsMain.chkShowCodeExampleChange(Sender: TObject);
begin
  memCodeExample.Visible := chkShowCodeExample.Checked;
end;

procedure TfrmdsPICOscSettingsMain.FormCreate(Sender: TObject);
begin
  tmrStartup.Enabled := True;
end;

procedure TfrmdsPICOscSettingsMain.tmrStartupTimer(Sender: TObject);
begin
  tmrStartup.Enabled := False;
  btnCompute.Click;
  pnlPLL.Left := 21;
  pnlPLL.Top := 480;
  pnlAPLL.Left := 8;
  pnlAPLL.Top := 300;
end;

end.
