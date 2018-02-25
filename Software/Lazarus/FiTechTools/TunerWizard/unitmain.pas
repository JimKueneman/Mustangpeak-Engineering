unit unitMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    PageControl1: TPageControl;
    PageControlAccel: TPageControl;
    RadioGroup1: TRadioGroup;
    TabSheetAccelStart: TTabSheet;
    TabSheetAccelTune: TTabSheet;
    TabSheetAccel: TTabSheet;
    TabSheet2: TTabSheet;
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

end.

