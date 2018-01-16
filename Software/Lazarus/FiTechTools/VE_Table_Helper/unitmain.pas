unit unitMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, Grids, ComCtrls, ExtCtrls, Menus, csvdocument, Clipbrd, LCLType;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    ComboBoxSoftwareVer: TComboBox;
    EditAccel20F: TEdit;
    EditAccel65F: TEdit;
    EditAccel165F: TEdit;
    EditFastAccel165F: TEdit;
    EditFastAccel65F: TEdit;
    EditFastAccel20F: TEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    OpenDialog: TOpenDialog;
    PageControl1: TPageControl;
    Panel1: TPanel;
    ScrollBar1: TScrollBar;
    ScrollBar2: TScrollBar;
    ScrollBar3: TScrollBar;
    ScrollBox1: TScrollBox;
    StringGridTransientOverall: TStringGrid;
    StringGridTransientMultVsIAT: TStringGrid;
    StringGridTransientAccelFuel: TStringGrid;
    StringGridTransientFastAccelFuel: TStringGrid;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure StringGridTransientOverallKeyPress(Sender: TObject; var Key: char
      );
  private
    FClipboard: TClipboard;
    FCsvDoc: TCSVDocument;

  protected
    property Clipboard: TClipboard read FClipboard write FClipboard;

  public
    property CsvDoc: TCSVDocument read FCsvDoc write FCsvDoc;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  PressureColumn, RPMColumn, AFRColumn: Integer;
begin
  OpenDialog.DefaultExt := 'csv';
  if OpenDialog.Execute then
  begin
    CsvDoc.LoadFromFile(OpenDialog.FileName);
    PressureColumn := CsvDoc.IndexOfCol('MAP', 0);
    RPMColumn := CsvDoc.IndexOfCol('RPM', 0);
    AFRColumn := CsvDoc.IndexOfCol('AFR', 0);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FClipboard := TClipboard.Create;
  CsvDoc := TCSVDocument.Create;



  StringGridTransientOverall.Cells[0,1] := '-22';
  StringGridTransientOverall.Cells[0,2] := '-4';
  StringGridTransientOverall.Cells[0,3] := '41';
  StringGridTransientOverall.Cells[0,4] := '71.6';
  StringGridTransientOverall.Cells[0,5] := '122.0';
  StringGridTransientOverall.Cells[0,6] := '158.0';
  StringGridTransientOverall.Cells[0,7] := '186.8';
  StringGridTransientOverall.Cells[0,8] := '212.0';

  StringGridTransientOverall.Cells[1,0] := '800';
  StringGridTransientOverall.Cells[2,0] := '1200';
  StringGridTransientOverall.Cells[3,0] := '2300';
  StringGridTransientOverall.Cells[4,0] := '3500';
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FCsvDoc);
  FreeAndNil(FClipboard);
end;

procedure TForm1.StringGridTransientOverallKeyPress(Sender: TObject;
  var Key: char);
var
  s: string;
begin
  if Key = 'v' then
  begin
    Clipboard.Open;
  //  if Clipboard.HasFormat(pcfText) then
    begin
      s := Clipboard.AsText;
    end;
//    StringGridTransientOverall.;
  end;
end;

end.

