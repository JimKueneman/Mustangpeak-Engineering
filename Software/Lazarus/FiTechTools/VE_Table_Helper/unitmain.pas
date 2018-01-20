unit unitMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, Grids, ComCtrls, ExtCtrls, Menus, csvdocument, Clipbrd, LCLType,
  contnrs;

type

  { TVeExtratorData }

  { TEngineData }

  TEngineData = class
  private
    FAFR: real;
    FAFRLearn: real;
    FAFRTrim: real;
    FAirTemp: real;
    FAlpha_N: real;
    FBARO: real;
    FBattery: real;
    FCoolant: real;
    FCylinderTemp: real;
    FFuelFlowRate: real;
    FIACLearn: real;
    FIACSteps: real;
    FIdlePID: real;
    FInjectDuty: real;
    FMAP: real;
    FRPM: real;
    FTPS: real;
    FVacuum: real;
  published
    property RPM: real read FRPM write FRPM;
    property MAP: real read FMAP write FMAP;
    property Coolant: real read FCoolant write FCoolant;
    property AFR: real read FAFR write FAFR;
    property AFRTrim: real read FAFRTrim write FAFRTrim;
    property AFRLearn: real read FAFRLearn write FAFRLearn;
    property TPS: real read FTPS write FTPS;
    property InjectDuty: real read FInjectDuty write FInjectDuty;
    property IACSteps: real read FIACSteps write FIACSteps;
    property IACLearn: real read FIACLearn write FIACLearn;
    property IdlePID: real read FIdlePID write FIdlePID;
    property AirTemp: real read FAirTemp write FAirTemp;
    property CylinderTemp: real read FCylinderTemp write FCylinderTemp;
    property BARO: real read FBARO write FBARO;
    property Alpha_N: real read FAlpha_N write FAlpha_N;
    property FuelFlowRate: real read FFuelFlowRate write FFuelFlowRate;
    property Battery: real read FBattery write FBattery;
    property Vacuum: real read FVacuum write FVacuum;
  end;

  { TEngineDataList }

  TEngineDataList = class(TObjectList)
  private
    function GetItem(Index: Integer): TEngineData;
    procedure SetItem(Index: Integer; AValue: TEngineData);
  public
    property Items[Index: Integer]: TEngineData read GetItem write SetItem; default;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    ComboBoxSoftwareVer: TComboBox;
    OpenDialog: TOpenDialog;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    StringGridTransientOverall: TStringGrid;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure StringGridTransientOverallKeyPress(Sender: TObject; var Key: char);
  private
    FClipboard: TClipboard;
    FEngineDataList: TEngineDataList;

  protected
    property Clipboard: TClipboard read FClipboard write FClipboard;

    procedure ParseCsvToEngineDataList(ACsvDoc: TCSVDocument);
    procedure PlotDataList;

  public
    property EngineDataList: TEngineDataList read FEngineDataList write FEngineDataList;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TEngineDataList }

function TEngineDataList.GetItem(Index: Integer): TEngineData;
begin
  Result := TEngineData( inherited GetItem(Index));
end;

procedure TEngineDataList.SetItem(Index: Integer; AValue: TEngineData);
begin
  Inherited SetItem(INdex, AValue);
end;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  CsvDoc: TCSVDocument;
begin
  OpenDialog.DefaultExt := 'csv';
  if OpenDialog.Execute then
  begin
    CsvDoc := TCSVDocument.Create;
    CsvDoc.LoadFromFile(OpenDialog.FileName);
    ParseCsvToEngineDataList(CsvDoc);
    PlotDataList;
    FreeAndNil(CsvDoc);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FClipboard := TClipboard.Create;
  FEngineDataList := TEngineDataList.Create(True);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FClipboard);
  FreeAndNil(FEngineDataList);
end;

procedure TForm1.StringGridTransientOverallKeyPress(Sender: TObject;
  var Key: char);
var
  s: string;
begin
  if Key = 'v' then
  begin

    StringGridTransientOverall.CopyToClipboard();

    Clipboard.Open;
  //  if Clipboard.HasFormat(pcfText) then
    begin
      s := Clipboard.AsText;
    end;
//    StringGridTransientOverall.;
  end;
end;

procedure TForm1.ParseCsvToEngineDataList(ACsvDoc: TCSVDocument);

var
  i, Col: Integer;
  EngineItem: TEngineData;
  AFloat: real;
begin
  EngineDataList.Clear;
  for i := 1 to ACsvDoc.RowCount - 1 do
  begin
    Col := ACsvDoc.IndexOfCol('RPM', 0);
    if Col > -1 then
    begin
      if TryStrToFloat(ACsvDoc.Cells[Col, i], AFloat) then
      begin
        EngineItem := TEngineData.Create;
        EngineItem.RPM := AFloat;
        EngineDataList.Add(EngineItem);

        Col := ACsvDoc.IndexOfCol('MAP', 0);
        if Col > -1 then
           if TryStrToFloat(ACsvDoc.Cells[Col, i], AFloat) then
             EngineItem.MAP := AFloat;

        Col := ACsvDoc.IndexOfCol('Vacuum', 0);
        if Col > -1 then
          if TryStrToFloat(ACsvDoc.Cells[Col, i], AFloat) then
            EngineItem.Vacuum := AFloat;

        Col := ACsvDoc.IndexOfCol('Coolant Temp', 0);
        if Col > -1 then
          if TryStrToFloat(ACsvDoc.Cells[Col, i], AFloat) then
            EngineItem.Coolant := AFloat;

        Col := ACsvDoc.IndexOfCol('AFR', 0);
        if Col > -1 then
          if TryStrToFloat(ACsvDoc.Cells[Col, i], AFloat) then
            EngineItem.AFR := AFloat;

        Col := ACsvDoc.IndexOfCol('AFR Trim %', 0);
        if Col > -1 then
          if TryStrToFloat(ACsvDoc.Cells[Col, i], AFloat) then
            EngineItem.AFRTrim := AFloat;

        Col := ACsvDoc.IndexOfCol('AFR Learn %', 0);
        if Col > -1 then
          if TryStrToFloat(ACsvDoc.Cells[Col, i], AFloat) then
            EngineItem.AFRLearn := AFloat;

        Col := ACsvDoc.IndexOfCol('TPS', 0);
        if Col > -1 then
          if TryStrToFloat(ACsvDoc.Cells[Col, i], AFloat) then
            EngineItem.TPS := AFloat;

        Col := ACsvDoc.IndexOfCol('Inject Duty%', 0);
        if Col > -1 then
          if TryStrToFloat(ACsvDoc.Cells[Col, i], AFloat) then
            EngineItem.InjectDuty := AFloat;

        Col := ACsvDoc.IndexOfCol('IAC Steps', 0);
        if Col > -1 then
          if TryStrToFloat(ACsvDoc.Cells[Col, i], AFloat) then
            EngineItem.IACSteps := AFloat;

        Col := ACsvDoc.IndexOfCol('IAC Learn', 0);
        if Col > -1 then
          if TryStrToFloat(ACsvDoc.Cells[Col, i], AFloat) then
            EngineItem.IACLearn := AFloat;

        Col := ACsvDoc.IndexOfCol('Idle PID', 0);
        if Col > -1 then
          if TryStrToFloat(ACsvDoc.Cells[Col, i], AFloat) then
            EngineItem.IdlePID := AFloat;

        Col := ACsvDoc.IndexOfCol('Fuel Flowrate', 0);
        if Col > -1 then
          if TryStrToFloat(ACsvDoc.Cells[Col, i], AFloat) then
            EngineItem.FuelFlowRate := AFloat;

        Col := ACsvDoc.IndexOfCol('Air Temp', 0);
        if Col > -1 then
          if TryStrToFloat(ACsvDoc.Cells[Col, i], AFloat) then
            EngineItem.AirTemp := AFloat;

        Col := ACsvDoc.IndexOfCol('Cylinder Temp', 0);
        if Col > -1 then
          if TryStrToFloat(ACsvDoc.Cells[Col, i], AFloat) then
            EngineItem.CylinderTemp := AFloat;

        Col := ACsvDoc.IndexOfCol('Battery', 0);
        if Col > -1 then
          if TryStrToFloat(ACsvDoc.Cells[Col, i], AFloat) then
            EngineItem.Battery := AFloat;

        Col := ACsvDoc.IndexOfCol('BARO', 0);
        if Col > -1 then
          if TryStrToFloat(ACsvDoc.Cells[Col, i], AFloat) then
            EngineItem.BARO := AFloat;

        Col := ACsvDoc.IndexOfCol('Alpha-N MAP', 0);
        if Col > -1 then
          if TryStrToFloat(ACsvDoc.Cells[Col, i], AFloat) then
            EngineItem.Alpha_N := AFloat;

        Col := ACsvDoc.IndexOfCol('Inject Duty%', 0);
        if Col > -1 then
          if TryStrToFloat(ACsvDoc.Cells[Col, i], AFloat) then
            EngineItem.Alpha_N := AFloat;
      end;
    end;
  end;
end;

procedure TForm1.PlotDataList;
var
  i: Integer;
begin
  StringGridTransientOverall.BeginUpdate;
  try
    StringGridTransientOverall.Clear;

    StringGridTransientOverall.ColCount := 19;
    StringGridTransientOverall.RowCount := EngineDataList.Count + 1;
    StringGridTransientOverall.Cells[1, 0] := 'RPM';
    StringGridTransientOverall.Cells[2, 0] := 'AFR';
    StringGridTransientOverall.Cells[3, 0] := 'AFR Learn';
    StringGridTransientOverall.Cells[4, 0] := 'AFR Trim';
    StringGridTransientOverall.Cells[5, 0] := 'TPS';
    StringGridTransientOverall.Cells[6, 0] := 'BARO';
    StringGridTransientOverall.Cells[7, 0] := 'Injector Duty';
    for i := 1 to EngineDataList.Count - 1 do
    begin
      StringGridTransientOverall.Cells[0, i] := IntToStr(i);
      StringGridTransientOverall.Cells[1, i] := FloatToStr(EngineDataList[i-1].RPM);
      StringGridTransientOverall.Cells[2, i] := FloatToStr(EngineDataList[i-1].AFR);
      StringGridTransientOverall.Cells[3, i] := FloatToStr(EngineDataList[i-1].AFRLearn);
      StringGridTransientOverall.Cells[4, i] := FloatToStr(EngineDataList[i-1].AFRTrim);
      StringGridTransientOverall.Cells[5, i] := FloatToStr(EngineDataList[i-1].TPS);
      StringGridTransientOverall.Cells[6, i] := FloatToStr(EngineDataList[i-1].BARO);
      StringGridTransientOverall.Cells[7, i] := FloatToStr(EngineDataList[i-1].InjectDuty);
    end;
  finally
    StringGridTransientOverall.EndUpdate(True);
  end;
end;

end.

