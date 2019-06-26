unit unitMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TASources, TASeries, TAFuncSeries,
  Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, math;

type
  TRealPoint = record
    X, Y: real;
  end;
  PRealPoint = ^TRealPoint;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    ChartPostion: TChart;
    CubicSplineSeries: TCubicSplineSeries;
    ComboBoxEngine: TComboBox;
    EditRodLength: TEdit;
    EditDegrees: TEdit;
    LabelDegrees: TLabel;
    LabelRodLength: TLabel;
    LineSeriesPistonAccel: TLineSeries;
    LineSeriesPistonPosition: TLineSeries;
    LineSeriesPistonVelocity: TLineSeries;
    EditRPM: TEdit;
    EditBore: TEdit;
    EditStroke: TEdit;
    EditCylinders: TEdit;
    LabelBore: TLabel;
    LabelCylinders1: TLabel;
    LabelStroke: TLabel;
    LabelCylinders: TLabel;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure ComboBoxEngineChange(Sender: TObject);
  private

  public
    procedure ReCalculate;

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

function CalculateMinPistonPosition(Stroke, RodLength: real): real;
var
  CrankThrow: real;
  RadianAngle: real;
  Root: real;
begin
  CrankThrow := Stroke/2;
  RadianAngle := degtorad(180);
  Root := sqrt(sqr(RodLength) - sqr(CrankThrow * sqr(sin(RadianAngle))));
  Result := CrankThrow * cos(RadianAngle) + Root;
end;

function CalculatePistonPosition(Stroke, RodLength, DegreeAngle: real): real;
var
  CrankThrow: real;
  RadianAngle: real;
  Root: real;
begin
  CrankThrow := Stroke/2;
  RadianAngle := degtorad(DegreeAngle);
  Root := sqrt(sqr(RodLength) - sqr(CrankThrow * sqr(sin(RadianAngle))));
  Result := CrankThrow * cos(RadianAngle) + Root;
end;

function CalcuatePistonVelocity(Stroke, RodLength, DegreeAngle: real): real;
var
  CrankThrow: real;
  RadianAngle: real;
  Num, Den: real;
begin
  CrankThrow := Stroke/2;
  RadianAngle := degtorad(DegreeAngle);
  Num := sqr(CrankThrow) * sin(RadianAngle) * cos(RadianAngle);
  Den := sqrt(sqr(RodLength) - (sqr(CrankThrow) * sqr( sin(RadianAngle))));
  Result := -CrankThrow * sin(RadianAngle) - Num/Den;
end;

function CalcuatePistonAccel(Stroke, RodLength, DegreeAngle: real): real;
var
  CrankThrow: real;
  RadianAngle: real;
  Num, Den, Num1, Den1: real;
begin
  CrankThrow := Stroke/2;
  RadianAngle := degtorad(DegreeAngle);
  Num := sqr(CrankThrow) * (sqr(cos(RadianAngle)) - sqr(sin(RadianAngle)));
  Den := sqrt(sqr(RodLength) - (sqr(CrankThrow) * sqr( sin(RadianAngle))));
  Num1 := sqr(CrankThrow) * sqr(CrankThrow) * (sqr(cos(RadianAngle)) * sqr(sin(RadianAngle)));
  Den1 := sqrt(sqr(RodLength) - (sqr(CrankThrow) * sqr( sin(RadianAngle))));
  Den1 := Den1 * Den1 * Den1;
  Result := -CrankThrow * cos(RadianAngle) - Num/Den - Num1/Den1;
end;

function ConvertRPM_to_Hertz(RPM: real): real;
begin
  Result := RPM / 60;    // 2pi rad/sec = 1Hz = 60RPM
end;


function ConvertHertz_to_RPM(Hertz: real): real;
begin
  Result := Hertz * 60;    // 2pi rad/sec = 1Hz = 60RPM
end;

function ConvertRPM_to_Omega(RPM: real): real;
begin
  Result := ConvertRPM_to_Hertz(RPM) * 2 * pi;
end;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  ReCalculate;
end;

procedure TForm1.ComboBoxEngineChange(Sender: TObject);
begin
  case ComboBoxEngine.ItemIndex of
    0 : begin EditRodLength.Caption := '6.123'; EditStroke.Caption := '3.31'; EditBore.Caption := '3.63'; end;  // Mopar 273
    1 : begin EditRodLength.Caption := '6.123'; EditStroke.Caption := '3.31'; EditBore.Caption := '3.91'; end;   // Mopar 318
    2 : begin EditRodLength.Caption := '6.123'; EditStroke.Caption := '3.31'; EditBore.Caption := '4.04'; end;   // Mopar 340
    3 : begin EditRodLength.Caption := '6.123'; EditStroke.Caption := '3.58'; EditBore.Caption := '4.00'; end;  // Mopar 360
    4 : begin EditRodLength.Caption := '6.768'; EditStroke.Caption := '3.375'; EditBore.Caption := '4.25'; end;   // Mopar 383
    5 : begin EditRodLength.Caption := '6.86'; EditStroke.Caption := '3.75'; EditBore.Caption := '4.25'; end;  // Mopar 426
    6 : begin EditRodLength.Caption := '6.768'; EditStroke.Caption := '3.75'; EditBore.Caption := '4.32'; end;   // Mopar 440
  end;
  ReCalculate;
end;

procedure TForm1.ReCalculate;
var
  i: Integer;
  Stroke, RodLength, MinPistonPos: real;
  LobeScale: real;
begin
  LineSeriesPistonPosition.BeginUpdate;
  LineSeriesPistonAccel.BeginUpdate;
  LineSeriesPistonVelocity.BeginUpdate;
  CubicSplineSeries.BeginUpdate;
  try
    LineSeriesPistonPosition.Clear;
    LineSeriesPistonAccel.Clear;
    LineSeriesPistonVelocity.Clear;
    CubicSplineSeries.Clear;

    Stroke := StrToFloat(EditStroke.Caption);
    RodLength := StrToFloat(EditRodLength.Caption);
    MinPistonPos := CalculateMinPistonPosition(Stroke, RodLength);

    LobeScale := 5;


    for i := -(StrToInt(EditDegrees.Caption) div 2) to (StrToInt(EditDegrees.Caption) div 2) do
    begin
      LineSeriesPistonPosition.AddXY(i, CalculatePistonPosition(Stroke, RodLength, i) - MinPistonPos, 'Position', clBlue);
      LineSeriesPistonVelocity.AddXY(i, CalcuatePistonVelocity(Stroke, RodLength, i), 'Velocity', clGreen);
      LineSeriesPistonAccel.AddXY(i, CalcuatePistonAccel(Stroke, RodLength, i), 'Accel', clRed);
    end;

    CubicSplineSeries.AddXY(-StrToInt(EditDegrees.Caption) div 2, 0 * LobeScale);
    CubicSplineSeries.AddXY(-240, 0 * LobeScale);
    CubicSplineSeries.AddXY(-230, 0 * LobeScale);
    CubicSplineSeries.AddXY(-220, 0 * LobeScale);
    CubicSplineSeries.AddXY(-210, 0 * LobeScale);
    CubicSplineSeries.AddXY(-200, 0.050 * LobeScale);
    CubicSplineSeries.AddXY(-100, 0.500 * LobeScale);
    CubicSplineSeries.AddXY(0, 0.050 * LobeScale);
    CubicSplineSeries.AddXY(10, 0 * LobeScale);
    CubicSplineSeries.AddXY(20, 0 * LobeScale);
    CubicSplineSeries.AddXY(30, 0 * LobeScale);
    CubicSplineSeries.AddXY(40, 0 * LobeScale);
    CubicSplineSeries.AddXY(StrToInt(EditDegrees.Caption) div 2, 0 * LobeScale);

  finally
    LineSeriesPistonPosition.EndUpdate;
    LineSeriesPistonAccel.EndUpdate;
    LineSeriesPistonVelocity.EndUpdate;
    CubicSplineSeries.EndUpdate;
  end;
end;

end.

