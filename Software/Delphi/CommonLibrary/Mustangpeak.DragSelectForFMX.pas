unit Mustangpeak.DragSelectForFMX;

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls;

interface

type
  TDragManager = class(TComponent)
  public
    procedure HandleMouseDown(Target: TFMXObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure HandleMouseMove(Target: TFMXObject; Shift: TShiftState; X, Y: Single);
    procedure HandleMouseUp(Target: TFMXObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
  end;

implementation

{ TDragManager }

procedure TDragManager.HandleMouseDown(Target: TFMXObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin

end;

procedure TDragManager.HandleMouseMove(Target: TFMXObject; Shift: TShiftState;
  X, Y: Single);
begin

end;

procedure TDragManager.HandleMouseUp(Target: TFMXObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin

end;

end.
