unit mustangpeak.sketchpad;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Objects, FMX.Graphics, System.Generics.Collections,
  FMX.Layouts;

type
  TSketchpad = class;  // Forward

  TSketchPadView = class(TRectangle)
  private
    FSketchpad: TSketchpad;
  protected
    procedure Paint; override;
    property SketchPad: TSketchpad read FSketchpad write FSketchpad;
  end;

  TSketchpad = class(TScrollBox)
  private
    FPadSize: TSizeF;
    FPadView: TSketchPadView;
    FPadShowGridLines: Boolean;
    FPadGridLines: TPointF;
    procedure SetPadSize(const Value: TSizeF);
    procedure SetPadShowGridLines(const Value: Boolean);
    procedure SetPadGridLines(const Value: TPointF);
  protected

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property PadView: TSketchPadView read FPadView;
  published
    property PadSize: TSizeF read FPadSize write SetPadSize;
    property PadShowGridLines: Boolean read FPadShowGridLines write SetPadShowGridLines;
    property PadGridLines: TPointF read FPadGridLines write SetPadGridLines;
  end;

procedure Register;

implementation


procedure Register;
begin
 // RegisterComponents('Mustangpeak', [TSketchpad]);
end;

{ TSketchpad }


constructor TSketchpad.Create(AOwner: TComponent);
begin
  inherited;
  FPadView := TSketchPadView.Create(Self);
  PadView.SketchPad := Self;
  PadView.Parent := Self;
  PadView.Position.X := 0;
  PadView.Position.Y := 0;


  PadView.Width := 200;
  PadView.Height := 300;
  PadView.Fill.Kind := TBrushKind.None;
  PadView.Fill.Gradient.Color := TAlphaColorRec.Aqua;
  PadView.Fill.Gradient.Color1 := TAlphaColorRec.Antiquewhite;
 { PadView.Fill.Color := TAlphaColorRec.Blue;   }

  // Broken in OSX, won't show
  AniCalculations.AutoShowing := False;
  FPadGridLines.X := 80;
  FPadGridLines.Y := 20;
end;

destructor TSketchpad.Destroy;
begin
  PadView.Parent := nil;
  PadView.SketchPad := nil;
  inherited;
end;

procedure TSketchpad.SetPadGridLines(const Value: TPointF);
begin
  FPadGridLines := Value;
end;

procedure TSketchpad.SetPadShowGridLines(const Value: Boolean);
begin
  FPadShowGridLines := Value;
  InvalidateRect(ContentRect);
end;

procedure TSketchpad.SetPadSize(const Value: TSizeF);
begin
  if Value <> FPadSize then
  begin
    FPadSize := Value;
    PadView.Width := PadSize.cx;
    PadView.Height := PadSize.cy;
  end;
end;

{ TSketchPadView }



procedure TSketchPadView.Paint;
var
  i: single;
begin
  inherited;
  if SketchPad.PadShowGridLines then
  begin
    i := Sketchpad.ViewportPosition.Y/Sketchpad.PadGridLines.Y - Sketchpad.PadGridLines.Y;
    while (i < Sketchpad.ViewportPosition.Y + Height) and (i < Sketchpad.ViewPortPosition.Y + Height)  do
    begin
      Canvas.DrawLine(TPointF.Create(Sketchpad.ViewportPosition.X, i), TPointF.Create(Sketchpad.ViewportPosition.X + Width, i), 0.5);
      i := i + Sketchpad.PadGridLines.Y;
    end;

    i := Sketchpad.ViewportPosition.X/Sketchpad.PadGridLines.X - Sketchpad.PadGridLines.X;
    while (i < Sketchpad.ViewportPosition.X + Width) and (i < Sketchpad.ViewPortPosition.X + Width)  do
    begin
      Canvas.DrawLine(TPointF.Create(i, Sketchpad.ViewportPosition.Y), TPointF.Create(i, Sketchpad.ViewportPosition.Y + Height), 0.5);
      i := i + Sketchpad.PadGridLines.X;
    end;
  end;
end;

initialization
  RegisterFmxClasses([TSketchPadView, TSketchpad]);

end.
