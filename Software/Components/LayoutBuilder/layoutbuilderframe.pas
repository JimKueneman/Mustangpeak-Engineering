unit layoutbuilderframe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls, Forms, Types, Graphics, FPCanvas, contnrs;

const
  SELECTIONRECTCOLOR = clBlack;
  SELECTIONRECTPEN = psDot;

type
  TFRect = record
    Left, Top, Right, Bottom: Double;
  end;
  PFRect = ^TFRect;

  TFPoint = record
    X, Y: Double;
  end;

  { TMustangpeakSketchPadDoodle }

  TMustangpeakSketchPadDoodle = class(TPersistent)
  private
    FCanvas: TCanvas;
    FCaption: WideString;
    FColor: TColor;
    FFont: TFont;
    FHeight: Double;
    FLeft: Double;
    FFocused: Boolean;
    FScale: Double;
    FSelected: Boolean;
    FSizeCacheDirty: Boolean;    // If the doodle calculates and stores the width/height properites this is reset when scale changes to signal it must be recalculated
    FTop: Double;
    FVisible: Boolean;
    FOnRepaint: TNotifyEvent;
    FWidth: Double;
    function GetHeight: Double; virtual;
    function GetHeightScaled: Integer; virtual;
    function GetLeftScaled: Integer;
    function GetTopScaled: Integer;
    function GetWidth: Double; virtual;
    function GetWidthScaled: Integer; virtual;
    procedure SetCaption(AValue: WideString);
    procedure SetColor(AValue: TColor);
    procedure SetFocused(AValue: Boolean);
    procedure SetFont(AValue: TFont);
    procedure SetHeight(AValue: Double);
    procedure SetLeft(AValue: Double);
    procedure SetScale(AValue: Double);
    procedure SetSelected(AValue: Boolean);
    procedure SetTop(AValue: Double);
    procedure SetVisible(AValue: Boolean);
    procedure SetWidth(AValue: Double);
  protected
    property Canvas: TCanvas read FCanvas write FCanvas;
    property OnRepaint: TNotifyEvent read FOnRepaint write FOnRepaint;
    property Scale: Double read FScale write SetScale;
    property SizeCacheDirty: Boolean read FSizeCacheDirty write FSizeCacheDirty;
    procedure DrawFocusRect;
    procedure DrawSelectionRect;
    procedure DoRepaint;
  public
    property Caption: WideString read FCaption write SetCaption;
    property Color: TColor read FColor write SetColor;
    property Focused: Boolean read FFocused write SetFocused;
    property Font: TFont read FFont write SetFont;
    property Left: Double read FLeft write SetLeft;
    property LeftScaled: Integer read GetLeftScaled;
    property Height: Double read GetHeight write SetHeight;
    property HeightScaled: Integer read GetHeightScaled;
    property Selected: Boolean read FSelected write SetSelected;
    property Top: Double read FTop write SetTop;
    property TopScaled: Integer read GetTopScaled;
    property Visible: Boolean read FVisible write SetVisible;
    property Width: Double read GetWidth write SetWidth;
    property WidthScaled: Integer read GetWidthScaled;

    constructor Create(ALeft, ATop: Double; ACaption: WideString; AColor: TColor; AFont: TFont); virtual;
    destructor  Destroy; override;
    procedure Draw; virtual; abstract;
    function PtInObj(Point: TFPoint): Boolean; virtual; abstract;
    function Bounds: TFRect; virtual;
    function BoundsScaled: TRect; virtual;
    function TopLeft: TFPoint; virtual;
  end;


  { TMustangpeakSketchPadTextDoodle }

  TMustangpeakSketchPadTextDoodle = class(TMustangpeakSketchPadDoodle)
  private
    FTransparentBkGnd: Boolean;
    function GetHeight: Double; override;
    function GetHeightScaled: Integer; override;
    function GetWidth: Double; override;
    function GetWidthScaled: Integer; override;
  public
    property TransparentBkGnd: Boolean read FTransparentBkGnd write FTransparentBkGnd;

    procedure Draw; override;
    function PtInObj(Point: TFPoint): Boolean; override;
  end;

  { TMustangpeakSketchPadGraphicDoodle }

  TMustangpeakSketchPadGraphicDoodle = class(TMustangpeakSketchPadDoodle)
  public
    constructor Create(ALeft, ATop, AWidth, AHeight: Double; AColor: TColor; ACaption: WideString; AFont: TFont); reintroduce; virtual;
    procedure Draw; override;
    function PtInObj(Point: TFPoint): Boolean; override;
  end;

  { TMustangpeakSketchPadDoodles }

  TMustangpeakSketchPadDoodles = class(TObjectList)
  private
    function Get(Index: Integer): TMustangpeakSketchPadDoodle;
    procedure Put(Index: Integer; AValue: TMustangpeakSketchPadDoodle);
  public
    property Items[Index: Integer]: TMustangpeakSketchPadDoodle read Get write Put; default;
  end;

  { TMustangpeakSketchPadGrid }

  TMustangpeakSketchPadGrid = class(TComponent)
  private
    FColor: TColor;
    FStyle: TFPPenStyle;
    FX: Double;
    FY: Double;
    function GetXScaled: Integer;
    function GetYScaled: Integer;
    procedure SetColor(AValue: TColor);
    procedure SetStyle(AValue: TFPPenStyle);
    procedure SetX(AValue: Double);
    procedure SetY(AValue: Double);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Color: TColor read FColor write SetColor;
    property X: Double read FX write SetX;
    property Y: Double read FY write SetY;
    property XScaled: Integer read GetXScaled;
    property YScaled: Integer read GetYScaled;
    property Style: TFPPenStyle read FStyle write SetStyle;
  end;

  TMustangpeakSketchPadInvalidateRectEvent = procedure(Sender: TObject; Rect: PFRect) of object;
  { TMustangpeakSketchPad }

  TMustangpeakSketchPad = class(TComponent)
  private
    FBorder: Integer;
    FCanvas: TCanvas;
    FColor: TColor;
    FFont: TFont;
    FOnBeginUpdate: TNotifyEvent;
    FOnEndUpdate: TNotifyEvent;
    FOnInvalidateRect: TMustangpeakSketchPadInvalidateRectEvent;
    FOnLayoutHeightChanged: TNotifyEvent;
    FOnLayoutWidthChanged: TNotifyEvent;
    FGrid: TMustangpeakSketchPadGrid;
    FDoodles: TMustangpeakSketchPadDoodles;
    FLayoutHeight: Double;
    FLayoutWidth: Double;
    FScale: Double;
    function GetLayoutScaledHeight: Integer;
    function GetLayoutScaledWidth: Integer;
    procedure SetBorder(AValue: Integer);
    procedure SetColor(AValue: TColor);
    procedure SetFont(AValue: TFont);
    procedure SetLayoutHeight(AValue: Double);
    procedure SetLayoutWidth(AValue: Double);
    procedure SetScale(AValue: Double);
  protected
    property Canvas: TCanvas read FCanvas write FCanvas;
    property Doodles: TMustangpeakSketchPadDoodles read FDoodles;

    procedure DoCallbackBeginUpdate;
    procedure DoCallbackEndUpdate;
    procedure DoCallbackLayoutWidthChanged;
    procedure DoCallbackLayoutHeightChanged;
    procedure DoCallbackInvalidateRect(Sender: TObject; Rect: PFRect);
    procedure PaintHorzGridLines;
    procedure PaintVertGridLines;
    procedure PaintDoodles;
  public
    property OnBeginUpdate: TNotifyEvent read FOnBeginUpdate write FOnBeginUpdate;
    property OnEndUpdate: TNotifyEvent read FOnEndUpdate write FOnEndUpdate;
    property OnLayoutWidthChanged: TNotifyEvent read FOnLayoutWidthChanged write FOnLayoutWidthChanged;
    property OnLayoutHeightChanged: TNotifyEvent read FOnLayoutHeightChanged write FOnLayoutHeightChanged;
    property OnInvalidateRect: TMustangpeakSketchPadInvalidateRectEvent read FOnInvalidateRect write FOnInvalidateRect;
    property LayoutWidth: Double read FLayoutWidth write SetLayoutWidth;
    property LayoutHeight: Double read FLayoutHeight write SetLayoutHeight;
    property LayoutScaledWidth: Integer read GetLayoutScaledWidth;
    property LayoutScaledHeight: Integer read GetLayoutScaledHeight;

    constructor Create(AOwner: TComponent; ACanvas: TCanvas); reintroduce; virtual;
    destructor Destroy; override;

    function AddGraphicDoodle(ALeft, ATop, AWidth, AHeight: Double; ACaption: WideString; AColor: TColor; AFont: TFont): TMustangpeakSketchPadGraphicDoodle;
    function AddTextDoodle(ALeft, ATop: Double; ACaption: WideString; AColor: TColor; AFont: TFont): TMustangpeakSketchPadTextDoodle;
    procedure HideAllDoodles;
    procedure Paint;
    procedure RePaint(Sender: TObject);
    procedure SelectAll;
    procedure ShowAllDoodles;
    procedure UnSelectAll;
  published
    property Border: Integer read FBorder write SetBorder;
    property Color: TColor read FColor write SetColor;
    property Font: TFont read FFont write SetFont;
    property Grid: TMustangpeakSketchPadGrid read FGrid write FGrid;
    property Scale: Double read FScale write SetScale;
  end;

  { TLayoutBuilderPaintBox }

  TLayoutBuilderPaintBox = class(TScrollBox)
  private
    FSketchpad: TMustangpeakSketchPad;
    FUpdateCount: Integer;
  protected
    procedure Paint; override;

    procedure CallbackLayoutWidthChanged(Sender: TObject);
    procedure CallbackLayoutHeightChanged(Sender: TObject);
    procedure CallbackInvalidateRect(Sender: TObject; Rect: PFRect);
    procedure CallbackBeginUpdate(Sender: TObject);
    procedure CallbackEndUpdate(Sender: TObject);

    procedure DoOnResize; override;
    procedure RecalculateScrollbars;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property UpdateCount: Integer read FUpdateCount;

    procedure LockUpdate;
    procedure UnLockUpdate;
  published
    property Sketchpad: TMustangpeakSketchPad read FSketchpad write FSketchpad;
  end;

procedure Register;

implementation

uses
  Math;

procedure Register;
begin

end;

{ TMustangpeakSketchPadGraphicDoodle }

constructor TMustangpeakSketchPadGraphicDoodle.Create(ALeft, ATop, AWidth,
  AHeight: Double; AColor: TColor; ACaption: WideString; AFont: TFont);
begin
  inherited Create(ALeft, ATop, ACaption, AColor, AFont);
  Width := AWidth;
  Height := AHeight;
end;

procedure TMustangpeakSketchPadGraphicDoodle.Draw;
begin
  Canvas.Font.Assign(Font);
  Canvas.Brush.Color := Color;
  Canvas.Rectangle(BoundsScaled);
end;

function TMustangpeakSketchPadGraphicDoodle.PtInObj(Point: TFPoint): Boolean;
begin

end;

{ TMustangpeakSketchPad }

procedure TMustangpeakSketchPad.SetScale(AValue: Double);
var
  i: Integer;
begin
  if AValue > 0 then
  begin
    if FScale=AValue then Exit;
    FScale:=AValue;
    for i := 0 to Doodles.Count - 1 do
      Doodles[i].Scale := Scale;
    DoCallbackLayoutWidthChanged;
    DoCallbackLayoutHeightChanged;
    RePaint(Self);
  end;
end;

procedure TMustangpeakSketchPad.DoCallbackBeginUpdate;
begin
  if Assigned(OnBeginUpdate) then
    OnBeginUpdate(Self);
end;

procedure TMustangpeakSketchPad.DoCallbackEndUpdate;
begin
  if Assigned(OnEndUpdate) then
    OnEndUpdate(Self);
end;

procedure TMustangpeakSketchPad.DoCallbackLayoutWidthChanged;
begin
  if Assigned(OnLayoutWidthChanged) then
    OnLayoutWidthChanged(Self)
end;

procedure TMustangpeakSketchPad.DoCallbackLayoutHeightChanged;
begin
  if Assigned(OnLayoutHeightChanged) then
    OnLayoutHeightChanged(Self)
end;

procedure TMustangpeakSketchPad.DoCallbackInvalidateRect(Sender: TObject; Rect: PFRect);
begin
  if Assigned(OnInvalidateRect) then
    OnInvalidateRect(Sender, Rect)
end;

constructor TMustangpeakSketchPad.Create(AOwner: TComponent; ACanvas: TCanvas);
begin
  inherited Create(AOwner);
  FCanvas := ACanvas;
  FDoodles := TMustangpeakSketchPadDoodles.Create(True);
  FFont := TFont.Create;
  Font.Size := 10;
  FGrid := TMustangpeakSketchPadGrid.Create(Self);
  FScale := 1.0;
  LayoutWidth := 100.0;
  LayoutHeight := 100.0;
end;

destructor TMustangpeakSketchPad.Destroy;
begin
  FreeAndNil(FDoodles);
  FreeAndNil(FFont);
  inherited Destroy;
end;

function TMustangpeakSketchPad.AddGraphicDoodle(ALeft, ATop, AWidth,
  AHeight: Double; ACaption: WideString; AColor: TColor; AFont: TFont
  ): TMustangpeakSketchPadGraphicDoodle;
begin
  Result := TMustangpeakSketchPadGraphicDoodle.Create(ALeft, ATop, AWidth, AHeight, AColor, ACaption, AFont);
  Result.Scale := Scale;
  Result.Canvas := Canvas;
  Doodles.Add(Result);
  Result.OnRepaint := @RePaint;
  RePaint(Self);
end;

function TMustangpeakSketchPad.AddTextDoodle(ALeft, ATop: Double;
  ACaption: WideString; AColor: TColor; AFont: TFont
  ): TMustangpeakSketchPadTextDoodle;
begin
  Result := TMustangpeakSketchPadTextDoodle.Create(ALeft, ATop, ACaption, AColor,  AFont);
  Result.Scale := Scale;
  Result.Canvas := Canvas;
  Doodles.Add(Result);
  Result.OnRepaint := @RePaint;
  RePaint(Self);
end;

procedure TMustangpeakSketchPad.HideAllDoodles;
var
  i: Integer;
begin
  DoCallbackBeginUpdate;
  try
    for i := 0 to Doodles.Count - 1 do
      Doodles[i].Visible := False;
  finally
    DoCallbackEndUpdate;
  end;
end;

procedure TMustangpeakSketchPad.Paint;
begin
  Canvas.Brush.Color := Color;
  Canvas.FillRect(Border, Border, LayoutScaledWidth-1, LayoutScaledHeight-1);
  PaintHorzGridLines;
  PaintVertGridLines;
  PaintDoodles;
end;

function TMustangpeakSketchPad.GetLayoutScaledHeight: Integer;
begin
  Result := Trunc(LayoutHeight * Scale);
    if Result < 1 then
      Result := 1
end;

function TMustangpeakSketchPad.GetLayoutScaledWidth: Integer;
begin
  Result := Trunc(LayoutWidth * Scale);
    if Result < 1 then
      Result := 1
end;

procedure TMustangpeakSketchPad.SetBorder(AValue: Integer);
begin
  if FBorder =AValue then Exit;
  FBorder :=AValue;
  RePaint(Self);
end;

procedure TMustangpeakSketchPad.SetColor(AValue: TColor);
begin
  if FColor=AValue then Exit;
  FColor:=AValue;
  RePaint(Self);
end;

procedure TMustangpeakSketchPad.SetFont(AValue: TFont);
begin
  if Assigned(AValue) then
    Font.Assign(AValue);
end;

procedure TMustangpeakSketchPad.SetLayoutHeight(AValue: Double);
begin
  if FLayoutHeight=AValue then Exit;
  FLayoutHeight:=AValue;
  DoCallbackLayoutHeightChanged;
  RePaint(Self);
end;

procedure TMustangpeakSketchPad.SetLayoutWidth(AValue: Double);
begin
  if FLayoutWidth=AValue then Exit;
  FLayoutWidth:=AValue;
  DoCallbackLayoutWidthChanged;
  RePaint(Self);
end;

procedure TMustangpeakSketchPad.PaintHorzGridLines;
var
  i: Integer;
begin
  i := 0;
  Canvas.Pen.Color := Grid.Color;
  Canvas.Pen.Style := Grid.Style;
  while i <= LayoutScaledHeight do
  begin
    Canvas.Line(0, i, LayoutScaledWidth, i);
    Inc(i, Grid.YScaled);
  end;
  Canvas.Line(0, LayoutScaledHeight, LayoutScaledWidth, LayoutScaledHeight);
end;

procedure TMustangpeakSketchPad.PaintVertGridLines;
var
  i: Integer;
begin
  i := 0;
  Canvas.Pen.Color := Grid.Color;
  Canvas.Pen.Style := Grid.Style;
  while i <= LayoutScaledWidth do
  begin
    Canvas.Line(i, 0, i, LayoutScaledHeight);
    Inc(i, Grid.XScaled);
  end;
  Canvas.Line(LayoutScaledWidth, 0, LayoutScaledWidth, LayoutScaledHeight);
end;

procedure TMustangpeakSketchPad.PaintDoodles;
var
  i: Integer;
begin
  for i := 0 to Doodles.Count - 1 do
  begin
    if Doodles[i].Visible then
      Doodles[i].Draw;
  end;
end;

procedure TMustangpeakSketchPad.RePaint(Sender: TObject);
begin
  DoCallbackInvalidateRect(Sender, nil);
end;

procedure TMustangpeakSketchPad.SelectAll;
var
  i: Integer;
begin
  DoCallbackBeginUpdate;
  try
    for i := 0 to Doodles.Count - 1 do
      Doodles[i].Selected := True;
  finally
    DoCallbackEndUpdate
  end;
end;

procedure TMustangpeakSketchPad.ShowAllDoodles;
var
  i: Integer;
begin
  DoCallbackBeginUpdate;
  try
    for i := 0 to Doodles.Count - 1 do
      Doodles[i].Visible := True;
  finally
    DoCallbackEndUpdate
  end;
end;

procedure TMustangpeakSketchPad.UnSelectAll;
var
  i: Integer;
begin
  DoCallbackBeginUpdate;
  try
    for i := 0 to Doodles.Count - 1 do
      Doodles[i].Selected := False;
  finally
    DoCallbackEndUpdate
  end;
end;

{ TMustangpeakSketchPadDoodles }

function TMustangpeakSketchPadDoodles.Get(Index: Integer): TMustangpeakSketchPadDoodle;
begin
  Result := TMustangpeakSketchPadDoodle( inherited Get(Index))
end;

procedure TMustangpeakSketchPadDoodles.Put(Index: Integer; AValue: TMustangpeakSketchPadDoodle);
begin
  inherited Put(Index, AValue)
end;

{ TMustangpeakSketchPadDoodle }

procedure TMustangpeakSketchPadDoodle.SetColor(AValue: TColor);
begin
  if FColor=AValue then Exit;
  FColor:=AValue;
  DoRepaint;
end;

function TMustangpeakSketchPadDoodle.GetLeftScaled: Integer;
begin
  Result := Trunc(FLeft * Scale);
end;

function TMustangpeakSketchPadDoodle.GetHeightScaled: Integer;
begin
  Result := Trunc(FHeight * Scale)
end;

function TMustangpeakSketchPadDoodle.GetHeight: Double;
begin
  Result := FHeight
end;

function TMustangpeakSketchPadDoodle.GetTopScaled: Integer;
begin
  Result := Trunc(FTop * Scale);
end;

function TMustangpeakSketchPadDoodle.GetWidth: Double;
begin
  Result := FWidth
end;

function TMustangpeakSketchPadDoodle.GetWidthScaled: Integer;
begin
  Result := Trunc(FWidth * Scale);
end;

procedure TMustangpeakSketchPadDoodle.SetCaption(AValue: WideString);
begin
  if FCaption=AValue then Exit;
  FCaption:=AValue;
  DoRepaint;
end;

procedure TMustangpeakSketchPadDoodle.SetFocused(AValue: Boolean);
begin
  if Focused=AValue then Exit;
  Focused:=AValue;
  DoRepaint;
end;

procedure TMustangpeakSketchPadDoodle.SetFont(AValue: TFont);
begin
  Font.Assign(AValue);
  DoRepaint;
end;

procedure TMustangpeakSketchPadDoodle.SetHeight(AValue: Double);
begin
  if FHeight=AValue then Exit;
  FHeight:=AValue;
  DoRepaint;
end;

procedure TMustangpeakSketchPadDoodle.SetLeft(AValue: Double);
begin
  if FLeft=AValue then Exit;
  FLeft:=AValue;
  DoRepaint;
end;

procedure TMustangpeakSketchPadDoodle.SetScale(AValue: Double);
begin
  if FScale=AValue then Exit;
  FScale:=AValue;
  SizeCacheDirty := True;
end;

procedure TMustangpeakSketchPadDoodle.SetSelected(AValue: Boolean);
begin
  if FSelected=AValue then Exit;
  FSelected:=AValue;
  DoRepaint;
end;

procedure TMustangpeakSketchPadDoodle.SetTop(AValue: Double);
begin
  if FTop=AValue then Exit;
  FTop:=AValue;
  DoRepaint;
end;

procedure TMustangpeakSketchPadDoodle.SetVisible(AValue: Boolean);
begin
  if FVisible=AValue then Exit;
  FVisible:=AValue;
  DoRepaint;
end;

procedure TMustangpeakSketchPadDoodle.SetWidth(AValue: Double);
begin
  if FWidth=AValue then Exit;
  FWidth:=AValue;
  DoRepaint;
end;

procedure TMustangpeakSketchPadDoodle.DrawFocusRect;
begin

end;

procedure TMustangpeakSketchPadDoodle.DrawSelectionRect;
begin
  Canvas.Pen.Color := SELECTIONRECTCOLOR;
  Canvas.Pen.Style := SELECTIONRECTPEN;
  Canvas.Frame(BoundsScaled);
end;

procedure TMustangpeakSketchPadDoodle.DoRepaint;
begin
  if Assigned(OnRepaint) then
    OnRepaint(Self);
end;

constructor TMustangpeakSketchPadDoodle.Create(ALeft, ATop: Double;
  ACaption: WideString; AColor: TColor; AFont: TFont);
begin
  inherited Create;
  FFont := TFont.Create;
  Left := ALeft;
  Top := ATop;
  Color := AColor;
  Caption := ACaption;
  if Assigned(Font) then
    Font.Assign(AFont);
  FVisible := True;
end;

destructor TMustangpeakSketchPadDoodle.Destroy;
begin
  FreeAndNil(FFont);
  inherited Destroy;
end;

function TMustangpeakSketchPadDoodle.Bounds: TFRect;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Right := Left + Width;
  Result.Bottom := Top + Height;
end;

function TMustangpeakSketchPadDoodle.BoundsScaled: TRect;
begin
  Result.Left := LeftScaled;
  Result.Top := TopScaled;
  Result.Bottom := TopScaled + HeightScaled;
  Result.Right := LeftScaled + WidthScaled;
end;

function TMustangpeakSketchPadDoodle.TopLeft: TFPoint;
begin
  Result.X := Trunc(Left * Scale);
  Result.Y := Trunc(Top * Scale);
end;

{ TMustangpeakSketchPadTextDoodle }

function TMustangpeakSketchPadTextDoodle.GetHeight: Double;
begin
  if SizeCacheDirty then
  begin
    Canvas.Font.Assign(Font);
    FHeight := Canvas.TextExtent(Caption).cy;
  end;
  Result := FHeight;
end;

function TMustangpeakSketchPadTextDoodle.GetHeightScaled: Integer;
begin
  Result:= Trunc(GetHeight * Scale) + 1;
end;

function TMustangpeakSketchPadTextDoodle.GetWidth: Double;
begin
  if SizeCacheDirty then
  begin
    Canvas.Font.Assign(Font);
    FWidth := Canvas.TextExtent(Caption).cx;
  end;
  Result :=  FWidth;
end;

function TMustangpeakSketchPadTextDoodle.GetWidthScaled: Integer;
begin
  Result := Trunc(GetWidth * Scale) + 1;
end;

procedure TMustangpeakSketchPadTextDoodle.Draw;
begin
  if Assigned(Canvas) then
  begin
    if Font.Size = 0 then Font.Size := 10;
    Canvas.Font.Assign(Font);
    Canvas.Font.Height := Trunc(Canvas.Font.Height * Scale);
    Canvas.Brush.Color := Color;
    if Canvas.Font.Height = 0 then
      Canvas.Font.Height := 1;
    if TransparentBkGnd then
      Canvas.Brush.Style := bsClear
    else
      Canvas.Brush.Style := bsSolid;
    Canvas.TextOut(LeftScaled, TopScaled, String( Caption));
    if Selected then
      DrawSelectionRect;
    if Focused then
      DrawFocusRect;
  end;
end;

function TMustangpeakSketchPadTextDoodle.PtInObj(Point: TFPoint): Boolean;
begin
  Result := False;
end;

{ TMustangpeakSketchPadGrid }

procedure TMustangpeakSketchPadGrid.SetX(AValue: Double);
begin
  if FX=AValue then Exit;
  if AValue < 1 then AValue := 1;
  FX:=AValue;
  TMustangpeakSketchPad(Owner).RePaint(Self);
end;

procedure TMustangpeakSketchPadGrid.SetStyle(AValue: TFPPenStyle);
begin
  if FStyle=AValue then Exit;
  FStyle:=AValue;
  TMustangpeakSketchPad(Owner).RePaint(Self);
end;

procedure TMustangpeakSketchPadGrid.SetColor(AValue: TColor);
begin
  if FColor=AValue then Exit;
  FColor:=AValue;
  TMustangpeakSketchPad(Owner).RePaint(Self);
end;

function TMustangpeakSketchPadGrid.GetXScaled: Integer;
begin
  Result := Trunc(X * TMustangpeakSketchPad(Owner).Scale);
  if Result < 1 then Result := 1
end;

function TMustangpeakSketchPadGrid.GetYScaled: Integer;
begin
  Result := Trunc(Y * TMustangpeakSketchPad(Owner).Scale);
  if Result < 1 then Result := 1
end;

procedure TMustangpeakSketchPadGrid.SetY(AValue: Double);
begin
  if FY=AValue then Exit;
  if AValue < 1 then AValue := 1;
  FY:=AValue;
  TMustangpeakSketchPad(Owner).RePaint(Self);
end;

constructor TMustangpeakSketchPadGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  X := 10.0;
  Y := 10.0;
  Color := clWhite;
end;

{ TLayoutBuilderPaintBox }


procedure TLayoutBuilderPaintBox.Paint;
begin
  inherited Paint;
  Sketchpad.Paint;
end;

procedure TLayoutBuilderPaintBox.RecalculateScrollbars;
begin
  if Sketchpad.LayoutScaledWidth > ClientWidth - (BorderWidth * 2) then
  begin
    HorzScrollBar.Page := 1;
    HorzScrollBar.Range := Sketchpad.LayoutScaledWidth - ClientWidth - (BorderWidth * 2)
  end
  else
    HorzScrollBar.Range := 0;
  if Sketchpad.LayoutScaledHeight > ClientHeight - (BorderWidth * 2) then
  begin
    VertScrollBar.Page := 1;
    VertScrollBar.Range := Sketchpad.LayoutScaledHeight - ClientHeight - (BorderWidth * 2)
  end
  else
    VertScrollBar.Range := 0;
end;

procedure TLayoutBuilderPaintBox.CallbackLayoutWidthChanged(Sender: TObject);
begin
  RecalculateScrollbars;
end;

procedure TLayoutBuilderPaintBox.CallbackLayoutHeightChanged(Sender: TObject);
begin
  RecalculateScrollbars;
end;

procedure TLayoutBuilderPaintBox.CallbackInvalidateRect(Sender: TObject; Rect: PFRect);
begin
  if UpdateCount = 0 then
    Repaint;
end;

procedure TLayoutBuilderPaintBox.CallbackBeginUpdate(Sender: TObject);
begin
  LockUpdate
end;

procedure TLayoutBuilderPaintBox.CallbackEndUpdate(Sender: TObject);
begin
  UnlockUpdate;
end;

constructor TLayoutBuilderPaintBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Sketchpad := TMustangpeakSketchPad.Create(Self, Canvas);
  Sketchpad.OnLayoutHeightChanged := @CallbackLayoutHeightChanged;
  Sketchpad.OnLayoutWidthChanged := @CallbackLayoutWidthChanged;
  Sketchpad.OnInvalidateRect := @CallbackInvalidateRect;
  Sketchpad.OnBeginUpdate := @CallbackBeginUpdate;
  Sketchpad.OnEndUpdate := @CallbackEndUpdate;
  Sketchpad.LayoutWidth := ClientWidth/Sketchpad.Scale;
  Sketchpad.LayoutHeight := ClientHeight/Sketchpad.Scale;
  FUpdateCount := 0;
  HorzScrollBar.Tracking := True;
  VertScrollBar.Tracking := True;
  Color := clBlack
end;

destructor TLayoutBuilderPaintBox.Destroy;
begin
  FreeAndNil(FSketchpad);
  inherited Destroy;
end;

procedure TLayoutBuilderPaintBox.DoOnResize;
begin
  inherited DoOnResize;
  RecalculateScrollbars;
end;

procedure TLayoutBuilderPaintBox.LockUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TLayoutBuilderPaintBox.UnLockUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount < 0 then FUpdateCount := 0;
  if FUpdateCount = 0 then
    Repaint;
end;


end.

