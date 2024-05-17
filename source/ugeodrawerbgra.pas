unit uGeoDrawerBGRA;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, Graphics,
  BGRABitmap, BGRABitmapTypes, BGRACanvas,
  uBasicGeoDrawer;

type
  TGeoDrawerBGRA = class(TBasicGeoDrawer)
  private
    FBitmap: TBGRABitmap;
  protected
    function GetFillColor: TColor; override;
    function GetFillOpacity: Double; override;
    function GetStrokeColor: TColor; override;
    function GetStrokeOpacity: Double; override;
    function GetStrokeWidth: Double; override;
    procedure SetFillColor(AValue: TColor); override;
    procedure SetFillOpacity(AValue: Double); override;
    procedure SetStrokeColor(AValue: TColor); override;
    procedure SetStrokeOpacity(AValue: Double); override;
    procedure SetStrokeWidth(AValue: Double); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Circle(ACenterX, ACenterY, ARadius: Double); override;
    procedure DrawToCanvas(ACanvas: TCanvas; AWidth, AHeight: Integer); override;
    procedure Init(AWidth, AHeight: Double; ABkColor: TColor); override;
    procedure PolyBezier(APts: TDblPoints); override;
    procedure Polygon(APts: TDblPoints); override;
    procedure Rectangle(ALeft, ATop, AWidth, AHeight: Double); override;
    procedure SaveToFile(AFileName: String; AWidth, AHeight: Integer); override;
    procedure Square(ALeft, ATop, ASize: Double); override;
  end;

implementation

constructor TGeodrawerBGRA.Create;
begin
  FBitmap := TBGRABitmap.Create;
end;

destructor TGeoDrawerBGRA.Destroy;
begin
  FBitmap.Free;
  inherited;
end;

procedure TGeoDrawerBGRA.Circle(ACenterX, ACenterY, ARadius: Double);
begin
  FBitmap.CanvasBGRA.Ellipse(
    round(ACenterX - ARadius),
    round(ACenterY - ARadius),
    round(ACenterX + ARadius),
    round(ACenterY + ARadius)
  );
end;

procedure TGeoDrawerBGRA.DrawToCanvas(ACanvas: TCanvas; AWidth, AHeight: Integer);
var
  x, y: Integer;
begin
  y := 0;
  while y < AHeight do
  begin
    x := 0;
    while x < AWidth do
    begin
      FBitmap.Draw(ACanvas, X, Y);
      inc(x, FBitmap.Width);
    end;
    inc(y, FBitmap.Height);
  end;
end;

function TGeoDrawerBGRA.GetFillColor: TColor;
begin
  if FBitmap.CanvasBGRA.Brush.Style = bsClear then
    Result := clNone
  else
    Result := FBitmap.CanvasBGRA.Brush.Color;
end;

function TGeoDrawerBGRA.GetFillOpacity: double;
begin
  Result := FBitmap.CanvasBGRA.Brush.Opacity / 255;
end;

function TGeoDrawerBGRA.GetStrokeColor: TColor;
begin
  if FBitmap.CanvasBGRA.Pen.Style = psClear then
    Result := clNone
  else
    Result := FBitmap.CanvasBGRA.Pen.Color;
end;

function TGeoDrawerBGRA.GetStrokeOpacity: double;
begin
  Result := FBitmap.CanvasBGRA.Pen.Opacity / 255;
end;

function TGeoDrawerBGRA.GetStrokeWidth: double;
begin
  Result := FBitmap.CanvasBGRA.Pen.Width;
end;

procedure TGeoDrawerBGRA.Init(AWidth, AHeight: Double; ABkColor: TColor);
begin
  FBitmap.SetSize(round(AWidth), round(AHeight));
  FBitmap.Fill(ABkColor);
//  FBitmap.GradientFill(0, 0, round(AWidth), round(AHeight), ABkColor, Random($FFFFFF), gtLinear, PointF(0,0), PointF(100,100), dmSet);
end;

procedure TGeoDrawerBGRA.PolyBezier(APts: TDblPoints);
var
  P: array of TPoint = nil;
  j: Integer;
begin
  SetLength(P, Length(APts.Data));
  for j := 0 to High(APts.Data) do
    P[j] := APts.Data[j].Point;
  FBitmap.CanvasBGRA.PolyBezier(P);
end;

procedure TGeoDrawerBGRA.Polygon(APts: TDblPoints);
var
  P: array of TPoint = nil;
  j: Integer;
begin
  SetLength(P, Length(APts.Data));
  for j := 0 to High(APts.Data) do
    P[j] := APts.Data[j].Point;
  FBitmap.CanvasBGRA.Polygon(P);
end;

procedure TGeoDrawerBGRA.Rectangle(ALeft, ATop, AWidth, AHeight: Double);
begin
  FBitmap.CanvasBGRA.Rectangle(
    round(ALeft),
    round(ATop),
    round(ALeft + AWidth),
    round(ATop + AHeight)
  );
end;

procedure TGeoDrawerBGRA.SetFillColor(AValue: TColor);
begin
  if AValue = clNone then
    FBitmap.CanvasBGRA.Brush.Style := bsClear
  else
  begin
    FBitmap.CanvasBGRA.Brush.Style := bsSolid;
    FBitmap.CanvasBGRA.Brush.Color := AValue;
  end;
end;

procedure TGeoDrawerBGRA.SetFillOpacity(AValue: Double);
begin
  FBitmap.CanvasBGRA.Brush.Opacity := round(AValue * 255);
end;

procedure TGeoDrawerBGRA.SetStrokeColor(AValue: TColor);
begin
  if AValue = clNone then
    FBitmap.CanvasBGRA.Pen.Style := psClear
  else
  begin
    FBitmap.CanvasBGRA.Pen.Color := AValue;
    FBitmap.CanvasBGRA.Pen.Style := psSolid;
  end;
end;

procedure TGeoDrawerBGRA.SetStrokeOpacity(AValue: Double);
begin
  FBitmap.CanvasBGRA.Pen.Opacity := round(AValue * 255);
end;

procedure TGeoDrawerBGRA.SetStrokeWidth(AValue: Double);
begin
  FBitmap.CanvasBGRA.Pen.Width := round(AValue);
end;

procedure TGeoDrawerBGRA.Square(ALeft, ATop, ASize: Double);
begin
  Rectangle(ALeft, ATop, ASize, ASize);
end;

procedure TGeoDrawerBGRA.SaveToFile(AFileName: String; AWidth, AHeight:Integer);
var
  bmp: TBGRABitmap;
begin
  if AWidth <= 0 then
    FBitmap.SaveToFile(AFileName)
  else
  begin
    bmp := TBGRABitmap.Create(AWidth, AHeight);
    try
      bmp.Fill(FBitmap);
      bmp.SaveToFile(AFileName);
    finally
      bmp.Free;
    end;
  end;
end;

end.

