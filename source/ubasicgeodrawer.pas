unit uBasicGeoDrawer;

{$mode ObjFPC}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, Math, GraphUtil, Graphics;

type
  TDblPoint = record
    X,Y: Double;
    function Point: TPoint;
  end;

  TDblPoints = record
    Data: array of TDblPoint;
    procedure Init(ACount: Integer);
    function Rotate(Angle, CenterX, CenterY: Double): TDblPoints;
    function Scale(AFactorX, AFactorY: Double): TDblPoints;
    function Translate(dx, dy: Double): TDblPoints;
  end;

function DblPoint(X, Y: Double): TDblPoint;

type
  TBasicGeoDrawer = class
  protected
    function GetFillColor: TColor; virtual; abstract;
    function GetFillOpacity: Double; virtual; abstract;
    function GetStrokeColor: TColor; virtual; abstract;
    function GetStrokeOpacity: Double; virtual; abstract;
    function GetStrokeWidth: Double; virtual; abstract;
    procedure SetFillColor(AValue: TColor); virtual; abstract;
    procedure SetFillOpacity(AValue: Double); virtual; abstract;
    procedure SetStrokeColor(AValue: TColor); virtual; abstract;
    procedure SetStrokeOpacity(AValue: Double); virtual; abstract;
    procedure SetStrokeWidth(AValue: Double); virtual; abstract;

  public
    constructor Create; virtual; abstract;
    procedure Circle(ACenterX, ACenterY, ARadius: Double); virtual; abstract;
    procedure DrawToCanvas(ACanvas: TCanvas; AWidth, AHeight: Integer); virtual; abstract;
    procedure Init(AWidth, AHeight: Double; ABkColor: TColor); virtual; abstract;
    procedure PolyBezier(APts: TDblPoints); virtual; abstract;
    procedure Polygon(APts: TDblPoints); virtual; abstract;
    procedure Rectangle(ALeft, ATop, AWidth, AHeight: Double); virtual; abstract;
    procedure SaveToFile(AFileName: String; AWidth, AHeight: Integer); virtual; abstract;
    procedure Square(ALeft, ATop, ASize: Double); virtual; abstract;

    property FillColor: TColor read GetFillColor write SetFillColor;
    property FillOpacity: Double read GetFillOpacity write SetFillOpacity;
    property StrokeColor: TColor read GetStrokeColor write SetStrokeColor;
    property StrokeOpacity: Double read GetStrokeOpacity write SetStrokeOpacity;
    property StrokeWidth: Double read GetStrokeWidth write SetStrokeWidth;
  end;

  TGeoDrawerClass = class of TBasicGeoDrawer;

implementation

{ TDblPoint }

function TDblPoint.Point: TPoint;
begin
  Result.X := round(X);
  Result.Y := round(Y);
end;

function DblPoint(X, Y: Double): TDblPoint;
begin
  Result.X := X;
  Result.Y := Y;
end;


{ TDblPoints }

procedure TDblPoints.Init(ACount: Integer);
begin
  SetLength(Data, ACount);
end;

// Angle in degrees, positive in counter-clockwise direction (opposite to svg!)
function TDblPoints.Rotate(Angle, CenterX, CenterY: Double): TDblPoints;
var
  sa, ca: Double;
  j: Integer;
begin
  SinCos(DegToRad(Angle), sa, ca);
  Result.Init(Length(Data));
  for j := 0 to Length(Data)-1 do
  begin
    Result.Data[j].X :=  ca * (Data[j].X - CenterX) + sa * (Data[j].Y - CenterY) + CenterX;
    Result.Data[j].Y := -sa * (Data[j].X - CenterX) + ca * (Data[j].Y - CenterY) + CenterY;
  end;
end;

function TDblPoints.Scale(AFactorX, AFactorY: Double): TDblPoints;
var
  j: Integer;
begin
  Result.Init(Length(Data));
  for j := 0 to Length(Data)-1 do
    Result.Data[j] := DblPoint(Data[j].X * AFactorX, Data[j].Y * AFactorY);
end;

function TDblpoints.Translate(dx, dy: Double): TDblPoints;
var
  j: Integer;
begin
  Result.Init(Length(Data));
  for j := 0 to Length(Data)-1 do
    Result.Data[j] := DblPoint(Data[j].X + dx, Data[j].Y + dy);
end;

end.

