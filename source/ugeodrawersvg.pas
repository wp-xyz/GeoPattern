unit uGeoDrawerSVG;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, uBasicGeoDrawer;

type
  TGeoDrawerSVG = class(TBasicGeoDrawer)
  private
    FLines: TStrings;
    FPointSettings: TFormatSettings;
    FFillColor: TColor;
    FFillOpacity: Double;
    FStrokeColor: TColor;
    FStrokeOpacity: Double;
    FStrokeWidth: Double;
    function GetFillColorStr: String;
    function GetFillOpacityStr: String;
    function GetStrokeColorStr: String;
    function GetStrokeOpacityStr: String;
    function GetStrokeWidthStr: String;
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
    procedure Init(AWidth, AHeight: Double; ABackColor: TColor); override;
    procedure PolyBezier(APts: TDblPoints); override;
    procedure Polygon(APts: TDblPoints); override;
    procedure Rectangle(ALeft, ATop, AWidth, AHeight: Double); override;
    procedure SaveToFile(AFileName: String; AWidth, AHeight: Integer); override;
    procedure Square(ALeft, ATop, ASize: Double); override;
  end;


implementation

function ColorToStr(AColor: TColor): String;
begin
  Result := Format('#%.2x%.2x%.2x', [Red(AColor), Green(AColor), Blue(AColor)]);
end;

function StrToColor(AColor: String): TColor;
var
  r, g, b: Byte;
begin
  r := StrToInt('$' + AColor[2] + AColor[3]);
  g := StrToInt('$' + AColor[4] + AColor[5]);
  b := StrToInt('$' + AColor[6] + AColor[7]);
  Result := r + g shl 8 + b shl 16;
end;

constructor TGeoDrawerSVG.Create;
begin
  FLines := TStringList.Create;

  FPointSettings := DefaultFormatSettings;
  FPointSettings.DecimalSeparator := '.';

  FFillColor := clBlack;
  FFillOpacity := 1.0;
  FStrokeColor := clBlack;
  FStrokeOpacity := 1.0;
  FStrokeWidth := 1.0;
end;

destructor TGeoDrawerSVG.Destroy;
begin
  FLines.Free;
end;

procedure TGeoDrawerSVG.Circle(ACenterX, ACenterY, ARadius: Double);
begin
  FLines.Add(Format(
    '  <circle cx="%.3g" cy="%.3g" r="%.3g" %s%s%s%s%s/>',
      [ACenterX, ACenterY, ARadius,
       GetFillColorStr, GetFillOpacityStr, GetStrokeColorStr, GetStrokeOpacityStr, GetStrokeWidthStr],
    FPointSettings
  ));
end;

procedure TGeoDrawerSVG.DrawToCanvas(ACanvas: TCanvas; AWidth, AHeight: Integer);
begin
  raise Exception.Create('DrawToCanvas not supported by TGeoDrawerSVG');
end;

function TGeoDrawerSVG.GetFillColor: TColor;
begin
  Result := FFillColor;
end;

function TGeoDrawerSVG.GetFillColorStr: String;
begin
  if FFillColor = clNone then
    Result := 'fill="none" '
  else
    Result := Format('fill="%s" ', [ColorToStr(FFillColor)]);
end;

function TGeoDrawerSVG.GetFillOpacity: Double;
begin
  Result := FFillOpacity;
end;

function TGeoDrawerSVG.GetFillOpacityStr: String;
begin
  if FFillColor = clNone then
    Result := ''
  else
    Result := Format('fill-opacity="%.3g" ', [FFillOpacity], FPointSettings);
end;

function TGeoDrawerSVG.GetStrokeColor: TColor;
begin
  Result := FStrokeColor;
end;

function TGeoDrawerSVG.GetStrokeColorStr: String;
begin
  if FStrokeColor = clNone then
    Result := 'stroke="none" '
  else
    Result := Format('stroke="%s" ', [ColorToStr(FStrokeColor)]);
end;

function TGeoDrawerSVG.GetStrokeOpacity: Double;
begin
  Result := FStrokeOpacity;
end;

function TGeoDrawerSVG.GetStrokeOpacityStr: String;
begin
  if FStrokeColor = clNone then
    Result := ''
  else
    Result := Format('stroke-opacity="%.3g" ', [FStrokeOpacity], FPointSettings);
end;

function TGeoDrawerSVG.GetStrokeWidth: Double;
begin
  Result := FStrokeWidth;
end;

function TGeoDrawerSVG.GetStrokeWidthStr: String;
begin
  if FStrokeColor = clNone then
    Result := ''
  else
    Result := Format('stroke-width="%g" ', [FStrokeWidth]);
end;

procedure TGeoDrawerSVG.Init(AWidth, AHeight: Double; ABackColor: TColor);
begin
  FLines.Clear;
  FLines.Add('<svg width="%.3g" height="%.3g" xmlns="http://www.w3.org/2000/svg">',
    [AWidth, AHeight]);
  FLines.Add('  <rect x="0" y="0" width="100%%" height="100%%" fill="%s" />', [ColorToStr(ABackColor)]);
end;

procedure TGeoDrawerSVG.PolyBezier(APts: TDblPoints);
var
  pts: String;
  i, n: Integer;
begin
  n := Length(APts.Data);
  if n mod 4 <> 0 then
    raise Exception.Create('Point count in PolyBezier must be a multiple of 4.');

  pts := '';
  i := 0;
  while i < n-1 do
  begin
    pts := pts + Format(' M%.3g %.3g C%.3g %.3g, %.3g %.3g, %.3g %.3g',
    [
      APts.Data[i].X,   APts.Data[i].Y,
      APts.Data[i+1].X, APts.Data[i+1].Y,
      APts.Data[i+2].X, APts.Data[i+2].Y,
      APts.Data[i+3].X, APts.Data[i+3].Y
    ], FPointSettings);
    inc(i, 4);
  end;
  Delete(pts, 1, 1);
  FLines.Add(Format('  <path d="%s" %s%s%s%s%s/>',
    [pts,
     GetFillColorStr, GetFillOpacityStr, GetStrokeColorStr, GetStrokeOpacityStr, GetStrokeWidthStr
    ]
  ));
end;

procedure TGeoDrawerSVG.Polygon(APts: TDblPoints);
var
  pts: String;
  i: Integer;
begin
  pts := '';
  for i := 0 to Length(APts.Data)-1 do
    pts := Format('%s, %.3g, %.3g', [pts, APts.Data[i].X, APts.Data[i].Y], FPointSettings);
  Delete(pts, 1, 2);   // Delete leading ", "
  FLines.Add(Format('  <polyline points="%s" %s%s%s%s%s/>',
    [pts,
     GetFillColorStr, GetFillOpacityStr, GetStrokeColorStr, GetStrokeOpacityStr, GetStrokeWidthStr
    ]));
end;

procedure TGeoDrawerSVG.Rectangle(ALeft, ATop, AWidth, AHeight: Double);
begin
  FLines.Add(Format(
    '  <rect x="%.3g" y="%.3g" width="%.3g" height="%.3g" %s%s%s%s%s/>',
    [ALeft, ATop, AWidth, AHeight,
     GetFillColorStr, GetFillOpacityStr, GetStrokeColorStr, GetStrokeOpacityStr, GetStrokeWidthStr
    ],
    FPointSettings
  ));
end;

procedure TGeoDrawerSVG.SaveToFile(AFileName: String; AWidth, AHeight: Integer);
begin
  FLines.Add('</svg>');
  FLines.SaveToFile(AFileName)
end;

procedure TGeoDrawerSVG.SetFillColor(AValue: TColor);
begin
  FFillColor := AValue;
end;

procedure TGeoDrawerSVG.SetFillOpacity(AValue: Double);
begin
  FFillOpacity := AValue;
end;

procedure TGeoDrawerSVG.SetStrokeColor(AValue: TColor);
begin
  FStrokeColor := AValue;
end;

procedure TGeoDrawerSVG.SetStrokeOpacity(AValue: Double);
begin
  FStrokeOpacity := AValue;
end;

procedure TGeoDrawerSVG.SetStrokeWidth(AValue: Double);
begin
  FStrokeWidth := AValue;
end;

procedure TGeoDrawerSVG.Square(ALeft, ATop, ASize: Double);
begin
  Rectangle(ALeft, ATop, ASize, ASize);
end;

end.

