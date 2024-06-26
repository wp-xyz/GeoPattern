unit uGeoPatterns;

{$mode ObjFPC}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, fgl, Math, GraphUtil, Graphics,
  uBasicGeoDrawer;

type
  TGeoPatternType = (
    ptHexagons, ptOverlappingCircles, ptSquares, ptOctogons,
    ptTriangles, ptOverlappingRings, ptConcentricCircles, ptNestedSquares,
    ptMosaicSquares, ptSineWaves, ptPlusSigns, ptXes,
    ptChevrons, ptDiamonds, ptPlaid, ptTesselation,
    ptPackedCircles, ptPietMondrian
  );

  TGeoPattern = class;

  TGeoPatternOptions = record
    OverridePatternType: Boolean;
    OverrideBaseColor: Boolean;
    OverrideFillColor1: Boolean;
    OverrideFillColor2: Boolean;
    OverrideMaxFillOpacity: Boolean;
    OverrideMinFillOpacity: Boolean;
    OverrideStrokeColor: Boolean;
    OverrideStrokeOpacity: Boolean;
    PatternType: TGeoPatternType;
    BaseColor: TColor;
    FillColor1: TColor;
    FillColor2: TColor;
    StrokeColor: TColor;
    MaxFillOpacity: Double;
    MinFillOpacity: Double;
    StrokeOpacity: Double;
    procedure Init;
  end;

  TGeoPattern = class
  private
    FGenerator: String;
    FHash: String;
    FDrawer: TBasicGeoDrawer;
    FBkColor: TColor;
    FWidth, FHeight: Integer;
    FTileWidth, FTileHeight: Double;
    FOptions: TGeoPatternOptions;
    FPatternType: TGeoPatternType;

    function BuildPlusShape(ASquareSize: Double): TDblPoints;

    function BaseColor: TColor;
    function FillColor(AValue: Integer): TColor;
    function FillOpacity(AValue: Integer): Double;
    function StrokeColor: TColor;
    function StrokeOpacity: Double;

  protected
    procedure GenerateHexagons;
    procedure GenerateOverlappingCircles;
    procedure GenerateSquares;
    procedure GenerateOctogons;
    procedure GenerateTriangles;
    procedure GenerateOverlappingRings;
    procedure GenerateConcentricCircles;
    procedure GenerateNestedSquares;
    procedure GenerateMosaicSquares;
    procedure GenerateSineWaves;
    procedure GeneratePlusSigns;
    procedure GenerateXes;
    procedure GenerateChevrons;
    procedure GenerateDiamonds;
    procedure GeneratePlaid;
    procedure GenerateTesselation;
    procedure GeneratePackedCircles;
    procedure GeneratePietMondrian;

    procedure GenerateBackground;
    procedure GeneratePattern;

  public
    constructor Create(AString: String; AWidth, AHeight: Integer;
      ADrawerClass: TGeoDrawerClass; AOptions: TGeoPatternOptions);
    destructor Destroy; override;
    procedure DrawToCanvas(ACanvas: TCanvas);
    class procedure GetPatternList(AList: TStrings);
    procedure SaveToFile(AFileName: String);
    property PatternType: TGeoPatternType read FPatternType;
  end;

implementation

uses
  sha1, TypInfo;

const
  BASE_COLOR = TColor($3c3c93);        // '#933c3c'
  FILL_COLOR_DARK = TColor($020202);   // '#222'
  FILL_COLOR_LIGHT = TColor($0d0d0d);  // '#ddd'
  OPACITY_MIN = 0.02;
  OPACITY_MAX = 0.35;
  STROKE_COLOR = clBlack;
  STROKE_OPACITY = 0.02;

function Map(value, vMin, vMax, dMin, dMax: Double): Double;
begin
  if vMax <> vMin then
    Result := dMin + (value - vMin) * (dMax - dMin) / (vMax - vMin)
  else
    Result := vMax;
end;

function HexVal(AHash: String; AIndex: Integer; ALength: Integer = 1): Integer;
var
  s: String;
begin
  AIndex := AIndex mod Length(AHash);
  s := Copy(AHash, AIndex + 1, ALength);
  Result := StrToInt('$'+s);
end;


{ TGeoPatternOptions }

procedure TGeoPatternOptions.Init;
begin
  OverridePatternType := false;
  OverrideBaseColor := false;
  OverrideFillColor1 := false;
  OverrideFillColor2 := false;
  OverrideStrokeColor := false;
  OverrideMaxFillOpacity := false;
  OverrideMinFillOpacity := false;
  OverrideStrokeOpacity := false;
  PatternType := Low(TGeoPatternType);
  BaseColor := BASE_COLOR;
  FillColor1 := FILL_COLOR_LIGHT;
  FillColor2 := FILL_COLOR_DARK;
  StrokeColor := STROKE_COLOR;
  MaxFillOpacity := OPACITY_MAX;
  MinFillOpacity := OPACITY_MIN;
  StrokeOpacity := STROKE_OPACITY;
end;


{ TGeoPattern

  Adapted from https://github.com/azoyan/geopattern/blob/main/geopattern/lib.lua
}

constructor TGeoPattern.Create(AString: String; AWidth, AHeight: Integer;
  ADrawerClass: TGeoDrawerClass; AOptions: TGeoPatternOptions);
begin
  inherited Create;
  FWidth := AWidth;
  FHeight := AHeight;
  FDrawer := ADrawerClass.Create;
  FOptions := AOptions;
  FGenerator := AString;
  FHash := SHA1Print(SHA1String(FGenerator));
  GenerateBackground;
  GeneratePattern;
end;

destructor TGeoPattern.Destroy;
begin
  FDrawer.Free;
  inherited;
end;

function TGeoPattern.BaseColor: TColor;
begin
  if FOptions.OverrideBaseColor then
    Result := FOptions.BaseColor
  else
    Result := BASE_COLOR;
end;

function TGeoPattern.FillColor(AValue: Integer): TColor;
var
  color1, color2: TColor;
begin
  if FOptions.OverrideFillColor1 then color1 := FOptions.FillColor1 else color1 := FILL_COLOR_LIGHT;
  if FOptions.OverrideFillColor2 then color2 := FOptions.FillColor2 else color2 := FILL_COLOR_DARK;

  if AValue mod 2 = 0 then
    Result := color1
  else
    Result := color2;
end;

function TGeoPattern.FillOpacity(AValue: Integer): Double;
var
  mn, mx: Double;
begin
  if FOptions.OverrideMaxFillOpacity then mx :=FOptions.MaxFillOpacity else mx := OPACITY_MAX;
  if FOptions.OverrideMinFillOpacity then mn :=FOptions.MinFillOpacity else mn := OPACITY_MIN;
  Result := Map(AValue, 0, 15, mn, mx);
end;

function TGeoPattern.StrokeColor: TColor;
begin
  if FOptions.OverrideStrokeColor then
    Result := FOptions.StrokeColor
  else
    Result := STROKE_COLOR;
end;

function TGeoPattern.StrokeOpacity: Double;
begin
  if FOptions.OverrideStrokeOpacity then
    Result := FOptions.StrokeOpacity
  else
    Result := STROKE_OPACITY;
end;

function TGeoPattern.BuildPlusShape(ASquareSize: Double): TDblPoints;
begin
  Result.Init(13);
  Result.Data[0] := DblPoint(0, ASquareSize);
  Result.Data[1] := DblPoint(ASquareSize, ASquareSize);
  Result.Data[2] := DblPoint(ASquareSize, 0);
  Result.Data[3] := DblPoint(ASquareSize*2, 0);
  Result.Data[4] := DblPoint(ASquareSize*2, ASquareSize);
  Result.Data[5] := DblPoint(ASquareSize*3, ASquareSize);
  Result.Data[6] := DblPoint(ASquareSize*3, ASquareSize*2);
  Result.Data[7] := DblPoint(ASquareSize*2, ASquareSize*2);
  Result.Data[8] := DblPoint(ASquareSize*2, ASquareSize*3);
  Result.Data[9] := DblPoint(ASquareSize, ASquareSize*3);
  Result.Data[10] := DblPoint(ASquareSize, ASquareSize*2);
  Result.Data[11] := DblPoint(0, ASquareSize*2);
  Result.Data[12] := DblPoint(0, ASquareSize);
end;


procedure TGeoPattern.DrawToCanvas(ACanvas: TCanvas);
var
  w, h: Double;
begin
  if FGenerator <> '' then
  begin
    if FWidth = 0 then w := FTileWidth else w := FWidth;
    if FHeight = 0 then h := FTileHeight else h := FHeight;
    FDrawer.DrawToCanvas(ACanvas, round(w), round(h));
  end;
end;

procedure TGeoPattern.SaveToFile(AFileName: String);
var
  w, h: Double;
begin
  if FWidth = 0 then w := FTileWidth else w := FWidth;
  if FHeight = 0 then h := FTileHeight else h := FHeight;
  FDrawer.SaveToFile(AFileName, round(w), round(h));
end;

class procedure TGeoPattern.GetPatternList(AList: TStrings);
var
  pt: TGeoPatternType;
  s: String;
  i: Integer;
begin
  AList.BeginUpdate;
  try
    AList.Clear;
    for pt in TGeoPatternType do
    begin
      s := GetEnumName(TypeInfo(TGeoPatternType), Integer(pt));
      Delete(s, 1, 2);  // Remove the 'pt'
      for i := Length(s) downto 2 do
        if s[i] in ['A'..'Z'] then Insert(' ', s, i);
      AList.Add(s);
    end;
  finally
    AList.EndUpdate;
  end;
end;

procedure TGeoPattern.GenerateBackground;
var
  hue: Integer;
  hueOffset: Integer;
  satOffset: Integer;
  H, L, S: Byte;
begin
  ColorToHLS(BaseColor, H, L, S);

  if not FOptions.OverrideBaseColor then
  begin
    hue := HexVal(FHash, 14, 3);
    hueOffset := round(Map(hue, 0, 4095, 0, 255));
    satOffset := HexVal(FHash, 17);

    H := Byte((H * 256 - hueOffset) mod 256);
    if satOffset mod 2 = 0 then
      S := Min(255, Byte(S * 256 + satOffset))
    else
      S := Max(0, Byte(S * 256 - satOffset));
  end;

  FBkColor := HLSToColor(H, L, S);
end;

procedure TGeoPattern.GeneratePattern;
var
  idx, n: Integer;
begin
  if FOptions.OverridePatternType then
    idx := ord(FOptions.PatternType)
  else
    idx := HexVal(FHash, 0, 2);
  n := ord(High(TGeoPatternType)) + 1;
  FPatternType := TGeoPatternType(idx mod n);
  case FPatternType of
    ptHexagons: GenerateHexagons;
    ptOverlappingCircles: GenerateOverlappingCircles;
    ptSquares: GenerateSquares;
    ptOctogons: GenerateOctogons;
    ptTriangles: GenerateTriangles;
    ptOverlappingRings: GenerateOverlappingRings;
    ptConcentricCircles: GenerateConcentricCircles;
    ptNestedSquares: GenerateNestedSquares;
    ptMosaicSquares: GenerateMosaicSquares;
    ptSineWaves: GenerateSineWaves;
    ptPlusSigns: GeneratePlusSigns;
    ptXes: GenerateXes;
    ptChevrons: GenerateChevrons;
    ptDiamonds: GenerateDiamonds;
    ptPlaid: GeneratePlaid;
    ptTesselation: GenerateTesselation;
    ptPackedCircles: GeneratePackedCircles;
    ptPietMondrian: GeneratePietMondrian;
  end;
end;

procedure TGeoPattern.GenerateHexagons;

  function BuildHexagonShape(ASideLength: Double): TDblPoints;
  var
    a, b, c: Double;
  begin
    c := ASideLength;
    a := c / 2;
    b := sin(DegToRad(60)) * c;
    Result.Init(7);
    Result.Data[0] := DblPoint(0,   b  );
    Result.Data[1] := DblPoint(a,   0  );
    Result.Data[2] := DblPoint(a+c, 0  );
    Result.Data[3] := DblPoint(2*c, b  );
    Result.Data[4] := DblPoint(a+c, 2*b);
    Result.Data[5] := DblPoint(a,   2*b);
    Result.Data[6] := DblPoint(0,   b  );
  end;

var
  sideLength: Integer;
  hexHeight: Integer;
  hexWidth: Integer;
  hex: TDblPoints;
  dy: Double;
  i, x, y: Integer;
  value: Integer;
begin
  sideLength := round(Map(HexVal(FHash, 0), 0, 15, 8, 60));
  hexHeight := round(sideLength * sqrt(3.0));
  hexWidth := round(sideLength * 2);
  hex := BuildHexagonShape(sideLength);

  FTileWidth := hexWidth * 3 + sideLength * 3;
  FTileHeight := hexHeight * 6;

  FDrawer.Init(FTileWidth, FTileHeight, FBkColor);
  FDrawer.StrokeColor := StrokeColor;
  FDrawer.StrokeOpacity := StrokeOpacity;

  i := 0;
  for y := 0 to 5 do
    for x := 0 to 5 do
    begin
      if x mod 2 = 0 then
        dy := y * hexHeight
      else
        dy := y * hexHeight + hexHeight / 2;
      value := HexVal(FHash, i);
      FDrawer.FillColor := FillColor(value);
      FDrawer.FillOpacity := FillOpacity(value);

      FDrawer.Polygon(hex.Translate(x * sideLength * 1.5 - hexWidth / 2, dy - hexHeight / 2));

      if x = 0 then
        FDrawer.Polygon(hex.Translate(6 * sideLength * 1.5 - hexWidth / 2, dy - hexHeight / 2));

      if y = 0 then
      begin
        if x mod 2 = 0 then
          dy := 6 * hexHeight
        else
          dy := 6 * hexHeight + hexHeight / 2;
        FDrawer.Polygon(hex.Translate(x * sideLength * 1.5 - hexWidth / 2, dy - hexHeight / 2));
      end;

      if (x = 0) and (y = 0) then
        FDrawer.Polygon(hex.Translate(6 * sideLength * 1.5 - hexWidth / 2, 5 * hexHeight + hexHeight / 2));

      inc(i);
    end;
end;

procedure TGeoPattern.GenerateOverlappingCircles;
var
  scale: Integer;
  diameter: Double;
  radius: Double;
  value: Integer;
  i, x, y: Integer;
begin
  scale := HexVal(FHash, 0);
  diameter := Map(scale, 0, 15, 25, 200);
  radius := diameter / 2;

  FTileWidth := radius*6;
  FTileHeight := radius*6;

  FDrawer.Init(FTileWidth, FTileHeight, FBkColor);
  FDrawer.StrokeColor := clNone;   // no stroke

  i := 0;
  for y := 0 to 5 do
    for x := 0 to 5 do
    begin
      value := HexVal(FHash, i);
      FDrawer.FillColor := FillColor(value);
      FDrawer.FillOpacity := FillOpacity(value);
      FDrawer.Circle(x*radius, y*radius, radius);
      if (x = 0) then
        FDrawer.Circle(6*radius, y*radius, radius);
      if (y = 0) then
        FDrawer.Circle(x*radius, 6*radius, radius);
      if (x = 0) and (y = 0) then
        FDrawer.Circle(6*radius, 6*radius, radius);
      inc(i);
    end;
end;

procedure TGeoPattern.GenerateSquares;
var
  scale: Integer;
  squareSize: Double;
  value: Integer;
  i, x, y: Integer;
begin
  scale := HexVal(FHash, 0);
  squareSize := Map(scale, 0, 15, 10, 60);

  FTileWidth := squareSize*6;
  FTileHeight := squareSize*6;
  FDrawer.Init(FTileWidth, FTileHeight, FBkColor);
  FDrawer.StrokeColor := StrokeColor;
  FDrawer.StrokeOpacity := StrokeOpacity;

  i := 0;
  for y := 0 to 5 do
    for x := 0 to 5 do
    begin
      value := HexVal(FHash, i);
      FDrawer.FillColor := FillColor(value);
      FDrawer.FillOpacity := FillOpacity(value);
      FDrawer.Square(x*squareSize, y*squareSize, squareSize);
      inc(i);
    end;
end;

procedure TGeoPattern.GenerateOctogons;

  function BuildOctogonShape(ASquareSize: Double): TDblPoints;
  var
    s, c: Double;
  begin
    s := ASquareSize;
    c := s / 3;
    Result.Init(9);
    Result.Data[0] := DblPoint(c,     0    );
    Result.Data[1] := DblPoint(s - c, 0    );
    Result.Data[2] := DblPoint(s,     c    );
    Result.Data[3] := DblPoint(s,     s - c);
    Result.Data[4] := DblPoint(s - c, s    );
    Result.Data[5] := DblPoint(c,     s    );
    Result.Data[6] := DblPoint(0,     s - c);
    Result.Data[7] := DblPoint(0,     c    );
    Result.Data[8] := DblPoint(c,     0    );
  end;

var
  scale: Double;
  squareSize: Double;
  tile: TDblPoints;
  i, x, y: Integer;
  value: Integer;
begin
  scale := HexVal(FHash, 0);
  squareSize := round(Map(scale, 0, 15, 10, 60));
  tile := BuildOctogonShape(squareSize);

  FTileWidth := squareSize*6;
  FTileHeight := squareSize*6;
  FDrawer.Init(FTileWidth, FTileHeight, FBkColor);
  FDrawer.StrokeColor := StrokeColor;
  FDrawer.StrokeOpacity := StrokeOpacity;

  i := 0;
  for y := 0 to 5 do
    for x := 0 to 5 do
    begin
      value := HexVal(FHash, i);
      FDrawer.FillColor := FillColor(value);
      FDrawer.FillOpacity := FillOpacity(value);
      FDrawer.Polygon(tile.Translate(x*squareSize, y*squareSize));
      inc(i);
    end;
end;

procedure TGeoPattern.GenerateTriangles;

  function BuildTriangleShape(ASideLength, AHeight: Double): TDblPoints;
  var
    halfWidth: Double;
  begin
    halfWidth := ASideLength / 2;
    Result.Init(4);
    Result.Data[0] := DblPoint(halfWidth, 0);
    Result.Data[1] := DblPoint(ASideLength, AHeight);
    Result.Data[2] := DblPoint(0, AHeight);
    Result.Data[3] := DblPoint(halfWidth, 0);
  end;

var
  sideLength, triangleHeight: Double;
  triangle: TDblPoints;
  rotation: Double;
  i, j, x, y: Integer;
  value: Integer;
begin
  sideLength := round(Map(HexVal(FHash, 0), 0, 15, 15, 80));
  triangleHeight := sideLength / 2 * sqrt(3);
  triangle := BuildTriangleShape(sideLength, triangleHeight);

  FTileWidth := sideLength*3;
  FTileHeight := triangleHeight*6;
  FDrawer.Init(FTileWidth, FTileHeight, FBkColor);
  FDrawer.StrokeColor := StrokeColor;
  FDrawer.StrokeOpacity := StrokeOpacity;

  i := 0;
  for y := 0 to 5 do
    for x := 0 to 5 do
    begin
      rotation := 0;
      if y mod 2 = 0 then
      begin
        if x mod 2 = 0 then rotation := 180 else rotation := 0;
      end else
      begin
        if x mod 2 <> 0 then rotation := 180 else rotation := 0;
      end;

      value := HexVal(FHash, i);
      FDrawer.FillColor := FillColor(value);
      FDrawer.FillOpacity := FillOpacity(value);

      FDrawer.Polygon(triangle
        .Rotate(rotation, sideLength*0.5, triangleHeight*0.5)
        .Translate((x-1)*sideLength*0.5, triangleHeight*y)
      );

      if x = 0 then
        FDrawer.Polygon(triangle
          .Rotate(rotation, sideLength*0.5, triangleHeight*0.5)
          .Translate(5*sideLength*0.5, triangleHeight*y)
        );

      inc(i);
    end;
end;

procedure TGeoPattern.GenerateOverlappingRings;
var
  ringSize: Double;
  strokeWidth: Double;
  value: Integer;
  i, x, y: Integer;
begin
  ringSize := Map(HexVal(FHash, 0), 0, 15, 10, 60);
  strokeWidth := ringSize / 4;

  FTileWidth := ringSize*6;
  FTileHeight := ringSize*6;
  FDrawer.Init(FTileWidth, FTileHeight, FBkColor);
  FDrawer.FillColor := clNone;  // no fill

  i := 0;
  for y := 0 to 5 do
    for x := 0 to 5 do
    begin
      value := HexVal(FHash, i);
      FDrawer.StrokeColor := FillColor(value);
      FDrawer.StrokeOpacity := FillOpacity(value);
      FDrawer.StrokeWidth := round(strokeWidth);
      FDrawer.Circle(x*ringSize, y*ringSize, ringSize - strokeWidth/2);
      if (y=0) then
        FDrawer.Circle(x*ringSize, 6*ringSize, ringSize - strokeWidth/2);
        if (x=0) then
          FDrawer.Circle(6*ringSize, y*ringSize, ringSize - strokeWidth/2);
        if (x=0) and (y=0) then
          FDrawer.Circle(6*ringSize, 6*ringSize, ringSize - strokeWidth/2);
      inc(i);
    end;
end;

procedure TGeoPattern.GenerateConcentricCircles;
var
  ringSize: Double;
  strokeWidth: Double;
  value: Integer;
  i, x, y: Integer;
begin
  ringSize := Map(HexVal(FHash, 0), 0, 15, 10, 60);
  strokeWidth := ringSize / 5;

  FTileWidth := (ringSize + strokewidth)*6;
  FTileHeight := (ringSize + strokeWidth)*6;
  FDrawer.Init(FTileWidth, FTileHeight, FBkColor);
  FDrawer.StrokeWidth := round(strokeWidth);

  i := 0;
  for y := 0 to 5 do
    for x := 0 to 5 do
    begin
      value := HexVal(FHash, i);
      FDrawer.FillColor := clNone;  // no fill
      FDrawer.StrokeColor := FillColor(value);
      FDrawer.StrokeOpacity := FillOpacity(value);
      FDrawer.Circle(
        x * ringSize + x * strokeWidth + (ringSize + strokeWidth) / 2,
        y * ringSize + y * strokeWidth + (ringSize + strokeWidth) / 2,
        ringSize / 2);

      value := HexVal(FHash, 39-i);
      FDrawer.StrokeColor := clNone;  // no stroke
      FDrawer.FillColor := FillColor(value);
      FDrawer.FillOpacity := FillOpacity(value);
      FDrawer.Circle(
        x * ringSize + x * strokeWidth + (ringSize + strokeWidth) / 2,
        y * ringSize + y * strokeWidth + (ringSize + strokeWidth) / 2,
        ringSize / 4);
      inc(i);
    end;
end;

procedure TGeoPattern.GenerateNestedSquares;
var
  scale: Integer;
  blockSize: Double;
  squareSize: Double;
  value: Integer;
  i, x, y: Integer;
begin
  scale := HexVal(FHash, 0);
  blockSize := Map(scale, 0, 15, 4, 12);
  squareSize := blockSize*7;

  FTileWidth := (squareSize + blockSize)*6 + blockSize*6;
  FTileHeight := (squareSize + blockSize)*6 + blockSize*6;
  FDrawer.Init(FTileWidth, FTileHeight, FBkColor);

  FDrawer.FillColor := clNone;  // no fill
  FDrawer.StrokeWidth := round(blockSize);

  i := 0;
  for y := 0 to 5 do
    for x := 0 to 5 do
    begin
      value := HexVal(FHash, i);
      FDrawer.StrokeColor := FillColor(value);
      FDrawer.StrokeOpacity := FillOpacity(value);
      FDrawer.Square(
        x * squareSize + x * blockSize * 2 + blockSize / 2,
        y * squareSize + y * blockSize * 2 + blockSize / 2,
        squareSize);

      value := HexVal(FHash, 39-i);
      FDrawer.StrokeColor := FillColor(value);
      FDrawer.StrokeOpacity := FillOpacity(value);
      FDrawer.Square(
        x * squareSize + x * blockSize * 2 + blockSize / 2 + blockSize * 2,
        y * squareSize + y * blockSize * 2 + blockSize / 2 + blockSize * 2,
        blockSize * 3
      );

      inc(i);
    end;
end;

procedure TGeoPattern.GenerateMosaicSquares;

  function BuildRightTriangleShape(ASideLength: Double): TDblPoints;
  begin
    Result.Init(4);
    Result.Data[0] := DblPoint(0, 0);
    Result.Data[1] := DblPoint(ASideLength, ASideLength);
    Result.Data[2] := DblPoint(0, ASideLength);
    Result.Data[3] := DblPoint(0, 0);
  end;

  procedure DrawInnerMosaicTile(X, Y, ATriangleSize: Double; AValue1, AValue2: Integer);
  var
    triangle: TDblPoints;
  begin
    FDrawer.FillOpacity := FillOpacity(AValue1);
    FDrawer.FillColor := FillColor(AValue2);
    FDrawer.StrokeColor := StrokeColor;
    FDrawer.StrokeOpacity := StrokeOpacity;

    triangle := BuildRightTriangleShape(ATriangleSize);
    FDrawer.Polygon(triangle.Scale(-1, -1).Translate(X + ATriangleSize, Y + ATriangleSize*2));
    FDrawer.Polygon(triangle.Scale(1, 1).Translate(X + ATriangleSize, Y));
  end;

  procedure DrawOuterMosaicTile(X, Y, ATriangleSize: Double; AValue: Integer);
  var
    triangle: TDblPoints;
  begin
    FDrawer.FillOpacity := FillOpacity(AValue);
    FDrawer.FillColor := FillColor(AValue);
    FDrawer.StrokeColor := StrokeColor;
    FDrawer.StrokeOpacity := StrokeOpacity;

    triangle := BuildRightTriangleShape(ATriangleSize);
    FDrawer.Polygon(triangle.Scale(1, -1).Translate(X, Y + ATriangleSize));
    FDrawer.Polygon(triangle.Scale(-1, -1).Translate(X + ATriangleSize * 2, Y + ATriangleSize));
    FDrawer.Polygon(triangle.Scale(1, 1).Translate(X, Y + ATriangleSize));
    FDrawer.Polygon(triangle.Scale(-1, 1).Translate(X + ATriangleSize * 2, Y + ATriangleSize));
  end;

var
  triangleSize: Double;
  i, x, y: Integer;
begin
  triangleSize := Map(HexVal(FHash, 0), 0, 15, 15, 50);

  FTileWidth := triangleSize*8;
  FTileHeight := triangleSize*8;
  FDrawer.Init(FTileWidth, FTileHeight, FBkColor);

  i := 0;
  for y := 0 to 3 do
    for x := 0 to 3 do
    begin
      if x mod 2 = 0 then
      begin
        if y mod 2 = 0 then
          DrawOuterMosaicTile(
            x * triangleSize * 2,
            y * triangleSize * 2,
            triangleSize,
            HexVal(FHash, i)
          )
        else
          DrawInnerMosaicTile(
            x * triangleSize * 2,
            y * triangleSize * 2,
            triangleSize,
            HexVal(FHash, i),
            HexVal(FHash, i+1)
          );
      end
      else
      begin
        if y mod 2 = 0 then
          DrawInnerMosaicTile(
            x * triangleSize * 2,
            y * triangleSize * 2,
            triangleSize,
            HexVal(FHash, i),
            HexVal(FHash, i+1)
          )
        else
          DrawOuterMosaicTile(
            x * triangleSize * 2,
            y * triangleSize * 2,
            triangleSize,
            HexVal(FHash, i)
          )
      end;
      inc(i);
    end;
end;

// Missing part for generator string 111111111111111111111111111111111111111111111111111111111111111
// when amplitude factor 1.5 is used --> replaced by 1.0 --> ok
procedure TGeoPattern.GenerateSineWaves;
var
  period: Integer;
  amplitude: Integer;
  waveWidth: Integer;
  value: Integer;
  xOffset: Double;
  i: Integer;
  pts: TDblPoints;
begin
  period := Math.Floor(Map(HexVal(FHash, 0), 0, 15, 100, 400));
  amplitude := Math.Floor(Map(HexVal(FHash, 1), 0, 15, 30, 100));
  waveWidth := Math.Floor(Map(HexVal(FHash, 2), 0, 15, 3, 30));

  FTileWidth := period;
  FTileHeight := waveWidth * 36;
  FDrawer.Init(FTileWidth, FTileHeight, FBkColor);

  FDrawer.StrokeWidth := waveWidth;
  FDrawer.FillColor := clNone;  // no fill

  xOffset := period / 4 * 0.7;

  {             c----x----c

                /        \

         x----c            c----x }
  pts.Init(8);
  // 1st segment
  pts.Data[0] := DblPoint(0, amplitude);                    // 1st curve point (x)
  pts.Data[1] := DblPoint(xOffset, amplitude);              // control point of 1st curve pt (c)
  pts.Data[2] := DblPoint(period*0.5 - xOffset, 0);         // control point of 2nd curve pt (c)
  pts.Data[3] := DblPoint(period*0.5, 0);                   // 2nd curve point (x)
  // 2nd segment
  pts.Data[4] := pts.Data[3];                               // duplicated 2nd curve point as start point of 2nd segment  // not needed in svg S segment
  pts.Data[5] := DblPoint(period*0.5 + xOffset, 0);         // control point of 2nd curve pt  // not needed in svg S segment
  pts.Data[6] := DblPoint(period*1.0 - xOffset, amplitude); // control point of last curve pt
  pts.Data[7] := DblPoint(period*1.0, amplitude);           // last curve point

  for i := 0 to 35 do
  begin
    value := HexVal(FHash, i);
    FDrawer.StrokeOpacity := FillOpacity(value);
    FDrawer.StrokeColor := FillColor(value);
    FDrawer.PolyBezier(pts.Translate(0, waveWidth*i - amplitude*1.0));          // wp: Replaced factor 1.5 by 1.0
    FDrawer.PolyBezier(pts.Translate(0, waveWidth*i - amplitude*1.0 + waveWidth*36));
  end;
end;

procedure TGeoPattern.GeneratePlusSigns;
var
  value: Integer;
  squareSize: Double;
  plusSize: Double;
  plusShape: TDblPoints;
  i, x, y, dx: Integer;
begin
  squareSize := round(Map(HexVal(FHash, 0), 0, 15, 10, 25));
  plusSize := squareSize*3;
  plusShape := BuildPlusShape(squareSize);

  FTileWidth := squareSize*12;
  FTileHeight := squareSize*12;
  FDrawer.Init(FTileWidth, FTileHeight, FBkColor);

  FDrawer.StrokeColor := StrokeColor;
  FDrawer.StrokeOpacity := StrokeOpacity;

  i := 0;
  for y := 0 to 5 do
  begin
    for x := 0 to 5 do
    begin
      value := HexVal(FHash, i mod Length(FHash));
      FDrawer.FillOpacity := FillOpacity(value);
      FDrawer.FillColor := FillColor(value);

      if y mod 2 = 0 then dx := 0 else dx := 1;

      FDrawer.Polygon(plusShape.Translate(
        x * plusSize - x * squareSize + dx * squareSize - squareSize,
        y * plusSize - y * squareSize - plusSize / 2)
      );

      if (x = 0) then
        FDrawer.Polygon(plusShape.Translate(
          4 * plusSize - x * squareSize + dx * squareSize - squareSize,
          y * plusSize - y * squareSize - plusSize / 2)
        );

      if (y = 0) then
        FDrawer.Polygon(plusShape.Translate(
          x * plusSize - x * squareSize + dx * squareSize - squareSize,
          4 * plusSize - y * squareSize - plusSize / 2)
        );

      if (x = 0) and (y = 0) then
        FDrawer.Polygon(plusShape.Translate(
          4 * plusSize - x * squareSize + dx * squareSize - squareSize,
          4 * plusSize - y * squareSize - plusSize / 2)
        );

      inc(i);
    end;
  end;
end;

{ Changed the algorithm from that in the "official" version becasuse that
  produces the same pattern as the "diamonds" generator (https://github.com/suyash/geopattern-rs ...)
  and looks strange here in the BGRA version. }
procedure TGeoPattern.GenerateXes;
var
  value: Integer;
  squareSize, xSize, xSize2: Double;
  xShape: TDblPoints;
  x, y, i: Integer;
begin
  squareSize := Map(HexVal(FHash, 0), 0, 15, 10, 25);
  xSize := squareSize * 3 * 0.943;    // width of 45°-rotated "plus" (0.943 = 2 * sqrt(2) / 3)
  xSize2 := xSize / 2;                // half-width of rotated "plus"
  xShape := BuildPlusShape(squareSize).Rotate(45, xSize2, xSize2);

  FTileWidth := xSize*6;
  FTileHeight := xSize*6;
  FDrawer.Init(FTileWidth, FTileHeight, FBkColor);
  FDrawer.StrokeColor := clNone;

  i := 0;
  for y := 0 to 5 do
  begin
    for x := 0 to 5 do
    begin
      value := HexVal(FHash, i);
      FDrawer.FillColor := FillColor(value);
      FDrawer.FillOpacity := FillOpacity(value);
      FDrawer.Polygon(xShape.Translate(x * xSize, y * xSize));
      inc(i);
    end;
  end;
end;

procedure TGeoPattern.GenerateChevrons;

  function BuildChevronShape(AWidth, AHeight: Double; Part: Integer): TDblPoints;
  var
    e: Double;
  begin
    e := AHeight * 0.66;
    case Part of
     1: begin
          Result.Init(5);
          Result.Data[0] := DblPoint(0, 0);
          Result.Data[1] := DblPoint(AWidth / 2, AHeight - e);
          Result.Data[2] := DblPoint(AWidth / 2, AHeight);
          Result.Data[3] := DblPoint(0, e);
          Result.Data[4] := DblPoint(0, 0);
        end;
     2: begin
          Result.Init(5);
          Result.Data[0] := DblPoint(AWidth / 2, AHeight - e);
          Result.Data[1] := DblPoint(AWidth, 0);
          Result.Data[2] := DblPoint(AWidth, e);
          Result.Data[3] := DblPoint(AWidth / 2, AHeight);
          Result.Data[4] := DblPoint(AWidth / 2, AHeight - e);
        end;
    end;
  end;

var
  chevronWidth, chevronHeight: Double;
  chevron1: TDblPoints;
  chevron2: TDblPoints;
  i, x, y: Integer;
  value: Integer;
begin
  chevronWidth := round(Map(HexVal(FHash, 0), 0, 15, 30, 80));
  chevronHeight := round(Map(HexVal(FHash, 0), 0, 15, 30, 80));
  chevron1 := BuildChevronShape(chevronWidth, chevronHeight, 1);
  chevron2 := BuildChevronShape(chevronWidth, chevronHeight, 2);

  FTileWidth := chevronWidth* 6;
  FTileHeight := chevronHeight * 6 * 0.66;
  FDrawer.Init(FTileWidth, FTileHeight, FBkColor);

  FDrawer.StrokeColor := StrokeColor;
  FDrawer.StrokeOpacity := StrokeOpacity;
  FDrawer.StrokeWidth := 1;

  i := 0;
  for y := 0 to 5 do
    for x := 0 to 5 do
    begin
      value := HexVal(FHash, i);
      FDrawer.FillColor := FillColor(value);
      FDrawer.FillOpacity := FillOpacity(value);

      FDrawer.Polygon(chevron1.Translate(x * chevronWidth, y * chevronHeight * 0.66 - chevronHeight / 2));
      FDrawer.Polygon(chevron2.Translate(x * chevronWidth, y * chevronHeight * 0.66 - chevronHeight / 2));

      if y = 0 then
      begin
        FDrawer.Polygon(chevron1.Translate(x * chevronWidth, 6 * chevronHeight * 0.66 - chevronHeight / 2));
        FDrawer.Polygon(chevron2.Translate(x * chevronWidth, 6 * chevronHeight * 0.66 - chevronHeight / 2));
      end;

      inc(i);
    end;
end;

procedure TGeoPattern.GenerateDiamonds;

  function BuildDiamondShape(AWidth, AHeight: Double): TDblPoints;
  begin
    Result.Init(4);
    Result.Data[0] := DblPoint(AWidth / 2, 0);
    Result.Data[1] := DblPoint(AWidth, AHeight / 2);
    Result.Data[2] := DblPoint(AWidth / 2, AHeight);
    Result.Data[3] := DblPoint(0, AHeight / 2);
  end;

var
  diamondWidth, diamondHeight: Double;
  diamond: TDblPoints;
  i, x, y, dx: Integer;
  value: Integer;
begin
  diamondWidth := round(Map(HexVal(FHash, 0), 0, 15, 10, 50));
  diamondHeight := round(Map(HexVal(FHash, 0), 0, 15, 10, 50));
  diamond := BuildDiamondShape(diamondWidth, diamondHeight);

  FTileWidth := diamondWidth * 6;
  FTileHeight := diamondHeight * 3;
  FDrawer.Init(FTileWidth, FTileHeight, FBkColor);

  FDrawer.StrokeColor := StrokeColor;
  FDrawer.StrokeOpacity := StrokeOpacity;
  FDrawer.StrokeWidth := 1;

  i := 0;
  for y := 0 to 5 do
    for x := 0 to 5 do
    begin
      value := HexVal(FHash, i);
      FDrawer.FillColor := FillColor(value);
      FDrawer.FillOpacity := FillOpacity(value);
      if y mod 2 = 0 then
        dx := 0
      else
        dx := round(diamondWidth / 2);

      FDrawer.Polygon(diamond.Translate(x * diamondWidth - diamondWidth / 2 + dx, diamondHeight / 2 * y - diamondHeight / 2));

      if x = 0 then
        FDrawer.Polygon(diamond.Translate(6 * diamondWidth - diamondWidth / 2 + dx, diamondHeight / 2 * y - diamondHeight / 2));

      if y = 0 then
        FDrawer.Polygon(diamond.Translate(x * diamondWidth - diamondWidth / 2 + dx, diamondHeight / 2 * 6 - diamondHeight / 2));

      if (x = 0) and (y = 0) then
       FDrawer.Polygon(diamond.Translate(6 * diamondWidth - diamondWidth / 2 + dx, diamondHeight / 2 * 6 - diamondHeight / 2));

      inc(i);
    end;
end;

procedure TGeoPattern.GeneratePlaid;
var
  lWidth, lHeight, lSpace, lStripeHeight, lStripeWidth: Double;
  x, y: Double;
  i, value: Integer;
begin
  lHeight := 0;
  i := 0;
  while i < 35 do
  begin
    lSpace := HexVal(FHash, i);
    lStripeHeight := HexVal(FHash, i+1) + 5;
    lHeight := lHeight + lSpace + 5 + lStripeHeight + 5;
    inc(i, 2);
  end;

  lWidth := 0;
  i := 0;
  while i < 35 do
  begin
    lSpace := HexVal(FHash, i);
    lStripeWidth := HexVal(FHash, i+1);
    lWidth := lWidth + lSpace + 5 + lStripeWidth + 5;
    inc(i, 2);
  end;

  FTileWidth := lWidth;
  FTileHeight := lHeight;
  FDrawer.Init(FTileWidth, FTileHeight, FBkColor);

  FDrawer.StrokeColor := clNone; //StrokeColor;
//  FDrawer.StrokeOpacity := StrokeOpacity;

  i := 0;
  y := 0;
  while i < 35 do
  begin
    lSpace := HexVal(FHash, i);
    y := y + lSpace + 5;
    value := HexVal(FHash, i+1);
    FDrawer.FillColor := FillColor(value);
    FDrawer.FillOpacity := FillOpacity(value);
    lStripeHeight := value + 5;
    FDrawer.Rectangle(0, round(y), round(lWidth), round(lStripeHeight));
    y := y + lStripeHeight;
    inc(i, 2);
  end;

  i := 0;
  x := 0;
  while i < 35 do
  begin
    lSpace := HexVal(FHash, i);
    x := x + lSpace + 5;
    value := HexVal(FHash, i+1);
    FDrawer.FillColor := FillColor(value);
    FDrawer.FillOpacity := FillOpacity(value);
    lStripeWidth := value + 5;
    FDrawer.Rectangle(round(x), 0, round(lStripeWidth), round(lHeight));
    x := x + lStripeWidth;
    inc(i, 2);
  end;
end;

procedure TGeoPattern.GenerateTesselation;

  function BuildRotatedTriangleShape(ASideLength, ATriangleWidth: Double): TDblPoints;
  begin
    Result.Init(4);
    Result.Data[0] := DblPoint(0, 0);
    Result.Data[1] := DblPoint(ATriangleWidth, ASideLength/2);
    Result.Data[2] := DblPoint(0, ASideLength);
    Result.Data[3] := DblPoint(0, 0);
  end;

  function BuildRectangleShape(AWidth, AHeight: Double): TDblPoints;
  begin
    Result.Init(5);
    Result.Data[0] := DblPoint(0, 0);
    Result.Data[1] := DblPoint(AWidth, 0);
    Result.Data[2] := DblPoint(AWidth, AHeight);
    Result.Data[3] := DblPoint(0, AHeight);
    Result.Data[4] := DblPoint(0, 0);
  end;

var
  sideLength: Double;           // Side length of the squares, triangles and hexagons
  hexHeight, hexWidth: Double;  // Width and height of the hexagon
  triangleHeight: Double;       // Height of equilateral triangle (side length = sideLength)
  sideLength2, hexHeight2, hexWidth2, triangleHeight2: Double;     // half of these lengths
  triangle, rect: TDblPoints;
  value: Integer;
  i: Integer;
begin
  sideLength := Map(HexVal(FHash, 0), 0, 15, 5, 40);
  sideLength2 := sideLength / 2;
  hexHeight := sidelength * sqrt(3.0);
  hexHeight2 := hexHeight / 2;
  hexWidth := sideLength * 2;
  hexWidth2 := hexWidth / 2;
  triangleHeight := sideLength2 * sqrt(3.0);
  triangleHeight2 := triangleHeight / 2;
  triangle := BuildRotatedTriangleShape(sidelength, triangleHeight);
  rect := BuildRectangleShape(sideLength, sideLength);

  FTileWidth := sideLength * 3 + triangleHeight * 2;
  FTileHeight := hexHeight * 2 + sideLength * 2;
  FDrawer.Init(FTileWidth, FTileHeight, FBkColor);
  FDrawer.StrokeColor := StrokeColor;
  FDrawer.StrokeOpacity := StrokeOpacity;
  FDrawer.StrokeWidth := 1;

  for i := 0 to 19 do
  begin
    value := HexVal(FHash, i);
    FDrawer.FillColor := FillColor(value);
    FDrawer.FillOpacity := FillOpacity(value);
    case i of
      0: begin
           FDrawer.Rectangle(-sideLength2, -sideLength2, sideLength, sideLength);
           FDrawer.Rectangle(FTileWidth - sideLength2, -sideLength2, sideLength, sideLength);
           FDrawer.Rectangle(-sideLength2, FTileHeight - sideLength2, sideLength, sideLength);
           FDrawer.Rectangle(FTileWidth - sideLength2, FTileHeight - sideLength2, sideLength, sideLength);
         end;
      1: FDrawer.Rectangle(hexWidth2 + triangleHeight, hexHeight2, sideLength, sideLength);
      2: begin
           FDrawer.Rectangle(-sideLength2, FTileHeight / 2 - sideLength2, sideLength, sideLength);
           FDrawer.Rectangle(FTileWidth - sideLength2, FTileHeight / 2 - sideLength2, sideLength, sideLength);
         end;
      3: FDrawer.Rectangle(hexWidth2 + triangleHeight, hexHeight * 1.5 + sideLength, sideLength, sideLength);
      4: begin
           FDrawer.Polygon(triangle.Rotate(0, sideLength2, triangleHeight2).Translate(sideLength2, -sideLength2));
           FDrawer.Polygon(triangle.Scale(1, -1).Rotate(0, sideLength2, triangleHeight2).Translate(sideLength2, FTileHeight + sideLength2));
         end;
      5: begin
           FDrawer.Polygon(triangle.Scale(-1, 1).Rotate(0, sideLength2, triangleHeight2).Translate(FTileWidth - sideLength2, -sideLength2));
           FDrawer.Polygon(triangle.Scale(-1, -1).Rotate(0, sideLength2, triangleHeight2).Translate(FTileWidth - sideLength2, FTileHeight + sideLength2));
         end;
      6: FDrawer.Polygon(triangle.Translate(FTileWidth / 2 + sideLength2, hexHeight2));
      7: FDrawer.Polygon(triangle.Scale(-1, 1).Translate(FTileWidth - FTileWidth / 2 - sideLength2, hexHeight2));
      8: FDrawer.Polygon(triangle.Scale(1, -1).Translate(FTileWidth / 2 + sideLength2, FTileHeight - hexHeight2));
      9: FDrawer.Polygon(triangle.Scale(-1, -1).Translate(FTileWidth - FTileWidth / 2 - sideLength2, FTileHeight - hexHeight2));
     10: FDrawer.Polygon(triangle.Translate(sideLength2, FTileHeight / 2 - sideLength2));
     11: FDrawer.Polygon(triangle.Scale(-1, 1).Translate(FTileWidth - sideLength2, FTileHeight / 2 - sideLength2));
     12: FDrawer.Polygon(rect.Rotate(30, 0, 0).Translate(sideLength2, sideLength2));
     13: FDrawer.Polygon(rect.Rotate(30, 0, 0).Translate(-FTileWidth + sideLength2, sideLength2).Scale(-1, 1));
     14: FDrawer.Polygon(rect.Rotate(-30, 0, sideLength).Translate(sideLength2, FTileHeight / 2 - sideLength2 - sideLength));
     15: FDrawer.Polygon(rect.Rotate(-30, 0, sideLength).Translate(-FTileWidth + sideLength2, FTileHeight/2 - sideLength2 - sideLength).Scale(-1, 1));
     16: FDrawer.Polygon(rect.Rotate(-30, 0, sideLength).Translate(sideLength2, -FTileHeight + FTileHeight/2 - sideLength2 - sideLength).Scale(1, -1));
     17: FDrawer.Polygon(rect.Rotate(-30, 0, sideLength).Translate(-FTileWidth + sideLength2, -FTileHeight + FTileHeight/2 - sideLength2 - sideLength).Scale(-1,-1));
     18: FDrawer.Polygon(rect.Rotate(30, 0, 0).Translate(sideLength2, -FTileHeight + sideLength2).Scale(1, -1));
     19: FDrawer.Polygon(rect.Rotate(30, 0, 0).Translate(-FTileWidth + sideLength2, -FTileHeight + sideLength2).Scale(-1,-1));
    end;
  end;
end;

// -----------------------------------------------------------------------------
// Packed circles
// https://generativeartistry.com/tutorials/circle-packing/
// -----------------------------------------------------------------------------
type
  TCircle = record
    X, Y, Radius: Double;
    class operator = (A, B: TCircle): Boolean;
  end;
  TCircleList = specialize TFPGList<TCircle>;

class operator TCircle.=(A, B: TCircle): Boolean;
begin
  Result := (A.X = B.X) and (A.Y = B.Y) and (A.Radius = B.Radius);
end;

function CreateCircle(X, Y, Radius: Double): TCircle;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Radius := Radius;
end;

procedure TGeoPattern.GeneratePackedCircles;
const
  MIN_RADIUS = 5;
  TOTAL_CIRCLES = 500;
  CREATE_CIRCLE_ATTEMPTS = 500;
var
  circles: TCircleList;

  function DoesCircleHaveACollision(ACircle: TCircle): Boolean;
  var
    i: Integer;
    otherCircle: TCircle;
    centerDist: Double;
  begin
    Result := True;
    for i := 0 to circles.Count-1 do
    begin
      otherCircle := circles{%H-}[i];
      centerDist := sqrt(sqr(ACircle.X - otherCircle.X) + sqr(ACircle.Y - otherCircle.Y));
      if centerDist <= ACircle.Radius + otherCircle.Radius then
        exit;
    end;
    if (ACircle.X + ACircle.Radius > FTileWidth) or (ACircle.X - ACircle.Radius < 0) then
      exit;
    if (ACircle.Y + ACircle.Radius > FTileHeight) or (ACircle.Y - ACircle.Radius < 0) then
      exit;
    Result := false;
  end;

  procedure CreateAndDrawCircle(AMinRadius, AMaxRadius: Integer);
  var
    newCircle: TCircle;
    circleSafeToDraw: Boolean = false;
    tries, i: Integer;
    radiusSize: Integer;
    value: Integer;
  begin
    RandSeed := HexVal(FHash, 1) + HexVal(FHash, 2)*256;
    for tries := 0 to CREATE_CIRCLE_ATTEMPTS-1 do
    begin
      newCircle := CreateCircle(Random * FTileWidth, Random * FTileHeight, MIN_RADIUS);
      if DoesCircleHaveACollision(newCircle) then
        Continue
      else
      begin
        circleSafeToDraw := true;
        Break;
      end;
    end;
    if not circleSafeToDraw then
      exit;
    for radiusSize := AMinRadius to AMaxRadius do
    begin
      newCircle.Radius := radiusSize;
      if DoesCircleHaveACollision(newCircle) then
      begin
        newCircle.Radius := newCircle.Radius - 1;
        break;
      end;
    end;

    {%H-}circles.Add(newCircle);
    i := circles.Count;
    value := HexVal(FHash, i);
    FDrawer.FillColor := FillColor(value);
    FDrawer.FillOpacity := FillOpacity(value);
    FDrawer.Circle(newCircle.X, newCircle.Y, newCircle.Radius);

    inc(i);
  end;

var
  maxRadius: Double;
  i: Integer;
begin
  FTileWidth := Map(HexVal(FHash, 0), 0, 15, 30, 300);
  FTileHeight := FTileWidth;
  FDrawer.Init(FTileWidth, FTileHeight, FBkColor);
  FDrawer.StrokeColor := StrokeColor;
  FDrawer.StrokeOpacity := StrokeOpacity;

  maxRadius := FTileWidth / 2;

  circles := TCircleList.Create;
  try
    for i := 0 to TOTAL_CIRCLES-1 do
      CreateAndDrawCircle(MIN_RADIUS, round(maxRadius));
  finally
    circles.Free;
  end;
end;

// -----------------------------------------------------------------------------
// Piet Mondrian
// https://generativeartistry.com/tutorials/piet-mondrian/
// -----------------------------------------------------------------------------
procedure TGeoPattern.GeneratePietMondrian;
type
  TSquare = record
    X, Y: Double;
    Width, Height: Double;
    Color: TColor;
  end;
var
  squares: array of TSquare = nil;

  function CreateSquare(X, Y, Width, Height: Double): TSquare;
  begin
    Result.X := X;
    Result.Y := Y;
    Result.Width := Width;
    Result.Height := Height;
    Result.Color := 0;
  end;

  procedure PushSquare(ASquare: TSquare);
  begin
    SetLength(squares, Length(squares)+1);
    squares[Length(squares)-1] := ASquare;
  end;

  procedure SplitOnX(ASquare: TSquare; SplitAt: Double);
  var
    squareA: TSquare;
    squareB: TSquare;
  begin
    squareA := CreateSquare(ASquare.X, ASquare.Y, ASquare.Width - (ASquare.Width - SplitAt + ASquare.x), ASquare.Height);
    squareB := CreateSquare(SplitAt, ASquare.Y, ASquare.Width - SplitAt + ASquare.X, ASquare.Height);
    PushSquare(squareA);
    PushSquare(squareB);
  end;

  procedure SplitOnY(ASquare: TSquare; SplitAt: Double);
  var
    squareA: TSquare;
    squareB: TSquare;
  begin
    squareA := CreateSquare(ASquare.X, ASquare.Y, ASquare.Width, ASquare.Height - (ASquare.Height - SplitAt + ASquare.Y));
    squareB := CreateSquare(ASquare.X, SplitAt, ASquare.Width, ASquare.Height - SplitAt + ASquare.Y);
    PushSquare(squareA);
    PushSquare(squareB);
  end;

  procedure SplitSquaresWith(X, Y: Double);
  var
    i, j: Integer;
    square: TSquare;
  begin
    for i := High(squares) downto 0 do
    begin
      square := squares[i];
      if (X > 0) and (X > square.X) and (X < square.X + square.Width) then
        if Random > 0.5 then
        begin
          // Delete the element at index i
          for j := i+1 to Length(squares)-1 do
            squares[j-1] := squares[j];
          SetLength(squares, Length(squares)-1);
          SplitOnX(square, X);
        end;
      if (Y > 0) and (Y > square.Y) and (Y < square.Y + square.Height) then
        if Random > 0.5 then
        begin
          // Delete the element at index i
          for j := i+1 to Length(squares)-1 do
            squares[j-1] := squares[j];
          SetLength(squares, Length(squares)-1);
          SplitOnY(square, Y);
        end;
    end;
  end;

const
  WHITE = $F1F5F2;
  COLORS: array[0..2] of TColor = ($2009D4, $A25613, $42D8F7);
var
  c: Double;
  step: double;
  i, j: Integer;
  strokeWidth: Integer;
begin
  FTileWidth := Map(HexVal(FHash, 0), 0, 15, 120, 400);
  FTileHeight := FTileWidth;
  FDrawer.Init(FTileWidth, FTileHeight, FBkColor);
  FDrawer.StrokeColor := clBlack; //StrokeColor;
  FDrawer.StrokeOpacity := 1.0;   //StrokeOpacity;
  strokeWidth := round(Map(HexVal(FHash, 1), 0, 15, 1, 8));
  FDrawer.StrokeWidth := strokeWidth;
  FDrawer.FillOpacity := 1.0;
  step := FTileWidth / 7;
  RandSeed := HexVal(FHash, 2, 2);

  SetLength(squares, 1);
  squares[0] := CreateSquare(0, 0, FTileWidth, FTileHeight);
  c := 0;
  while c < FTileWidth do
  begin
    SplitSquaresWith(0, c);
    SplitSquaresWith(c, 0);
    c := c + step;
  end;

  for i := 0 to High(COLORS) do
  begin
    repeat
      j := Random(Length(squares));
    until (squares[j].Color = 0) and (squares[j].Width > 2*strokewidth) and (squares[j].Height > 2*strokewidth);
    squares[j].Color := COLORS[i];
  end;

  for i := 0 to High(squares) do
  begin
    if squares[i].Color = 0 then
      FDrawer.FillColor := WHITE
    else
      FDrawer.FillColor := squares[i].Color;
    FDrawer.Rectangle(squares[i].X, squares[i].Y, squares[i].Width, squares[i].Height);
  end;
end;

end.

