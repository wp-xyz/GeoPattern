unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons, Menus, Spin,
  uGeoPatterns, uBasicGeoDrawer, uGeoDrawerBGRA, uGeoDrawerSVG;

type

  { TMainForm }

  TMainForm = class(TForm)
    Button1: TButton;
    cbOverrideStrokeColor: TCheckBox;
    cbOverrideFillColor1: TCheckBox;
    cbOverrideFillColor2: TCheckBox;
    cbOverrideStrokeOpacity: TCheckBox;
    cbOverrideBaseColor: TCheckBox;
    cbOverridePatternType: TCheckBox;
    clbBaseColor: TColorButton;
    cbOverrideMaxFillOpacity: TCheckBox;
    cbOverrideMinFillOpacity: TCheckBox;
    clbStrokeColor: TColorButton;
    clbFillColor1: TColorButton;
    clbFillColor2: TColorButton;
    cmbPatterns: TComboBox;
    GroupBox3: TGroupBox;
    gbPatterns: TGroupBox;
    Label1: TLabel;
    seMaxFillOpacity: TFloatSpinEdit;
    seStrokeOpacity: TFloatSpinEdit;
    seMinFillOpacity: TFloatSpinEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    MenuItem9: TMenuItem;
    ParamsPanel: TPanel;
    SaveButton: TButton;
    GeneratorEdit: TEdit;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    PaintBox1: TPaintBox;
    Panel1: TPanel;
    SaveDialog: TSaveDialog;
    SizePopupMenu: TPopupMenu;
    Separator1: TMenuItem;
    procedure Button1Click(Sender: TObject);
    procedure cbOverridePatternTypeChange(Sender: TObject);
    procedure clbColorChanged(Sender: TObject);
    procedure cbOverrideColorChange(Sender: TObject);
    procedure cbOverrideMaxFillOpacityChange(Sender: TObject);
    procedure cbOverrideMinFillOpacityChange(Sender: TObject);
    procedure cbOverrideStrokeOpacityChange(Sender: TObject);
    procedure cmbPatternsChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure GeneratorEditChange(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure seOpacityChange(Sender: TObject);
    procedure seMinFillOpacityChange(Sender: TObject);
    procedure seStrokeOpacityChange(Sender: TObject);
  private
    FPatternOptions: TGeoPatternOptions;
    FActivated: Boolean;

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}


{ TMainForm }

procedure TMainForm.PaintBox1Paint(Sender: TObject);
var
  patt: TGeoPattern;
begin
  patt := TGeoPattern.Create(GeneratorEdit.Text, Paintbox1.Width, Paintbox1.Height, TGeoDrawerBGRA, FPatternOptions);
  try
    patt.DrawToCanvas(Paintbox1.Canvas);
  finally
    patt.Free;
  end;
end;

procedure TMainForm.GeneratorEditChange(Sender: TObject);
begin
  SaveButton.Enabled := GeneratorEdit.Text <> '';
  Paintbox1.Invalidate;
end;

procedure TMainForm.SaveButtonClick(Sender: TObject);
var
  patt: TGeoPattern;
  i, idx: Integer;
  w, h: Integer;
  p: Integer;
  sw, sh, fn, ext: String;
  menuItem: TMenuItem;
  drawer: TGeoDrawerClass;
begin
  if SaveDialog.Execute then
  begin
    if Sender is TMenuItem then
    begin
      menuItem := TMenuItem(Sender);
      idx := SizePopupMenu.Items.IndexOf(menuItem);
    end else
    if Sender is TButton then
      for i := 0 to SizePopupMenu.Items.Count-1 do
        if SizePopupMenu.Items[i].Checked then
        begin
          idx := i;
          break;
        end
    else
      idx := 0;

    case idx of
      0: begin w := 0; h := 0; end;    // tile size only
      1: begin w := Paintbox1.Width; h := Paintbox1.Height; end;
      2: begin w := Monitor.width; h := Monitor.Height; end;
      else
        p := pos('x', SizePopupMenu.Items[idx].Caption);
        sw := Copy(SizePopupMenu.Items[idx].Caption, 1, p-2);
        sh := Copy(SizePopupMenu.Items[idx].Caption, p+2);
        w := StrToInt(sw);
        h := StrToInt(sh);
    end;

    fn := SaveDialog.FileName;
    ext := Lowercase(ExtractFileExt(fn));
    case ext of
      '.png': drawer := TGeoDrawerBGRA;
      '.svg': drawer := TGeoDrawerSVG;
      else raise Exception.Create('Drawer class not defined.');
    end;

    patt := TGeoPattern.Create(GeneratorEdit.Text, w, h, drawer, FPatternOptions);
    try
      patt.SaveToFile(SaveDialog.Filename);
    finally
      patt.Free;
    end;
  end;
end;

procedure TMainForm.clbColorChanged(Sender: TObject);
begin
  if Sender = clbBaseColor then
    FPatternOptions.BaseColor := clbBaseColor.ButtonColor
  else
  if Sender = clbFillColor1 then
    FPatternOptions.FillColor1 := clbFillColor1.ButtonColor
  else
  if Sender = clbFillColor2 then
    FPatternOptions.FillColor2 := clbFillColor2.ButtonColor
  else
  if Sender = clbStrokeColor then
    FPatternOptions.StrokeColor := clbStrokeColor.ButtonColor
  else
    exit;
  Paintbox1.Invalidate;
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  ParamsPanel.Visible := not ParamsPanel.Visible;
end;

procedure TMainForm.cbOverridePatternTypeChange(Sender: TObject);
begin
  FPatternOptions.OverridePatternType := cbOverridePatternType.Checked;
  cmbPatterns.Enabled := FPatternOptions.OverridePatternType;
  Paintbox1.Invalidate;
end;

procedure TMainForm.cbOverrideColorChange(Sender: TObject);
begin
  if Sender = cbOverrideBaseColor then
  begin
    FPatternOptions.OverrideBaseColor := cbOverrideBaseColor.Checked;
    clbBaseColor.Enabled := FPatternOptions.OverrideBaseColor;
  end else
  if Sender = cbOverrideFillColor1 then
  begin
    FPatternOptions.OverrideFillColor1 := cbOverrideFillColor1.Checked;
    clbFillColor1.Enabled := FPatternOptions.OverrideFillColor1;
  end else
  if Sender = cbOverrideFillColor2 then
  begin
    FPatternOptions.OverrideFillColor2 := cbOverrideFillColor2.Checked;
    clbFillColor2.Enabled := FPatternOptions.OverrideFillColor2;
  end else
  if Sender = cbOverrideStrokeColor then
  begin
    FPatternOptions.OverrideStrokeColor := cbOverrideStrokeColor.Checked;
    clbStrokeColor.Enabled := FPatternOptions.OverrideStrokeColor;
  end else
    exit;
  Paintbox1.Invalidate;
end;

procedure TMainForm.cbOverrideMaxFillOpacityChange(Sender: TObject);
begin
  FPatternOptions.OverrideMaxFillOpacity := cbOverrideMaxFillOpacity.Checked;
  seMaxFillOpacity.Enabled := FPatternOptions.OverrideMaxFillOpacity;
  Paintbox1.Invalidate;
end;

procedure TMainForm.cbOverrideMinFillOpacityChange(Sender: TObject);
begin
  FPatternOptions.OverrideMinFillOpacity := cbOverrideMinFillOpacity.Checked;
  seMinFillOpacity.Enabled := FPatternOptions.OverrideMinFillOpacity;
  Paintbox1.Invalidate;
end;

procedure TMainForm.cbOverrideStrokeOpacityChange(Sender: TObject);
begin
  FPatternOptions.OverrideStrokeOpacity := cbOverrideStrokeOpacity.Checked;
  seStrokeOpacity.Enabled := FPatternOptions.OverrideStrokeOpacity;
  Paintbox1.Invalidate;
end;

procedure TMainForm.cmbPatternsChange(Sender: TObject);
begin
  FPatternOptions.PatternType := TGeoPatternType(cmbPatterns.ItemIndex);
  Paintbox1.Invalidate;
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  if not FActivated then
  begin
    FActivated := true;
    Constraints.MinHeight := Panel1.Height +
      Label1.Top + Label1.Height +
      ParamsPanel.BorderSpacing.Bottom;
  end;
end;

procedure TMainForm.seOpacityChange(Sender: TObject);
begin
  if Sender = seMaxFillOpacity then
    FPatternOptions.MaxFillOpacity := seMaxFillOpacity.Value
  else
  if Sender = seMinFillOpacity then
    FPatternOptions.MinFillOpacity := seMinFillOpacity.Value
  else
  if Sender = seStrokeOpacity then
    FPatternOptions.StrokeOpacity := seStrokeOpacity.Value;
  Paintbox1.Invalidate;
end;

procedure TMainForm.seMinFillOpacityChange(Sender: TObject);
begin
  FPatternOptions.MinFillOpacity := seMinFillOpacity.Value;
  Paintbox1.Invalidate;
end;

procedure TMainForm.seStrokeOpacityChange(Sender: TObject);
begin
  FPatternOptions.StrokeOpacity := seStrokeOpacity.Value;
  Paintbox1.Invalidate;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FPatternOptions.Init;
  TGeoPattern.GetPatternList(cmbPatterns.Items);
  cmbPatterns.ItemIndex := ord(FPatternOptions.PatternType);
  clbBaseColor.ButtonColor := FPatternOptions.BaseColor;
  clbFillColor1.ButtonColor := FPatternOptions.FillColor1;
  clbFillColor2.ButtonColor := FPatternOptions.FillColor2;
  clbStrokeColor.ButtonColor := FPatternOptions.StrokeColor;
  seMaxFillOpacity.Value := FPatternOptions.MaxFillOpacity;
  seMinFillOpacity.Value := FPatternOptions.MinFillOpacity;
  seStrokeOpacity.Value := FPatternOptions.StrokeOpacity;
end;


end.

