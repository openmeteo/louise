{******************************************************************}
{                                                                  }
{  LoUISE library                                                  }
{                                                                  }
{  Copyright (c) 2000-09 National Technical University of Athens   }
{                                                                  }
{******************************************************************}

unit frmrose;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Series, TeEngine, TeeShape, ExtCtrls, TeeProcs, Chart, ts, StdCtrls;

type
  TArcPoints = record
    Center, StartArc, MidArc, EndArc: TPoint;
  end;

type
  TDrawingMode = (rdmCircSections, rdmRectSections, rdmHalfRectSections,
    rdmSolidDiagram);

type
  TDrawingStyleRec = record
    PenWidth: Integer;
    PenColor, BrushColor: TColor;
    BrushStyle: TBrushStyle;
    Mode: TDrawingMode;
  end;

const
  DrawingStyles: array[0..4] of TDrawingStyleRec =
  (( PenWidth: 1;
     PenColor: clGray;
     BrushColor: clOlive;
     BrushStyle: bsSolid;
     Mode: rdmCircSections ),
   ( PenWidth: 1;
     PenColor: clRed;
     BrushColor: clLtGray;
     BrushStyle: bsSolid;
     Mode: rdmRectSections ),
   ( PenWidth: 1;
     PenColor: clBlack;
     BrushColor: clNavy;
     BrushStyle: bsSolid;
     Mode: rdmHalfRectSections ),
   ( PenWidth: 2;
     PenColor: clGreen;
     BrushColor: clYellow;
     BrushStyle: bsSolid;
     Mode: rdmSolidDiagram ),
   ( PenWidth: 2;
     PenColor: clNavy;
     BrushColor: clYellow;
     BrushStyle: bsClear;
     Mode: rdmSolidDiagram )
  );

type TArrayOfInteger = array of Integer;

type T2DArrayOfInteger = array of TArrayOfInteger;

type
  TFrmRoseDiagram = class(TForm)
    Chart: TChart;
    rgrpSectionCount: TRadioGroup;
    ColorDialog: TColorDialog;
    btnAlterBrushColor: TButton;
    btnAlterPenColor: TButton;
    chkAxesOverRose: TCheckBox;
    btnChangeStyle: TButton;
    btnCopyClipboard: TButton;
    PrintDialog: TPrintDialog;
    btnPrint: TButton;
    grpSpeedDistribution: TGroupBox;
    chkLogScales: TCheckBox;
    chkPenColorSameToBrush: TCheckBox;
    rgrpSpeedClasses: TRadioGroup;
    chkShowLegend: TCheckBox;
    Series5: TBarSeries;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ChartBeforeDrawSeries(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure rgrpSectionCountClick(Sender: TObject);
    procedure btnAlterPenColorClick(Sender: TObject);
    procedure chkAxesOverRoseClick(Sender: TObject);
    procedure btnChangeStyleClick(Sender: TObject);
    procedure btnCopyClipboardClick(Sender: TObject);
    procedure btnPrintClick(Sender: TObject);
    procedure chkLogScalesClick(Sender: TObject);
    procedure rgrpSpeedClassesClick(Sender: TObject);
  private
    FSectionCount, FMax, FClassesCount: Integer;
    FMaxSpeed: Real;
    FPenWidth: Integer;
    FBrushColor, FPenColor: TColor;
    FBrushStyle: TBrushStyle;
    FDirectionTimeseries, FSpeedTimeseries: TTimeseries;
    FSectionStats: T2DArrayOfInteger;
    FMode: TDrawingMode;
    FStyleIndex: Integer;
    FLogScale: Boolean;
    FSpeedDisplay: Boolean;
    FLegendPosition: Integer;
    FMaximumPercent: Real;
    function ScaleFun(AValue: Real): Real;
    procedure PickDrawingStyle(AStyle: TDrawingStyleRec);
    procedure SetControlStatus; overload;
    procedure SetControlStatus(FullInvalidate: Boolean); overload;
    function ConvertX(a: Real): Integer;
    function ConvertY(a: Real): Integer;
    function CalcArcPoints(Azimuth, ARadius, AWidth: Real): TArcPoints;
    procedure DrawAxes;
    procedure DrawCircSection(Azimuth, ARadius, AWidth: Real);
    procedure DrawRectSection(Azimuth, ARadius, AWidth: Real);
    procedure CalcSeriesStats;
    procedure DrawRose;
    procedure DrawSolidDiagram(AClass: Integer);
    procedure DrawLegend;
  public
    property ATimeseries: TTimeseries read FDirectionTimeseries write
      FDirectionTimeseries;
    property SpeedTimeseries: TTimeseries read FSpeedTimeseries write
      FSpeedTimeseries;
  end;

implementation

{$R *.dfm}

uses Math, tsprocess, Contnrs, Dates;

{ Math - General functions }

type
TRGB=record
  R, G, B: Byte;
end;

function HueToRGB(AHue: Real): TRGB;
begin
  with Result do
  begin
    if AHue<0.2 then
    begin
      R := Round(AHue*255/0.2);
      G := 0;
      B := 255;
    end else if AHue<0.4 then begin
      R := 255;
      G := 0;
      B := Round((0.4-AHue)*255/0.2);
    end else if AHue<0.6 then begin
      R := 255;
      G := Round((AHue-0.4)*255/0.2);
      B := 0;
    end else if AHue<0.8 then begin
      R := Round((0.8-AHue)*255/0.2);
      G := 255;
      B := 0;
    end else begin
      R := 0;
      G := 255;
      B := Round((AHue-0.8)*255/0.2);
    end;
  end;
end;

function RGBToColor(ARGB: TRGB): TColor;
begin
  Result:=ARGB.B Shl 16 Or
          ARGB.G Shl 8  Or
          ARGB.R;
end;

function HueToColor(AHue: Real): TColor;
begin
  Result := RGBToColor(HueToRGB(AHue));
end;

procedure TFrmRoseDiagram.PickDrawingStyle(AStyle: TDrawingStyleRec);
begin
  with AStyle do
  begin
    FBrushColor := BrushColor;
    FBrushStyle := BrushStyle;
    FPenWidth := PenWidth;
    FPenColor := PenColor;
    FMode := Mode;
  end;
end;

function TFrmRoseDiagram.ConvertX(a: Real): Integer;
begin
  with Chart.BottomAxis do
    Result := Round((a - Minimum) / (Maximum - Minimum) *
      (IEndPos - IStartPos)) + IStartPos;
end;

function TFrmRoseDiagram.ConvertY(a: Real): Integer;
begin
  with Chart.LeftAxis do
    Result := IEndPos - Round((a - Minimum) /
      (Maximum - Minimum) * (IEndPos - IStartPos));
end;

function TFrmRoseDiagram.ScaleFun(AValue: Real): Real;
begin
  if FLogScale then Result := Log10(AValue+0.001) else
  Result := AValue;
end;

function TFrmRoseDiagram.CalcArcPoints(Azimuth, ARadius, AWidth: Real):
  TArcPoints;
begin
  Result.Center.X := ConvertX(0);
  Result.Center.Y := ConvertY(0);
  Result.StartArc.X := ConvertX(ARadius*sin(AZimuth-AWidth/2));
  Result.StartArc.Y := ConvertY(ARadius*cos(AZimuth-AWidth/2));
  Result.EndArc.X := ConvertX(ARadius*sin(AZimuth+AWidth/2));
  Result.EndArc.Y := ConvertY(ARadius*cos(AZimuth+AWidth/2));
  Result.MidArc.X := ConvertX(ARadius*sin(AZimuth));
  Result.MidArc.Y := ConvertY(ARadius*cos(AZimuth));
end;

procedure TFrmRoseDiagram.CalcSeriesStats;
var
  i, j, k, a, b: Integer;
  AWidth, AValue, ASpeed: Real;
  ATimeseriesList: TObjectList;
  ACommonPeriod: TDateTimeList;
  ADenominator: Integer;
begin
  AWidth := 360/FSectionCount;
  SetLength(FSectionStats, FClassesCount);
  for i := 0 to FClassesCount-1 do
  begin
    SetLength(FSectionStats[i], FSectionCount);
    for j := 0 to FSectionCount-1 do FSectionStats[i][j] := 0;
  end;
  FMaxSpeed := 0;
  if FSpeedTimeseries <> nil then
    with FSpeedTimeseries do
      for i := 0 to Count-1 do
        if not Items[i].IsNull then if Items[i].AsFloat>FMaxSpeed then
          FMaxSpeed := Items[i].AsFloat;
  ATimeseriesList := nil;
  ACommonPeriod := nil;
  b := -1;
  ASpeed := 0;
  try
    ATimeseriesList := TObjectList.Create(False);
    ATimeseriesList.Add(FDirectionTimeseries);
    if FSpeedTimeseries<>nil then ATimeseriesList.Add(FSpeedTimeseries);
    ACommonPeriod := GetCommonPeriod(ATimeseriesList, 0);
    for k := 0 to FClassesCount-1 do
      for i := 0 to ACommonPeriod.Count-1 do
      begin
        a := FDirectionTimeseries.IndexOf(ACommonPeriod[i]);
        if FSpeedTimeseries<>nil then
          b := FSpeedTimeseries.IndexOf(ACommonPeriod[i]);
        AValue := FDirectionTimeseries[a].AsFloat;
        if FSpeedTimeseries<>nil then ASpeed := FSpeedTimeseries[b].AsFloat;
        if FSpeedTimeseries<>nil then if ASpeed<0 then
        begin
          ASpeed := Abs(ASpeed);
          AValue := AValue + 180;
        end;
        while AValue<(-0.5*AWidth) do AValue := AValue + 360;
        while AValue>=(360-0.5*AWidth) do AValue := AValue - 360;
        j := Floor((AValue+AWidth*0.5)/AWidth);
        Assert(j>-1);
        Assert(j<FSectionCount);
        if k>0 then
        begin
          if ScaleFun(ASpeed)>
            ( (FClassesCount-k)*ScaleFun(FMaxSpeed)/FClassesCount ) then
            Continue;
        end;
        Inc(FSectionStats[k][j]);
      end;
  finally
    ATimeseriesList.Free;
    ACommonPeriod.Free;
  end;
  FMax := 0;
  ADenominator := 0;
  for i := 0 to Length(FSectionStats[0])-1 do
  begin
    if FSectionStats[0][i]>FMax then FMax := FSectionStats[0][i];
    ADenominator := ADenominator + FSectionStats[0][i];
  end;
  if ADenominator > 0 then FMaximumPercent := FMax / ADenominator else
    FMaximumPercent := 1;
end;

{ Event handlers }

procedure TFrmRoseDiagram.FormCreate(Sender: TObject);
begin
  FDirectionTimeseries := nil;
  FSpeedTimeseries := nil;
  PickDrawingStyle(DrawingStyles[0]);
  FStyleIndex := 0;
  FSectionCount := 36;
  FClassesCount := 1;
  FLegendPosition := 0;
end;

procedure TFrmRoseDiagram.FormResize(Sender: TObject);
begin
  Chart.Height := Min(ClientHeight, ClientWidth)-10;
  Chart.Width := Chart.Height;
end;

procedure TFrmRoseDiagram.FormShow(Sender: TObject);
begin
  FSpeedDisplay := FSpeedTimeseries<>nil;
  if FSpeedDisplay then FClassesCount := 8 else FClassesCount := 1;
  SetControlStatus;
end;

procedure TFrmRoseDiagram.rgrpSectionCountClick(Sender: TObject);
begin
  with (Sender as TRadioGroup) do
    FSectionCount := StrToInt(Items[ItemIndex]);
  SetControlStatus(True);
end;

procedure TFrmRoseDiagram.rgrpSpeedClassesClick(Sender: TObject);
begin
  with (Sender as TRadioGroup) do
    FClassesCount := StrToInt(Items[ItemIndex]);
  SetControlStatus(True);
end;

procedure TFrmRoseDiagram.btnAlterPenColorClick(Sender: TObject);
begin
  if (Sender as TButton).Tag = 0 then
    ColorDialog.Color := FPenColor else
    ColorDialog.Color := FBrushColor;
  if not ColorDialog.Execute then Exit;
  if (Sender as TButton).Tag = 0 then
    FPenColor := ColorDialog.Color else
    FBrushColor := ColorDialog.Color;
  SetControlStatus(False);
end;

procedure TFrmRoseDiagram.btnChangeStyleClick(Sender: TObject);
begin
  Inc(FStyleIndex);
  if FStyleIndex>Length(DrawingStyles)-1 then FStyleIndex := 0;
  PickDrawingStyle(DrawingStyles[FStyleIndex]);
  SetControlStatus(False);
end;

procedure TFrmRoseDiagram.btnCopyClipboardClick(Sender: TObject);
begin
  Chart.CopyToClipboardMetafile(True);
end;

procedure TFrmRoseDiagram.btnPrintClick(Sender: TObject);
begin
  if PrintDialog.Execute then
    Chart.Print;
end;

procedure TFrmRoseDiagram.ChartBeforeDrawSeries(Sender: TObject);
begin
  with Chart.Canvas do
    Font.PixelsPerInch := Chart.PrintResolution;
  if not chkAxesOverRose.Checked then DrawAxes;
  DrawRose;
  if chkAxesOverRose.Checked then DrawAxes;
  if (FSpeedDisplay and chkShowLegend.Checked) then DrawLegend;
end;

procedure TFrmRoseDiagram.chkAxesOverRoseClick(Sender: TObject);
begin
  SetControlStatus((Sender as TCheckBox).Tag=1);
  if Sender = chkShowLegend then
  begin
    if chkShowLegend.Checked then Exit;
    Inc(FLegendPosition);
    if FLegendPosition>3 then FLegendPosition := 0;
  end;
end;

procedure TFrmRoseDiagram.chkLogScalesClick(Sender: TObject);
begin
  FLogScale := (Sender as TCheckBox).Checked;
  SetControlStatus;
end;

{ Set control status }

procedure TFrmRoseDiagram.SetControlStatus;
begin
  SetControlStatus(True);
end;

procedure TFrmRoseDiagram.SetControlStatus(FullInvalidate: Boolean);
var
  ACursor: TCursor;
begin
  btnAlterBrushColor.Visible := not FSpeedDisplay;
  grpSpeedDistribution.Visible := FSpeedDisplay;
  chkLogScales.Checked := FLogScale;
  btnAlterPenColor.Visible := not chkPenColorSameToBrush.Checked;
  ACursor := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;
    with rgrpSectionCount do
      ItemIndex := Items.IndexOf(IntToStr(FSectionCount));
    if FSpeedDisplay then
      with rgrpSpeedClasses do
        ItemIndex := Items.IndexOf(IntToStr(FClassesCount));
    if FullInvalidate then
      CalcSeriesStats;
    Chart.BottomAxis.Minimum := -FMax*1.05;
    Chart.LeftAxis.Minimum := -FMax*1.05;
    Chart.BottomAxis.Maximum := FMax*1.05;
    Chart.LeftAxis.Maximum := FMax*1.05;
    Chart.Refresh;
  finally
    Screen.Cursor := ACursor;
  end;
end;

{ Drawing functions }

procedure TFrmRoseDiagram.DrawRose;
var
  i, j: Integer;
begin
  for j := 0 to FClassesCount-1 do
  begin
    if FClassesCount>1 then
      FBrushColor := HueToColor((FClassesCount-1-j)/(FClassesCount-1));
    if chkPenColorSameToBrush.Checked then
      FPenColor := FBrushColor;
    case FMode of
      rdmCircSections:
        for i := 0 to Length(FSectionStats[j])-1 do
          DrawCircSection(i*2*Pi/FSectionCount, FSectionStats[j][i],
            2*Pi/FSectionCount);
      rdmRectSections:
        for i := 0 to Length(FSectionStats[j])-1 do
          DrawRectSection(i*2*Pi/FSectionCount, FSectionStats[j][i],
            2*Pi/FSectionCount);
      rdmHalfRectSections:
        for i := 0 to Length(FSectionStats[j])-1 do
          DrawRectSection(i*2*Pi/FSectionCount, FSectionStats[j][i],
            Pi/FSectionCount);
      rdmSolidDiagram:
        DrawSolidDiagram(j);
      else
        Assert(False);
    end;
  end;
end;

procedure TFrmRoseDiagram.DrawCircSection(Azimuth, ARadius, AWidth: Real);
var
  a: array[0..6] of TPoint;
  AArcPoints: TArcPoints;
begin
  AArcPoints := CalcArcPoints(Azimuth, ARadius, AWidth);
  a[0] := AArcPoints.Center;
  a[1] := AArcPoints.StartArc;
  a[3] := AArcPoints.MidArc;
  a[5] := AArcPoints.EndArc;
  a[6] := AArcPoints.Center;
  AArcPoints := CalcArcPoints(Azimuth, ARadius, AWidth/2);
  a[2] := AArcPoints.StartArc;
  a[4] := AArcPoints.EndArc;
  with Chart.Canvas do
  begin
    Brush.Color := FBrushColor;
    Brush.Style := FBrushStyle;
    Pen.Color := FPenColor;
    Pen.Width := FPenWidth;
    Pen.Style := psSolid;
    Polygon(a);
  end;
end;

procedure TFrmRoseDiagram.DrawRectSection(Azimuth, ARadius, AWidth: Real);
var
  a: array[0..3] of TPoint;
  AArcPoints: TArcPoints;
begin
  AArcPoints := CalcArcPoints(Azimuth, ARadius, AWidth);
  a[0] := AArcPoints.Center;
  a[1] := AArcPoints.StartArc;
  a[2] := AArcPoints.EndArc;
  a[3] := AArcPoints.Center;
  with Chart.Canvas do
  begin
    Brush.Color := FBrushColor;
    Brush.Style := FBrushStyle;
    Pen.Color := FPenColor;
    Pen.Width := FPenWidth;
    Pen.Style := psSolid;
    Polygon(a);
  end;
end;

type
  TArrayOfTPoint = array of TPoint;

procedure TFrmRoseDiagram.DrawSolidDiagram(AClass: Integer);
var
  i: Integer;
  a: TArrayOfTPoint;
  AArcPoints: TArcPoints;
begin
  SetLength(a, Length(FSectionStats[AClass])+1);
  for i := 0 to Length(FSectionStats[AClass])-1 do
  begin
    AArcPoints := CalcArcPoints(i*2*Pi/FSectionCount, FSectionStats[AClass][i],
    Pi/FSectionCount);
    a[i] := AArcPoints.MidArc;
  end;
  a[Length(FSectionStats[AClass])] := a[0];
  with Chart.Canvas do
  begin
    Brush.Color := FBrushColor;
    Brush.Style := FBrushStyle;
    Pen.Color := FPenColor;
    Pen.Width := FPenWidth;
    Pen.Style := psSolid;
    Polygon(a);
  end;
end;

procedure TFrmRoseDiagram.DrawLegend;
var
  i: Integer;
  x1, x2, y1, y2: Integer;
  xoffset, yoffset: Integer;
  rwidth, rheight: Integer;
  s: string;
begin
  x1 := ConvertX(-FMax*1.02);
  y1 := ConvertY(FMax*1.05);
  x2 := ConvertX(-FMax*1.02+FMax/5);
  y2 := ConvertY(FMax*1.00-FClassesCount*FMax/19);
  rwidth := x2-x1;
  rheight := y2-y1;
  with Chart do
    case FLegendPosition of
      0: begin xoffset := 0; yoffset := 0; end;
      1: begin xoffset := Width-rwidth-2*x1; yoffset := 0; end;
      2: begin xoffset := Width-rwidth-2*x1; yoffset := Height-rheight-2*y1; end;
      3: begin xoffset := 0; yoffset := Height-rheight-2*y1; end;
    end;
  with Chart.Canvas do
  begin
    Brush.Color := clWhite;
    Pen.Color := clBlack;
    Pen.Style := psSolid;
    Pen.Width := 1;
    Brush.Style := bsSolid;
    Rectangle(xoffset+x1, yoffset+y1, xoffset+x2, yoffset+y2);
    Font.Orientation := 0;
    Font.Height := -11;
    x1 := ConvertX(-FMax);
    x2 := ConvertX(-FMax+FMax/20);
    TextOut(x2+2+xoffset, ConvertY(FMax*1.045)+yoffset, '0');
    for i := 0 to FClassesCount-1 do
    begin
      Brush.Color := HueToColor(i/(FClassesCount-1));
      Pen.Color := Brush.Color;
      y1 := ConvertY(FMax*1.03 - i*FMax/19);
      y2 := ConvertY(FMax*1.03 - i*FMax/19 - FMax/20);
      Rectangle(xoffset+x1, yoffset+y1, xoffset+x2, yoffset+y2);
      Brush.Color := clWhite;
      s := FormatFloat('0.00', (i+1)*FMaxSpeed/(FClassesCount));
      TextOut(x2+2+xoffset, (y1+y2) div 2+yoffset, s);
    end;
  end;

end;

procedure TFrmRoseDiagram.DrawAxes;
var
  i: Integer;
  ARadius, ActualRadius: Real;
begin
  with Chart.Canvas do
  begin
    Pen.Color := clBlack;
    Pen.Style := psSolid;
    Brush.Style := bsClear;
    Pen.Width := 2;
    Font.Height := Max(-13,Min(-Round(12*Chart.Width/518),-11));
    Line(ConvertX(-FMax*1.05), ConvertY(0), ConvertX(FMax*1.05), ConvertY(0));
    Font.Style := [fsBold];
    Font.Orientation := 900;
    TextOut(ConvertX(-FMax*1.05), ConvertY(0)-TextWidth('W') div 2, 'W');
    TextOut(ConvertX(FMax*1.05)-FontHeight, ConvertY(0)+
      TextWidth('E') * 4 div 3, 'E');
    Line(ConvertX(0), ConvertY(-FMax*1.05), ConvertX(0), ConvertY(FMax*1.05));
    Font.Orientation := 0;
    TextOut(ConvertX(0)-TextWidth('S')*4 div 3,
      ConvertY(-FMax*1.05)-FontHeight div 2, 'S');
    TextOut(ConvertX(0)+TextWidth('N') div 2, ConvertY(FMax*1.05), 'N');
    Pen.Width := 1;
    Pen.Style := psSolid;
    for i := 1 to 359 do
    begin
      if i mod 10 = 0 then
        ARadius := FMax*1.03 else
      if i mod 5 =0 then ARadius := FMax*1.02
        else ARadius := FMax*1.014;
      Line(ConvertX(FMax*1.01*sin(i*Pi/180)),
        ConvertY(FMax*1.01*cos(i*Pi/180)),
        ConvertX(ARadius*sin(i*Pi/180)),
        ConvertY(ARadius*cos(i*Pi/180)));
    end;
    Pen.Width := 1;
    Pen.Style := psDot;
    Font.Style := [];
    Line(ConvertX(-FMax*1.05/sqrt(2)), ConvertY(-FMax*1.05/sqrt(2)),
      ConvertX(FMax*1.05/sqrt(2)), ConvertY(FMax*1.05/sqrt(2)));
    Line(ConvertX(-FMax*1.05/sqrt(2)), ConvertY(FMax*1.05/sqrt(2)),
      ConvertX(FMax*1.05/sqrt(2)), ConvertY(-FMax*1.05/sqrt(2)));
    TextOut(ConvertX(-FMax*1.07/sqrt(2)), ConvertY(-FMax*1.07/sqrt(2)), 'SW');
    TextOut(ConvertX(FMax*1.07/sqrt(2)), ConvertY(-FMax*1.07/sqrt(2)), 'SE');
    TextOut(ConvertX(-FMax*1.07/sqrt(2)), ConvertY(FMax*1.07/sqrt(2)), 'NW');
    TextOut(ConvertX(FMax*1.07/sqrt(2)), ConvertY(FMax*1.07/sqrt(2)), 'NE');
    Font.Style := [];
    Pen.Style := psDot;
    for i := 1 to 16 do
    begin
      ARadius := (FMax * i) / 16;
      Ellipse(ConvertX(-ARadius), ConvertY(-ARadius), ConvertX(ARadius),
        ConvertY(ARadius));
      if i mod 2 = 0 then
      begin
        ActualRadius := (i /16) * 100 * FMaximumPercent;
        Font.Orientation := 0;
        TextOut(ConvertX(0), ConvertY(ARadius),
          FormatFloat('0.000', ActualRadius)+'%');
        TextOut(ConvertX(0), ConvertY(-ARadius),
          FormatFloat('0.000', ActualRadius)+'%');
        Font.Orientation := 900;
        TextOut(ConvertX(-ARadius), ConvertY(0),
          FormatFloat('0.000', ActualRadius)+'%');
        TextOut(ConvertX(ARadius), ConvertY(0),
          FormatFloat('0.000', ActualRadius)+'%');
        Pen.Style := psDot;
      end else
      begin
        Pen.Style := psSolid;
      end;
    end;
  end;
end;

end.
