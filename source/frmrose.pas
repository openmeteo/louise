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

type
  TFrmRoseDiagram = class(TForm)
    Chart: TChart;
    Series2: TLineSeries;
    rgrpSectionCount: TRadioGroup;
    ColorDialog: TColorDialog;
    btnAlterBrushColor: TButton;
    btnAlterPenColor: TButton;
    chkAxesOverRose: TCheckBox;
    btnChangeStyle: TButton;
    btnCopyClipboard: TButton;
    PrintDialog: TPrintDialog;
    btnPrint: TButton;
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
  private
    FSectionCount, FMax: Integer;
    FPenWidth: Integer;
    FBrushColor, FPenColor: TColor;
    FBrushStyle: TBrushStyle;
    FTimeseries: TTimeseries;
    FSectionStats: TArrayOfInteger;
    FMode: TDrawingMode;
    FStyleIndex: Integer;
    procedure PickDrawingStyle(AStyle: TDrawingStyleRec);
    procedure SetControlStatus;
    function ConvertX(a: Real): Integer;
    function ConvertY(a: Real): Integer;
    function CalcArcPoints(Azimuth, ARadius, AWidth: Real): TArcPoints;
    procedure DrawAxes;
    procedure DrawCircSection(Azimuth, ARadius, AWidth: Real);
    procedure DrawRectSection(Azimuth, ARadius, AWidth: Real);
    procedure CalcSeriesStats;
    procedure DrawRose;
    procedure DrawSolidDiagram;
  public
    property ATimeseries: TTimeseries read FTimeseries write FTimeseries;
  end;


implementation

{$R *.dfm}

uses Math;

{ Math - General functions }

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
  i, j: Integer;
  AWidth, AValue: Real;
begin
  AWidth := 360/FSectionCount;
  SetLength(FSectionStats, FSectionCount);
  for i := 0 to FSectionCount-1 do FSectionStats[i] := 0;
  for i := 0 to FTimeseries.Count-1 do
  begin
    if FTimeseries[i].IsNull then Continue;
    AValue := FTimeseries[i].AsFloat;
    while AValue<(-0.5*AWidth) do AValue := AValue + 360;
    while AValue>=(360-0.5*AWidth) do AValue := AValue - 360;
    j := Floor((AValue+AWidth*0.5)/AWidth);
    Assert(j>-1);
    Assert(j<FSectionCount);
    Inc(FSectionStats[j]);
  end;
end;

{ Event handlers }

procedure TFrmRoseDiagram.FormCreate(Sender: TObject);
begin
  FTimeseries := nil;
  PickDrawingStyle(DrawingStyles[0]);
  FStyleIndex := 0;
  FSectionCount := 36;
end;

procedure TFrmRoseDiagram.FormResize(Sender: TObject);
begin
  Chart.Height := Min(ClientHeight, ClientWidth)-10;
  Chart.Width := Chart.Height;
end;

procedure TFrmRoseDiagram.FormShow(Sender: TObject);
begin
  SetControlStatus;
end;

procedure TFrmRoseDiagram.rgrpSectionCountClick(Sender: TObject);
begin
  with (Sender as TRadioGroup) do
    FSectionCount := StrToInt(Items[ItemIndex]);
  SetControlStatus;
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
  SetControlStatus;
end;

procedure TFrmRoseDiagram.btnChangeStyleClick(Sender: TObject);
begin
  Inc(FStyleIndex);
  if FStyleIndex>Length(DrawingStyles)-1 then FStyleIndex := 0;
  PickDrawingStyle(DrawingStyles[FStyleIndex]);
  SetControlStatus;
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
end;

procedure TFrmRoseDiagram.chkAxesOverRoseClick(Sender: TObject);
begin
  SetControlStatus;
end;

{ Set control status }

procedure TFrmRoseDiagram.SetControlStatus;
var
  i:Integer;
  ACursor: TCursor;
begin
  ACursor := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;
    FMax := 0;
    with rgrpSectionCount do
      ItemIndex := Items.IndexOf(IntToStr(FSectionCount));
    CalcSeriesStats;
    for i := 0 to Length(FSectionStats)-1 do
      if FSectionStats[i]>FMax then FMax := FSectionStats[i];
    Chart.BottomAxis.Minimum := -FMax-1;
    Chart.LeftAxis.Minimum := -FMax-1;
    Chart.BottomAxis.Maximum := FMax+1;
    Chart.LeftAxis.Maximum := FMax+1;
    Chart.Refresh;
  finally
    Screen.Cursor := ACursor;
  end;
end;

{ Drawing functions }

procedure TFrmRoseDiagram.DrawRose;
var
  i: Integer;
begin
  case FMode of
    rdmCircSections:
      for i := 0 to Length(FSectionStats)-1 do
        DrawCircSection(i*2*Pi/FSectionCount, FSectionStats[i],
          2*Pi/FSectionCount);
    rdmRectSections:
      for i := 0 to Length(FSectionStats)-1 do
        DrawRectSection(i*2*Pi/FSectionCount, FSectionStats[i],
          2*Pi/FSectionCount);
    rdmHalfRectSections:
      for i := 0 to Length(FSectionStats)-1 do
        DrawRectSection(i*2*Pi/FSectionCount, FSectionStats[i],
          Pi/FSectionCount);
    rdmSolidDiagram:
      DrawSolidDiagram;
    else
      Assert(False);
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

procedure TFrmRoseDiagram.DrawSolidDiagram;
var
  i: Integer;
  a: TArrayOfTPoint;
  AArcPoints: TArcPoints;
begin
  SetLength(a, Length(FSectionStats)+1);
  for i := 0 to Length(FSectionStats)-1 do
  begin
    AArcPoints := CalcArcPoints(i*2*Pi/FSectionCount, FSectionStats[i],
    Pi/FSectionCount);
    a[i] := AArcPoints.MidArc;
  end;
  a[Length(FSectionStats)] := a[0];
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
        ActualRadius := i*100 / (16*FSectionCount);
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
