{******************************************************************}
{                                                                  }
{  LoUISE library                                                  }
{                                                                  }
{  Copyright (c) 2000-2009 National Technical University of Athens }
{                                                                  }
{******************************************************************}


unit frmrgopt;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, Grids, tsprocess, StdCtrls;

type
  TFrmRegressionOptimizationOptions = class(TForm)
    StringGrid: TStringGrid;
    btnOK: TButton;
    btnCancel: TButton;
    procedure FormShow(Sender: TObject);
    procedure StringGridSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
  private
    procedure SetControlStatus;
  public
    OptimizationArray: TSeasonalOptimizeRegressionOptions;
  end;

implementation

{$R *.dfm}

resourcestring
  rsNotUsed = 'Not used';
  rsUse = 'Always used';
  rsAuto = 'Automatic';

function GetOptionString(AOption: TOptimizeRegressionOption): string;
begin
  Result := '';
  case AOption of
    oroDoNotUse: Result := rsNotUsed;
    oroUse: Result := rsUse;
    oroAuto: Result := rsAuto;
  else
    Assert(False);
  end;
end;

procedure TFrmRegressionOptimizationOptions.FormShow(Sender: TObject);
var
  i: Integer;
begin
  with StringGrid do
  begin
    DefaultRowHeight := DefaultRowHeight * Screen.PixelsPerInch div 96;
    RowCount := Length(OptimizationArray)+1;
    ColCount := Length(OptimizationArray[0].TimeseriesSettings)+3;
    for i := 1 to RowCount-1 do
      Cells[0, i] := IntToStr(i);
    for i := 1 to ColCount-2 do
      Cells[i, 0] := 'Variable: '+IntToStr(i);
    Cells[ColCount-2, 0] := 'Max time series';
    Cells[ColCount-1, 0] := 'Only Positives';
  end;
  SetControlStatus;
end;

procedure TFrmRegressionOptimizationOptions.SetControlStatus;
var
  i, j: Integer;
begin
  for i := 0 to Length(OptimizationArray)-1 do
  begin
    for j := 0 to Length(OptimizationArray[i].TimeseriesSettings)-1 do
      StringGrid.Cells[j+1,i+1] :=
        GetOptionString(OptimizationArray[i].TimeseriesSettings[j]);
    StringGrid.Cells[StringGrid.ColCount-2, i+1] :=
      IntToStr(OptimizationArray[i].MaxTimeseries);
    StringGrid.Cells[StringGrid.ColCount-1, i+1] :=
      BoolToStr(OptimizationArray[i].OnlyPositiveCoefficients, True);
  end;
end;

procedure TFrmRegressionOptimizationOptions.StringGridSelectCell(
  Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
begin
  with OptimizationArray[ARow-1] do
  begin
    if ACol<StringGrid.ColCount-2 then
    begin
      case TimeseriesSettings[ACol-1] of
        oroDoNotUse: TimeseriesSettings[ACol-1] := oroUse;
        oroUse: TimeseriesSettings[ACol-1] := oroAuto;
        oroAuto: TimeseriesSettings[ACol-1] := oroDoNotUse;
      end;
    end else if ACol=StringGrid.ColCount-2 then
    begin
      Inc(MaxTimeseries);
      if MaxTimeseries>Length(TimeseriesSettings) then
        MaxTimeseries := 1;
    end else
    begin
      OnlyPositiveCoefficients := not OnlyPositiveCoefficients;
    end
  end;
  SetControlStatus;
end;

end.
