{******************************************************************}
{                                                                  }
{  LoUISE library                                                  }
{                                                                  }
{  Copyright (c) 2000-2004 National Technical University of Athens }
{                                                                  }
{******************************************************************}

unit penmdlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, tsgrid;

type
  TFrmPenman = class(TForm)
    BtnOK: TButton;
    BtnCancel: TButton;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    EdtA_e: TEdit;
    EdtA_L: TEdit;
    EdtA_s: TEdit;
    EdtB_e: TEdit;
    EdtB_L: TEdit;
    EdtB_s: TEdit;
    BtnDefaultParms: TButton;
    EdtLatDeg: TEdit;
    EdtLatMin: TEdit;
    EdtLatSec: TEdit;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    rgrpCalculationType: TRadioGroup;
    pnlPenman: TPanel;
    EdtAltitude: TEdit;
    Label15: TLabel;
    Label10: TLabel;
    EdtAlbedo: TEdit;
    Label11: TLabel;
    grpParams: TGroupBox;
    lblAlpha: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    edtAlphaParam: TEdit;
    edtBetaParam: TEdit;
    edtCeParam: TEdit;
    grpBlaneyCriddle: TGroupBox;
    edtKc: TEdit;
    Label18: TLabel;
    rgrpSunshine: TRadioGroup;
    btnCalcParams: TButton;
    edtDetermination: TEdit;
    lblDeterminationFactor: TLabel;
    rgrpMonthDay: TRadioGroup;
    procedure BtnDefaultParmsClick(Sender: TObject);
    procedure rgrpCalculationTypeClick(Sender: TObject);
    procedure IFormShow(Sender: TObject);
    procedure btnCalcParamsClick(Sender: TObject);
  private
    procedure SetParmsToDefaults;
    procedure SetControlStatus;
    procedure CalculateParams;
  public
    DefaultA_e, DefaultB_e: Real;
    DefaultA_L, DefaultB_L: Real;
    DefaultA_s, DefaultB_s: Real;
    DefaultA_e_PM, DefaultB_e_PM: Real;
    DefaultAlbedo, DefaultAlbedo_PM: Real;
    TimeseriesGrid: TTimeseriesGrid;
    TemperatureIndex, EvaporationIndex: Integer;
  end;

var
  FrmPenman: TFrmPenman;

implementation

uses
  tsevap;

{$R *.DFM}

procedure TFrmPenman.SetParmsToDefaults;
begin
  EdtA_L.Text := Format('%.2f', [DefaultA_L]);
  EdtB_L.Text := Format('%.2f', [DefaultB_L]);
  EdtA_s.Text := Format('%.2f', [DefaultA_s]);
  EdtB_s.Text := Format('%.2f', [DefaultB_s]);
  case rgrpCalculationType.ItemIndex of
    0,2..5:
    begin
      EdtA_e.Text := Format('%.2f', [DefaultA_e]);
      EdtB_e.Text := Format('%.3f', [DefaultB_e]);
      EdtAlbedo.Text := Format('%.2f', [DefaultAlbedo]);
    end;
    1:
    begin
      EdtA_e.Text := Format('%.2f', [DefaultA_e_PM]);
      EdtB_e.Text := Format('%.3f', [DefaultB_e_PM]);
      EdtAlbedo.Text := Format('%.2f', [DefaultAlbedo_PM]);      
    end;
  else
    Assert(False);
  end;
end;

procedure TFrmPenman.BtnDefaultParmsClick(Sender: TObject);
begin
  SetParmsToDefaults;
end;

procedure TFrmPenman.rgrpCalculationTypeClick(Sender: TObject);
begin
  SetControlStatus;
  SetParmsToDefaults;
end;

procedure TFrmPenman.IFormShow(Sender: TObject);
begin
  SetControlStatus;
end;

procedure TFrmPenman.SetControlStatus;
begin
  case rgrpCalculationType.ItemIndex of
    0,1:
    begin
      GroupBox1.Visible := True;
      pnlPenman.Visible := True;
      grpParams.Visible := False;
      grpBlaneyCriddle.Visible := False;
      rgrpSunshine.Enabled := True;
      rgrpMonthDay.Enabled := True;
    end;
    2:
    begin
      GroupBox1.Visible := False;
      pnlPenman.Visible := False;
      grpParams.Visible := False;
      grpBlaneyCriddle.Visible := False;
      rgrpSunshine.Enabled := False;
      rgrpMonthDay.Enabled := False;
    end;
    3:
    begin
      GroupBox1.Visible := False;
      pnlPenman.Visible := False;
      grpParams.Visible := False;
      grpBlaneyCriddle.Visible := True;
      rgrpSunshine.Enabled := False;
      rgrpMonthDay.Enabled := False;
    end;
    4:
    begin
      GroupBox1.Visible := False;
      pnlPenman.Visible := False;
      grpParams.Visible := False;
      grpBlaneyCriddle.Visible := False;
      rgrpSunshine.Enabled := False;
      rgrpMonthDay.Enabled := True;
    end;
    5:
    begin
      GroupBox1.Visible := False;
      pnlPenman.Visible := False;
      grpParams.Visible := True;
      grpBlaneyCriddle.Visible := False;
      rgrpSunshine.Enabled := False;
      rgrpMonthDay.Enabled := True;
    end;
    else
      Assert(False);
  end;
  btnCalcParams.Enabled :=
    (TimeseriesGrid<>nil) and (rgrpCalculationType.ItemIndex=5) and
      (EvaporationIndex>-1);
  lblDeterminationFactor.Enabled := btnCalcParams.Enabled;
  edtDetermination.Enabled := btnCalcParams.Enabled;
  edtDetermination.Text := '';
end;

procedure TFrmPenman.btnCalcParamsClick(Sender: TObject);
begin
  CalculateParams;
end;

resourcestring
  rsEvapNotDef = 'Evaporation time series for calibration not defined';

procedure TFrmPenman.CalculateParams;
var
  Alpha, Beta, Ce, Latitude: Real;
  ACursor: TCursor;
  AMonthlyDay: TPenmanMonthlyDay;
begin
  Assert(TimeseriesGrid<>nil);
  Assert(grpParams.Enabled);
  AMonthlyDay := pmodRepresentative;
  case rgrpMonthDay.ItemIndex of
    0: AMonthlyDay := pmodRepresentative;
    1: AMonthlyDay := pmodMiddle;
    else Assert(False);
  end;
  if EvaporationIndex < 0 then raise Exception.Create(rsEvapNotDef);
  Latitude := StrToFloat(EdtLatDeg.Text)+ StrToFloat(EdtLatMin.Text)/60+
    StrToFloat(EdtLatSec.Text)/3600;
  ACursor := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;
    edtDetermination.Text := FormatFloat('0.000',
      ParametricEvapFind(TimeseriesGrid.Data[TemperatureIndex],
        TimeseriesGrid.Data[EvaporationIndex], Latitude, Alpha, Beta, Ce,
          AMonthlyDay));
    edtAlphaParam.Text := FormatFloat('0.00E+0', Alpha);
    edtBetaParam.Text := FormatFloat('0.000', Beta);
    edtCeParam.Text := FormatFloat('0.00E+0',Ce);
  finally
    Screen.Cursor := ACursor;
  end;
end;

end.
