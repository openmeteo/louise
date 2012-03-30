{******************************************************************}
{                                                                  }
{  LoUISE library                                                  }
{                                                                  }
{  Copyright (c) 2006    National Technical University of Athens   }
{                                                                  }
{******************************************************************}

unit hymoddlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Menus, ComCtrls, tsgrid, TeeProcs, TeEngine,
  Chart, Series, hydromodel, ts, dates, midi;

type

  TArrayOfInteger = array of Integer;

  TMusicalStyle = record
    StyleName: string;
    DamperPedal: Integer;
    PatchNumber: array[0..10] of Integer;
    Channels: array[0..10] of Integer;
    Sounds: array[0..10] of Integer;
    NotesShift: array[0..10] of Integer;
  end;

const
    MusicalStyles: array[0..9] of TMusicalStyle =
    (
     (StyleName: 'None'; DamperPedal: 127;
        PatchNumber: (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
        Channels: (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
        Sounds: (36, 43, 48, 52, 55, 57, 60, 62, 64, 67, 72);
        NotesShift: (-12,-12,0,0,0,0,0,12,12,12,12)),
     (StyleName: 'Debussy'; DamperPedal: 127;
        PatchNumber: (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
        Channels: (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
        Sounds: (36, 43, 48, 52, 55, 57, 60, 62, 64, 67, 72);
        NotesShift: (-12,-12,-12,0,0,0,0,12,12,12,24)),
     (StyleName: 'Fantasy rain'; DamperPedal: 127;
        PatchNumber: (10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10);
        Channels: (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
        Sounds: (60, 62, 64, 66, 68, 70, 72, 74, 76, 78, 80);
        NotesShift: (10,16,12,18,18,20,22,26,28,36,40)),
     (StyleName: 'Pop corn rain'; DamperPedal: 0;
        PatchNumber: (12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12);
        Channels: (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
        Sounds: (43, 48, 55, 60, 64, 67, 72, 74, 76, 79, 84);
        NotesShift: (-12,0,0,0,0,0,12,12,12,24,24)),
     (StyleName: 'Jazzy'; DamperPedal: 0;
        PatchNumber: (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
        Channels: (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
        Sounds: (48, 51, 55, 58, 60, 62, 63, 65, 66, 67, 70);
        NotesShift: (-12,-12,0,0,0,0,12,12,12,12,24)),
     (StyleName: 'Jazz Trio'; DamperPedal: 0;
        PatchNumber: (0, 0, 0, 32, 0, 0, 0, 0, 0, 0, 0);
        Channels: (9, 0, 0, 1, 0, 0, 0, 9, 0, 0, 0);
        Sounds: (36, 51, 38 ,43, 46, 48, 51, 53, 54, 55, 60);
        NotesShift: (0,12,12,-12,12,12,24,0,24,24,24)),
     (StyleName: 'Drums'; DamperPedal: 0;
        PatchNumber: (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
        Channels: (9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9);
        Sounds: (48, 51, 55, 58, 60, 62, 63, 65, 66, 67, 70);
        NotesShift: (-12,-10,-8,-6,-4,0,4,8,10,12,16)),
     (StyleName: 'Horns'; DamperPedal: 0;
        PatchNumber: (61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61);
        Channels: (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
        Sounds: (48, 55, 60, 64, 67, 70, 74, 78, 82, 84, 87);
        NotesShift: (0,0,0,0,0,0,0,0,0,0,0)),
     (StyleName: 'Major chord'; DamperPedal: 127;
        PatchNumber: (24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24);
        Channels: (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
        Sounds: (36, 40, 43, 48, 52, 55, 60, 64, 67, 72, 76);
        NotesShift: (-12,-12,-12,0,0,0,0,12,12,12,24)),
     (StyleName: 'Major scales'; DamperPedal: 0;
        PatchNumber: (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
        Channels: (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
        Sounds: (48, 50, 52, 53, 55, 57, 59, 60, 62, 64, 65);
        NotesShift: (-12,-10,-5,0,0,7,7,12,12,19,24))
    );

type
  TFrmHydroModel = class(TForm)
    pgcPages: TPageControl;
    tbcSimulation: TTabSheet;
    picSoilTank: TImage;
    picGroundTank: TImage;
    picPercolation: TImage;
    picOutflow: TImage;
    picQi: TImage;
    picQb: TImage;
    picRainfall: TImage;
    picPumping: TImage;
    picEvapotraspiration: TImage;
    picQd: TImage;
    picQs: TImage;
    picStopInnerFlow: TLabel;
    picStopPercolation: TLabel;
    picStopBaseFlow: TLabel;
    picStopOutflow: TLabel;
    picK: TImage;
    LblQd: TLabel;
    lblQs: TLabel;
    lblQi: TLabel;
    lblH1: TLabel;
    lblH2: TLabel;
    lblQb: TLabel;
    lblKsi: TLabel;
    lblLambda: TLabel;
    lblKappa: TLabel;
    lblEpsilon: TLabel;
    lblMi: TLabel;
    lblPhi: TLabel;
    lblK: TLabel;
    lblRainfallTS: TLabel;
    lblSoilTank: TLabel;
    lblGroundTank: TLabel;
    picRunoff: TImage;
    lblS0: TLabel;
    lblY0: TLabel;
    lblPercolation: TLabel;
    lblOutflow: TLabel;
    edtKappa: TEdit;
    chkPercolation: TCheckBox;
    edtMi: TEdit;
    edtEpsilon: TEdit;
    chkInnerFlow: TCheckBox;
    edtH1: TEdit;
    edtH2: TEdit;
    chkBaseFlow: TCheckBox;
    edtLambda: TEdit;
    edtKsi: TEdit;
    chkOutflow: TCheckBox;
    edtPhi: TEdit;
    chkPumping: TCheckBox;
    chkPotentialEvap: TCheckBox;
    edtK: TEdit;
    chkMeasuredRunoff: TCheckBox;
    grpOutputTimeseries: TGroupBox;
    chkOutRunoff: TCheckBox;
    chkOutEvapotraspiration: TCheckBox;
    chkOutPercolation: TCheckBox;
    chkOutOutflow: TCheckBox;
    chkOutSoilStorage: TCheckBox;
    chkOutGroundStorage: TCheckBox;
    btnCalculate: TButton;
    btnCancel: TButton;
    edtS0: TEdit;
    edtY0: TEdit;
    tbcCalibration: TTabSheet;
    grpCalibrate: TGroupBox;
    chkCalKappa: TCheckBox;
    chkCalLambda: TCheckBox;
    chkCalMi: TCheckBox;
    chkCalKsi: TCheckBox;
    chkCalEpsilon: TCheckBox;
    chkCalPhi: TCheckBox;
    chkCalK: TCheckBox;
    chkCalH1: TCheckBox;
    chkCalH2: TCheckBox;
    chkCalS0: TCheckBox;
    chkCalY0: TCheckBox;
    btnCalibrate: TButton;
    mnuMain: TMainMenu;
    mnuFile: TMenuItem;
    mnuEdit: TMenuItem;
    PrintDialog: TPrintDialog;
    PrinterSetupDialog: TPrinterSetupDialog;
    mnuPrint: TMenuItem;
    mnuPrinterSetup: TMenuItem;
    chkOutError: TCheckBox;
    mnuCopyForm: TMenuItem;
    GroupBox1: TGroupBox;
    edtMinKappa: TEdit;
    edtMinY0: TEdit;
    edtMinLambda: TEdit;
    edtMinMi: TEdit;
    edtMinKsi: TEdit;
    edtMinEpsilon: TEdit;
    edtMinPhi: TEdit;
    edtMinK: TEdit;
    edtMinH1: TEdit;
    edtMinH2: TEdit;
    edtMinS0: TEdit;
    GroupBox2: TGroupBox;
    edtMaxKappa: TEdit;
    edtMaxY0: TEdit;
    edtMaxLambda: TEdit;
    edtMaxMi: TEdit;
    edtMaxKsi: TEdit;
    edtMaxEpsilon: TEdit;
    edtMaxPhi: TEdit;
    edtMaxK: TEdit;
    edtMaxH1: TEdit;
    edtMaxH2: TEdit;
    edtMaxS0: TEdit;
    GroupBox3: TGroupBox;
    pgbKappa: TProgressBar;
    pgbLambda: TProgressBar;
    pgbMi: TProgressBar;
    pgbKsi: TProgressBar;
    pgbEpsilon: TProgressBar;
    pgbPhi: TProgressBar;
    pgbK: TProgressBar;
    pgbH1: TProgressBar;
    pgbH2: TProgressBar;
    pgbS0: TProgressBar;
    pgbY0: TProgressBar;
    btnTransferParameters: TButton;
    GroupBox4: TGroupBox;
    edtCalKappa: TEdit;
    edtCalY0: TEdit;
    edtCalLambda: TEdit;
    edtCalMi: TEdit;
    edtCalKsi: TEdit;
    edtCalEpsilon: TEdit;
    edtCalPhi: TEdit;
    edtCalK: TEdit;
    edtCalH1: TEdit;
    edtCalH2: TEdit;
    edtCalS0: TEdit;
    chart: TChart;
    edtObjectiveFunction: TEdit;
    seriesActualRunoff: TLineSeries;
    seriesCalculatedRunoff: TLineSeries;
    lblObjectiveFunction: TLabel;
    lblNumOfIterations: TLabel;
    edtNumOfIterations: TEdit;
    btnTransferFromModel: TButton;
    tbcDates: TTabSheet;
    lblCalculationDates: TLabel;
    lstCalculationDates: TListBox;
    lstMeasurementDates: TListBox;
    lblMeasurementDates: TLabel;
    btnStop: TButton;
    mnuCopyValues: TMenuItem;
    mnuPasteValues: TMenuItem;
    N1: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    N2: TMenuItem;
    mnuOpenParametersFromFile: TMenuItem;
    mnuWriteParametersToFile: TMenuItem;
    N3: TMenuItem;
    mnuSound: TMenuItem;
    mnuStyle: TMenuItem;
    mnuMidiPort: TMenuItem;
    mnuTempoDelay: TMenuItem;
    SuperFast0ms1: TMenuItem;
    PrestoFast1ms1: TMenuItem;
    Allegro5msdefault1: TMenuItem;
    Andante1: TMenuItem;
    Largo16ms1: TMenuItem;
    procedure chkPotentialEvapClick(Sender: TObject);
    procedure IFormShow(Sender: TObject);
    procedure mnuPrintClick(Sender: TObject);
    procedure mnuPrinterSetupClick(Sender: TObject);
    procedure mnuCopyFormClick(Sender: TObject);
    procedure btnCalculateClick(Sender: TObject);
    procedure lstPotentialEvapKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure pgcPagesChange(Sender: TObject);
    procedure btnCalibrateClick(Sender: TObject);
    procedure btnTransferParametersClick(Sender: TObject);
    procedure btnTransferFromModelClick(Sender: TObject);
    procedure lstPotentialEvapClick(Sender: TObject);
    procedure IFormCreate(Sender: TObject);
    procedure IFormDestroy(Sender: TObject);
    procedure lstMeasuredRunoffClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure mnuCopyValuesClick(Sender: TObject);
    procedure mnuPasteValuesClick(Sender: TObject);
    procedure mnuOpenParametersFromFileClick(Sender: TObject);
    procedure mnuWriteParametersToFileClick(Sender: TObject);
    procedure GetSliderPosArray(SliderArray: TArrayOfInteger);
    procedure PlayMusic;
    procedure SuperFast0ms1Click(Sender: TObject);
  private
    FPrevSliders, FCurSliders: TArrayOfInteger;
    FMusicalStyleSelected, FMidiPortSelected: Integer;
    FCommonPeriod, FMeasuredPeriod: TDateTimeList;
    FStopOptim: Boolean;
    FTempoDelay: Integer;
    procedure RadioItemClick(Sender: TObject);
    procedure SetControlStatus;
    procedure Calculate;
    procedure Calibrate;
    procedure DrawChart(Actual, Calculated: TTimeseries);
    procedure ProgressIndicator(Params: TBasinSimParams; index, total: Integer);
    procedure GetCalParams(Params: TBasinSimParams);
    procedure SetCalculationDatesList;
    procedure SetMeasurementDatesList;
    procedure DisableControls;
    procedure EnableControls;
    procedure DecodeText(AText: string; ANameValueSeparator,
      ADecimalSeparator: Char);
    function EncodeText(ANameValueSeparator, ADecimalSeparator: Char): string;
  public
    TimeseriesGrid: TTimeseriesGrid;
    RainfallIndex, EvaporationIndex, PumpingIndex, RunoffIndex: Integer;
  end;

implementation

{$R *.DFM}

uses
  Clipbrd, tsprocess, contnrs, uiutils, strutils;

procedure TFrmHydroModel.SetControlStatus;
begin
  chkPotentialEvap.Enabled := (EvaporationIndex>-1);
  picEvapotraspiration.Visible := chkPotentialEvap.Checked;

  picStopInnerFlow.Visible := not chkInnerFlow.Checked;
  lblQi.Enabled := chkInnerFlow.Checked;
  picQi.Visible := chkInnerFlow.Checked;
  lblLambda.Enabled := chkInnerFlow.Checked;
  edtLambda.Enabled := chkInnerFlow.Checked;
  lblH1.Enabled := chkInnerFlow.Checked;
  edtH1.Enabled := chkInnerFlow.Checked;

  picStopPercolation.Visible := not chkPercolation.Checked;
  picPercolation.Visible := chkPercolation.Checked;
  lblMi.Enabled := chkPercolation.Checked;
  edtMi.Enabled := chkPercolation.Checked;

  chkPumping.Enabled := (chkPercolation.Checked) and (PumpingIndex>-1);
  picPumping.Visible := (chkPumping.Enabled) and (chkPumping.Checked);

  lblY0.Enabled := chkPercolation.Checked;
  edtY0.Enabled := chkPercolation.Checked;

  chkBaseFlow.Enabled := chkPercolation.Checked;
  lblPercolation.Visible := chkPercolation.Checked;
  lblQb.Enabled := chkBaseFlow.Enabled and chkBaseFlow.Checked;
  picQb.Visible := lblQb.Enabled;
  picStopBaseFlow.Visible := not lblQb.Enabled;
  lblKsi.Enabled := lblQb.Enabled;
  edtKsi.Enabled := lblQb.Enabled;
  lblH2.Enabled := lblQb.Enabled;
  edtH2.Enabled := lblQb.Enabled;

  chkOutFlow.Enabled := chkBaseFlow.Enabled;
  lblPhi.Enabled := chkBaseFlow.Enabled and chkOutflow.Checked;
  edtPhi.Enabled := lblPhi.Enabled;
  picOutFlow.Visible := lblPhi.Enabled;
  picStopOutFlow.Visible := not lblPhi.Enabled;
  lblOutflow.Visible := lblPhi.Enabled;

  chkOutPercolation.Enabled := chkPercolation.Checked;
  chkOutOutflow.Enabled := chkOutflow.Checked and chkOutflow.Enabled;
  chkOutGroundStorage.Enabled := chkPercolation.Checked;
  chkOutError.Enabled := chkMeasuredRunoff.Checked;

  chkMeasuredRunoff.Enabled := (RunoffIndex>-1);

  grpCalibrate.Enabled := chkMeasuredRunoff.Checked;

  chkCalKappa.Enabled := edtKappa.Enabled and grpCalibrate.Enabled;
  chkCalLambda.Enabled := edtLambda.Enabled and grpCalibrate.Enabled;
  chkCalMi.Enabled := edtMi.Enabled and grpCalibrate.Enabled;
  chkCalKsi.Enabled := edtKsi.Enabled and grpCalibrate.Enabled;
  chkCalEpsilon.Enabled := grpCalibrate.Enabled;
  chkCalPhi.Enabled := edtPhi.Enabled and grpCalibrate.Enabled;
  chkCalK.Enabled := grpCalibrate.Enabled;
  chkCalH1.Enabled := edtH1.Enabled and grpCalibrate.Enabled;
  chkCalH2.Enabled := edtH2.Enabled and grpCalibrate.Enabled;
  chkCalS0.Enabled := grpCalibrate.Enabled;
  chkCalY0.Enabled := edtY0.Enabled and grpCalibrate.Enabled;

  btnCalibrate.Enabled := grpCalibrate.Enabled;
  btnTransferParameters.Enabled := grpCalibrate.Enabled;

  edtCalKappa.Enabled := edtKappa.Enabled;
  edtCalLambda.Enabled := edtLambda.Enabled;
  edtCalMi.Enabled := edtMi.Enabled;
  edtCalKsi.Enabled := edtKsi.Enabled;
  edtCalEpsilon.Enabled := edtEpsilon.Enabled;
  edtCalPhi.Enabled := edtPhi.Enabled;
  edtCalK.Enabled := edtK.Enabled;
  edtCalH1.Enabled := edtH1.Enabled;
  edtCalH2.Enabled := edtH2.Enabled;
  edtCalS0.Enabled := edtS0.Enabled;
  edtCalY0.Enabled := edtY0.Enabled;

  edtCalKappa.ReadOnly := chkCalKappa.Checked and chkCalKappa.Enabled;
  edtCalLambda.ReadOnly := chkCalLambda.Checked and chkCalLambda.Enabled;
  edtCalMi.ReadOnly := chkCalMi.Checked and chkCalMi.Enabled;
  edtCalKsi.ReadOnly := chkCalKsi.Checked and chkCalKsi.Enabled;
  edtCalEpsilon.ReadOnly := chkCalEpsilon.Checked and chkCalEpsilon.Enabled;
  edtCalPhi.ReadOnly := chkCalPhi.Checked and chkCalPhi.Enabled;
  edtCalK.ReadOnly := chkCalK.Checked and chkCalK.Enabled;
  edtCalH1.ReadOnly := chkCalH1.Checked and chkCalH1.Enabled;
  edtCalH2.ReadOnly := chkCalH2.Checked and chkCalH2.Enabled;
  edtCalS0.ReadOnly := chkCalS0.Checked and chkCalS0.Enabled;
  edtCalY0.ReadOnly := chkCalY0.Checked and chkCalY0.Enabled;

  if chkCalKappa.Checked and chkCalKappa.Enabled then
    edtCalKappa.Color := clYellow else edtCalKappa.Color := clWindow;
  if chkCalLambda.Checked and chkCalLambda.Enabled then
    edtCalLambda.Color := clYellow else edtCalLambda.Color := clWindow;
  if chkCalMi.Checked and chkCalMi.Enabled then
    edtCalMi.Color := clYellow else edtCalMi.Color := clWindow;
  if chkCalKsi.Checked and chkCalKsi.Enabled then
    edtCalKsi.Color := clYellow else edtCalKsi.Color := clWindow;
  if chkCalEpsilon.Checked and chkCalEpsilon.Enabled then
    edtCalEpsilon.Color := clYellow else edtCalEpsilon.Color := clWindow;
  if chkCalPhi.Checked and chkCalPhi.Enabled then
    edtCalPhi.Color := clYellow else edtCalPhi.Color := clWindow;
  if chkCalK.Checked and chkCalK.Enabled then
    edtCalK.Color := clYellow else edtCalK.Color := clWindow;
  if chkCalH1.Checked and chkCalH1.Enabled then
    edtCalH1.Color := clYellow else edtCalH1.Color := clWindow;
  if chkCalH2.Checked and chkCalH2.Enabled then
    edtCalH2.Color := clYellow else edtCalH2.Color := clWindow;
  if chkCalS0.Checked and chkCalS0.Enabled then
    edtCalS0.Color := clYellow else edtCalS0.Color := clWindow;
  if chkCalY0.Checked and chkCalY0.Enabled then
    edtCalY0.Color := clYellow else edtCalY0.Color := clWindow;

  pgbKappa.Visible := chkCalKappa.Checked and chkCalKappa.Enabled;
  pgbLambda.Visible := chkCalLambda.Checked and chkCalLambda.Enabled;
  pgbMi.Visible := chkCalMi.Checked and chkCalMi.Enabled;
  pgbKsi.Visible := chkCalKsi.Checked and chkCalKsi.Enabled;
  pgbEpsilon.Visible := chkCalEpsilon.Checked and chkCalEpsilon.Enabled;
  pgbPhi.Visible := chkCalPhi.Checked and chkCalPhi.Enabled;
  pgbK.Visible := chkCalK.Checked and chkCalK.Enabled;
  pgbH1.Visible := chkCalH1.Checked and chkCalH1.Enabled;
  pgbH2.Visible := chkCalH2.Checked and chkCalH2.Enabled;
  pgbS0.Visible := chkCalS0.Checked and chkCalS0.Enabled;
  pgbY0.Visible := chkCalY0.Checked and chkCalY0.Enabled;

  edtMinKappa.Enabled := chkCalKappa.Checked and chkCalKappa.Enabled;
  edtMinLambda.Enabled := chkCalLambda.Checked and chkCalLambda.Enabled;
  edtMinMi.Enabled := chkCalMi.Checked and chkCalMi.Enabled;
  edtMinKsi.Enabled := chkCalKsi.Checked and chkCalKsi.Enabled;
  edtMinEpsilon.Enabled := chkCalEpsilon.Checked and chkCalEpsilon.Enabled;
  edtMinPhi.Enabled := chkCalPhi.Checked and chkCalPhi.Enabled;
  edtMinK.Enabled := chkCalK.Checked and chkCalK.Enabled;
  edtMinH1.Enabled := chkCalH1.Checked and chkCalH1.Enabled;
  edtMinH2.Enabled := chkCalH2.Checked and chkCalH2.Enabled;
  edtMinS0.Enabled := chkCalS0.Checked and chkCalS0.Enabled;
  edtMinY0.Enabled := chkCalY0.Checked and chkCalY0.Enabled;

  edtMaxKappa.Enabled := chkCalKappa.Checked and chkCalKappa.Enabled;
  edtMaxLambda.Enabled := chkCalLambda.Checked and chkCalLambda.Enabled;
  edtMaxMi.Enabled := chkCalMi.Checked and chkCalMi.Enabled;
  edtMaxKsi.Enabled := chkCalKsi.Checked and chkCalKsi.Enabled;
  edtMaxEpsilon.Enabled := chkCalEpsilon.Checked and chkCalEpsilon.Enabled;
  edtMaxPhi.Enabled := chkCalPhi.Checked and chkCalPhi.Enabled;
  edtMaxK.Enabled := chkCalK.Checked and chkCalK.Enabled;
  edtMaxH1.Enabled := chkCalH1.Checked and chkCalH1.Enabled;
  edtMaxH2.Enabled := chkCalH2.Checked and chkCalH2.Enabled;
  edtMaxS0.Enabled := chkCalS0.Checked and chkCalS0.Enabled;
  edtMaxY0.Enabled := chkCalY0.Checked and chkCalY0.Enabled;
end;

procedure TFrmHydroModel.chkPotentialEvapClick(Sender: TObject);
begin
  SetControlStatus;
  if (Sender = chkPotentialEvap) or (Sender = chkPercolation) or
    (Sender = chkPumping) then SetCalculationDatesList;
  if Sender = chkMeasuredRunoff then
    SetMeasurementDatesList;
end;

procedure TFrmHydroModel.IFormShow(Sender: TObject);
begin
  SetControlStatus;
  SetCalculationDatesList;
  SetMeasurementDatesList;
end;

procedure TFrmHydroModel.mnuPrintClick(Sender: TObject);
begin
  if PrintDialog.Execute then
    Print;
end;

procedure TFrmHydroModel.mnuPrinterSetupClick(Sender: TObject);
begin
  PrinterSetupDialog.Execute;
end;

procedure TFrmHydroModel.mnuCopyFormClick(Sender: TObject);
begin
  Clipboard.Assign(GetFormImage);
end;

procedure TFrmHydroModel.btnCalculateClick(Sender: TObject);
begin
  Calculate;
  ModalResult := mrOK;
end;

function FindCheckedItem(AMenuItem: TMenuItem): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to AMenuItem.Count-1 do
    if AMenuItem.Items[i].Checked then
    begin
      Result := i;
      Break;
    end;
end;

procedure TFrmHydroModel.btnCalibrateClick(Sender: TObject);

  procedure OpenMidi;
  var i: Integer;
  begin
    FMusicalStyleSelected := FindCheckedItem(mnuStyle);
    FMidiPortSelected := FindCheckedItem(mnuMidiPort);
    if (FMidiPortSelected>-1) and (FMusicalStyleSelected>0) then
      MidiOutput.Open(FMidiPortSelected)
    else Exit;
    with MusicalStyles[FMusicalStyleSelected] do
    begin
      for i := 0 to 10 do
      begin
        MidiOutput.Send(FMidiPortSelected, 176+Channels[i], 64, DamperPedal);
        MidiOutput.Send(FMidiPortSelected, 192+Channels[i], PatchNumber[i], 0);
      end;
    end;
  end;

  procedure CloseMidi;
  var i: Integer;
  begin
    if (FMidiPortSelected>-1) and (FMusicalStyleSelected>0) then
    begin
      with MusicalStyles[FMusicalStyleSelected] do
        for i := 0 to 10 do
        begin
          MidiOutput.Send(FMidiPortSelected, 176+Channels[i], 64, 0);
          MidiOutput.Send(FMidiPortSelected, 176+Channels[i], 23, 0);
        end;
      MidiOutput.Close(FMidiPortSelected)
    end;
  end;

begin
  FStopOptim := False;
  try
    DisableControls;
    Application.ProcessMessages;
    OpenMidi;
    Calibrate;
  finally
    CloseMidi;
    EnableControls;
  end;
end;

procedure TFrmHydroModel.EnableControls;
begin
  Assert(True);
end;

procedure TFrmHydroModel.DisableControls;
begin
  Assert(True);
  btnStop.Enabled := True;
end;

procedure TFrmHydroModel.pgcPagesChange(Sender: TObject);
begin
  if pgcPages.ActivePageIndex = 1 then
  begin
    edtNumOfIterations.Text := '';
    SetControlStatus;
  end;
end;

procedure TFrmHydroModel.btnTransferParametersClick(Sender: TObject);
begin
  edtKappa.Text := edtCalKappa.Text;
  edtLambda.Text := edtCalLambda.Text;
  edtMi.Text := edtCalMi.Text;
  edtKsi.Text := edtCalKsi.Text;
  edtEpsilon.Text := edtCalEpsilon.Text;
  edtPhi.Text := edtCalPhi.Text;
  edtK.Text := edtCalK.Text;
  edtH1.Text := FormatFloat('0.000',StrToFloat(edtCalH1.Text)*
    StrToFloat(edtCalK.Text));
  edtH2.Text := edtCalH2.Text;
  edtS0.Text := FormatFloat('0.000',StrToFloat(edtCalS0.Text)*
    StrToFloat(edtCalK.Text));
  edtY0.Text := edtCalY0.Text;
  pgcPages.ActivePageIndex := 0;
  SetControlStatus;
end;

procedure TFrmHydroModel.btnTransferFromModelClick(Sender: TObject);
begin
  edtCalKappa.Text := edtKappa.Text;
  edtCalLambda.Text := edtLambda.Text;
  edtCalMi.Text := edtMi.Text;
  edtCalKsi.Text := edtKsi.Text;
  edtCalEpsilon.Text := edtEpsilon.Text;
  edtCalPhi.Text := edtPhi.Text;
  edtCalK.Text := edtK.Text;
  edtCalH1.Text := FormatFloat('0.000',StrToFloat(edtH1.Text)/
    StrToFloat(edtK.Text));
  edtCalH2.Text := edtH2.Text;
  edtCalS0.Text := FormatFloat('0.000',StrToFloat(edtS0.Text)/
    StrToFloat(edtK.Text));
  edtCalY0.Text := edtY0.Text;
  SetControlStatus;
end;

procedure TFrmHydroModel.lstPotentialEvapKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key in [VK_CLEAR, VK_DELETE, VK_BACK] then
    TListBox(Sender).ItemIndex := -1;
end;

procedure TFrmHydroModel.lstPotentialEvapClick(Sender: TObject);
begin
  SetCalculationDatesList;
end;

procedure TFrmHydroModel.lstMeasuredRunoffClick(Sender: TObject);
begin
  SetMeasurementDatesList;
end;

procedure TFrmHydroModel.RadioItemClick(Sender: TObject);
begin
  if Sender is TMenuItem then
    with Sender as TMenuItem do
      Checked := not Checked;
end;

procedure TFrmHydroModel.IFormCreate(Sender: TObject);
var
  i: Integer;

  procedure AddSubmenuItem(ACaption: string; ParentMenu: TMenuItem;
    GroupID: Integer);
  var
    ANewItem: TMenuItem;
  begin
    ANewItem := nil;
    try
      ANewItem := TMenuItem.Create(nil);
      ANewItem.Caption := ACaption;
      ANewItem.RadioItem := True;
      ANewItem.GroupIndex := GroupID;
      ANewItem.OnClick := RadioItemClick;
      ParentMenu.Add(ANewItem);
      ANewItem := nil;
    except
      ANewItem.Free;
      raise;
    end;
  end;

begin
  FCommonPeriod := nil;
  FMeasuredPeriod := nil;
  FixComponentDecSeparators(Self);
  SetLength(FPrevSliders, 11);
  SetLength(FCurSliders, 11);
  for i := 0 to 10 do
  begin
    FPrevSliders[i] := -1;
    FCurSliders[i] := -1;
  end;
  for i := 0 to MidiOutput.Devices.Count-1 do
    AddSubmenuItem(MidiOutput.Devices[i], mnuMidiPort, 158);
  for i := 0 to Length(MusicalStyles)-1 do
    with MusicalStyles[i] do
      AddSubmenuItem(StyleName, mnuStyle, 159);
  if mnuMidiPort.Count>0 then
    with  mnuMidiPort.Items[0] do
    begin
      Caption := Caption+' [default]';
      Checked := True;
    end;
  mnuStyle.Items[0].Checked := True;
  FTempoDelay := 120;
end;

procedure TFrmHydroModel.IFormDestroy(Sender: TObject);
begin
  FCommonPeriod.Free;
  FMeasuredPeriod.Free;
end;

resourcestring
  rsTSRunoffTitle = 'Runoff';
  rsTSEvaporationTitle = 'Evapotranspiration';
  rsTSPercolationTitle = 'Percolation';
  rsTSOutflowTitle = 'Outflow';
  rsTSSoilStorageTitle = 'Soil storage';
  rsTSGroundStorageTitle = 'Ground storage';
  rsTSErrorTitle = 'Error';
  rsRainfallMustBeSupplied = 'Rainfall time series must be supplied';
  rsEvaporationShouldBeSupplied =
    'Evapotranspiration time series should be supplied';
  rsPumpingShouldBeSupplied = 'Pumping time series should be supplied';
  rsMeasuredRunoffShouldBeSupplied =
    'Measured runoff time series should be supplied';
procedure TFrmHydroModel.Calculate;
var
  Params: TBasinSimParams;
  Timeseries: TBasinSimTimeseries;
  ACommonPeriod: TDateTimeList;

  procedure SetParams;
  begin
    with Params do
    begin
      Params[bsptSoilStorage] := 0;
      Params[bsptH1Ratio] := 0;
      Params[bsptH2] := 0;
      Params[bsptEpsilon] := 1;
      Params[bsptKappa] := 0;
      Params[bsptLambda] := 0;
      Params[bsptMi] := 0;
      Params[bsptKsi] := 0;
      Params[bsptPhi] := 0;
      Params[bsptInitialSoilStorageRatio] := 0;
      Params[bsptInitialGroundStorage] := 0;
      if edtK.Enabled then Params[bsptSoilStorage] :=
        StrToFloat(edtK.Text);
      if edtH1.Enabled then Params[bsptH1Ratio] := StrToFloat(edtH1.Text)/
        Params[bsptSoilStorage];
      if edtH2.Enabled then Params[bsptH2] := StrToFloat(edtH2.Text);
      if edtEpsilon.Enabled then Params[bsptEpsilon] :=
        StrToFloat(edtEpsilon.Text);
      if edtKappa.Enabled then Params[bsptKappa] := StrToFloat(edtKappa.Text);
      if edtLambda.Enabled then Params[bsptLambda] :=
        StrToFloat(edtLambda.Text);
      if edtMi.Enabled then Params[bsptMi] := StrToFloat(edtMi.Text);
      if edtKsi.Enabled then Params[bsptKsi] := StrToFloat(edtKsi.Text);
      if edtPhi.Enabled then Params[bsptPhi] := StrToFloat(edtPhi.Text);
      if edtS0.Enabled then Params[bsptInitialSoilStorageRatio] :=
        StrToFloat(edtS0.Text)/Params[bsptSoilStorage];
      if edtY0.Enabled then Params[bsptInitialGroundStorage] :=
        StrToFloat(edtY0.Text);
    end;
  end;

  procedure InitTimeseries;
  begin
    Timeseries.Rainfall := nil;
    Timeseries.Potevap := nil;
    Timeseries.Pumping := nil;
    Timeseries.MeasuredRunoff := nil;
    Timeseries.SoilStorage := nil;
    Timeseries.GroundStorage := nil;
    Timeseries.Evaporation := nil;
    Timeseries.Percolation := nil;
    Timeseries.Runoff := nil;
    Timeseries.Outflow := nil;
    Timeseries.Error := nil;
  end;

  procedure SetTimeseries;
  begin
    if RainfallIndex >-1 then
      Timeseries.Rainfall := TimeseriesGrid.Data[RainfallIndex];
    if Timeseries.Rainfall = nil then
      raise Exception.Create(rsRainfallMustBeSupplied);      
    if (chkPotentialEvap.Checked) and (chkPotentialEvap.Enabled) then
    begin
      if EvaporationIndex >-1 then
        Timeseries.Potevap := TimeseriesGrid.Data[EvaporationIndex];
      if Timeseries.Potevap = nil then
        raise Exception.Create(rsEvaporationShouldBeSupplied);
    end;
    if (chkPumping.Checked) and (chkPumping.Enabled) then
    begin
      if PumpingIndex >-1 then
        Timeseries.Pumping := TimeseriesGrid.Data[PumpingIndex];
      if Timeseries.Pumping = nil then
        raise Exception.Create(rsPumpingShouldBeSupplied);
    end;
    if (chkMeasuredRunoff.Checked) and (chkMeasuredRunoff.Enabled) then
    begin
      if RunoffIndex >-1 then
        Timeseries.MeasuredRunoff :=
          TimeseriesGrid.Data[RunoffIndex];
      if Timeseries.MeasuredRunoff = nil then
        raise Exception.Create(rsMeasuredRunoffShouldBeSupplied);
    end;
  end;

  procedure CreateTimeseries;
  begin
    if chkOutRunoff.Enabled and chkOutRunoff.Checked then
    begin
      Timeseries.Runoff := TTimeseries.Create;
      Timeseries.Runoff.AssignMeta(Timeseries.Rainfall);
      Timeseries.Runoff.Title := rsTSRunoffTitle;
      Timeseries.Runoff.Comment := '';
    end;
    if chkOutEvapotraspiration.Enabled and chkOutEvapotraspiration.Checked then
    begin
      Timeseries.Evaporation := TTimeseries.Create;
      Timeseries.Evaporation.AssignMeta(Timeseries.Rainfall);
      Timeseries.Evaporation.Title := rsTSEvaporationTitle;
      Timeseries.Evaporation.Comment := '';
    end;
    if chkOutPercolation.Enabled and chkOutPercolation.Checked then
    begin
      Timeseries.Percolation := TTimeseries.Create;
      Timeseries.Percolation.AssignMeta(Timeseries.Rainfall);
      Timeseries.Percolation.Title := rsTSPercolationTitle;
      Timeseries.Percolation.Comment := '';
    end;
    if chkOutOutflow.Enabled and chkOutOutflow.Checked then
    begin
      Timeseries.Outflow := TTimeseries.Create;
      Timeseries.Outflow.AssignMeta(Timeseries.Rainfall);
      Timeseries.Outflow.Title := rsTSOutflowTitle;
      Timeseries.Outflow.Comment := '';
    end;
    if chkOutSoilStorage.Enabled and chkOutSoilStorage.Checked then
    begin
      Timeseries.SoilStorage := TTimeseries.Create;
      Timeseries.SoilStorage.AssignMeta(Timeseries.Rainfall);
      Timeseries.SoilStorage.Title := rsTSSoilStorageTitle;
      Timeseries.SoilStorage.Comment := '';
    end;
    if chkOutGroundStorage.Enabled and chkOutGroundStorage.Checked then
    begin
      Timeseries.GroundStorage := TTimeseries.Create;
      Timeseries.GroundStorage.AssignMeta(Timeseries.Rainfall);
      Timeseries.GroundStorage.Title := rsTSGroundStorageTitle;
      Timeseries.GroundStorage.Comment := '';
    end;
    if chkOutError.Enabled and chkOutError.Checked then
    begin
      Timeseries.Error := TTimeseries.Create;
      Timeseries.Error.AssignMeta(Timeseries.Rainfall);
      Timeseries.Error.Title := rsTSErrorTitle;
      Timeseries.Error.Comment := '';
    end;
  end;

  procedure AddTimeseriesToGrid;
  begin
    if Timeseries.Runoff <> nil then
      TimeseriesGrid.Add(Timeseries.Runoff);
    if Timeseries.Evaporation <> nil then
      TimeseriesGrid.Add(Timeseries.Evaporation);
    if Timeseries.Percolation <> nil then
      TimeseriesGrid.Add(Timeseries.Percolation);
    if Timeseries.Outflow <> nil then
      TimeseriesGrid.Add(Timeseries.Outflow);
    if Timeseries.SoilStorage <> nil then
      TimeseriesGrid.Add(Timeseries.SoilStorage);
    if Timeseries.GroundStorage <> nil then
      TimeseriesGrid.Add(Timeseries.GroundStorage);
    if Timeseries.Error <> nil then
      TimeseriesGrid.Add(Timeseries.Error);
  end;

  procedure FreeTimeseries;
  begin
    Timeseries.Runoff.Free;
    Timeseries.Evaporation.Free;
    Timeseries.Percolation.Free;
    Timeseries.Outflow.Free;
    Timeseries.SoilStorage.Free;
    Timeseries.GroundStorage.Free;
    Timeseries.Error.Free;
  end;

  procedure SetCommonPeriod;
  var
    i: Integer;
  begin
    Assert(FCommonPeriod.Count=lstCalculationDates.Items.Count);
    for i := 0 to FCommonPeriod.Count-1 do
      if lstCalculationDates.Selected[i] then
        ACommonPeriod.Add(FCommonPeriod[i]);
  end;

begin
  SetParams;
  InitTimeseries;
  SetTimeseries;
  ACommonPeriod := nil;
  try try
    ACommonPeriod := TDateTimeList.Create;
    CreateTimeseries;
    SetCommonPeriod;
    CalcBasinSim(Timeseries, Params, ACommonPeriod);
    AddTimeseriesToGrid;
  except
    FreeTimeseries;
    raise;
  end;
  finally
     ACommonPeriod.Free;
  end;
end;

resourcestring
  rsMeasurdRunoffShouldBeSet = 'Measured runoff time series should be set '+
    'in order to optimize parameters';

procedure TFrmHydroModel.Calibrate;
var
  Params: TBasinSimParams;
  Timeseries: TBasinSimTimeseries;
  ACursor: TCursor;
  ACommonPeriod, AMeasuredPeriod: TDateTimeList;

  procedure SetParams;
  begin
    with Params do
    begin
      Params[bsptSoilStorage] := 0;
      Params[bsptH1Ratio] := 0;
      Params[bsptH2] := 0;
      Params[bsptEpsilon] := 1;
      Params[bsptKappa] := 0;
      Params[bsptLambda] := 0;
      Params[bsptMi] := 0;
      Params[bsptKsi] := 0;
      Params[bsptPhi] := 0;
      Params[bsptInitialSoilStorageRatio] := 0;
      Params[bsptInitialGroundStorage] := 0;
      if edtCalK.Enabled then Params[bsptSoilStorage] :=
        StrToFloat(edtCalK.Text);
      if edtCalH1.Enabled then Params[bsptH1Ratio] := StrToFloat(edtCalH1.Text);
      if edtCalH2.Enabled then Params[bsptH2] := StrToFloat(edtCalH2.Text);
      if edtCalEpsilon.Enabled then Params[bsptEpsilon] :=
        StrToFloat(edtCalEpsilon.Text);
      if edtCalKappa.Enabled then Params[bsptKappa] :=
        StrToFloat(edtCalKappa.Text);
      if edtCalLambda.Enabled then Params[bsptLambda] :=
        StrToFloat(edtCalLambda.Text);
      if edtCalMi.Enabled then Params[bsptMi] := StrToFloat(edtCalMi.Text);
      if edtCalKsi.Enabled then Params[bsptKsi] := StrToFloat(edtCalKsi.Text);
      if edtCalPhi.Enabled then Params[bsptPhi] := StrToFloat(edtCalPhi.Text);
      if edtCalS0.Enabled then Params[bsptInitialSoilStorageRatio] :=
        StrToFloat(edtCalS0.Text);
      if edtCalY0.Enabled then Params[bsptInitialGroundStorage] :=
        StrToFloat(edtCalY0.Text);
      OptParams[bsptSoilStorage] :=
        chkCalK.Enabled and chkCalK.Checked;
      OptParams[bsptH1Ratio] := chkCalH1.Enabled and chkCalH1.Checked;
      OptParams[bsptH2] := chkCalH2.Enabled and chkCalH2.Checked;
      OptParams[bsptEpsilon] :=
        chkCalEpsilon.Enabled and chkCalEpsilon.Checked;
      OptParams[bsptKappa] := chkCalKappa.Enabled and chkCalKappa.Checked;
      OptParams[bsptLambda] := chkCalLambda.Enabled and chkCalLambda.Checked;
      OptParams[bsptMi] := chkCalMi.Enabled and chkCalMi.Checked;
      OptParams[bsptKsi] := chkCalKsi.Enabled and chkCalKsi.Checked;
      OptParams[bsptPhi] := chkCalPhi.Enabled and chkCalPhi.Checked;
      OptParams[bsptInitialSoilStorageRatio] :=
        chkCalS0.Enabled and chkCalS0.Checked;
      OptParams[bsptInitialGroundStorage] :=
        chkCalY0.Enabled and chkCalY0.Checked;
      if edtMinK.Enabled then MinParams[bsptSoilStorage] :=
        StrToFloat(edtMinK.Text);
      if edtMinH1.Enabled then MinParams[bsptH1Ratio] :=
        StrToFloat(edtMinH1.Text);
      if edtMinH2.Enabled then MinParams[bsptH2] := StrToFloat(edtMinH2.Text);
      if edtMinEpsilon.Enabled then MinParams[bsptEpsilon] :=
        StrToFloat(edtMinEpsilon.Text);
      if edtMinKappa.Enabled then MinParams[bsptKappa] :=
        StrToFloat(edtMinKappa.Text);
      if edtMinLambda.Enabled then MinParams[bsptLambda] :=
        StrToFloat(edtMinLambda.Text);
      if edtMinMi.Enabled then MinParams[bsptMi] := StrToFloat(edtMinMi.Text);
      if edtMinKsi.Enabled then MinParams[bsptKsi] :=
        StrToFloat(edtMinKsi.Text);
      if edtMinPhi.Enabled then MinParams[bsptPhi] :=
        StrToFloat(edtMinPhi.Text);
      if edtMinS0.Enabled then MinParams[bsptInitialSoilStorageRatio] :=
        StrToFloat(edtMinS0.Text);
      if edtMinY0.Enabled then MinParams[bsptInitialGroundStorage] :=
        StrToFloat(edtMinY0.Text);
      if edtMaxK.Enabled then MaxParams[bsptSoilStorage] :=
        StrToFloat(edtMaxK.Text);
      if edtMaxH1.Enabled then MaxParams[bsptH1Ratio] :=
        StrToFloat(edtMaxH1.Text);
      if edtMaxH2.Enabled then MaxParams[bsptH2] := StrToFloat(edtMaxH2.Text);
      if edtMaxEpsilon.Enabled then MaxParams[bsptEpsilon] :=
        StrToFloat(edtMaxEpsilon.Text);
      if edtMaxKappa.Enabled then MaxParams[bsptKappa] :=
        StrToFloat(edtMaxKappa.Text);
      if edtMaxLambda.Enabled then MaxParams[bsptLambda] :=
        StrToFloat(edtMaxLambda.Text);
      if edtMaxMi.Enabled then MaxParams[bsptMi] := StrToFloat(edtMaxMi.Text);
      if edtMaxKsi.Enabled then MaxParams[bsptKsi] :=
        StrToFloat(edtMaxKsi.Text);
      if edtMaxPhi.Enabled then MaxParams[bsptPhi] :=
        StrToFloat(edtMaxPhi.Text);
      if edtMaxS0.Enabled then MaxParams[bsptInitialSoilStorageRatio] :=
        StrToFloat(edtMaxS0.Text);
      if edtMaxY0.Enabled then MaxParams[bsptInitialGroundStorage] :=
        StrToFloat(edtMaxY0.Text);
    end;
  end;

  procedure InitTimeseries;
  begin
    Timeseries.Rainfall := nil;
    Timeseries.Potevap := nil;
    Timeseries.Pumping := nil;
    Timeseries.MeasuredRunoff := nil;
    Timeseries.SoilStorage := nil;
    Timeseries.GroundStorage := nil;
    Timeseries.Evaporation := nil;
    Timeseries.Percolation := nil;
    Timeseries.Runoff := nil;
    Timeseries.Outflow := nil;
    Timeseries.Error := nil;
  end;

  procedure SetTimeseries;
  begin
    if RainfallIndex >-1 then
      Timeseries.Rainfall := TimeseriesGrid.Data[RainfallIndex];
    if Timeseries.Rainfall = nil then
      raise Exception.Create(rsRainfallMustBeSupplied);      
    if (chkPotentialEvap.Checked) and (chkPotentialEvap.Enabled) then
    begin
      if EvaporationIndex >-1 then
        Timeseries.Potevap := TimeseriesGrid.Data[EvaporationIndex];
      if Timeseries.Potevap = nil then
        raise Exception.Create(rsEvaporationShouldBeSupplied);
    end;
    if (chkPumping.Checked) and (chkPumping.Enabled) then
    begin
      if PumpingIndex >-1 then
        Timeseries.Pumping := TimeseriesGrid.Data[PumpingIndex];
      if Timeseries.Pumping = nil then
        raise Exception.Create(rsPumpingShouldBeSupplied);
    end;
    if (chkMeasuredRunoff.Checked) and (chkMeasuredRunoff.Enabled) then
    begin
      if RunoffIndex >-1 then
        Timeseries.MeasuredRunoff :=
          TimeseriesGrid.Data[RunoffIndex];
      if Timeseries.MeasuredRunoff = nil then
        raise Exception.Create(rsMeasuredRunoffShouldBeSupplied);
    end;
  end;

  procedure CreateTimeseries;
  begin
    Timeseries.Runoff := TTimeseries.Create;
    Timeseries.Runoff.AssignMeta(Timeseries.Rainfall);
  end;

  procedure FreeTimeseries;
  begin
    Timeseries.Runoff.Free;
  end;

  procedure SetCommonPeriod;
  var
    i: Integer;
  begin
    Assert(FCommonPeriod.Count=lstCalculationDates.Items.Count);
    for i := 0 to FCommonPeriod.Count-1 do
      if lstCalculationDates.Selected[i] then
        ACommonPeriod.Add(FCommonPeriod[i]);
  end;

  procedure SetMeasuredPeriod;
  var
    i: Integer;
  begin
    Assert(FMeasuredPeriod.Count=lstMeasurementDates.Items.Count);
    for i := 0 to FMeasuredPeriod.Count-1 do
      if lstMeasurementDates.Selected[i] then
        AMeasuredPeriod.Add(FMeasuredPeriod[i]);
  end;

begin
  seriesActualRunoff.Clear;
  seriesCalculatedRunoff.Clear;
  SetParams;
  InitTimeseries;
  SetTimeseries;
  ACursor := Screen.Cursor;
  ACommonPeriod := nil;
  AMeasuredPeriod := nil;
  try
    Screen.Cursor := crHourGlass;
    ACommonPeriod := TDateTimeList.Create;
    AMeasuredPeriod := TDateTimeList.Create;
    CreateTimeseries;
    if Timeseries.MeasuredRunoff = nil then
      raise Exception.Create(rsMeasurdRunoffShouldBeSet);
    SetCommonPeriod;
    SetMeasuredPeriod;
    CalcBasinOpt(Timeseries, ACommonPeriod, AMeasuredPeriod, Params,
      ProgressIndicator, FStopOptim);
    GetCalParams(Params);
    DrawChart(Timeseries.MeasuredRunoff, Timeseries.Runoff);
  finally
    FreeTimeseries;
    ACommonPeriod.Free;
    AMeasuredPeriod.Free;
    Screen.Cursor := ACursor;
  end;
end;

procedure TFrmHydroModel.GetSliderPosArray(SliderArray: TArrayOfInteger);
var
  i, j: Integer;
begin
  j := 0;
  for i := 0 to ComponentCount-1 do
    if Components[i] is TProgressBar then
    begin
      SliderArray[j] := TProgressBar(Components[i]).Position;
      Inc(j);
      if j=11 then Break;
    end;
end;

procedure TFrmHydroModel.PlayMusic;
var
  i: Integer;
begin
  if (FMidiPortSelected>-1) and (FMusicalStyleSelected>0) then
    begin
    for i := 0 to Length(FPrevSliders)-1 do
      if FPrevSliders[i]<>FCurSliders[i] then
      begin
        with MusicalStyles[FMusicalStyleSelected] do
        begin
          if FPrevSliders[i]>=0 then
            MidiOutput.Send(0, 128+Channels[i], Sounds[FPrevSliders[i] div 10]+
              NotesShift[i], 100);
          MidiOutput.Send(0, 144+Channels[i], Sounds[FCurSliders[i] div 10]+
            NotesShift[i], 100);
        end;
      end;
    Sleep(FTempoDelay);
  end;
end;

procedure TFrmHydroModel.GetCalParams(Params: TBasinSimParams);
begin
  edtObjectiveFunction.Text := FormatFloat('0.000', -1*Params.fopt);
  edtNumOfIterations.Text := IntToStr(Params.eval);
  edtCalK.Text := FormatFloat('0.00', Params.Params[bsptSoilStorage]);
  edtCalH1.Text := FormatFloat('0.000', Params.Params[bsptH1Ratio]);
  edtCalH2.Text := FormatFloat('0.00', Params.Params[bsptH2]);
  edtCalEpsilon.Text := FormatFloat('0.000', Params.Params[bsptEpsilon]);
  edtCalKappa.Text := FormatFloat('0.000', Params.Params[bsptKappa]);
  edtCalLambda.Text := FormatFloat('0.000', Params.Params[bsptLambda]);
  edtCalMi.Text := FormatFloat('0.000', Params.Params[bsptMi]);
  edtCalKsi.Text := FormatFloat('0.000', Params.Params[bsptKsi]);
  edtCalPhi.Text := FormatFloat('0.000', Params.Params[bsptPhi]);
  edtCalS0.Text :=
    FormatFloat('0.000', Params.Params[bsptInitialSoilStorageRatio]);
  edtCalY0.Text :=
    FormatFloat('0.00', Params.Params[bsptInitialGroundStorage]);
  GetSliderPosArray(FPrevSliders);
  with Params do
  begin
    try
      if pgbK.Visible then
      pgbK.Position := Round(100*
        (Params[bsptSoilStorage]-MinParams[bsptSoilStorage])/
          (MaxParams[bsptSoilStorage]-MinParams[bsptSoilStorage]));
      if pgbH1.Visible then
      pgbH1.Position := Round(100*
        (Params[bsptH1Ratio]-MinParams[bsptH1Ratio])/
          (MaxParams[bsptH1Ratio]-MinParams[bsptH1Ratio]));
      if pgbH2.Visible then
      pgbH2.Position := Round(100*
        (Params[bsptH2]-MinParams[bsptH2])/
          (MaxParams[bsptH2]-MinParams[bsptH2]));
      if pgbEpsilon.Visible then
      pgbEpsilon.Position := Round(100*
        (Params[bsptEpsilon]-MinParams[bsptEpsilon])/
          (MaxParams[bsptEpsilon]-MinParams[bsptEpsilon]));
      if pgbKappa.Visible then
      pgbKappa.Position := Round(100*
        (Params[bsptKappa]-MinParams[bsptKappa])/
          (MaxParams[bsptKappa]-MinParams[bsptKappa]));
      if pgbLambda.Visible then
      pgbLambda.Position := Round(100*
        (Params[bsptLambda]-MinParams[bsptLambda])/
          (MaxParams[bsptLambda]-MinParams[bsptLambda]));
      if pgbMi.Visible then
      pgbMi.Position := Round(100*
        (Params[bsptMi]-MinParams[bsptMi])/
          (MaxParams[bsptMi]-MinParams[bsptMi]));
      if pgbKsi.Visible then
      pgbKsi.Position := Round(100*
        (Params[bsptKsi]-MinParams[bsptKsi])/
          (MaxParams[bsptKsi]-MinParams[bsptKsi]));
      if pgbPhi.Visible then
      pgbPhi.Position := Round(100*
        (Params[bsptPhi]-MinParams[bsptPhi])/
          (MaxParams[bsptPhi]-MinParams[bsptPhi]));
      if pgbS0.Visible then
      pgbS0.Position := Round(100*
        (Params[bsptInitialSoilStorageRatio]-
          MinParams[bsptInitialSoilStorageRatio])/
            (MaxParams[bsptInitialSoilStorageRatio]-
              MinParams[bsptInitialSoilStorageRatio]));
      if pgbY0.Visible then
      pgbY0.Position := Round(100*
        (Params[bsptInitialGroundStorage]-MinParams[bsptInitialGroundStorage])/
          (MaxParams[bsptInitialGroundStorage]-
            MinParams[bsptInitialGroundStorage]));
      GetSliderPosArray(FCurSliders);
      PlayMusic;
    except
      on EMathError do
        Exit;
      else
        raise;
    end;
  end;
end;

procedure TFrmHydroModel.ProgressIndicator(Params: TBasinSimParams;
  index, total: Integer);
begin
  GetCalParams(Params);
  Application.ProcessMessages;
end;

procedure TFrmHydroModel.DrawChart(Actual, Calculated: TTimeseries);
var
  i: Integer;
begin
  seriesActualRunoff.Clear;
  seriesCalculatedRunoff.Clear;
  for i := 0 to Actual.Count-1 do
  begin
    if Actual[i].IsNull then
      seriesActualRunoff.AddNullXY(Actual[i].Date, 0,'')
    else
      seriesActualRunoff.AddXY(Actual[i].Date, Actual[i].AsFloat,'',
        seriesActualRunoff.SeriesColor);
  end;
  for i := 0 to Calculated.Count-1 do
  begin
    if Calculated[i].IsNull then
      seriesCalculatedRunoff.AddNullXY(Calculated[i].Date, 0,'')
    else
      seriesCalculatedRunoff.AddXY(Calculated[i].Date, Calculated[i].AsFloat,'',
        seriesCalculatedRunoff.SeriesColor);
  end;
end;

procedure TFrmHydroModel.SetCalculationDatesList;
var
  TimeseriesList: TObjectList;
  i: Integer;
begin
  lstCalculationDates.Clear;
  TimeseriesList := nil;
  try
    TimeseriesList := TObjectList.Create(False);
    if RainfallIndex >= 0 then
      TimeseriesList.Add(TimeseriesGrid.Data[RainfallIndex]);
    if (chkPotentialEvap.Checked) and (chkPotentialEvap.Enabled) then
      if EvaporationIndex >=0 then
        TimeseriesList.Add(TimeseriesGrid.Data[EvaporationIndex]);
    if (chkPumping.Checked) and (chkPumping.Enabled) then
      if PumpingIndex >= 0 then
        TimeseriesList.Add(TimeseriesGrid.Data[PumpingIndex]);
    if FCommonPeriod<>nil then
      FreeAndNil(FCommonPeriod);
    if TimeseriesList.Count > 0 then
    begin
      FCommonPeriod := GetCommonPeriod(TimeseriesList, 0);
      for i := 0 to FCommonPeriod.Count-1 do
        lstCalculationDates.Items.Add(DateTimeToStr(FCommonPeriod[i]));
      for i := 0 to lstCalculationDates.Items.Count-1 do
        lstCalculationDates.Selected[i] := True;
    end;
  finally
    TimeseriesList.Free;
  end;
end;

procedure TFrmHydroModel.SetMeasurementDatesList;
var
  TimeseriesList: TObjectList;
  i: Integer;
begin
  lstMeasurementDates.Clear;
  TimeseriesList := nil;
  try
    TimeseriesList := TObjectList.Create(False);
    if (chkMeasuredRunoff.Enabled) and (chkMeasuredRunoff.Checked) then
      if RunoffIndex >=0 then
        TimeseriesList.Add(TimeseriesGrid.Data[RunoffIndex]);
    if FMeasuredPeriod<>nil then
      FreeAndNil(FMeasuredPeriod);
    if TimeseriesList.Count > 0 then
    begin
      FMeasuredPeriod := GetCommonPeriod(TimeseriesList, 0);
      for i := 0 to FMeasuredPeriod.Count-1 do
        lstMeasurementDates.Items.Add(DateTimeToStr(FMeasuredPeriod[i]));
      for i := 0 to lstMeasurementDates.Items.Count-1 do
        lstMeasurementDates.Selected[i] := True;
    end;
  finally
    TimeseriesList.Free;
  end;
end;

procedure TFrmHydroModel.SuperFast0ms1Click(Sender: TObject);
begin
  with (Sender as TMenuItem) do
  begin
    Checked := True;
    FTempoDelay := Tag;
  end;
end;

procedure TFrmHydroModel.btnStopClick(Sender: TObject);
begin
  FStopOptim := True;
end;

procedure TFrmHydroModel.DecodeText(AText: string; ANameValueSeparator,
  ADecimalSeparator: Char);

  procedure SetValue(AComponent: TComponent; AValue: string);
  begin
    if AValue='' then Exit;
    if AComponent is TEdit then
      TEdit(AComponent).Text :=
        ReplaceStr(AValue, ADecimalSeparator, SysUtils.DecimalSeparator)
    else if AComponent is TCheckBox then
      TCheckBox(AComponent).Checked := StrToBool(AValue)
    else
      Assert(False);
  end;

var
  AStringList: TStringList;
  i: Integer;
begin
  AStringList := nil;
  try
    AStringList := TStringList.Create;
    AStringList.NameValueSeparator := ANameValueSeparator;
    AStringList.Text := AText;
    for i := 0 to ComponentCount-1 do
      if (Components[i] is TCheckBox) or (Components[i] is TEdit) then
        with Components[i] do
          SetValue(Self.Components[i], AStringList.Values[Name]);
  finally
    AStringList.Free;
  end;
end;

function TFrmHydroModel.EncodeText(ANameValueSeparator,
  ADecimalSeparator: Char): string;

  function GetValue(AComponent: TComponent; AName: string): string;
  begin
    Result := '';
    if AComponent is TEdit then
      Result := AName + ANameValueSeparator +
        ReplaceStr( TEdit(AComponent).Text, SysUtils.DecimalSeparator,
          ADecimalSeparator)
    else if AComponent is TCheckBox then
      Result := AName + ANameValueSeparator +
        BoolToStr(TCheckBox(AComponent).Checked, True)
    else
      Assert(False);
  end;

var
  i: Integer;
begin
  Result := '';
  for i := 0 to ComponentCount-1 do
    if (Components[i] is TCheckBox) or (Components[i] is TEdit) then
      with Components[i] do
      begin
        if Result<>'' then Result := Result+#13#10;
        Result := Result + GetValue(Self.Components[i], Name);
      end;
end;

procedure TFrmHydroModel.mnuCopyValuesClick(Sender: TObject);
var
  AStringList: TStringList;
begin
  AStringList := nil;
  try
    AStringList := TStringList.Create;
    AStringList.NameValueSeparator := #9;
    AStringList.Text := EncodeText(#9, SysUtils.DecimalSeparator);
    Clipboard.AsText := AStringList.Text;
  finally
    AStringList.Free;
  end;
end;

procedure TFrmHydroModel.mnuPasteValuesClick(Sender: TObject);
begin
  DecodeText(Clipboard.AsText, #9, SysUtils.DecimalSeparator);
end;

procedure TFrmHydroModel.mnuOpenParametersFromFileClick(Sender: TObject);
var
  AStringList: TStringList;
begin
  if not OpenDialog.Execute then Exit;
  AStringList := nil;
  try
    AStringList := TStringList.Create;
    AStringList.NameValueSeparator := '=';
    AStringList.LoadFromFile(OpenDialog.FileName);
    DecodeText(AStringList.Text, '=', '.');
  finally
    AStringList.Free;
  end;
end;

procedure TFrmHydroModel.mnuWriteParametersToFileClick(Sender: TObject);
var
  AStringList: TStringList;
begin
  if not SaveDialog.Execute then Exit;
  AStringList := nil;
  try
    AStringList := TStringList.Create;
    AStringList.NameValueSeparator := '=';
    AStringList.Text := EncodeText('=', '.');
    AStringList.SaveToFile(SaveDialog.FileName);
  finally
    AStringList.Free;
  end;
end;

end.
