{******************************************************************}
{                                                                  }
{  LoUISE library                                                  }
{                                                                  }
{  Copyright (c) 2000-2004 National Technical University of Athens }
{                                                                  }
{******************************************************************}

unit frmprcsdlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls;

type
  TFrmProcessingDialog = class(TForm)
    ProgressBar: TProgressBar;
    bntStop: TButton;
    lblProcessing: TLabel;
    Label1: TLabel;
    lblPercent: TLabel;
    Label2: TLabel;
    lblCompleted: TLabel;
    Label3: TLabel;
    lblTotal: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    lblElapsed: TLabel;
    Label7: TLabel;
    lblRemaining: TLabel;
    procedure FormShow(Sender: TObject);
    procedure bntStopClick(Sender: TObject);
  private
    StartTime: TDateTime;
    MaxPos, MinPos, Pos: Integer;
  public
    ToStopPlease: Boolean;
    procedure Initialize(Steps: Integer);
    procedure StepOne;
  end;

{var}
{  FrmProcessingDialog: TFrmProcessingDialog;}

implementation

{$R *.DFM}

resourcestring
  rsStatusProcessing = 'Processing...';
  rsStatusStoping = 'Stopping...';

procedure TFrmProcessingDialog.Initialize(Steps: Integer);
begin
  lblProcessing.Caption := rsStatusProcessing;
  MinPos := 0;
  MaxPos := Steps;
  Pos := 0;
  ProgressBar.Min := MinPos;
  ProgressBar.Max := MaxPos;
  ProgressBar.Position := Pos;
  lblTotal.Caption := IntToStr(Steps div 10)+'000';
  lblCompleted.Caption := '0';
  lblPercent.Caption := '0.0 %';
  StartTime := Now;
  ToStopPlease := False;
end;

procedure TFrmProcessingDialog.StepOne;
begin
  Inc(Pos);
  ProgressBar.Position := Pos;
  lblPercent.Caption := FormatFloat('0.0', 100 * Pos / MaxPos)+' %';
  lblCompleted.Caption := IntToStr(Pos div 10)+'000';
  lblElapsed.Caption := FormatDateTime('hh:nn:ss', Now-StartTime);
  lblRemaining.Caption := FormatDateTime('hh:nn:ss',
    (Now-StartTime)*(MaxPos-Pos) / (1+Pos-MinPos));
  Application.ProcessMessages;
end;

procedure TFrmProcessingDialog.FormShow(Sender: TObject);
begin
  Application.ProcessMessages;
end;

procedure TFrmProcessingDialog.bntStopClick(Sender: TObject);
begin
  lblProcessing.Caption := rsStatusStoping;
  ToStopPlease := True;
end;

end.
