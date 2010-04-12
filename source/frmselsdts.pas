{******************************************************************}
{                                                                  }
{  Itia library                                                    }
{                                                                  }
{  Copyright (c) 2000-04 National Technical University of Athens   }
{                                                                  }
{******************************************************************}

{** Selection of timeseries for stage-discharge GUI}
unit frmselsdts;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, tsgrid, interpol;

type
  TSelectSDTimeseries = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    lstStageTS: TListBox;
    lstDischargeTS: TListBox;
    memoStageTS: TMemo;
    memoDischargeTS: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    procedure btnOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lstStageTSClick(Sender: TObject);
  private
    procedure PopulateList(AList: TListBox);
    procedure PopulateMemo(AMemo: TMemo; AList: TListBox);
    procedure SetControlStatus;
  public
    TimeseriesGrid: TTimeseriesGrid;
    TransientCurveList: TTransientCurveList;
  end;

implementation

{$R *.DFM}

procedure TSelectSDTimeseries.FormShow(Sender: TObject);
begin
  Assert(TimeseriesGrid<>nil);
  Assert(TransientCurveList<>nil);
  SetControlStatus;
end;

procedure TSelectSDTimeseries.FormCreate(Sender: TObject);
begin
  TimeseriesGrid := nil;
  TransientCurveList := nil;
end;

resourcestring
  rsNotValidSelection = 'Not valid selections';

procedure TSelectSDTimeseries.btnOKClick(Sender: TObject);
begin
  if (lstStageTS.ItemIndex<0) or (lstDischargeTS.ItemIndex<0) then
    raise Exception.Create(rsNotValidSelection);
  TransientCurveList.SetHydrometricPoints(
    TimeseriesGrid.Data[lstStageTS.ItemIndex],
    TimeseriesGrid.Data[lstDischargeTS.ItemIndex]);
  ModalResult := mrOk;
end;

procedure TSelectSDTimeseries.SetControlStatus;
begin
  PopulateList(lstStageTS);
  PopulateList(lstDischargeTS);
  PopulateMemo(memoStageTS, lstStageTS);
  PopulateMemo(memoDischargeTS, lstDischargeTS);
end;

procedure TSelectSDTimeseries.PopulateList(AList: TListBox);
var
  i: Integer;
  AIndex: Integer;
begin
  AIndex := AList.ItemIndex;
  AList.Clear;
  for i := 0 to TimeseriesGrid.Count-1 do
    AList.Items.Add(IntToStr(i+1)+': '+TimeseriesGrid.Data[i].Title);
  AList.ItemIndex := AIndex;
end;

procedure TSelectSDTimeseries.PopulateMemo(AMemo: TMemo; AList: TListBox);
begin
  AMemo.Clear;
  if AList.ItemIndex<0 then Exit;
  AMemo.Lines.Text := TimeseriesGrid.Data[AList.ItemIndex].Comment;
end;

procedure TSelectSDTimeseries.lstStageTSClick(Sender: TObject);
begin
  SetControlStatus;
end;

end.
