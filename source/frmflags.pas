{******************************************************************}
{                                                                  }
{  LoUISE library                                                  }
{                                                                  }
{  Copyright (c) 2000-04 National Technical University of Athens   }
{                                                                  }
{******************************************************************}

unit frmflags;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls;

type
  TFrmSetFlagsDialog = class(TForm)
    ScrollBox: TScrollBox;
    btnOK: TButton;
    btnCancel: TButton;
    btnAddNewFlag: TButton;
    FlowPanel: TFlowPanel;
    btnUncheckAll: TButton;
    procedure FormShow(Sender: TObject);
    procedure btnAddNewFlagClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnUncheckAllClick(Sender: TObject);
  private
    FSelectionFlags, FOnFlags, FMixedFlags: string;
    FTurnOnFlags, FTurnOffFlags: string;
    FUncheckAll: Boolean;
  public
    property SelectionFlags: string read FSelectionFlags write FSelectionFlags;
    property OnFlags: string read FOnFlags write FOnFlags;
    property MixedFlags: string read FMixedFlags write FMixedFlags;
    property TurnOnFlags: string read FTurnOnFlags write FTurnOnFlags;
    property TurnOffFlags: string read FTurnOffFlags write FTurnOffFlags;
  end;

implementation

{$R *.dfm}

uses
  istrutils, Character;

resourcestring
  rsInputFlag = 'Enter a new flag, or a new flag list, space separated';
  rsInputFlagCaption = 'Enter a new flag';

procedure TFrmSetFlagsDialog.btnAddNewFlagClick(Sender: TObject);
var
  s: string;
  i, j: Integer;
  ACheckBox: TCheckBox;
  FlagExists: Boolean;
begin
  s := '';
  if not InputQuery(rsInputFlagCaption, rsInputFlag, s) then Exit;
  if s = '' then Exit;
  s := TrimAllSpaces(s).ToUpper();
  ACheckBox := nil;
  for i := 1 to DelimitedStringCount(s, ' ') do
  begin
    FlagExists := False;
    for j := 0 to ComponentCount-1 do
      if Components[j] is TCheckBox then
        with Components[j] as TCheckBox do
          if Caption = DelimitedStringItem(s, i, ' ') then
          begin
            FlagExists := True;
            Break;
          end;
    if FlagExists then Continue;
    with ACheckBox do
    begin
      try
        ACheckBox := TCheckBox.Create(Self);
        Tag := 0;
        Caption := DelimitedStringItem(s, i, ' ');
        Width := 110;
        Parent := FlowPanel;
        if Top+Height>FlowPanel.ClientHeight then
          FlowPanel.ClientHeight := Top+Height;
        ACheckBox := nil;
      finally
        ACheckBox.Free;
      end;
    end;
  end;
end;

procedure TFrmSetFlagsDialog.btnOKClick(Sender: TObject);
var
  i: Integer;
  TurnOnFlagsList, TurnOffFlagsList: TStringList;
begin
  TurnOnFlagsList := nil;
  TurnOffFlagsList := nil;
  try
    TurnOnFlagsList := TStringList.Create;
    TurnOffFlagsList := TStringList.Create;
    for i := 0 to ComponentCount -1 do
      if Components[i] is TCheckBox then
        with Components[i] as TCheckBox do
          case Tag of
             0: if State = cbChecked then TurnOnFlagsList.Add(Caption);
             1: if State = cbUnchecked then TurnOffFlagsList.Add(Caption);
             2: if State = cbChecked then TurnOnFlagsList.Add(Caption) else
                if State = cbUnchecked then TurnOffFlagsList.Add(Caption);
          end;
    FTurnOnFlags := TurnOnFlagsList.Text;
    FTurnOffFlags := TurnOffFlagsList.Text;
  finally
    TurnOnFlagsList.Free;
    TurnOffFlagsList.Free;
  end;
end;

procedure TFrmSetFlagsDialog.btnUncheckAllClick(Sender: TObject);
var
  i: Integer;
begin
  FUncheckAll := not FUncheckAll;
  for i := 0 to ComponentCount-1 do
    if Components[i] is TCheckBox then
      with Components[i] as TCheckBox do
      begin
        State := cbUnchecked;
        if not FUncheckAll then
          case Tag of
            1: State := cbChecked;
            2: State := cbGrayed;
          end;
      end;
end;

procedure TFrmSetFlagsDialog.FormCreate(Sender: TObject);
begin
  FUncheckAll := False;
end;

procedure TFrmSetFlagsDialog.FormShow(Sender: TObject);
var
  ACheckBox: TCheckBox;
  SelectionFlagsList, OnFlagsList, MixedFlagsList: TStringList;
  i: Integer;
begin
  Assert(FSelectionFlags<>'');
  SelectionFlagsList := nil;
  OnFlagsList := nil;
  MixedFlagsList := nil;
  try
    SelectionFlagsList := TStringList.Create;
    OnFlagsList := TStringList.Create;
    MixedFlagsList := TStringList.Create;
    SelectionFlagsList.Text := FSelectionFlags;
    OnFlagsList.Text := FOnFlags;
    MixedFlagsList.Text := FMixedFlags;
    ACheckBox := nil;
    for i := 0 to SelectionFlagsList.Count-1 do
      with ACheckBox do
      begin
        try
          ACheckBox := TCheckBox.Create(Self);
          Caption := SelectionFlagsList[i];
          Width := 110;
          Tag := 0;
          if MixedFlagsList.IndexOf(Caption)>-1 then
          begin
            Tag := 2;
            AllowGrayed := True;
            State := cbGrayed;
          end;
          if OnFlagsList.IndexOf(Caption)>-1 then
          begin
            Tag := 1;
            State := cbChecked;
          end;
          Parent := FlowPanel;
          if Top+Height>FlowPanel.ClientHeight then
            FlowPanel.ClientHeight := Top+Height;
          ACheckBox := nil;
        finally
          ACheckBox.Free;
        end;
      end;
  finally
    SelectionFlagsList.Free;
    OnFlagsList.Free;
    MixedFlagsList.Free;
  end;
end;


end.

