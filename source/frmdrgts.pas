{******************************************************************}
{                                                                  }
{  LoUISE library                                                  }
{                                                                  }
{  Copyright (c) 2000-06 National Technical University of Athens   }
{                                                                  }
{******************************************************************}

unit frmdrgts;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, GenUtils, Buttons;

type
  EMissingTimeseries = class(Exception);

type
  TFrmTimeseriesSelections = class(TForm)
    treeTimeseriesToUse: TTreeView;
    lstAvailableTimeseries: TListBox;
    imgTrashCan: TImage;
    lblAvailableTimeseries: TLabel;
    lblTimeseriesSelections: TLabel;
    btnOK: TButton;
    btnCancel: TButton;
    btnInfo: TBitBtn;
    procedure treeTimeseriesToUseDragOver(Sender, Source: TObject;
      X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure treeTimeseriesToUseDragDrop(Sender, Source: TObject; X,
      Y: Integer);
    procedure imgTrashCanDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure imgTrashCanDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure LFormCreate(Sender: TObject);
    procedure btnInfoClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    FInfoString: string;
    FSelections: TTsSelectionsArray;
    procedure ReadBack;
  public
    procedure Initialize(Selections: TTsSelectionsArray);
  end;

implementation

{$R *.dfm}

{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}

procedure TFrmTimeseriesSelections.LFormCreate(Sender: TObject);
begin
  FInfoString := '';
end;

procedure TFrmTimeseriesSelections.FormDestroy(Sender: TObject);
var
  i: Integer;
begin
  with treeTimeseriesToUse do
    for i := 0 to Items.Count-1 do
      if Items[i].Data <> nil then
        Dispose(Items[i].Data);
end;

function AddPointerIntegerValue(ANumber: Integer): Pointer;
var
  PInteger: ^Integer;
begin
  New(PInteger);
  PInteger^ := ANumber;
  Result := PInteger;
end;

resourcestring
  rsUnique = ' (unique)';
  rsUnlimited = ' (unlimited)';
  rsMax = ' (max: ';

procedure TFrmTimeseriesSelections.Initialize(Selections: TTsSelectionsArray);
var
  i: Integer;
  ATreeNode: TTreeNode;
  s: string;
begin
  {** Clear bellow, does not free the associated Data pointers. Inizialize
      method should be called once after creating the form.
  }
  treeTimeseriesToUse.Items.Clear;
  FInfoString := '';
  FSelections := Selections;
  for i := Low(Selections) to High(Selections) do
  begin
    s := Selections[i].Name;
    case Selections[i].MaxNum of
      0: s := s + rsUnlimited;
      1: s := s + rsUnique;
    else
      s := s + rsMax+IntToStr(Selections[i].MaxNum)+')';
    end;
    FInfoString := FInfoString + s + ' - ' + Selections[i].Description+#13#10;
    SetLength(Selections[i].Timeseries, 0);
    ATreeNode := nil;
    try
      ATreeNode := treeTimeseriesToUse.Items.Add(nil, s);
      ATreeNode.Data := nil;
      ATreeNode.Data := AddPointerIntegerValue(Selections[i].MaxNum);
    except
      if ATreeNode.Data<>nil then Dispose(ATreeNode.Data);
      ATreeNode.Free;
      raise;
    end;
  end;
end;

resourcestring
  rsTimeseriesShouldHave =
    ' variable, at least ';
  rsDefined = ' time series should be defined';

procedure TFrmTimeseriesSelections.ReadBack;
var
  i, j, k: Integer;
begin
  j := 0;
  for i := 0 to treeTimeseriesToUse.Items.Count-1 do
    if treeTimeseriesToUse.Items[i].Level = 0 then Inc(j);
  Assert(Length(FSelections)=j);
  j := 0;
  for i := 0 to treeTimeseriesToUse.Items.Count-1 do
    if treeTimeseriesToUse.Items[i].Level = 0 then
    begin
      if treeTimeseriesToUse.Items[i].Count<FSelections[j].MinNum then
        raise EMissingTimeseries.Create(FSelections[j].Name+
          rsTimeseriesShouldHave+ IntToStr(FSelections[j].MinNum)+rsDefined);
      SetLength(FSelections[j].Timeseries, treeTimeseriesToUse.Items[i].Count);
      for k := 0 to treeTimeseriesToUse.Items[i].Count-1 do
        FSelections[j].Timeseries[k] :=
          Integer(treeTimeseriesToUse.Items[i].Item[k].Data^);
      Inc(j);
    end;
end;

procedure TFrmTimeseriesSelections.treeTimeseriesToUseDragOver(Sender, Source:
  TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  i: Integer;
begin
  Accept := False;
  if Source = nil then Exit;
  if Source = lstAvailableTimeseries then
  begin
    if treeTimeseriesToUse.DropTarget <> nil then
      if treeTimeseriesToUse.DropTarget.Level = 0 then
      begin
        Accept := True;
        if (lstAvailableTimeseries.Items.Count>0) and
        (lstAvailableTimeseries.ItemIndex >-1) then
          for i := 0 to treeTimeseriesToUse.DropTarget.Count-1 do
            with  treeTimeseriesToUse.DropTarget.Item[i] do
              if (Level = 1) and (Data<>nil) then
                if Integer(Data^) = lstAvailableTimeseries.ItemIndex then
                  if Parent = treeTimeseriesToUse.DropTarget then
                    Accept := False;
      end;
  end;
  if Source = treeTimeseriesToUse then
    with treeTimeseriesToUse do
      if DropTarget<>nil then
        if DropTarget.Level = 0 then
          if Selected.Level = 1 then
          begin
            Accept := True;
            for i := 0 to DropTarget.Count-1 do
              if (DropTarget.Item[i].Data<>nil)and (Selected.Data<>nil) then
                if Integer(DropTarget.Item[i].Data^) =
                Integer(Selected.Data^) then
                  Accept := False;
          end;
  with treeTimeseriesToUse do
    if DropTarget <> nil then
      if DropTarget.Level = 0 then
        if Integer(DropTarget.Data^) > 0 then
          if Integer(DropTarget.Data^)<= DropTarget.Count then
            Accept := False;
end;

procedure TFrmTimeseriesSelections.treeTimeseriesToUseDragDrop(Sender,
  Source: TObject; X, Y: Integer);
var
  ATreeNode: TTreeNode;
  AIndex: Integer;
begin
  if treeTimeseriesToUse.DropTarget = nil then Exit;
  if Source = lstAvailableTimeseries then
  begin
    if lstAvailableTimeseries.Items.Count<1 then Exit;
    if lstAvailableTimeseries.ItemIndex<0 then Exit;
    ATreeNode := nil;
    try
      ATreeNode :=
        treeTimeseriesToUse.Items.AddChild(treeTimeseriesToUse.DropTarget,
        lstAvailableTimeseries.Items[lstAvailableTimeseries.ItemIndex]);
      AIndex := lstAvailableTimeseries.ItemIndex;
      ATreeNode.Data := nil;
      ATreeNode.Data := AddPointerIntegerValue(AIndex);
    except
      if ATreeNode.Data<>nil then Dispose(ATreeNode.Data);
      ATreeNode.Free;
      raise;
    end;
    treeTimeseriesToUse.DropTarget.Expand(True);
  end;
  if Source = treeTimeseriesToUse then
  begin
    with treeTimeseriesToUse do
      Selected.MoveTo(DropTarget, naAddChild);
  end;
end;

procedure TFrmTimeseriesSelections.imgTrashCanDragOver(Sender, Source: TObject;
  X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept := False;
  if Source = nil then Exit;
  if Source = treeTimeseriesToUse then
    if treeTimeseriesToUse.Selected.Level = 1 then Accept := True;
end;

procedure TFrmTimeseriesSelections.imgTrashCanDragDrop(Sender, Source: TObject;
  X, Y: Integer);
begin
  if Source = treeTimeseriesToUse then
  begin
    with (Source as TTreeView).Selected do
    begin
      if Data <> nil then
        Dispose(Data);
      Delete;
    end;
  end;
end;

procedure TFrmTimeseriesSelections.btnInfoClick(Sender: TObject);
begin
  ShowMessage(FInfoString);
end;

procedure TFrmTimeseriesSelections.btnOKClick(Sender: TObject);
begin
  ReadBack;
  ModalResult := mrOk;
end;

{$WARN UNSAFE_TYPE ON}
{$WARN UNSAFE_CODE ON}

end.
