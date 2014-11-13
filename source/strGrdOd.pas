{******************************************************************}
{                                                                  }
{  LoUISE library                                                  }
{                                                                  }
{  Copyright (c) 2000-06 National Technical University of Athens   }
{                                                                  }
{******************************************************************}

{** A set of components designed to easily visualize time series.}
Unit StrGrdOd;

Interface

Uses Windows, SysUtils, Messages, Classes, Controls, Grids, Graphics, Dialogs,
     Menus, genutils, dbgrids, ValEdit;


{  Modify tyhe following values in order to change the default property values
   for TOdStringGrid
}
const ODSTRGRDDEFAULTCOLOR             = clWhite;
      ODSTRGRDDEFAULTFIXEDCOLOR        = $00DDDDDD;
      ODSTRGRDDEFAULTSELECTEDCELLCOLOR = $00804000;
      ODSTRGRDDEFAULTSELECTEDFONTCOLOR = clWhite;
      ODSTRGRDDEFAULTROWHEIGHT         = 18;
      ODMINCOLWIDTH                    = 30;


type

  TOdStringGrid = class;
  TOdValueListEditor = class;

  {** A db grid with Odysseus standard colours
  }
  TOdDBGrid = class(TDBGrid)
  public
    constructor Create(AOwner: TComponent); override;
  end;


  {** TOdPopupMenu is based on TPopupMenu.
      TOdPopupMenu prevents some menu items under certain conditions from being selected.
  }
  TOdPopupMenu = class(TPopupMenu)
  private
     strGrid:TOdStringGrid;
  public
     procedure Popup(X, Y: Integer); override;
  end;

  {** TOdStringGrid is based on TStringGrid and introduces some new futures
      which may be useful in applications
  }
  TOdStringGrid = class(TStringGrid)
  private
     saveDialogStrg:TSaveDialog;
     FSelectedCellColor:TColor;
     FSelectedFontColor:TColor;
     FGoPasteNonEditable:Boolean;
     FFitColToWidth:Integer;
     procedure SetSelectedCellColor(Value:TColor);
     procedure SetSelectedFontColor(Value:TColor);
     procedure SetFitColToWidth(Value:Integer);
     procedure SelectAll;
  public
  {** This is the default Popup menu of the TOdStringGrid generated at run time.
      The following menu items are currently available:
        * Select all (Selects all - fixed and non-fixed - cells of the grid)
        * Copy - Copies the selected fields or the entire grid into the clipboard.
                If goRangeSelect	is selected and goEditing=false then only the
                selected fields are copied into the clipboard. Otherwise the
                entire Grid is copied.)
        * Paste - Pastes the clipboard into a StringGrid. The clipboard is pasted
                 into a set of cells the top-left of which is the first non fixed
                 selected cell.
                 If the pasted area is larger than the available cell area, no
                 new columns or rows are added.
                 This menu item is enabled only if the Grid is editable.
        * Save to CSV-file - Saves the grid in a CSV-file
      Any popup menu associated with the TOdStringGrid at design time will cover
      this menu. Restore the default menu by assigning odPopUpMenu to popupenu.
      @SeeAlso <See Method=OdStringGridToClipboard>
      @SeeAlso <See Method=OdClipboardToFitStringGrid>
      @SeeAlso <See Method=saveToCSV>
  }
     odPopUpMenu:TOdPopupMenu;  
     procedure clickPopupMenu(Sender: TObject); virtual;
  {** Saves the Grid in a comma-separated values (CSV) file format
  }
     procedure saveToCSV(const FileName: String);
  {** Copies the Grid cells specified by FirstCol, EndCol, FirstRow, EndRow
      into the clipboard using StringGridToClipboard.
      @SeeAlso <See Method=OdClipboardToFitStringGrid>
  }
     procedure OdStringGridToClipboard(FirstCol, EndCol, FirstRow, EndRow: Integer);
  {** Pastes the clipboard into the StringGrid using ClipboardToFitStringGrid.
      @SeeAlso <See Method=OdStringGridToClipboard>
  }
     procedure OdClipboardToFitStringGrid(StartCol, StartRow: Integer);  virtual;
  {** Paints the selected cells with a distinguish color.
      @SeeAlso <See Property=SelectedCellColor>
      @SeeAlso <See Property=SelectedFontColor>      
  }
     procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
  {** Takes into account the FitColToWidth property.
      @SeeAlso <See Property=FitColToWidth>
  }
     procedure Paint; override;
  {** Uses the quicksort algorithm to sort the rows l to r
      of the grid by the values of column col, which have to be real.
  }
     procedure sortGrid(l,r: integer; col:Integer);
  {** Uses the quicksort algorithm to sort the grid by column
      in ascending or descending order.
  }
     procedure SortStringgrid(byColumn: LongInt; ascending: Boolean);
     constructor Create(AOwner: TComponent); override;
     destructor Destroy; override;
  published
  {** Introduces a distiguish color for selected cells as a property
  }
     property SelectedCellColor:TColor read FSelectedCellColor write SetSelectedCellColor;
  {** Introduces a distiguish color for fonts of selected cells as a property
  }
     property SelectedFontColor:TColor read FSelectedFontColor write SetSelectedFontColor;
  {** Modifies the width of the column with the assigned index number so that
      the grid fits (if possible) to the components client width.
      Set the property at design or run-time in order to expand or shrink
      the width of the column with the assigned index number. The column width
      can shrink only up to a number of pixels given by ODMINCOLWIDTH.
      The first column is column zero.
      If the value of the property is negative or greater than the number of
      columns-1 no action is taken. -1 is the default value.
  }
     property FitColToWidth:Integer read FFitColToWidth write SetFitColToWidth;
  {** Controls if paste on non fixed non editable cells of the grid is allowed
  }
     property goPasteNonEditable:Boolean read FGoPasteNonEditable write FGoPasteNonEditable;
  end;


  {** TOdValPopupMenu is based on TPopupMenu.
      TOdValPopupMenu prevents some menu items under certain conditions from being selected.
  }
  TOdValPopupMenu = class(TPopupMenu)
  private
     strGrid:TOdValueListEditor;
  public
     procedure Popup(X, Y: Integer); override;
  end;

  {** TOdValueListEditor is based on TValueListEditor and introduces some new futures
      which may be useful in applications
  }
  TOdValueListEditor = class(TValueListEditor)
  private
     saveDialogStrg:TSaveDialog;
     FSelectedCellColor:TColor;
     FSelectedFontColor:TColor;
     FGoPasteNonEditable:Boolean;
     FFitColToWidth:Integer;
     procedure SetSelectedCellColor(Value:TColor);
     procedure SetSelectedFontColor(Value:TColor);
     procedure SetFitColToWidth(Value:Integer);
     procedure SelectAll;
  public
  {** This is the default Popup menu of the TOdValueListEditor generated at run time.
      The following menu items are currently available:
        * Select all (Selects all - fixed and non-fixed - cells of the grid)
        * Copy - Copies the selected fields or the entire grid into the clipboard.
                If goRangeSelect	is selected and goEditing=false then only the
                selected fields are copied into the clipboard. Otherwise the
                entire Grid is copied.)
        * Paste - Pastes the clipboard into a StringGrid. The clipboard is pasted
                 into a set of cells the top-left of which is the first non fixed
                 selected cell.
                 If the pasted area is larger than the available cell area, no
                 new columns or rows are added.
                 This menu item is enabled only if the Grid is editable.
        * Save to CSV-file - Saves the grid in a CSV-file
      Any popup menu associated with the TOdStringGrid at design time will cover
      this menu. Restore the default menu by assigning odPopUpMenu to popupenu.
      @SeeAlso <See Method=OdStringGridToClipboard>
      @SeeAlso <See Method=OdClipboardToFitStringGrid>
      @SeeAlso <See Method=saveToCSV>
  }
     odPopUpMenu:TOdValPopupMenu;
     procedure clickPopupMenu(Sender: TObject); virtual;
  {** Saves the Grid in a comma-separated values (CSV) file format
  }
     procedure saveToCSV(const FileName: String);
  {** Copies the Grid cells specified by FirstCol, EndCol, FirstRow, EndRow
      into the clipboard using StringGridToClipboard.
      @SeeAlso <See Method=OdClipboardToFitStringGrid>
  }
     procedure OdStringGridToClipboard(FirstCol, EndCol, FirstRow, EndRow: Integer);
  {** Pastes the clipboard into the StringGrid using ClipboardToFitStringGrid.
      @SeeAlso <See Method=OdStringGridToClipboard>
  }
     procedure OdClipboardToFitStringGrid(StartCol, StartRow: Integer);  virtual;
  {** Paints the selected cells with a distinguish color.
      @SeeAlso <See Property=SelectedCellColor>
      @SeeAlso <See Property=SelectedFontColor>      
  }
     procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
  {** Takes into account the FitColToWidth property.
      @SeeAlso <See Property=FitColToWidth>
  }
     procedure Paint; override;
     constructor Create(AOwner: TComponent); override;
     destructor Destroy; override;
  published
  {** Introduces a distiguish color for selected cells as a property
  }
     property SelectedCellColor:TColor read FSelectedCellColor write SetSelectedCellColor;
  {** Introduces a distiguish color for fonts of selected cells as a property
  }
     property SelectedFontColor:TColor read FSelectedFontColor write SetSelectedFontColor;
  {** Modifies the width of the column with the assigned index number so that
      the grid fits (if possible) to the components client width.
      Set the property at design or run-time in order to expand or shrink
      the width of the column with the assigned index number. The column width
      can shrink only up to a number of pixels given by ODMINCOLWIDTH.
      The first column is column zero.
      If the value of the property is negative or greater than the number of
      columns-1 no action is taken. -1 is the default value.
  }
     property FitColToWidth:Integer read FFitColToWidth write SetFitColToWidth;
  {** Controls if paste on non fixed non editable cells of the grid is allowed
  }
     property goPasteNonEditable:Boolean read FGoPasteNonEditable write FGoPasteNonEditable;
  end;



procedure SortStrGrid(Grid: TStringGrid; byColumn: LongInt; ascending: Boolean);

implementation

uses Clipbrd, math, uiutils, genopts;


resourcestring
  rsErrorWriteFile = 'Unable to write to file:';
  rsSaveToCSV      = 'Save to CSV-file';
  rsItemSelectAll  = 'Select all';
  rsCopy           = 'Copy';
  rsPaste          = 'Paste';



(************************************************)
(************************************************)
(*                 TOdPopupMenu                 *)
(************************************************)
(************************************************)

procedure TOdPopupMenu.Popup(X, Y: Integer);
var i:Integer;
begin
     for i:=0 to Items.count-1 do
       if (Items[i].name='mnItemPaste') then
       begin
            if (goEditing in strGrid.options) or (strGrid.goPasteNonEditable) then
                  Items[i].enabled:=true
            else  Items[i].enabled:=false;
       end;
     inherited popup(X,Y);
end;




(************************************************)
(************************************************)
(*              TOdValPopupMenu                 *)
(************************************************)
(************************************************)

procedure TOdValPopupMenu.Popup(X, Y: Integer);
var i:Integer;
begin
     for i:=0 to Items.count-1 do
       if (Items[i].name='mnItemPaste') then
       begin
            if (goEditing in strGrid.options) or (strGrid.goPasteNonEditable) then
                  Items[i].enabled:=true
            else  Items[i].enabled:=false;
       end;
     inherited popup(X,Y);
end;



(************************************************)
(************************************************)
(*                   TOdDBGrid                  *)
(************************************************)
(************************************************)

constructor TOdDBGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FixedColor := ODSTRGRDDEFAULTFIXEDCOLOR;
end;




(************************************************)
(************************************************)
(*                 TOdStringGrid                *)
(************************************************)
(************************************************)

constructor TOdStringGrid.create(AOwner: TComponent);
begin
  inherited create(AOwner);
  // Create a popup menu

  odPopUpMenu := nil;
  saveDialogStrg := nil;
     try
           odPopUpMenu:=TOdPopupMenu.create(Self);
           odPopUpMenu.strGrid:=Self;
        //create the menu list
        //select all
           odPopUpMenu.Items.Add(NewItem(rsItemSelectAll, TextToShortCut(''),false,true, clickPopupMenu, 0, 'mnItemSelectAll'));
        //copy
           odPopUpMenu.Items.Add(NewItem(rsCopy, TextToShortCut(''),false,true, clickPopupMenu, 0, 'mnItemCopy'));
        //paste
           odPopUpMenu.Items.Add(NewItem(rsPaste, TextToShortCut(''),false,true, clickPopupMenu, 0, 'mnItemPaste'));
        //save to CSV file
           odPopUpMenu.Items.Add(NewItem(rsSaveToCSV, TextToShortCut(''),false,true, clickPopupMenu, 0, 'mnSaveCSV'));

        //Create a TSaveDialog
           saveDialogStrg:=TSaveDialog.Create(Self);
           try
           with saveDialogStrg do
           begin
                Filter := 'comma-separated values (*.csv)|*.CSV';
                DefaultExt:='csv';
           end;
           except
                saveDialogStrg.Free;
                raise
           end;
     except
           odPopUpMenu.free;
           raise;
     end;


     PopupMenu           := odPopUpMenu;

   //apply default property values
     FFitColToWidth      := -1;
     FGoPasteNonEditable := false;
     Color               := ODSTRGRDDEFAULTCOLOR;
     FixedColor          := ODSTRGRDDEFAULTFIXEDCOLOR;
     FSelectedCellColor  := ODSTRGRDDEFAULTSELECTEDCELLCOLOR;
     FSelectedFontColor  := ODSTRGRDDEFAULTSELECTEDFONTCOLOR;
     DefaultRowHeight    := ODSTRGRDDEFAULTROWHEIGHT;
end;



destructor TOdStringGrid.destroy;
var MenuItem1: TMenuItem;
    i:Integer;
begin
     for i:=PopupMenu.Items.Count-1 downto 0 do
     begin
          MenuItem1:=PopupMenu.Items[i];
          MenuItem1.Free;
     end;
     PopupMenu.free;
     saveDialogStrg.free;
     inherited destroy;
end;

procedure TOdStringGrid.SetSelectedCellColor(Value:TColor);
begin
     FSelectedCellColor:=Value;
     invalidate;
end;


procedure TOdStringGrid.SetSelectedFontColor(Value:TColor);
begin
     FSelectedFontColor:=Value;
     invalidate;
end;


procedure TOdStringGrid.SetFitColToWidth(Value:Integer);
var NrCol,allColWidths,i,diff:Integer;
begin
     FFitColToWidth := Value;
     NrCol:=ColCount;
     if (Value<0) or (Value>NrCol-1) then exit;

     allColWidths:=0;
     for i:=0 to NrCol-1 do  allColWidths:=allColWidths+ColWidths[i];
     diff:=ColWidths[Value]+(ClientWidth-allColWidths)-(NrCol+1)*GridLineWidth;
     ColWidths[Value] := max(ODMINCOLWIDTH, diff);
end;


procedure TOdStringGrid.DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
begin
     if gdSelected	in AState then
     begin
          canvas.brush.color := SelectedCellColor;
          canvas.font.color  := SelectedFontColor;
     end
     else canvas.font.color  := font.color;

     { At this point there used to be this statement:
         canvas.textrect (ARect,ARect.left+2,ARect.top+0,cells[ACol,Arow]);
       It's not clear to us why it was here, and in Delphi XE5 it causes
       the headings of the grid to be printed twice, one of them slightly
       displaced. We temporarily leave this comment here, but if there are no
       problems for some time it should be deleted. A.X., 2014-11-13. }

     inherited DrawCell(ACol, ARow, ARect, AState);
end;


procedure TOdStringGrid.paint;
begin
     SetFitColToWidth(FFitColToWidth);
     inherited paint;
end;


procedure TOdStringGrid.clickPopupMenu(Sender: TObject);
var  SelectedMenueItem:TMenuItem;
begin
     SelectedMenueItem:=Sender as TMenuItem;
     if SelectedMenueItem.Name='mnItemSelectAll' then  SelectAll
     else if SelectedMenueItem.Name='mnItemCopy' then
        if (goRangeSelect in options) and (not (goEditing in options)) then
           OdStringGridToClipboard(selection.Left, selection.right, selection.Top, selection.Bottom)
        else
           OdStringGridToClipboard(0, ColCount-1, 0, RowCount-1)
     else if SelectedMenueItem.Name='mnItemPaste' then
     begin
           OdClipboardToFitStringGrid(max(selection.Left,FixedCols),max(selection.Top,FixedRows));
     end
     else if SelectedMenueItem.Name='mnSaveCSV' then
        if saveDialogStrg.Execute then  SaveToCSV(saveDialogStrg.FileName);
end;


procedure TOdStringGrid.SelectAll;
var   myRect: TGridRect;
begin
  myRect.Left   := 0;
  myRect.Top    := 0;
  myRect.Right  := ColCount-1;
  myRect.Bottom := RowCount-1;
  Selection     := myRect;
end;


procedure TOdStringGrid.SaveToCSV(const FileName: String);
var
   F:TextFile;
   iRow,iCol : integer;
   quote, s: string;
begin
  try
    AssignFile(F, Filename);
    Rewrite(F);
    for iRow := 0 to RowCount-1 do
    begin
      for iCol := 0 to ColCount-1 do
      begin
        S := Cells[iCol,iRow];
        if Pos(',', S) > 0 then Quote := '"' else Quote := '';
        if iCol = ColCount-1 then
          Writeln(F, Quote,S,Quote)
        else
          Write(F, Quote,S,Quote,',');
      end;
    end;
    CloseFile(F);
  except
    MessageDlg(rsErrorWriteFile +#13+ Filename, mtError, [mbOK], 0);
  end;
end;


procedure TOdStringGrid.OdStringGridToClipboard(FirstCol, EndCol, FirstRow, EndRow: Integer);
begin
  StringGridToClipboard(TStringGrid(Self), FirstCol, EndCol, FirstRow, EndRow);
end;


procedure TOdStringGrid.OdClipboardToFitStringGrid(StartCol, StartRow: Integer);
begin
    if (goEditing in options) or (goPasteNonEditable) then
       ClipboardToStringGrid(TStringGrid(Self), StartCol, StartRow, True);
end;


procedure TOdStringGrid.sortGrid(l,r: integer; col:Integer);
var
   i,j,k: integer;
   x2, y2: real;
begin
      i:=l;  j:=r;  x2:=StrToFloat(Cells[col,((l+r) DIV 2)]);
      repeat
        while (StrToFloat(Cells[col,i])<x2) do i :=i+1;
        while (x2<StrToFloat(Cells[col,j])) do j :=j-1;
        if i<=j then
        begin
          for k:=0 to ColCount-1 do
          begin
               y2:=StrToFloat(Cells[k,i]);
               Cells[k,i]:=Cells[k,j];
               Cells[k,j]:=FloatToStr(y2);
          end;
          i :=i+1; j :=j-1;
        end;
      until i>j;
      if l<j then sortGrid(l,j,col);
      if i<r then sortGrid(i,r,col);
end;


Procedure SortStrGrid( Grid: TStringGrid; byColumn: LongInt;
                       ascending: Boolean );
  Procedure ExchangeGridRows( i, j: Integer );
  Var
    k: Integer;
  Begin
    With Grid Do
      For k:= 0 To ColCount-1 Do
        Cols[k].Exchange(i,j);
  End;
  procedure QuickSort(L, R: Integer);
  var
    I, J: Integer;
    P: String;
  begin
    repeat
      I := L;
      J := R;
      P := Grid.Cells[byColumn, (L + R) shr 1];
      repeat
        while CompareStr(Grid.Cells[byColumn, I], P) < 0 do Inc(I);
        while CompareStr(Grid.Cells[byColumn, J], P) > 0 do Dec(J);
        if I <= J then
        begin
          If I <> J Then
            ExchangeGridRows( I, J );
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if L < J then QuickSort(L, J);
      L := I;
    until I >= R;
  end;
 Procedure InvertGrid;
   Var
     i, j: Integer;
   Begin
     i:= Grid.Fixedrows;
     j:= Grid.Rowcount-1;
     While i < j Do Begin
       ExchangeGridRows( I, J );
       Inc( i );
       Dec( j );
     End; { While }
   End;
Begin
  //Screen.Cursor := crHourglass;
  Grid.Perform( WM_SETREDRAW, 0, 0 );
  try
    QuickSort( Grid.FixedRows, Grid.Rowcount-1 );
    If not ascending Then
      InvertGrid;
  finally
    Grid.Perform( WM_SETREDRAW, 1, 0 );
    Grid.Refresh;
    //Screen.Cursor := crDefault;
  end;
End;

procedure TOdStringGrid.SortStringgrid(byColumn: LongInt; ascending: Boolean);
begin
    SortStrGrid(self, byColumn, ascending);
end;



(************************************************)
(************************************************)
(*                TOdValueListEditor            *)
(************************************************)
(************************************************)

constructor TOdValueListEditor.create(AOwner: TComponent);
begin
  odPopUpMenu := nil;
  saveDialogStrg := nil;
  inherited create(AOwner);
     try
        // Create a popup menu
           odPopUpMenu:=TOdValPopupMenu.create(Self);
           odPopUpMenu.strGrid:=Self;
        //create the menu list
        //select all
           odPopUpMenu.Items.Add(NewItem(rsItemSelectAll, TextToShortCut(''),false,true, clickPopupMenu, 0, 'mnItemSelectAll'));
        //copy
           odPopUpMenu.Items.Add(NewItem(rsCopy, TextToShortCut(''),false,true, clickPopupMenu, 0, 'mnItemCopy'));
        //paste
           odPopUpMenu.Items.Add(NewItem(rsPaste, TextToShortCut(''),false,true, clickPopupMenu, 0, 'mnItemPaste'));
        //save to CSV file
           odPopUpMenu.Items.Add(NewItem(rsSaveToCSV, TextToShortCut(''),false,true, clickPopupMenu, 0, 'mnSaveCSV'));

        //Create a TSaveDialog
           saveDialogStrg:=TSaveDialog.Create(Self);
           try
           with saveDialogStrg do
           begin
                Filter := 'comma-separated values (*.csv)|*.CSV';
                DefaultExt:='csv';
           end;
           except
                saveDialogStrg.Free;
                raise
           end;
     except
           odPopUpMenu.free;
           raise;
     end;


     PopupMenu           := odPopUpMenu;

   //apply default property values
     FFitColToWidth      := -1;
     FGoPasteNonEditable := false;
     Color               := ODSTRGRDDEFAULTCOLOR;
     FixedColor          := ODSTRGRDDEFAULTFIXEDCOLOR;
     FSelectedCellColor  := ODSTRGRDDEFAULTSELECTEDCELLCOLOR;
     FSelectedFontColor  := ODSTRGRDDEFAULTSELECTEDFONTCOLOR;
     DefaultRowHeight    := ODSTRGRDDEFAULTROWHEIGHT;
end;



destructor TOdValueListEditor.destroy;
var MenuItem1: TMenuItem;
    i:Integer;
begin
     for i:=PopupMenu.Items.Count-1 downto 0 do
     begin
          MenuItem1:=PopupMenu.Items[i];
          MenuItem1.Free;
     end;
     PopupMenu.free;
     saveDialogStrg.free;
     inherited destroy;
end;

procedure TOdValueListEditor.SetSelectedCellColor(Value:TColor);
begin
     FSelectedCellColor:=Value;
     invalidate;
end;


procedure TOdValueListEditor.SetSelectedFontColor(Value:TColor);
begin
     FSelectedFontColor:=Value;
     invalidate;
end;


procedure TOdValueListEditor.SetFitColToWidth(Value:Integer);
var NrCol,allColWidths,i,diff:Integer;
begin
     FFitColToWidth := Value;
     NrCol:=ColCount;
     if (Value<0) or (Value>NrCol-1) then exit;

     allColWidths:=0;
     for i:=0 to NrCol-1 do  allColWidths:=allColWidths+ColWidths[i];
     diff:=ColWidths[Value]+(ClientWidth-allColWidths)-(NrCol+1)*GridLineWidth;
     ColWidths[Value] := max(ODMINCOLWIDTH, diff);
end;


procedure TOdValueListEditor.DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
begin
     if gdSelected	in AState then
     begin
          canvas.brush.color := SelectedCellColor;
          canvas.font.color  := SelectedFontColor;
     end
     else canvas.font.color  := font.color;

     { At this point there used to be this statement:
         canvas.textrect (ARect,ARect.left+2,ARect.top+0,cells[ACol,Arow]);
       It's not clear to us why it was here, and in Delphi XE5 it causes
       the headings of the grid to be printed twice, one of them slightly
       displaced. We temporarily leave this comment here, but if there are no
       problems for some time it should be deleted. A.X., 2014-11-13. }

     inherited DrawCell(ACol, ARow, ARect, AState);
end;


procedure TOdValueListEditor.paint;
begin
     SetFitColToWidth(FFitColToWidth);
     inherited paint;
end;


procedure TOdValueListEditor.clickPopupMenu(Sender: TObject);
var  SelectedMenueItem:TMenuItem;
begin
     SelectedMenueItem:=Sender as TMenuItem;
     if SelectedMenueItem.Name='mnItemSelectAll' then  SelectAll
     else if SelectedMenueItem.Name='mnItemCopy' then
        if (goRangeSelect in options) and (not (goEditing in options)) then
           OdStringGridToClipboard(selection.Left, selection.right, selection.Top, selection.Bottom)
        else
           OdStringGridToClipboard(0, ColCount-1, 0, RowCount-1)
     else if SelectedMenueItem.Name='mnItemPaste' then
     begin
           OdClipboardToFitStringGrid(max(selection.Left,FixedCols),max(selection.Top,FixedRows));
     end
     else if SelectedMenueItem.Name='mnSaveCSV' then
        if saveDialogStrg.Execute then  SaveToCSV(saveDialogStrg.FileName);
end;


procedure TOdValueListEditor.SelectAll;
var   myRect: TGridRect;
begin
  myRect.Left   := 0;
  myRect.Top    := 0;
  myRect.Right  := ColCount-1;
  myRect.Bottom := RowCount-1;
  Selection     := myRect;
end;


procedure TOdValueListEditor.SaveToCSV(const FileName: String);
var
   F:TextFile;
   iRow,iCol : integer;
   quote, s: string;
begin
  try
    AssignFile(F, Filename);
    Rewrite(F);
    for iRow := 0 to RowCount-1 do
    begin
      for iCol := 0 to ColCount-1 do
      begin
        S := Cells[iCol,iRow];
        if Pos(',', S) > 0 then Quote := '"' else Quote := '';
        if iCol = ColCount-1 then
          Writeln(F, Quote,S,Quote)
        else
          Write(F, Quote,S,Quote,',');
      end;
    end;
    CloseFile(F);
  except
    MessageDlg(rsErrorWriteFile +#13+ Filename, mtError, [mbOK], 0);
  end;
end;


procedure TOdValueListEditor.OdStringGridToClipboard(FirstCol, EndCol, FirstRow, EndRow: Integer);
begin
  ValueListEditorToClipboard(TValueListEditor(Self), FirstCol, EndCol, FirstRow, EndRow);
end;


procedure TOdValueListEditor.OdClipboardToFitStringGrid(StartCol, StartRow: Integer);
begin
    if (goEditing in options) or (goPasteNonEditable) then
       ClipboardToValueListEditor(TValueListEditor(Self), StartCol, StartRow);
end;




End.
