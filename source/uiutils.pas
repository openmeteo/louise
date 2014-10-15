{******************************************************************}
{                                                                  }
{  LoUISE library                                                  }
{                                                                  }
{  Copyright (c) 2000-01 National Technical University of Athens   }
{                                                                  }
{******************************************************************}

{** Various general purpose utilities. }
unit uiutils;

interface

uses SysUtils, Classes, Messages, Windows, Grids, dbgrids, ValEdit;

procedure StringGridToClipboard(StringGrid: TStringGrid;
  FirstCol, EndCol, FirstRow, EndRow: Integer);
procedure ValueListEditorToClipboard(ValueListEditor: TValueListEditor;
  FirstCol, EndCol, FirstRow, EndRow: Integer);

{** Pastes the clipboard into a StringGrid.
ClipboardToStringGrid pastes the clipboard into the specified StringGrid. The
clipboard is pasted into a set of cells the top-left of which is at (StartCol,
EndCol).  Numbering of columns and rows of the StringGrid (including fixed
columns and rows) starts from 0. If the pasted area is so large as to require
more rows or columns than those present, more columns or rows are added to the
StringGrid as needed.
The pasted object must be a single string where the cells are delimited by tabs
and CR-LF pairs.  This convention is used by most other programs, including
Microsoft Excel, thus making it possible to paste the text copied from
there.                                         
@SeeAlso <See Routine=ClipboardToStringGrid>
@SeeAlso <See Routine=ClipboardToFitStringGrid>
}
procedure ClipboardToStringGrid(StringGrid: TStringGrid;
  StartCol, StartRow: Integer); overload;
{** This is an overloaded function to specify if to fit to the spceified area.
    Set the Fit to True in order to keep the pasted area without growing with
    new columns or rows, even the clipboard area is larger.
}
procedure ClipboardToStringGrid(StringGrid: TStringGrid;
  StartCol, StartRow: Integer; Fit: Boolean); overload;

{** Same as the overloaded version ClipboardToStringGrid but for TValueListEditor.
    The pasted area will not grow with new columns or rows, even the clipboard
    area is larger.}
procedure ClipboardToValueListEditor(ValueListEditor: TValueListEditor;
  StartCol, StartRow: Integer); overload;

{** Copy a DBGrid to the Clipboard. Only the visible fields of the grid
    are copied, using lookup tables.
}
procedure DBGridToClipboard(DBGrid: TDBGrid);

Procedure CopyStreamToClipboard( fmt: Cardinal; S: TStream );

procedure FixComponentDecSeparators(AComponent: TComponent);

implementation

{$C+}

uses clipbrd, math, StdCtrls;

{$WARN UNSAFE_TYPE OFF}

procedure StringGridToClipboard(StringGrid: TStringGrid;
  FirstCol, EndCol, FirstRow, EndRow: Integer);
var
  i, j: Integer;
  ToClipboard: string;
begin
  ToClipboard := '';
  for i := FirstRow to EndRow do
  begin
    for j := FirstCol to EndCol do
    begin
      if j<>FirstCol then ToClipboard := ToClipboard+#9;
      ToClipboard := ToClipboard+StringGrid.Cells[j,i];
    end;
    ToClipboard := ToClipboard+#13#10;
  end;
  Clipboard.AsText := ToClipboard;
end;


{$WARN UNSAFE_TYPE OFF}

procedure ValueListEditorToClipboard(ValueListEditor: TValueListEditor;
  FirstCol, EndCol, FirstRow, EndRow: Integer);
var
  i, j: Integer;
  ToClipboard: string;
begin
  ToClipboard := '';
  for i := FirstRow to EndRow do
  begin
    for j := FirstCol to EndCol do
    begin
      if j<>FirstCol then ToClipboard := ToClipboard+#9;
      if (i>0) and (j=0) then
         ToClipboard := ToClipboard+ValueListEditor.ItemProps[ValueListEditor.Cells[0,i]].KeyDesc
      else
         ToClipboard := ToClipboard+ValueListEditor.Cells[j,i];
    end;
    ToClipboard := ToClipboard+#13#10;
  end;
  Clipboard.AsText := ToClipboard;
end;


{$WARN UNSAFE_TYPE OFF}

procedure ClipboardToStringGrid(StringGrid: TStringGrid;
  StartCol, StartRow: Integer);
begin
  ClipboardToStringGrid(StringGrid, StartCol, StartRow, False);
end;

procedure ClipboardToStringGrid(StringGrid: TStringGrid;
  StartCol, StartRow: Integer; Fit: Boolean);
var
  i, j: Integer;
  Cliptext, s: string;
  p: PChar;
begin
  Cliptext := Clipboard.AsText;
  i := StartRow;
  j := StartCol;
  p := PChar(Cliptext);
  s := '';
  if (not Fit) and  (p^<>#0) then
  begin
       StringGrid.RowCount:=max(StringGrid.RowCount, StartRow+1);
       StringGrid.ColCount:=max(StringGrid.ColCount, StartCol+1);
  end;
  while p^<>#0 do
  begin
    case p^ of
      #13:
        begin
          StringGrid.Cells[j,i] := s;
          s := '';
          Inc(p); { Skip #10 as well }
          j := StartCol;
          Inc(i);
          if (not Fit) and (i>=StringGrid.RowCount) and ((p+1)^<>#0) then
            StringGrid.RowCount:=StringGrid.RowCount+1;
        end;
      #9:
        begin
          StringGrid.Cells[j,i] := s;
          s := '';
          Inc(j);
          if (not Fit) and (j>=StringGrid.ColCount) then
            StringGrid.ColCount := StringGrid.ColCount+1;
        end;
    else
      s := s+p^;
    end;
    Inc(p);
  end;
  if (s<>'') then
    if Fit then
    begin
      if (i<StringGrid.RowCount) and (j<StringGrid.ColCount) then
        StringGrid.Cells[j,i] := s;
    end else
      StringGrid.Cells[j,i] := s;    
end;

{$WARN UNSAFE_TYPE OFF}


procedure ClipboardToValueListEditor(ValueListEditor: TValueListEditor;
  StartCol, StartRow: Integer);
var
  i, j: Integer;
  Cliptext, s: string;
  p: PChar;
begin
  Cliptext := Clipboard.AsText;
  i := StartRow;
  j := StartCol;
  p := PChar(Cliptext);
  s := '';
  while p^<>#0 do
  begin
    case p^ of
      #13:
        begin
          if (i<ValueListEditor.RowCount) and (j<ValueListEditor.ColCount) then
            ValueListEditor.Cells[j,i] := s;
          s := '';
          Inc(p); { Skip #10 as well }
          j := StartCol;
          Inc(i);
        end;
      #9:
        begin     
          if (i<ValueListEditor.RowCount) and (j<ValueListEditor.ColCount) then
            ValueListEditor.Cells[j,i] := s;
          s := '';
          Inc(j);
        end;
    else
      s := s+p^;
    end;
    Inc(p);
  end;
  if (s<>'') then
    begin
      if (i<ValueListEditor.RowCount) and (j<ValueListEditor.ColCount) then
      begin
        ValueListEditor.Cells[j,i] := s;
      end;
    end;
end;

{$WARN UNSAFE_TYPE OFF}

Procedure CopyStreamToClipboard( fmt: Cardinal; S: TStream );
  Var
    hMem: THandle;
    pMem: Pointer;
  Begin
    S.Position := 0;
    hMem := GlobalAlloc( GHND or GMEM_DDESHARE, S.Size );
    If hMem <> 0 Then Begin
      pMem := GlobalLock( hMem );
      If pMem <> Nil Then Begin
        try
          S.Read( pMem^, S.Size );
          S.Position := 0;
        finally
          GlobalUnlock( hMem );
        end;
        Clipboard.Open;
        try
          Clipboard.SetAsHandle( fmt, hMem );
        finally
          Clipboard.Close;
        end;
      End { If }
      Else Begin
        GlobalFree( hMem );
        OutOfMemoryError;
      End;
    End { If }
    Else
      OutOfMemoryError;
  End; { CopyStreamToClipboard }

{$WARN UNSAFE_TYPE OFF}

procedure DBGridToClipboard(DBGrid: TDBGrid);
var
  ToClipboard, ABookmark: string;
  i: Integer;
begin
  ToClipboard := '';
  ABookmark := string(DBGrid.DataSource.DataSet.Bookmark);
  DBGrid.DataSource.DataSet.First;
  if not DBGrid.DataSource.DataSet.bof then Exit;
  for i := 0 to DBGrid.Columns.Count-1 do
  begin
    ToClipboard := ToClipboard + DBGrid.Columns.Items[i].Field.DisplayName;
    if i <> DBGrid.Columns.Count-1 then ToClipboard := ToClipboard + #9;
  end;
  ToClipboard := ToClipboard + #13;
  with DBGrid.DataSource.DataSet do
  begin
    while not eof do
    begin
      for i := 0 to DBGrid.Columns.Count-1 do
      begin
        ToClipboard := ToClipboard + DBGrid.Columns.Items[i].Field.DisplayText;
        if i <> DBGrid.Columns.Count-1 then ToClipboard := ToClipboard + #9;
      end;
      Next;
      if not eof then ToClipboard := ToClipboard + #13;
    end;
  end;
  Clipboard.AsText := ToClipboard;
  DBGrid.DataSource.DataSet.Bookmark := TBytes(ABookmark);
end;

{$WARN UNSAFE_TYPE OFF}

procedure FixComponentDecSeparators(AComponent: TComponent);
var
  i: Integer;
  AFloat: Real;
  SavedDecimalSeparator: char;
begin
  with AComponent do
    for i := 0 to ComponentCount-1 do
      if Components[i] is TEdit then
        with Components[i] as TEdit do
        begin
          try
            SavedDecimalSeparator := FormatSettings.DecimalSeparator;
            try
              FormatSettings.DecimalSeparator := '.';
              AFloat := StrToFloat(Text);
            finally
              FormatSettings.DecimalSeparator := SavedDecimalSeparator;
            end;
            Text := FloatToStr(AFloat);
          except
            {}
          end;
        end;
end;

end.
