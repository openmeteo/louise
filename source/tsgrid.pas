{******************************************************************}
{                                                                  }
{  LoUISE library                                                  }
{                                                                  }
{  Copyright (c) 2000-04 National Technical University of Athens   }
{                                                                  }
{******************************************************************}

{** A set of components designed to easily visualize time series.}
unit tsgrid;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, Ts, Math, extctrls, comctrls, stdctrls, buttons, menus, Contnrs,
  filterdlg, Dates, icomponent, StrGrdOd;

type
  TDateDisplayFormat = (ddfComplex, ddfSimple);

type
  {** Represents the time series display format.
      @author A.X.
      @SeeAlso <See Property=TTimeseriesGrid.DisplayFormat>
  }
  TDisplayFormat = (dfSimple, dfTable);

  {** Represents various highlighting modes.
      @author A.X.
      @SeeAlso <See Property=TTimeseriesGird.HighlightMode>
  }
  THighlightMode = (hlNone, hlFlag, hlMax, hlMin, hlMaxAndMin, hlLarge, hlSmall,
    hlLargeAndSmall, hlNull);

  {** A single undo buffer holding original records and periods.
      An undo buffer is holding several properties of the orignal time series
      such as the Undo action as a string (Caption), modified flag,
      the period altering the time series (between FirstRecordDate and
      LastRecordDate) and the original time series records.
      @author Stefanos
  }
  TTsGridBufferEntry = class(TPersistent)
  public
    Caption: string;
    ModifiedFlag: Boolean;
    ID: Integer;
    FirstRecordDate: TDateTime;
    LastRecordDate: TDateTime;
    Records: TTimeseries;
    constructor Create;
    destructor Destroy; override;
  end;

  {** A set of undo buffers for a single timeseries of a ts grid.
      @author Stefanos
  }
  TTsGridSingleUndoBuffer = class(TObjectList)
  public
  {** The Undo pointer is used to control the depth of undo - redo actions.
      @author Stefanos
  }
    UndoPointer: Integer;
    IsProcessing: Boolean;
    constructor Create(AOwnsObjects: Boolean);
  end;

  {** An object list holding sets of unfo buffers for the whole time series grid.
  }
  TTsGridUndoBuffers = class(TObjectList)
  end;

{ TTimeseriesGrid }

const
  tgMaxCols = 37;
  tgMaxMonthDays = 31;
  tgMaxTimeseries =500;

type

  {** Used internally as the building block of TDisplayedTable.
  }
  TDisplayedTableItem = record
    DisplayedValue: string;
    RelatedTsRecord: TTsRecord;
    DateRepresented: TDateTime;
    BgColor: TColor;
  end;
  {** Used internally to hold a row of a displayed table.
  }
  TDisplayedRow = array[0..tgMaxCols-1] of TDisplayedTableItem;
  {** Used internally to hold displayed table for dfTable display format.
  }
  TDisplayedTable = array of TDisplayedRow;
  {** A type used to specify how record filtering is done.
      TFilterCondition is used by the FilterCondition property of
      TTimeseriesGrid to specify the
      kind of filtering. fcGreater means that a record will be filtered out
      unless it is greater than FilterValue. Likewise for fcLess.
      fcFlag means that a record will be
      filtered out unless it has the flag specified by FilterValue set. Likewise
      for fcHasNotFlag. Finally, fcIsNull and fcIsNotNull allow null and not
      null values respectively.<p>
      fcNone is only used internally. Do not use fcNone.
      @SeeAlso <See Property=TTimeseriesGrid.Filtered>
      @SeeAlso <See Property=TTimeseriesGrid.FilterCondition>
      @SeeAlso <See Property=TTimeseriesGrid.FilterValue>
  }
  TFilterCondition = (fcNone, fcGreater, fcLess, fcHasFlag, fcHasNotFlag,
    fcIsNull, fcIsNotNull, fcIntactRecords, fcModifiedRecords, fcNewRecords);

  {** Used internally to specify how a time stamp is displayed.
      The meanings of the values are:
      <ul>
        <li>sfFull: Display as yyyy/mm/dd hh:mm
        <li>sfMonth: Display as yyyy/mm
        <li>sfYear: Display as yyyy
        <li>sfHydrologicalYear: Display as yyyy-yy
      </ul>
  }
  TTimeStampFormat = record
    MonthSpan: Integer;
    Cardinal: Integer;
    constructor Create(ACardinal: Integer);
    class operator Equal(a: TTimeStampFormat; b: TTimeStampFormat): Boolean;
  end;

  var sfFull, sfMonth, sfYear, sfHydrologicalYear, sfManyMonths,
    sfManyYears: TTimeStampFormat;

  type
  {** Used internally in an array which holds the grid's time stamp column.}
  TGridTimeStamp = record
    SortingDate, DisplayDate: TDateTime;
    DateFormat: TTimeStampFormat;
  end;

  {** Used internally to hold the statistics for a row or column. }
  type TRowColStatistics = record
    Mean, Sum, StDev, VarianceCoef, MaxValue, MinValue: Real;
    UpperLimit, LowLimit: Real;
    ValueCount, MissingValues, HighValues, LowValues: Integer;
  end;

  {** Used to specify an index range. }
  type TIndexRange = record
    StartIndex, EndIndex: Integer;
  end;

  TTimeseriesGridFont = class;

  {** A grid control designed to simplify and automate the handling of time series.
      TTimeseriesGrid is used instead of TStringGrid whenever a list or table of
      time series data is to be viewed. TTimeseriesGrid includes functionality
      to display time series with a number of standard formats.
      @author A.X.
  }
  TTimeseriesGrid = class(TCustomGrid)
  private
    { Warning: Never use the private attribute FActiveIndex, even in the
      implementation of TTimeseriesGrid; see the implementation of the function
      GetActiveIndex for an explanation. }
    FActiveIndex: Integer;
    FData: TObjectList;
    FBaseDate: TDateTime;
    FFilteredSeries: TTimeseries;
    FNewBaseDate: TDateTime; { See FNewDisplayFormat below for an
      explanation. }
    FDates: array of TGridTimeStamp;
    FDisplayFormat: TDisplayFormat;
    FFirstColumnDefaultWidth: Integer;
    FNewDisplayFormat: TDisplayFormat; { While reformatting the grid,
      FDisplayFormat actually holds the display format as it was before starting
      to reformat; FNewDisplayFormat holds the new display format (they may be
      the same if reformatting is not caused by changing the display format).
      See also the implementation of GetDisplayFormat for more information. }
    FIsReformatting: Boolean; {  Becomes true if grid is in the process of being
                                 reformatted. }
    FFullyInvalidated: Boolean;
    FFlagsVisible: Boolean;
    FLowerBoundCoefficient: Single;
    FStatisticsVisible: Boolean;
    FUpperBoundCoefficient: Single;
    FFontForModified, FFontForUnmodified, FFontForNew: TTimeseriesGridFont;
    FSavedFont: TFont; { See private procs SaveFont and RestoreFont for this }
    FBgColorForStatistics: TColor;
    FHighlightColor: TColor;
    FFilteredColor: TColor;
    FHighlightMode: THighlightMode;
    FDisplayedTable: TDisplayedTable;
    FFiltered: Boolean;
    FFilterCondition: TFilterCondition;
    FFilterValue: Variant;
    FReadOnly: Boolean;
    FCheckOnly: Boolean; { For an explanation of FCheckOnly, see SetEditText. }
    FOverwriteDates: Boolean; { To check for existing records when pasting}
    FUseDateOffset: Boolean; { Used to specify how to sort records when viewing
      time series as list. When mixing monthly and annual with smaller time
      steps, it is set to True by Reformat; otherwise, it is set to False by
      Reformat. See TTimeseries.SortingDate for more information. }
    FHydrologicalYear: Boolean;
    FNewHydrologicalYear: Boolean; { For an explanation of this, see
      FNewDisplayFormat above. }
    FSavedColWidths: array of Integer; { Used to temporarily save ColWidths
      when changing from dfSimple to dfTable; when returning to dfSimple,
      original column widths are restored. }
    FOnSelectCell: TSelectCellEvent;
    FOnDrawCell: TDrawCellEvent;
    FOnTopLeftChanged: TNotifyEvent;
    FOnColumnMoved: TMovedEvent;
    FSavedPopupMenu: TPopupMenu;
    FUndoBuffers: TTsGridUndoBuffers;
    FUndoIDPointer: Integer;
    FDontUndoFlag: Boolean;
    FHYearOrigin: Integer;
    procedure SetFiltered(Filtered: Boolean);
    procedure ReformatSimple;
    procedure ReformatTable;
    procedure DrawCellSimple(ACol, ARow: Longint; ARect: TRect);
    procedure DrawCellTable(ACol, ARow: Longint; ARect: TRect);
    function GetCount: Integer;
    procedure SetLowerBoundCoefficient(Value: Single);
    procedure SetStatisticsVisible(Value: Boolean);
    procedure SetUpperBoundCoefficient(Value: Single);
    function GetActiveIndex: Integer;
    procedure SetActiveIndex(Value: Integer);
    function GetActiveDate: TDateTime;
    procedure SetActiveDate(Value: TDateTime);
    function GetBaseDate: TDateTime;
    procedure SetBaseDate(Value: TDateTime);
    function GetDisplayFormat: TDisplayFormat;
    procedure SetDisplayFormat(Value: TDisplayFormat);
    procedure SetFlagsVisible(Value: Boolean);
    procedure SetReadOnly(Value: Boolean);
    function GetReadOnly: Boolean;
    function Get(Index: Integer): TTimeseries;
    procedure Put(Index: Integer; Item: TTimeseries);
    { See the implementation section for an explanation }
    procedure AdjustFont(TsRecord: TTsRecord); overload;
    procedure AdjustFont(ACol, ARow: Longint); overload;
    procedure SaveFont;
    procedure RestoreFont;
    function GetOwnsTimeseries: Boolean;
    procedure SetOwnsTimeseries(OwnsTimeseries: Boolean);
    {** Returns the width of the text of a cell.
        TTimeseriesGrid calls GetCellWidth internally to determine the width
        of text in a cell. This is most useful when autofitting column widths.
    }
    function GetCellTextWidth(ACol, ARow: Longint): Integer;
    procedure SetBgColorForStatistics(Value: TColor);
    procedure SetHighlightColor(Value: TColor);
    procedure SetFilteredColor(Value: TColor);
    procedure SetHighlightMode(Value: THighlightMode);
    {** Calculates the statistics for a row or column.
        GetRowColStatistics is used internally to determine the statistics of
        a specified row or column. Only one of ARow or ACol is used, the other
        one being -1.
    }
    function GetRowColStatistics(ARow, ACol: Longint): TRowColStatistics;
    function GetHydrologicalYear: Boolean;
    procedure SetHydrologicalYear(Value: Boolean);
    function GetSelectionRange: TIndexRange;
    procedure SetSelectionRange(Value: TIndexRange);
    { Saves ColWidths temporarily to FSavedColWidths. }
    procedure SaveColWidths;
    { Restores ColWidths from FSavedColWidths. }
    procedure RestoreColWidths;
  protected
    {** Grid controls call ShowEditor when the grid enters edit mode.
        ShowEditor displays the window of the inplace editor over the
        currently selected cell. If the inplace editor does not yet exist,
        ShowEditor creates it.
    }
    procedure ShowEditor;
    {** Grid controls call HideEditor when the grid goes out of edit mode.
        HideEditor hides the window of the inplace editor.
    }
    procedure HideEditor;
    {** Indicates whether the inplace edit control can be created to allow editing.
	Grid controls call CanEditShow internally before creating the editor for
	a cell. CanEditShow is called after the grid has determined that the
        field can be edited by calling CanEditModify.<p>
        TTimeseriesGrid overrides TCustomGrid.CanEditShow to make specialized
        checking of whether a cell can be edited or not. Most Options,
        including goEditing, are ignored. A cell is allowed to be edited if
        the grid has the focus, and if it is in dfSimple DisplayFormat.
        @SeeAlso <See Property=DisplayFormat>
        @SeeAlso <Jump File=Del5Vcl.hlp K="TCustomGrid,CanEditShow" Text=TCustomGrid.CanEditShow>
        @SeeAlso <Jump File=Del5Vcl.hlp K="TCustomGrid,CanEditModify" Text=TCustomGrid.CanEditModify>
    }
    function CanEditShow: Boolean; override;
    {** Generates an OnColumnMoved event.
        TTimeseriesGrid calls ColumnMoved internally after a column in the grid
        has moved (e.g. when dragging and dropping columns when DisplayFormat is
        dfSimple). It overrides the inherited method to generate an
        OnColumnMoved event.
        @SeeAlso <See Property=OnColumnMoved>
    }
    procedure ColumnMoved(FromIndex, ToIndex: Longint); override;
    {** Draws a cell.
        DrawCell is used internally to draw the contents of a cell. It also
        generates an OnDrawCell event.
    }
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect;
      AState: TGridDrawState); override;
    {** Returns the value of the indicated cell formatted for editing.
        For more information on this method, see the method it overrides,
        TCustomGrid.GetEditText.
        @SeeAlso <Jump File=Del5Vcl.hlp K="TCustomGrid,GetEditText" Text=TCustomGrid.GetEditText>
    }
    function GetEditText(ACol, ARow: Longint): string; override;
    {** Provides the interface for a method that updates the text associated with a cell.
        For more information on this method, see the method it overrides,
        TCustomGrid.SetEditText.
        @SeeAlso <Jump File=Del5Vcl.hlp K="TCustomGrid,SetEditText" Text=TCustomGrid.SetEditText>
    }
    procedure SetEditText(ACol, ARow: Longint; const Value: string); override;
    {** Determines whether a particular cell in the grid can be selected.
        This method overrides the inherited one and limits the way cells can
        be selected. For example, if a filter is active, changing the active
        time series is not allowed. SelectCell also generates an OnSelectCell
        event.
        @SeeAlso <Jump File=Del5Vcl.hlp K="TCustomGrid,SelectCell" Text=TCustomGrid.SelectCell>
    }
    function SelectCell(ACol, ARow: Longint): Boolean; override;
    {** Provides special processing when the user presses a mouse button.
        Normally MouseDown only calls the inherited method. However, if a filter
        is active and the mouse is clicked in a time series other than the
        active one, nothing happens.
        @SeeAlso <Jump File=Del5Vcl.hlp K="TCustomGrid,MouseDown" Text=TCustomGrid.MouseDown>
    }
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    {** Used internally to move dragged columns.
        EndColumnDrag overrides the inherited method in order to handle column
        dragging. When column dragging occurs, EndColumnDrag is called, which
        in turn calls Move to move the display order of the time series.
        @SeeAlso <See Method=Move>
    }
    function EndColumnDrag(var Origin, Destination: Integer; const MousePt:
      TPoint): Boolean; override;
    {** Generates an OnTopLeftChanged event.
        TTimeseriesGrid calls TopLeftChanged internally whenever the LeftCol
        or TopRow property changes. After calling the inherited method to update
        the position of the inplace editor, TopLeftChanged generates an
        OnTopLeftChanged event.
    }
    procedure TopLeftChanged; override;
    {** Provides special processing when the user presses a key.
        After generating an OnKeyPress event, KeyPress:
        <ul>
          <li>If EditorMode is False and a number or letter key or space bar
          or minus symbol is pressed, starts inplace editing.
          <li>If EditorMode is True and Enter is pressed, finishes editing and
          moves the selection downward.
          <li>If EditorMode is True and Esc is pressed, cancels changes.
        </ul>
    }
    procedure KeyPress(var Key: Char); override;
    {** Provides special processing when the user presses a key.
        After calling the inherited procedure to generate an OnKeyDown event,
        screen out problematic key combinations, and processing navigational
        key combinations and F2, KeyDown cancels changes made with the inplace
        editor when the user presses Esc.
    }
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    {** Provides special processing when the user moves the mouse.
        After calling the inherited method, MouseMove alters the Hint property
        if the mouse goes over the time series heading, and sets it to the
        time series' Comment.
        @SeeAlso <See Property=TTimeseries.Comment>
    }
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    {** Specifies the active time series.
        When the Data array contains more than one time series, then one of
	those is the active one. If the DisplayFormat is dfTable, only the
	active time series is displayed on the grid. If the DisplayFormat is
	dfSimple, all time series are displayed; in that case, the active time
	series is the one to which the selected cell belongs; if the user
	selects a cell belonging to another time series, ActiveIndex will change
	accordingly.<p>
	Use ActiveIndex to determine the index of the active time series. Set
	ActiveIndex to select the displayed time series when in dfTable display
	format, or to move the active cell to another time series when in
	dfSimple.
	@SeeAlso <See Method=ActiveTimeseries>
	@SeeAlso <See Property=DisplayFormat>
	@SeeAlso <See Property=Data>
    }
    property ActiveIndex: Integer read GetActiveIndex write SetActiveIndex;
    {** Specifies the date and time that corresponds to the selected cell.
        Read ActiveDate to determine the date and time that corresponds to
        the selected cell.<p>
	Set ActiveDate to change the selected cell. If DisplayFormat is
	dfSimple, the selected cell remains on the same time series. If
	DisplayFormat is dfTable, ActiveDate may also change BaseDate.
        <p>
	ActiveDate is automatically modified if the user changes the selected
	cell.<p>
        If the selected cell is not a cell containing valid time series data
        (for example, it contains statistics), ActiveDate returns idaEmpty.
	@SeeAlso <See Property=DisplayFormat>
        @SeeAlso <See Property=BaseDate>
    }
    property ActiveDate: TDateTime read GetActiveDate write SetActiveDate;
    {** The number of time series in the Data array.
        Use Count to determine how many time series are displayed on the grid.
        @SeeAlso <See Property=Data>
    }
    property Count: Integer read GetCount;
    {** Specifies the date displayed for a table.
        If DisplayFormat is dfTable, BaseDate specifies the month displayed
        if the active time series is hourly, or the year displayed if it is
        daily. If DisplayFormat is dfSimple, or the active time series is
        annual, reading or setting BaseDate raises an exception.<p>
        Only the month and year are significant. The day is ignored and set
        to 1. For hourly time series, the month is left as it is. For daily
        time series, the year (normal or hydrological) is defined to be that
        which contains the specified date, which is changed accordingly to
        1/10/xxxx or 1/1/xxxx.
        @SeeAlso <See Property=ActiveDate>
        @SeeAlso <See Property=DisplayFormat>
        @SeeAlso <See Property=HydrologicalYear>
    }
    property BaseDate: TDateTime read GetBaseDate write SetBaseDate;
    {** Specifies the lower bound coefficient when viewing statistics.
        LowerBoundCoefficient is used for the idfTable display formats, when
	StatisticsVisible is true. Values of the time series less than the mean
	minus standard deviation times LowerBoundCoefficient are considered
	suspicious.
        @SeeAlso <See Property=DisplayFormat>
        @SeeAlso <See Property=StatisticsVisible>
        @SeeAlso <See Property=UpperBoundCoefficient>
    }
    property LowerBoundCoefficient: Single read FLowerBoundCoefficient
      write SetLowerBoundCoefficient;
    {** Specifies whether statistics are displayed.
        In idfTable display formats, rows and columns with statistics may be
	displayed. StatisticsVisible specifies whether statistics will be
	displayed or not.
	@SeeAlso <See Property=DisplayFormat>
    }
    property StatisticsVisible: Boolean read FStatisticsVisible
      write SetStatisticsVisible;
    {** Specifies the upper bound coefficient when viewing statistics.
        UpperBoundCoefficient is used for the idfTable display formats, when
	StatisticsVisible is true. Values of the time series greater than the
	mean plus standard deviation times UpperBoundCoefficient are considered
	suspicious.
        @SeeAlso <See Property=DisplayFormat>
        @SeeAlso <See Property=StatisticsVisible>
        @SeeAlso <See Property=LowerBoundCoefficient>
    }
    property UpperBoundCoefficient: Single read FUpperBoundCoefficient
      write SetUpperBoundCoefficient;
    {** Contains the time series displayed on the grid.
        Use Data to obtain a pointer to a specific time series object displayed
	on the grid. The Index parameter is zero-based. In idfSimple display
	format, the time series are displayed in the order in which the are
	listed in Data, left to right. Set Data to change the time series
	displayed at a specific location.<p>
	Use Data with the Count property to iterate through all time series
	displayed.<p>
	In idfTable formats, only the time series specified by ActiveIndex is
	displayed.
	@SeeAlso <See Property=Count>
	@SeeAlso <See Property=ActiveIndex>
    }
    property Data[Index: Integer]: TTimeseries read Get
      write Put;
    {** Specifies whether the time series is filtered.
        Set Filtered to True to display only those records of the time series
        that pass the filter defined by FilterCondition and FilterValue.<p>
        Filtering only works for idfSimple display format. For other display
        formats, filtering options are ignored. If more than one time series
        are displayed, only the active time series is taken into account for
        filtering.
        @SeeAlso <See Property=FilterCondition>
        @SeeAlso <See Property=FilterValue>
        @SeeAlso <See Property=DisplayFormat>
        @SeeAlso <See Property=ActiveIndex>
        @SeeAlso <See Method=ActiveTimeseries>
    }
    property Filtered: Boolean read FFiltered write SetFiltered;
    {** Specifies the filter condition.
        FilterCondition specifies the filter condition for filtering.
        FilterCondition is read only. To set it, use SetFilter.
        @SeeAlso <See Method=SetFilter>
        @SeeAlso <See Property=Filtered>
        @SeeAlso <See Property=FilterValue>
        @SeeAlso <See Class=TFilterDialog>
    }
    property FilterCondition: TFilterCondition read FFilterCondition;
    {** Specifies the filter value.
        FilterValue specifies the value against which each record is tested
        by means of FilterCondition. FilterValue is read only; to set it, use
        SetFilter.
        @SeeAlso <See Method=SetFilter>
        @SeeAlso <See Property=Filtered>
        @SeeAlso <See Property=FilterCondition>
        @SeeAlso <See Class=TFilterDialog>
    }
    property FilterValue: Variant read FFilterValue;
    {** Returns or sets the selection range for the active time series.
        Read SelectionRange to determine the start and end index of the selected
        part of the active time series. The display format must be dfSimple and
        the selection must cover only the active time series, or an exception is
        raised.<p>
        Set SelectionRange to select the specified range of the active time
        series when in dfSimple format.
        @SeeAlso <See Property=DisplayFormat>
        @SeeAlso <See Property=ActiveTimeseries>
    }
    property SelectionRange: TIndexRange read GetSelectionRange
      write SetSelectionRange;
    property GridHeight;
    property GridWidth;
    {** Specifies the row count.
        Do not set row count. This property is provided for reading only. It is
        not possible to enforce read-only, because it is an inherited property
        declared as writeable.
    }
    property RowCount;
    {** Specifies the col count.
        Do not set col count. This property is provided for reading only. It is
        not possible to enforce read-only, because it is an inherited property
        declared as writeable.
    }
    property ColCount;
    property Selection;
    property TabStops;
    property TopRow;
    {** Returns the active time series.
        ActiveTimeseries returns Data[ActiveIndex].
	@SeeAlso <See Property=Data>
	@SeeAlso <See Property=ActiveIndex>
        @SeeAlso <See Method=GetTimeseries>
    }
    function ActiveTimeseries: TTimeseries;
    {** Creates an instance of TTimeseriesGrid.
	Call Create to create an instance of TTimeseriesGrid at runtime. For
	grids placed on forms at design time, Create is called automatically.<p>
	After calling the inherited constructor, Create creates the helper
	objects used by TTimeseriesGrid to manage the time series and their
	associated objects.
    }
    constructor Create(AOwner: TComponent); override;
    {** Destroys an instance of TTimeseriesGrid.
        Do not call Destroy directly in an application. Instead, call Free. Free
	verifies that the grid is not nil, and only then calls Destroy.<p>
	Destroy frees the helper objects used to manage the time series and
	their associated objects. If OwnsTimeseries is True, it also frees the
        time series objects.
        @SeeAlso <See Property=OwnsTimeseries>
    }
    destructor Destroy; override;
    {** Reformats the grid visually based on the values of its properties.
        Normally you never have to call Reformat. It is mostly used internally.
	If you add or remove rows or time series or change the grid's
	properties, Reformat will be called automatically. If you really need to
	explicitly reformat the grid, do not use Reformat; use FullInvalidate
	instead, which will mark it as invalid. It will be reformatted when it
	receives the repaint message from Windows. There is no performance
	penalty if you call FullInvalidate multiple times.
        @SeeAlso <See Method=FullInvalidate>
    }
    procedure Reformat;
    {** Inserts a new time series to the end of the list of displayed timeseries.
        Call add to insert a new time series object at the end of the Data
	array. Add returns the index of the new time series,where the first
	series in the list has an index of 0.
	@SeeAlso <See Property=Count>
	@SeeAlso <See Method=Delete>
	@SeeAlso <See Method=Insert>
	@SeeAlso <See Property=Data>
	@SeeAlso <See Method=Remove>
    }
    function Add(Item: TTimeseries): Integer;
    {** Deletes all displayed time series.
	Call Clear to empty the Data array and set the Count to 0. If
        OwnsTimeseries is True, Clear also frees the time series objects.
	@SeeAlso <See Property=Count>
	@SeeAlso <See Method=Delete>
	@SeeAlso <See Property=Data>
	@SeeAlso <See Method=Remove>
        @SeeAlso <See Property=OwnsTimeseries>
    }
    procedure Clear;
    {** Removes a time series at the position given by the Index parameter.
        Call Delete to remove the series at a specific position of the Data
	array. The index is zero-based, so the first series has an Index value
	of 0. Calling Delete moves up all series in the Data array that follow
	the deleted series.<p>
        If OwnsTimeseries is True, Delete also frees the time series object.
	@SeeAlso <See Method=Add>
	@SeeAlso <See Method=Clear>
	@SeeAlso <See Property=Count>
	@SeeAlso <See Method=Insert>
	@SeeAlso <See Property=Data>
	@SeeAlso <See Method=Remove>
        @SeeAlso <See Property=OwnsTimeseries>
    }
    procedure Delete(Index: Integer);
    {** Removes a specified time series from the list without freeing the time series object.
        Call Extract to remove a time series from the Data list without freeing
        the time series itself.
        @SeeAlso <See Method=Remove>
        @SeeAlso <See Property=OwnsTimeseries>
    }
    function Extract(Timeseries: TTimeseries): TTimeseries;
    {** Returns the index of the time series displayed in a specific column.
        GetIndex returns the index of the time series the data of which are
        displayed in the specified column. If DisplayFormat is not idfSimple,
        GetIndex returns ActiveIndex regardless of column.
        @SeeAlso <See Property=DisplayFormat>
        @SeeAlso <See Method=ActiveIndex>
    }
    function GetIndex(ACol: Longint): Integer;
    {** Returns the index of the first entry in the displayed time series list with a specified value.
	Call IndexOf to get the index for a time series object in the Data
	array.  Specify the object as the Timeseries parameter.<p>
	The first series in the array has index 0. If a series appears more than
	once in the array, IndexOf returns the index of the first appearance.
	@SeeAlso <See Property=Data>
	@SeeAlso <See Method=Remove>
    }
    function IndexOf(Timeseries: TTimeseries): Integer;
    {** Returns Data[0].
        Call First to get the first time series in the Data array.
    }
    function First: TTimeseries;
    {** Adds a time series to the displayed time series at the position specified by Index.
	Call Insert to add Timeseries to the middle of the Data array. The Index
	parameter is a zero-based index, so the first position in the array has
	an index of 0. Insert adds the time series at the indicated position,
	shifting the time series that previously occupied that position, and all
	subsequent time series, up.
	@SeeAlso <See Method=Add>
	@SeeAlso <See Property=Count>
	@SeeAlso <See Method=Delete>
	@SeeAlso <See Property=Data>
	@SeeAlso <See Method=Remove>
    }
    procedure Insert(Index: Integer; Timeseries: TTimeseries);
    {** Changes the display order of a time series.
	Call Move to move the time series at position CurIndex so that it
	occupies the position NewIndex. CurIndex and NewIndex are zero-based
	indexes into the Data array.<p>
        You probably don't need to use Move, as TTimeseriesGrid automatically
        handles column dragging when the DisplayFormat is dfSimple and calls
        Move internally.
	@SeeAlso <See Method=IndexOf>
	@SeeAlso <See Property=Data>
        @SeeAlso <See Property=DisplayFormat>
    }
    procedure Move(CurIndex, NewIndex: Integer);
    {** Returns Data[Count-1].
        Call Last to retrieve the last time series in the Data array.
        @SeeAlso <See Method=First>
    }
    function Last: TTimeseries;
    {** Removes the first reference to the Timeseries parameter from the displayed time series.
	Call Remove to remove a specific time series from the Data array when
	its index is unknown. The value returned is the index of the time series
	in the Data array before it was removed. After a time series is removed,
	all the series that follow it are moved up in index position.<p>
	If the Data array contains more than one copy of the time series, only
	the first copy is deleted.<p>
	If OwnsTimeseries is True, Remove also frees the time series object.
	@SeeAlso <See Method=Add>
	@SeeAlso <See Property=Count>
	@SeeAlso <See Method=Delete>
	@SeeAlso <See Method=Insert>
	@SeeAlso <See Property=Data>
        @SeeAlso <See Property=OwnsTimeseries>
    }
    function Remove(Timeseries: TTimeseries): Integer;
    {** Returns true if ADate is displayed on time series grid.
        It uses the private array FDates to check if ADate is
        equal to the DateDisplayed property of an FDates item.
        IsDateDisplayed may be used to cope with filtrered data
        displayed on time series grid, e.g. to delete only the
        filtered data displayed. If Filtered is set to False,
        InDateDisplayed returns True in every case
        @Author Stefanos
    }
    function IsDateDisplayed(ADate: TDateTime): Boolean;
    {** Returns a TMethod representing the Invalidate method.
        This is only used internally.
        @SeeAlso <See Property=TTimeseries.Invalidators>
    }
    function InvalidateMethod: TMethod;
    {** Returns a TMethod representing the FullInvalidate method.
        This is only used internally.
        @SeeAlso <See Property=TTimeseries.FullInvalidators>
    }
    function FullInvalidateMethod: TMethod;
    {** Sets the filtering options.
        SetFilter sets the FilterCondition and FilterValue properties. Set
        the Filtered property to true after using SetFilter to actually
        activate filtering.
        @SeeAlso <See Property=FilterCondition>
        @SeeAlso <See Property=FilterValue>
        @SeeAlso <See Property=Filtered>
    }
    procedure SetFilter(FilterCondition: TFilterCondition;
      FilterValue: Variant);
    {** Copies the specified range to the Clipboard in CF_TEXT format.
        Use CopyToClipboard to replace the contents of the Clipboard with the
        specified range. If the range consists of more than one cells, they
        are separated with tab and CR characters (Excel works that way).<P>
        To copy the selection to the clipboard, use
        CopyToClipboard(Selection).<P>
        @SeeAlso <See Method=CopyToClipboardWithDates>
        @SeeAlso <See Method=PasteFromClipboard>
        @SeeAlso <Jump File=Del5Vcl.hlp K="TCustomGrid,Selection" Text=Selection>
        @SeeAlso <Jump File=Del5Vcl.hlp K="TGridRect type," Text="TGridRect type">
    }
    procedure CopyToClipboard(ARect: TGridRect);
    {** Copies the specified range and corresponding dates and flags to the Clipboard in CF_TEXT format.
        If the DisplayFormat is dfSimple, CopyToClipboardWithDates replaces the
        contents of the Clipboard with the specified range, which must not
        include the dates column; in addition to the specified range,
        CopyToClipboardWithDates also copies the dates that correspond to the
	range. If DisplayFormat is dfTable, or if the specified range
        contains cells from the dates column, CopyToClipboardWithDates raises an
	exception.<P>
        The main reason for the use of CopyToClipboardWithDates is to copy the
        selection with dates, as in CopyToClipboardWithDates(Selection).
        CopyToClipboard cannot do this since the dates are on a fixed column
        which is not possible to select.<P>
        When ARect spans only one time series, then any cells with diagonal
        lines (i.e. corresponding to nonexistent records) are not copied. In
        addition, values and flags of the selected rows are copied, regardless
        whether the flags are visible or whether they are selected. If ARect
        spans more than one time series, all selected cells, and no more
        (except for the dates), are copied.<p>
        @SeeAlso <See Method=CopyToClipboard>
        @SeeAlso <See Method=PasteFromClipboard>
        @SeeAlso <Jump File=Del5Vcl.hlp K="TCustomGrid,Selection" Text=Selection>
        @SeeAlso <Jump File=Del5Vcl.hlp K="TGridRect type," Text="TGridRect type">
    }
    procedure CopyToClipboardWithDates(ARect: TGridRect);
    {** Pastes values from the clipboard.
        PasteFromClipboard assumes that the Clipboard contains text in rows
        and columns separated by tab and CR characters, and pastes that text
        into the specified range of the grid. Usually you will paste text in
        the selection via PasteFromClipboard(Selection). For this reason, for
        the rest of this text we will refer to ARect as "selection", although
        it could be different. Any raised exceptions also use terms like
        "selection" or "selected cells" in the messages, so that the messages
        have meaning for the end user in the vast majority of cases in which
        ARect will be the Selection.<P>
        The clipboard text must contain the same number of columns in all rows,
        or an exception is raised.<P>
        First we will deal with the case where the clipboard text's leftmost
        column does not contain dates. In that case, if the selection consists
        of one cell only, the clipboard is pasted in a cell range such that
        the selected cell is the top-left cell of that range. If the selection
        consists of more than one cells, then the shape of the selection must
        be the same as the shape of the clipboard text.<P>
        If the clipboard text's leftmost column consists of dates, additional
	restrictions apply. First, the DisplayFormat must be dfSimple. Second,
	only one time series may be pasted, i.e. the clipboard text must have
	either a date column only, or a date column plus a value column, or a
	date column plus a value column plus a flag column. Third, the selection
	is irrelevant (except that the selected cell specifies the active
	timeseries), and to avoid misinterpretation by the end user it must be
	one cell only in size. If the time series contains records with all the
	pasted dates, these dates must be consecutive. If for some of the pasted
	dates there does not already exist such a record in the time series,
	records are created and inserted; after the insertion, all pasted
	records (whether they are modified or entirely new) must be
	consecutive.<p>
        For time steps less than monthly, pasted dates must be in the format
        yyyy-mm-dd hh:nn; for monthly time steps, they can either be
        in the format yyyy-mm-01 00:00, or in yyyy-mm; for annual time series
        with HydrologicalYear set to False, they can either be in
        yyyy-01-01 00:00, or in yyyy; for annual time series with
        HydrologicalYear set to True, they can either be in yyyy-10-01 00:00,
        or in yyyy-zz, where zz=yyyy+1. See TTsRecord.Date for more
        information.
	Whenever one of the constraints is not met, PasteFromClipboard raises an
	exception. An exception is also raised in other obvious error cases, for
	example when a pasted value cell does not contain a number or when a
	pasted flag cell does not contain a valid flag specification.
	PasteFromClipboard checks before actually pasting, so normally when an
	exception occurs no text will have been pasted.<P>
	PasteFromClipboard returns the rectangle actually affected, which may be
	different from ARect (for example, when ARect is one cell only and the
	pasted text is more than one cells). Normally you will want to select
	the affected rectangle after pasting and thus write
        <PRE>Selection := PasteFromClipboard(Selection);</PRE>
        @SeeAlso <See Method=CopyToClipboard>
        @SeeAlso <See Method=CopyToClipboardWithDates>
        @SeeAlso <See Property=TTsRecord.Date>
        @SeeAlso <Jump File=Del5Vcl.hlp K="TCustomGrid,Selection" Text=Selection>
        @SeeAlso <Jump File=Del5Vcl.hlp K="TGridRect type," Text="TGridRect type">
    }
    function PasteFromClipboard(ARect: TGridRect): TGridRect;
    {** Auto-fits the column widths.
        Auto-fits the specified column.
    }
    {** A procedure to paste some tabular data to a monthly timeseries.
        Tabular data must be 12 colums wide representing the month values
        of a year (row). Each row represents a year (common or hydrological).
        User must specify if to use hydrological year and the year itself,
        respectively. An attempt to paste data to existing records will
        prompt user to decide if existing data will be ovewritten or not.
        If a timeseries considered with non monthly timestep, an expection
        is raised. If try to paste data not arranged to 12 colums, an exception
        is raised. If a data value is non arithmetic like spaces, a null record
        is created.
        @Author Stefanos
        @SeeAlso <See Method=PasteDailyTable>
    }
    procedure PasteMonthlyTable;

    {** A procedure to paste some tabular data to a daily timeseries.
        Tabular data must be 12 columns wide representing the months
        and 31 rows tall representing the days respectively. User must
        specify the Year, starting month and a (positive) time offset in
        HH:MM format (hours/ minutes). If a timeseries considere with non
        monthly timestep then a exception is raised. Simularly if try
        to paste data not arrange in 12x31 columnsxrows, an exception
        is raised. If a data value is non arithmeitc like spaces, a null
        record is created.
        @Author Stefanos
        @SeeAlso <See Method=PasteMonthlyTable>
    }
    procedure PasteDailyTable;

    procedure AutoFit(ACol: Longint); overload;
    {** Auto-fits all column widths.
        Auto-fits all column widths.
    }
    procedure AutoFit; overload;
    {** Returns the text associated with a cell.
        TTimeseriesGrid calls GetCellText internally to determine the text
        displayed on a specified cell. It also returns the related time series
        record in TsRecord, or nil if there is no related TsRecord.
    }
    function GetCellText(ACol, ARow: Longint; var TsRecord: TTsRecord;
      DateDisplayFormat: TDateDisplayFormat=ddfComplex): string;
    property VisibleColCount;
    property VisibleRowCount;
    property EditorMode;
    {** Prepare buffer adds a new buffer entry to the buffers list.
    }
    procedure PrepareBuffer(Index: Integer; FirstRecordDate,
      LastRecordDate: TDateTime; Caption: string; ID: Integer);
    {** Increase the Undo pointer by one.
    }
    procedure FinalizeBuffer(Index: Integer);
    {** Revert last undo entry by discarding last added entry.
    }
    procedure RevertBuffer(Index: Integer);
    {** Undo the last user action.
    }
    procedure Undo(Index: Integer); 
    {** Redo last undone action.
    }
    procedure Redo(Index: Integer);
    {** Timeseries (Data[Index]) can undo.
    }
    function CanUndo(Index: Integer): Boolean;
    {** Timeseries (Data[Index]) can redo.
    }
    function CanRedo(Index: Integer): Boolean;
    {** Read the current undo caption.
    }
    function UndoCaption(Index: Integer): string;
    {** Read the current redo caption.
    }
    function RedoCaption(Index: Integer): string;
    {** Read the Undo ID;
    }
    function UndoID(Index: Integer): Integer;
    {** Read the Redo ID;
    }
    function RedoID(Index: Integer): Integer;
    {** Increase Undo ID Pointer by one.
    }
    procedure IncUndoIDPointer;
    {** Decode date from selected cells.
    }
    function SelectedDate: TDateTime; overload;
    {** Decode date from selected cells.
    }
    function SelectedDate(ARow: Integer): TDateTime; overload;
    {** Reset undo buffer, e.g. when save changes.
    }
    procedure ResetBuffer(Index: Integer);
  published
    {** Specifies the display format for the time series.
        If DisplayFormat has the value dfSimple, then all time series contained
	in the Data array are displayed. The left column is fixed and contains
	the date, whereas each time series occupies one column (if FlagsVisible
	is False) or two columns (if FlagsVisible is True). The top line is
	fixed and contains the time series titles. Time
	series entries with identical dates are displayed on the same row.
	Entries with different dates are displayed on separate rows, and empty
	cells are displayed for time series which do not contain a value in that
	date.<p>
	If DisplayFormat has the value dfTable, only the time series specified
	by ActiveIndex is displayed; this time series must be hourly, daily, or
	monthly. The time series is displayed as a table. For hourly time
	series, a single month is displayed, with the day of month on the left
	and the hour of day on top; for daily time series, a single year is
	displayed, with the months on top and the days of month on the left; for
	monthly time series, the entire time series is displayed, with the year
	on the left and the months on top.  An additional column on the right
	and row at the bottom display the sum or the mean of each row or column.
	The time series' VariableType property specifies whether sum or mean
	will be displayed; if the time series is cumulative, the sum is
	displayed if this is reasonable; otherwise the mean is displayed. For
	example, knowning the sum of a month is reasonable; however, there's not
	much meaning in the sum of the 1st day of all months, thus in that case
	the mean is displayed.<p>
	If StatisticsVisible is True, then additional rows and columns exist
	with number of values, number of missing values, standard deviation,
	variance co-efficient, maximum and minimum values, upper and lower
	bound, and number of extremely high and low values. The upper and lower
	bounds are equal to the mean value plus or minus the standard deviation
	times UpperBoundCoefficient and LowerBoundCoefficient. The number of
	extremely high or low values is the number of values that exceed the
	upper or lower bound. Values out of limits are marked with a star. A
	column or row with the mean value is also displayed even for cumulative
	variables.<p>
	The month or year displayed for hourly and daily time series is
	specified by ActiveDate.<p>
        DisplayFormat automatically reverts to dfSimple when the first time
        series is added.
	@SeeAlso <See Property=Data>
	@SeeAlso <See Property=FlagsVisible>
	@SeeAlso <See Property=ActiveIndex>
	@SeeAlso <See Method=ActiveTimeseries>
	@SeeAlso <See Property=ActiveDate>
	@SeeAlso <See Property=TTimeseries.VariableType>
        @SeeAlso <See Property=TTimeseries.Title>
	@SeeAlso <See Property=StatisticsVisible>
	@SeeAlso <See Property=UpperBoundCoefficient>
	@SeeAlso <See Property=LowerBoundCoefficient>
        @SeeAlso <See Property=HydrologicalYear>
      }
    property DisplayFormat: TDisplayFormat read GetDisplayFormat
      write SetDisplayFormat;
    {** Specifies whether flags are displayed.
	In the dfSimple display format, only the value for each time series can
	be displayed, or a column for the values plus an additional column for
	the flags. FlagsVisible specifies whether the flags column will be
	visible.
	@SeeAlso <See Property=DisplayFormat>
    }
    property FlagsVisible: Boolean read FFlagsVisible write SetFlagsVisible;
    {** Specifies the font style for modified values.
        In the dfSimple display format, the grid displays differently time
	series values with different MStatus. FontForModified, FontForUnmodified
	and FontForNew specify the font style used in records with
	MStatus equal to imsModified, imsUnmodified, and imsNew respectively.
	Only the Style property has effect; the other properties
	are taken from the Font property, which is inherited by TControl.<p>
	The default Style is equal to [], [fsItalic] and [fsBold] for
	FontForUnmodified, FontForModified, and FontForNew respectively.
	@SeeAlso <See Property=DisplayFormat>
	@SeeAlso <See Property=TTsRecord.MStatus>
    }
    property FontForModified: TTimeseriesGridFont read FFontForModified;
    {** Specifies the font style for modified values.
        In the dfSimple display format, the grid displays differently time
	series values with different MStatus. FontForModified, FontForUnmodified
	and FontForNew specify the font style used in records with
	MStatus equal to imsModified, imsUnmodified, and imsNew respectively.
	Only the Style property has effect; the other properties
	are taken from the Font property, which is inherited by TControl.<p>
	The default Style is equal to [], [fsItalic] and [fsBold] for
	FontForUnmodified, FontForModified, and FontForNew respectively.
	@SeeAlso <See Property=DisplayFormat>
	@SeeAlso <See Property=TTsRecord.MStatus>
    }
    property FontForNew: TTimeseriesGridFont read FFontForNew;
    {** Specifies the font style for modified values.
        In the dfSimple display format, the grid displays differently time
	series values with different MStatus. FontForModified, FontForUnmodified
	and FontForNew specify the font style used in records with
	MStatus equal to imsModified, imsUnmodified, and imsNew respectively.
	Only the Style property has effect; the other properties
	are taken from the Font property, which is inherited by TControl.<p>
	The default Style is equal to [], [fsItalic] and [fsBold] for
	FontForUnmodified, FontForModified, and FontForNew respectively.
	@SeeAlso <See Property=DisplayFormat>
	@SeeAlso <See Property=TTsRecord.MStatus>
    }
    property FontForUnmodified: TTimeseriesGridFont read FFontForUnmodified;
    {** Specifies the background color used for statistics in dfTable format.
        When viewing data in dfTable display format, there is one or more
        columns on the right and one or more columns at the bottom which
        display the mean or total, and other statistics. BgColorForStatistics
        specifies the background color for these columns and rows.
        @SeeAlso <See Property=DisplayFormat>
        @SeeAlso <See Property=StatisticsVisible>
        @SeeAlso <Jump File=Del5Vcl.hlp K="TColor type" Text="TColor type">
    }
    property BgColorForStatistics: TColor read FBgColorForStatistics
      write SetBgColorForStatistics;
    {** Specifies the background color used for highlighted cells.
        @SeeAlso <See Property=HighlightMode>
        @SeeAlso <Jump File=Del5Vcl.hlp K="TColor type" Text="TColor type">
    }
    property HighlightColor: TColor read FHighlightColor
      write SetHighlightColor;
    {** Specifies the background color used for cells that are filtered.}
    property FilteredColor: TColor read FFilteredColor
      write SetFilteredColor;
    {** Specifies how to determine which cells will be highlighted.
        Set HighlightMode to one of the following values:
        <ul>
          <li>hlNone: No cells are highlighted.
          <li>hlFlag: Cells with any flags set are highlighted.
          <li>hlMax, hlMin (only for DisplayFormat=dfTable): Maximum or minimum
              value of month or year is
              highlighted. If the maximum/minimum value occurs more than once,
              more than one cells are highlighted.
          <li>hlMaxOrMin (only for DisplayFormat=dfTable and hourly or daily
              time step): Maximum and minimum value of month or year is
              highlighted. More than two cells may be highlighted if the maximum
              or minimum occurs more than once.
          <li>hlLarge, hlSmall, hlLargeAndSmall: (only for DisplayFormat=dfTable
              with statistics visible): Values exceeding the upper and/or lower
              bound (rowwise or columnwise) are highlighted.
          <li>hlNull: Null values are highlighted. For DisplayFormat=dfTable,
              entirely missing records are also highlighted.
        </ul>
        When the value of HighlightMode is invalid for the DisplayFormat or
        time step, no cells are highlighted.
        @SeeAlso <See Property=HighlightColor>
        @SeeAlso <See Property=DisplayFormat>
    }
    property HighlightMode: THighlightMode read FHighlightMode
      write SetHighlightMode;
    {** Specifies whether the hydrological year will be used in tables.
        When viewing daily and monthly time series as a table, HydrologicalYear
        specifies whether the first column will be January or October.
        @SeeAlso <See Property=DisplayFormat>
    }
    property HydrologicalYear: Boolean read GetHydrologicalYear
      write SetHydrologicalYear;
    {** Allows TTimeseriesGrid to free the time series objects when they are deleted from Data or when the grid is destroyed.
        OwnsTimeseries allows TTimeseriesGrid to control the memory of the
        time series objects held by the Data property. If OwnsTimeseries is
        True,
        <ul>
          <li>calling Delete or Remove frees the deleted time series in addition
          to removing it from the list.
          <li>calling Clear frees all the time series objects in addition to
          emptying the list.
          <li>calling the destructor frees all the time series in the list in
          addition to destroying the TTimeseriesGrid.
          <li>assigning a new value to an index in Data frees the time series
          object that previously occupied that position in the list.
        </ul>
        Even if OwnsTimeseries is True, the Extract method can be used to remove
        time series from the list without freeing them.
    }
    property OwnsTimeseries: Boolean read GetOwnsTimeseries
      write SetOwnsTimeseries;
    {** The Undo ID Pointer to group concurent actions to several time series of
        the time series grid.
    }
    property UndoIDPointer: Integer read FUndoIDPointer write FUndoIDPointer;
    property Align;
    property Anchors;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Constraints;
    property DefaultDrawing;
    property Enabled;
    property FixedColor;
    property GridLineWidth;
    property ParentColor;
    property PopupMenu;
    {** Use read only with care, it is used mainly internaly..
    }
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property ScrollBars;
    {** Determines whether the grid displays a Help Hint when the mouse pointer rests momentarily on a time series heading.
        The Help Hint is the value of the Comment property of the time
        series shown in the column where the mouse pointer rests.
    }
    property ShowHint;
    property TabOrder;
    property Visible;
    property OnClick;
    {** Occurs immediately after the position of a column changes.
        Use OnColumnMoved to perform special processing when the DisplayFormat
        is dfSimple and the position of a grid column changes (that is, the
        time series are reordered). The FromIndex parameter is the old index of
        the column, and the ToIndex parameter is the new index.
        @SeeAlso <See Property=DisplayFormat>
    }
    property OnColumnMoved: TMovedEvent read FOnColumnMoved write FOnColumnMoved;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    {** Occurs when a cell in the grid is about to be drawn.
        Write an OnDrawCell in the unlikely case you need some processing
        whenever a cell is about to be drawn. Note that unlike other grids,
        TTimeseriesGrid will draw the cell regardless of whether OnDrawCell
        has done any drawing or not, and will probably overwrite whatever
        OnDrawCell did. Thus, OnDrawCell is provided only for completeness,
        in case any side-effects are required, and not for drawing.
    }
    property OnDrawCell: TDrawCellEvent read FOnDrawCell write FOnDrawCell;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    {** Occurs before a cell in the grid is selected.
        Write an OnSelectCell event handler to specify whether any particular
        cell in the grid can be selected. The Col and Row parameters indicate
        the column and row indexes of the cell that is about to be selected.
        Set the CanSelect parameter to False to prevent the cell being selected.
        <p>
        TTimeseriesGrid normally forbids selecting of fixed rows and columns,
        and permits all others.
    }
    property OnSelectCell: TSelectCellEvent read FOnSelectCell write FOnSelectCell;
    property OnStartDock;
    property OnStartDrag;
    {** Occurs immediately after the TopRow property or the LeftCol property changes.
        Use OnTopLeftChanged to perform special processing when the non-fixed
        cells in the grid are scrolled.
    }
    property OnTopLeftChanged: TNotifyEvent read FOnTopLeftChanged write FOnTopLeftChanged;
    {** Invalidates the grid.
        FullInvalidate is like Invalidate, except that it causes a complete
        reformatting of the grid (calculation of number of rows and columns
        from scratch). Invalidate only invalidates it visually, that is, it
        merely causes redrawing of the cells.
        @SeeAlso <Jump File=Del5Vcl.hlp K="TWinControl,Invalidate" Text=Invalidate>
        @SeeAlso <See Method=Reformat>
    }
    procedure FullInvalidate;
    procedure Invalidate; override;
    property HYearOrigin: Integer read FHYearOrigin write FHYearOrigin;
  end;

  { TDatePicker }

  TDatePickerFormat = (idpMonthYear, idpHydrologicalYear, idpCalendarYear);

  TDatePicker = class(TCustomPanel)
  private
    FActiveDate: TDateTime;
    FFormat: TDatePickerFormat;
    FPeriod: TPeriod;
    FCmbBxMonth: TComboBox;
    FCmbBxYear: TComboBox;
    FBtnFirst: TSpeedButton;
    FBtnPrev: TSpeedButton;
    FBtnNext: TSpeedButton;
    FBtnLast: TSpeedButton;
    procedure SetActiveDate(Value: TDateTime);
    procedure SetFormat(Value: TDatePickerFormat);
  public
    property ActiveDate: TDateTime read FActiveDate write SetActiveDate;
    property Format: TDatePickerFormat read FFormat write SetFormat;
    property Period: TPeriod read FPeriod;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { TTimeseriesGridFont }

  TTimeseriesGridFont = class(TFont)
  private
    FOwner: TTimeseriesGrid;
  protected
    function GetColor: TColor;
    procedure SetColor(Value: TColor);
    function GetStyle: TFontStyles;
    procedure SetStyle(Value: TFontStyles);
  public
    constructor Create(AOwner: TTimeseriesGrid);
    procedure Assign(Source: TPersistent); override;
    property Color: TColor read GetColor write SetColor;
    property Style: TFontStyles read GetStyle write SetStyle;
  end;

  {** Displays a filter definition dialog.
      TFilterDialog displays a modal Windows dialog box for defining a filter
      for displaying time series records. The dialog does not appear at runtime
      until it is activated by a call to the Execute method. When the user
      clicks OK, the dialog closes and the filter defined is stored in the
      FilterCondition and FilterValue properties.
      @author A.X.
      @SeeAlso <See Property=TTimeseriesGrid.Filtered>
      @SeeAlso <See Property=FilterCondition>
      @SeeAlso <See Property=FilterValue>
      @SeeAlso <See Method=Execute>
  }
  TFilterDialog = class(TIComponent)
  private
    FFilterCondition: TFilterCondition;
    FFilterValue: Variant;
    FTitle, FPrompt: TCaption;
    FFlagsUsed: string;
  public
    {** Creates and initializes a TFilterDialog instance.
        The Create method generates a TFilterDialog instance, but the new
        dialog does not appear at runtime until the Execute method is called.
        @SeeAlso <See Method=Execute>
    }
    constructor Create(AOwner: TComponent); override;
    {** Displays the filter definition dialog.
        Execute opens the filter definition dialog, returning True when the
        user defines a filter and clicks OK. If the user clicks Cancel,
        Execute returns False.
    }
    function Execute: Boolean;
    {**
    }
    property FlagsUsed: string read FFlagsUsed write FFlagsUsed;
  published
    {** Specifies the filter condition.
        Read FilterCondition after running the Execute method in order to
        determine the filter condition specified by the user on the dialog.
        Normally all you have to do afterwards is assign it to the
        FilterCondition property of a TTimeseriesGrid.<p>
        Set FilterCondition before running the Execute method in order to
        set the default filter condition for the dialog.
        @SeeAlso <See Property=TTimeseriesGrid.FilterCondition>
        @SeeAlso <See Property=FilterValue>
        @SeeAlso <See Method=Execute>
    }
    property FilterCondition: TFilterCondition read FFilterCondition
      write FFilterCondition;
    {** Specifies the filter value.
        Read FilterValue after running the Execute method in order to
        determine the filter value specified by the user on the dialog.
        Normally all you have to do afterwards is assign it to the FilterValue
        property of a TTimeseriesGrid.<p>
        Set FilterValue before running the Execute method in order to set the
        default filter value for the dialog.
        @SeeAlso <See Property=TTimeseriesGrid.FilterValue>
        @SeeAlso <See Property=FilterCondition>
        @SeeAlso <See Method=Execute>
    }
    property FilterValue: Variant read FFilterValue write FFilterValue;
    {** Specifies the text in the dialog's title bar.
        Use Title to specify the text that appears in the dialog's title bar.
        If no value is assigned to Title, the dialog has the title
        "Define filter".
    }
    property Title: TCaption read FTitle write FTitle;
    {** Specifies the prompt displayed in the dialog.
        Use Prompt to specify the text that appears as a prompt in the dialog.
        If no value is assigned to Prompt, the dialog has the prompt
        "Only display records such that:".
    }
    property Prompt: TCaption read FPrompt write FPrompt;
  end;

implementation

{$C+}
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}

uses Clipbrd, variants, istrutils, DateUtils, TypInfo;

{ TTimeStampFormat }

constructor TTimeStampFormat.Create(ACardinal: Integer);
begin
  Cardinal := ACardinal;
end;

class operator TTimeStampFormat.Equal(a: TTimeStampFormat;
  b: TTimeStampFormat): Boolean;
begin
  Result := a.Cardinal = b.Cardinal;
end;

{ TTsGridBufferEntry }

constructor TTsGridBufferEntry.Create;
begin
  inherited Create;
  Records := nil;
end;

destructor TTsGridBufferEntry.Destroy;
begin
  if Assigned(Records) then
    Records.Free;
  inherited Destroy;
end;

{ TTsGridSingleUndoBuffer }

constructor TTsGridSingleUndoBuffer.Create(AOwnsObjects: Boolean);
begin
  inherited Create(AOwnsObjects);
  IsProcessing := False;
end; 

resourcestring
  rsFlags = 'Flags';
  rsModified = 'Modified';
  rsFirst = 'First';
  rsPrevious = 'Prev';
  rsNext = 'Next';
  rsLast = 'Last';
  rsFunctionNotSupportedYet = 'Function not supported yet';

{ TTimeseriesGrid }

resourcestring
  rsMean = 'Mean';
  rsSum = 'Sum';
  rsValueCount = 'Number of values';
  rsMissingValues = 'Missing values';
  rsStDev = 'Standard deviation';
  rsVarianceCoef = 'Variance coefficient';
  rsMaxValue = 'Maximum value';
  rsMinValue = 'Minimum value';
  rsUpperLimit = 'Upper limit';
  rsLowerLimit = 'Lower limit';
  rsHighValues = 'High values';
  rsLowValues = 'Low values';

  rsAbbrMean = 'Mean';
  rsAbbrSum = 'Sum';
  rsAbbrValueCount = 'N';
  rsAbbrMissingValues = 'Miss';
  rsAbbrStDev = 'Stdev';
  rsAbbrVarianceCoef = 'Var coef';
  rsAbbrMaxValue = 'Max';
  rsAbbrMinValue = 'Min';
  rsAbbrUpperLimit = 'Up lim';
  rsAbbrLowerLimit = 'Low lim';
  rsAbbrHighValues = 'N Hi';
  rsAbbrLowValues = 'N Low';

constructor TTimeseriesGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFilteredSeries := nil;
  FData := TObjectList.Create(True);
  FUndoBuffers := TTsGridUndoBuffers.Create(True);
  FFontForModified := TTimeseriesGridFont.Create(Self);
  FFontForUnmodified := TTimeseriesGridFont.Create(Self);
  FFontForNew := TTimeseriesGridFont.Create(Self);
  FSavedFont := TFont.Create;
  FFontForModified.Style := [fsItalic];
  FFontForUnmodified.Style := [];
  FFontForNew.Style := [fsBold];
  FDisplayFormat := dfSimple;
  FNewDisplayFormat := dfSimple;
  FFiltered := False;
  FFilterCondition := fcNone;
  FFullyInvalidated := True;
  FUndoIDPointer := 0;
  FBgColorForStatistics := $00D0D0D0;
  FFilteredColor := $00FFFCEC;
  FHighlightColor := clYellow;
  FHighlightMode := hlNone;
  FUpperBoundCoefficient := 3;
  FLowerBoundCoefficient := 3;
  FHydrologicalYear := False;
  FNewHydrologicalYear := False;
  FIsReformatting := False;
  FCheckOnly := False;
  FOverwriteDates := False;
  FReadOnly := False;
  RowCount := 1;
  ColCount := 1;
  TopRow := 0;
  LeftCol := 0;
  DefaultRowHeight := (18*Screen.PixelsPerInch) div 96;
  DefaultColWidth := (DefaultColWidth+8) * Screen.PixelsPerInch div 96;
  FixedColor := ODSTRGRDDEFAULTFIXEDCOLOR;
  FHYearOrigin := 10;
  Options := Options + [goColSizing, goColMoving, goDrawFocusSelected, goTabs];
  FFirstColumnDefaultWidth := 105 * Screen.PixelsPerInch div 96;
end;

destructor TTimeseriesGrid.Destroy;
begin
  FSavedFont.Free;
  FFontForNew.Free;
  FFontForUnmodified.Free;
  FFontForModified.Free;
  FDates := nil;
  FDisplayedTable := nil;
  FData.Free;
  FUndoBuffers.Free;
  inherited Destroy;
end;

procedure TTimeseriesGrid.FullInvalidate;
begin
  Invalidate;
  FFullyInvalidated := True;
end;

procedure TTimeseriesGrid.Invalidate;
begin
  Inherited;
end;

function TTimeseriesGrid.InvalidateMethod: TMethod;
begin
  Result.Data := Self;
  Result.Code := MethodAddress('Invalidate');
end;

function TTimeseriesGrid.FullInvalidateMethod: TMethod;
begin
  Result.Data := Self;
  Result.Code := MethodAddress('FullInvalidate');
end;

resourcestring
  rsNoFilterSpecified = 'Cannot set Filtered to true unless a filter is '+
    'specified with SetFilter.';
  rsFcNoneOnlyUsedInternally = 'fcNone is only used internally.';

function TTimeseriesGrid.GetOwnsTimeseries: Boolean;
begin
  Result := FData.OwnsObjects;
end;

procedure TTimeseriesGrid.SetOwnsTimeseries(OwnsTimeseries: Boolean);
begin
  FData.OwnsObjects := OwnsTimeseries;
end;


function TTimeseriesGrid.GetReadOnly: Boolean;
begin
  Result := FReadOnly;
end;

procedure TTimeseriesGrid.SetReadOnly(Value: Boolean);
begin
  FReadOnly := Value;
end;

procedure TTimeseriesGrid.SetFiltered(Filtered: Boolean);
begin
  if Filtered and (FilterCondition=fcNone) then
    raise Exception.Create(rsNoFilterSpecified);
  if Filtered=FFiltered then Exit;
  FFiltered := Filtered;
  FFilteredSeries := nil;
  FullInvalidate;
end;

procedure TTimeseriesGrid.SetFilter(FilterCondition: TFilterCondition;
  FilterValue: Variant);
begin
  case FilterCondition of
    fcNone: raise Exception.Create(rsFcNoneOnlyUsedInternally);
    fcGreater, fcLess: FFilterValue := VarAsType(FilterValue, varDouble);
    fcHasFlag, fcHasNotFlag: FFilterValue := VarAsType(FilterValue, varString);
    fcIsNull, fcIsNotNull, fcIntactRecords, fcModifiedRecords,
      fcNewRecords: FFilterValue := Null;
  else
    Assert(False);
  end;
  FFilterCondition := FilterCondition;
  if Filtered then FullInvalidate;
end;

function TTimeseriesGrid.GetIndex(ACol: Longint): Integer;
begin
  if FDisplayFormat<>dfSimple then
    Result := ActiveIndex
  else if not FlagsVisible then
    Result := ACol-1
  else
    Result := (ACol-1) div 2;
end;

procedure TTimeseriesGrid.ShowEditor;
begin
  if FReadOnly then Exit;
  if (not EditorMode) and (PopupMenu<>nil) then
  begin
    FSavedPopupMenu := PopupMenu;
    PopupMenu := nil;
  end;
  inherited;
end;

procedure TTimeseriesGrid.HideEditor;
begin
  inherited;
  PopupMenu := FSavedPopupMenu;
end;

function TTimeseriesGrid.ActiveTimeseries: TTimeseries;
begin
  Result := Data[ActiveIndex];
end;

function TTimeseriesGrid.CanEditShow: Boolean;
begin
  Result := False;
  if not EditorMode then Exit;
  if csDesigning in ComponentState then Exit;
  if not HandleAllocated then Exit;
  if (not (goAlwaysShowEditor in Options)) and (not Focused) then Exit;
  if FDisplayFormat<>dfSimple then Exit;
  if (Col<1) or (Row<1) then Exit;
  if Data[GetIndex(Col)].SortingIndexOf(FDates[Row-1].SortingDate, FUseDateOffset)=-1 then Exit;
  if FReadOnly then Exit;  
  Result := True;
end;

function TTimeseriesGrid.GetEditText(ACol, ARow: Longint): string;
begin
  case FDisplayFormat of
    dfSimple:
      with TTimeseries(Data[GetIndex(ACol)]) do
        if FlagsVisible and not Odd(ACol) then
          Result := Items[SortingIndexOf(FDates[ARow-1].SortingDate, FUseDateOffset)].GetAllFlags
        else
        begin
          if Items[SortingIndexOf(FDates[ARow-1].SortingDate, FUseDateOffset)].IsNull then Result := ''
          else Result :=
            Format('%.'+IntToStr(Max(0,Precision))+'f',
              [Items[SortingIndexOf(FDates[ARow-1].SortingDate, FUseDateOffset)].AsFloat]);
        end;
  else
    Assert(False, rsFunctionNotSupportedYet);
  end;
end;

{ Note: SetEditText only actually makes changes if FCheckOnly is False.
  Otherwise, it only checks to see if these changes would be accepted, and
  raises an exception if not.
  This trick is used by PasteFromClipboard, which does the job twice; once for
  checking, once for doing, so that the text is not half-pasted in case of
  an error.
}
procedure TTimeseriesGrid.SetEditText(ACol, ARow: Longint; const Value: string);
var
  SavedText: string;
  SavedMStatus: TMStatus;
  SavedTimeseriesModified: Boolean;
begin
  if EditorMode then Exit; { Process only when finished editing (otherwise
                             the following runs on each keystroke) }
  FDontUndoFlag := False;
  case FDisplayFormat of
    dfSimple:
      with TTimeseries(Data[GetIndex(ACol)]) do
        if FlagsVisible and not Odd(ACol) then
        with Items[SortingIndexOf(FDates[ARow-1].SortingDate, FUseDateOffset)] do
        begin
          SavedText := GetAllFlags;
          SavedMStatus := MStatus;
          SavedTimeseriesModified := Modified;
          SetAllFlags(UpperCase(Value));
          if SavedText = GetAllFlags then
          begin
            MStatus := SavedMStatus;
            Modified := SavedTimeseriesModified;
            FDontUndoFlag := True;
          end;
          if FCheckOnly then
          begin
            SetAllFlags(SavedText);
            MStatus := SavedMStatus;
          end;
        end else
        with Items[SortingIndexOf(FDates[ARow-1].SortingDate, FUseDateOffset)] do
        begin
          SavedText := AsString;
          SavedMStatus := MStatus;
          SavedTimeseriesModified := Modified;
          AsString := Value;
          if SavedText = AsString then
          begin
            MStatus := SavedMStatus;
            Modified := SavedTimeseriesModified;
            FDontUndoFlag := True;            
          end;
          if FCheckOnly then
          begin
            if SavedText = '' then SetNull else AsFloat := StrToFloat(SavedText);
            MStatus := SavedMStatus;
          end;
        end;
  else
    Assert(False, rsFunctionNotSupportedYet);
  end;
end;

procedure TTimeseriesGrid.ReformatSimple;
var i, j: Integer;
    sd, sd1, sd2, dd, dd1, dd2: TDateTime; // sd: sorting date; dd: displ date
    TopRowDate, SavedActiveDate: TDateTime;
    PassesFilter: Boolean;
    TsIndex: array[0..tgMaxTimeseries-1] of Integer;{ Used to hold one index in each timeseries
                                     while scanning them altogether to determine
                                     the union of their dates. We assume that
                                     never will there be more than tgMaxTimeseries timeseries
                                     in a grid. }
    TsDateFormat: array[0..tgMaxTimeseries-1] of TTimeStampFormat; {Used to hold the time stamp
                                     format for each of the time series. }
    Ts: TTimeseries;
    sf: TTimeStampFormat;
begin
  Options := Options - [goColMoving]; { Temporarily disable column dragging. }
  SavedActiveDate := ActiveDate;

  { Which date was displayed until now at the top row? }
  if (FDisplayFormat=dfSimple) and (TopRow>=1) and (TopRow<=Length(FDates)) then
    TopRowDate := FDates[TopRow-1].SortingDate
  else
    TopRowDate := idaEmpty;

  { Determine if DateOffset will be used when sorting records. }
  FUseDateOffset := False;
  for i := 0 to Count-1 do
    if Data[i].TimeStep<tstMonthly then
      FUseDateOffset := FUseDateOffset or True;

  { Fill-in TsDateFormat. }
  for i := 0 to Count-1 do
    if Data[i].TimeStep<tstMonthly then
      TsDateFormat[i] := sfFull
    else
    begin
      if Data[i].TimeStep=tstAnnual then
      begin
        if Data[i].HydrologicalYear then TsDateFormat[i] := sfHydrologicalYear
        else TsDateFormat[i] := sfYear
      end else if Data[i].TimeStep=tstMonthly then
        TsDateFormat[i] := sfMonth
      else if Data[i].TimeStep<tstAnnual then
        TsDateFormat[i] := sfManyMonths
      else
      begin
        if Data[i].TimeStep.LengthMonths mod 12 = 0 then
          TsDateFormat[i] := sfManyYears
        else
          TsDateFormat[i] := sfManyMonths
      end;
      TsDateFormat[i].MonthSpan := Data[i].TimeStep.LengthMonths;
    end;

  { Determine rows of the grid }
  SetLength(FDates, 0);
  if not Filtered then
  begin
    { Determine the union of the visible dates for all timeseries }
    Assert(Count<=tgMaxTimeseries);
    { Determine the lowest date in all time series. Set TsIndex[i] = 0 for
      all time series. TsIndex[i] = -1 means that we have been past the end of
      the time series.
    }
    sd := 1e10;
    sf := sfFull; dd := 0; // Eliminate compiler warning
    for i := 0 to Count-1 do
    begin
      TsIndex[i] := 0;
      if (Data[i].Count=0) then
        TsIndex[i] := -1
      else
      begin
        sd1 := Data[i].First.SortingDate(FUseDateOffset);
        dd1 := Data[i].First.Date;
        if (DiffInSecs(sd1, sd)<0) then
        begin
          sd := sd1;
          dd := dd1;
          sf := TsDateFormat[i];
        end;
      end;
    end;
    if sd<0.9e10 then
    begin
      SetLength(FDates, 1);
      FDates[0].DisplayDate := dd;
      FDates[0].SortingDate := sd;
      FDates[0].DateFormat := sf;
    end;
    { Now go to the main loop. }
    sd2 := 0; dd2 := 0; dd1 := 0; // Eliminates compiler warning
    while True do
    begin
      sd1 := 1e10;
      { For each time series, find index that is greater than previous date (d).
        Meanwhile, also find the lowest date d1 greater than d. }
      for i := Count-1 downto 0 do
      begin
        j := TsIndex[i];
        if j = -1 then Continue;
        Ts := Data[i];
        while j<Ts.Count do
        begin
          sd2 := Ts[j].SortingDate(FUseDateOffset);
          dd2 := Ts[j].Date;
          if DiffInSecs(sd2, sd)>0 then Break else Inc(j);
        end;
        if j=Ts.Count then j := -1
        else if DiffInSecs(sd2, sd1)<0 then
        begin
          sd1 := sd2;
          dd1 := dd2;
          sf := TsDateFormat[i];
        end;
        TsIndex[i] := j;
      end;
      if sd1>0.9e10 then Break;
      j := Length(FDates);
      SetLength(FDates, j+1);
      FDates[j].DisplayDate := dd1;
      FDates[j].SortingDate := sd1;
      FDates[j].DateFormat := sf;
      sd := sd1;
    end;
  end
  else if Count>0 then
  begin
    i := 0;
    while i<Count do
    begin
      if Data[i]=FFilteredSeries then
        Break;
      Inc(i);
    end;
    if i = Count then FFilteredSeries := ActiveTimeseries;
    { Determine records that pass the filter }
    for i := 0 to FFilteredSeries.Count-1 do
      with FFilteredSeries[i] do
      begin
        PassesFilter := False;
        case FilterCondition of
          fcGreater: PassesFilter := (not IsNull) and (AsFloat>FilterValue);
          fcLess: PassesFilter := (not IsNull) and (AsFloat<FilterValue);
          fcIsNull: PassesFilter := IsNull;
          fcIsNotNull: PassesFilter := not IsNull;
          fcHasFlag: PassesFilter := GetFlag(FilterValue);
          fcHasNotFlag: PassesFilter := not GetFlag(FilterValue);
          fcIntactRecords: PassesFilter := (MStatus=msUnmodified);
          fcModifiedRecords: PassesFilter := (MStatus=msModified);
          fcNewRecords: PassesFilter := (MStatus=msNew);
        else
          Assert(False);
        end;
        if PassesFilter then
        begin
          j := Length(FDates);
          SetLength(FDates, j+1);
          FDates[j].SortingDate := SortingDate(FUseDateOffset);
          FDates[j].DisplayDate := Date;
          FDates[j].DateFormat := TsDateFormat[ActiveIndex];
        end;
      end;
  end;

  { Determine number of rows and columns }
  if FlagsVisible then ColCount := (Count*2) + 1 else ColCount := Count + 1;
  RowCount := Length(FDates) + 1;
  if RowCount>1 then FixedRows := 1;
  if ColCount>1 then FixedCols := 1;
  TopRow := 0;

  if FDisplayFormat=dfSimple then
  begin
    { Keep the same top row we had before. }
    i := 0;
    j := Length(FDates);
    while i<j do
    begin
      if DiffInSecs(FDates[i].SortingDate, TopRowDate)>=0 then Break;
      Inc(i);
    end;
    if i=j then i := -1;
    if RowCount>1 then
      if TopRowDate = idaEmpty then
	TopRow := 1
      else if (i=-1) and (Length(FDates)<VisibleRowCount) then
	TopRow := 1
      else if (i=-1) and (Length(FDates)>=VisibleRowCount) then
	TopRow := Length(FDates)-VisibleRowCount+1
      else
	TopRow := i+1;
  end else
  begin
    if FDisplayFormat=dfTable then ActiveIndex := FActiveIndex;
  end;

  RestoreColWidths;
  FDisplayFormat := FNewDisplayFormat;
  FHydrologicalYear := FNewHydrologicalYear;
  Options := Options + [goColMoving];
  if (Count>0) and (SavedActiveDate<>idaEmpty) then
    i := ActiveTimeseries.NearestTo(SavedActiveDate)
  else
    i := -1;
  if i<>-1 then ActiveDate := ActiveTimeseries[i].Date;
end;

const tgNumOfStatistics = 10;

procedure TTimeseriesGrid.ReformatTable;
var
  i, j, k: Integer;
  Year, Month, Day, Year2: Word;
  DataEndRow, DataEndCol: Integer;
  d, SavedActiveDate: TDateTime;
  Stats: TRowColStatistics;
  GrandTotal, Maximum, Minimum: Real;
  GrandCount: Integer;
  ShownMean, ExtraColOrRow: Boolean;
begin
  { Eliminate compiler warnings. }
  d := 0;

  SavedActiveDate := ActiveDate;
  if FDisplayFormat=dfSimple then SaveColWidths;
  Options := Options - [goColMoving];
  if (ActiveIndex = -1) or (ActiveTimeseries.Count=0) then
  begin
    RowCount := 0;
    ColCount := 0;
    FDisplayFormat := FNewDisplayFormat;
    Exit;
  end;

  { Determine RowCount and ColCount. }
//  case ActiveTimeseries.TimeStep of
    if ActiveTimeseries.TimeStep=tstHourly then
               begin
                 DecodeDate(FNewBaseDate, Year, Month, Day);
                 RowCount := MonthDays[IsLeapYear(Year), Month] + 2;
                 ColCount := 24+2;
               end
    else if ActiveTimeseries.TimeStep=tstDaily then begin
                 RowCount := 31+2;
                 ColCount := 12+2;
               end
    else if ActiveTimeseries.TimeStep=tstMonthly then begin
                  ColCount := 12+2;
                  if FNewHydrologicalYear then
                    RowCount := 2+
                      (FindHydrologicalYear(ActiveTimeseries.Last.Date,
                        FHYearOrigin)-
                      FindHydrologicalYear(ActiveTimeseries.First.Date,
                        FHYearOrigin)+1)
                  else
                    RowCount := 2+
                      (YearOf(ActiveTimeseries.Last.Date)-
                      YearOf(ActiveTimeseries.First.Date)+1);
                end
  else
    Assert(False);
//  end;

  SetLength(FDisplayedTable,RowCount+tgNumOfStatistics+1);
  { Clear FDisplayedTable. }
  for i := RowCount+tgNumOfStatistics downto 0 do
    for j := tgMaxCols-1 downto 0 do
      with FDisplayedTable[i, j] do
      begin
        DisplayedValue := '';
        RelatedTsRecord := nil;
        DateRepresented := idaEmpty;
        BgColor := 0; // Signal to use default color
      end;

  DataEndRow := RowCount-2;
  DataEndCol := ColCount-2;
  if StatisticsVisible then
  begin
    RowCount := RowCount + tgNumOfStatistics;
    ColCount := ColCount + tgNumOfStatistics;
    { There's one additional row or column, with the sum, if the variable is
      Cumulative. }
    if ActiveTimeseries.VariableType in [vtCumulative, vtMinimum, vtMaximum] then
      if ActiveTimeseries.TimeStep=tstDaily then RowCount := RowCount + 1
      else ColCount := ColCount+1;
  end;
  { Determine fixed rows and cols. }
  FixedRows := 1;
  FixedCols := 1;
  for j := DataEndCol downto 1 do
    if ActiveTimeseries.TimeStep = tstHourly then
      FDisplayedTable[0, j].DisplayedValue := IntToStr(j-1) // 0 to 23
    else if not FNewHydrologicalYear then
      FDisplayedTable[0, j].DisplayedValue := IntToStr(j)  // 1 to 12
    else if j<=12-FHYearOrigin+1{3} then
      FDisplayedTable[0, j].DisplayedValue := IntToStr(j+FHYearOrigin-1{9})
    else
      FDisplayedTable[0, j].DisplayedValue := IntToStr(j-(12-FHYearOrigin+1){3});
  for i := DataEndRow downto 1 do
    if ActiveTimeseries.TimeStep<>tstMonthly then
      FDisplayedTable[i, 0].DisplayedValue := IntToStr(i)
    else if FNewHydrologicalYear then
    begin
      Year := FindHydrologicalYear(ActiveTimeseries[0].Date, FHYearOrigin)+i-1;
      FDisplayedTable[i, 0].DisplayedValue :=
          Format('%4.4d-%2.2d', [Year, (Year+1) mod 100]);
    end else
      FDisplayedTable[i, 0].DisplayedValue :=
        IntToStr(YearOf(ActiveTimeseries[0].Date)+i-1);
  if ActiveTimeseries.TimeStep=tstHourly then
    FDisplayedTable[0,0].DisplayedValue :=FormatDateTime('yyyy/mm',FNewBaseDate)
  else if (ActiveTimeseries.TimeStep=tstDaily) and FNewHydrologicalYear then
    FDisplayedTable[0,0].DisplayedValue := Format('%4.4d-%2.2d',
      [YearOf(FNewBaseDate), (YearOf(FNewBaseDate)+1) mod 100])
  else if (ActiveTimeseries.TimeStep=tstDaily) and not FNewHydrologicalYear then
    FDisplayedTable[0,0].DisplayedValue := IntToStr(YearOf(FNewBaseDate));

  { Put date corresponding to top-left column in Year, Month, Day. }
  if (ActiveTimeseries.TimeStep=tstMonthly) and FNewHydrologicalYear then
  begin
    Year := FindHydrologicalYear(ActiveTimeseries[0].Date, FHYearOrigin);
    Month := FHYearOrigin{10};
  end
  else if (ActiveTimeseries.TimeStep=tstMonthly) and not FNewHydrologicalYear then
  begin
    Year := YearOf(ActiveTimeseries[0].Date);
    Month := 1;
  end else
    DecodeDate(FNewBaseDate, Year, Month, Day);

  { Determine values inside table. }
  Maximum := -1e37;
  Minimum := 1e37;
  for i := DataEndRow downto 1 do
    for j := DataEndCol downto 1 do
    begin
      if ActiveTimeseries.TimeStep=tstHourly then
        d := AddDateTime(EncodeDateTime(Year, Month, i, j-1, 0, 0, 0),
          ActiveTimeseries.DateOffset)
      else if ActiveTimeseries.TimeStep=tstDaily then
      begin
        if not FNewHydrologicalYear then k := j
        else if j<=(12-FHYearOrigin+1){3} then k := j+(FHYearOrigin-1){9}
        else k := j-(12-FHYearOrigin+1){3};
        Year2 := Year;
        if FNewHydrologicalYear and (k<FHYearOrigin{10}) then Inc(Year2);
        if i>MonthDays[IsLeapYear(Year2), k] then
          d := idaEmpty
        else
          d := AddDateTime(EncodeDate(Year2, k, i), ActiveTimeseries.DateOffset);
      end else if (ActiveTimeseries.TimeStep=tstMonthly) then
      begin
        if not FNewHydrologicalYear then k := j
        else if j<=(12-FHYearOrigin+1){3} then k := j+(FHYearOrigin-1){9}
        else k := j-(12-FHYearOrigin+1){3};
        Year2 := Year+i-1;
        if FNewHydrologicalYear and (k<HYearOrigin{10}) then Inc(Year2);
        d := EncodeDate(Year2, k, 1);
      end else
        Assert(False);
      FDisplayedTable[i, j].DateRepresented := d;
      if d = idaEmpty then k := -2
      else k := ActiveTimeseries.IndexOf(d);
      if (k=-1) and (HighlightMode=hlNull) then
        FDisplayedTable[i,j].BgColor := HighlightColor
      else if k>=0 then
      begin
        with FDisplayedTable[i, j] do
        begin
          DisplayedValue := ActiveTimeseries[k].AsString;
          RelatedTsRecord := ActiveTimeseries[k];
          if (HighlightMode=hlFlag) and (RelatedTsRecord.GetAllFlags<>'') then
            BgColor := HighlightColor;
          if (HighlightMode=hlNull) and (RelatedTsRecord.IsNull) then
            BgColor := HighlightColor;
        end;
        if (not ActiveTimeseries[k].IsNull) then
        begin
          if ActiveTimeseries[k].AsFloat>Maximum then
            Maximum := ActiveTimeseries[k].AsFloat;
          if ActiveTimeseries[k].AsFloat<Minimum then
            Minimum := ActiveTimeseries[k].AsFloat;
        end;
      end;
    end;

  { Repass table and highlight maximums or minimums if higlight mode says so. }
  if HighlightMode in [hlMax, hlMin, hlMaxAndMin] then
    for i := DataEndRow downto 1 do
      for j := DataEndCol downto 1 do
        with FDisplayedTable[i, j] do
          if (RelatedTsRecord<>nil) and (not RelatedTsRecord.IsNull)
          and
            (((HighlightMode in [hlMax,hlMaxAndMin])
              and
              (RelatedTsRecord.AsFloat=Maximum))
            or
             ((HighlightMode in [hlMin,hlMaxAndMin])
              and
             (RelatedTsRecord.AsFloat=Minimum)))
          then
            BgColor := HighlightColor;

  { Calculate and display statistics for rows. }
  GrandTotal := 0;
  GrandCount := 0;
  for i := DataEndRow downto 1 do
  begin
    for j := ColCount-1 downto DataEndCol+1 do
      FDisplayedTable[i, j].BgColor := BgColorForStatistics;
    Stats := GetRowColStatistics(i, -1);
    j := DataEndCol+1;
    ExtraColOrRow := False;
    ShownMean := False;
    if (ActiveTimeseries.VariableType in [vtCumulative, vtMinimum,
    vtMaximum]) and    
    (ActiveTimeseries.TimeStep.TimeStepIn([tstHourly, tstMonthly])) then
    begin
      ExtraColOrRow := True;
      if Stats.ValueCount>0 then
      begin
        case ActiveTimeseries.VariableType of
        vtCumulative:
        begin
          FDisplayedTable[i, j].DisplayedValue := Format('%.2f', [Stats.Sum]);
          FDisplayedTable[0, j].DisplayedValue := rsAbbrSum;
        end;
        vtMaximum:
        begin
          FDisplayedTable[i, j].DisplayedValue := Format('%.2f', [Stats.MaxValue]);
          FDisplayedTable[0, j].DisplayedValue := rsAbbrMaxValue;
        end;
        vtMinimum:
        begin
          FDisplayedTable[i, j].DisplayedValue := Format('%.2f', [Stats.MinValue]);
          FDisplayedTable[0, j].DisplayedValue := rsAbbrMinValue;
        end;
        else
          Assert(False);
        end;
      end;
    end else
    begin
      if Stats.ValueCount>0 then
      begin
        FDisplayedTable[i, j].DisplayedValue := Format('%.2f',[Stats.Mean]);
        FDisplayedTable[0, j].DisplayedValue := rsAbbrMean;
      end;
      ShownMean := True;
    end;
    GrandTotal := GrandTotal + Stats.Sum;
    Inc(GrandCount, Stats.ValueCount);
    if StatisticsVisible then
    begin
      if ExtraColOrRow or (not ShownMean and (Stats.ValueCount>0)) then
        Inc(j);
      if not ShownMean and (Stats.ValueCount>0) then
      begin
        FDisplayedTable[i, j].DisplayedValue:=Format('%.2f',[Stats.Mean]);
        FDisplayedTable[0, j].DisplayedValue := rsAbbrMean;
      end;
      Inc(j);
      if Stats.ValueCount>1 then
        FDisplayedTable[i, j].DisplayedValue :=Format('%.2f',[Stats.StDev]);
      FDisplayedTable[0, j].DisplayedValue := rsAbbrStDev;
      Inc(j);
      if (Stats.ValueCount>1) and (Stats.Mean>1e-37) then
        FDisplayedTable[i, j].DisplayedValue:=Format('%.2f',[Stats.VarianceCoef]);
      FDisplayedTable[0, j].DisplayedValue := rsAbbrVarianceCoef;
      Inc(j);
      FDisplayedTable[i,j].DisplayedValue := IntToStr(Stats.ValueCount);
      FDisplayedTable[0, j].DisplayedValue := rsAbbrValueCount;
      Inc(j);
      FDisplayedTable[i,j].DisplayedValue :=IntToStr(Stats.MissingValues);
      FDisplayedTable[0, j].DisplayedValue := rsAbbrMissingValues;
      Inc(j);
      if (Stats.ValueCount>0) then
        FDisplayedTable[i, j].DisplayedValue :=Format('%.2f',[Stats.MaxValue]);
      FDisplayedTable[0, j].DisplayedValue := rsAbbrMaxValue;
      Inc(j);
      if (Stats.ValueCount>0) then
        FDisplayedTable[i, j].DisplayedValue :=Format('%.2f',[Stats.MinValue]);
      FDisplayedTable[0, j].DisplayedValue := rsAbbrMinValue;
      Inc(j);
      if (Stats.ValueCount>1) then
        FDisplayedTable[i, j].DisplayedValue :=Format('%.2f',[Stats.UpperLimit]);
      FDisplayedTable[0, j].DisplayedValue := rsAbbrUpperLimit;
      Inc(j);
      if (Stats.ValueCount>1) then
        FDisplayedTable[i, j].DisplayedValue :=Format('%.2f',[Stats.LowLimit]);
      FDisplayedTable[0, j].DisplayedValue := rsAbbrLowerLimit;
      Inc(j);
      if (Stats.ValueCount>1) then
        FDisplayedTable[i,j].DisplayedValue :=IntToStr(Stats.HighValues);
      FDisplayedTable[0, j].DisplayedValue := rsAbbrHighValues;
      Inc(j);
      if (Stats.ValueCount>1) then
        FDisplayedTable[i,j].DisplayedValue :=IntToStr(Stats.LowValues);
      FDisplayedTable[0, j].DisplayedValue := rsAbbrLowValues;
    end;
  end;

  { Calculate and display statistics for columns. }

  for j := DataEndCol downto 1 do
  begin
    for i := RowCount-1 downto DataEndRow+1 do
      FDisplayedTable[i, j].BgColor := BgColorForStatistics;
    Stats := GetRowColStatistics(-1, j);
    i := DataEndRow+1;
    ShownMean := False;
    ExtraColOrRow := False;
    if (ActiveTimeseries.VariableType in [vtCumulative, vtMaximum,
    vtMinimum]) and
    (ActiveTimeseries.TimeStep=tstDaily) then
    begin
      ExtraColOrRow := True;
      if Stats.ValueCount>0 then
      begin
        case ActiveTimeseries.VariableType of
          vtCumulative:
          begin
            FDisplayedTable[i, j].DisplayedValue := Format('%.2f', [Stats.Sum]);
            FDisplayedTable[i, 0].DisplayedValue := rsSum;
          end;
          vtMaximum:
          begin
            FDisplayedTable[i, j].DisplayedValue := Format('%.2f', [Stats.MaxValue]);
            FDisplayedTable[i, 0].DisplayedValue := rsMaxValue;
          end;
          vtMinimum:
          begin
            FDisplayedTable[i, j].DisplayedValue := Format('%.2f', [Stats.MinValue]);
            FDisplayedTable[i, 0].DisplayedValue := rsMinValue;
          end;
        else
          Assert(False);
        end;
      end;
    end else
    begin
      if Stats.ValueCount>0 then
      begin
        FDisplayedTable[i, j].DisplayedValue := Format('%.2f',[Stats.Mean]);
        FDisplayedTable[i, 0].DisplayedValue := rsMean;
      end;
      ShownMean := True;
    end;
    if StatisticsVisible then
    begin
      if ExtraColOrRow or (not ShownMean and (Stats.ValueCount>0)) then
        Inc(i);
      if not ShownMean and (Stats.ValueCount>0) then
      begin
        FDisplayedTable[i, j].DisplayedValue:=Format('%.2f',[Stats.Mean]);
        FDisplayedTable[i, 0].DisplayedValue := rsMean;
      end;
      Inc(i);
      if Stats.ValueCount>1 then
        FDisplayedTable[i, j].DisplayedValue :=Format('%.2f',[Stats.StDev]);
      FDisplayedTable[i, 0].DisplayedValue := rsStDev;
      Inc(i);
      if (Stats.ValueCount>1) and (Stats.Mean>1e-37) then
        FDisplayedTable[i, j].DisplayedValue:=Format('%.2f',[Stats.VarianceCoef]);
      FDisplayedTable[i, 0].DisplayedValue := rsVarianceCoef;
      Inc(i);
      FDisplayedTable[i,j].DisplayedValue := IntToStr(Stats.ValueCount);
      FDisplayedTable[i, 0].DisplayedValue := rsValueCount;
      Inc(i);
      FDisplayedTable[i,j].DisplayedValue :=IntToStr(Stats.MissingValues);
      FDisplayedTable[i, 0].DisplayedValue := rsMissingValues;
      Inc(i);
      if (Stats.ValueCount>0) then
        FDisplayedTable[i, j].DisplayedValue :=Format('%.2f',[Stats.MaxValue]);
      FDisplayedTable[i, 0].DisplayedValue := rsMaxValue;
      Inc(i);
      if (Stats.ValueCount>0) then
        FDisplayedTable[i, j].DisplayedValue :=Format('%.2f',[Stats.MinValue]);
      FDisplayedTable[i, 0].DisplayedValue := rsMinValue;
      Inc(i);
      if (Stats.ValueCount>1) then
        FDisplayedTable[i, j].DisplayedValue :=Format('%.2f',[Stats.UpperLimit]);
      FDisplayedTable[i, 0].DisplayedValue := rsUpperLimit;
      Inc(i);
      if (Stats.ValueCount>1) then
        FDisplayedTable[i, j].DisplayedValue :=Format('%.2f',[Stats.LowLimit]);
      FDisplayedTable[i, 0].DisplayedValue := rsLowerLimit;
      Inc(i);
      if (Stats.ValueCount>1) then
        FDisplayedTable[i,j].DisplayedValue :=IntToStr(Stats.HighValues);
      FDisplayedTable[i, 0].DisplayedValue := rsHighValues;
      Inc(i);
      if (Stats.ValueCount>1) then
        FDisplayedTable[i,j].DisplayedValue :=IntToStr(Stats.LowValues);
      FDisplayedTable[i, 0].DisplayedValue := rsLowValues;
    end;
  end;

  with FDisplayedTable[DataEndRow+1, DataEndCol+1] do
    if ActiveTimeseries.VariableType = vtCumulative then                        
    if GrandCount>0 then
      DisplayedValue := Format('%.2f', [GrandTotal/(DataEndRow)])
    else if GrandCount>0 then
      DisplayedValue := Format('%.2f', [GrandTotal/GrandCount]);

  { Activate correct cell. }
  FHydrologicalYear := FNewHydrologicalYear;
  FBaseDate := FNewBaseDate;
  if (FDisplayFormat=dfSimple) or (ActiveTimeseries.TimeStep=tstAnnual) then
    ActiveDate := SavedActiveDate;
    { otherwise, leave cell where it is }
  FDisplayFormat := FNewDisplayFormat;

  AutoFit;
end;

procedure TTimeseriesGrid.Reformat;
var SavedCursor: TCursor;
begin
  FFullyInvalidated := False;
  FIsReformatting := True;
  SavedCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    case FNewDisplayFormat of
      dfSimple: ReformatSimple;
      dfTable: ReformatTable
    else
      Assert(False);
    end;
    Invalidate;
  finally
    Screen.Cursor := SavedCursor;
    FIsReformatting := False;
  end;
end;

function TTimeseriesGrid.GetCellText(ACol, ARow: Longint;
  var TsRecord: TTsRecord; DateDisplayFormat: TDateDisplayFormat): string;

  function GetCellTextSimple(ACol, ARow: Longint; var TsRecord: TTsRecord):
  string;
  var
    CurrentTimeseries: TTimeseries;
    CurrentIndex: Integer;
    Year, Month, Day, Year2, Month2: Word;
    ADate2: TDateTime;
  begin
    TsRecord := nil;
    if (ACol<>0) and (ARow<>0) then
    begin
      CurrentTimeseries := TTimeseries(Data[GetIndex(ACol)]);
      CurrentIndex := CurrentTimeseries.SortingIndexOf(FDates[ARow-1].SortingDate,
        FUseDateOffset);
      if CurrentIndex<>-1 then
      begin
        TsRecord := CurrentTimeseries[CurrentIndex];
        if FlagsVisible and not Odd(ACol) then
          Result := TsRecord.GetAllFlags
        else
          Result := TsRecord.AsString;
      end else
        Result := '';
    end
    else if (ACol = 0) and (ARow <> 0) then begin
      if FDates[ARow-1].DateFormat=sfFull then
        Result := FormatDateTime('yyyy/mm/dd hh:nn', FDates[ARow-1].DisplayDate)
      else if FDates[ARow-1].DateFormat=sfMonth then
        Result := FormatDateTime('yyyy/mm', FDates[ARow-1].DisplayDate)
      else if FDates[ARow-1].DateFormat=sfYear then
        Result := FormatDateTime('yyyy', FDates[ARow-1].DisplayDate)
      else if FDates[ARow-1].DateFormat=sfHydrologicalYear then
      begin
        DecodeDate(FDates[ARow-1].DisplayDate, Year, Month, Day);
        Result := Format('%4.4d-%2.2d', [Year, (Year+1) mod 100]);
      end
      else if (FDates[ARow-1].DateFormat=sfManyMonths) or
        (FDates[ARow-1].DateFormat=sfManyYears) then
      begin
        if DateDisplayFormat=ddfComplex then
        begin
          DecodeDate(FDates[ARow-1].DisplayDate, Year, Month, Day);
          ADate2 := IncMonth(FDates[ARow-1].DisplayDate,
            FDates[ARow-1].DateFormat.MonthSpan -1);
          DecodeDate(ADate2, Year2, Month2, Day);
          if (FDates[ARow-1].DateFormat=sfManyMonths)
            or ((FDates[ARow-1].DateFormat=sfManyYears) and (Month<>1)) then
            Result := Format('%4.4d/%2.2d - %4.4d/%2.2d',
              [Year, Month, Year2, Month2])
          else
            Result := Format('%4.4d - %4.4d', [Year, Year2]);
        end else if DateDisplayFormat=ddfSimple then
        begin
          Result := FormatDateTime('yyyy/mm', FDates[ARow-1].DisplayDate)
        end;
      end
      else
        Assert(False);
    end
    else if (ACol<>0) and (ARow=0) and not FlagsVisible then
    begin
      Result := Data[ACol-1].Title;
      if Data[ACol-1].Modified then Result := Result{+' ('+rsModified+')'};
    end else if (ACol<>0) and (ARow=0) and FlagsVisible and Odd(ACol) then
    begin
      Result := Data[GetIndex(ACol)].Title;
      if Data[GetIndex(ACol)].Modified then Result := Result{+' ('+rsModified+')'};
    end else if (ACol<>0) and (ARow=0) and FlagsVisible and not Odd(ACol) then
      Result := rsFlags;
  end;

begin
  if FDisplayFormat = dfSimple then
    Result := GetCellTextSimple(ACol, ARow, TsRecord)
  else if FDisplayFormat = dfTable then
  begin
    Result := FDisplayedTable[ARow, ACol].DisplayedValue;
    TsRecord := FDisplayedTable[ARow, ACol].RelatedTsRecord;
  end
  else
    Assert(False);
end;

procedure TTimeseriesGrid.DrawCellSimple(ACol, ARow: Longint; ARect: TRect);
var
  CurrentIndex: Integer;
  CurrentTimeseries: TTimeseries;
  ReferencePoint: Integer;
  ATsRecord: TTsRecord;
  SavedColor: TColor;
  AStatisticsBgColor: Boolean;
  s: string;
begin
  s := '';
  SaveFont;
  AStatisticsBgColor := False;

  { Determine cell contents }
  if (ACol<>0) and (ARow<>0) then
  begin
    CurrentTimeseries := TTimeseries(Data[GetIndex(ACol)]);
    AStatisticsBgColor := CurrentTimeseries.StatisticsBgColor;
    CurrentIndex := CurrentTimeseries.SortingIndexOf(FDates[ARow-1].SortingDate,
      FUseDateOffset);
    if CurrentIndex<>-1 then
      AdjustFont(CurrentTimeseries[CurrentIndex])
    else
    begin
      Polyline(Canvas.Handle, ARect, 2);
      Exit;
    end;
  end;
  s := GetCellText(ACol, ARow, ATsRecord)+' '; { The space at the end is for
                                                 a right border. }

  { Determine cell alignment }
  SetTextAlign(Canvas.Handle, TA_RIGHT);
  ReferencePoint := ARect.Right-2;
  if (ARow=0) and (ACol>0) then
  begin
    if TTimeseries(Data[GetIndex(ACol)]).Modified then
      Canvas.Font.Style := Canvas.Font.Style + [fsItalic] else
      Canvas.Font.Style := Canvas.Font.Style - [fsItalic];
    SetTextAlign(Canvas.Handle, TA_CENTER);
    ReferencePoint := (ARect.Left+ARect.Right) div 2;
  end
  else if ACol=0 then
  begin
    SetTextAlign(Canvas.Handle, TA_LEFT);
    ReferencePoint := ARect.Left+2;
  end;

  SavedColor := Canvas.Brush.Color;
  if (HighlightMode=hlFlag) and (ATsRecord<>nil) and
    (ATsRecord.GetAllFlags<>'') and
    (Canvas.Brush.Color=clWindow) then
    Canvas.Brush.Color := HighlightColor;
  if (HighlightMode=hlNull) and (ATsRecord<>nil) and ATsRecord.IsNull and
  (Canvas.Brush.Color=clWindow) then
    Canvas.Brush.Color := HighlightColor;
  if AStatisticsBgColor and (ATsRecord<>nil) and
  (Canvas.Brush.Color=clWindow) then
    Canvas.Brush.Color := BgColorForStatistics;
  if Filtered and (FFilteredSeries<>nil) and (ATsRecord<>nil) and
    (ATsRecord.Owner = FFilteredSeries) and
    (Canvas.Brush.Color=clWindow) then
    Canvas.Brush.Color := FFilteredColor;
  ExtTextOut(Canvas.Handle, ReferencePoint, ARect.Top + 2, ETO_CLIPPED or
    ETO_OPAQUE, @ARect, PChar(s), Length(s), nil);
  Canvas.Brush.Color := SavedColor;
  RestoreFont;
end;

procedure TTimeseriesGrid.DrawCellTable(ACol, ARow: Longint; ARect: TRect);
var
  ReferencePoint: Integer;
  s: string;
  SavedColor: TColor;
begin
  SaveFont;

  { Determine cell alignment }
  SetTextAlign(Canvas.Handle, TA_RIGHT);
  ReferencePoint := ARect.Right-2;
  if (ARow=0) or (ACol=0) then
  begin
    SetTextAlign(Canvas.Handle, TA_CENTER);
    ReferencePoint := (ARect.Left+ARect.Right) div 2;
  end;

  SavedColor := Canvas.Brush.Color;
  with FDisplayedTable[ARow, ACol] do
    if (BgColor<>0) and (Canvas.Brush.Color=clWindow) then
      Canvas.Brush.Color := BgColor;
  s := FDisplayedTable[ARow, ACol].DisplayedValue + ' ';
    { The space at the end is for a right border. }
  ExtTextOut(Canvas.Handle, ReferencePoint, ARect.Top + 2, ETO_CLIPPED or
    ETO_OPAQUE, @ARect, PChar(s), Length(s), nil);
  Canvas.Brush.Color := SavedColor;
  RestoreFont;
end;

function TTimeseriesGrid.GetCellTextWidth(ACol, ARow: Longint): Integer;
var
  ASize: TSize;
  s: string;
  ATsRecord: TTsRecord;
begin
  SaveFont;
  if FDisplayFormat=dfSimple then AdjustFont(ACol, ARow);
  s := GetCellText(ACol, ARow, ATsRecord);
  GetTextExtentPoint32(Canvas.Handle, PChar(s), Length(s), ASize);
  Result := ASize.cx;
  RestoreFont;
end;

procedure TTimeseriesGrid.AutoFit(ACol: Longint);
var i, w: Integer;
begin
  w := GetCellTextWidth(ACol, RowCount-1);
  for i := RowCount-2 downto 0 do
    w := Max(w, GetCellTextWidth(ACol, i));
  ColWidths[ACol] := w + (10*Screen.PixelsPerInch) div 96;
end;

procedure TTimeseriesGrid.AutoFit;
var i: Integer;
begin
  for i := ColCount-1 downto 0 do Autofit(i);
end;

function TTimeseriesGrid.GetRowColStatistics(ARow, ACol: Longint):
  TRowColStatistics;
var
  i, n: Integer;
  ADisplayedTableItem: TDisplayedTableItem;
  a: Real;
begin
  if ARow=-1 then n := RowCount-1
  else n := ColCount-1;

  { Pass 1: Find whatever possible, i.e. whatever does not depend on mean. }
  Result.ValueCount := 0;
  Result.Sum := 0;
  Result.MaxValue := -1e38;
  Result.MinValue := 1e38;
  Result.MissingValues := 0;
  for i := n downto 1 do
  begin
    if ARow=-1 then ADisplayedTableItem := FDisplayedTable[i, ACol]
    else ADisplayedTableItem := FDisplayedTable[ARow, i];
    with ADisplayedTableItem do
      if (RelatedTsRecord<>nil) and not RelatedTsRecord.IsNull then
      begin
        a := RelatedTsRecord.AsFloat;
        Inc(Result.ValueCount);
        Result.Sum := Result.Sum + a;
        Result.MaxValue := Max(Result.MaxValue, a);
        Result.MinValue := Min(Result.MinValue, a);
      end else if DateRepresented<>idaEmpty then
        Inc(Result.MissingValues);
  end;
  with Result do if ValueCount>0 then Mean := Sum/ValueCount;

  if Result.ValueCount<=1 then Exit; // Variance and all else undefined.

  { Pass 2 - find variance. }
  a := 0;
  for i := n downto 1 do
  begin
    if ARow=-1 then ADisplayedTableItem := FDisplayedTable[i, ACol]
    else ADisplayedTableItem := FDisplayedTable[ARow, i];
    with ADisplayedTableItem do
      if (RelatedTsRecord<>nil) and not RelatedTsRecord.IsNull then
        a := a + Sqr(RelatedTsRecord.AsFloat-Result.Mean);
  end;
  Result.StDev := Sqrt(a/(Result.ValueCount-1));
  if Result.Mean>1e-37 then Result.VarianceCoef := Result.StDev/Result.Mean;
  Result.UpperLimit := Result.Mean + UpperBoundCoefficient*Result.StDev;
  Result.LowLimit := Result.Mean - LowerBoundCoefficient*Result.StDev;

  { Pass 3 - find low and high values. }
  Result.HighValues := 0;
  Result.LowValues := 0;
  for i := n downto 1 do
  begin
    if ARow=-1 then ADisplayedTableItem := FDisplayedTable[i, ACol]
    else ADisplayedTableItem := FDisplayedTable[ARow, i];
    with ADisplayedTableItem do
      if (RelatedTsRecord<>nil) and not RelatedTsRecord.IsNull then
      begin
        a := RelatedTsRecord.AsFloat;
        if a>Result.UpperLimit then
        begin
          Inc(Result.HighValues);
          if StatisticsVisible and (HighlightMode in [hlLarge, hlLargeAndSmall])
          then BgColor := HighlightColor;
        end;
        if a<Result.LowLimit then
        begin
          Inc(Result.LowValues);
          if StatisticsVisible and (HighlightMode in [hlSmall, hlLargeandSmall])
          then BgColor := HighlightColor;
        end;
      end;
    if ARow=-1 then FDisplayedTable[i, ACol] := ADisplayedTableItem
    else FDisplayedTable[ARow, i] := ADisplayedTableItem;
  end;
end;

function TTimeseriesGrid.GetCount: Integer;
begin
  Result := FData.Count;
end;

{ GetActiveIndex doesn't always return FActiveIndex. This happens only in
  display formats that display one timeseries at a time. For other formats,
  GetActiveIndex ignores FActiveIndex and determines the ActiveIndex from the
  selected cell. }
function TTimeseriesGrid.GetActiveIndex: Integer;
begin
  if Count=0 then
    Result := -1
  else if FDisplayFormat = dfTable then
    Result := FActiveIndex
  else if FlagsVisible then
    Result := Max(0, (Col-1) div 2)
  else
    Result := Max(0, Col-1);
  if Result>Count-1 then
    Result := Count-1;
end;

resourcestring
  rsListIndexOutOfBounds = 'List index out of bounds';
  
procedure TTimeseriesGrid.SetActiveIndex(Value: Integer);
begin
  if (Value<0) or (Value>=Count) then
    raise EListError.Create(rsListIndexOutOfBounds+' ('+IntToStr(Value)+')');
  if FDisplayFormat = dfTable then
  begin
    if not (Data[Value].TimeStep.TimeStepIn([tstHourly, tstDaily, tstMonthly])) then
      DisplayFormat := dfSimple
    else
      FActiveIndex := Value;
    FullInvalidate;
  end else if FlagsVisible then
  begin
//    Assert(Value*2+1<=ColCount);
    Col := Max(Min((Value*2)+1, ColCount-2), 0);
  end else
  begin
//    Assert(Value+1<=ColCount);
    Col := Max(Min(Value+1, ColCount-1), 0);
  end;
end;

procedure TTimeseriesGrid.SetLowerBoundCoefficient(Value: Single);
begin
  if Abs(FLowerBoundCoefficient-Value)<1e-37 then Exit;
  FLowerBoundCoefficient := Value;
  if FDisplayFormat=dfTable then FullInvalidate;
end;

procedure TTimeseriesGrid.SetStatisticsVisible(Value: Boolean);
begin
  if FStatisticsVisible=Value then Exit;
  FStatisticsVisible := Value;
  if FDisplayFormat=dfTable then FullInvalidate;
end;

procedure TTimeseriesGrid.SetUpperBoundCoefficient(Value: Single);
begin
  if Abs(FUpperBoundCoefficient-Value)<1e-37 then Exit;
  FUpperBoundCoefficient := Value;
  if FDisplayFormat = dfTable then FullInvalidate;
end;

function TTimeseriesGrid.GetHydrologicalYear: Boolean;
begin
  Result := FNewHydrologicalYear;
  { For an explanation, see the implementation of
    TTimeseriesGrid.GetDisplayFormat. }
end;

procedure TTimeseriesGrid.SetHydrologicalYear(Value: Boolean);
var ADate: TDateTime;
begin
  if FHydrologicalYear = Value then Exit;
  FNewHydrologicalYear := Value;
  if (FDisplayFormat=dfTable) and (ActiveIndex>=0) and
  (ActiveTimeseries.TimeStep.TimeStepIn([tstDaily, tstMonthly])) then
  begin
    if ActiveDate=idaEmpty then ADate := FBaseDate else ADate := ActiveDate;
    if Value then
      FNewBaseDate := (EncodeDate(FindHydrologicalYear(ADate, FHYearOrigin),
        FHYearOrigin, 1))
    else
      FNewBaseDate := EncodeDate(YearOf(ADate), 1, 1);
    FullInvalidate;
  end else
    FHydrologicalYear := FNewHydrologicalYear;
end;

function TTimeseriesGrid.GetActiveDate: TDateTime;
var Year, Month, Day: Word;
begin
  Result := idaEmpty;
  if ActiveIndex=-1 then Exit;
  if FDisplayFormat = dfSimple then
  begin
    if (Row>0) and (Length(FDates)>0) then
      Result := FDates[Row-1].DisplayDate
    else Exit;
  end else if ActiveTimeseries.TimeStep = tstHourly then
  begin
    DecodeDate(FBaseDate, Year, Month, Day);
    if (Row<=0) or (Row>MonthDays[IsLeapYear(Year), Month]) then Exit;
    Result := AddDateTime(EncodeDate(Year, Month, 1), (Row-1)+(Col-1)/24);
    Result := AddDateTime(Result, ActiveTimeseries.DateOffset);
  end else if ActiveTimeseries.TimeStep = tstDaily then
  begin
    DecodeDate(IncMonth(FBaseDate, Col-1), Year, Month, Day);
    if (Row<=0) or (Col<=0) or (Col>12) or
    (Row>MonthDays[IsLeapYear(Year),Month]) then Exit;
    Result := AddDateTime(IncMonth(FBaseDate, Col-1),
      (Row-1)+ActiveTimeseries.DateOffset)
  end else if ActiveTimeseries.TimeStep = tstMonthly then
  begin
    if (Col<=0) or (Col>12) or (Row<=0) then Exit;
    if FHydrologicalYear then
      Result := EncodeDate(FindHydrologicalYear(ActiveTimeseries[0].Date,
        FHYearOrigin),FHYearOrigin, 1)
    else
      Result := EncodeDate(YearOf(ActiveTimeseries[0].Date), 1, 1);
    Result := IncMonth(Result, (Col-1)+12*(Row-1));
    if ActiveTimeseries.IndexOf(Result)=-1 then Result := idaEmpty;
  end else
    Assert(False);
end;

procedure TTimeseriesGrid.SetActiveDate(Value: TDateTime);
var
  i: Integer;
  Year, Month, Day, Hour, Min, Sec, MSec: Word;
  r, c: Integer;
begin
  if FDisplayFormat = dfSimple then
  begin
    i := Length(FDates)-1;
    while i>=0 do
    begin
      if DiffInSecs(FDates[i].DisplayDate, Value)=0 then Break;
      Dec(i);
    end;
    if i=-1 then i := 0;
    if i+1>RowCount-1 then Row := RowCount-1 else Row := i+1;
    Exit;
  end;
  if ActiveIndex<0 then Exit;
  BaseDate := Value;
  DecodeDate(Value, Year, Month, Day);
  DecodeTime(Value, Hour, Min, Sec, MSec);
//  case ActiveTimeseries.TimeStep of
    if ActiveTimeseries.TimeStep=tstHourly then
      begin
        Row := Day;
        Col := Hour+1;
      end
    else if ActiveTimeseries.TimeStep=tstDaily then
      begin
        Row := Day;
        if not FHydrologicalYear then Col := Month
        else if Month>=FHYearOrigin{10} then Col := Month-(FHYearOrigin-1){9}
        else Col := Month+(12-FHYearOrigin+1){3};
      end
    else if ActiveTimeseries.TimeStep=tstMonthly then
      begin
        if not FHydrologicalYear then
        begin
          r := Year-YearOf(ActiveTimeseries[0].Date)+1;
          c := Month;
        end else
        begin
          r := FindHydrologicalYear(Value)-
            FindHydrologicalYear(ActiveTimeseries[0].Date)+1;
          if Month>=FHYearOrigin{10} then c := Month-(FHYearOrigin-1){9}
          else c := Month+(12-FHYearOrigin+1){3};
        end;
        if r<1 then r := 1;
        if c<1 then c := 1;
        if r>=RowCount then r := RowCount-1;
        if c>=ColCount then c := ColCount-1;
        Row := r;
        Col := c;
      end
  else
    Assert(False);
//  end;
end;

resourcestring
  rsBaseDateInaccessible =
    'BaseDate is inaccessible for this display format and time step.';

function TTimeseriesGrid.GetBaseDate: TDateTime;
begin
  if (DisplayFormat<>dfTable) or (ActiveIndex=-1) or
  (not ActiveTimeseries.TimeStep.TimeStepIn([tstHourly, tstDaily])) then
    raise Exception.Create(rsBaseDateInaccessible);

  Result := FNewBaseDate;
  { See GetDisplayFormat for an explanation. }
end;

procedure TTimeseriesGrid.SetBaseDate(Value: TDateTime);
var d: TDateTime;
begin
  if (DisplayFormat<>dfTable) or (ActiveIndex=-1) or
  (not ActiveTimeseries.TimeStep.TimeStepIn([tstHourly, tstDaily])) then
    raise Exception.Create(rsBaseDateInaccessible);

  d := idaEmpty;
  if ActiveTimeseries.TimeStep=tstHourly then
    d := EncodeDate(YearOf(Value), MonthOf(Value), 1)
  else if ActiveTimeseries.TimeStep=tstDaily then
    if FHydrologicalYear then
      d := EncodeDate(FindHydrologicalYear(Value, FHYearOrigin),
        FHYearOrigin, 1)
    else
      d := EncodeDate(YearOf(Value), 1, 1);
  if (d<>idaEmpty) and (DiffInSecs(d, FBaseDate)<>0) then
  begin
    FNewBaseDate := d;
    FullInvalidate;
  end;
end;

function TTimeseriesGrid.GetDisplayFormat: TDisplayFormat;
begin
  Result := FNewDisplayFormat;
  { FNewDisplayFormat is the same as FDisplayFormat, unless the DisplayFormat
    has just been changed and reformatting has not completed. When changing
    DisplayFormat, FDisplayFormat remains as it was, and FNewDisplayFormat is
    set to the new value. Thus, while reformatting, it is known from where we
    come from (because it is needed, for example, to run GetActiveDate as it
    was in the old format, in order to activate the correct cell in the new
    format). Routines using TTimeseriesGrid, if changing DisplayFormat, and then
    try to access it before reformatting occurs, should get the value they
    assigned, thus this function always returns FNewDisplayFormat. Routines
    internal to TTimeseriesGrid should always use FDisplayFormat, as this is
    the actual format until reformatting ends; the Reformat method also uses
    other internal routines and expects them to think we're still in the old
    format. }
end;

resourcestring
  rsInvalidTimeStepForDfTable = 'Cannot display table for this time step.';

procedure TTimeseriesGrid.SetDisplayFormat(Value: TDisplayFormat);
begin
  if FDisplayFormat = Value then Exit;
  EditorMode := False;
  if (Value=dfTable) and (ActiveIndex>=0) and
  (not (ActiveTimeseries.TimeStep.TimeStepIn([tstHourly, tstDaily, tstMonthly]))) then
    raise Exception.Create(rsInvalidTimeStepForDfTable);
  { For an explanation of the following, see TTimeseriesGrid.GetActiveIndex }
  FActiveIndex := ActiveIndex;
  if (Value=dfTable) and (ActiveIndex>=0) then
    if ActiveTimeseries.TimeStep=tstHourly then
      FNewBaseDate := EncodeDate(YearOf(ActiveDate), MonthOf(ActiveDate), 1)
    else if ActiveTimeseries.TimeStep=tstDaily then
      if FNewHydrologicalYear then
        FNewBaseDate := EncodeDate(FindHydrologicalYear(ActiveDate,
          FHYearOrigin), FHYearOrigin, 1)
      else
        FNewBaseDate := EncodeDate(YearOf(ActiveDate), 1, 1);
  FNewDisplayFormat := Value;
  FullInvalidate;
end;

procedure TTimeseriesGrid.SetFlagsVisible(Value: Boolean);
begin
  if FFlagsVisible = Value then Exit;
  FFlagsVisible := Value;
  FullInvalidate;
end;

function TTimeseriesGrid.Add(Item: TTimeseries): Integer;
var
  ATsGridSingleUndoBuffer: TTsGridSingleUndoBuffer;
begin
  Result := FData.Add(Item);
  ATsGridSingleUndoBuffer := nil;
  try
    ATsGridSingleUndoBuffer := TTsGridSingleUndoBuffer.Create(True);
    ATsGridSingleUndoBuffer.UndoPointer := 0;
    FUndoBuffers.Add(ATsGridSingleUndoBuffer);
    ATsGridSingleUndoBuffer := nil;
  finally
    ATsGridSingleUndoBuffer.Free;
  end;
  Item.AddInvalidator(InvalidateMethod);
  Item.AddFullInvalidator(FullInvalidateMethod);
  if Count=1 then
  begin
    FDisplayFormat := dfSimple;// Revert to simple when loading first timeseries.
    FNewDisplayFormat := dfSimple;
  end;
  FullInvalidate;
end;

procedure TTimeseriesGrid.Clear;
var
  i: Integer;
  ts: TTimeseries;
begin
  Assert(FData.Count = FUndoBuffers.Count);
  for i := 0 to FData.Count do
  begin
    ts := TTimeseries(FData[i]);
    ts.RemoveInvalidator(InvalidateMethod);
    ts.RemoveFullInvalidator(FullInvalidateMethod);
    FData.Delete(i);
    FUndoBuffers.Delete(i);
  end;
  FData.Clear;
  FUndoBuffers.Clear;
  FullInvalidate;
end;

procedure TTimeseriesGrid.Delete(Index: Integer);
var
  ts: TTimeseries;
begin
  Assert(FData.Count = FUndoBuffers.Count);
  ts := TTimeseries(FData[Index]);
  ts.RemoveInvalidator(InvalidateMethod);
  ts.RemoveFullInvalidator(FullInvalidateMethod);
  FData.Delete(Index);
  FUndoBuffers.Delete(Index);
  if FActiveIndex>=Index then Dec(FActiveIndex);
  if FActiveIndex<0 then FActiveIndex := Count-1;
  FullInvalidate;
end;

function TTimeseriesGrid.Extract(Timeseries: TTimeseries):
  TTimeseries;
begin
  Result := TTimeseries(FData.Extract(Timeseries));
end;

resourcestring
  rsNoTimeseries = 'No time series is loaded on the grid.';

function TTimeseriesGrid.First: TTimeseries;
begin
  if Count<=0 then raise EListError.Create(rsNoTimeseries);
  Result := Data[0];
end;

function TTimeseriesGrid.IndexOf(Timeseries: TTimeseries): Integer;
begin
  Result := FData.IndexOf(Timeseries);
end;

procedure TTimeseriesGrid.Insert(Index: Integer; Timeseries: TTimeseries);
var
  ATsGridSingleUndoBuffer: TTsGridSingleUndoBuffer;
begin
  FData.Insert(Index, Timeseries);
  ATsGridSingleUndoBuffer := nil;
  try
    ATsGridSingleUndoBuffer := TTsGridSingleUndoBuffer.Create(True);
    ATsGridSingleUndoBuffer.UndoPointer := 0;
    FUndoBuffers.Insert(Index, ATsGridSingleUndoBuffer);
    ATsGridSingleUndoBuffer := nil;
  finally
    ATsGridSingleUndoBuffer.Free;
  end;
  Timeseries.AddInvalidator(InvalidateMethod);
  Timeseries.AddFullInvalidator(FullInvalidateMethod);
  if Count=1 then
    DisplayFormat := dfSimple;// Revert to simple when loading first timeseries.
  FullInvalidate;
end;

function TTimeseriesGrid.Last: TTimeseries;
begin
  if Count<=0 then raise EListError.Create(rsNoTimeseries);
  Result := Data[Count-1];
end;

procedure TTimeseriesGrid.Move(CurIndex, NewIndex: Integer);
begin
  Assert(FData.Count = FUndoBuffers.Count);
  if CurIndex=NewIndex then Exit;
  FData.Move(CurIndex, NewIndex);
  FUndoBuffers.Move(CurIndex, NewIndex);
  FullInvalidate;
  if DisplayFormat=dfSimple then ColumnMoved(CurIndex, NewIndex);
end;

function TTimeseriesGrid.Remove(Timeseries: TTimeseries): Integer;
begin
  Result := IndexOf(Timeseries);
  Delete(Result);
end;

function TTimeseriesGrid.Get(Index: Integer): TTimeseries;
begin
  Result := TTimeseries(FData[Index]);
end;

procedure TTimeseriesGrid.Put(Index: Integer; Item: TTimeseries);
var ts: TTimeseries;
begin
  Assert(Index<=Count);
  Assert(FData.Count = FUndoBuffers.Count);
  if Index<Count then
  begin
    ts := TTimeseries(FData[Index]);
    ts.RemoveInvalidator(InvalidateMethod);
    ts.RemoveFullInvalidator(FullInvalidateMethod);
  end else if Count=0 then
    DisplayFormat := dfSimple;
  FData[Index] := Item;
  TTsGridSingleUndoBuffer(FUndoBuffers[Index]).Clear;
  TTsGridSingleUndoBuffer(FUndoBuffers[Index]).UndoPointer := 0;
  Item.AddInvalidator(InvalidateMethod);
  Item.AddFullInvalidator(FullInvalidateMethod);
  FullInvalidate;
end;

procedure TTimeseriesGrid.DrawCell(ACol, ARow: Longint; ARect: TRect;
  AState: TGridDrawState);
begin
  if FIsReformatting then Exit;
  if Assigned(FOnDrawCell) then FOnDrawCell(Self, ACol, ARow, ARect, AState);
  if FFullyInvalidated then
    Reformat;
  if DefaultDrawing then
    if FDisplayFormat = dfSimple then DrawCellSimple(ACol, ARow, ARect)
    else if FDisplayFormat = dfTable then DrawCellTable(ACol, ARow, ARect)
    else Assert(False);
  if gdSelected	in AState then
  begin
    canvas.brush.color := ODSTRGRDDEFAULTSELECTEDCELLCOLOR;
    canvas.font.color  := ODSTRGRDDEFAULTSELECTEDFONTCOLOR;
  end
  else canvas.font.color  := font.color;
  if FDisplayFormat = dfSimple then
    if ACol = 0 then
      ColWidths[0] := FFirstColumnDefaultWidth;
end;

{ Procedures SaveFont, RestoreFont, AdjustFont.
  AdjustFont adjusts Canvas.Font according to TsRecord.MStatus and the settings
  of FontForModified, FontForNew, FontForUnmodified.
  SaveFont saves the current properties of Canvas.Font in a temporary private
  object, FSavedFont. RestoreFont restores them. Usually those functions are
  used in the various DrawCell functions. SaveFont is called
  first, then AdjustFont, then ExtTextOut, then RestoreFont. }

procedure TTimeseriesGrid.SaveFont;
begin
  FSavedFont.Assign(Canvas.Font);
end;

procedure TTimeseriesGrid.RestoreFont;
begin
  Canvas.Font.Assign(FSavedFont);
end;

{ Of the two overloaded AdjustFont functions, the first one adjusts font for
  dfSimple and for a specified time series record. The second one works for
  all display formats; for a specific row and column, it calls the first one
  only if needed.
}
procedure TTimeseriesGrid.AdjustFont(TsRecord: TTsRecord);
begin
  case TsRecord.MStatus of
    msUnmodified: Canvas.Font.Style := FontForUnmodified.Style;
    msModified: Canvas.Font.Style := FontForModified.Style;
    msNew: Canvas.Font.Style := FontForNew.Style;
  else
    Assert(False);
  end;
end;

procedure TTimeseriesGrid.AdjustFont(ACol, ARow: Longint);
var
  CurrentTimeseries: TTimeseries;
  CurrentIndex: Integer;
begin
  if FDisplayFormat = dfTable then Exit;
  if (ACol=0) or (ARow<>0) then Exit;
  CurrentTimeseries := TTimeseries(Data[GetIndex(ACol)]);
  CurrentIndex := CurrentTimeseries.SortingIndexOf(FDates[ARow-1].SortingDate,
    FUseDateOffset);
  if CurrentIndex<>-1 then
    AdjustFont(CurrentTimeseries[CurrentIndex]);
end;

procedure TTimeseriesGrid.CopyToClipboard(ARect: TGridRect);
var
  i, j: Integer;
  ToClipboard: string;
  ATsRecord: TTsRecord;
begin
  ToClipboard := '';
  for i := ARect.Top to ARect.Bottom do
  begin
    for j := ARect.Left to ARect.Right do
    begin
      ToClipboard := ToClipboard + GetCellText(j, i, ATsRecord);
      if j<>ARect.Right then ToClipboard := ToClipboard+#9;
    end;
    if i<>ARect.Bottom then ToClipboard := ToClipboard+#13;
  end;
  Clipboard.AsText := ToClipboard;
end;

procedure TTimeseriesGrid.CopyToClipboardWithDates(ARect: TGridRect);
var
  i, j: Integer;
  AStringList: TStringList;
  s, ToClipboard: string;
  ATsRecord: TTsRecord;
  OnlyOne: Boolean;
begin
  Assert(ARect.Left>0);
  if FDisplayFormat=dfSimple then
  begin
    OnlyOne := (GetIndex(ARect.Left)=GetIndex(ARect.Right));
    if OnlyOne then
      if FlagsVisible then
      begin
        ARect.Left := GetIndex(ARect.Left)*2+1;
        ARect.Right := ARect.Left+1;
      end else
      begin
        ARect.Left := GetIndex(ARect.Left)+1;
        ARect.Right := ARect.Left;
      end;
    AStringList := nil;
    try
      AStringList := TStringList.Create;
      for i := ARect.Top to ARect.Bottom do
      begin
        s := GetCellText(0, i, ATsRecord, ddfSimple) + #9;
        for j := ARect.Left to ARect.Right do
        begin
          s := s + GetCellText(j, i, ATsRecord);
          if j<>ARect.Right then s := s+#9;
        end;
        if OnlyOne and (not FlagsVisible) and (ATsRecord<>nil) then
          s := s+#9+ATsRecord.GetAllFlags;
        if (ATsRecord<>nil) or (not OnlyOne) then AStringList.Add(s);
      end;
      Clipboard.AsText := AStringList.Text;
    finally
      AStringList.Free;
    end;
  end else if FDisplayFormat = dfTable then
  begin
    ToClipboard := #9;
    for j := ARect.Left to ARect.Right do
    begin
      ToClipboard := ToClipboard + GetCellText(j, 0, ATsRecord);
      if j<>ARect.Right then ToClipboard := ToClipboard+#9;
    end;
    ToClipboard := ToClipboard+#13;
    for i := ARect.Top to ARect.Bottom do
    begin
      ToClipboard := ToClipboard + GetCellText(0, i, ATsRecord) + #9;
      for j := ARect.Left to ARect.Right do
      begin
        ToClipboard := ToClipboard + GetCellText(j, i, ATsRecord);
        if j<>ARect.Right then ToClipboard := ToClipboard+#9;
      end;
      if i<>ARect.Bottom then ToClipboard := ToClipboard+#13;
    end;
    Clipboard.AsText := ToClipboard;
  end else
    Assert(False);
end;

resourcestring
//  rsPastedAreaMustBeRectangular = 'All rows must have the same number of '+
//    'columns when pasting.';
  rsSelectionSizeDoesNotMatchClipboardText = 'Either the size of the '+
    'selection does not match the size of text in the clipboard, or pasting '+
    'the clipboard would exceed the limits of the grid.';
  rsPasteDatesInDfSimpleOnly = 'Pasting dates is only allowed in simple '+
    'display format.';
  rsPasteDatesIncorrectColumns = 'When pasting dates, either dates only may '+
    'be pasted, or dates plus values, or, if flags are shown, dates plus '+
    'values plus flags.';
  rsSelectionMustBeOneCellWhenPastingDates = 'When pasting dates, one cell '+
    'only must be selected.';
  rsDatesAreNotConsecutive = 'When pasting dates, all dates must be '+
    'consecutive.';
  rsCantPasteToNowhere = 'Cannot paste to nonexistent cells (maybe you '+
    'selected "Copy" instead of "Copy with dates"?)';
  rsCanOnlyPasteInSimpleDisplayFormat = 'You cannot paste in this display '+
    'format. You can only paste in simple display format.';
  rsOverwriteConfirmation = 'This paste operation will overwrite some records '+
    'of the time series. Press OK to proceed or Cancel to abort paste operation';
  rsPaste = 'Paste';
  rsPasteSelectionContainsNoexistent =
    'Cannot paste since paste selection contains noexistent cells '+
      '(these with diagonal line). Proceed with closing some time series '+
      'with different time step.';

function TTimeseriesGrid.PasteFromClipboard(ARect: TGridRect): TGridRect;

var DateFormat: string; { Automatically determined date format when pasting
                          dates. }

  procedure ParseClipboard(CellsToPaste: TObjectList);
  var
    ClipboardStrings: TStringList;
    PasteColCount: Integer;
    i, j: Integer;
  begin
    CellsToPaste.Clear;
    ClipboardStrings := nil;
    try
      ClipboardStrings := TStringList.Create;
      ClipboardStrings.Text := Clipboard.AsText;
      if ClipboardStrings.Count<1 then Exit;
      PasteColCount := DelimitedStringCount(ClipboardStrings[0], #9);
      for i := 0 to ClipboardStrings.Count -1 do
      begin
        PasteColCount := Max(PasteColCount,
          DelimitedStringCount(ClipboardStrings[i], #9));
      end;
      for i := 0 to ClipboardStrings.Count-1 do
      begin
        if PasteColCount<>DelimitedStringCount(ClipboardStrings[i], #9) then
        begin
          for j := DelimitedStringCount(ClipboardStrings[i], #9) to PasteColCount-1 do
          begin
            ClipboardStrings[i] := ClipboardStrings[i] + #9;
          end;
        end;
        CellsToPaste.Add(TStringList.Create);
        for j := 1 to PasteColCount do
          TStringList(CellsToPaste.Last).Add
            (DelimitedStringItem(ClipboardStrings[i], j, #9));
      end;
    finally
      ClipboardStrings.Free;
    end;
  end;

  function PasteWithoutDates(ARect: TGridRect; CellsToPaste: TObjectList): TGridRect;
  var
    RectWidth, RectHeight: Integer;
    i, j: Integer;
    ACol, ARow: Integer;
  begin
    RectWidth := ARect.Right-ARect.Left+1;
    RectHeight := ARect.Bottom-ARect.Top+1;
    IncUndoIDPointer;
    if (RectWidth=1) and (RectHeight=1) then
    begin
      ARect.Right := Min(ARect.Left+TStringList(CellsToPaste[0]).Count-1,
        ColCount-1);
      ARect.Bottom := Min(ARect.Top+CellsToPaste.Count-1, RowCount-1);
      RectWidth := ARect.Right-ARect.Left+1;
      RectHeight := ARect.Bottom-ARect.Top+1;
    end;
    if (RectWidth<>TStringList(CellsToPaste[0]).Count)
    or (RectHeight<>CellsToPaste.Count) then
      raise Exception.Create(rsSelectionSizeDoesNotMatchClipboardText);
    for i := 0 to RectWidth-1 do
    begin
      ACol := ARect.Left+i;
      try
        if not FCheckOnly then
          PrepareBuffer(GetIndex(ACol), SelectedDate(ARect.Top-1),
            SelectedDate(ARect.Top+RectHeight-2), rsPaste, FUndoIDPointer);
        for j := 0 to RectHeight-1 do
        begin
          ARow := ARect.Top+j;
          if ARow < 1 then
            raise Exception.Create(rsCantPasteToNowhere);
          if TTimeseries(Data[GetIndex(ACol)]).IndexOf(FDates[ARow-1].DisplayDate)
          < 0 then
            raise Exception.Create(rsCantPasteToNowhere);
          if TTimeseries(Data[GetIndex(ACol)]).SortingIndexOf(FDates[
            ARow-1].SortingDate, FUseDateOffset)<0 then
            raise Exception.Create(rsPasteSelectionContainsNoexistent);
          try
            SetEditText(ACol, ARow, TStringList(CellsToPaste[j])[i]);
          except
            on EConvertError do
              SetEditText(ACol, ARow, '');
            else
              raise;
          end;
        end;
        if not FCheckOnly then FinalizeBuffer(GetIndex(ACol));
      except
        RevertBuffer(GetIndex(ACol));
        raise;
      end;
    end;
    Result := ARect;
  end;

  function PasteWithDates(ARect: TGridRect; CellsToPaste: TObjectList): TGridRect;
  var
    RectWidth, RectHeight: Integer;
    ColsToPaste: Word;
    Index: Integer;
    DateList: TDateTimeList;
    i, j: Integer;
    d1, d2: TDateTime;
    TempTimeseries, TargetTimeseries: TTimeseries;

    procedure CheckDates(Timeseries: TTimeseries; DateList: TDateTimeList);
    var
      i: Integer;
      FirstRecord, LastRecord: Integer;
    begin
      FirstRecord := Timeseries.PositionOfNext(DateList.First);
      if FirstRecord=-1 then Exit;
      LastRecord := Timeseries.PositionOfPrevious(DateList.Last);
      if LastRecord=-1 then Exit;
      for i := FirstRecord to LastRecord do
        if DateList.IndexOf(Timeseries[i].Date)=-1 then
          raise Exception.Create(rsDatesAreNotConsecutive);
    end;

    function StringToDate(S: string): TDateTime;
    var
      AHYearOrigin: Integer;
    begin
      if DateFormat<>'aaaa-bb' then
        Result := FormatStrToDateTime(DateFormat, S)
      else
      begin
        AHYearOrigin := FHYearOrigin;
        if (ActiveTimeseries<>nil) and
          (ActiveTimeseries.TimeStep=tstAnnual) then
          AHYearOrigin:= ActiveTimeseries.NominalOffset.Months+1;
        Result := HYearToDate(S, AHYearOrigin);
      end;
    end;

  begin
    if FDisplayFormat<>dfSimple then
      raise Exception.Create(rsPasteDatesInDfSimpleOnly);
    ColsToPaste := TStringList(CellsToPaste[0]).Count;
    if (ColsToPaste<1) or (ColsToPaste>3) then
      raise Exception.Create(rsPasteDatesIncorrectColumns);
    RectWidth := ARect.Right-ARect.Left+1;
    RectHeight := ARect.Bottom-ARect.Top+1;
    if (RectWidth<>1) or (RectHeight<>1) then
      raise Exception.Create(rsSelectionMustBeOneCellWhenPastingDates);
    Index := GetIndex(ARect.Left);
    DateList := nil;
    TempTimeseries := nil;
    Inc(FUndoIDPointer);
    try try
      TempTimeseries := TTimeseries.Create;
      TempTimeseries.Assign(Data[Index]);
      if FCheckOnly then TargetTimeseries := TempTimeseries
      else TargetTimeseries := Data[Index];
      DateList := TDateTimeList.Create;
      for i := 0 to CellsToPaste.Count-1 do
        DateList.Add(StringToDate(TStringList(CellsToPaste[i])[0]));
      if not FCheckOnly then
        PrepareBuffer(Index, DateList.First, DateList.Last, rsPaste,
          FUndoIDPointer);
      CheckDates(TargetTimeseries, DateList);
      for i := 0 to CellsToPaste.Count-1 do
      begin
        j := TargetTimeseries.IndexOf(DateList[i]);
        if j=-1 then
          j := TargetTimeseries.Insert(DateList[i], True, 0, '', msNew)
        else
          FOverwriteDates := True;
        with TStringList(CellsToPaste[i]) do
        begin
          if Count>1 then
            if Strings[1]='' then TargetTimeseries[j].SetNull
            else begin
              try
                TargetTimeseries[j].AsFloat := StrToFloat(Strings[1]);
              except
                on EConvertError do
                  TargetTimeseries[j].SetNull;
                else
                  raise;
              end;
            end;
          if Count>2 then
            TargetTimeseries[j].SetAllFlags(Strings[2]);
        end;
      end;
      if not FCheckOnly then
      begin
        { Find correct value to return. }
        if FlagsVisible and not Odd(ARect.Left) then Dec(ARect.Left);
        ARect.Right := ARect.Left;
        if ColsToPaste>2 then Inc(ARect.Right);
        ARect.Right := Min(ARect.Right, ColCount-1);
        Reformat;
        with ActiveTimeseries do
          if IndexOf(DateList.First)>-1 then
            d1 := Items[IndexOf(DateList.First)].SortingDate(FUseDateOffset)
          else
            d1 := idaEmpty;
        with ActiveTimeseries do
          if IndexOf(DateList.Last)>-1 then
            d2 := Items[IndexOf(DateList.Last)].SortingDate(FUseDateOffset)
          else
            d2 := idaEmpty;
        for i := Length(FDates)-1 downto 0 do
        begin
          if DiffInSecs(FDates[i].SortingDate, d1)=0 then ARect.Top := i+1;
          if DiffInSecs(FDates[i].SortingDate, d2)=0 then ARect.Bottom := i+1;
        end;
        Result := ARect;
        FinalizeBuffer(Index);
      end;
    finally
      TempTimeseries.Free;
      DateList.Free;
    end;
    except
      if not FCheckOnly then RevertBuffer(Index);
      raise;
    end;
  end;

var
  CellsToPaste: TObjectList; { Holds list of one TStringList per row }
  SavedShortDateFormat: string;
begin
  if DisplayFormat<>dfSimple then
    raise Exception.Create(rsCanOnlyPasteInSimpleDisplayFormat);
  CellsToPaste := nil;
  EditorMode := False;
  SavedShortDateFormat := ShortDateFormat;
  try
    if (StrIComp(PChar(ShortDateFormat),PChar('d/m/yyyy'))=0) or
      (StrIComp(PChar(ShortDateFormat),PChar('d-m-yyyy'))=0) then
      ShortDateFormat := 'dd/mm/yyyy';
    if (StrIComp(PChar(ShortDateFormat),PChar('m/d/yyyy'))=0) or
      (StrIComp(PChar(ShortDateFormat),PChar('m-d-yyyy'))=0) then
      ShortDateFormat := 'mm/dd/yyyy';
    CellsToPaste := TObjectList.Create(True);
    ParseClipboard(CellsToPaste); { Parse clipboard into CellsToPaste. }
    if TStringList(CellsToPaste.First).Count > 0 then
      DateFormat := GetDateFormat(TStringList(CellsToPaste.First)[0],
        [gdfAllowHydrologicalYear])
    else
      exit;
    if DateFormat = 'yyyy' then
      if (DisplayFormat<>dfSimple)
      or (Selection.Top<>Selection.Bottom) or (Selection.Left<>Selection.Right)
      or (ActiveTimeseries.TimeStep<>tstAnnual)
      or (ActiveTimeseries.HydrologicalYear) then
        DateFormat := '';
    if DateFormat<>'' then
    begin
      FCheckOnly := True;
      FOverwriteDates := False;
      PasteWithDates(ARect, CellsToPaste);
      FCheckOnly := False;
      if FOverwriteDates then
      begin
        if MessageDlg(rsOverwriteConfirmation,mtConfirmation,mbOkCancel,0)=mrOK then
          Result := PasteWithDates(ARect, CellsToPaste) else
          Result := ARect;
      end else
          Result := PasteWithDates(ARect, CellsToPaste);
    end else
    begin
      FCheckOnly := True;
      PasteWithoutDates(ARect, CellsToPaste);
      FCheckOnly := False;
      Result := PasteWithoutDates(ARect, CellsToPaste);
    end;
  finally
    ShortDateFormat := SavedShortDateFormat;
    CellsToPaste.Free;
    FCheckOnly := False;
  end;
end;

resourcestring
  rsTimeSeriesMustBeMonthly = 'Time series must be of monthly time step';
  rsMustBeTwelveColums = 'Data to copy must be arranged in 12 colums';
  rsInputDesiredYear = 'Input year';
  rsInputDesiredYearExplained =
    'Input the desired year to start copy data';
  rsIsHydrologicalYear =
    'Press Yes to use the Hydrological Year, No for common year, Cancel aborts';
  rsARecortExistsOverwrite =
    'Records with same dates as the data copied exists. Press Yes to replace'+
    ' data or No to keep original data';

procedure TTimeseriesGrid.PasteMonthlyTable;
var
  ClipboardStrings: TStringList;
  countCols, countRows: Integer;
  ATimeSeries: TTimeseries;
  ARange: TIndexRange;
  AYear: Integer;
  ADate: TDateTime;
  i,j,AIndex: Integer;
  FlagReplaceData: Boolean;
  FlagReplaceDataQuestion: Boolean;
  AString: string;
begin
{Dummy initializations}
  AIndex := 0;
  FlagReplaceData := False;
  FlagReplaceDataQuestion := True;
{}
  ATimeSeries := ActiveTimeseries;
  if ATimeSeries.TimeStep<>tstMonthly then
    raise Exception.Create(rsTimeSeriesMustBeMonthly);
  ClipboardStrings := TStringList.Create;
  ClipboardStrings.Text := Clipboard.AsText;
  if ClipboardStrings.Count<1 then Exit;
  countCols := DelimitedStringCount(ClipboardStrings[0], #9);
  countRows := ClipboardStrings.Count;
  if CountCols<>12 then
    raise Exception.Create(rsMustBeTwelveColums);
  if not InputQuery(rsInputDesiredYear, rsInputDesiredYearExplained, AString) then Exit;
  AYear := StrToInt(Astring);
  ADate := EncodeDate(AYear,FHYearOrigin,1);
  case MessageDlg(rsIsHydrologicalYear,mtConfirmation,mbYesNoCancel,0) of
{   mrYes:
 Don't handle mrYes, default value considered}
    mrNo:
      ADate := EncodeDate(AYear,1,1);
    mrCancel:
      Exit;
  end;
  IncUndoIDPointer;
  PrepareBuffer(ActiveIndex, ADate, IncMonth(ADate, countRows*12-1),
    rsPaste, FUndoIDPointer);
  for i := 0 to countRows - 1 do
  begin
    for j := 0 to countCols -1 do
    begin
      try try
        AIndex := ATimeSeries.Insert(ADate, True, 0, '', msNew);
        try
          ATimeSeries.Items[AIndex].AsFloat :=
            StrToFloat(DelimitedStringItem(ClipboardStrings[i],j+1, #9));
        except
          on EConvertError do
            ATimeSeries.Items[AIndex].SetNull;
          else
            raise;
        end;
      except
        on ERecordAlreadyExists do
        begin
          if FlagReplaceDataQuestion then
          begin
            case MessageDlg(rsARecortExistsOverwrite,mtConfirmation,[mbYes,mbNo],0) of
              mrYes:
                FlagReplaceData := True;
              mrNo:
                FlagReplaceData := False;
            end;
          end;
          FlagReplaceDataQuestion := False;
          if FlagReplaceData then
          begin
            AIndex := ATimeSeries.IndexOf(ADate);
            try
              ATimeSeries.Items[AIndex].AsFloat :=
                StrToFloat(DelimitedStringItem(ClipboardStrings[i],j+1, #9));
            except
              on EConvertError do
                ATimeSeries.Items[AIndex].SetNull;
              else
                raise;
            end;
          end;
        end
        else
        begin
          RevertBuffer(ActiveIndex);
          raise;
        end;
      end;
      finally
        ADate := IncMonth(ADate,1);
        if (i=0) and (j=0) then ARange.StartIndex := AIndex;
      end;
    end;
  end;
  FinalizeBuffer(ActiveIndex);
  ARange.EndIndex := AIndex;
  SelectionRange := ARange;
end;

resourcestring
  rsTimeSeriesMustBeDaily = 'Time series must be of daily time step';
  rsEnterFirstColumnMonth = 'Enter the month corresponding to the first '+
    'column';
  rsMustBeTwelveXThirty = 'Data to copy must be arranged in 12 colums and '+
    '31 rows';
  rsEnterDesiredTimeOffset ='Enter desired time offset in HH:MM form '+
    '(neg. values not accepted)';

procedure TTimeseriesGrid.PasteDailyTable;
var
  ClipboardStrings: TStringList;
  countCols, countRows: Integer;
  ATimeSeries: TTimeseries;
  ARange: TIndexRange;
  AYear,AMonth, ADay: Word;
  ADate: TDateTime;
  i,j,AIndex: Integer;
  FlagReplaceData: Boolean;
  FlagReplaceDataQuestion: Boolean;
  AString: string;
begin
{Dummy initializations}
  AString := '';
  AIndex := 0;
  FlagReplaceData := False;
  FlagReplaceDataQuestion := True;
{}
  ATimeSeries := ActiveTimeseries;
  if ATimeSeries.TimeStep<>tstDaily then
    raise Exception.Create(rsTimeSeriesMustBeDaily);
  ClipboardStrings := TStringList.Create;
  ClipboardStrings.Text := Clipboard.AsText;
  if ClipboardStrings.Count<1 then Exit;
  countCols := DelimitedStringCount(ClipboardStrings[0], #9);
  countRows := ClipboardStrings.Count;
  if (CountCols<>12) or (CountRows<>31) then
    raise Exception.Create(rsMustBeTwelveXThirty);
  if not InputQuery(rsInputDesiredYear, rsInputDesiredYearExplained, AString)
    then Exit;
  AYear := StrToInt(Astring);
  ASTring := '';
  if not InputQuery(rsEnterFirstColumnMonth, rsEnterFirstColumnMonth, Astring)
    then Exit;
  AMonth := StrToInt(AString);
  ADate := EncodeDate(AYear,AMonth,1);
  AString := '';
  if not InputQuery(rsEnterDesiredTimeOffset, rsEnterDesiredTimeOffset, AString)
    then Exit;
  ADate := AddDateTime(ADate, StrToTime(AString));
  IncUndoIDPointer;
  PrepareBuffer(ActiveIndex, ADate, SubtractDateTime(IncYear(ADate), 1),
    rsPaste, FUndoIDPointer);
  for j := 0 to countCols -1 do
  begin
    DecodeDate(ADate, AYear, AMonth, ADay);
    for i := 0 to MonthDays[IsLeapYear(AYear), AMonth]-1 do
    begin
      try try
        AIndex := ATimeSeries.Insert(ADate, True, 0, '', msNew);
        try
          ATimeSeries.Items[AIndex].AsFloat :=
            StrToFloat(DelimitedStringItem(ClipboardStrings[i],j+1, #9));
        except
          on EConvertError do
            ATimeseries.Items[AIndex].SetNull;
          else
            raise;
        end;
      except
        on ERecordAlreadyExists do
        begin
          if FlagReplaceDataQuestion then
          begin
            case MessageDlg(rsARecortExistsOverwrite,mtConfirmation,[mbYes,mbNo],0) of
              mrYes:
                FlagReplaceData := True;
              mrNo:
                FlagReplaceData := False;
            end;
          end;
          FlagReplaceDataQuestion := False;
          if FlagReplaceData then
          begin
            AIndex := ATimeSeries.IndexOf(ADate);
            try
              ATimeSeries.Items[AIndex].AsFloat :=
                StrToFloat(DelimitedStringItem(ClipboardStrings[i],j+1, #9));
            except
              on EConvertError do
                ATimeSeries.Items[AIndex].SetNull;
              else
                raise;
            end;
          end;
        end
        else begin
          RevertBuffer(ActiveIndex);
          raise;
        end;
      end;
      finally
        ADate := AddDateTime(ADate, 1);
        if (i=0) and (j=0) then ARange.StartIndex := AIndex;
      end;
    end;
  end;
  FinalizeBuffer(ActiveIndex);
  ARange.EndIndex := AIndex;
  SelectionRange := ARange;
end;

procedure TTimeseriesGrid.SetBgColorForStatistics(Value: TColor);
begin
  if (FBgColorForStatistics=Value) or (FDisplayFormat<>dfTable) then Exit;
  FBgColorForStatistics := Value;
  FullInvalidate;
end;

procedure TTimeseriesGrid.SetFilteredColor(Value: TColor);
begin
  if (FFilteredColor=Value) then Exit;
  FFilteredColor := Value;
  FullInvalidate;
end;

procedure TTimeseriesGrid.SetHighlightColor(Value: TColor);
begin
  if FHighlightColor=Value then Exit;
  FHighlightColor := Value;
  if FDisplayFormat = dfTable then FullInvalidate
  else Invalidate;
end;

procedure TTimeseriesGrid.SetHighlightMode(Value: THighlightMode);
begin
  if FHighlightMode=Value then Exit;
  FHighlightMode := Value;
  if FDisplayFormat = dfTable then FullInvalidate
  else Invalidate;
end;

function TTimeseriesGrid.IsDateDisplayed(ADate: TDateTime): Boolean;
var
  Low, High, Mid: Integer;
  Diff: Int64;
begin
  if not Filtered then
  begin
    Result := True;
    Exit;
  end;
  Result := False;
  Low := 0;
  High := Length(FDates)-1;
  while Low<=High do
  begin
    Mid := (Low+High) div 2;
    Diff := DiffInSecs(ADate, FDates[Mid].DisplayDate);
    if Diff < 0 then
      High := Mid-1
    else if Diff > 0 then
      Low := Mid+1
    else
    begin
      Low := Mid;
      Break;
    end;
  end;
  if Low<Length(FDates) then
    if DiffInSecs(FDates[Low].DisplayDate, ADate)=0 then
      Result := True;
end;

resourcestring
  rsOnlyActiveMustBeSelected = 'Only one time series must be selected.';
  rsNoRecordsSelected = 'No time series records are selected.';
  rsAvailableInSimpleFormatOnly =
    'This currently works only when viewing as list.';

function TTimeseriesGrid.GetSelectionRange: TIndexRange;
var
  i: Integer;
  ATsRecord: TTsRecord;
begin
  if DisplayFormat<>dfSimple then
    raise Exception.Create(rsAvailableInSimpleFormatOnly);
  if GetIndex(Selection.Left)<>GetIndex(Selection.Right) then
    raise Exception.Create(rsOnlyActiveMustBeSelected);
  Result.StartIndex := -1;
  Result.EndIndex := -1;
  for i := Selection.Top to Selection.Bottom do
  begin
    GetCellText(Selection.Left, i, ATsRecord);
    if ATsRecord<>nil then break;
  end;
  if ATsRecord<>nil then
    Result.StartIndex := ActiveTimeseries.IndexOf(ATsRecord.Date);
  for i := Selection.Bottom downto Selection.Top do
  begin
    GetCellText(Selection.Left, i, ATsRecord);
    if ATsRecord<>nil then break;
  end;
  if ATsRecord<>nil then
    Result.EndIndex := ActiveTimeseries.IndexOf(ATsRecord.Date);
  if Result.StartIndex = -1 then
  begin
    Assert(Result.EndIndex = -1);
    raise Exception.Create(rsNoRecordsSelected);
  end;
  Assert(Result.EndIndex<>-1);
end;

procedure TTimeseriesGrid.SetSelectionRange(Value: TIndexRange);
var ARect: TGridRect;
begin
  if DisplayFormat<>dfSimple then
    raise Exception.Create(rsAvailableInSimpleFormatOnly);
  Update; // Has to be reformatted if there are changes
  if FlagsVisible then
  begin
    ARect.Left := (ActiveIndex*2)+1;
    ARect.Right := ARect.Left+1;
  end else
  begin
    ARect.Left := ActiveIndex+1;
    ARect.Right := ARect.Left;
  end;
  ARect.Top := 1;
  while ARect.Top<RowCount do
  begin
    if Abs(DiffInSecs(FDates[ARect.Top-1].DisplayDate,
    ActiveTimeseries[Value.StartIndex].Date))<1 then
      Break;
    Inc(ARect.Top);
  end;
  ARect.Bottom := ARect.Top;
  while ARect.Bottom<RowCount do
  begin
    if Abs(DiffInSecs(FDates[ARect.Bottom-1].DisplayDate,
    ActiveTimeseries[Value.EndIndex].Date))<1 then
      Break;
    Inc(ARect.Bottom);
  end;
  Selection := ARect;
  if (Length(FDates)<VisibleRowCount) then
    TopRow := 1
  else if (Length(FDates)>=VisibleRowCount) then
    TopRow := Min(ARect.Top, Length(FDates)-VisibleRowCount+1);
end;

procedure TTimeseriesGrid.SaveColWidths;
var i: Integer;
begin
  SetLength(FSavedColWidths, ColCount);
  for i := ColCount-1 downto 0 do
    FSavedColWidths[i] := ColWidths[i];
end;

procedure TTimeseriesGrid.RestoreColWidths;
var i: Integer;
begin
  for i := Min(ColCount, Length(FSavedColWidths))-1 downto 0 do
    ColWidths[i] := FSavedColWidths[i];
end;

function TTimeseriesGrid.SelectCell(ACol, ARow: Longint): Boolean;
begin
  Result := inherited SelectCell(ACol, ARow);
  if not Result then Exit;
  if Assigned(FOnSelectCell) then FOnSelectCell(Self, ACol, ARow, Result);
end;

resourcestring
  rsEditValue = 'Edit value';

procedure TTimeseriesGrid.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  AEditorMode: Boolean;
  ASelection: TGridRect;
begin
  AEditorMode := EditorMode;
  if PopupMenu = nil then
    PopupMenu := FSavedPopupMenu;
  if AEditorMode then
    if (MouseCoord(X,Y).X <> X) or (MouseCoord(X,Y).Y <> Y) then
    begin
      try
        HideEditor;
        if ActiveIndex>-1 then
          if ActiveTimeseries.Count>0 then
            if FDontUndoFlag then RevertBuffer(ActiveIndex) else
              FinalizeBuffer(ActiveIndex);
      except
        if ActiveIndex>-1 then
          if ActiveTimeseries.Count>0 then
            RevertBuffer(ActiveIndex);
        raise;
      end;
    end;
  if MouseCoord(X,Y).X = 0 then
  begin
    ASelection.Left := 1;
    ASelection.Top := MouseCoord(X,Y).Y;
    ASelection.Right := ColCount-1;
    ASelection.Bottom := ASelection.Top;
    Selection := ASelection;
  end;
  if MouseCoord(X,Y).Y = 0 then
  begin
    ASelection.Top := 1;
    ASelection.Left := MouseCoord(X,Y).X;
    ASelection.Bottom := RowCount-1;
    ASelection.Right := ASelection.Left;
    Selection := ASelection;
  end;
  if ssDouble in Shift then
    if not AEditorMode then
      if not FReadOnly then
        if (not (MouseCoord(X,Y).X = 0)) and (not (MouseCoord(X,Y).Y = 0)) then
        begin
          if ActiveIndex>-1 then
            if ActiveTimeseries.Count>0 then
            begin
              IncUndoIDPointer;
              PrepareBuffer(ActiveIndex, SelectedDate, SelectedDate,
                rsEditValue, FUndoIDPointer);
            end;
          ShowEditor;
        end;
  inherited;
end;

function TTimeseriesGrid.EndColumnDrag(var Origin, Destination: Integer;
  const MousePt: TPoint): Boolean;
var i, FromIndex, ToIndex, SavedWidth1, SavedWidth2: Integer;

  procedure MoveColWidths(FromIndex, ToIndex: Integer);
  var FromValueCol, ToValueCol: Integer;
  begin
    if FlagsVisible then
    begin
      FromValueCol := FromIndex*2+1;
      ToValueCol := ToIndex*2+1;
    end else
    begin
      FromValueCol := FromIndex+1;
      ToValueCol := ToIndex+1;
    end;
    ColWidths[ToValueCol] := ColWidths[FromValueCol];
    if FlagsVisible then ColWidths[ToValueCol+1] := ColWidths[FromValueCol+1];
  end;

begin
  Assert(DisplayFormat = dfSimple);
  Result := False;
  FromIndex := GetIndex(Origin);
  ToIndex := GetIndex(Destination);
  if FromIndex=ToIndex then Exit;
  Move(FromIndex, ToIndex);

  { Adjust column widths. }
  SavedWidth2 := 0; { This only suppresses a warning. }
  if FlagsVisible then
  begin
    SavedWidth1 := ColWidths[FromIndex*2+1];
    SavedWidth2 := ColWidths[FromIndex*2+2];
  end else
    SavedWidth1 := ColWidths[FromIndex+1];
  if ToIndex>FromIndex then
    for i := FromIndex to ToIndex-1 do MoveColWidths(i+1, i)
  else
    for i := FromIndex downto ToIndex+1 do MoveColWidths(i-1, i);
  if FlagsVisible then
  begin
    ColWidths[ToIndex*2+1] := SavedWidth1;
    ColWidths[ToIndex*2+2] := SavedWidth2;
  end else
    ColWidths[ToIndex+1] := SavedWidth1;
end;

procedure TTimeseriesGrid.TopLeftChanged;
begin
  inherited TopLeftChanged;
  if Assigned(FOnTopLeftChanged) then FOnTopLeftChanged(Self);
end;

procedure TTimeseriesGrid.ColumnMoved(FromIndex, ToIndex: Longint);
begin
  if Assigned(FOnColumnMoved) then FOnColumnMoved(Self, FromIndex, ToIndex);
end;

procedure TTimeseriesGrid.KeyPress(var Key: Char);
begin
  if Assigned(OnKeyPress) then OnKeyPress(Self, Key);
  if (Key = #13) then
  begin
    if EditorMode then begin
      try
        HideEditor;
        if ActiveIndex>-1 then
          if ActiveTimeseries.Count>0 then
            if FDontUndoFlag then RevertBuffer(ActiveIndex) else
              FinalizeBuffer(ActiveIndex);
      except
        if ActiveIndex>-1 then
          if ActiveTimeseries.Count>0 then
            RevertBuffer(ActiveIndex);
        raise;
      end;
    end;
    Perform(WM_KEYDOWN, VK_DOWN, 0);
    {Workarround to refresh undo buffers for the last record}
    if Row=RowCount-1 then
    begin
      Perform(WM_KEYDOWN, VK_UP, 0);
      Perform(WM_KEYDOWN, VK_DOWN, 0);
    end;
  end else if not EditorMode and CharInSet(Key, [' ','0'..'9','A'..'Z','a'..'z','-',
    DecimalSeparator]) then
    if not FReadOnly then
    begin
      if ActiveIndex>-1 then
        if ActiveTimeseries.Count>0 then
        begin
          IncUndoIDPointer;
          PrepareBuffer(ActiveIndex, SelectedDate, SelectedDate, rsEditValue,
            FUndoIDPointer);
        end;
      ShowEditor;
      InPlaceEditor.Perform(WM_CHAR, Integer(Key), 0);
      Key := #0;
    end;
end;

procedure TTimeseriesGrid.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if FReadOnly then
    if not (Key in [VK_DOWN, VK_UP, VK_RIGHT, VK_LEFT, VK_RETURN,
      VK_PRIOR, VK_NEXT, VK_HOME, VK_END]) then
      Exit;
  if Key = VK_F2 then
  begin
    if ActiveIndex>-1 then
      if ActiveTimeseries.Count>0 then
        if SelectedDate<>idaEmpty then
        begin
          IncUndoIDPointer;
          PrepareBuffer(ActiveIndex, SelectedDate, SelectedDate, rsEditValue,
            FUndoIDPointer);
        end;
    ShowEditor;
  end;
  if EditorMode and (Key in [VK_DOWN, VK_UP, VK_RIGHT, VK_LEFT,
      VK_PRIOR, VK_NEXT, VK_HOME, VK_END]) then
  begin
    if ActiveIndex>-1 then
      if ActiveTimeseries.Count>0 then
        FinalizeBuffer(ActiveIndex);
    HideEditor;
  end;
  inherited;
  if EditorMode and (Key=VK_ESCAPE) then
  begin
    if ActiveIndex>-1 then
      if ActiveTimeseries.Count>0 then
        RevertBuffer(ActiveIndex);
    InplaceEditor.Undo;
    HideEditor;
    Key := 0;
  end;
end;

resourcestring
  rsRecords = ' records';
  rsNoTitle = 'No title';

procedure TTimeseriesGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  AGridCoord: TGridCoord;
  NewHint: string;

  function GetHintText(ATimeseries: TTimeseries): string;
  begin
    Result := '';
    if ATimeseries=nil then Exit;
    Result := ATimeseries.Title;
    if Result = '' then Result := rsNoTitle;
    Result := Result+#13#10+IntToStr(ATimeseries.Count)+rsRecords+
      #13#10#13#10+ATimeseries.Comment+'|';
  end;

begin
  inherited;
  NewHint := '';
  AGridCoord := MouseCoord(X, Y);
  if (AGridCoord.Y = 0) and (Count>0) then
    case DisplayFormat of
      dfTable:
        if AGridCoord.X=0 then NewHint := GetHintText(ActiveTimeseries);
      dfSimple:
        if AGridCoord.X>0 then
          NewHint := GetHintText(Data[GetIndex(AGridCoord.X)]);
    else
      Assert(False);
    end
  else
    NewHint := '';
  if NewHint<>Hint then
  begin
    Application.CancelHint;
    Hint := NewHint;
  end;
end;

procedure TTimeseriesGrid.PrepareBuffer(Index: Integer; FirstRecordDate,
      LastRecordDate: TDateTime; Caption: string; ID: Integer);
var
  i, AStart, AEnd: Integer;
  ABufferEntry: TTsGridBufferEntry;
  ATimeseries: TTimeseries;
  ACursor: TCursor;
begin
  ACursor := Screen.Cursor;
  AStart := Data[Index].IndexOf(FirstRecordDate);
  AEnd := Data[Index].IndexOf(LastRecordDate);
  ABufferEntry := nil;
  ATimeseries := nil;
  try
    (FUndoBuffers[Index] as TTsGridSingleUndoBuffer).IsProcessing := True;
    Screen.Cursor := crHourGlass;
    ABufferEntry := TTsGridBufferEntry.Create;
    ATimeseries := TTimeseries.Create;
    ATimeseries.AssignMeta(Data[Index]);
    ATimeseries.Clear;
    ABufferEntry.ModifiedFlag := Data[Index].Modified;
    ABufferEntry.FirstRecordDate := FirstRecordDate;
    ABufferEntry.LastRecordDate := LastRecordDate;
    ABufferEntry.Caption := Caption;
    ABufferEntry.ID := ID;
    ABufferEntry.Records := ATimeseries;
    for i := AStart to AEnd do
      if i>-1 then
        with Data[Index].Items[i] do
          if IsNull then
            ABufferEntry.Records.Add(Date, IsNull, 0, GetAllFlags, MStatus) else
            ABufferEntry.Records.Add(Date, IsNull, AsFloat, GetAllFlags, MStatus);
{*Workarround*}
{ I have to run AssignMeta twice to avoid setting automatically DateOffsetUnsp
  to True by calling the Add method of the TTimeseries }
    ATimeseries.AssignMeta(Data[Index]);
    with (FUndoBuffers[Index] as TTsGridSingleUndoBuffer) do
      if UndoPointer=Count then
        Add(ABufferEntry) else
        Insert(UndoPointer, ABufferEntry);
    ATimeseries := nil;
    ABufferEntry := nil;
  finally
    Screen.Cursor := ACursor;
    ATimeseries.Free;
    ABufferEntry.Free;
  end;
end;

procedure TTimeseriesGrid.FinalizeBuffer(Index: Integer);
begin
  if not (FUndoBuffers[Index] as TTsGridSingleUndoBuffer).IsProcessing then
    Exit;
  with FUndoBuffers[Index] as TTsGridSingleUndoBuffer do
    Inc(UndoPointer);
  with (FUndoBuffers[Index] as TTsGridSingleUndoBuffer) do
    while UndoPointer < Count do
      Delete(Count-1);
  (FUndoBuffers[Index] as TTsGridSingleUndoBuffer).IsProcessing := False;
end;

procedure TTimeseriesGrid.RevertBuffer(Index: Integer);
begin
  if not (FUndoBuffers[Index] as TTsGridSingleUndoBuffer).IsProcessing then
    Exit;
  with FUndoBuffers[Index] as TTsGridSingleUndoBuffer do
    if Count>0 then
    Delete(UndoPointer);
  (FUndoBuffers[Index] as TTsGridSingleUndoBuffer).IsProcessing := False;
end;

procedure TTimeseriesGrid.Undo(Index: Integer);
var
  i, AStart, AEnd, AIndex: Integer;
  ABufferEntry: TTsGridBufferEntry;
  ARedoEntry: TTsGridBufferEntry;

  procedure AssignUndo;
  begin
    ARedoEntry := TTsGridBufferEntry.Create;
    ARedoEntry.Caption := ABufferEntry.Caption;
    ARedoEntry.ID := ABufferEntry.ID;
    ARedoEntry.ModifiedFlag := ABufferEntry.ModifiedFlag;
    ARedoEntry.FirstRecordDate := ABufferEntry.FirstRecordDate;
    ARedoEntry.LastRecordDate := ABufferEntry.LastRecordDate;
    ARedoEntry.Records := TTimeseries.Create;
    ARedoEntry.Records.AssignMeta(Data[Index]);
  end;

begin
  with FUndoBuffers[Index] as TTsGridSingleUndoBuffer do
  begin
    ARedoEntry := nil;
    try
      if UndoPointer<1 then Exit;
      Dec(UndoPointer);
      ABufferEntry := Items[UndoPointer] as TTsGridBufferEntry;
      AssignUndo;
      AStart := Data[Index].IndexOf(ABufferEntry.FirstRecordDate);
      AEnd := Data[Index].IndexOf(ABufferEntry.LastRecordDate);
      for i := AStart to AEnd do
        if i>-1 then
          with Data[Index].Items[i] do
            if IsNull then
              ARedoEntry.Records.Insert(Date, IsNull, 0, GetAllFlags,
              MStatus) else
              ARedoEntry.Records.Insert(Date, IsNull, AsFloat, GetAllFlags,
              MStatus);
      for i := AEnd downto AStart do
        if i>-1 then
          if ABufferEntry.Records.IndexOf(Data[Index][i].Date)<0 then
            Data[Index].Delete(i);
      ARedoEntry.Records.AssignMeta(Data[Index]);
      Data[Index].AssignMeta(ABufferEntry.Records);
      for i := 0 to ABufferEntry.Records.Count-1 do
        with ABufferEntry.Records[i] do
        begin
          AIndex := Data[Index].IndexOf(Date);
          if AIndex>=0 then
          begin
            Data[Index][AIndex].Assign(ABufferEntry.Records[i]);
          end else begin
            if IsNull then
              Data[Index].Insert(Date, IsNull, 0, GetAllFlags, MStatus) else
              Data[Index].Insert(Date, IsNull, AsFloat, GetAllFlags, MStatus);
          end;
        end;
      Data[Index].Modified := ABufferEntry.ModifiedFlag;
      Assert(UndoPointer < Count);
      Insert(UndoPointer, ARedoEntry);
      Delete(UndoPointer+1);
      ARedoEntry := nil;
    finally
      ARedoEntry.Free;
    end;
  end;
end;

procedure TTimeseriesGrid.Redo(Index: Integer);
var
  i, AStart, AEnd, AIndex: Integer;
  ABufferEntry: TTsGridBufferEntry;
  AUndoEntry: TTsGridBufferEntry;

  procedure AssignRedo;
  begin
    AUndoEntry := TTsGridBufferEntry.Create;
    AUndoEntry.Caption := ABufferEntry.Caption;
    AUndoEntry.ID := ABufferEntry.ID;
    AUndoEntry.ModifiedFlag := ABufferEntry.ModifiedFlag;
    AUndoEntry.FirstRecordDate := ABufferEntry.FirstRecordDate;
    AUndoEntry.LastRecordDate := ABufferEntry.LastRecordDate;
    AUndoEntry.Records := TTimeseries.Create;
    AUndoEntry.Records.AssignMeta(Data[Index]);
  end;

begin
  with FUndoBuffers[Index] as TTsGridSingleUndoBuffer do
  begin
    AUndoEntry := nil;
    try
      ABufferEntry := Items[UndoPointer] as TTsGridBufferEntry;
      AssignRedo;
      AStart := Data[Index].IndexOf(ABufferEntry.FirstRecordDate);
      AEnd := Data[Index].IndexOf(ABufferEntry.LastRecordDate);
      for i := AStart to AEnd do
        if i>-1 then
          with Data[Index].Items[i] do
            if IsNull then
              AUndoEntry.Records.Insert(Date, IsNull, 0, GetAllFlags,
              MStatus) else
              AUndoEntry.Records.Insert(Date, IsNull, AsFloat, GetAllFlags,
                MStatus);
      for i := AEnd downto AStart do
        if i>-1 then
          if ABufferEntry.Records.IndexOf(Data[Index][i].Date)<0 then
            Data[Index].Delete(i);
      AUndoEntry.Records.AssignMeta(Data[Index]);
      Data[Index].AssignMeta(ABufferEntry.Records);
      for i := 0 to ABufferEntry.Records.Count-1 do
        with ABufferEntry.Records[i] do
        begin
          AIndex := Data[Index].IndexOf(Date);
          if AIndex>=0 then
          begin
            Data[Index][AIndex].Assign(ABufferEntry.Records[i]);
          end else begin
            if IsNull then
              Data[Index].Insert(Date, IsNull, 0, GetAllFlags, MStatus) else
              Data[Index].Insert(Date, IsNull, AsFloat, GetAllFlags, MStatus);
          end;
        end;
      Data[Index].Modified := True;
      Assert(UndoPointer < Count);
      Insert(UndoPointer, AUndoEntry);
      Delete(UndoPointer+1);
      Inc(UndoPointer);
      AUndoEntry := nil;      
    finally
      AUndoEntry.Free;
    end;
  end;
end;

function TTimeseriesGrid.CanUndo(Index: Integer): Boolean;
begin
  with FUndoBuffers.Items[Index] as TTsGridSingleUndoBuffer do
    Result := (Count>0) and (UndoPointer>0);
end;

function TTimeseriesGrid.CanRedo(Index: Integer): Boolean;
begin
  with FUndoBuffers.Items[Index] as TTsGridSingleUndoBuffer do
    Result := (Count>0) and (UndoPointer<Count);
end;

function TTimeseriesGrid.UndoCaption(Index: Integer): string;
begin
 Result := '';
  with FUndoBuffers.Items[Index] as TTsGridSingleUndoBuffer do
    if (Count>0) and (UndoPointer>0) then
      Result := (Items[UndoPointer-1] as TTsGridBufferEntry).Caption;
end;

function TTimeseriesGrid.RedoCaption(Index: Integer): string;
begin
 Result := '';
  with FUndoBuffers.Items[Index] as TTsGridSingleUndoBuffer do
    if (Count>0) and (UndoPointer<Count) then
      Result := (Items[UndoPointer] as TTsGridBufferEntry).Caption;
end;

function TTimeseriesGrid.UndoID(Index: Integer): Integer;
begin
 Result := -1;
  with FUndoBuffers.Items[Index] as TTsGridSingleUndoBuffer do
    if (Count>0) and (UndoPointer>0) then
      Result := (Items[UndoPointer-1] as TTsGridBufferEntry).ID;
end;

function TTimeseriesGrid.RedoID(Index: Integer): Integer;
begin
 Result := -1;
  with FUndoBuffers.Items[Index] as TTsGridSingleUndoBuffer do
    if (Count>0) and (UndoPointer<Count) then
      Result := (Items[UndoPointer] as TTsGridBufferEntry).ID;
end;

procedure TTimeseriesGrid.IncUndoIDPointer;
begin
  if FUndoIDPointer < High(Integer) then
    Inc(FUndoIDPointer) else
    FUndoIDPointer := 0;
end;

function TTimeseriesGrid.SelectedDate: TDateTime;
begin
  Result := SelectedDate(Row-1);
end;

function TTimeseriesGrid.SelectedDate(ARow: Integer): TDateTime;
var
  AIndex: Integer;
begin
  with Data[ActiveIndex] do
  begin
    AIndex := SortingIndexOf(FDates[ARow].SortingDate, FUseDateOffset);
    if AIndex>-1 then
      Result := Items[AIndex].Date
    else
      Result := idaEmpty;
  end;
end;

procedure TTimeseriesGrid.ResetBuffer(Index: Integer);
begin
  with FUndoBuffers.Items[Index] as TTsGridSingleUndoBuffer do
  begin
    while Count>0 do
      Delete(Count-1);
    UndoPointer := 0;
  end;
end;

{ TDatePicker }

procedure TDatePicker.SetActiveDate(Value: TDateTime);
begin
  FActiveDate := Value;
end;

procedure TDatePicker.SetFormat(Value: TDatePickerFormat);
begin
  FFormat := Value;
end;

constructor TDatePicker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  { Create ComboBox with Months }
  FCmbBxMonth := TComboBox.Create(Self);
  FCmbBxMonth.Parent := Self;
  FCmbBxMonth.Top := 8;
  FCmbBxMonth.Left := 40;
  FCmbBxMonth.Width := 105;
  { Create ComboBox with Years }
  FCmbBxYear := TComboBox.Create(Self);
  FCmbBxYear.Parent := Self;
  FCmbBxYear.Top := 8;
  FCmbBxYear.Left := 176;
  FCmbBxYear.Width := 57;
  { Create First SpeedButton }
  FBtnFirst := TSpeedButton.Create(Self);
  FBtnFirst.Parent := Self;
  FBtnFirst.Top := 8;
  FBtnFirst.Left := 312;
  FBtnFirst.Caption := rsFirst;
  { Create Previous SpeedButton }
  FBtnPrev := TSpeedButton.Create(Self);
  FBtnPrev.Parent := Self;
  FBtnPrev.Top := 8;
  FBtnPrev.Left := 337;
  FBtnPrev.Caption := rsPrevious;
  { Create Next SpeedButton }
  FBtnNext := TSpeedButton.Create(Self);
  FBtnNext.Parent := Self;
  FBtnNext.Top := 8;
  FBtnNext.Left := 362;
  FBtnNext.Caption := rsNext;
  { Create Last SpeedButton }
  FBtnLast := TSpeedButton.Create(Self);
  FBtnLast.Parent := Self;
  FBtnLast.Top := 8;
  FBtnLast.Left := 387;
  FBtnLast.Caption := rsLast;
end;

destructor TDatePicker.Destroy;
begin
  FBtnLast.Free;
  FBtnNext.Free;
  FBtnPrev.Free;
  FBtnFirst.Free;
  FCmbBxYear.Free;
  FCmbBxMonth.Free;
  inherited Destroy;
end;

{ TTimeseriesGridFont }

constructor TTimeseriesGridFont.Create(AOwner: TTimeseriesGrid);
begin
  inherited Create;
  FOwner := AOwner;
end;

function TTimeseriesGridFont.GetColor: TColor;
begin
  Result := inherited Color;
end;

procedure TTimeseriesGridFont.SetColor(Value: TColor);
begin
  inherited SetColor(Value);
  FOwner.Invalidate;
end;

function TTimeseriesGridFont.GetStyle: TFontStyles;
begin
  Result := inherited Style;
end;

procedure TTimeseriesGridFont.SetStyle(Value: TFontStyles);
begin
  inherited SetStyle(Value);
  FOwner.Invalidate;
end;

procedure TTimeseriesGridFont.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  FOwner.Invalidate;
end;

{ TFilterDialog }

resourcestring
  rsInvalidFilterCondition = 'Invalid filter condition';
  rsDefaultFilterDialogTitle = 'Define filter';
  rsDefaultFilterPrompt = 'Only display records such that:';

constructor TFilterDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFilterCondition := fcGreater;
  FFilterValue := 0;
  FTitle := rsDefaultFilterDialogTitle;
  FPrompt := rsDefaultFilterPrompt;
end;


function TFilterDialog.Execute: Boolean;
var
  i: Integer;
  FrmFilterDialog: TFrmFilterDialog;
begin
  FrmFilterDialog := TFrmFilterDialog.Create(Self);
  try
    FrmFilterDialog.Caption := FTitle;
    FrmFilterDialog.LblFilterPrompt.Caption := FPrompt;
    FrmFilterDialog.HelpType := FHelpType;
    FrmFilterDialog.HelpKeyword := FHelpKeyword;
    FrmFilterDialog.HelpFile := FHelpFile;
    FrmFilterDialog.HelpContext := FHelpContext;
    FrmFilterDialog.CboFlag.Items.Text := FFlagsUsed;
    case FFilterCondition of
      fcGreater, fcLess:
        with FrmFilterDialog do
        begin
          RbtnValue.Checked := True;
          EdtValue.Text := FloatToStr(FFilterValue);
          if FFilterCondition=fcGreater then CboValueCondition.ItemIndex := 0
          else CboValueCondition.ItemIndex := 1;
        end;
      fcIsNull, fcIsNotNull, fcIntactRecords, fcModifiedRecords, fcNewRecords:
        with FrmFilterDialog do
        begin
          RbtnValue.Checked := True;
          case FFilterCondition of
            fcIsNull: CboValueCondition.ItemIndex := 2;
            fcIsNotNull: CboValueCondition.ItemIndex := 3;
            fcIntactRecords: CboValueCondition.ItemIndex := 4;
            fcModifiedRecords: CboValueCondition.ItemIndex := 5;
            fcNewRecords: CboValueCondition.ItemIndex := 6;
          else
            Assert(False);
          end;
        end;
      fcHasFlag, fcHasNotFlag:
        with FrmFilterDialog do
        begin
          RbtnFlag.Checked := True;
          for i := 0 to CboFlag.Items.Count-1 do
            if CboFlag.Items[i]=FFilterValue then
              CboFlag.ItemIndex := i;
          if FFilterCondition=fcHasFlag then CboFlagCondition.ItemIndex := 0
          else CboFlagCondition.ItemIndex := 1;
        end;
    else
      Assert(False, rsInvalidFilterCondition);
    end;
    Result := (FrmFilterDialog.ShowModal = mrOK);
    with FrmFilterDialog do
      if RbtnValue.Checked = True then
        case CboValueCondition.ItemIndex of
          0: begin
               FFilterValue := Value;
               FFilterCondition := fcGreater;
             end;
          1: begin
               FFilterValue := Value;
               FFilterCondition := fcLess;
             end;
          2: FFilterCondition := fcIsNull;
          3: FFilterCondition := fcIsNotNull;
          4: FFilterCondition := fcIntactRecords;
          5: FFilterCondition := fcModifiedRecords;
          6: FFilterCondition := fcNewRecords;
        else
          Assert(False);
        end
      else if RbtnFlag.Checked = True then
      begin
        if CboFlagCondition.ItemIndex = 0 then FFilterCondition := fcHasFlag
        else FFilterCondition := fcHasNotFlag;
        FFilterValue := CboFlag.Items[CboFlag.ItemIndex];
      end else
        Assert(False);
  finally
    FrmFilterDialog.Free;
  end;
end;

initialization
  sfFull := TTimeStampFormat.Create(0);
  sfMonth := TTimeStampFormat.Create(1);
  sfYear := TTimeStampFormat.Create(2);
  sfHydrologicalYear := TTimeStampFormat.Create(3);
  sfManyMonths := TTimeStampFormat.Create(4);
  sfManyYears := TTimeStampFormat.Create(5);

end.
