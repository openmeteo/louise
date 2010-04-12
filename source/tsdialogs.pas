{******************************************************************}
{                                                                  }
{  LoUISE library                                                  }
{                                                                  }
{  Copyright (c) 2000-09 National Technical University of Athens   }
{                                                                  }
{******************************************************************}

{** Various dialogs for specifying parameters for time series management and
processing. }
unit tsdialogs;

interface

uses Classes, Ts, TsProcess, Dates, Controls, GenUtils, tsgraphdlg,
  forms, tsgrid, icomponent, contnrs, istrutils, SysUtils;

type

  {** Displays a range check parameters dialog.
      TRangeCheckDialog displays a modal Windows dialog box for specifying
      the parameters to be used in range checking. The dialog does not
      appear at runtime until it is activated by a call to the Execute method.
      When the user clicks OK, the dialog closes and the options selected are
      stored in the TRangeCheckDialog properties.
      @author A.X.
      @SeeAlso <See Routine=RangeCheck>
  }
  TRangeCheckDialog = class(TIComponent)
  private
    FLowLimit, FHighLimit: Double;
    FFlagsUsed: string;
    FRangeFlag: string;
    FHighLimitLabelText: string;
    FLowLimitVisible: Boolean;
    FAutoLowHighVisible: Boolean;
    FProbabilityLevelVisible: Boolean;
    FAutoLow: Boolean;
    FAutoHigh: Boolean;
    FProbabilityLevel: Real;
    FMarkOutLabelText: string;
    FDialogBoxName: string;
  public
    {** Creates and initializes a TRangeCheckDialog instance.
        The Create method generates a TRangeCheckDialog instance, but the
	new dialog does not appear on the form at runtime until the Execute
	method is called.
	@SeeAlso <See Method=Execute>
    }
    constructor Create(AOwner: TComponent); override;
    {** Displays the dialog.
        Execute opens the range check parameters dialog, returning True when
	the user clicks OK. If the user clicks Cancel, Execute returns False.<p>
    }
    function Execute: Boolean;
  published
    {** Specifies the low limit.
        The LowLimit property returns the low limit specified by the user.<p>
	To make a LowLimit appear by default in the dialog's edit box, assign
	a value to LowLimit in the Object Inspector or in program code.
	Programmatic changes to LowLimit have no effect while the dialog is
	active.
	@SeeAlso <See Routine=RangeCheck>
    }
    property LowLimit: Double read FLowLimit write FLowLimit;
    {** Specifies the high limit.
        The HighLimit property returns the high limit specified by the user.<p>
	To make a HighLimit appear by default in the dialog's edit box, assign
	a value to HighLimit in the Object Inspector or in program code.
	Programmatic changes to HighLimit have no effect while the dialog is
	active.
	@SeeAlso <See Routine=RangeCheck>
    }
    property HighLimit: Double read FHighLimit write FHighLimit;
    {** Set AutoHigh to true in order to set HighLimit automatically
        in respect to the sample statistical properties and
        the Normal distribution.
        @SeeAlso <See Property=AutoLow>
        @SeeAlso <See Property=ProbabilityLevel>
    }
    property AutoHigh: Boolean read FAutoHigh write FAutoHigh;
    {** Set AutoLow to true in order to set LowLimit automatically
        in respect to the sample statistical properties and
        the Normal distribution.
        @SeeAlso <See Property=AutoHigh>
        @SeeAlso <See Property=ProbabilityLevel>
    }
    property AutoLow: Boolean read FAutoLow write FAutoLow;
    {** Set the desired probability level to mark out of
        range values. e.g. a value of 95% would mark out of
        range lower and upper 2.5% when AutoLow and AutoHigh
        properties are set. Considers normal distribution and
        sample statistical properties
        @SeeAlso <See Property=AutoHigh>
        @SeeAlso <See Property=AutoLow>
    }
    property ProbabilityLevel: Real read FProbabilityLevel
      write FProbabilityLevel;
    {** Set AutoLow and AutoHigh checkboxes visibles or not
    }
    property AutoLowHighVisible: Boolean read FAutoLowHighVisible
      write FAutoLowHighVisible;
    {** Set Probability level edit box and labels visible or not
    }
    property ProbabilityLevelVisible: Boolean read FProbabilityLevelVisible
      write FProbabilityLevelVisible;
    {** Specifies the flag to be used for out of range records.
	To specify the default for the dialog's combo box, assign a value to
	RangeFlag in the Object Inspector or in program code; this flag must
        be one of those listed in the Flags property.
	Programmatic changes to RangeFlag have no effect while the
	dialog is active.
        @SeeAlso <See Routine=RangeCheck>
        @SeeAlso <See Property=Flags>
    }
    property RangeFlag: string read FRangeFlag write FRangeFlag;
    {** Specifies the flags.
        Use the Object Inspector to connect the TRangeCheckDialog to a TFlags
        component.
    }
    property FlagsUsed: string read FFlagsUsed write FFlagsUsed;
    {** Specifies Hight limit label text
    }
    property HighLimitLabel: string read FHighLimitLabelText
      write FHighLimitLabelText;
    {** Specifies Low limit label and edit box visible status
    }
    property LowLimitVisible: Boolean read FLowLimitVisible write FLowLimitVisible;
    {** Specifies Mark caption
    }
    property MarkLabel: string read FMarkOutLabelText write FMarkOutLabelText;
    {** The dialog box name (caption)
    }
    property DialogBoxName: string read FDialogBoxName write FDialogBoxName;
  end;

  {** Displays a regularize step parameters dialog.
      TRegularizeStepDialog displays a modal Windows dialog box for specifying
      the parameters to be used in time step regularization. The dialog does not
      appear at runtime until it is activated by a call to the Execute method.
      When the user clicks OK, the dialog closes and the options selected are
      stored in the TRegularizeStepDialog properties.
      @author A.X.
      @SeeAlso <See Routine=RegularizeStep>
  }
  TRegularizeStepDialog = class(TIComponent)
  private
    FTimeOffset, FStepMinutes: Integer;
    FMethod: TRegularizeStepMethod;
    FFlagsUsed: string;
    FNewDateFlag: string;
  public
    {** Creates and initializes a TRegularizeStepDialog instance.
        The Create method generates a TRegularizeStepDialog instance, but the
	new dialog does not appear on the form at runtime until the Execute
	method is called.
	@SeeAlso <See Method=Execute>
    }
    constructor Create(AOwner: TComponent); override;
    {** Displays the dialog.
        Execute opens the regularize step parameters dialog, returning True when
        the user clicks OK. If the user clicks Cancel, Execute returns False.<p>
    }
    function Execute: Boolean;
    {** Set step minutes to specify time step for the resulting time series.
        After executing the form, read the value to get the actual value for
        the resulting time series.
        If set to zero (0), variable step is considered, user should specify
        excplicitely a step value.
    }
    property StepMinutes: Integer read FStepMinutes write FStepMinutes;
  published
    {** Specifies the time offset.
        The TimeOffset property returns the specified time offset.<p>
	To make an offset appear by default in the dialog's edit box, assign
	a value to TimeOffset in the Object Inspector or in program code.
	Programmatic changes to TimeOffset have no effect while the dialog is
	active.
	@SeeAlso <See Routine=RegularizeStep>
    }
    property TimeOffset: Integer read FTimeOffset write FTimeOffset;
    {** Specifies the processing method.
        The Method property returns the selected source variable type, which
        defines the processing method.<p>
	To make a type appear by default in the dialog's radio control, assign
	a value to Method in the Object Inspector or in program code.
	Programmatic changes to Method have no effect while the dialog is
	active.
	@SeeAlso <See Routine=RegularizeStep>
    }
    property Method: TRegularizeStepMethod read FMethod write FMethod;
    {** Specifies the selected flag to be used for records with dates which did not originally exist.
	To specify the default for the dialog's combo box, assign a value to
	NewDateFlag in the Object Inspector or in program code; this flag must
        be one of those listed in the Flags property.
	Programmatic changes to NewDateFlag have no effect while the
	dialog is active.
        @SeeAlso <See Routine=RegularizeStep>
        @SeeAlso <See Property=Flags>
    }
    property NewDateFlag: string read FNewDateFlag write FNewDateFlag;
    {** Specifies the flags.
        Use the Object Inspector to connect the TRegularizeStepDialog to a TFlags
        component.
    }
    property FlagsUsed: string read FFlagsUsed write FFlagsUSed;
  end;

  {** Displays an aggregation parameters dialog.
      TAggregationDialog displays a modal Windows dialog box for specifying the
      parameters to be used in time series aggregation. The dialog does not
      appear at runtime until it is activated by a call to the Execute method.
      When the user clicks OK, the dialog closes and the options selected are
      stored in the TAggregationDialog properties.
      @author A.X.
      @SeeAlso <See Routine=Aggregate>
  }
  TAggregationDialog = class(TIComponent)
  private
    FMethod: TAggregateMethod;
    FTimeOffset: Integer;
    FHydrologicalYear: Boolean;
    FSeasonal: Boolean;
    FSourceTimeStep: TTimeStep;
    FMissingAllowed: Integer;
    FFlagsUsed: string;
    FMissingFlag: string;
    FFromMonth: Integer;
    FToMonth: Integer;
  public
    {** Creates and initializes a TAggregationDialog instance.
        The Create method generates a TAggregationDialog instance, but the new
	dialog does not appear on the form at runtime until the Execute method
	is called.
	@SeeAlso <See Method=Execute>
    }
    constructor Create(AOwner: TComponent); override;
    {** Displays the dialog.
        Execute opens the aggregation parameters dialog, returning True when
	the user clicks OK. If the user clicks Cancel, Execute returns False.<p>
	You must set SourceTimeStep before executing Execute.
	@SeeAlso <See Property=SourceTimeStep>
    }
    function Execute: Boolean;
    {** Specifies the source time step.
        Set SourceTimeStep before calling execute in order to alter the appearance
	of the dialog. Specifically, if SourceTimeStep is other than tstMonthly,
	the hydrological year check box is disabled; if SourceTimeStep is
	tstDaily or tstMonthly, the time offset edit box is disabled; in
	addition, the dialog's title is set accordingly.
    }
    property SourceTimeStep: TTimeStep read FSourceTimeStep
      write FSourceTimeStep;
  published
    {** Specifies the aggregation method.
        The Method property returns the selected aggregation method.<p>
	To make a method appear by default in the dialog's radio control, assign
	a value to Method in the Object Inspector or in program code.
	Programmatic changes to Method have no effect while the dialog is
	active.
	@SeeAlso <See Routine=Aggregate>
    }
    property Method: TAggregateMethod read FMethod write FMethod;
    {** Specifies the time offset.
        The TimeOffset property returns the specified time offset.<p>
	To make an offset appear by default in the dialog's edit box, assign
	a value to TimeOffset in the Object Inspector or in program code.
	Programmatic changes to TimeOffset have no effect while the dialog is
	active.
	@SeeAlso <See Routine=Aggregate>
    }
    property TimeOffset: Integer read FTimeOffset write FTimeOffset;
    {** Specifies whether the hydrological year will be used.
	In monthly to yearly aggregation, the HydrologicalYear property
	specifies whether the user has selected to use the hydrological year.<p>
	HydrologicalYear is turned on by default. To turn it off, assign False
	to HydrologicalYear in the Object Inspector or in program code.
	Programmatic changes to HydrologicalYear have no effect while the dialog
	is active.
	@SeeAlso <See Routine=Aggregate>
    }
    property HydrologicalYear: Boolean read FHydrologicalYear write
      FHydrologicalYear;
    {** Specifies the number of missing source values allowed for a destination value.
        The MissingAllowed property returns the specified number of allowed
        missing values.<p>
	To make a value appear by default in the dialog's edit box, assign
	a value to MissingAllowed in the Object Inspector or in program code.
	Programmatic changes to MissingAllowed have no effect while the dialog
        is active.
	@SeeAlso <See Routine=Aggregate>
    }
    property MissingAllowed: Integer read FMissingAllowed write FMissingAllowed;
    {** Specifies the flag used when some source records are missing.
        The MissingFlag property returns the specified flag to be raised
        whenever some source values are missing.<p>
	To make a flag appear by default in the dialog's combo box, assign
	a value to MissingFlag in the Object Inspector or in program code. The
        flag must be one of those listed in the Flags property.
	Programmatic changes to MissingFlag have no effect while the dialog
        is active.
	@SeeAlso <See Routine=Aggregate>
        @SeeAlso <See Property=Flags>
    }
    property MissingFlag: string read FMissingFlag write FMissingFlag;
    {**
    }
    property FlagsUsed: string read FFlagsUsed write FFlagsUsed;
    {** Specifies whether seasonal aggregation will be used for aggregating
        monthly timeseries to annual instead of full months aggregation.
    }
    property Seasonal: Boolean read FSeasonal write FSeasonal;
    {** Season begining for seasonal aggregation.
    }
    property FromMonth: Integer read FFromMonth write FFromMonth;
    {** Season ending for seasonal aggregation.
    }
    property ToMonth: Integer read FToMonth write FToMonth;
  end;

  {** Displays an aggregation parameters dialog.
      TAggregationDialog displays a modal Windows dialog box for specifying the
      parameters to be used in time series aggregation. The dialog does not
      appear at runtime until it is activated by a call to the Execute method.
      When the user clicks OK, the dialog closes and the options selected are
      stored in the TAggregationDialog properties.
      @SeeAlso <See Routine=Aggregate>
  }
  TAggregateSeriesDialog = class(TIComponent)
  private
    FSource, FDest: TTimeseries;
    FOptions: TAggregationOptionsRec;
    FHYearOrigin: Integer;
    FFlagsUsed: string;
  public
    {** Creates and initializes a TAggregationDialog instance.
        The Create method generates a TAggregateSeriesDialog instance, but the
        new dialog does not appear on the form at runtime until the Execute
        method is called.
        @SeeAlso <See Method=Execute>
    }
    constructor Create(AOwner: TComponent); override;
    {** Displays the dialog.
        Execute opens the aggregation parameters dialog, returning True when
        the user clicks OK. If the user clicks Cancel, Execute returns False.
    }
    function Execute: Boolean;
    {** Set the flags used in order to select.
    }
    property FlagsUsed: string read FFlagsUsed write FFlagsUsed;
    {** Source time series to aggregate.
    }
    property Source: TTimeseries read FSource write FSource;
    {** Destination time series.
        Destination time series is used to transfer some of the aggregation
        parameters such as offsets, time steps, etc. It is returned as an
        empty time series.
    }
    property Dest: TTimeseries read FDest write FDest;
    {** Aggregation Options}
    property Options: TAggregationOptionsRec read FOptions;
  published
    {** Specifies the number of missing source values allowed for a destination value.
        The MissingAllowed property returns the specified number of allowed
        missing values.<p>
        To make a value appear by default in the dialog's edit box, assign
        a value to MissingAllowed in the Object Inspector or in program code.
        Programmatic changes to MissingAllowed have no effect while the dialog
        is active.
        @SeeAlso <See Routine=Aggregate>
    }
    property MissingAllowed: Integer read FOptions.MissingAllowed write
      FOptions.MissingAllowed;
    {** Specifies the flag used when some source records are missing.
        The MissingFlag property returns the specified flag to be raised
        whenever some source values are missing.<p>
        To make a flag appear by default in the dialog's combo box, assign
        a value to MissingFlag in the Object Inspector or in program code. The
        flag must be one of those listed in the Flags property.
        Programmatic changes to MissingFlag have no effect while the dialog
        is active.
        @SeeAlso <See Routine=Aggregate>
        @SeeAlso <See Property=Flags>
    }
    property MissingFlag: string read FOptions.MissingFlag write
      FOptions.MissingFlag;
    {** Clear tails from null values.
    }
    property DeleteNullEnds: Boolean read FOptions.DeleteNullEnds write
      FOptions.DeleteNullEnds;
    {** Run withoud producing aggregate values just null values.
        Use to get the time stamps of the resulting time series.
    }
    property DummyRun: Boolean read FOptions.DummyRun write FOptions.DummyRun;
    {** Set to ouput missing time series.
    }
    property CalcMissingSeries: Boolean read FOptions.CalcMissingSeries write
      FOptions.CalcMissingSeries;
    {** Hydrological year origin for "other hydrological year"
    }
    property HYearOrigin: Integer read FHYearOrigin write FHYearOrigin;
  end;

  {** Displays an IDF construction parameters dialog (extreme values time
      series with monthly or yearly time step from ten-minute, hourly or
      daily time series).<p>
      TIDFEvaluationDialog displays a modal Windows dialog box for specifying the
      parameters to be used in time series to IDF. The dialog does not
      appear at runtime until it is activated by a call to the Execute method.
      When the user clicks OK, the dialog closes and the options selected are
      stored in the TIDFEvaluationDialog properties.
      @author Stefanos
      @SeeAlso <See Routine=TimeseriesToSingleIDF>
  }
  TIDFEvaluationDialog = class(TIComponent)
  private
    FAnalysisTimestep: TTimeStep;
    FFlagsUsed: string;
    FMissingFlag: string;
    FMarginalFlag: string;
    FAllowMissing: Boolean;
    FCalculateIntensity: Boolean;
    FMultiplier: Integer;
    FHydrologicalYear: Boolean;
    FCalculateMissingTimeseries: Boolean;
    FCalculateDayNumberTimeseries: Boolean;
  public
    {** Creates and initializes a TAggregationDialog instance.
        The Create method generates a TAggregationDialog instance, but the new
	dialog does not appear on the form at runtime until the Execute method
	is called.
	@SeeAlso <See Method=Execute>
    }
    constructor Create(AOwner: TComponent); override;
    {** Displays the dialog.
        Execute opens the aggregation parameters dialog, returning True when
	the user clicks OK. If the user clicks Cancel, Execute returns False.<p>
    }
    function Execute: Boolean;
    {** AnalysisTimeStep specifies resulting time series time step.
    }
    property AnalysisTimestep: TTimeStep read FAnalysisTimeStep
      write FAnalysisTimeStep;
  published
    {** Allow missing values to the accumulation process.
    }
    property AllowMissing: Boolean read FAllowMissing write FAllowMissing;
    {** Multiplier specifies the accumulation interval by multiplied basic
        time step of the original time series.
    }
    property Multiplier: Integer read FMultiplier write FMultiplier;
    {** Set CalculateIntensity to true in order to calculate for intesity
        instead of total height.
    }
    property CalculateIntensity: Boolean read FCalculateIntensity
      write FCalculateIntensity;
    {** Specifies whether the hydrological year will be used.
	In monthly to yearly aggregation, the HydrologicalYear property
	specifies whether the user has selected to use the hydrological year.<p>
	HydrologicalYear is turned on by default. To turn it off, assign False
	to HydrologicalYear in the Object Inspector or in program code.
	Programmatic changes to HydrologicalYear have no effect while the dialog
	is active.
	@SeeAlso <See Routine=TimeseriesToSingleIDF>
    }
    property HydrologicalYear: Boolean read FHydrologicalYear write
      FHydrologicalYear;

    {** Specifies the flags.
        Use the Object Inspector to connect the TAggregationDialog to a TFlags
        component.
    }
    property FlagsUsed: string read FFlagsUsed write FFlagsUsed;
    {** Specifies the flag used when some source records are missing.
        The MissingFlag property returns the specified flag to be raised
        whenever some source values are missing.<p>
	To make a flag appear by default in the dialog's combo box, assign
	a value to MissingFlag in the Object Inspector or in program code. The
        flag must be one of those listed in the Flags property.
	Programmatic changes to MissingFlag have no effect while the dialog
        is active.
	@SeeAlso <See Routine=TimeseriesToSingleIDF>
        @SeeAlso <See Property=Flags>
    }
    property MissingFlag: string read FMissingFlag write FMissingFlag;
    {** Specifies the flag used when accumation interval bounds with
        missing values.
        The MarginalFlag property returns the specified flag to be raised
        whenever some source values bounds with missing values.<p>
	To make a flag appear by default in the dialog's combo box, assign
	a value to MarginalFlag in the Object Inspector or in program code. The
        flag must be one of those listed in the Flags property.
	Programmatic changes to MarginalFlag have no effect while the dialog
        is active.
	@SeeAlso <See Routine=TimeseriesToSingleIDF>
        @SeeAlso <See Property=Flags>
    }
    property MarginalFlag: string read FMarginalFlag write FMarginalFlag;
    {** Specity to output a timeseries with missing values percent.
    }
    property CalculateMissingTimeseries: Boolean
      read FCalculateMissingTimeseries write FCalculateMissingTimeseries;
    {** Specity to output a timeseries with day number.
    }
    property CalculateDayNumberTimeseries: Boolean
      read FCalculateDayNumberTimeseries write FCalculateDayNumberTimeseries;
  end;

  {** Displays a regression parameters dialog.
      TRegressionDialog displays a modal Windows dialog box for specifying the
      parameters to be used in time series regression. The dialog does not
      appear at runtime until it is activated by a call to the Execute method.
      When the user clicks OK, the dialog closes and the options selected are
      stored in the TRegressionDialog properties.
      @author A.X.
      @SeeAlso <See Routine=RegressTimeseries>
  }
  TRegressionDialog = class(TIComponent)
  private
    FLag: Integer;
    FCrossesZero, FOrganic, FSeasonal, FDoFilling, FMeanValue: Boolean;
    FDoExtendAfter, FDoExtendBefore, FDonotFillInnerValues: Boolean;
    FRandomTerm, FTruncToZero, FRandomSeed, FOptimize: Boolean;
  public
    {** Creates and initializes a TRegressionDialog instance.
        The Create method generates a TRegressionDialog instance, but the new
	dialog does not appear on the form at runtime until the Execute method
	is called.
	@SeeAlso <See Method=Execute>
    }
    constructor Create(AOwner: TComponent); override;
    {** Displays the dialog.
        Execute opens the regression parameters dialog, returning True when
	the user clicks OK. If the user clicks Cancel, Execute returns False.<p>
    }
    function Execute: Boolean;
  published
    {** Specifies the order of auto-correlation.
        The Lag property returns the auto-correlation order specified by the
	user.<p>
	To make a lag appear by default in the dialog's radio control, assign
	a value to Lag in the Object Inspector or in program code.
	Programmatic changes to Lag have no effect while the dialog is
	active.
	@SeeAlso <See Routine=RegressTimeseries>
    }
    property Lag: Integer read FLag write FLag;
    {** Specifies whether the constant term will be zero.
        The CrossesZero property specifies whether the user has selected a zero
	constant term.<p>
	To specify a default setting for the check box, assign a value to
	CrossesZero in the Object Inspector or in program code.
	Programmatic changes to CrossesZero have no effect while the dialog is
	active.
	@SeeAlso <See Routine=RegressTimeseries>
    }
    property CrossesZero: Boolean read FCrossesZero write FCrossesZero;
    {** Specifies whether the regression will be organic.
        The Organic property specifies whether the user has selected the organic
	regression check box.<p>
	To specify a default setting for the check box, assign a value to
	Organic in the Object Inspector or in program code.
	Programmatic changes to Organic have no effect while the dialog is
	active.
	@SeeAlso <See Routine=RegressTimeseries>
    }
    property Organic: Boolean read FOrganic write FOrganic;
    {** Specifies whether the regression will be seasonal.
        The Seasonal property specifies whether the user has selected seasonal
	regression.<p>
	To specify a default setting for the check box, assign a value to
	Seasonal in the Object Inspector or in program code.
	Programmatic changes to Seasonal have no effect while the dialog is
	active.
	@SeeAlso <See Routine=RegressTimeseries>
    }
    property Seasonal: Boolean read FSeasonal write FSeasonal;
    {** Do filling on missing records of the dependent time series,
        after regression process. A New timeseries may be created
        that is a copy of the dependent time series, with filled
        data in the place of the null records.
    }
    property DoFilling: Boolean read FDoFilling write FDoFilling;
    {** Extends values to the past.
        Null values are being added before first date of the original
        timeseries, then null values are filled.
    }
    property DoExtendBefore: Boolean read FDoExtendBefore write FDoExtendBefore;
    {** Extends values to the future.
        Null values are being added after last date of the original
        timeseries, then null values are filled.
    }
    property DoExtendAfter: Boolean read FDoExtendAfter write FDoExtendAfter;
    {** Do not fill Inner values if extend (before or after) is activated.
    }
    property DonotFillInnerValues: Boolean read FDonotFillInnerValues
      write FDonotFillInnerValues;
    {** Add random term on filled records, depentdent on the mean value and
        the standard deviation of the errors.
    }
    property RandomTerm: Boolean read FRandomTerm write FRandomTerm;
    {** When randomseed is active, a Randomize is called before execution
        of infilling.
    }
    property RandomSeed: Boolean read FRandomSeed write FRandomSeed;
    {** Truncate negative values of filled records to zero.
    }
    property TruncToZero: Boolean read FTruncToZero write FTruncToZero;
    {** Calculates the constant term based only on the mean value of a single Timeseries.
    }
    property MeanValue: Boolean read FMeanValue write FMeanValue;
    {** Optimization request for multi time series regression.
    }
    property Optimize: Boolean read FOptimize write FOptimize;
  end;

  {** Displays a regression results window.
      TRegrResultsDialog displays a modal Windows dialog box which shows the
      regression results. The dialog does not appear at runtime until it is
      activated by a call to the Execute method.
      @author A.X.
      @SeeAlso <See Routine=RegressTimeseries>
  }
  TRegrResultsDialog = class(TIComponent)
  private
    FRegressionResults: TObject;
    FDependentTimeseries: TTimeseries;
    FIndependentTimeseries: TObjectList;
    FHYearOrigin: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    {** Specifies the results.
        Set RegressionResults to point either to a TRegressionResults object
        or to a TObjectList of TRegressionResults. After that, call Execute,
        and the results will be formatted and displayed on the grid's dialog.
        @SeeAlso <See Method=Execute>
    }
    property RegressionResults: TObject read FRegressionResults
      write FRegressionResults;
    {** Displays the dialog.
        Execute opens the regression parameters dialog, displaying the results
        stored in the RegressionResults property.<p>
        @SeeAlso <See Property=RegressionResults>
    }
    property DependentTimeseries: TTimeseries read FDependentTimeseries
      write FDependentTimeseries;
    property IndependentTimeseries: TObjectList read FIndependentTimeseries
      write FIndependentTimeseries;
    procedure Execute;
    property HYearOrigin: Integer read FHYearOrigin write FHYearOrigin;
  end;

  {** Displays a dialog for specifying flags.
      TFlagsDialog displays a modal Windows dialog box for specifying the
      status word for a time series record. The dialog does not appear at
      runtime until it is activated by a call to the Execute method. When
      the user clicks OK, the dialog closes and the flags checked are stored in
      the FlagsChecked property.
      @author A.X.
  }
  TFlagsDialog = class(TIComponent)
  private
    FSelectionFlags, FOnFlags, FMixedFlags: string;
    FTurnOnFlags, FTurnOffFlags: string;
  public
    {** Set selectionFlags to display checkboxes on the form.
        Should be a LF/CR separated string list.
    }
    property SelectionFlags: string read FSelectionFlags write FSelectionFlags;
    {** Set OnFlags with Flags that are set on.
        Should be a LF/CR separated string list.
    }
    property OnFlags: string read FOnFlags write FOnFlags;
    {** Set MixedFlags with Flags that are in mixed state (grayed).
        Should be a LF/CR separated string list.
    }
    property MixedFlags: string read FMixedFlags write FMixedFlags;
    {** Read to turn on flags.
        Should be a LF/CR separated string list.
    }
    property TurnOnFlags: string read FTurnOnFlags write FTurnOnFlags;
    {** Read to turn off flags.
        Should be a LF/CR separated string list.
    }
    property TurnOffFlags: string read FTurnOffFlags write FTurnOffFlags;
    {** Displays the dialog.
        Execute opens the dialog, returning True when the user clicks OK. If the
        user clicks Cancel, Execute returns False.<p>
    }
    function Execute: Boolean;
  end;

  {**  An interface for the creation of new time series or to show / alter
       the properties of an existing time series.
  }
  TTimeseriesWizard = class(TIComponent)
  private
    FMainTimeseries, FTimeseriesCopy: TTimeseries;
    FNewTimeseriesMode: Boolean;
    FCaption: string;
    FTemplatesDir: string;
    FHYearOrigin: Integer;
  public
    {** Creates and initializes a TTimeseriesPropertiesDialog instance.
        The Create method generates a TTimeseriesPropertiesDialog instance, but
        the new dialog does not appear on the form at runtime until the Execute
        method is called.
        @SeeAlso <See Method=Execute>
    }
    constructor Create(AOwner: TComponent); override;
    {** Displays the dialog.
        Execute opens the dialog, returning True when the user clicks OK. If the
        user clicks Cancel, Execute returns False.<p>
    }
    function Execute: Boolean;
    {**
    }
    property MainTimeseries: TTimeseries read FMainTimeseries write
      FMainTimeseries;
  published
    {** Set NewTimeseriesMode to true when creating new time series.
    }
    property NewTimeseriesMode: Boolean read FNewTimeseriesMode write
      FNewTimeseriesMode;
    {** Set the caption of the form.
    }
    property Caption: string read FCaption write FCaption;
    {**
    }
    property HYearOrigin: Integer read FHYearOrigin write FHYearOrigin;
    {**
    }
    property TemplatesDir: string read FTemplatesDir write FTemplatesDir;
  end;

  {** Displays a dialog for entering time series properties.
      TTimeseriesPropertiesDialog displays a modal Windows dialog box for
      specifying time series properties such as time step, date offset,
      hydrological year, and variable type. The
      dialog does not appear at runtime until it is activated by a call to the
      Execute method. When the user clicks OK, the dialog closes and the options
      selected are stored in the TTimeseriesPropertiesDialog properties.
      @author A.X.
  }
  TTimeseriesPropertiesDialog = class(TIComponent)
  private
    FTimeStep: TTimestep;
    FStrictTimeStep: Boolean;
    FHydrologicalYear: Boolean;
    FDateOffset: Real;
    FDateOffsetUnspecified: Boolean;
    FVariableType: Integer;
    FComment: string;
    FTitle: string;
    FMUnit: string;
    FPrecision: integer;
  public
    {** Creates and initializes a TTimeseriesPropertiesDialog instance.
        The Create method generates a TTimeseriesPropertiesDialog instance, but
        the new dialog does not appear on the form at runtime until the Execute
	method is called.
	@SeeAlso <See Method=Execute>
    }
    constructor Create(AOwner: TComponent); override;
    {** Displays the dialog.
	Execute opens the dialog, returning True when the user clicks OK. If the
	user clicks Cancel, Execute returns False.<p>
    }
    function Execute: Boolean;
    {** Specifies the time step.
        The TimeStep property returns the time step the user has selected in the
        time step combo box. To make a time step appear by default in the
        dialog's combo box, assign a value to TimeStep in the
        Object Inspector or in program code. Programmatic changes to TimeStep
        have no effect while the dialog is active.
        @SeeAlso <See Property=TTimeseries.TimeStep>
    }
    property TimeStep: TTimestep read FTimeStep write FTimeStep;
  published
    {** Specifies whether the time step is strict.
        The StrictTimeStep property returns the value selected by the user in
        the strict time step combo box. This value has meaning only if the
        time step is less than monthly. To specify whether the check box will
        be selected by default, assign a value to StrictTimeStep in the Object
        Inspector or in program code. Programmatic changes to StrictTimeStep
        have no effect while the dialog is active.
        @SeeAlso <See Property=TimeStep>
        @SeeAlso <See Property=TTimeseries.TimeStepRegular>
    }
    property StrictTimeStep: Boolean read FStrictTimeStep write FStrictTimeStep;
    {** Specifies whether the hydrological year is used.
        The HydrologicalYear property returns the value selected by the user in
        the hydrological year combo box. This value has meaning only if the
        time step is montly or annual. To specify whether the check box will
        be selected by default, assign a value to HydrologicalYear in the Object
        Inspector or in program code. Programmatic changes to HydrologicalYear
        have no effect while the dialog is active.
        @SeeAlso <See Property=TimeStep>
        @SeeAlso <See Property=TTimeseries.HydrologicalYear>
    }
    property HydrologicalYear: Boolean read FHydrologicalYear
      write FHydrologicalYear;
    {** Specifies the date offset.
        The DateOffset property returns the value selected by the user in
        the date offset edit box. This value has meaning only if the
        time step is monthly or annual and if DateOffsetUnspecified is False. To
        make an offset appear by default
        in the dialog's edit box, assign a value to DateOffset in the Object
        Inspector or in program code, and assign False to DateOffsetUnspecified.
        Programmatic changes to DateOffset have no effect while the dialog is
        active.
        @SeeAlso <See Property=TimeStep>
        @SeeAlso <See Property=DateOffsetUnspecified>
        @SeeAlso <See Property=TTimeseries.DateOffset>
    }
    property DateOffset: Real read FDateOffset write FDateOffset;
    {** Specifies whether the date offset is specified.
        The DateOffsetUnspecified property returns True if the user has
        specified an empty date offset edit box. This value has meaning only if
        the time step is monthly or annual. To specify an empty edit box as
        default, assign True to DateOffsetUnspecified in the Object
        Inspector or in program code. Programmatic changes to
        DateOffsetUnspecified have no effect while the dialog is active.
        @SeeAlso <See Property=TimeStep>
        @SeeAlso <See Property=DateOffset>
        @SeeAlso <See Property=TTimeseries.DateOffsetUnspecified>
    }
    property DateOffsetUnspecified: Boolean read FDateOffsetUnspecified
      write FDateOffsetUnspecified;
    {** Specifies the variable type.
        The VariableType property returns the variable type the user has
        selected in the variable type combo box. To make a variable type appear
        by default in the dialog's combo box, assign a value to VariableType in
        the Object Inspector or in program code. Programmatic changes to
        VariableType have no effect while the dialog is active.
        @SeeAlso <See Property=TTimeseries.VariableType>
    }
    property VariableType: Integer read FVariableType write FVariableType;
    {** Specifies the display title.
        The Title property returns the display title the user has entered
        in the dialog's edit box. To make a title appear by
        default in the dialog's edit box, assign a value to Title in
        the Object Inspector or in program code. Programmatic changes to
        Title have no effect while the dialog is active.
        @SeeAlso <See Property=TTimeseries.Title>
        @SeeAlso <See Property=Comment>
    }
    property Title: string read FTitle write FTitle;
    {** Specifies the display comment.
        The Comment property returns the display comment the user has
        entered in the dialog's memo field. To make a comment appear by
        default in the dialog's memo field, assign a value to Comment in
        the Object Inspector or in program code. Programmatic changes to
        Comment have no effect while the dialog is active.
        @SeeAlso <See Property=TTimeseries.Comment>
        @SeeAlso <See Property=Title>
    }
    property Comment: string read FComment write FComment;
    {** Specifies the measurement unit.
        The MUnit property returns the measurement unit the user has specified
        in the dialog's edit box. To make a unit appear by default in the
        dialog's edit box, assign a value to MUnit in the Object Inspector or
        in program code. Programmatic changes to MUnit have no effect while
        the dialog is active.
    }
    property MUnit: string read FMUnit write FMUnit;
    {** Specifies the number of decimal digits that the timeseries' values will
        have when displaying.
        Programmatic changes to Precision have no effect while
        the dialog is active.
        @SeeAlso <See Property=TTimeseries.Precision>
    }
    property Precision: integer read FPrecision write FPrecision;
  end;

  {** Displays a Penman evaporation parameters dialog.
      TPenmanDialog displays a modal Windows dialog box for specifying the
      parameters to be used in calculating evaporation by Penman. The dialog
      does not appear at runtime until it is activated by a call to the Execute
      method. When the user clicks OK, the dialog closes and the options
      selected are stored in the TPenmanDialog properties.
      @author A.X.
      @SeeAlso <See Routine=CalcPenmanEvap>
  }
  TPenmanDialog = class(TIComponent)
  private
    FLatitude: Real;
    FAltitude: Integer;
    FAlbedo: Real;
    FCalculationType: Integer;
    FSunshineType: Integer;
    FMonthlyDay: Integer;
    FA_e, FB_e, FA_L, FB_L, FA_s, FB_s: Real;
    FDefaultA_e, FDefaultB_e: Real;
    FDefaultA_L, FDefaultB_L: Real;
    FDefaultA_s, FDefaultB_s: Real;
    FDefaultA_e_PM, FDefaultB_e_PM: Real;
    FDefaultAlbedo, FDefaultAlbedo_PM: Real;    
    FAlpha, FBeta, FCe: Real;
    FCropCoefficient: Real;
    FTimeseriesGrid: TTimeseriesGrid;
    FEvaporationIndex, FTemperatureIndex: Integer;
  public
    {** Creates and initializes a TPenmanDialog instance.
        The Create method generates a TPenmanDialog instance, but the new
	dialog does not appear on the form at runtime until the Execute method
	is called.
	@SeeAlso <See Method=Execute>
    }
    constructor Create(AOwner: TComponent); override;
    {** Displays the dialog.
        Execute opens the Penman parameters dialog, returning True when
	the user clicks OK. If the user clicks Cancel, Execute returns False.<p>
    }
    function Execute: Boolean;
    {** Temperature index on timeseries grid, needed for parametric calibration.
    }
    property TemperatureIndex: Integer read FTemperatureIndex
      write FTemperatureIndex;
    {** Evaporation index on timeseries grid, needed for parametric calibration.
    }
    property EvaporationIndex: Integer read FEvaporationIndex
      write FEvaporationIndex;
  published
    {** Specifies the latitude.
        The Latitude property returns the latitude in decimal degrees,
        automatically converted from the degrees, minutes and seconds specified
        by the user.<p>
        To make a latitude appear by default in the dialog's edit box,
        assign a value to Latitude in the Object Inspector or in program code.
	Programmatic changes to Lag have no effect while the dialog is
	active.
        @SeeAlso <See Routine=CalcPenmanEvap>
    }
    property Latitude: Real read FLatitude write FLatitude;
    {** Specifies the altitude.
        The Altitude property returns the Altitude specified
        by the user.<p>
        To make an altitude appear by default in the dialog's edit box,
        assign a value to Altitude in the Object Inspector or in program code.
	Programmatic changes to Altitude have no effect while the dialog is
	active.
        @SeeAlso <See Routine=CalcPenmanEvap>
    }
    property Altitude: Integer read FAltitude write FAltitude;
    {** Specifies the albedo.
        The Albedo property returns the Albedo specified
        by the user.<p>
        To make an albedo appear by default in the dialog's edit box,
        assign a value to Albedo in the Object Inspector or in program code.
	Programmatic changes to Albedo have no effect while the dialog is
	active.
        @SeeAlso <See Routine=CalcPenmanEvap>
    }
    property Albedo: Real read FAlbedo write FAlbedo;
    {** Specifies parameters for the formulas.
        A_e and B_e are the parameters of the Brunt formula; A_L and B_L are
        the parameters of the cloud effect formula; A_s and B_s are the
        parameters of the Prescott co-efficient formula. Use these properties
        to determine the values specified by the user. To make a value appear
        by default in the dialog's edit box, assign a value to any of these
        properties in the Object Inspector or in program code. Programmatic
        changes have no effect while the dialog is active.
        @SeeAlso <See Routine=DefaultA_e>
    }
    property A_e: Real read FA_e write FA_e;
    {** Specifies parameters for the formulas.
        See <See Property=A_e>.
    }
    property B_e: Real read FB_e write FB_e;
    {** Specifies parameters for the formulas.
        See <See Property=A_e>.
    }
    property A_L: Real read FA_L write FA_L;
    {** Specifies parameters for the formulas.
        See <See Property=A_e>.
    }
    property B_L: Real read FB_L write FB_L;
    {** Specifies parameters for the formulas.
        See <See Property=A_e>.
    }
    property A_s: Real read FA_s write FA_s;
    {** Specifies parameters for the formulas.
        See <See Property=A_e>.
    }
    property B_s: Real read FB_s write FB_s;
    {** Alpha param for parametric - empirical evaluation of Evaporation
    }
    property Alpha: Real read FAlpha write FAlpha;
    {** Beta param for parametric - empirical evaluation of Evaporation
    }
    property Beta: Real read FBeta write FBeta;
    {** Ce param for parametric - empirical evaluation of Evaporation
    }
    property Ce: Real read FCe write FCe;
    {** Specifies default parameters for the formulas.
        When the user clicks the Defaults button, the edit boxes for parameters
        A_e, B_e, A_L, B_L, A_s and B_s are reverted to default values. The
        DefaultA_e, DefaultB_e etc. properties specify which these default
        values are (normally they will be the same as the initial A_e, B_e
        etc. properties).
        @SeeAlso <See Property=A_e>
    }
    property DefaultA_e: Real read FDefaultA_e write FDefaultA_e;
    {** Specifies default parameters for the formulas.
        See <See Property=DefaultA_e>.
    }
    property DefaultB_e: Real read FDefaultB_e write FDefaultB_e;
    {** Specifies default parameters for the formulas.
        Specific for Penman - Monteith method.
        See <See Property=DefaultA_e>.
    }
    property DefaultA_e_PM: Real read FDefaultA_e_PM write FDefaultA_e_PM;
    {** Specifies default parameters for the formulas.
        Specific for Penman - Monteith method.
        See <See Property=DefaultA_e>.
    }
    property DefaultB_e_PM: Real read FDefaultB_e_PM write FDefaultB_e_PM;
    {** Specifies default parameters for the formulas.
        See <See Property=DefaultA_e>.
    }
    property DefaultA_L: Real read FDefaultA_L write FDefaultA_L;
    {** Specifies default parameters for the formulas.
        See <See Property=DefaultA_e>.
    }
    property DefaultB_L: Real read FDefaultB_L write FDefaultB_L;
    {** Specifies default parameters for the formulas.
        See <See Property=DefaultA_e>.
    }
    property DefaultA_s: Real read FDefaultA_s write FDefaultA_s;
    {** Specifies default parameters for the formulas.
        See <See Property=DefaultA_e>.
    }
    property DefaultB_s: Real read FDefaultB_s write FDefaultB_s;
    {** Specifies default parameters for the formulas.
        See <See Property=DefaultA_e>.
    }
    property DefaultAlbedo: Real read FDefaultAlbedo write FDefaultAlbedo;
    {** Specifies default parameters for the formulas.
        See <See Property=DefaultA_e>.
    }
    property DefaultAlbedo_PM: Real read FDefaultAlbedo_PM
      write FDefaultAlbedo_PM;
    {**
    }
    property TimeseriesGrid: TTimeseriesGrid read FTimeseriesGrid
      write FTimeseriesGrid;
    {** Specifies calculation type. Values:
          0: A Penman calculation
          1: Penman - Monteith
          2: A Thornthwaite calculation
          3: Blaney-Criddle
          4: Hargreaves
          5: A Parametric - Empirical calculation
    }
    property CalculationType: Integer read FCalculationType write FCalculationType;
    {** Specifies the type of Sunshine timeseries. Values:
          0: Sunshine duration (min)
          1: Sunshine perecntage (0-1)
          2: Radiation (kJ/Sqm/d)
    }
    property SunshineType: Integer read FSunShineType write FSunShineType;
    {** Specifies the method to find the day for monthly calculations.
          0: The representative day of the month
          1: The midle of the month
    }
    property MonthlyDay: Integer read FMonthlyDay write FMonthlyDay;
    {** Crop Coefficient for Blaney-Criddle method.
    }
    property CropCoefficient: Real read FCropCoefficient write FCropCoefficient;
  end;

  {** Displays a dialog for linear combinations of timeseries.
      TLinearCombinationDialog displays a modal Windows dialog box for
      specifying the coefficients for linear combination of timeseries. The
      dialog does not appear at runtime until it is activated by a call to the
      Execute method. When the user clicks OK, the dialog closes and the
      coefficients specified are stored in the TLinearCombinationDialog
      properties.
      @author A.X.
      @SeeAlso <See Routine=TimeseriesLinComb>
  }
  TLinearCombinationDialog = class(TIComponent)
  private
    FCoefficients: TFloatList;
    FCoefficientTitles: TStringList;
    function GetCoefficientCount: Integer;
    procedure SetCoefficientCount(Value: Integer);
    function GetCoefficient(Index: Integer): Real;
    procedure SetCoefficient(Index: Integer; Value: Real);
    function GetCoefficientTitle(Index: Integer): string;
    procedure SetCoefficientTitle(Index: Integer; Value: string);

  public
    {** Creates and initializes a TLinearCombinationDialog instance.
	The Create method generates a TLinearCombinationDialog instance, but the
	new dialog does not appear on the form at runtime until the Execute
	method is called.
	@SeeAlso <See Method=Execute>
    }
    constructor Create(AOwner: TComponent); override;
    {** Destroys the object.
    }
    destructor Destroy; override;
    {** Displays the dialog.
	Execute opens the dialog, returning True when the user clicks OK. If the
	user clicks Cancel, Execute returns False.<p>
    }
    function Execute: Boolean;
    {** Specifies the number of coefficients.
        Write CoefficientCount before calling execute in order to specify the
	number of coefficients, which should be one more than the number of
	timeseries (the extra coefficient is the constant term).
    }
    property CoefficientCount: Integer read GetCoefficientCount
      write SetCoefficientCount;
    {** Returns the coefficients.
        Coefficients[0] is the constant term, and Coefficients[1 through
	CoefficientCount-1] are the coefficients for the timeseries, as
	specified by the user on the dialog. Setting Coefficients before calling
	Execute sets default values for the coefficients.
    }
    property Coefficients[Index: Integer]: Real read GetCoefficient
      write SetCoefficient;
    {** Sets the coefficient titles.
        Use CoefficientTitles to set the titles displayed on the grid for the
        coefficient. CoefficientTitles[0] corresponds to the constant term, and
        CoefficientTitles[1 through CoefficientCount-1] to the coefficients of
        the timeseries. It is probably desirable to set those to the titles of
        the timeseries.
        @SeeAlso <See Property=TTimeseries.Title>
    }
    property CoefficientTitles[Index: Integer]: string 
      read GetCoefficientTitle write SetCoefficientTitle;
  published

  end;

  {** Time Series, graphical representation capabilities.
      TTimeSeriesGraphForm is a Form Component, to show time series graphs.
      The form has functions such as zoom, pan, color cycling, print capability,
      copy to clipboard etc controled by a menu and key presses.
      @Author Stefanos
  }
  TTimeSeriesGraphForm = class(TIComponent)
  private
    FrmTSeriesGraph: TFrmTSeriesGraph;
    FAllowDifferentUnits: Boolean;
    procedure SetVisible(Value: Boolean);
    function GetVisible: Boolean;
    procedure SetWindowState(Value: TWindowState);
    function GetWindowState: TWindowState;
  public
    {** Creates the form component
    }
    constructor Create(AOwner: TComponent); override;
    {** Destroy the component instance.
    }
    destructor Destroy; override;
    {** Add a timeseries to the graph. On succesfull addition it returns True.
        If the timeseries allready exists no timeseries is added and it returns
        False.
    }
    function Add(ATimeseries: TTimeseries): Boolean;
    {** Removes a timeseries from the graph. On succesfull removal it returns
        True.
    }
    function Remove(ATimeseries: TTimeseries): Boolean;
    {** Show Method. On first show ,the actual graph form is created;
    }
    procedure Show;
    {** Hide Method.
    }
    procedure Hide;
    {** Clear the graph area by unloading Time series from the graph
    }
    procedure ClearArea;
    {** Brings graph form to the front
    }
    procedure BringToFront;
    {** Visible property. If graph form is not created, Visible returns
        False and an attempt to set it, donothing.
    }
    property Visible: Boolean read GetVisible write SetVisible;
    {** Window State property
    }
    property WindowState: TWindowState read GetWindowState write SetWindowState;
  published
    {** Allows to draw graphs with several measurement units. Default value
        is False (only timeseries with the same units may coexist in the
        same graph.
    }
    property AllowDifferentUnits: Boolean read FAllowDifferentUnits write
      FAllowDifferentUnits;
  end;

  TStatisticsForm = class(TIComponent)
  private
    FTimeseries: TTimeseries;
    FHYearOrigin: Integer;
  public
    {** Creates the form component
    }
    constructor Create(AOwner: TComponent); override;
    {** Displays the dialog.
	Execute opens the dialog, returning True when the user clicks OK. If the
	user clicks Cancel, Execute returns False.<p>
    }
    function Execute: Boolean;
    {** Set a timeseries to make statistical calculations.
        If a timeseries is not set, then an assertion
        is raised;
    }
    property Timeseries: TTimeseries read FTimeseries write FTimeseries;
  published
    property HYearOrigin: Integer read FHYearOrigin write FHYearOrigin;
  end;

{** Displays a dialog for double mass analysis. The dialog is a form
    representing the double mass curve itself and some controls (user
    interface) for processing.
}
  TDoubleMassDialog = class(TIComponent)
  private
    FIndependentTimeseries: TTimeseries;
    FDependentTimeseries: TTimeseries;
    FDestinationTimeseries: TTimeseries;
  public
{** Set the independent timeseries.
    If no independent timeseries is set, then an exception is raises.
}
    property IndependentTimeseries: TTimeseries read FIndependentTimeseries
      write FIndependentTimeseries;
{** Set the dependent timeseries.
    If no dependent timeseries is set, then an exception is raises.
}
    property DependentTimeseries: TTimeseries read FDependentTimeseries
      write FDependentTimeseries;
{** Set the destination timeseries.
    If no destination timeseries is set, then an exception is raises.
}
    property DestinationTimeseries: TTimeseries read FDestinationTimeseries
      write FDestinationTimeseries;
{** Creates and initializes a TDoubleMassDialog instance.
    The Create method generates a TDoubleMassDialog instance, but
    the new dialog does not appear on the form at runtime until the Execute
    method is called.
    @SeeAlso <See Method=Execute>
}
    constructor Create(AOwner: TComponent); override;
{** Displays the dialog.
    Execute opens the DoubleMassCurve analysis dialog, when user press
    OK a mrOK is returned and a altered DoubleMassCurve property is set.
    If user press cancel, a mrCancel is returned.
}
    function Execute: Boolean;
  end;

{** A Dialog to set the desired timeseries for IDF curves evaluation.<p>
    This dialog should brought-up from a form containing a TTimeseriesGrid
    object.<p>
    User choose the desired timeseries for IDF analysis. Then he press
    the "IDF analysis" button for the actual IDF evaluation or he closes
    the dialog by pressing the close button.<p>
    In a future version Load and Save capabilities will be added.
    @SeeAlso <See Unit=tsidf>
}
  TSetIDFTSDialog = class(TIComponent)
  private
    FTimeseriesGrid: TTimeseriesGrid;
  public
{** Creates and initializes a TSetIDFTSDialog instance.
    @SeeAlso <See Method=Execute>
}
    constructor Create(AOwner: TComponent); override;
{** Displays the dialog.<p>
    User sets the desired timeseries for IDF evaluation. Then, he may
    choose analysis functino in order to evaluate the IDF curves or
    Close in order to terminate the module.
}
    function Execute: Boolean;
  published
{** TSetIDFTSDialog in being called from an tsgrid form.<p> Set the
    TimeseriesGrid property to the TTimeseriesGrid object of the form
    in order to retrieve timeseries. If not TimeseriesGrid property
    is set then an Assertion failure is raised when Execte method
    is called.
    @SeeAlso <See Method=Execute>
}
    property TimeseriesGrid: TTimeseriesGrid read FTimeseriesGrid
      write FTimeseriesGrid;
  end;

{** A simple hydrological model (Basin Simulation Process).
    TSimpleHydroModel is a full featured dialog that calculates then
    runoff and add it to the timeseries grid supplied.
    @author Stefanos
    @SeeAlso <See Routine=BasinSim>
    @SeeAlso <See Routine=CalcBasinOpt>
}
  TSimpleHydroModel = class(TIComponent)
  private
    FTimeseriesGrid: TTimeseriesGrid;
    FEnableCalibration: Boolean;
    FRainfallIndex, FEvaporationIndex, FRunoffIndex, FPumpingIndex: Integer;
  public
{** Creates and initializes a TSetIDFTSDialog instance.
    @SeeAlso <See Method=Execute>
}
    constructor Create(AOwner: TComponent); override;
{** Displays the dialog.<p>
    User sets the desired timeseries for IDF evaluation. Then, he may
    choose analysis functino in order to evaluate the IDF curves or
    Close in order to terminate the module.
}
    function Execute: Boolean;
{** Rainfall index on the timeseries grid.
}
    property RainfallIndex: Integer read FRainfallIndex write FRainfallIndex;
{** Evaporation index on the timeseries grid.
}
    property EvaporationIndex: Integer read FEvaporationIndex
      write FEvaporationIndex;
{** Pumping index on the timeseries grid.
}
    property PumpingIndex: Integer read FPumpingIndex write FPumpingIndex;
{** Runoff index on the timeseries grid.
}
    property RunoffIndex: Integer read FRunoffIndex write FRunoffIndex;
  published
{** Specify a timeseries grid object.
    Timeseries grid should be specified or else an exception is raised.
    Basin simulation algorith load timeseries from the timeseries grid,
    then new (calculated) timeseries are writen to the timeseries grid.
}
    property TimeseriesGrid: TTimeseriesGrid read FTimeseriesGrid
      write FTimeseriesGrid;
{** Enable (default) or disable calibration, e.g. for educational versions.
}
    property EnableCalibration: Boolean read FEnableCalibration
      write FEnableCalibration default True;
  end;

{** A stage-discharge dialog analysis.
}
  TStageDischargeDialog = class(TIComponent)
  private
    FDenseTSIndex, FSparseTSIndex, FHQStageTSIndex, FHQDischTSIndex: Integer;
    FTimeseriesGrid: TTimeseriesGrid;
    FDBSession: TComponent;
  public
{** Creates and initializes a TStageDischargeDialog instance.
    @SeeAlso <See Method=Execute>
}
    constructor Create(AOwner: TComponent); override;
{** Displays the dialog.<p>
    Execute returns true after a calculation, requested by the
    user, is done. Returns False if user press the Cancel button.
}
    function Execute: Boolean;
  published
{** The timeseries is required to read /write timeseries.
}
    property TimeseriesGrid: TTimeseriesGrid read FTimeseriesGrid
      write FTimeseriesGrid;
{** This is a Database Component usefull to load / write data.
}
    property DBSession: TComponent read FDBSession write FDBSession;
{** The sparse stage timeseries index on timeseries grid used for calibration.
}
    property SparseTSIndex: Integer read FSparseTSIndex write FSparseTSIndex;
{** The Stage timeseries index on timeseries grid.
}
    property DenseTSIndex: Integer read FDenseTSIndex write FDenseTSIndex;
{** The Stage timeseries index on timeseries grid, measured from stage-discharge
    measurements sessions.
}
    property HQStageTSIndex: Integer read FHQStageTSIndex
      write FHQStageTSIndex;
{** The discharge timeseries index on timeseries grid, measrured from
    stage-discharge measurements sessions.
}
    property HQDischTSIndex: Integer read FHQDischTSIndex
      write FHQDischTSIndex;
  end;

  TTsprocessSelectionsDialog = class(TIComponent)
  private
    FTimeseriesGrid: TTimeseriesGrid;
    FSelectionArray: TTsSelectionsArray;
  public
    {** Displays the timeseries processes dialog.
        Use method before execute to set the selection properties.
        Read results with method after execute.
    }
    function Execute: Boolean;
    {** Creates the components instance.
    }
    constructor Create(AOwner: TComponent);  override;
    {** Destroys the component by freeing its associated memory.
    }
    destructor Destroy; override;
    {** Call before Execute.
    }
    procedure SetSelectionArray(ASelectionArray: TTsSelectionsArray);
  published
    {** The timeseries grid object to read timeseries.
    }
    property TimeseriesGrid: TTimeseriesGrid read FTimeseriesGrid write
      FTimeseriesGrid;
  end;

  {** A dialog for viewing and editing multi timeseries.
  }
  TMultiTimeseriesDialog = class(TIComponent)
  private
    FMultiTimeseries: TMultiTimeseries;
    FTargetTimestep: TTimeStep;
    FCaption: string;
    FIsHydrologicalYear: Boolean;
    FReadOnly: Boolean;
  public
    {** Displays the multi timeseries dialog.
        Returns (True) when modification have been made to the timeseries.
        If no modifications have been done, then it returns False.
    }
    function Execute: Boolean;
    {** Creates the components instance.
    }
    constructor Create(AOwner: TComponent);  override;
    {** Destroys the component by freeing its associated memory.
    }
    destructor Destroy; override;
    {** Specify the actual TMultiTimeseries object to edit.
        You have to set MultiTimeseries before running the Execute function
        or else an Assertion failure occurs...
    }
    property MultiTimeseries: TMultiTimeseries read FMultiTimeseries write
      FMultiTimeseries;
  published
    {** Specify the default timestep for new tmultitimeseries
    }
    property TargetTimestep: TTimeStep read FTargetTimestep write
      FTargetTimestep;
    {** Specify for hydrological year for annual timestep.
    }
    property IsHydrologicalYear: Boolean read FIsHydrologicalYear write
      FIsHydrologicalYear;
    {** Use read only to use dialog as a simple viewer.
    }
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
    {** Form caption
    }
    property Caption: string read FCaption write FCaption;
  end;

  {** A form component allowing making complex calculations with time series.
      @author Stefanos  
  }
  TComplexCalculationsDialog = class(TIComponent)
  private
    FTimeseriesGrid: TTimeseriesGrid;
  public
    {** Displays the calculations dialog.
    }
    function Execute: Boolean;
    {** Creates the components instance.
    }
    constructor Create(AOwner: TComponent);  override;
    {** Destroys the component by freeing its associated memory.
    }
    destructor Destroy; override;
  published
    {** Supply a time series grid object to feed the component with time series.
    }
    property TimeseriesGrid: TTimeseriesGrid read FTimeseriesGrid write
      FTimeseriesGrid;
  end;

  {**
  }
  TDisaggregationDialog = class(TIComponent)
  private
    FMethod: TDisaggregateMethod;
    FRandomModel: TDisaggregateRandomModel;
  public
    {** Creates and initializes a TDisaggregationDialog instance.
        The Create method generates a TDisaggregationDialog instance, but the new
	dialog does not appear on the form at runtime until the Execute method
	is called.
	@SeeAlso <See Method=Execute>
    }
    constructor Create(AOwner: TComponent); override;
    {** Displays the dialog.
        Execute opens the aggregation parameters dialog, returning True when
	the user clicks OK. If the user clicks Cancel, Execute returns False.<p>
	@SeeAlso <See Property=SourceTimeStep>
    }
    function Execute: Boolean;
  published
    {** Specifies the disaggregation method.
        The Method property returns the selected disaggregation method.<p>
        To make a method appear by default in the dialog's radio control, assign
        a value to Method in the Object Inspector or in program code.
        Programmatic changes to Method have no effect while the dialog is
        active.
        @SeeAlso <See Routine=TimeseriesDisaggregate>
    }
    property Method: TDisaggregateMethod read FMethod write FMethod;
    {** Set the random model if a random term is included.
    }
    property RandomModel: TDisaggregateRandomModel read FRandomModel write
      FRandomModel;
  end;

{** Displays a dialog for Hydrometric calculations.analysis.
    The dialog is a form with several controls showing cross sections,
    measurement sessions, section slices, measurments, etc.
    You should specity Dischage and Stage Time series objects for
    outputing the results of calculations.
}
  THydrometryDialog = class(TIComponent)
  private
    FStageTimeseries: TTimeseries;
    FDischargeTimeseries: TTimeseries;
  public
{** Set the stage time series.
    If no stage timeseries is set, then an exception is raises.
}
    property StageTimeseries: TTimeseries read FStageTimeseries
      write FStageTimeseries;
{** Set the discharge timeseries.
    If no discharge timeseries is set, then an exception is raises.
}
    property DischargeTimeseries: TTimeseries read FDischargeTimeseries
      write FDischargeTimeseries;
{** Creates and initializes a THydrometyDialog instance.
    The Create method generates a THydrometryDialog instance, but
    the new dialog does not appear on the form at runtime until the Execute
    method is called.
    @SeeAlso <See Method=Execute>
}
    constructor Create(AOwner: TComponent); override;
{** Displays the dialog.
    Execute opens the Hydrometry analysis dialog, when user press then
    "Calculate" button a mrOK is returned and a altered DoubleMassCurve
    property is set, or else mrCancel is returned.
}
    function Execute: Boolean;
  end;

{** Displays a dialog for Hydrometric Timeseries areal integration.
}
  TTimeseriesIntegrationDialog = class(TIComponent)
  private
    FDest: TTimeseries;
    FTimeseriesList: TObjectList;
  public
{** A time series holding the result of analysis.
}
    property DestinationTimeseries: TTimeseries read FDest write FDest;
{** An object list holding the time series to integrate.
}
    property TimeseriesList: TObjectList read FTimeseriesList write
      FTimeseriesList;
{** Creates and initializes a TTimeseriesIntegrationDialog instance.
    The Create method generates a TTimeseriesIntegrationDialog instance, but
    the new dialog does not appear on the form at runtime until the Execute
    method is called.
    @SeeAlso <See Method=Execute>
}
    constructor Create(AOwner: TComponent); override;
{** Displays the dialog.
    Execute opens the Timeseries integration dialog, when user press then
    "Calculate" button a mrOK is returned or else mrCancel is returned.
}
    function Execute: Boolean;
  end;

{** Displays a dialog for Hydrometric Timeseries areal integration.
}
  TTimeseriesImportDataDialog = class(TIComponent)
  private
    FOptions: TImportDataToTimeseriesOptions;
    FFileName: string;
  public
    property FileName: string read FFileName write FFileName;
{** Creates and initializes a TTimeseriesImportDataDialog instance.
    The Create method generates a TTimeseriesImportDataDialog instance, but
    the new dialog does not appear on the form at runtime until the Execute
    method is called.
    @SeeAlso <See Method=Execute>
}
    constructor Create(AOwner: TComponent); override;
{** Displays the dialog.
    Execute opens the Timeseries import data dialog, when user press then
    "Import" button a mrOK is returned or else mrCancel is returned.
}
    function Execute: Boolean;
{** Data Import Options }
    property Options: TImportDataToTimeseriesOptions read FOptions write
      FOptions;
  published
  {** Set DateColumn to 0 if dates are not parsed.
      If dates are not parsed, then StartDate, and DatesIncrement are used.}
    property DateColumn: Integer read FOptions.DateColumn write
      FOptions.DateColumn;
  {** A DataColumne should be specified (first column = 1) or else Assertion
      Failure occurs.
  }
    property DataColumn: Integer read FOptions.DataColumn
      write FOptions.DataColumn;
  {** Data delimiter}
    property Delimiter: Char read FOptions.Delimiter write FOptions.Delimiter;
  {** Decimal symbol (such as . )}
    property DecimalSymbol: Char read FOptions.DecimalSymbol
      write FOptions.DecimalSymbol;
  {** Use this option to process lines with trimallspaces
  }
    property TrimSpaces: Boolean read FOptions.TrimSpaces
      write FOptions.TrimSpaces;
  {** Store this property to use later to TTimeseries.ImportData.
  }
    property Overwrite: Boolean read FOptions.Overwrite
      write FOptions.Overwrite;
  {** if a NullValueString found, treat this record as null.
  }
    property NullValueString: string read FOptions.NullValueString
      write FOptions.NullValueString;
  {** Specify DateFormat or leave-it empty to auto detect.
  }
    property DateFormat: string read FOptions.DateFormat
      write FOptions.DateFormat;
  {** Specify StartDate when no parsing dates}
    property  StartDate: TDateTime read FOptions.StartDate
      write FOptions.StartDate;
  {** Specify Increments when no parsing dates}
    property DateIncrementMonths: Integer read FOptions.DateIncrementMonths
      write FOptions.DateIncrementMonths;
  {** Specify Increments when no parsing dates}
    property DateIncrementMinutes: Integer read FOptions.DateIncrementMinutes
      write FOptions.DateIncrementMinutes;
  {** Halt parsing on read errors}
    property HaltOnError: Boolean read FOptions.HaltOnError write
      FOptions.HaltOnError;
  {** Ignore empty lines (proceed to next)}
    property IgnoreEmptyLines: Boolean read FOptions.IgnoreEmptyLines write
      FOptions.IgnoreEmptyLines;
  end;

implementation

uses  Dialogs, frmflags, AggrDlg, RegStDlg, RngChkDlg,
  TsPropsDlg, RegrDlg, RgrRsltDlg, PenmDlg, LinCombDlg,
  frmdblmass, evidfdlg, idfsetdlg, tsdblmass, hymoddlg,
  frmsd, frmstats, frmdrgts, mlttsdlg, frmdisaggr, frmtscalcparse, iform,
  frmhydrm, frmtsarea, frmimpts, frmtswiz, frmaggr;

{ TRangeCheckDialog }

resourcestring
  rsHighLimitLabel = 'High limit:';
  rsMarkOutLabelText = 'Mark out of range values with flag:';
  rsRangeCheckDialogName = 'Range check';

constructor TRangeCheckDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLowLimit := 0;
  FHighLimit := 0;
  FHighLimitLabelText := rsHighLimitLabel;
  FLowLimitVisible := True;
  FMarkOutLabelText := rsMarkOutLabelText;
  FDialogBoxName := rsRangeCheckDialogName;
  FAutoLow := False;
  FAutoHigh := False;
  FProbabilityLevel := 0.99;
  FAutoLowHighVisible := True;
  FProbabilityLevelVisible := True;
end;

function TRangeCheckDialog.Execute: Boolean;
var
  FrmRangeCheck: TFrmRangeCheck;
begin
  FrmRangeCheck := TFrmRangeCheck.Create(Self);
  try
    with FrmRangeCheck do
    begin
      EdtLowLimit.Text := FloatToStr(FLowLimit);
      EdtHighLimit.Text := FloatToStr(FHighLimit);
      lblHighLimit.Caption := FHighLimitLabelText;
      lblLowLimit.Visible := FLowLimitVisible;
      EdtLowLimit.Visible := FLowLimitVisible;
      lblMarkOut.Caption := FMarkOutLabelText;
      chkAutoLow.Checked := FAutoLow;
      chkAutoHigh.Checked := FAutoHigh;
      edtProbabilityLevel.Text := FloatToStr(100*FProbabilityLevel);
      edtProbabilityLevel.Visible := FProbabilityLevelVisible;
      lblProbabilityLevel.Visible := edtProbabilityLevel.Visible;
      chkAutoLow.Visible := FAutoLowHighVisible;
      chkAutoHigh.Visible := FAutoLowHighVisible;
      Caption := FDialogBoxName;
      CmbRangeFlag.Clear;
      CmbRangeFlag.Items.Text := FFlagsUsed;
      CmbRangeFlag.Text := FRangeFlag;
      HelpType := FHelpType;
      HelpKeyword := FHelpKeyword;
      HelpFile := FHelpFile;
      HelpContext := FHelpContext;
      Result := (ShowModal = mrOK);
      FLowLimit := StrToFloat(EdtLowLimit.Text);
      FHighLimit := StrToFloat(EdtHighLimit.Text);
      FRangeFlag := CmbRangeFlag.Text;
      FAutoLow := chkAutoLow.Checked;
      FAutoHigh := chkAutoHigh.Checked;
      FProbabilityLevel := 0.01*StrToFloat(edtProbabilityLevel.Text);
    end;
  finally
    FrmRangeCheck.Free;
  end;
end;

{ TRegularizeStepDialog }

constructor TRegularizeStepDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTimeOffset := 0;
  FStepMinutes := 0;
  FMethod := rsmInstantaneous;
  FNewDateFlag := 'DATEINSERT';
end;

resourcestring
  rsMinutes = ' minute';
  rsHours = ' hour';
  rsDays = ' day';

const
  CmbStepsList:  array[0..12] of Integer = (5, 10, 15, 20, 30, 60, 120, 180,
    240, 360, 480, 720, 1440);

function TRegularizeStepDialog.Execute: Boolean;
var
  FrmRegularizeStep: TFrmRegularizeStep;
begin
  FrmRegularizeStep := TFrmRegularizeStep.Create(Self);
  try
    with FrmRegularizeStep do
    begin
      EdtTimeOffset.Text := IntToStr(TimeOffset);
      CmbNewDateFlag.Clear;
      CmbNewDateFlag.Items.Text := FFlagsUsed;
      CmbNewDateFlag.Text := FNewDateFlag;
      case Method of
        rsmInstantaneous:   RgrpMethod.ItemIndex := 0;
        rsmVector:          RgrpMethod.ItemIndex := 1;
        rsmCumulativeCStep: RgrpMethod.ItemIndex := 2;
        rsmCumulativeVStep: RgrpMethod.ItemIndex := 3;
        rsmChangeTime:      RgrpMethod.ItemIndex := 4;
      else
        Assert(False);
      end;
      if FStepMinutes = 0 then
        rbDefaultTimestep.Enabled := False
      else rbDefaultTimestep.Enabled := True;
      if FStepMinutes>0 then
      begin
        rbDefaultTimestep.Checked := True;
        if (FStepMinutes mod 1440)=0 then
          lblDefaultStep.Caption := IntToStr(FStepMinutes div 1440)+rsDays
        else if (FStepMinutes mod 60)=0 then
          lblDefaultStep.Caption := IntToStr(FStepMinutes div 60)+rsHours
        else
          lblDefaultStep.Caption := IntToStr(FStepMinutes)+rsMinutes;
      end else
        rbTimestepFromList.Checked := True;
      HelpType := FHelpType;
      HelpKeyword := FHelpKeyword;
      HelpFile := FHelpfile;
      HelpContext := FHelpContext;
      Result := (ShowModal = mrOK);
      case RgrpMethod.ItemIndex of
        0: FMethod := rsmInstantaneous;
        1: FMethod := rsmVector;
        2: FMethod := rsmCumulativeCStep;
        3: FMethod := rsmCumulativeVStep;
        4: FMethod := rsmChangeTime;
      else
        Assert(False);
      end;
      if rbTimestepFromList.Checked then
        FStepMinutes := CmbStepsList[cmbTimeSteps.ItemIndex];
      if rbOtherTimeStep.Checked then
        FStepMinutes := spinCustomStep.Value;
      FTimeOffset := StrToInt(EdtTimeOffset.Text);
      FNewDateFlag := CmbNewDateFlag.Text;
    end;
  finally
    FrmRegularizeStep.Free;
  end;
end;

{ TAggregationDialog }

constructor TAggregationDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMethod := agmSum;
  FTimeOffset := 0;
  FHydrologicalYear := True;
  FMissingAllowed := 0;
  FSourceTimeStep := tstUnknown; { This is wrong and will raise an exception
                                     unless changed before Execute. }
end;

resourcestring
  rsAggregateToTenMinute = 'Aggregate to ten minute';
  rsAggregateToHourly = 'Aggregate to hourly';
  rsAggregateToDaily = 'Aggregate to daily';
  rsAggregateToMonthly = 'Aggregate to monthly';
  rsAggregateToYearly = 'Aggregate to yearly';
  rsWrongTimeStep = 'Can only aggregate ten-minute, hourly, daily, or monthly '+
    'time series.';

function TAggregationDialog.Execute: Boolean;
var
  FrmAggregation: TFrmAggregation;
begin
  Result := False;
  FrmAggregation := TFrmAggregation.Create(Self);
  try
    with FrmAggregation do
    begin
      case SourceTimeStep.OrdinalValue of
        7: Caption := rsAggregateToTenMinute;
        1: Caption := rsAggregateToHourly;
        2:    Caption := rsAggregateToDaily;
        3:     Caption := rsAggregateToMonthly;
        4:   Caption := rsAggregateToYearly;
      else
        raise Exception.Create(rsWrongTimeStep);
      end;
      ChkHydrologicalYear.Enabled := (SourceTimeStep = tstMonthly);
      chkSeasonal.Enabled := ChkHydrologicalYear.Enabled;
      cmbFrom.Enabled := ChkHydrologicalYear.Enabled;
      cmbTo.Enabled := ChkHydrologicalYear.Enabled;
      EdtTimeOffset.Enabled := SourceTimeStep.TimeStepIn([tstTenMinute, tstHourly,
        tstDaily, tstFiveMinute]);
      LblTimeOffset.Enabled := EdtTimeOffset.Enabled;
      case Method of
        agmSum: RgrpMethod.ItemIndex := 0;
	agmAverage: RgrpMethod.ItemIndex := 1;
	agmMaximum: RgrpMethod.ItemIndex := 2;
	agmMinimum: RgrpMethod.ItemIndex := 3;
        agmVector: RgrpMethod.ItemIndex := 4;
        agmInstant: RgrpMethod.ItemIndex := 5;
      else
        Assert(False);
      end;
      EdtTimeOffset.Text := IntToStr(TimeOffset);
      EdtMissingAllowed.Text := IntToStr(MissingAllowed);
      CmbMissingFlag.Clear;
      CmbMissingFlag.Items.Text := FFlagsUsed;
      CmbMissingFlag.Text := FMissingFlag;
      ChkHydrologicalYear.Checked := HydrologicalYear;
      chkSeasonal.Checked := Seasonal;
      HelpType := FHelpType;
      HelpKeyword := FHelpKeyword;
      HelpFile := FHelpFile;
      HelpContext := FHelpContext;
      Result := (ShowModal = mrOK);
      case RgrpMethod.ItemIndex of
        0: FMethod := agmSum;
	1: FMethod := agmAverage;
	2: FMethod := agmMaximum;
	3: FMethod := agmMinimum;
        4: FMethod := agmVector;
        5: FMethod := agmInstant;
      else
        Assert(False);
      end;
      FTimeOffset := StrToInt(EdtTimeOffset.Text);
      FMissingAllowed := StrToInt(EdtMissingAllowed.Text);
      FMissingFlag := CmbMissingFlag.Text;
      FHydrologicalYear := ChkHydrologicalYear.Checked;
      FSeasonal := chkSeasonal.Checked;
      FFromMonth := cmbFrom.ItemIndex+1;
      FToMonth := cmbTo.ItemIndex+1;
      if FHydrologicalYear then
      begin
        if FFromMonth<4 then FFromMonth := FFromMonth+9 else
          FFromMonth := FFromMonth - 3;
        if FToMonth<4 then FToMonth := FToMonth+9 else
          FToMonth := FToMonth - 3;
      end;
    end;
  finally
    FrmAggregation.Free;
  end;
end;

{ TAggregateSeriesDialog }

constructor TAggregateSeriesDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  with FOptions do
  begin
    MissingAllowed := 0;
    MissingFlag := 'MISSING';
    DeleteNullEnds := True;
    DummyRun := False;
    DeleteNullEnds := True;
    CalcMissingSeries := True;
    SeasonalAggregation := False;
    FromMonth := 1;
    ToMonth := 12;
    FromDay := 0;
    ToDay := 32;
  end;
  FDest := nil;
  FSource := nil;
  FHYearOrigin := 9;
end;

function TAggregateSeriesDialog.Execute: Boolean;
var
  FrmAggregationDialog: TFrmAggregationDialog;
begin
  Assert(Source<>nil);
  Assert(Dest<>nil);
  FrmAggregationDialog := nil;
  try
    FrmAggregationDialog := TFrmAggregationDialog.Create(Self);
    with FrmAggregationDialog do
    begin
      AggrOptions := FOptions;
      HYearOrigin := FHYearOrigin;
      SourceSeries := FSource;
      DestSeries := FDest;
      HelpType := FHelpType;
      HelpKeyword := FHelpKeyword;
      HelpFile := FHelpFile;
      HelpContext := FHelpContext;
      Result := (ShowModal = mrOK);
      if Result then FOptions := AggrOptions;
    end;
  finally
    FrmAggregationDialog.Free;
  end;
end;

{ TIDFEvaluationDialog }

constructor TIDFEvaluationDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAnalysisTimestep := tstYearly;
  FAllowMissing := True;
  FCalculateIntensity := False;
  FMultiplier := 1;
  FHydrologicalYear := True;
  FCalculateMissingTimeseries := False;
  FCalculateDayNumberTimeseries := False;
end;

function TIDFEvaluationDialog.Execute: Boolean;
var
  FrmIDFEval: TFrmIDFEval;
begin
  FrmIDFEval := TFrmIDFEval.Create(Self);
  try
    with FrmIDFEval do
    begin
//      case FAnalysisTimeStep of
        if FAnalysisTimestep=tstYearly then rgrpAnalysisTimeStep.ItemIndex := 0
        else if FAnalysisTimestep=tstMonthly then
          rgrpAnalysisTimeStep.ItemIndex := 1
      else
        Assert(False);
//      end;
      if FCalculateIntensity  then
        rgrpIDFVariable.ItemIndex := 1
      else
        rgrpIDFVariable.ItemIndex := 0;
      edtMultiplier.Text := IntToStr(FMultiplier);
      cmbMissingFlags.Clear;
      cmbMarginalFlags.Clear;
      cmbMissingFlags.Items.Text := FFlagsUsed;
      cmbMarginalFlags.Items.Text := FFlagsUsed;
      cmbMissingFlags.Text := FMissingFlag;
      cmbMarginalFlags.Text := FMarginalFlag;
      chkHydrologicalYear.Checked := FHydrologicalYear;
      chkAllowMissingValues.Checked := FAllowMissing;
      chkCalculatetMissing.Checked := FCalculateMissingTimeseries;
      chkDayNumber.Checked := FCalculateDayNumberTimeseries;
      HelpType := FHelpType;
      HelpKeyword := FHelpKeyword;
      HelpFile := FHelpFile;
      HelpContext := FHelpContext;
      Result := (ShowModal = mrOK);
      case rgrpAnalysisTimeStep.ItemIndex of
        0: FAnalysisTimeStep := tstYearly;
        1: FAnalysisTimeStep := tstMonthly;
      else
        Assert(False);
      end;
      case rgrpIDFVariable.ItemIndex of
        0: FCalculateIntensity := False;
        1: FCalculateIntensity := True;
      else
        Assert(False);
      end;
      FHydrologicalYear := chkHydrologicalYear.Checked;
      FMissingFlag := cmbMissingFlags.Text;
      FMarginalFlag := cmbMarginalFlags.Text;
      FMultiplier := StrToInt(edtMultiplier.Text);
      FAllowMissing := chkAllowMissingValues.Checked;
      FCalculateMissingTimeseries := chkCalculatetMissing.Checked;
      FCalculateDayNumberTimeseries := chkDayNumber.Checked;
    end;
  finally
    FrmIDFEval.Free;
  end;
end;

{ TRegressionDialog }

constructor TRegressionDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLag := 0;
  FCrossesZero := False;
  FOrganic := False;
  FSeasonal := False;
  FDoFilling := False;
  FRandomTerm := False;
  FTruncToZero := True;
  FRandomSeed := False;
  FDoExtendBefore := False;
  FDoExtendAfter := False;
  FDonotFillInnerValues := False;
  FMeanValue := False;
  FOptimize := False;
end;

function TRegressionDialog.Execute: Boolean;
var
  FrmRegression: TFrmRegression;
begin
  FrmRegression := TFrmRegression.Create(Self);
  try
    with FrmRegression do
    begin
      EdtLag.Text := IntToStr(FLag);
      ChkCrossesZero.Checked := FCrossesZero;
      ChkOrganic.Checked := FOrganic;
      ChkSeasonal.Checked := FSeasonal;
      ChkDoFilling.Checked := FDoFilling;
      chkRandomTerm.Checked := FRandomTerm;
      chkTruncToZero.Checked := FTruncToZero;
      chkRandomSeed.Checked := FRandomSeed;
      chkDoExtendBefore.Checked := FDoExtendBefore;
      chkDoExtendAfter.Checked := FDoExtendAfter;
      chkDonotFillInnerValues.Checked := FDonotFillInnerValues;
      chkMeanValue.Checked := FMeanValue;
      chkOptimize.Checked := FOptimize;
      HelpType := FHelpType;
      HelpKeyword := FHelpKeyword;
      HelpFile := FHelpFile;
      HelpContext := FHelpContext;
      Result := (ShowModal = mrOK);
      FLag := StrToInt(Trim(EdtLag.Text));
      FCrossesZero := ChkCrossesZero.Checked;
      FOrganic := ChkOrganic.Checked;
      FSeasonal := ChkSeasonal.Checked;
      FDoFilling := ChkDoFilling.Checked;
      FRandomTerm := chkRandomTerm.Checked;
      FTruncToZero := chkTruncToZero.Checked;
      FRandomSeed := chkRandomSeed.Checked;
      FDoExtendBefore := chkDoExtendBefore.Checked;
      FDoExtendAfter := chkDoExtendAfter.Checked;
      FDonotFillInnerValues := chkDonotFillInnerValues.Checked;
      FMeanValue := chkMeanValue.Checked;
      FOptimize := chkOptimize.Checked;
    end;
  finally
    FrmRegression.Free;
  end;
end;

{ TRegrResultsDialog }

constructor TRegrResultsDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHYearOrigin := 10;
end;

procedure TRegrResultsDialog.Execute;
var FrmRegressionResults: TFrmRegressionResults;
begin
  FrmRegressionResults := TFrmRegressionResults.Create(Self);
  try
    with FrmRegressionResults do
    begin
      RegressionResults := FRegressionResults;
      DependentTimeseries := FDependentTimeseries;
      IndependentTimeseries := FIndependentTimeseries;
      HYearOrigin := FHYearOrigin;
      HelpType := FHelpType;
      HelpKeyword := FHelpKeyword;
      HelpFile := FHelpFile;
      HelpContext := FHelpContext;
      ShowModal;
    end;
  finally
    FrmRegressionResults.Free;
  end;
end;

{ TFlagsDialog }

function TFlagsDialog.Execute: Boolean;
var
  FrmSetFlagsDialog: TFrmSetFlagsDialog;
begin
  FrmSetFlagsDialog := nil;
  try
    FrmSetFlagsDialog := TFrmSetFlagsDialog.Create(Self);
    with FrmSetFlagsDialog do
    begin
      SelectionFlags := FSelectionFlags;
      OnFlags := FOnFlags;
      MixedFlags := FMixedFlags;
      HelpType := FHelpType;
      HelpKeyword := FHelpKeyword;
      HelpFile := FHelpFile;
      HelpContext := FHelpContext;
      Result := (ShowModal = mrOK);
      FTurnOnFlags := TurnOnFlags;
      FTurnOffFlags := TurnOffFlags;
      if not Result then Exit;
    end;
  finally
    FrmSetFlagsDialog.Free;
  end;
end;

{ TTimeseriesWizard }

constructor TTimeseriesWizard.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMainTimeseries := nil;
  FTimeseriesCopy := nil;
  FCaption := 'Set caption string';
  FHYearOrigin := 10;
  FTemplatesDir := '';
end;

function TTimeseriesWizard.Execute: Boolean;
var
  FrmTimeseriesWizard: TFrmTimeseriesWizard;
begin
  FrmTimeseriesWizard := nil;
  FTimeseriesCopy := nil;
  try
    Assert(FMainTimeseries<>nil);
    FTimeseriesCopy := TTimeseries.Create;
    FTimeseriesCopy.AssignMeta(MainTimeseries);
    FrmTimeseriesWizard := TFrmTimeseriesWizard.Create(nil);
    with FrmTimeseriesWizard do
    begin
      HelpType := FHelpType;
      HelpKeyword := FHelpKeyword;
      HelpFile := FHelpFile;
      HelpContext := FHelpContext;
      MainTs := FMainTimeseries;
      TsCopy := FTimeseriesCopy;
      Caption := FCaption;
      HYearOrigin := FHYearOrigin;
      NewTimeseriesMode := FNewTimeseriesMode;
      TemplatePath := FTemplatesDir;
      Result := (ShowModal = mrOK);
      if Result then
        if not FMainTimeseries.CompareMeta(FTimeseriesCopy) then
          if not FNewTimeseriesMode then
            Result := False;
      if not Result then Exit;
      FTimeseriesCopy.Modified := FMainTimeseries.CompareMeta(FTimeseriesCopy);
      FMainTimeseries.AssignMeta(FTimeseriesCopy);
    end;
  finally
    FTimeseriesCopy.Free;
    FrmTimeseriesWizard.Free;
  end;
end;

{ TTimeseriesPropertiesDialog}

constructor TTimeseriesPropertiesDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTimeStep := tstUnknown;
  FStrictTimeStep := False;
  FHydrologicalYear := False;
  FDateOffsetUnspecified := True;
  FVariableType := vtInstantaneous;
  FPrecision := tsDefaultPrecision;
end;

function TTimeseriesPropertiesDialog.Execute: Boolean;
var
  FrmTimeseriesProperties: TFrmTimeseriesProperties;
  i: Integer;
begin
  FrmTimeseriesProperties := TFrmTimeseriesProperties.Create(Self);
  try
    with FrmTimeseriesProperties do
    begin
      for i := TimeStepsSize-1 downto 0 do
        if AvailableTimesteps[i]=FTimeStep then CmbTimeStep.ItemIndex := i;
      for i := VariableTypesSize-1 downto 0 do
        if TsPropsDlg.VariableTypes[i]=FVariableType then
          CmbVariableType.ItemIndex := i;
      ChkStrictTimeStep.Checked := FStrictTimeStep;
      ChkHydrologicalYear.Checked := FHydrologicalYear;
      if FDateOffsetUnspecified then EdtDateOffset.Text := ''
      else EdtDateOffset.Text := IntToStr(Round(FDateOffset*1440));
      EdtTitle.Text := FTitle;
      EdtMUnit.Text := FMUnit;
      EdtPrecision.Text := IntToStr(FPrecision);
      MemComment.Lines.Text := FComment;
      HelpType := FHelpType;
      HelpKeyword := FHelpKeyword;
      HelpFile := FHelpFile;
      HelpContext := FHelpContext;
      Result := (ShowModal = mrOK);
      if not Result then Exit;
      FTimeStep := AvailableTimesteps[CmbTimeStep.ItemIndex];
      FVariableType := TsPropsDlg.VariableTypes[CmbVariableType.ItemIndex];
      FMUnit := EdtMUnit.Text;
      FPrecision := StrToInt(Trim(EdtPrecision.Text));
      FStrictTimeStep := ChkStrictTimeStep.Checked;
      FHydrologicalYear := ChkHydrologicalYear.Checked;
      if EdtDateOffset.Text='' then FDateOffsetUnspecified := True
      else
      begin
        FDateOffsetUnspecified := False;
        FDateOffset := StrToInt(EdtDateOffset.Text)/1440;
      end;
      FTitle := EdtTitle.Text;
      FComment := MemComment.Lines.Text;
    end;
  finally
    FrmTimeseriesProperties.Free;
  end;
end;

{ TPenmanDialog }

constructor TPenmanDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FA_e := 0.56;
  FB_e := 0.08;
  FA_L := 0.1;
  FB_L := 0.9;
  FA_s := 0.25;
  FB_s := 0.50;
  FAlpha := 0.0001;
  FBeta := 0.6;
  FCe := 0.02;
  FDefaultA_e := 0.56;
  FDefaultB_e := 0.08;
  FDefaultA_e_PM := 0.34;
  FDefaultB_e_PM := 0.044;
  FDefaultA_L := 0.1;
  FDefaultB_L := 0.9;
  FDefaultA_s := 0.25;
  FDefaultB_s := 0.50;
  FDefaultAlbedo := 0.08;
  FDefaultAlbedo_PM := 0.25;
  FLatitude := 0;
  FAltitude := 0;
  FAlbedo := 0.08;
  FCropCoefficient := 1.0;
  FCalculationType := 0;
  FSunshineType := 0;
  FMonthlyDay := 0;
  FTimeseriesGrid := nil;
  EvaporationIndex := -1;
  FTemperatureIndex := -1;
end;

function TPenmanDialog.Execute: Boolean;
var
  FrmPenman: TFrmPenman;
  Deg, Min: Integer;
  Sec: Real;
begin
  FrmPenman := TFrmPenman.Create(Self);
  try
    with FrmPenman do
    begin
      rgrpCalculationType.ItemIndex := FCalculationType;
      rgrpSunshine.ItemIndex := FSunshineType;
      rgrpMonthDay.ItemIndex := FMonthlyDay;
      DegreesToDegMinSec(FLatitude, Deg, Min, Sec);
      EdtLatDeg.Text := IntToStr(Deg);
      EdtLatMin.Text := IntToStr(Min);
      EdtLatSec.Text := FormatFloat('0.0',Sec);
      EdtAltitude.Text := IntToStr(FAltitude);
      EdtAlbedo.Text := Format('%.2f', [FAlbedo]);
      EdtA_e.Text := Format('%.2f', [FA_e]);
      EdtB_e.Text := Format('%.3f', [FB_e]);
      EdtA_L.Text := Format('%.2f', [FA_L]);
      EdtB_L.Text := Format('%.2f', [FB_L]);
      EdtA_s.Text := Format('%.2f', [FA_s]);
      EdtB_s.Text := Format('%.2f', [FB_s]);
      edtAlphaParam.Text := Format('%.5f',[FAlpha]);
      edtBetaParam.Text := Format('%.2f',[FBeta]);
      edtCeParam.Text := Format('%.3f',[FCe]);
      DefaultA_e := FDefaultA_e;
      DefaultB_e := FDefaultB_e;
      DefaultA_e_PM := FDefaultA_e_PM;
      DefaultB_e_PM := FDefaultB_e_PM;
      DefaultA_L := FDefaultA_L;
      DefaultB_L := FDefaultB_L;
      DefaultA_s := FDefaultA_s;
      DefaultB_s := FDefaultB_s;
      DefaultAlbedo := FDefaultAlbedo;
      DefaultAlbedo_PM := FDefaultAlbedo_PM;
      TimeseriesGrid := FTimeseriesGrid;
      EvaporationIndex := FEvaporationIndex;
      TemperatureIndex := FTemperatureIndex;
      edtKc.Text := FormatFloat('0.000',FCropCoefficient);
      HelpType := FHelpType;
      HelpKeyword := FHelpKeyword;
      HelpFile := FHelpFile;
      HelpContext := FHelpContext;
      Result := (ShowModal = mrOK);
      Deg := StrToInt(EdtLatDeg.Text);
      Min := StrToInt(EdtLatMin.Text);
      Sec := StrToFloat(EdtLatSec.Text);
      FLatitude := DegMinSecToDegrees(Deg, Min, Sec);
      FAltitude := StrToInt(EdtAltitude.Text);
      FAlbedo := StrToFloat(EdtAlbedo.Text);
      FA_e := StrToFloat(EdtA_e.Text);
      FB_e := StrToFloat(EdtB_e.Text);
      FA_L := StrToFloat(EdtA_L.Text);
      FB_L := StrToFloat(EdtB_L.Text);
      FA_s := StrToFloat(EdtA_s.Text);
      FB_s := StrToFloat(EdtB_s.Text);
      FAlpha := StrToFloat(edtAlphaParam.Text);
      FBeta := StrToFloat(edtBetaParam.Text);
      FCe := StrToFloat(edtCeParam.Text);
      FCropCoefficient := StrToFloat(edtKc.Text);
      FCalculationType := rgrpCalculationType.ItemIndex;
      FSunShineType := rgrpSunshine.ItemIndex;
      FMonthlyDay := rgrpMonthDay.ItemIndex;
    end;
  finally
    FrmPenman.Free;
  end;
end;

{ TLinearCombinationDialog }

constructor TLinearCombinationDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCoefficients := TFloatList.Create;
  FCoefficientTitles := TStringList.Create;
  CoefficientCount := 1;
end;

destructor TLinearCombinationDialog.Destroy;
begin
  FCoefficientTitles.Free;
  FCoefficients.Free;
  inherited Destroy;
end;

function TLinearCombinationDialog.GetCoefficientCount: Integer;
begin
  Result := FCoefficients.Count;
end;

procedure TLinearCombinationDialog.SetCoefficientCount(Value: Integer);
var i: Integer;
begin
  i := FCoefficients.Count;
  Assert(FCoefficientTitles.Count=i);
  while i<Value do
  begin
    FCoefficients.Add(0);
    FCoefficientTitles.Add('');
    Inc(i);
  end;
  while i>Value do
  begin
    FCoefficients.Delete(FCoefficients.Count-1);
    FCoefficientTitles.Delete(FCoefficientTitles.Count-1);
    Dec(i);
  end;
end;

function TLinearCombinationDialog.GetCoefficient(Index: Integer): Real;
begin
  Result := FCoefficients.Items[Index];
end;

procedure TLinearCombinationDialog.SetCoefficient(Index: Integer; Value: Real);
begin
  FCoefficients.Items[Index] := Value;
end;

function TLinearCombinationDialog.GetCoefficientTitle(Index: Integer): string;
begin
  Result := FCoefficientTitles[Index];
end;

procedure TLinearCombinationDialog.SetCoefficientTitle(Index: Integer;
  Value: String);
begin
  FCoefficientTitles[Index] := Value;
end;

function TLinearCombinationDialog.Execute: Boolean;
var
  i: Integer;
  FrmLinearComb: TFrmLinearComb;
begin

  FrmLinearComb := nil;

  try
    { Create a new form object }
    FrmLinearComb := TFrmLinearComb.Create(Self);

    { Build the string grid in the linear combinations form }
    FrmLinearComb.SgrdCoefficients.ColWidths[0] := 200;
    FrmLinearComb.SgrdCoefficients.RowCount := CoefficientCount;
    for i:=0 to CoefficientCount-1 do
        FrmLinearComb.SgrdCoefficients.Cells[0,i]:= CoefficientTitles[i];

    FrmLinearComb.HelpType := FHelpType;
    FrmLinearComb.HelpKeyword := FHelpKeyword;
    FrmLinearComb.HelpFile := FHelpFile;
    FrmLinearComb.HelpContext := FHelpContext;
    { Show the linear combinations form }
    Result:=False;
    if FrmLinearComb.ShowModal=mrCancel then
    begin
      Exit;
    end;

    { Load the grid values into FCoefficients }
    for i:=0 to CoefficientCount-1 do
    begin
      if FrmLinearComb.SgrdCoefficients.Cells[1,i]='' then Coefficients[i] := 0
      else
        Coefficients[i] := StrtoFloat(FrmLinearComb.SgrdCoefficients.Cells[1,i]);
    end;

  finally  
    FrmLinearComb.free;
  end;

  Result:=True;
end;

{TTimeSeriesGraphForm}

constructor TTimeSeriesGraphForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FrmTSeriesGraph := nil;
  FAllowDifferentUnits := False;
end;

destructor TTimeSeriesGraphForm.Destroy;
begin
  if FrmTSeriesGraph<>nil then FrmTSeriesGraph.Free;
  inherited Destroy;
end;

procedure TTimeSeriesGraphForm.Show;
begin
  if FrmTSeriesGraph = nil then
    FrmTSeriesGraph := TFrmTSeriesGraph.Create(Self);
  FrmTSeriesGraph.AllowDifferentUnits := FAllowDifferentUnits;
  FrmTSeriesGraph.HelpType := FHelpType;
  FrmTSeriesGraph.HelpKeyword := FHelpKeyword;
  FrmTSeriesGraph.HelpFile := FHelpFile;
  FrmTSeriesGraph.HelpContext := FHelpContext;
  FrmTSeriesGraph.Show;
end;

function TTimeSeriesGraphForm.Remove(ATimeseries: TTimeseries): Boolean;
begin
  if FrmTSeriesGraph <> nil then
    Result := FrmTSeriesGraph.Remove(ATimeSeries)
  else
    Result := False;
end;

function TTimeSeriesGraphForm.Add(ATimeseries: TTimeseries): Boolean;
begin
  if FrmTSeriesGraph <> nil then
   begin
    FrmTSeriesGraph.AllowDifferentUnits := FAllowDifferentUnits;
    Result := FrmTSeriesGraph.Add(ATimeSeries);
   end else
    Result := False;
end;

procedure TTimeSeriesGraphForm.Hide;
begin
  if FrmTSeriesGraph <> nil then
    FrmTSeriesGraph.Hide;
end;

procedure TTimeSeriesGraphForm.ClearArea;
begin
  if FrmTSeriesGraph <> nil then
    FrmTSeriesGraph.ClearArea;
end;

procedure TTimeSeriesGraphForm.BringToFront;
begin
  if FrmTSeriesGraph <> nil then
    FrmTSeriesGraph.BringToFront;
end;

procedure TTimeSeriesGraphForm.SetVisible(Value: Boolean);
begin
  if FrmTSeriesGraph <> nil then
    FrmTSeriesGraph.Visible := Value;
end;

function TTimeSeriesGraphForm.GetVisible: Boolean;
begin
  if FrmTSeriesGraph <> nil then
    Result := FrmTSeriesGraph.Visible
  else
    Result := False;
end;

procedure TTimeSeriesGraphForm.SetWindowState(Value: TWindowState);
begin
  if FrmTSeriesGraph <> nil then
    FrmTSeriesGraph.WindowState := Value;
end;

function TTimeSeriesGraphForm.GetWindowState: TWindowState;
begin
  if FrmTSeriesGraph <> nil then
    Result := FrmTSeriesGraph.WindowState
  else
    Result := wsNormal;
end;

{TStatisticsForm}

constructor TStatisticsForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHYearOrigin := 10;
  FTimeseries := nil;
end;

function TStatisticsForm.Execute: Boolean;
var
  FrmStatistics: TFrmStatistics;
begin
  Assert(FTimeseries<>nil);
  FrmStatistics := nil;
  try
    Result := False;
    FrmStatistics := TFrmStatistics.Create(Self);
    FrmStatistics.HelpType := FHelpType;
    FrmStatistics.HelpKeyword := FHelpKeyword;
    FrmStatistics.HelpFile := FHelpFile;
    FrmStatistics.HelpContext := FHelpContext;
    FrmStatistics.HYearOrigin := FHYearOrigin;
    FrmStatistics.SetTS(FTimeseries);
    Result := (FrmStatistics.ShowModal = mrOK);
  finally
    if FrmStatistics<>nil then FrmStatistics.Release;
    FTimeseries := nil;
  end;
end;

{ TDoubleMassDialog }

constructor TDoubleMassDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIndependentTimeseries := nil;
  FDependentTimeseries := nil;
  FDestinationTimeseries := nil;
end;

function TDoubleMassDialog.Execute: Boolean;
var
  FrmDoubleMass: TFrmDoubleMass;
  ADoubleMassCurve: TDoubleMassCurve;
begin
  FrmDoubleMass := TFrmDoubleMass.Create(Self);
  SetLength(ADoubleMassCurve,0);
  Assert(FIndependentTimeseries<>nil);
  Assert(FDependentTimeseries<>nil);
  Assert(FDestinationTimeseries<>nil);
  try
    Result := False;
    TimeseriesToDoubleMassCurve(FIndependentTimeseries, FDependentTimeseries,
      ADoubleMassCurve);
    with FrmDoubleMass do
    begin
      DoubleMassCurve := ADoubleMassCurve;
      HelpType := FHelpType;
      HelpKeyword := FHelpKeyword;
      HelpFile := FHelpFile;
      HelpContext := FHelpContext;      
      Result := (ShowModal = mrOK);
      ADoubleMassCurve := DoubleMassCurve;
    end;
    DoubleMassCurveToTimeseries(FDependentTimeseries, FDestinationTimeseries,
      ADoubleMassCurve);
  finally
    FrmDoubleMass.Free;
    SetLength(ADoubleMassCurve,0);
  end;
end;

{ TSetIDFTSDialog }

constructor TSetIDFTSDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTimeseriesGrid := nil;
end;

function TSetIDFTSDialog.Execute;
var
  FrmIDFSet: TFrmIDFSet;
begin
  Assert(FTimeseriesGrid <> nil);
  FrmIDFSet := TFrmIDFSet.Create(Self);
  try
    Result := False;
    with FrmIDFSet do
    begin
      SetTimeseriesGrid(FTimeseriesGrid);
      HelpType := FHelpType;
      HelpKeyword := FHelpKeyword;
      HelpFile := FHelpFile;
      HelpContext := FHelpContext;      
      Result := (ShowModal = mrOK);
    end;
  finally
    FrmIDFSet.Free;
  end;
end;

{ TSimpleHydroModel }

constructor TSimpleHydroModel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTimeseriesGrid := nil;
  FEnableCalibration := True;
  FRainfallIndex := -1;
  FRunoffIndex := -1;
  FEvaporationIndex := -1;
  FPumpingIndex := -1;
end;

function TSimpleHydroModel.Execute;
var
  FrmHydroModel: TFrmHydroModel;
begin
  Assert(FTimeseriesGrid <> nil);
  FrmHydroModel := TFrmHydroModel.Create(Self);
  try
    Result := False;
    with FrmHydroModel do
    begin
      TimeseriesGrid :=  FTimeseriesGrid;
      tbcCalibration.TabVisible := FEnableCalibration;
      lstMeasurementDates.Visible := FEnableCalibration;
      lblMeasurementDates.Visible := FEnableCalibration;
      RainfallIndex := FRainfallIndex;
      EvaporationIndex := FEvaporationIndex;
      PumpingIndex := FPumpingIndex;
      RunoffIndex := FRunoffIndex;
      HelpType := FHelpType;
      HelpKeyword := FHelpKeyword;
      HelpFile := FHelpFile;
      HelpContext := FHelpContext;      
      Result := (ShowModal = mrOK);
    end;
  finally
    FrmHydroModel.Free;
  end;
end;

{ TStageDischargeDialog }

constructor TStageDischargeDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTimeseriesGrid := nil;
  FDBSession := nil;
  FDenseTSIndex := -1;
  FSparseTSIndex := -1;
  FHQStageTSIndex := -1;
  FHQDischTSIndex := -1;
end;

function TStageDischargeDialog.Execute;
var
  FrmStageDischarge: TFrmStageDischarge;
begin
  Assert(FTimeseriesGrid <> nil);
  //Assert(FDBSession <> nil); (bug 2696)
  FrmStageDischarge := TFrmStageDischarge.Create(Self);
  try
    Result := False;
    with FrmStageDischarge do
    begin
      TimeseriesGrid :=  FTimeseriesGrid;
      DBSession := FDBSession;
      DenseTSIndex := FDenseTSIndex;
      SparseTSIndex := FSparseTSIndex;
      HQStageTSIndex := FHQStageTSIndex;
      HQDischTSIndex := FHQDischTSIndex;
      HelpType := FHelpType;
      HelpKeyword := FHelpKeyword;
      HelpFile := FHelpFile;
      HelpContext := FHelpContext;
      Result := (ShowModal = mrOK);
    end;
  finally
    if FrmStageDischarge <> nil then FrmStageDischarge.Release;
  end;
end;

{ TTsprocessSelectionsDialog }

constructor TTsprocessSelectionsDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTimeseriesGrid := nil;
  FSelectionArray := nil;
end;

destructor TTsprocessSelectionsDialog.Destroy;
begin
  inherited Destroy;
end;

function TTsprocessSelectionsDialog.Execute;
var
  i: Integer;
  FrmTimeseriesSelections: TFrmTimeseriesSelections;
begin
  Assert(Length(FSelectionArray)>0);
  Assert(FTimeseriesGrid<>nil);
  FrmTimeseriesSelections := nil;
  try
    FrmTimeseriesSelections := TFrmTimeseriesSelections.Create(Self);
    with FrmTimeseriesSelections do
    begin
      lstAvailableTimeseries.Clear;
      for i := 0 to FTimeseriesGrid.Count-1 do
        lstAvailableTimeseries.Items.Add(IntToStr(i+1)+': '+
          FTimeseriesGrid.Data[i].Title);
      HelpType := FHelpType;
      HelpKeyword := FHelpKeyword;
      HelpFile := FHelpFile;
      HelpContext := FHelpContext;
      Initialize(FSelectionArray);
      Result := (ShowModal = mrOk);
    end;
  finally
    if FrmTimeseriesSelections<> nil then FrmTimeseriesSelections.Release;
  end;
end;

procedure TTsprocessSelectionsDialog.SetSelectionArray(ASelectionArray:
  TTsSelectionsArray);
begin
  FSelectionArray := ASelectionArray;
end;

{ TMultiTimeseriesDialog }

resourcestring
  rsMultiTimeseriesView = 'Multi time series view';

constructor TMultiTimeseriesDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTargetTimestep := tstMonthly;
  FIsHydrologicalYear := False;
  FReadOnly := False;
  FCaption := rsMultiTimeseriesView;
  FMultiTimeseries := nil;
end;

destructor TMultiTimeseriesDialog.Destroy;
begin
  inherited Destroy;
end;

function TMultiTimeseriesDialog.Execute: Boolean;
var
  FrmMultiTimeseries: TFrmMultiTimeseries;
begin
  Assert(FMultiTimeseries<>nil);
  FrmMultiTimeseries := nil;
  try
    FrmMultiTimeseries := TFrmMultiTimeseries.Create(Self);
    with FrmMultiTimeseries do
    begin
      MultiTimeseries := FMultiTimeseries;
      TargetTimestep := FTargetTimestep;
      IsHydrologicalYear := FIsHydrologicalYear;
      ReadOnly := FReadOnly;
      Caption := FCaption;
      HelpType := FHelpType;
      HelpKeyword := FHelpKeyword;
      HelpFile := FHelpFile;
      HelpContext := FHelpContext;
      Result := (ShowModal = mrYes);
    end;
  finally
    if FrmMultiTimeseries <> nil then FrmMultiTimeseries.Release;
  end;
end;

{ TComplexCalculationsDialog }

constructor TComplexCalculationsDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTimeseriesGrid := nil;
end;

destructor TComplexCalculationsDialog.Destroy;
begin
  inherited Destroy;
end;

function TComplexCalculationsDialog.Execute: Boolean;
var
  AForm: TFrmTimeseriesParser;
begin
  Assert(FTimeseriesGrid<>nil);
  AForm := nil;
  try
    AForm := TFrmTimeseriesParser.Create(Self);
    with AForm do
    begin
      TimeseriesGrid := Self.FTimeseriesGrid;
      HelpType := FHelpType;
      HelpKeyword := FHelpKeyword;
      HelpFile := FHelpFile;
      HelpContext := FHelpContext;
      Result := (ShowModal = mrOk);
    end;
  finally
    if AForm<>nil then AForm.Release;
  end;

end;

{ TDisaggregationDialog }

constructor TDisaggregationDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMethod := tdoCumulativeConstant;
  FRandomModel := tdrmUniform;
end;

function TDisaggregationDialog.Execute: Boolean;
var
  AForm: TFrmDissagregate;
begin
  AForm := nil;
  try
    AForm := TFrmDissagregate.Create(Self);
    with AForm do
    begin
      case FMethod of
        tdoCumulativeConstant: rgrpVariableType.ItemIndex := 0;
        tdoCumulativeRandom: rgrpVariableType.ItemIndex := 1;
        tdoAverageConstant: rgrpVariableType.ItemIndex := 2;
        tdoAverageRandom: rgrpVariableType.ItemIndex := 3;
      else
        Assert(False);
      end;
      case FRandomModel of
        tdrmUniform: rgrpRandomModel.ItemIndex := 0;
        tdrmExponential: rgrpRandomModel.ItemIndex := 1;
        tdrmLogarithmic: rgrpRandomModel.ItemIndex := 2;
        tdrmQuadric: rgrpRandomModel.ItemIndex := 3;
        tdrmHighOrder: rgrpRandomModel.ItemIndex := 4;
      else
        Assert(False);
      end;
      HelpType := FHelpType;
      HelpKeyword := FHelpKeyword;
      HelpFile := FHelpFile;
      HelpContext := FHelpContext;
      Result := (ShowModal = mrOk);
      case rgrpVariableType.ItemIndex of
        0: FMethod := tdoCumulativeConstant;
        1: FMethod := tdoCumulativeRandom;
        2: FMethod := tdoAverageConstant;
        3: FMethod := tdoAverageRandom;
      else
        Assert(False);
      end;
      case rgrpRandomModel.ItemIndex of
        0: FRandomModel := tdrmUniform;
        1: FRandomModel := tdrmExponential;
        2: FRandomModel := tdrmLogarithmic;
        3: FRandomModel := tdrmQuadric;
        4: FRandomModel := tdrmHighOrder;
      else
        Assert(False);
      end;
    end;
  finally
    if AForm<>nil then AForm.Release;
  end;
end;

{ THydrometryDialog }

constructor THydrometryDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStageTimeseries := nil;
  FDischargeTimeseries := nil;
end;

function THydrometryDialog.Execute: Boolean;
var
  FrmHydrometry: TFrmHydrometry;
begin
  FrmHydrometry := TFrmHydrometry.Create(Self);
  Assert(FStageTimeseries<>nil);
  Assert(FDischargeTimeseries<>nil);
  try
    Result := False;
    with FrmHydrometry do
    begin
      StageTimeseries := FStageTimeseries;
      DischargeTimeseries := FDischargeTimeseries;
      HelpType := FHelpType;
      HelpKeyword := FHelpKeyword;
      HelpFile := FHelpFile;
      HelpContext := FHelpContext;
      Result := (ShowModal = mrOK);
    end;
  finally
    FrmHydrometry.Free;
  end;
end;

{ TTimeseriesIntegrationDialog }

constructor TTimeseriesIntegrationDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTimeseriesList := nil;
  FDest := nil;
end;

function TTimeseriesIntegrationDialog.Execute: Boolean;
var
  FrmArealIntegrate: TFrmArealIntegration;
begin
  FrmArealIntegrate := TFrmArealIntegration.Create(Self);
  Assert(FDest<>nil);
  Assert(FTimeseriesList<>nil);
  try
    Result := False;
    with FrmArealIntegrate do
    begin
      Dest := FDest;
      TimeseriesList := FTimeseriesList;
      HelpType := FHelpType;
      HelpKeyword := FHelpKeyword;
      HelpFile := FHelpFile;
      HelpContext := FHelpContext;
      Result := (ShowModal = mrOK);
    end;
  finally
    FrmArealIntegrate.Free;
  end;
end;

{ TTimeseriesImportDataDialog }

constructor TTimeseriesImportDataDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  with FOptions do
  begin
    DateColumn := 1;
    DataColumn := 2;
    FlagsColumn := 0;
    Delimiter := ',';
    DecimalSymbol := '.';
    FlagsDelimiter := ' ';
    TrimSpaces := True;
    Overwrite := False;
    DateFormat := '';
    NullValueString := '';
    StartDate := FormatStrToDateTime('yyyy-mm-dd', '2000-01-01');
    DateIncrementMonths := 1;
    DateIncrementMinutes := 0;
    HaltOnError := True;
    IgnoreEmptyLines := True;
    Encoding := TEncoding.Default;
    FirstLine := 1;
  end;
  FFileName := '';
end;

function TTimeseriesImportDataDialog.Execute: Boolean;
var
  FrmImportDataToTimeseries: TFrmImportDataToTimeseries;
begin
  FrmImportDataToTimeseries := TFrmImportDataToTimeseries.Create(Self);
  Assert(FFilename<>'');
  try
    Result := False;
    with FrmImportDataToTimeseries do
    begin
      Options := FOptions;
      FileName := FFileName;
      HelpType := FHelpType;
      HelpKeyword := FHelpKeyword;
      HelpFile := FHelpFile;
      HelpContext := FHelpContext;
      Result := (ShowModal = mrOK);
      FOptions := Options;
    end;
  finally
    FrmImportDataToTimeseries.Free;
  end;
end;

end.
