{******************************************************************}
{                                                                  }
{  LoUISE library                                                  }
{                                                                  }
{  Copyright (c) 2001-2005 National Technical University of Athens }
{                                                                  }
{******************************************************************}

{** Unit with general options for the applications.
}
unit genopts;

interface

uses
  Sysutils, Classes, ilocales;

type
{** Enum types for the models.
}
  TOdModel=(OdHydrognomon,
            OdHydronomeas,
            OdHydrogeios,
            OdDipsos,
            OdCastalia,
            OdRypos,
            OdLerne,
            OdHeridanos,
            OdAls,
            OdHermes,
            OdUnknown);

type
{** Gentity type enum types.
}
  TGentityType=(
    igtAllGentities=0,
    igtDam=1,
    igtGenerator=2,
    igtStation=3,
    igtWaterTreatmentPlant=4,
    igtReservoir=5,
    igtBorehole=6,
    igtSpring=7,
    igtAqueduct=8,
    igtRiverSegment=9,
    igtBasin=10,
    igtIrrigatedArea=11,
    igtCity=12,
    igtWasteWaterTreatmentPlant=13,
    igtIndustries=14
  );

{** Plane coordinate systems.
}
  TPCSystem = ( PC_GR87 = 0, PC_UTM_ZONE29 = 1, PC_UTM_ZONE30 = 2,
    PC_UTM_ZONE31 = 3, PC_UTM_ZONE32 = 4, PC_UTM_ZONE33 = 5, PC_UTM_ZONE34 = 6,
    PC_UTM_ZONE35 = 7, PC_UTM_ZONE36 = 8, PC_UTM_ZONE37 = 9,
    PC_ED50_CYPRUS_ZONE6 = 10, PC_ED50_GREECE_ZONE4 = 11,
    PC_ED50_GREECE_ZONE5 = 12, PC_ED50_MEANEUROPE_ZONE0 =13,
    PC_ED50_MEANEUROPE_ZONE1 =14, PC_ED50_MEANEUROPE_ZONE2 =15,
    PC_ED50_MEANEUROPE_ZONE3 =16, PC_ED50_MEANEUROPE_ZONE4 =17,
    PC_ED50_MEANEUROPE_ZONE5 =18, PC_ED50_MEANEUROPE_ZONE6 =19
);

const LocalCoordsDescr: array[TPCSystem] of string =
  ('Greek Reference System 1987', 'WGS84 UTM Zone 29', 'WGS84 UTM Zone 30',
    'WGS84 UTM Zone 31', 'WGS84 UTM Zone 32', 'WGS84 UTM Zone 33',
    'WGS84 UTM Zone 34', 'WGS84 UTM Zone 35', 'WGS84 UTM Zone 36',
    'WGS84 UTM Zone 37', 'ED50 Cyprus Zone 6', 'ED50 Greece Zone 4',
    'ED50 Greece Zone 5', 'ED50 Mean Europe Zone 0', 'ED50 Mean Europe Zone 1',
    'ED50 Mean Europe Zone 2', 'ED50 Mean Europe Zone 3', 'ED50 Mean Europe Zone 4',
    'ED50 Mean Europe Zone 5', 'ED50 Mean Europe Zone 6');

{** Gentity type, table names.
}
const GentitiesTableNames: array[1..14] of string =
 ('dams', 'pumps_generators', 'stations', 'water_treatment_plants', 'reservoirs',
   'boreholes', 'springs', 'aqueducts', 'river_segments', 'basins',
   'irrig_areas', 'cities', 'wastewater_treatment_plants','industries');

type
  TInterpolatingCurveEditingMode =
    (icemStageDischarge, icemDischargeSedimentDischarge);

type
{** A class to store application options into registry.
    Use this class or write descedant classes to include
    special properties for each application.
}
  TGenApplicationOptions = class
  private
    FLastLogonUser: string;
    FLastLogonService: string;
    FLastLogonServer: string;
    FLastLogonPassword: string;
    FLastLogonPort: Integer;
    FAutoConnect: Boolean;
    FLocalCoordinateSystem: TPCSystem;
    FDatabaseEncoding: TDBEncoding;
  public
    {** Use LoadFromRegistry to load default values from registry.
        Default values are retrieved from /Software/Itia/General
        and they are: LastLogonUser, LastLogonService and
        LastLogonServer.<p>
        Override in a descendant class LoadFromRegistry to include
        custom properties.
        @SeeAlso <See Routine=WriteToRegistry>
    }
    procedure LoadFromRegistry; virtual;
    {** Use WriteToRegistry to write default values from registry.
        Default values are written to /Software/Itia/General
        and they are: LastLogonUser, LastLogonService and
        LastLogonServer.<p>
        Override in a descendant class WriteToRegistry to include
        custom properties.
        @SeeAlso <See Routine=LoadFromRegistry>        
    }
    procedure WriteToRegistry; virtual;
    {** Create initialize an TGenApplicationOptions object.
        Override create to include initialization to custom
        properties.
    }
    constructor Create; virtual;
    {** The last user login used by an Itia application.
    }
    property LastLogonUser: string read FLastLogonUser write FLastLogonUser;
    {** The last service (Database) used by an Itia application.
    }
    property LastLogonService: string read FLastLogonService
      write FLastLogonService;
    {** The last hostname (Database server) used by an Itia application.
    }
    property LastLogonServer: string read FLastLogonServer
      write FLastLogonServer;
    {** Last used password, stored encrypted.
    }
    property LastLogonPassword: string read FLastLogonPassword
      write FLastLogonPassword;
    {** Port used to connect to postgress (should be 5432 by default).
    }
    property LastLogonPort: Integer read FLastLogonPort
      write FLastLogonPort;
    {** Auto connection to database.
    }
    property AutoConnect: Boolean read FAutoConnect write FAutoConnect;
    {** Local coordinate system such as GR87, WGS84 - UTM zones
    }
    property LocalCoordinateSystem: TPCSystem read FLocalCoordinateSystem
      write FLocalCoordinateSystem;
    {** The database 8-bit encoding
    }
    property DatabaseEncoding: TDBEncoding read FDatabaseEncoding
      write FDatabaseEncoding;
  end;

{** Return an ordinal type (integer) identifying the gentity type.
    GetGentitiesGTypeOrd is using GentitiesTableNames array as well as
    TGentityType ordinal type in order to find the ordinal value.<p>
    ATableName value should be included within the GentitiesTableNames
    array items or else an assertion exception is raised.
    @SeeAlso <See Routine=GetGentitiesGType>
    @author Stefanos
}
function GetGentitiesGTypeOrd(ATableName: string): Integer;
{** Same as GetGentitiesGTypeOrd, returning a TGentityType ordinal type.
    @SeeAlso <See Routine=GetGentitiesGTypeOrd>
}
function GetGentitiesGType(ATableName: string): TGentityType;

{** Windows file type registration by windows extension and content type
    e.g.
      RegisterFileType('hts', 'text/vnd.openmeteo.timeseries',
        'Hydrognomon.hts', ParamStr(0), 'Hydrognomon time series');
    @author Stefanos
}
procedure RegisterFileType(ExtName, ContentTypeName, FileTypeName, AppName,
  Description: string);

{** Queries for an extension if it is available.
    Returns true if is already registred.
    @author Stefanos
}
function IsFileTypeRegistered(ExtName: string): Boolean;

{** Queries where a windows extension is registered to.
    @author Stefanos
}
function FileTypeRegisteredTo(ExtName: string): string;

{** Queries the program where the open file action is registered.
    @author Stefanos
}
function FileOpenRegisteredTo(FileTypeName: string): string;

implementation

{$ASSERTIONS ON}

uses Registry, Windows, GenUtils, shlobj;

constructor TGenApplicationOptions.Create;
begin
  inherited Create;
  FLastLogonUser := '';
  FLastLogonService := '';
  FLastLogonServer := 'localhost';
  FLastLogonPort := 5432;
  FLastLogonPassword := '';
  FAutoConnect := False;
  FLocalCoordinateSystem := PC_GR87;
  FDatabaseEncoding := DBENGreekISO;
end;

const
  Key = 4371;

procedure TGenApplicationOptions.LoadFromRegistry;
var
  Registry: TRegistry;
  RootKeys: array[1..2] of HKEY;
  i: Integer;
begin
  RootKeys[1] := HKEY_LOCAL_MACHINE;
  RootKeys[2] := HKEY_CURRENT_USER;
  for i := 1 to 2 do
  begin
    Registry := nil;
    try
      Registry := TRegistry.Create(KEY_READ);
      Registry.RootKey := RootKeys[i];
      if Registry.OpenKey('Software', False) then
        if Registry.OpenKey('Itia', False) then
          Registry.OpenKey('General', False);
      if Registry.ValueExists('LastLogonUser') then
        FLastLogonUser := Registry.ReadString('LastLogonUser');
      if Registry.ValueExists('LastLogonService') then
        FLastLogonService := Registry.ReadString('LastLogonService');
      if Registry.ValueExists('LastLogonServer') then
        FLastLogonServer := Registry.ReadString('LastLogonServer');
      if Registry.ValueExists('LastLogonPassword') then
        FLastLogonPassword := Decrypt(Registry.ReadString('LastLogonPassword'),
          Word(Key));
      if Registry.ValueExists('LastLogonPort') then
        FLastLogonPort := StrToInt(Registry.ReadString('LastLogonPort'));
      if Registry.ValueExists('LocalCoordinateSystem') then
        FLocalCoordinateSystem := TPCSystem(StrToInt(
          Registry.ReadString('LocalCoordinateSystem')));
      if Registry.ValueExists('DatabaseEncoding') then
        FDatabaseEncoding :=
          TDBEncoding(StrToInt(Registry.ReadString('DatabaseEncoding')));
      if Registry.ValueExists('AutoConnect') then
        FAutoConnect := StrToBool(Registry.ReadString('AutoConnect'));
    finally
      Registry.Free;
    end;
  end;
end;

procedure TGenApplicationOptions.WriteToRegistry;
var
  Registry: TRegistry;
begin
  Registry := nil;
  try
    Registry := TRegistry.Create(KEY_ALL_ACCESS);
    Registry.RootKey := HKEY_CURRENT_USER;    
    Assert(Registry.OpenKey('Software', True));
    Assert(Registry.OpenKey('Itia', True));
    Assert(Registry.OpenKey('General', True));
    Registry.WriteString('LastLogonUser', LastLogonUser);
    Registry.WriteString('LastLogonService', LastLogonService);
    Registry.WriteString('LastLogonServer', LastLogonServer);
    Registry.WriteString('LastLogonPassword', Encrypt(LastLogonPassword, Word(Key)));
    Registry.WriteString('LastLogonPort',IntToStr(LastLogonPort));
    Registry.WriteString('AutoConnect', BoolToStr(AutoConnect));
    Registry.WriteString('LocalCoordinateSystem',
      IntToStr(Ord(FLocalCoordinateSystem)));
    Registry.WriteString('DatabaseEncoding', IntToStr(Ord(FDatabaseEncoding)));
  finally
    Registry.Free;
  end;
end;

resourcestring
  rsInvalidTableName = 'SQL error - invalid table name';

function GetGentitiesGTypeOrd(ATableName: string): Integer;
var
  i: Integer;
begin
  Result := 0; {dummy}
  for i := Low(GentitiesTableNames) to High(GentitiesTableNames) do
  begin
    if GentitiesTableNames[i] = ATableName then
    begin
      Result := i;
      Exit;
    end;
  end;
  Assert(False, rsInvalidTableName);
end;

function GetGentitiesGType(ATableName: string): TGentityType;
var
  num: Integer;
begin
  num := GetGentitiesGTypeOrd(ATableName);
  for Result := Low(TGentityType) to High(TGentityType) do
    if Ord(Result) = num then
      Exit;
  Result := igtAllGentities; {dummy}
end;

{Write user values only to HKEY_CURRENT_USER to compe with possible errors}

procedure RegisterFileType(ExtName, ContentTypeName, FileTypeName, AppName,
  Description: string);
var
  Registry :TRegistry;
begin
  Registry := nil;
  try
    Registry := TRegistry.Create;
    with Registry do
    begin
      RootKey:=HKEY_CURRENT_USER;
{Delete previous key values}
      DeleteKey('\Software\Classes\.' + ExtName);
      DeleteKey('\Software\Classes\' + FileTypeName);
{Write file extension}
      OpenKey('\Software\Classes\.' + ExtName, True);
      WriteString('', FileTypeName);
      WriteString('Content Type', ContentTypeName);
      CloseKey;
{Write file type}
      CreateKey('\Software\Classes\'+FileTypeName) ;
      OpenKey('\Software\Classes\'+FileTypeName, False);
      WriteString('',Description);
      CloseKey;
{Workarround here!}
{Not a general case, select the second icon by default.. +'(,1)'}
{Note that hydrognomon.res is hacked in order to contain the second
 icon for time series documents (*.hts files)}
      OpenKey('\Software\Classes\'+FileTypeName + '\DefaultIcon', True) ;
      WriteString('', AppName + ',1');
      CloseKey;
      OpenKey('\Software\Classes\'+FileTypeName + '\shell\open\command', True);
      WriteString('', AppName+ ' "%1"');
      CloseKey;
    end;
  finally
   Registry.Free;
  end;
  SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
end;

{Query HKEY_CLASSES_ROOT rather than HKEY_CURRENT_USER\Software\Classes\ in
 order to get possible values from an installation sceme}

function IsFileTypeRegistered(ExtName: string): Boolean;
var
  Registry: TRegistry;
begin
  Registry := nil;
  try
    Registry := TRegistry.Create;
    Registry.RootKey:=HKEY_CLASSES_ROOT;
    Result := Registry.OpenKey('.' + ExtName, False);
    Registry.CloseKey;
  finally
    Registry.Free;
  end;
end;

function FileTypeRegisteredTo(ExtName: string): string;
var
  Registry: TRegistry;
begin
  Result := '';
  Registry := nil;
  try
    Registry := TRegistry.Create;
    Registry.RootKey:=HKEY_CLASSES_ROOT;
    Registry.OpenKey('.' + ExtName, False);
    Result := Registry.ReadString('');
    Registry.CloseKey;
  finally
    Registry.Free;
  end;
end;

function FileOpenRegisteredTo(FileTypeName: string): string;
var
  Registry: TRegistry;
begin
  Result := '';
  Registry := nil;
  try
    Registry := TRegistry.Create;
    with Registry do
    begin
      RootKey:=HKEY_CLASSES_ROOT;
      if OpenKey(FileTypeName+'\shell\open\command', False) then
        Result := ReadString('');
      CloseKey;
    end;
  finally
    Registry.Free;
  end;
end;

end.
