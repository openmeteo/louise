{******************************************************************}
{                                                                  }
{  LoUISE library                                                  }
{                                                                  }
{  Copyright (c) 2000-2005 National Technical University of Athens }
{                                                                  }
{******************************************************************}

unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TFrmMain = class(TForm)
    ChkInstallLibrary: TCheckBox;
    ChkInstallHelp: TCheckBox;
    BtnInstall: TButton;
    LblWillInstall: TLabel;
    Label2: TLabel;
    Label1: TLabel;
    procedure FormShow(Sender: TObject);
    procedure BtnInstallClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.DFM}

uses IFile, Registry, FileCtrl, GenUtils, iStrUtils;

procedure TFrmMain.FormShow(Sender: TObject);
resourcestring
  rsThisProgramWillInstallLouise = 'This program will install LoUISE';
  rsMaybeLouiseBplDoesNotExist =
    'Make sure louise.bpl, louise.dcp, '+
   'louisedsgn.bpl and louisedsgn.dcp are in the path from which'+
    #13#10'this installer is run, and that it contains version information.';
  rsProgramName = 'louise installer';
  rsFileNotFound = 'File not found';
begin
  Caption := rsProgramName+' '+
    GetFileVersionStr(ParamStr(0), ver3ItemsPlusParentheses);
  try
    LblWillInstall.Caption := rsThisProgramWillInstallLouise+' '+
      GetFileVersionStr(ExtractFilePath(ParamStr(0))+'\louise.bpl',
        ver3ItemsPlusParentheses);
    if (not FileExists(ExtractFilePath(ParamStr(0))+'\louise.dcp')) or
       (not FileExists(ExtractFilePath(ParamStr(0))+'\louisedsgn.bpl')) or
       (not FileExists(ExtractFilePath(ParamStr(0))+'\louisedsgn.dcp')) then
       raise Exception.Create(rsFileNotFound);
    ChkInstallLibrary.Enabled := True;
  except
    on E: Exception do
    begin
      E.Message := E.Message+#13#10+rsMaybeLouiseBplDoesNotExist;
      raise;
    end;
  end;
end;

var
  DelphiDir, HelpDir, BplDir, DefaultDir: string;

resourcestring
  rsCannotOpenRegistryKey = 'Cannot open registry key';

procedure DetermineInstallDirs;
var
  Registry: TRegistry;
begin
  Registry := nil;
  try
    Registry := TRegistry.Create;
    Registry.RootKey := HKEY_LOCAL_MACHINE;
    if not Registry.OpenKeyReadOnly('Software')
    or not Registry.OpenKeyReadOnly('Borland')
    or not Registry.OpenKeyReadOnly('Delphi')
    or not Registry.OpenKeyReadOnly('7.0') then
      raise Exception.Create
        (rsCannotOpenRegistryKey+
        ' HKEY_LOCAL_MACHINE\Software\Borland\Delphi\7.0');
    DelphiDir := Registry.ReadString('RootDir');
    BplDir := DelphiDir+'Projects\Bpl';
    HelpDir := DelphiDir+'Help';
    DefaultDir := DelphiDir+'Projects';
  finally
    Registry.Free;
  end;
end;

procedure InstallLibrary;
var
  Registry: TRegistry;
begin
  FileCopy(ExtractFilePath(ParamStr(0))+'\louise.bpl', BplDir+'\louise.bpl');
  FileCopy(ExtractFilePath(ParamStr(0))+'\louise.dcp', BplDir+'\louise.dcp');
  FileCopy(ExtractFilePath(ParamStr(0))+'\louisedsgn.bpl',
    BplDir+'\louisedsgn.bpl');
  FileCopy(ExtractFilePath(ParamStr(0))+'\louisedsgn.dcp',
    BplDir+'\louisedsgn.dcp');
  Registry := nil;
  try
    Registry := TRegistry.Create;
    Registry.RootKey := HKEY_CURRENT_USER;
    if not Registry.OpenKey('Software\Borland\Delphi\7.0\Known Packages', False)
    then
      raise Exception.Create
        (rsCannotOpenRegistryKey+
        ' HKEY_CURRENT_USER\Software\Borland\Delphi\7.0\Known Packages');
    Registry.WriteString(BplDir+'\louisedsgn.bpl',
      'Library of User Interface Special Edition'+
      ' - design time code');
    Registry.CloseKey;
  finally
    Registry.Free;
  end;
end;

procedure InstallHelp;
begin
  FileCopy(ExtractFilePath(ParamStr(0))+'\louise.hlp', HelpDir+'\louise.hlp');
  FileCopy(ExtractFilePath(ParamStr(0))+'\louise.toc', HelpDir+'\louise.toc');
  FileCopy(ExtractFilePath(ParamStr(0))+'\louise.als', HelpDir+'\louise.als');
  FileCopy(ExtractFilePath(ParamStr(0))+'\louise.als',
    DefaultDir+'\louise.als');
  if not Grep(HelpDir+'\d7.ohc', ':INCLUDE louise.toc', []) then
    AppendToFile(HelpDir+'\d7.ohc', ':INCLUDE louise.toc'#13#10);
  if not Grep(HelpDir+'\d7.ohl', ':LINK louise.hlp', []) then
    AppendToFile(HelpDir+'\d7.ohl', ':LINK louise.hlp'#13#10);
  if not Grep(HelpDir+'\d7.ohi', ':INDEX louise=louise.hlp', []) then
    AppendToFile(HelpDir+'\d7.ohi', ':INDEX louise=louise.hlp'#13#10);
  if FileExists(HelpDir+'\d7.gid') then
    DeleteFile(HelpDir+'\d7.gid');
end;

procedure TFrmMain.BtnInstallClick(Sender: TObject);
resourcestring
  rsLouiseHasBeenInstalled = 'LoUISE has been successfully installed.';
var SavedCursor: TCursor;
begin
  SavedCursor := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;
    DetermineInstallDirs;
    if ChkInstallLibrary.Checked and ChkInstallLibrary.Enabled then
      InstallLibrary;
    if ChkInstallHelp.Checked then InstallHelp;
  finally
    Screen.Cursor := SavedCursor;
  end;
  ShowMessage(rsLouiseHasBeenInstalled);
  Close;
end;

end.
