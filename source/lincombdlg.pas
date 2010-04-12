{******************************************************************}
{                                                                  }
{  LoUISE library                                                  }
{                                                                  }
{  Copyright (c) 2000-2004 National Technical University of Athens }
{                                                                  }
{******************************************************************}

unit lincombdlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ImgList, ComCtrls, ToolWin, ExtCtrls, tsgrid, TsDialogs,
  Grids, Ts, Matrix, Dates, StdCtrls, StrGrdOd;

type
  TFrmLinearComb = class(TForm)
    SgrdCoefficients: TOdStringGrid;
    Panel1: TPanel;
    Panel2: TPanel;
    BtnCancel: TButton;
    BtnOK: TButton;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmLinearComb: TFrmLinearComb;

implementation

{$R *.DFM}

procedure TFrmLinearComb.FormShow(Sender: TObject);
begin
  SgrdCoefficients.DefaultRowHeight := SgrdCoefficients.DefaultRowHeight *
    Screen.PixelsPerInch div 96;
end;

end.
