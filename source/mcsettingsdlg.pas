{******************************************************************}
{                                                                  }
{  LoUISE library                                                  }
{                                                                  }
{  Copyright (c) 2000-2004 National Technical University of Athens }
{                                                                  }
{******************************************************************}

unit mcsettingsdlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TFrmConfIntSettings = class(TForm)
    rgrpMCCount: TRadioGroup;
    rgrpMCPointsCount: TRadioGroup;
    rgrpMCConfidence: TRadioGroup;
    btnOK: TButton;
    btnCancel: TButton;
  private

  public

  end;

implementation

{$R *.DFM}

end.
