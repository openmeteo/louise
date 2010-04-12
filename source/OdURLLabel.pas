unit OdURLLabel;

interface

uses
  SysUtils, Classes, Controls, StdCtrls, Messages, ShellAPI, Windows, Graphics;

type
  TOdURLLabel = class(TCustomLabel)
  private
    { Private declarations }
    fURL : String;
    procedure SetURL(value : string);
    procedure CMTextChanged(var Message: TMessage);
    Message CM_TextChanged;
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create (AOwner : TComponent); override;
    procedure Click; override;
  published
    { Published declarations }
    Property URL : String Read fURL write setURL;
    Property Align;
    Property Alignment;
    Property AutoSize;
    Property Color;
    Property Cursor;
    Property Caption;
    Property Font;
    Property ParentFont;
  end;


implementation

procedure TOdURLLabel.Click;
begin
  ShellExecute(0, 'open', PChar(fURL), nil, nil, SW_SHOWNORMAL);
  inherited;
end;

procedure TOdURLLabel.SetURL(value : string);
begin
  if fURL <> Value then
  begin
    if (Caption = '') or (Caption = fURL) then
      Caption := Value;
    fURL := Value;
  end;
end;

procedure TOdURLLabel.CMTextChanged(var Message: TMessage);
begin
  inherited;
  if Caption = '' then
     Caption:= fURL;
end;

constructor TOdURLLabel.Create(AOwner: TComponent);
begin
  inherited ;
  Font.Color := clBlue;
  Font.Style := [fsUnderline];
  Cursor     := crHandPoint;
end;


end.



