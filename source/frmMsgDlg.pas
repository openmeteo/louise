unit frmMsgDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Contnrs;

type
  TMessageType = (msgInfo, msgWarning, msgError);

  TMessageDialog = class;

  TMsg = class
     msText:String;
     msType:TMessageType;
  end;

  TFrmMessageDlg = class(TForm)
    Panel1: TPanel;
    OKBtn: TButton;
    MemoMessages: TMemo;
    procedure OKBtnClick(Sender: TObject);
  private
    parentDlg:TMessageDialog;
  end;

{** This is a simple message dialog for displaying a summary
    of messages such as errors, warnings and other information}
  TMessageDialog = class (TComponent)
  private
    messageForm:TFrmMessageDlg;
    msList:TObjectList;
  public
  {** Adds a new message to the message list.
      The message consists of a text and a type
  }
    procedure addMsg(txt:String; msTyp:TMessageType);
  {** Clears all messages
  }
    procedure clearMsg;
  {** Shows all messages in an appropriate form
  }
    procedure showMsg;
    function count:Integer;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;


var
  FrmMessageDlg: TFrmMessageDlg;

implementation

{$R *.dfm}


(*********************************************)
(*********************************************)
(*               TMessageDialog                *)
(*********************************************)
(*********************************************)

constructor TMessageDialog.Create(AOwner: TComponent);
begin
     inherited Create(AOwner);
     messageForm := TFrmMessageDlg.Create(Self);
     messageForm.parentDlg:=self;
     msList := TObjectList.create;
end;


destructor TMessageDialog.Destroy;
begin
     msList.free;
     messageForm.Free;
     inherited Destroy;
end;


procedure TMessageDialog.clearMsg;
begin
     msList.clear;
end;

function TMessageDialog.count:Integer;
begin
     result:=msList.Count;
end;

procedure TMessageDialog.addMsg(txt:String; msTyp:TMessageType);
var ms:TMsg;
begin
     ms:=TMsg.create;
     ms.msText:=txt;
     ms.msType:=msTyp;
     msList.add(ms);
end;

resourcestring
    rsInfo    = 'Information';
    rsWarning = 'Warning';
    rsError   = 'Error';


procedure TMessageDialog.showMsg;
var i:Integer;
    messageStr,TypeStr:String;
begin
     messageForm.MemoMessages.Clear;
     for i:=0 to msList.Count-1 do
     begin
          case ((msList[i] as TMsg).msType) of
             msgInfo    : TypeStr := '['+rsInfo+']';
             msgWarning : TypeStr := '['+rsWarning+']    ';
             msgError   : TypeStr := '['+rsError+']      ';
          end;
          messageStr:=TypeStr+' '+(msList[i] as TMsg).msText;
          messageForm.MemoMessages.Lines.Append(messageStr);
     end;
     messageForm.ShowModal;
end;


procedure TFrmMessageDlg.OKBtnClick(Sender: TObject);
begin
     Close;
end;

end.
