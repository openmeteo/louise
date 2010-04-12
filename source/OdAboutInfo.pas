{******************************************************************}
{                                                                  }
{  LoUISE library                                                  }
{                                                                  }
{  Copyright (c) 2000-04 National Technical University of Athens   }
{                                                                  }
{******************************************************************}

unit OdAboutInfo;

interface

uses
  windows,math,SysUtils,ShellApi,Classes,comctrls,ValEdit,
  graphics,StdCtrls,OdURLLabel,forms,Controls, ExtCtrls;

type
  TMyLabel = class(TLabel)
  public
    XSign, YSign: Integer;
    procedure Assign(Source: TPersistent); override;
  end;

type Tpartner = class(TPersistent)
private
   fShow:Boolean;
   fName:string;
   fCopyrights:string ;
   fUrl:string;
   function getName:string;
   procedure setName(value:string);
   function getCopyrights:string;
   procedure setCopyrights(value:string);
   function getURL:string;
   procedure setURL(value:string);

public
published
    Property Show:Boolean
    read fShow write fShow;
    Property Name:String
    read getName write setName;
    Property Copyrights:String
    read getCopyrights write setCopyrights;
    Property URL:String
    read getURL write setURL;
end;

type
  TOdAboutInfo = class(TComponent)
  aform:Tform;
  BtnCloseForm,BtnCredits: TButton;
  amemo:tmemo;
  asc:tscrollbox;
  private
     fPartner1:Tpartner;
     fPartner2:Tpartner;
     fPartner3:Tpartner;
     fImageShow:Boolean;
     FIconShow: Boolean;
     ExitLoop:boolean;
     fEasternEggString:string;
     EasternEgg:STRING;
     fCreditsButtonShow:Boolean;
     fappname:string;
     fappDescription:string;
     Fcredits:TstringList;
     fbigimage:Tpicture;
     fIcon:Tpicture;
     FBuildDate: string;
     Hyd: array of TMyLabel;

     function GetPartner1:Tpartner;
     procedure SetPartner1(value:Tpartner);

     function GetCredits:TstringList;
     procedure SetCredits(value:TstringList);

     function GetPartner2:Tpartner;
     procedure SetPartner2(value:Tpartner);

     function GetPartner3:Tpartner;
     procedure SetPartner3(value:Tpartner);

     function GetEasternEggString:string;
     procedure SetEasternEggString(value:string);

     function GetImageShow:boolean;
     procedure SetImageShow(value:boolean);

     function GetIconShow: Boolean;
     procedure SetIconShow(value: Boolean);

     function GetCreditsButtonShow:boolean;
     procedure SetCreditsButtonShow(value:boolean);

     function GetBigImage:Tpicture;
     procedure SetBigImage(avalue:Tpicture);

     function GetIcon:Tpicture;
     procedure SetIcon(avalue:Tpicture);

     function GetAppDescription:string;

     procedure SetAppDescription(value:string);
     procedure BtnCloseFormclick(sender:tobject);
     procedure BtnCreditsclick(sender:tobject);
     procedure ShowCredits;
     procedure HideCredits;
     procedure aformKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
     procedure aformClose(Sender: TObject; var Action: TCloseAction);

     procedure AnimationTrigger(Sender: TObject);

    { Private declarations }
  protected
    { Protected declarations }
  public
{** function getVersion Extracts the FileVersion element of the VERSIONINFO
   structure that Delphi maintains as part of a project's
   options.
   Results are returned as a standard string.  Failure
   is reported as "".

}
    function getVersion : string;
    property BuildDate: string read FBuildDate write FBuildDate;
    Constructor create(AOwner:TComponent);override;
    destructor destroy;override;
    function execute:boolean;
  published

    Property Partner1:Tpartner
        read GetPartner1 write SetPartner1;

    Property Credits:TstringList
        read GetCredits write SetCredits;


    Property EasternEggString:string
        read GetEasternEggString write SetEasternEggString;

    Property Partner2:Tpartner
        read GetPartner2 write SetPartner2;

    Property Partner3:Tpartner
        read GetPartner3 write SetPartner3;

    Property BigImage:Tpicture
        read GetBigImage write SetBigImage;

      Property Icon:Tpicture
        read GetIcon write SetIcon;

    Property ImageShow:Boolean
        read GetImageShow write SetImageShow;

    property IconShow: Boolean read GetIconShow write SetIconShow default True;

    Property CreditsButtonShow:Boolean
        read GetCreditsButtonShow write SetCreditsButtonShow;

    property ApplicationName: string read fappname write fappname;

    Property AppDescription:String
        read GetAppDescription write SetAppDescription;
end;

var
  IsRunning: Boolean;

implementation

uses GenUtils;

procedure TOdAboutInfo.BtnCloseFormclick(sender:tobject);
begin
     exitloop:=true;
     if BtnCloseForm.Caption<>'Hide' THEN BtnCloseForm.Caption:='OK';
     if amemo.Visible then
     begin
          hideCredits;
          exit;
     end;
     aform.close;
end;
procedure TOdAboutInfo.aformClose(Sender: TObject; var Action: TCloseAction);
begin
     exitloop:=true;
     if BtnCloseForm.Caption<>'Hide' THEN BtnCloseForm.Caption:='OK';
     if amemo.Visible then
     begin
          hideCredits;
          exit;
     end;
     aform.close;
end;

procedure TOdAboutInfo.aformKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
       if (key=VK_ESCAPE) then
       begin
            exitloop:=true;
            EASTERNEGG:='';
            if BtnCloseForm.Caption<>'Hide' THEN BtnCloseForm.Caption:='OK';
            HideCredits;
            exitloop:=false;
            EXIT;
       end;

       if (key=VK_CAPITAL) then exit;

       EasternEgg:=EasternEgg+UPPERCASE(CHAR(KEY));                     

       IF UPPERCASE(EASTERNEGG)=UPPERCASE(EasternEggString) THEN
        ShowCredits
        ELSE
            IF (LENGTH(EasternEgg)>=length(EasternEggString)) or
            (UPPERCASE(EasternEgg[length(EasternEgg)])<>
              UPPERCASE(EasternEggString[length(EasternEgg)])) THEN
              EasternEgg :='';
end;
procedure TOdAboutInfo.HideCredits;
begin
   BtnCloseForm.Caption:='OK';
   if CreditsButtonShow then
   begin
        BtnCredits.Caption:='Credits';
        BtnCredits.Visible:=true;
        BtnCredits.Width:=50;
   END;
        if not amemo.Visible then exit;
        asc.Align:=AlTop;
        repeat
             asc.Height:=asc.Height-2;
             application.ProcessMessages;
             sleep(10);
        until (asc.Height<=4);
        BtnCloseForm.Caption:='OK';
        amemo.visible:=false;
        asc.Height:=1;
        asc.visible:=false;
        application.ProcessMessages;
        aform.Repaint;
end;
procedure TOdAboutInfo.showCredits;
var
  k,h:integer;
begin
   EASTERNEGG:='';
   exitloop:=false;
   if CreditsButtonShow then
   if BtnCredits.Caption='Hide' then
   begin
        HideCredits;
        exit;
   end;
   if CreditsButtonShow then
   begin
       amemo.Top:=1;
       BtnCredits.Caption:='Hide';
       BtnCredits.Visible:=false;
       BtnCredits.Width:=50;
       application.ProcessMessages;
    end;
    with asc do
    begin
      Visible:=true;
      Align:=AlTop;
      Align:=Alnone;
      Height:=1;
      AutoScroll:=false;
      Height:=1;
      Color:=clSkyBlue;
      width := aform.ClientWidth;
    end;
    with amemo do
    begin
        Visible:=true;
        Left:=0;
        Height:=aform.Height-80;
        width:=asc.Width;
        Lines.BeginUpdate;
        Color:=clSkyBlue;
        Font.Color:=clMenuHighlight;
        Font.Style:=[fsbold];
        Lines.Assign(Credits);
        Font.height:=11;
        h:=aform.Canvas.TextHeight(Text);
//        Height:=(Lines.Count+6)*h;
        BorderStyle:=bsnone;
        Alignment:=TaCenter;
        Lines.EndUpdate;
        ScrollBars := ssVertical;
        repaint;
    end;
    application.ProcessMessages;
    repeat
         asc.Height:=asc.Height+3;
         application.ProcessMessages;
         if exitloop then break;
         sleep(10);
    until (asc.Height>=aform.Height-30);
    amemo.Align:=alnone;
    k:=0;
    amemo.SelStart:=0;
    if amemo.Height>aform.Height then
    repeat
         amemo.top:=amemo.top-1;
         application.ProcessMessages;
         if exitloop then break;
         sleep(18);
         inc(k);
    until (k>= amemo.height-aform.Height);
    if amemo.Visible then BtnCloseForm.Caption:='Hide';
    BtnCloseForm.Visible:=true;
    BtnCloseForm.BringToFront;
    application.ProcessMessages;
end;

procedure TOdAboutInfo.BtnCreditsclick(sender:tobject);
begin
     ShowCredits;
end;

function TOdAboutInfo.getVersion : string;
begin
  Result := GetFileVersionStr(paramstr(0), ver3ItemsPlusParentheses);
end;

Constructor TOdAboutInfo.create(AOwner:TComponent);
begin
     inherited create(AOwner);
     fbigimage:=Tpicture.Create;
     fIcon:=Tpicture.Create;
     fcredits:=Tstringlist.Create;
     CreditsButtonShow:=false;
     if ficon=nil then
     ficon.Assign(application.Icon);
     fPartner1:=Tpartner.Create;
     fPartner2:=Tpartner.Create;
     fPartner3:=Tpartner.Create;
     FBuildDate := '';
     fEasternEggString:='Eastern egg text';
     fappname := 'Enter application name here';
     with fpartner1 do
     begin
         fShow:=True;
         fName:='N. T. University of Athens (ITIA research team)';
         fCopyrights:='Copyrights 1997-2005';
         fURL:='www.ntua.gr';
     end;

     with fpartner2 do
     begin
         fShow:=True;
         fPartner2.fName:='NAMA Consulting Engineers and Planners S.A.';
         fCopyrights:='Copyrights 2004-2005';
         fURL:='www.namanet.gr';
     end;

     with fpartner3 do
     begin
         fShow:=False;
         fName:='MDS Marathon Data Systems';
         fCopyrights:='Copyrights 2004-2005';
         fURL:='www.marathondata.gr';
     end;
     fAppDescription:='Application Description';
end;

destructor TOdAboutInfo.destroy;
begin
    if fbigimage <> nil then fbigimage.Free;
    if fIcon <> nil then fIcon.Free;
    if fPartner1 <> nil then fPartner1.Free;
    if fPartner2 <> nil then fPartner2.Free;
    if fPartner3 <> nil then fPartner3.Free;
    if fCredits  <> nil then fCredits.Free;
    inherited destroy;
end;

function TOdAboutInfo.GetBigImage:Tpicture;
begin
     result:=fbigimage;
end;

procedure TOdAboutInfo.SetBigImage(avalue:Tpicture);
begin
      fbigimage.Assign(avalue);
end;

function TOdAboutInfo.GetAppDescription: string;
begin
      result:=FAppDescription
end;

procedure TOdAboutInfo.SetAppDescription(value: string);
begin
     FAppDescription:=value;
end;

resourcestring
  rsAbout = 'About';
  rsVersion = 'Version: ';
  rsCredits = 'Credits';

function TOdAboutInfo.execute: boolean;
var
    lblCopyrights1,lblCopyrights2,
    lblCopyrights3,lblPartner1,
    lblPartner2,lblPartner3,
    lblApplicationTitle,LblVersion,
    lblDescription: TLabel;
    abevel:Tbevel;
    OdURLLabel1,OdURLLabel2,OdURLLabel3:TOdURLLabel;
    aIcon,aImage:Timage;
    i,maxwidth,offset:integer;

    procedure CreateTitle(ACaption: string; AParentForm: TForm; ALeft,
      ATop: Integer);
    var
      i: Integer;
      s: string;
      ADummy: TMyLabel;
    begin
      if aparentform.ControlCount>= 1 then
      begin
        ALeft:=aform.Controls[aparentform.ControlCount-1].left+aleft;
        ATop:=aform.Controls[aparentform.ControlCount-1].top+
          aform.Controls[aparentform.ControlCount-1].Height+atop;
      end else
      begin
        ALeft:=aform.Controls[0].left+Aleft;
        ATop:=aform.Controls[0].top+atop+
        aform.Controls[0].Height;
      end;
      s := ACaption;
      SetLength(Hyd, Length(s));
      for i := 0 to Length(s)-1 do
      begin
        Hyd[i] := TMyLabel.Create(AParentForm);
        with Hyd[i] do
        begin
          Parent := AParentForm;
          Font.Size := 18;
          Font.Name := 'Arial';
          Font.Style:=[fsbold];
          Font.Color := clBlack;
          Caption := s[i+1];
          Top := ATop;
          if i = 0 then Left := ALeft else
            Left := Hyd[i-1].Left+Hyd[i-1].Width;
          OnClick := AnimationTrigger;
          XSign := 1;
          YSign := 1;
          Transparent := True;
        end;
      end;
      ADummy := TMyLabel.Create(AParentForm);
      ADummy.Parent := AParentForm;
      ADummy.Assign(Hyd[0]);
      ADummy.Visible := False;
      IsRunning := False;
    end;

    function CreateLbl(acaption:string;aparentform:Tform;aleft,atop:integer):Tlabel;
    begin
         result:=TLabel.Create(aparentform);
         result.Parent:=aparentform;
         if  aparentform.ControlCount>= 2 then
         begin
               result.Left:=aform.Controls[aparentform.ControlCount-2].left+aleft;
               result.Top:=aform.Controls[aparentform.ControlCount-2].top+
               aform.Controls[aparentform.ControlCount-2].Height
               +atop
         end
         else
         begin
               result.Left:=aform.Controls[aparentform.ControlCount-1].left+Aleft;
               result.Top:=aform.Controls[aparentform.ControlCount-1].top+atop+
               aform.Controls[aparentform.ControlCount-1].Height

         end;
         result.Caption:= Acaption;
    end;

    function CreateLblURL(acaption,aurl:string;aparentform:Tform;aleft,atop:integer):TOdURLLabel;
    begin
         result:=TOdURLLabel.Create(aparentform);
         result.Parent:=aparentform;
         if  aparentform.ControlCount>= 2 then
         begin
               result.Left:=aform.Controls[aparentform.ControlCount-2].left+aleft;
               result.Top:=aform.Controls[aparentform.ControlCount-2].top+
               aform.Controls[aparentform.ControlCount-2].Height
               +atop
         end
         else
         begin
               result.Left:=aform.Controls[aparentform.ControlCount-1].left+Aleft;
               result.Top:=aform.Controls[aparentform.ControlCount-1].top+atop+
               aform.Controls[aparentform.ControlCount-1].Height

         end;
         result.Caption:= Acaption;
         result.URL:= Acaption;
    end;
begin
     exitloop:=false;
     
     try
         aform:=TForm.Create(self);
         with aform do
         begin
             DoubleBuffered := True;
             BorderStyle:=bsDialog;
             KeyPreview:=true;
             OnKeyDown:=aformKeyDown;
             OnClose:=aformClose;
             Width:=260;
             Height:=370;
             Position:=poMainFormCenter;
             Caption:= rsAbout+': '+fappname;
         end;
         aImage:=Timage.Create(self);
         aImage.Parent:=aform;
         offset:=3;
         if (ImageShow) and (bigimage.Graphic <>nil) then
         begin
             with aimage do
             begin
                 Picture:=BigImage;
                 AutoSize:=false;
                 Stretch:=false;
                 Width:=Picture.Width;
                 Height := Picture.Height;
                 AForm.Height := Max(Height+50, AForm.Height);
                 Top:=5;
                 Left:=5;
                 offset:=Width+5
             end;
         end;
         aicon:=Timage.Create(self);
         with aicon do
         begin
               Parent:=aform;
               Left:=offset+10;
               Top:=5;
               if FIconShow then
               begin
                 if icon.Graphic=nil then
                   Picture.Assign(application.Icon)
                 else
                   Picture.Assign(Icon);
               end;
               Visible:=true;
               AutoSize:=true;
               Height:=application.Icon.Height;
               width:=application.Icon.width;
         end;

        CreateTitle(fappname,aform,0,5);
{        lblApplicationTitle:=Createlbl(fappname,aform,0,5);
         with lblApplicationTitle.Font do
         begin
             Name := 'Arial';
             Color:=ClBlack;
             Style:=[fsbold];
             Size:=18;
         end;}

         LblVersion:=Createlbl(getVersion,aform,0,5);
         with LblVersion do
         begin
             Caption:=rsVersion+Caption;
             if FBuildDate<>'' then
               Caption := Caption + ' ['+FBuildDate+']';
             Font.Color:=clNavy;
         end;
             lblDescription:=Createlbl(GetAppDescription,aform,0,5);
             with lblDescription do
             begin
                 Font.Color:=clInactiveCaption;
                 WordWrap:=true;
                 Width:=250;
                 height:=30;
                 AutoSize:=true;
             end;
         abevel:=tbevel.Create(aform);
         with abevel do
         begin
             Parent:=aform;
             Width:=50;
             Height:=2;
             Style:=bsLowered;
             Left:=10+offset;
             Top:= aform.Controls[aform.ControlCount-2].top+aform.Controls[aform.ControlCount-2].Height+2;
             Visible:=true;
         end;
         if partner1.fShow then
         begin
               lblPartner1:=Createlbl(Partner1.Name,aform,0,15);
               lblCopyrights1:=Createlbl(Partner1.Copyrights,aform,0,3);
               OdURLLabel1:= CreateLblURL(Partner1.URL,Partner1.URL,aform,0,3);
         end;
         if partner2.fShow then
         begin
               lblPartner2:=Createlbl(Partner2.Name,aform,0,10);
               lblCopyrights2:=Createlbl(Partner2.Copyrights,aform,0,3);
               OdURLLabel2:= CreateLblURL(Partner2.URL,Partner2.URL,aform,0,3);
         end;
         if partner3.fShow then
         begin
               lblPartner3:=Createlbl(fPartner3.Name,aform,0,10);
               lblCopyrights3:=Createlbl(fPartner3.Copyrights,aform,0,3);
               OdURLLabel3:= CreateLblURL(fPartner3.URL,fPartner3.URL,aform,0,3);
         end;
         maxwidth:=0;
         for i:=0 to aform.ControlCount-1 do
         begin
              if maxwidth<=aform.Controls[i].Width then
                    maxwidth:=aform.Controls[i].Width
         end;
         if Length(Hyd)>1 then
           if maxwidth<=(Hyd[Length(Hyd)-1].Left-Hyd[0].Left+Hyd[Length(Hyd)-2].Width) then
             maxwidth := (Hyd[Length(Hyd)-1].Left-Hyd[0].Left+Hyd[Length(Hyd)-2].Width);
         aform.Width:=maxwidth+20+offset;
         BtnCloseForm:=TButton.Create(aform);
         with BtnCloseForm do
         begin
             Parent:=aform;
             Cancel:=true;
             Caption:='OK';
             Left:=aform.Width-70;
             Top:=aform.Controls[aform.ControlCount-2].top+aform.Controls[aform.ControlCount-2].Height+12;
             Visible:=true;
             Width:=60;
             onClick:=BtnCloseFormClick;
         end;

         abevel.Width:=aform.Width-100;

         aform.Height := aform.Controls[aform.ControlCount-1].Top+                  
           aform.Controls[aform.ControlCount-1].Height+50;
         AForm.Height := max(AForm.Height, AImage.Height+50);

         if partner1.Show or partner2.Show or partner3.Show then
         begin
         abevel:=tbevel.Create(aform);
         with abevel do
         begin
             Parent:=aform;
             Width:=aform.Width-30;
             Height:=2;
             Style:=bsLowered;
             Left:=10+offset;
             Top:= aform.Controls[aform.ControlCount-3].top+aform.Controls[aform.ControlCount-3].Height+5;
             Visible:=true;
         end;
         end;
//         aImage.Height:=BtnCloseForm.BoundsRect.Bottom-5;

         if (ImageShow) and (bigimage.Graphic<>nil) then
         begin
             abevel:=tbevel.Create(aform);
             with abevel do
             begin
                 Parent:=aform;
                 BoundsRect:=Aimage.BoundsRect;
                 Style:=bsLowered;
                 Visible:=true;
             end;
         end;
         abevel:=tbevel.Create(aform);
         with abevel do
         begin
             Parent:=aform;
             BoundsRect:=aIcon.BoundsRect;
             Style:=bsRaised;
             Visible:=true;
         end;
         aimage.Repaint;
         aform.Repaint;
         BtnCloseForm.Top := Aform.ClientHeight-28;
         if CreditsButtonShow then
         begin
               BtnCredits:=TButton.Create(aform);
           with BtnCredits do
           begin
               Parent:=aform;
               Caption:=rsCredits;
               Left:=abevel.Left;
               Width:=50;
               Top:=BtnCloseForm.Top;
               onClick:=BtnCreditsClick;
           end;
         end;
         asc:=Tscrollbox.Create(aform);
         asc.parent:=aform;
         amemo:=Tmemo.Create(asc);
         amemo.Parent:=asc;
         amemo.Visible:=false;
         asc.Visible:=false;
         application.ProcessMessages;
         aform.Showmodal;
         result:=true;
     finally
         if aform<>nil then aform.Release
     end;
end;
{ Tpartner }


function Tpartner.getname: string;
begin
    result:=fName
end;

function Tpartner.getCopyrights: string;
begin
     result:=fCopyrights;
end;

function Tpartner.getURL: string;
begin
     result:=fURL;
end;


procedure Tpartner.setname(value: string);
begin
      fname:=value;
end;

procedure Tpartner.setCopyrights(value: string);
begin
     fCopyrights:=value;
end;

procedure Tpartner.setURL(value: string);
begin
     fURL:=value;
end;


function TOdAboutInfo.GetPartner1: Tpartner;
begin
    result:=fPartner1;
end;

procedure TOdAboutInfo.SetPartner1(value: Tpartner);
begin
      fPartner1:=value;
end;

function TOdAboutInfo.GetPartner2: Tpartner;
begin
    result:=fPartner2;
end;

procedure TOdAboutInfo.SetPartner2(value: Tpartner);
begin
      fPartner2:=value;
end;

function TOdAboutInfo.GetPartner3: Tpartner;
begin
    result:=fPartner3;
end;

procedure TOdAboutInfo.SetPartner3(value: Tpartner);
begin
      fPartner3:=value;
end;

function TOdAboutInfo.GetImageShow: boolean;
begin
     result:=fImageShow;
end;

procedure TOdAboutInfo.SetImageShow(value:boolean);
begin
     fImageShow:=value;
end;

function TOdAboutInfo.GetIconShow: boolean;
begin
     result:=fIconShow;
end;

procedure TOdAboutInfo.SetIconShow(value:boolean);
begin
     fIconShow:=value;
end;

function TOdAboutInfo.GetIcon: Tpicture;
begin
     result:=fIcon;
end;

procedure TOdAboutInfo.SetIcon(avalue: Tpicture);
begin
      fIcon.Assign(avalue);
end;

function TOdAboutInfo.GetCredits: TstringList;
begin
      result:=fcredits;
end;

procedure TOdAboutInfo.SetCredits(value: TstringList);
begin
      fcredits.Assign(value);
end;

function TOdAboutInfo.GetCreditsButtonShow: boolean;
begin
       result:=fCreditsButtonShow;
end;

procedure TOdAboutInfo.SetCreditsButtonShow(value: boolean);
begin
     fCreditsButtonShow:=value;
end;

function TOdAboutInfo.GetEasternEggString: string;
begin
     result:=fEasternEggString;
end;

procedure TOdAboutInfo.SetEasternEggString(value: string);
begin
      fEasternEggString:=value;
end;

procedure TOdAboutInfo.AnimationTrigger(Sender: TObject);
var
  i: Integer;
  ASize: Integer;
  SavedMouse: TPoint;
begin
  IsRunning := not IsRunning;
  if not IsRunning then Exit;
  ASize := Length(Hyd);
  SavedMouse.X := Mouse.CursorPos.X-Hyd[0].Left;
  SavedMouse.Y := Mouse.CursorPos.Y-Hyd[0].Top;
  repeat
    Hyd[0].Top :=  Mouse.CursorPos.Y - SavedMouse.Y;
    Hyd[0].Left := Mouse.CursorPos.X - SavedMouse.X;
    for i := 1 to ASize -1 do
    begin
      Hyd[i].Top  := Hyd[i].Top  + (Hyd[0].Top-Hyd[i].Top) div (i+3);
      Hyd[i].Left := Hyd[i].Left + ((Hyd[i-1].Left+Hyd[i-1].Width)-Hyd[i].Left) div 4;
    end;
    Sleep(10);
    Application.ProcessMessages;
    If not IsRunning then Break;
  until False;
end;

procedure TMyLabel.Assign(Source: TPersistent);
begin
  if Source is TMyLabel then
    with Source as TMyLabel do
    begin
      Self.Width := Width;
      Self.Height := Height;
      Self.Caption := Caption;
      Self.Color := Color;
      Self.XSign := XSign;
      Self.YSign := YSign;
      Self.Left := Left;
      Self.Top := Top;
      Self.Font.Assign(Font);
  end else
    inherited Assign(Source);
end;

end.

