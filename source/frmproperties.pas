{******************************************************************}
{                                                                  }
{  LoUISE library                                                  }
{                                                                  }
{  Copyright (c) 2001-2005 National Technical University of Athens }
{                                                                  }
{******************************************************************}

unit frmproperties;                 

interface
                              
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, ExtCtrls, Grids, StrGrdOd, ToolWin, ComCtrls, StdCtrls, TypInfo;


const isNil=-31111;


type
  TPropertiesDialog = class;
  TNewObjFunct     = function(var gent:TObject):Boolean;
  TUpdateObjFunct  = function(obj:TObject):Boolean;
  TRestoreObjFunct = function(obj:TObject):Boolean;
  TGetObjFunct     = function(var obj:TObject):Boolean;

  TFrmPropertiesDlg = class(TForm)
    PanelPropertiesDlg: TPanel;
    ImageList1: TImageList;
    Panel2: TPanel;
    Button1: TButton;
    ToolBar1: TToolBar;
    BtnNew: TToolButton;
    ToolButton1: TToolButton;
    BtnDelete: TToolButton;
    BtnGet: TToolButton;
    BtnRestore: TToolButton;
    StrGrdProp: TOdStringGrid;
    procedure StrGrdPropDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure StrGrdPropMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BtnNewClick(Sender: TObject);
    procedure StrGrdPropDblClick(Sender: TObject);
    procedure BtnDeleteClick(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure StrGrdPropMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BtnRestoreClick(Sender: TObject);
    procedure BtnGetClick(Sender: TObject);
  private
    ascendingCol  : Boolean;
    parentDlg     : TPropertiesDialog;
    local_info    : PTypeInfo;
    local_objList : TList;
    local_row     : Integer;
    procedure callUpdateFkt;
    procedure reorderList;
    function execute(info:PTypeInfo; ObjList:TList):Boolean;
    function executeDontShow(info:PTypeInfo; ObjList:TList):Boolean;
    procedure setData;
  public
    constructor Create(AOwner: TComponent); override;
    { Public declarations }
  end;



{** A dialog, which shows the properties of any given List
    of objects (TList) in a tabular form.
}
  TPropertiesDialog = class(TComponent)
  private
    FCaption        : string;
    FReorderAllowed : Boolean;
  public
    PropertyForm    : TFrmPropertiesDlg;
    PanelPropertiesDlg: TPanel;
    newObj          : TNewObjFunct;
    updateObj       : TUpdateObjFunct;
    restoreObj      : TRestoreObjFunct;
    getObj          : TGetObjFunct;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
{** The caption of the dialog window.}
    property Caption: string read FCaption write FCaption;
{** Controls if reordering of the objects in the list is allowed.
    In case of true the user may change the order of the grid
    rows. The object list is reordered accordingly.
    In case of false, the user may reorder the rows of the grid,
    but the changes do not affect the order in the list. False is
    the default value.
    @SeeAlso <See Method=execute>
}
    property ReorderAllowed: Boolean read FReorderAllowed write FReorderAllowed;
{** A properly initialized dialog form is shown. The columns of
    the grid represent the properties and the rows represent the
    objects of the list. Only properties with the following type
    are shown:
     * Integer
     * Real
     * String
     * Boolean
     * Enumeration
    Writable property cells appear in different backgound color
    than read-only properties. One may reorder the objects
    in the list by changing the row position.
    If a newObj function is defined the user may create a new
    object by calling the object form. If an updateObj function
    is defined the user may modify the object properties by
    double-clicking on the objects row and calling the object form.
    @SeeAlso <See Property=ReorderAllowed>
}
    function execute(info:PTypeInfo; ObjList:TList):Boolean;
{** Similar to execut, but without showing the form. Use the routine
    to set the data records and then replace the PanelPropertiesDlg
    by changing its parent}
    function executeDontShow(info:PTypeInfo; ObjList:TList):Boolean;
  end;

  
{** Takes an object and the property name and returns the
    contents of the property, converting it to a string.
    The type of the property must be one of the following:
     * Integer
     * Real
     * String 
     * Boolean
     * Enumeration
}
function GetPropertyVal(Obj: TObject; propStr: string): string;
{** Assigns a value to the property of the object. The type of
    the value must match with the one of the property and
    must be one of the following:
     * Integer
     * Real
     * String  
     * Boolean
     * Enumeration
}
//procedure AssignPropertyVal(Obj: TObject; propStr: string; valStr:string);
function propertyIsReadOnly(grid1:TStringGrid; ACol, ARow:Integer):Boolean;

var
  FrmPropertiesDlg: TFrmPropertiesDlg;

implementation

{$R *.dfm}
uses math;


(*********************************************)
(*********************************************)
(*             TPropertiesDialog             *)
(*********************************************)
(*********************************************)
resourcestring
  rsObjectProperties='Object Properties';

constructor TPropertiesDialog.Create(AOwner: TComponent);
begin
     inherited Create(AOwner);
     FCaption               := rsObjectProperties;
     FReorderAllowed        := false;
     PropertyForm           := TFrmPropertiesDlg.Create(Self);
     PropertyForm.parentDlg := self;
     PanelPropertiesDlg     := PropertyForm.PanelPropertiesDlg;
end;


destructor TPropertiesDialog.Destroy;
begin
     PropertyForm.free;
     inherited Destroy;
end;


function TPropertiesDialog.execute(info:PTypeInfo; ObjList:TList):Boolean;
begin                                               
   //allow/prevent row movement
     with PropertyForm do
       if ReorderAllowed then StrGrdProp.Options := StrGrdProp.Options + [goRowMoving]
       else                   StrGrdProp.Options := StrGrdProp.Options - [goRowMoving];

     result := PropertyForm.execute(info, ObjList);
end;

function TPropertiesDialog.executeDontShow(info:PTypeInfo; ObjList:TList):Boolean;
begin              
   //allow/prevent row movement
     with PropertyForm do
       if ReorderAllowed then StrGrdProp.Options := StrGrdProp.Options + [goRowMoving]
       else                   StrGrdProp.Options := StrGrdProp.Options - [goRowMoving];

     result := PropertyForm.executeDontShow(info, ObjList);
end;


(*********************************************)
(*********************************************)
(*             TFrmPropertiesDlg             *)
(*********************************************)
(*********************************************)
constructor TFrmPropertiesDlg.Create(AOwner: TComponent);
begin
     inherited create(AOwner);
     ascendingCol := true;
end;

function TFrmPropertiesDlg.execute(info:PTypeInfo; ObjList:TList):Boolean;
var i:Integer;
begin
     local_info    := info;
     local_objList := objList;
   //clear the grid cells
     for i:=0 to StrGrdProp.RowCount-1  do StrGrdProp.Rows[i].Clear;
   //set the data
     setData;
   //showmodal
     Caption := parentDlg.Caption;
     if showModal= mrOK then
     begin
          result:=true;
       //(data already changed by now)
          //getData(info, ObjList);
     end
     else result:=false;
end;

function TFrmPropertiesDlg.executeDontShow(info:PTypeInfo; ObjList:TList):Boolean;
var i:Integer;
begin
     local_info    := info;
     local_objList := objList;
   //clear the grid cells
     for i:=0 to StrGrdProp.RowCount-1  do StrGrdProp.Rows[i].Clear;
   //set the data
     setData;

     { The author of this function forgot to tell it to return a result and
     ignored the compiler warning. He also didn't document what this function
     is supposed to do. I'm adding the statement below so that there is no
     compiler warning, but, frankly, I don't know what the correct fix should
     be. A.X. 2014-10-24. }
     Result := False;
end;

procedure TFrmPropertiesDlg.setData;
var i,j,propCount:Integer;
    obj1:TObject;
    data:     PTypeData;
    propList: PPropList;
    stringVal:String;
begin
     StrGrdProp.RowCount := max(2,local_ObjList.Count+1);
     data := GetTypeData(local_info);
     GetMem(propList, data^.PropCount * SizeOf(PPropInfo));
     try
        propCount := GetPropList(local_info, tkAny,  propList, false);
        StrGrdProp.ColCount := propCount+1;
        for j:=0 to propCount-1 do
          StrGrdProp.Cells[j+1,0] := string(propList^[j]^.Name);
        for i:=0 to local_ObjList.Count-1 do
        begin
             obj1:=local_ObjList[i];
             StrGrdProp.Cells[0,i+1]   := IntToStr(i+1);
             StrGrdProp.Objects[0,i+1] := obj1; //associate the object with the first cell of the row
             for j:=0 to propCount-1 do
             begin
                 stringVal := GetPropertyVal(obj1, string(propList^[j]^.Name));
                 if stringVal=FloatToStr(isNil) then
                      StrGrdProp.Cells[j+1,i+1] := ''
                 else StrGrdProp.Cells[j+1,i+1] := stringVal;
             end;
        end;
     finally
        FreeMem(propList, data^.PropCount * SizeOf(PPropInfo));
     end;
end;

(*
procedure TFrmPropertiesDlg.getData(info:PTypeInfo; ObjList:TList);
var i,j:Integer;
    obj1:TObject;
begin
     for i:=1 to StrGrdProp.RowCount-1 do
     begin
          obj1 := StrGrdProp.Objects[0,i];
          if obj1<>nil then
            for j:=1 to StrGrdProp.ColCount-1 do
               AssignPropertyVal(obj1, StrGrdProp.Cells[j,0], StrGrdProp.Cells[j,i]);
     end;
end;
*)

procedure TFrmPropertiesDlg.reorderList;
var i:Integer;
    obj1:TObject;
begin
   //reorder the list
     if parentDlg.ReorderAllowed then
     begin
          for i:=1 to StrGrdProp.RowCount-1 do
          begin
               obj1 := StrGrdProp.Objects[0,i];
               StrGrdProp.Cells[0,i] := IntToStr(i);
               if obj1<>nil then local_objList.Move(local_objList.IndexOf(obj1),i-1);
          end;
     end;
end;


procedure TFrmPropertiesDlg.StrGrdPropDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
var clOldBrushColor,clOldFontColor:TColor;
begin
  With (Sender as TStringGrid) do
  begin
    clOldBrushColor:=Canvas.Brush.Color;
    clOldFontColor :=Canvas.font.Color;
    if (Arow>0) and (Acol>0) then
      if selection.top=ARow then
      begin
         Canvas.Brush.Color := $00804000;
         Canvas.font.Color  := clWhite;
      end
      else begin
         Canvas.font.Color := clOldFontColor;
         if propertyIsReadOnly(TStringGrid(Sender), ACol, ARow) then
            Canvas.Brush.Color := clInactiveBorder
         else
            Canvas.Brush.Color := clCream;
      end;
    canvas.textrect (rect,rect.left+2,rect.top+0,cells[acol,arow]);
    Canvas.Brush.Color := clOldBrushColor;
    Canvas.font.Color  := clOldFontColor;
  end;
end;

procedure TFrmPropertiesDlg.StrGrdPropMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var  ACol, ARow: Longint;
begin
     with TStringGrid(Sender) do
     begin
          MouseToCell(X, Y, ACol, ARow);
          local_row := ARow;
          if (ARow=0) and (ACol>0) then
          begin
               if AscendingCol then StrGrdProp.SortStringgrid(ACol, true)
               else                 StrGrdProp.SortStringgrid(ACol, false);
               AscendingCol := not AscendingCol;
          end;
     end;
end;

resourcestring
  rsNoOperationSupported = 'Operation not supported';

procedure TFrmPropertiesDlg.BtnNewClick(Sender: TObject);
var obj1:TObject;
    i:Integer;
begin
     try
        parentDlg.newObj(obj1);
     except
        MessageDlg(rsNoOperationSupported, mtWarning, [mbOK], 0);
     end;
   //clear the grid cells
     for i:=0 to StrGrdProp.RowCount-1  do StrGrdProp.Rows[i].Clear;
   //set the data
     setData;
end;

procedure TFrmPropertiesDlg.callUpdateFkt;
var obj1:TObject;
    i:Integer;
begin
     if StrGrdProp.selection.top<1 then exit;
     obj1 := StrGrdProp.Objects[0,StrGrdProp.selection.top];
     if obj1=nil then exit;
     try
        parentDlg.UpdateObj(obj1);
     except
        MessageDlg(rsNoOperationSupported, mtWarning, [mbOK], 0);
     end;
   //clear the grid cells
     for i:=0 to StrGrdProp.RowCount-1  do StrGrdProp.Rows[i].Clear;
   //set the data
     setData;
end;

procedure TFrmPropertiesDlg.BtnRestoreClick(Sender: TObject);
var obj1:TObject;
    i:Integer;
begin
     if StrGrdProp.selection.top<1 then exit;
     obj1 := StrGrdProp.Objects[0,StrGrdProp.selection.top];
     if obj1=nil then exit;
     try
        parentDlg.RestoreObj(obj1);
     except
        MessageDlg(rsNoOperationSupported, mtWarning, [mbOK], 0);
     end;
   //clear the grid cells
     for i:=0 to StrGrdProp.RowCount-1  do StrGrdProp.Rows[i].Clear;
   //set the data
     setData;
end;

procedure TFrmPropertiesDlg.BtnGetClick(Sender: TObject);
var obj1:TObject;
    i:Integer;
begin
     try
        parentDlg.getObj(obj1);
     except
        MessageDlg(rsNoOperationSupported, mtWarning, [mbOK], 0);
     end;
   //clear the grid cells
     for i:=0 to StrGrdProp.RowCount-1  do StrGrdProp.Rows[i].Clear;
   //set the data
     setData;
end;

procedure TFrmPropertiesDlg.StrGrdPropDblClick(Sender: TObject);
begin
    if (local_row>0) then   callUpdateFkt;
end;

procedure TFrmPropertiesDlg.ToolButton1Click(Sender: TObject);
begin
    callUpdateFkt;
end;


procedure TFrmPropertiesDlg.BtnDeleteClick(Sender: TObject);
var i:Integer;
    obj1:TObject;
begin
     obj1 := StrGrdProp.Objects[0,StrGrdProp.selection.top];
     obj1.free;
   //clear the grid cells
     for i:=0 to StrGrdProp.RowCount-1  do StrGrdProp.Rows[i].Clear;
   //set the data
     setData;
end;


procedure TFrmPropertiesDlg.StrGrdPropMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
     reorderList;
end;


(*********************************************)
(*********************************************)
(*              other routines               *)
(*********************************************)
(*********************************************)

function GetPropertyVal(Obj: TObject; propStr: string): string;
var
 classinfo: Pointer;
 propinfo : PPropInfo;
begin
   result := '';
   classinfo := Obj.ClassInfo;
   propinfo := GetPropInfo(classinfo, propStr);
   if propinfo <> nil then
   begin
       case propinfo^.PropType^.Kind of
          tkInteger:      result := IntToStr(GetOrdProp(Obj, propinfo));
          tkLString:      result := GetStrProp(Obj, propinfo);
          tkFloat  :      result := FloatToStr(GetFloatProp(Obj, propinfo));
          tkEnumeration : result := IntToStr(GetOrdProp(Obj, propinfo));
       end;
   end;
end;

(*
procedure AssignPropertyVal(Obj: TObject; propStr: string; valStr:string);
var
 classinfo: Pointer;
 propinfo : PPropInfo;
begin
   classinfo := Obj.ClassInfo;
   propinfo := GetPropInfo(classinfo, propStr);
   if (propinfo<>nil) and (propinfo^.SetProc<>nil) then
   begin
       case propinfo^.PropType^.Kind of
          tkInteger:      SetOrdProp(Obj, propinfo, StrToInt(valStr));
          tkLString:      SetStrProp(Obj, propinfo, valStr);
          tkFloat  :      SetFloatProp(Obj, propinfo, StrToFloat(valStr));
          tkEnumeration : SetOrdProp(Obj, propinfo, StrToInt(valStr));
       end;
   end;
end;
*)

function propertyIsReadOnly(grid1:TStringGrid; ACol, ARow:Integer):Boolean;
var propinfo : PPropInfo;
    classinfo1: Pointer;
    obj1:TObject;
    propStr:String;
begin
      result := false;
      if (ACol<0) or (ARow<0) then exit;
      obj1 := grid1.Objects[0,ARow];
      if obj1=nil then exit;
      classinfo1 := Obj1.ClassInfo;
      propStr := grid1.Cells[ACol,0];
      propinfo := GetPropInfo(classinfo1, propStr);
      if propinfo=nil then exit;
      if (propinfo^.SetProc=nil) then result:=true
      else                            result:=false;
end;



end.
