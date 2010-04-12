{******************************************************************}
{                                                                  }
{  LoUISE library                                                  }
{                                                                  }
{  Copyright (c) 2003-04 National Technical University of Athens   }
{                                                                  }
{******************************************************************}

unit iform;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Contnrs;

type


  {** TOdForm provides methods which in a way implement an undo function.
      TOdForm temporarily stores property values of controls belonging
      to the form and restores them if desired. These methods may be
      particularly useful in forms designed for for data entry/manipulation.
  }
  TOdForm = class(TForm)
  private
  {** Lists a set of components belonging to the form.
      @SeeAlso <See Method=copyCtrlData>
      @SeeAlso <See Method=copyAllCtrlData>
      @SeeAlso <See Method=pasteCtrlData>
  }
    componentsList:TObjectList;
  public
  {** Clears the internal list, which holds all components
      involved in a copy/paste operation.
      @SeeAlso <See Method=copyCtrlData>
      @SeeAlso <See Method=pasteCtrlData>
  }
    procedure clearComponentsList;
  {** Stores some property values of the given component (Item) for later use.
      The component must belong to the form.
      The controls/properties stored are the following:
          TLabel.Caption
          TEdit.Text
          TStringGrid.ColCount
          TStringGrid.RowCount
          TStringGrid.Cells
          TRadioButton.Checked
          TCheckBox.Checked
          TComboBox.ItemIndex
          TMemo.Lines
      Use this method in order to specify selected controls of the form
      to be involved in a copy/paste operation.
      @SeeAlso <See Method=copyAllCtrlData>
      @SeeAlso <See Method=clearComponentsList>
      @SeeAlso <See Method=pasteCtrlData>
  }
    procedure copyCtrlData(Item: TComponent);
  {** Stores the values of all data input controls belonging to the form
      for later use. See copyCtrlData for a list of all possible saved/restored
      property values by the method.
      Use this procedure in order to make a snapshot of the input data at
      a specific time and restore the initial values at a later time.
      @SeeAlso <See Method=copyCtrlData>
      @SeeAlso <See Method=pasteCtrlData>
  }
    procedure copyAllCtrlData;
  {** Restores the data stored by the last copyCtrlData/copyAllCtrlData operation.
      If the size of a TStringGrid object has been changed, the original
      size is restored.
      If the size of a TComboBox object has been changed no item
      is selected (ItemIndex=-1).
      @SeeAlso <See Method=copyCtrlData>
      @SeeAlso <See Method=copyAllCtrlData>
  }
    procedure pasteCtrlData;
    constructor create(AOwner: TComponent); override;
    destructor destroy; override;
  end;


  {** A slightly altered TForm for using as the parent of all forms.
      The problem with TForm is that when a developer moves a form at design
      time, its Top and Left properties are changed, which badly affects the
      version control system; there are lots of fake check-ins, in which the
      only thing that has changed is Top and Left, and, worse, there are
      conflicts, because if two developers have moved the same form then they
      have both unvoluntarily altered the same part of the file.<p>

      TIForm addresses this problem by not storing Top, Left, Height, and
      Width. Instead, it defines the four additional properties
      <See Property=InitialTop>, <See Property=InitialLeft>,
      <See Property=InitialWidth>, and <See Property=InitialHeight>, which are
      stored.<p>

      When the form is moved or resized at design time, the changes will not
      be remembered. Instead, manually alter the InitialTop, InitialLeft,
      InitialWidth and InitialHeight settings in the Object Inspector. You
      may want to move/resize the form at design time, then copy the values
      from Top, Left etc. to InitialTop, InitialLeft etc.<p>

      The settings to InitialTop etc. have an effect only for certain values
      of Position.<p>

      <b>To convert an existing form to use TIForm:</b><p>

      <ol>
        <li>Change its class definition from "TMyForm = class(TForm)"
	    to "TMyForm = class(TIForm)".
	<li>Add iform to the uses clause.
	<li>View the form as text.
	<li>Change Left and Top to InitialLeft and InitialTop.
	<li>If Height and Width are specified, change them to
	InitialHeight and InitialWidth. Otherwise, ClientHeight and
	ClientWidth will probably be specified; in that case, add 27
	the ClientHeight and change it to InitialHeight, and add 8 to
	ClientWidth and change it to InitialWidth.
	<li>Save the form. If you don't do this job in Delphi, and use
	external editors/tools, it is a good idea if you also open the
	form in Delphi and save it again; this is because it may
	reorder the properties when saving.
      </ol>
      <p>
      
      <b>To create a new form based on TIForm:</b><p>

      There are two ways. The simplest is to create a new form
      normally, then change it to use TIForm as described above.
      Another way is to install TIForm in the repository (it is not
      done automatically when louise is installed); this requires
      using the LoUISE source and is done as follows:<p>

      <ol>
        <li>Open louise.dpk.
	<li>Open IForm.
	<li>Right-click on the form and select "Add to repository".
	Specify a name (like "Itia base form") and select the page
	"Forms".
	<li>Compile and install louise.
      </ol>
      <p>
      
      You can then create forms by selecting File,New,Forms, selecting
      the icon for TIForm, and specifying Inherit. (It may be needed to
      restart Delphi in order to be able to select Inherit.)<p>

      It would be better if louise automatically installed TIForm, but
      this requires using undocumented Delphi features and is
      postponed.<p>

      @SeeAlso <See Method=Loaded>
      @SeeAlso <Jump File=Delphi5.hlp K="TCustomForm,Position" Text=TCustomForm.Position>
      @author A.X.
  }
  TIForm = class (TOdForm)
  private
    FInitialTop, FInitialLeft, FInitialWidth, FInitialHeight: Integer;
  public
    {** Performs fix-ups when the form is first loaded into memory.
        Loaded calls the inherited method, then it copies values from
        InitialTop etc. to Top etc.
    }
    procedure Loaded; override;
  published
    {** Specifies the initial position of the form.
        See <See Class=TIForm>.
    }
    property InitialTop: Integer read FInitialTop write FInitialTop;
    {** Specifies the initial position of the form.
        See <See Class=TIForm>.
    }
    property InitialLeft: Integer read FInitialLeft write FInitialLeft;
    {** Specifies the initial size of the form.
        See <See Class=TIForm>.
    }
    property InitialWidth: Integer read FInitialWidth write FInitialWidth;
    {** Specifies the initial size of the form.
        See <See Class=TIForm>.
    }
    property InitialHeight: Integer read FInitialHeight write FInitialHeight;
    property Top stored false;
    property Left stored false;
    property Width stored false;
    property Height stored false;
  end;

{** TLForm is like the TIForm, but stores the ClientWidth/Height instead.
    See <See Class=TIForm>
}
  TLForm = class (TOdForm)
  private
    FInitialTop, FInitialLeft: Integer;
    FInitialClientWidth, FInitialClientHeight: Integer;
  public
    {** Performs fix-ups when the form is first loaded into memory.
        Loaded calls the inherited method, then it copies values from
        InitialTop etc. to Top etc.
    }
    procedure Loaded; override;
  published
    {** Specifies the initial position of the form.
        See <See Class=TLForm>.
    }
    property InitialTop: Integer read FInitialTop write FInitialTop;
    {** Specifies the initial position of the form.
        See <See Class=TLForm>.
    }
    property InitialLeft: Integer read FInitialLeft write FInitialLeft;
    {** Specifies the initial size of the form.
        See <See Class=TLForm>.
    }
    property InitialClientWidth: Integer read FInitialClientWidth
      write FInitialClientWidth;
    {** Specifies the initial size of the form.
        See <See Class=TLForm>.
    }
    property InitialClientHeight: Integer read FInitialClientHeight
      write FInitialClientHeight;
    property Top stored false;
    property Left stored false;
    property ClientWidth stored false;
    property ClientHeight stored false;
    property Width stored false;
    property Height stored false;
  end;

implementation

uses stdCtrls, Grids;

procedure TIForm.Loaded;
begin
  inherited;
  Top := InitialTop;
  Left := InitialLeft;
  Height := InitialHeight;
  Width := InitialWidth;
end;

procedure TLForm.Loaded;
begin
  inherited;
  Top := InitialTop;
  Left := InitialLeft;
  ClientHeight := InitialClientHeight;
  ClientWidth := InitialClientWidth;
end;



(*****************************************************************************
                              TOdForm
******************************************************************************)

constructor TOdForm.create(AOwner: TComponent);
begin
     inherited create(AOwner);
     componentsList:=TObjectList.Create;
end;

destructor TOdForm.destroy;
begin
     componentsList.Clear;
     componentsList.Free;
     inherited destroy;
end;


procedure TOdForm.clearComponentsList;
begin
     componentsList.Clear;
end;


procedure TOdForm.copyCtrlData(Item:TComponent);
var i,j:   Integer;
    newLabel:TLabel;
    newEdit:TEdit;
    newStringGrid:TStringGrid;
    newRadioButton:TRadioButton;
    newCheckBox:TCheckBox;
    newComboBox:TComboBox;
    newMemo:TMemo;
begin
     //TLable
          if (Item is TLabel) then
          begin
               newLabel:=TLabel.Create(Item);
               newLabel.Caption:=(Item as TLabel).Caption;
               componentsList.Add(newLabel);
          end;
     //TEdit
          if (Item is TEdit) then
          begin
               newEdit:=TEdit.Create(Item);
               newEdit.Text:=(Item as TEdit).Text;
               componentsList.Add(newEdit);
          end;
     //TStringGrid
          if (Item is TStringGrid) then
          begin
               newStringGrid:=TStringGrid.Create(Item);
               newStringGrid.ColCount:=(Item as TStringGrid).ColCount;
               newStringGrid.RowCount:=(Item as TStringGrid).RowCount;
               for i:=0 to (Item as TStringGrid).ColCount-1 do
                  for j:=0 to (Item as TStringGrid).RowCount-1 do
                      newStringGrid.Cells[i,j]:=(Item as TStringGrid).Cells[i,j];
               componentsList.Add(newStringGrid);
          end;
     //TRadioButton
          if (Item is TRadioButton) then
          begin
               newRadioButton:=TRadioButton.Create(Item);
               newRadioButton.Checked:=(Item as TRadioButton).Checked;
               componentsList.Add(newRadioButton);
          end;
     //TCheckBox
          if (Item is TCheckBox) then
          begin
               newCheckBox:=TCheckBox.Create(Item);
               newCheckBox.Checked:=(Item as TCheckBox).Checked;
               componentsList.Add(newCheckBox);
          end;
     //TComboBox
          if (Item is TComboBox) then
          begin
               newComboBox:=TComboBox.Create(Item);
               newComboBox.parent:= (Item as TComboBox).parent;
               newComboBox.visible:=false;
               for i:=0 to (Item as TComboBox).Items.Count-1 do
                   newComboBox.Items.Add((Item as TComboBox).Items[i]);
               newComboBox.ItemIndex:=(Item as TComboBox).ItemIndex;
               componentsList.Add(newComboBox);
          end;
     //TMemo
          if (Item is TMemo) then
          begin
               newMemo:=TMemo.Create(Item);
               newMemo.parent:= (Item as TMemo).parent;
               newMemo.visible:=false;
               for i:=0 to (Item as TMemo).Lines.Count-1 do
                   newMemo.Lines.Add((Item as TMemo).Lines[i]);
               componentsList.Add(newMemo);
          end;
end;



procedure TOdForm.copyAllCtrlData;
var compI:   Integer;
    Temp:TComponent;
begin
     componentsList.Clear;
     for compI := ComponentCount - 1 downto 0 do
     begin
          Temp := Components[compI];
          copyCtrlData(Temp);
     end;
end;




procedure TOdForm.pasteCtrlData;
var compI,i,j:   Integer;
    newComp:TComponent;
begin
     for compI:=0 to componentsList.count-1 do
     begin
          newComp := componentsList.Items[compI] as TComponent;
     //TLable
          if (newComp is TLabel) then
          begin
               (newComp.Owner as TLabel).Caption:=(newComp as TLabel).Caption;
          end;
     //TEdit
          if (newComp is TEdit) then
          begin
               (newComp.Owner as TEdit).Text:=(newComp as TEdit).Text;
          end;
     //TStringGrid
          if (newComp is TStringGrid) then
          begin
            (newComp.Owner as TStringGrid).ColCount:=(newComp as TStringGrid).ColCount;
            (newComp.Owner as TStringGrid).RowCount:=(newComp as TStringGrid).RowCount;
            for i:=0 to (newComp as TStringGrid).ColCount-1 do
               for j:=0 to (newComp as TStringGrid).RowCount-1 do
                  (newComp.Owner as TStringGrid).Cells[i,j]:=(newComp as TStringGrid).Cells[i,j];
          end;
     //TRadioButton
          if (newComp is TRadioButton) then
          begin
               (newComp.Owner as TRadioButton).Checked:=(newComp as TRadioButton).Checked;
          end;
     //TCheckBox
          if (newComp is TCheckBox) then
          begin
               (newComp.Owner as TCheckBox).Checked:=(newComp as TCheckBox).Checked;
          end;
     //TComboBox
          if (newComp is TComboBox) then
          begin
               if (newComp.Owner as TComboBox).Items.Count<>(newComp as TComboBox).Items.Count then
                  (newComp.Owner as TComboBox).ItemIndex:=-1
               else
                  (newComp.Owner as TComboBox).ItemIndex:=(newComp as TComboBox).ItemIndex;
          end;
     //TMemo
          if (newComp is TMemo) then
          begin
               (newComp.Owner as TMemo).Clear;
               for i:=0 to (newComp as TMemo).Lines.Count-1 do
                  (newComp.Owner as TMemo).Lines.add((newComp as TMemo).Lines[i]);
          end;
     end;
end;

(*****************************************************************************
                              TOdForm End
******************************************************************************)

end.
