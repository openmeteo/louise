{******************************************************************}
{                                                                  }
{  LoUISE library                                                  }
{                                                                  }
{  Copyright (c) 2000-05 National Technical University of Athens   }
{                                                                  }
{******************************************************************}

{** A unit with the TIComponent class
}
unit icomponent;

interface

uses Classes, Controls, Graphics, Types, Menus;

type
  {** A descendant of TComponent containing some custom properties.
      You have to pass FHelpFile and FHelpContext into your dialog
      associated properties before a ShowModal or Show action.
      @SeeAlso <See Property=FHelpContext>
      @SeeAlso <See Property=FHelpFile>
  }
  TIComponent = class(TComponent)
  protected
    {** Use FHelpFile into your component code.
        @SeeAlso <See Property=HelpFile>
    }
    FHelpFile: string;
    {** Use FHelpContext into your component code.
        @SeeAlso <See Property=HelpContext>
    }
    FHelpContext: THelpContext;
    {** Indicates whether the control’s context sensitive help topic is
        identified by context ID or by keyword.
        @SeeAlso <See Property=HelpType>
    }
    FHelpType: THelpType;
    {** Keyword for control’s context-sensitive help topic
        @SeeAlso <See Property=HelpKeyword>
    }
    FHelpKeyword: string;
  public
    {** Create initialize a TIComponent instance.
    }
    constructor Create(AOwner: TComponent); override;
  published
    {** Specifies the help file associated
        @SeeAlso <See Property=HelpContext>
        @SeeAlso <See Property=FHelpContext>
    }
    property HelpFile: string read FHelpFile write FHelpFile;
    {** Specifies the HelpID Context Help
        @SeeAlso <See Property=HelpFile>
        @SeeAlso <See Property=FHelpFile>        
    }
    property HelpContext: THelpContext read FHelpContext write FHelpContext default 0;
    {** Indicates whether the control’s context sensitive help topic is
        identified by context ID or by keyword.
    }
    property HelpType: THelpType read FHelpType write FHelpType default htContext;
    {** Keyword for control’s context-sensitive help topic
    }
    property HelpKeyword: string read FHelpKeyword write FHelpKeyword;
  end;

type
  TIShapeType = (stlRectangle, stlSquare, stlRoundRect, stlRoundSquare,
    stlEllipse, stlCircle, stlLineTLRB, stlLineBLTR, stlTriangleTop,
    stlTriangleBottom, stlDiamond);

type
  {** A similar to TShape drawing object allowing extra shapes like straight lines.
      See help of TShape.
  }
  TIShape = class(TGraphicControl)
  private
    FPen: TPen;
    FBrush: TBrush;
    FShape: TIShapeType;
    procedure SetBrush(Value: TBrush);
    procedure SetPen(Value: TPen);
    procedure SetShape(Value: TIShapeType);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    procedure StyleChanged(Sender: TObject);
    property Align;
    property Anchors;
    property Brush: TBrush read FBrush write SetBrush;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Constraints;
    property ParentShowHint;
    property Pen: TPen read FPen write SetPen;
    property Shape: TIShapeType read FShape write SetShape default stlRectangle;
    property ShowHint;
    property Visible;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

type
{** A TMainMenu class, to introduce the MergeCopy method.
}
  TIMainMenu = class(TMainMenu)
  public
{** Use MergeCopy instead of the standard Merge method of TMainMenu.
    MergeCopy has better behaviour, merged menus are not disappering...
}
    procedure MergeCopy(Menu: TMainMenu);
  end;

implementation

{ TIComponent }

constructor TIComponent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHelpType := htContext;
  FHelpFile := '';
  FHelpContext := 0;
  FHelpKeyword := '';  
end;

{ TIShape }

constructor TIShape.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  Width := 65;
  Height := 65;
  FPen := TPen.Create;
  FPen.OnChange := StyleChanged;
  FBrush := TBrush.Create;
  FBrush.OnChange := StyleChanged;
end;

destructor TIShape.Destroy;
begin
  FPen.Free;
  FBrush.Free;
  inherited Destroy;
end;

procedure TIShape.Paint;
var
  X, Y, W, H, S: Integer;
  TrianglePoints: array[0..3] of TPoint;
  DiamondPoints: array[0..4] of TPoint;
begin
  with Canvas do
  begin
    Pen := FPen;
    Brush := FBrush;
    X := Pen.Width div 2;
    Y := X;
    W := Width - Pen.Width + 1;
    H := Height - Pen.Width + 1;
    if Pen.Width = 0 then
    begin
      Dec(W);
      Dec(H);
    end;
    if W < H then S := W else S := H;
    if FShape in [stlSquare, stlRoundSquare, stlCircle] then
    begin
      Inc(X, (W - S) div 2);
      Inc(Y, (H - S) div 2);
      W := S;
      H := S;
    end;
    case FShape of
      stlRectangle, stlSquare:
        Rectangle(X, Y, X + W, Y + H);
      stlRoundRect, stlRoundSquare:
        RoundRect(X, Y, X + W, Y + H, S div 4, S div 4);
      stlCircle, stlEllipse:
        Ellipse(X, Y, X + W, Y + H);
      stlLineTLRB:
      begin  MoveTo(X,Y); LineTo(X+W,Y+H);end;
      stlLineBLTR:
      begin  MoveTo(X,Y+H); LineTo(X+W,Y);end;
      stlTriangleTop:
      begin
        TrianglePoints[0] := Point(X, Y+H);
        TrianglePoints[1] := Point(X+W, Y+H);
        TrianglePoints[2] := Point((X+W) div 2, Y);
        TrianglePoints[3] := Point(X, Y+H);
        Polygon(TrianglePoints);
      end;
      stlTriangleBottom:
      begin
        TrianglePoints[0] := Point(X, Y);
        TrianglePoints[1] := Point(X+W, Y);
        TrianglePoints[2] := Point((X+W) div 2, Y+H);
        TrianglePoints[3] := Point(X, Y);
        Polygon(TrianglePoints);
      end;
      stlDiamond:
      begin
        DiamondPoints[0] := Point((X+W) div 2, Y);
        DiamondPoints[1] := Point(X+W, (Y+H) div 2);
        DiamondPoints[2] := Point((X+W) div 2, Y+H);
        DiamondPoints[3] := Point(X, (Y+H) div 2);
        DiamondPoints[4] := Point((X+W) div 2, Y);
        Polygon(DiamondPoints);
      end;
    end;
  end;
end;

procedure TIShape.StyleChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TIShape.SetBrush(Value: TBrush);
begin
  FBrush.Assign(Value);
end;

procedure TIShape.SetPen(Value: TPen);
begin
  FPen.Assign(Value);
end;

procedure TIShape.SetShape(Value: TIShapeType);
begin
  if FShape <> Value then
  begin
    FShape := Value;
    Invalidate;
  end;
end;

{ TIMainMenu }

procedure TIMainMenu.MergeCopy(Menu: TMainMenu);
var
  StartCount: Integer;
  MergeItem: TMenuItem;
  X: Integer;
begin
  if Menu <> nil then
  begin
    while Menu.Items.Count > 0 do begin
      MergeItem := Menu.Items[0];
      Menu.Items.Delete(0);
      StartCount := Items.Count - 1;
      for X := 0 to StartCount do begin
        if Items[X].GroupIndex = MergeItem.GroupIndex then
          Break
        else if Items[X].GroupIndex > MergeItem.GroupIndex then begin
          Items.Insert(X, MergeItem);
          Break;
        end;
        if X = StartCount then
          Items.Add(MergeItem);
      end;
    end;
  end;
end;

end.
