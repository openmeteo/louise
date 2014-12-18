unit msglist;

interface

uses Contnrs;

type
  TMessageType = (msgInfo, msgWarning, msgError);

  TMsg = class(TObject)
  public
    Content: string;
    MType: TMessageType;
  end;

  /// <summary>Stores a list of messages to be shown to the user</summary>
  /// <remarks>
  ///   Use <c>TMessageList</c> when you want to collect many messages for
  ///   the user and show them later altogether. Use the
  ///   <c>AddMsg</c> procedure to add a message to the list,
  ///   and the <c>GetAllMessages</c> function to return a string
  ///   consisting of all the messages.
  /// </remarks>
  /// <example>
  ///   This is an example:
  ///   <code lang="Delphi">
  ///     AMessageList := TMessageList.Create;
  ///     try
  ///       do_something;
  ///       AMessageList.AddMsg('Important information', msgInfo);
  ///       do_more_things;
  ///       AMessageList.AddMsg('Important warning', msgWarning);
  ///       do_even_more_things;
  ///       AMessageList.AddMsg('Important error', msgError);
  ///     finally
  ///       AMessageList.ShowOrRaiseAndFree;
  ///     end;
  ///   </code>
  /// </example>
  TMessageList = class(TObjectList)
  private
    function GetErrorCount: Integer;
  public
    /// <summary>The number of messages of type msgError that have been added
    /// so far
    /// </summary>
    property ErrorCount: Integer read GetErrorCount;

    /// <summary>Renders all messages and returns them as a string</summary>
    function GetAllMessages: string;

    /// <summary>Adds a message to the list
    procedure AddMsg(Content: string; MType: TMessageType);

    /// <summary>Adds a message to the list, but only if a condition is true
    /// </summary>
    procedure AddMsgIf(Condition: Boolean; Content: string; MType: TMessageType);

    /// <summary>Adds a message to the list, but only if a string
    /// contains a value that is not a real number.</summary>
    procedure AddMsgIfNotReal(s: string; Content: string; MType: TMessageType;
                              AllowEmpty: Boolean = True);

    /// <summary>Adds a message to the list, but only if a string
    /// contains a value that is not a positive number or zero.</summary>
    procedure AddMsgIfNegative(s: string; Content: string; MType: TMessageType;
                               AllowEmpty: Boolean = True);

    /// <summary>Adds a message to the list, but only if a string
    /// contains a value that is not a positive integer or zero.</summary>
    procedure AddMsgIfINegative(s: string; Content: string; MType: TMessageType;
                                AllowEmpty: Boolean = True);

    /// <summary>Adds a message to the list, but only if a string
    /// contains a value that is not between zero and one.</summary>
    procedure AddMsgIfOutsideZeroOne(s: string;
                                     Content: string;
                                     MType: TMessageType;
                                     AllowEmpty: Boolean = True);

    /// <summary>Displays dialog box with the messages</summary>
    /// <remarks>
    ///   Will do nothing if there are no messages; otherwise it will
    ///   show a modal dialog box with the messages.
    /// </remarks>
    procedure Show;

    /// <summary>Shows messages or raises exception, then frees the object.
    /// </summary>
    /// <remarks>
    ///   Call this destructor at the end, in order to display the messages.
    ///   If one of the messages is an error, it will raise an exception whose
    ///   message will be the list of messages;
    ///   otherwise, it will show a message dialog with the messages (unless
    ///   there are no messages). In all cases, it will free the object in
    ///   the end.
    /// </remarks>
    ///
    destructor ShowOrRaiseAndFree; virtual;
  end;

implementation

uses Dialogs, SysUtils;

resourcestring
  rsInfo    = 'Information';
  rsWarning = 'Warning';
  rsError   = 'Error';

function TMessageList.GetErrorCount: Integer;
var i: Integer;
begin
  Result := 0;
  for i := 0 to Count-1 do
    if TMsg(Items[i]).MType = msgError then
      Inc(Result);
end;

function TMessageList.GetAllMessages: string;
var
  i: Integer;
  Item: TMsg;
  SMessageType: string;
begin
  Result := '';
  for i:=0 to Count-1 do
  begin
    Item := TMsg(Items[i]);
    case Item.MType of
      msgInfo: SMessageType := rsInfo;
      msgWarning: SMessageType := rsWarning;
      msgError: SMessageType := rsError;
    end;
    Result := Result + '[' + SMessageType + '] ' + Item.Content + #13#10;
  end;
end;

procedure TMessageList.AddMsg(Content: string; MType: TMessageType);
var AMsg: TMsg;
begin
  AMsg := TMsg.Create;
  try
    AMsg.Content := Content;
    AMsg.MType := MType;
    Add(AMsg);
  except
    AMsg.Free;
    raise;
  end;
end;

procedure TMessageList.AddMsgIf(Condition: Boolean;
                                Content: string; MType: TMessageType);
begin
  if Condition then
    AddMsg(Content, MType);
end;

procedure TMessageList.AddMsgIfNotReal(s: string;
                                       Content: string;
                                       MType: TMessageType;
                                       AllowEmpty: Boolean = True);
var AReal: Double;
begin
  AddMsgIf((s = '') and (not AllowEmpty), Content, MType);
  AddMsgIf((s <> '') and (not TryStrToFloat(s, AReal)), Content, MType);
end;

procedure TMessageList.AddMsgIfNegative(s: string;
                                       Content: string;
                                       MType: TMessageType;
                                       AllowEmpty: Boolean = True);
var AReal: Double;
begin
  AddMsgIf((s = '') and (not AllowEmpty), Content, MType);
  AddMsgIf((s <> '') and (not TryStrToFloat(s, AReal) or (AReal < 0)),
           Content, MType);
end;

procedure TMessageList.AddMsgIfINegative(s: string;
                                        Content: string;
                                        MType: TMessageType;
                                        AllowEmpty: Boolean = True);
var AInteger: Integer;
begin
  AddMsgIf((s = '') and (not AllowEmpty), Content, MType);
  AddMsgIf((s <> '') and (not TryStrToInt(s, AInteger) or (AInteger < 0)),
           Content, MType);
end;

procedure TMessageList.AddMsgIfOutsideZeroOne(s: string;
                                              Content: string;
                                              MType: TMessageType;
                                              AllowEmpty: Boolean = True);
var AReal: Double;
begin
  AddMsgIf((s = '') and (not AllowEmpty), Content, MType);
  AddMsgIf((s <> '') and (not TryStrToFloat(s, AReal)
                        or (AReal < 0) or (AReal > 1)), Content, MType);
end;

procedure TMessageList.Show;
begin
  if Count > 0 then
    ShowMessage(GetAllMessages);
end;

destructor TMessageList.ShowOrRaiseAndFree;
var
  i: Integer;
  HasErrors: Boolean;
begin
  if Self = nil then
    Exit;

  // Are there any errors?
  HasErrors := False;
  for i := 0 to Count - 1 do
    if TMsg(Items[i]).MType = msgError  then
      HasErrors := True;

  try
    if Count = 0 then
      Exit
    else if not HasErrors then
      ShowMessage(GetAllMessages)
    else
      raise Exception.Create(GetAllMessages);
  finally
    inherited Destroy;
  end;
end;

end.
