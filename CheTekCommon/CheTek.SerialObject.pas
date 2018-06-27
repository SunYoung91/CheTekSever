unit CheTek.SerialObject;

interface
uses System.Classes,qjson,System.Generics.Collections,System.TypInfo;
type

  TSerialObject = class(TPersistent)
  public
    constructor Create; virtual;
    procedure LoadDefault; virtual;
    procedure LoadFromFile(const FileName: String); virtual;
    procedure SaveToFile(const FileName: String); virtual;
    procedure SaveToStream(AStream: TStream); virtual;
    procedure LoadFromStream(AStream: TStream); virtual;
    class function RecordToJson<T>(var ARecord : T):String;
    class function JsonFromRtti(Ptr:Pointer ; TypeInfo:PTypeInfo):String;
    class procedure JsonToRtti(const Str : string ; Ptr:Pointer; TypeInfo:PTypeInfo);
    class function JsonToRecord<T : Record>( const S:String; var ARecord : T):Boolean;
    procedure FromJson(const JsonStr:String);
    function ToJsonString():String;
  end;


    //和TList没什么区别 只是增加可以序列化到Json 的操作 仅仅支持Record
  TJsonArray<T:Record> = class(TList<T>)
  public
    procedure FromJson(const Str: string);
    function ToJsonString(): string;
  end;

   TSerialObjectList = class(TList<TSerialObject>)
   public
      procedure FromJson(const Str: string);
      function ToJsonString(): string;
   end;

implementation

{ TSerialObject }

constructor TSerialObject.Create;
begin

end;

procedure TSerialObject.LoadDefault;
begin

end;

procedure TSerialObject.LoadFromFile(const FileName: String);
var
  Stream : TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  Try
    Stream.LoadFromFile(FileName);
    Self.LoadFromStream(Stream);
  Finally
    Stream.Free;
  End;
end;

procedure TSerialObject.LoadFromStream(AStream: TStream);
var
  JSON : TQJson;
begin
  JSON := TQJson.Create;
  Try
    JSON.LoadFromStream(AStream);
    JSON.ToRtti(Self);
  Finally
    JSON.Free;
  End;
end;

procedure TSerialObject.SaveToFile(const FileName: String);
var
  Stream : TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  Try
    Self.SaveToStream(Stream);
    Stream.SaveToFile(FileName);
  Finally
    Stream.Free;
  End;
end;

procedure TSerialObject.SaveToStream(AStream: TStream);
var
  JSON : TQJson;
begin
  JSON := TQJson.Create;
  Try
    JSON.FromRtti(Self);
    JSON.SaveToStream(AStream);
  Finally
    JSON.Free;
  End;
end;

function TSerialObject.ToJsonString: String;
var
  JSON : TQJson;
begin
  JSON := TQJson.Create;
  Try
    JSON.FromRtti(Self);
    Result := Json.ToString();
  Finally
    JSON.Free;
  End;
end;

procedure TSerialObject.FromJson(const JsonStr: String);
var
  JSON : TQJson;
begin
  JSON := TQJson.Create;
  Try
    JSON.Parse(JsonStr);
    JSON.ToRtti(Self);
  Finally
    JSON.Free;
  End;
end;

class function TSerialObject.RecordToJson<T>(var ARecord: T): String;
var
  JSON:TQJson;
begin
  JSON := TQJson.Create;
  try
    JSON.FromRecord(ARecord);
    Result := JSON.ToString();
  finally
    JSON.Free;
  end;
end;

class function TSerialObject.JsonFromRtti(Ptr: Pointer;
  TypeInfo: PTypeInfo): String;
var
  JSON : TQJson;
begin
  Result := '';
  JSON := TQJson.Create;
  try
    JSON.FromRtti(Ptr,TypeInfo);
    Result := JSON.ToString();
  finally
    JSON.Free;
  end;
end;

class procedure TSerialObject.JsonToRtti(const Str: string; Ptr: Pointer;
  TypeInfo: PTypeInfo);
var
  JSON : TQJson;
begin
  JSON := TQJson.Create;
  try
    JSON.Parse(Str);
    JSON.ToRtti(Ptr,TypeInfo);
  finally
    JSON.Free;
  end;
end;

class function TSerialObject.JsonToRecord<T>(const S:String; var ARecord: T): Boolean;
var
  JSON : TQJson;
begin
  Result := False;
  JSON := TQJson.Create;
  try
    JSON.Parse(S);
    JSON.ToRecord(ARecord);
    Result := True;
  finally
    JSON.Free;
  end;
end;

{ TJsonArray<T> }

procedure TJsonArray<T>.FromJson(const Str: string);
var
  json, childNode: TQJson;
  Value: T;
  I: Integer;
begin
  Self.Clear;
  json := TQJson.Create;
  try
    json.Parse(Str);
    for I := 0 to json.Count - 1 do
    begin
      childNode := json.Items[I];
      childNode.ToRecord<T>(Value);
      Self.Add(Value);
    end;
  finally
    json.Free;
  end;
end;

function TJsonArray<T>.ToJsonString: string;
var
  Value : T;
  I:Integer;
  List:TStringList;
begin
  List := TStringList.Create;
  Try
    for i := 0 to Self.Count - 1 do
    begin
      Value := Self[i];
      List.Add(TSerialObject.RecordToJson<T>(Value));
    end;

    List.Delimiter := ',';

    Result := '[' + List.Text + ']';
  Finally
    List.Free;
  End;
end;

{ TSerialObjectList }

procedure TSerialObjectList.FromJson(const Str: string);
var
  json, childNode: TQJson;
  Value: TSerialObject;
  I: Integer;
begin

  for Value in Self do
  begin
    Value.Free;
  end;

  Self.Clear;
  json := TQJson.Create;
  try

    json.Parse(Str);
    for I := 0 to json.Count - 1 do
    begin
      childNode := json.Items[I];
      Value := TSerialObject.Create;
      childNode.ToRtti(Value);
      Self.Add(Value);
    end;
  finally
    json.Free;
  end;
end;

function TSerialObjectList.ToJsonString: string;
var
  Value : TSerialObject;
  I:Integer;
  List:TStringList;
begin
  List := TStringList.Create;
  Try
    for i := 0 to Self.Count - 1 do
    begin
      Value := Self[i];
      List.Add(Value.ToJsonString());
    end;

    List.Delimiter := ',';

    Result := '[' + List.Text + ']';
  Finally
    List.Free;
  End;
end;

end.
