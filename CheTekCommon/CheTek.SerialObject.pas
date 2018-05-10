unit CheTek.SerialObject;

interface
uses System.Classes,qjson;
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
    class function JsonToRecord<T : Record>( const S:String; var ARecord : T):Boolean;
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

end.
