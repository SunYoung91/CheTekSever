unit DBRecordBase;

interface
uses System.Classes;
Type
  TDBRecordBase = class
  public
    class var TableNames : TStringList;
    class procedure AddTableName(const S:String);
  protected
    function _Q(const S:String):String;
  end;

implementation

{ TDBRecordBase }

function TDBRecordBase._Q(const S: String): String;
begin
  Result := #39 + S + #39;
end;

class procedure TDBRecordBase.AddTableName(const S: String);
begin
  if TableNames = nil then
    TableNames := TStringList.Create;

  TableNames.Add(S);
end;


initialization

finalization
begin
  if TDBRecordBase.TableNames <> nil then
  begin
    TDBRecordBase.TableNames.Free;
    TDBRecordBase.TableNames := nil;
  end;
end;



end.
