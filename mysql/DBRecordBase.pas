unit DBRecordBase;

interface
uses System.Classes,System.Generics.Collections,System.Types,MysqlOP;
Type
  TDBRecordBase = class;
  TDBRecordClass = class of TDBRecordBase;



  TDBRecordBase = class
  public
    class function GetCreateTableSql():TStringList ; virtual;
  protected
    function _Q(const S:String):String;
  end;

implementation

{ TDBRecordBase }

class function TDBRecordBase.GetCreateTableSql: TStringList;
begin
  Result := nil;
end;

function TDBRecordBase._Q(const S: String): String;
begin
  Result := #39 + S + #39;
end;




end.
