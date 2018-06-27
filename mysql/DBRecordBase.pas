unit DBRecordBase;

interface
uses System.Classes,System.Generics.Collections,System.Types,MysqlOP,System.SysUtils,CheTek.BaseObject,CheTek.SerialObject,System.TypInfo;
Type
  TDBRecordBase = class;
  TDBRecordClass = class of TDBRecordBase;



  TDBRecordBase = class
  public
    class function GetCreateTableSql():TStringList ; virtual;
  protected
    class function _Q(const S:String):String;
  end;


function MysqlDateTimeStrToDateTime(const Str:String):TDateTime;
function MysqlBoolean(Bo:Boolean):String;

//这两个函数是为了 兼容 DB 操作代码生成器所做的  正常的人工编写的代码不可以使用这两个函数。
function _DBJsonFromRtti(Ptr:Pointer ; TypeInfo:PTypeInfo):String;
procedure _DBJsonToRtti(const Str : string ; Ptr:Pointer; TypeInfo:PTypeInfo);

implementation

function MysqlDateTimeStrToDateTime(const Str:String):TDateTime;
var
  Setting : TFormatSettings;
begin
  Setting := TFormatSettings.Create;
  Setting.DateSeparator := '-';
  Setting.TimeSeparator := ':';
  Result := StrToDateTimeDef(Str,0,Setting)
end;

function MysqlBoolean(Bo:Boolean):String;
begin
  if bo then
    Result := 'true'
  else
    Result := 'false';
end;


function _DBJsonFromRtti(Ptr:Pointer ; TypeInfo:PTypeInfo):String;
var
  ClassObject:TObject;
begin
  if TypeInfo.Kind = tkClass then
  begin
    ClassObject := TObject(PNativeUint(Ptr)^);
    Result := (ClassObject as TSerialObject).ToJsonString();
  end else
  begin
    Result := TSerialObject.JsonFromRtti(Ptr,TypeInfo)
  end;
end;

procedure _DBJsonToRtti(const Str : string ; Ptr:Pointer; TypeInfo:PTypeInfo);
var
  ClassObject:TObject;
begin
  if TypeInfo.Kind = tkClass then
  begin
    ClassObject := TObject(PNativeUint(Ptr)^);
    (ClassObject as TSerialObject).FromJson(Str);
  end else
  begin
    TSerialObject.JsonToRtti(Str,Ptr,TypeInfo);
  end;
end;

{ TDBRecordBase }

class function TDBRecordBase.GetCreateTableSql: TStringList;
begin
  Result := nil;
end;

class function TDBRecordBase._Q(const S: String): String;
begin
   Result := #39 + S + #39;
end;



end.
