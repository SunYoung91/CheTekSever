unit DBRecordBase;

interface
uses System.Classes,System.Generics.Collections,System.Types,System.SysUtils,CheTek.BaseObject,CheTek.SerialObject,System.TypInfo,System.Rtti,JclStrings;
Type
  TDBRecordBase = class;
  TDBRecordClass = class of TDBRecordBase;



  TDBRecordBase = class
  public
    class function GetCreateTableSql():TStringList ; virtual;
  end;


function MysqlDateTimeStrToDateTime(const Str:String):TDateTime;
function MysqlBoolean(Bo:Boolean):String;
function MysqlQuote(const S : String):String;

//这两个函数是为了 兼容 DB 操作代码生成器所做的  正常的人工编写的代码不可以使用这两个函数。
function _DBJsonFromRtti(Ptr:Pointer ; TypeInfo:PTypeInfo):String;
procedure _DBJsonToRtti(const Str : string ; Ptr:Pointer; TypeInfo:PTypeInfo);


implementation

function MysqlQuote(const S : String):String;
begin
  Result := #39 + S + #39;
end;

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
  value : TValue;
begin
  if TypeInfo.Kind = tkClass then
  begin
    ClassObject := TObject(PNativeUint(Ptr)^);
    Result := (ClassObject as TSerialObject).ToJsonString();
  end else if TypeInfo.Kind = tkEnumeration then
  begin
    TValue.Make(Ptr, TypeInfo, value);
    Result := Format('{"E":"%s"}',[value.ToString()]);
  end else
  begin
    Result := TSerialObject.JsonFromRtti(Ptr,TypeInfo)
  end;
end;

procedure _DBJsonToRtti(const Str : string ; Ptr:Pointer; TypeInfo:PTypeInfo);
var
  ClassObject:TObject;
  EnumStr : String;
  value : Integer;
begin
  if TypeInfo.Kind = tkClass then
  begin
    ClassObject := TObject(PNativeUint(Ptr)^);
    (ClassObject as TSerialObject).FromJson(Str);
  end else  if TypeInfo.Kind = tkEnumeration then
  begin
    EnumStr := Trim(StrBetween(StrAfter(':',Str),'"','"'));
    Value := GetEnumValue(TypeInfo,EnumStr);
    if value >= 0  then
    begin
      case GetTypeData(TypeInfo).OrdType of
        otSByte:
           PShortint(Ptr)^ := value;
        otUByte:
          PByte(Ptr)^ := Value;
        otSWord:
          PSmallInt(Ptr)^ := Value;
        otUWord:
         PWord(Ptr)^ := Value;
        otSLong:
          PInteger(Ptr)^ := Value;
        otULong:
          PCardinal(Ptr)^ := Value;
      end;
    end;
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




end.
