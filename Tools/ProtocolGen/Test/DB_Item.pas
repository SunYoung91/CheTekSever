unit DB_Item;

interface
uses DBRecordBase,System.Classes,AttrType,System.SysUtils,CheTek.SerialObject,CheTek.Mysql,System.Generics.Collections;
Type
  TDBRecordOfItem = class(TDBRecordBase)
    SerialNumber:UInt64;
    ActorID:UInt64;
    ItemName:String;
    ConfigID:Integer;
    Attrs:TAttrs;
    ExpireTime:TDateTime;
    procedure Insert();
    procedure Update();
    class function GetCreateTableSql():TStringList;override;
    class function FeatchToList(MysqlRows:TMysqlRow):TList<TDBRecordOfItem>;
  class function QueryAnd_ActorID(ActorID : UInt64):TList<TDBRecordOfItem>;
  end;


implementation
uses CheTek.DBEngine;
procedure TDBRecordOfItem.Insert();
var
  Sql:String;
  Fields:String;
  Values:String;
  Rows : TMysqlRow;
  V : Array[1..6] of String;
begin
   Fields := Fields + 'SerialNumber';
  V[1] := IntToStr(SerialNumber);
   Fields := Fields + 'ActorID';
  V[2] := IntToStr(ActorID);
   Fields := Fields + 'ItemName';
  V[3] := Self._Q(ItemName);
   Fields := Fields + 'ConfigID';
  V[4] := IntToStr(ConfigID);
   Fields := Fields + 'Attrs';
  V[5] := Self._Q(Attrs);
   Fields := Fields + 'ExpireTime';
  V[6] := FormatDateTime('YYYY-MM-DD HH:NN:SS', Self.ExpireTime);;
  Sql := format('INSERT INTO Item (Fields) VALUES (Values);',[ V[1], V[2], V[3], V[4], V[5], V[6]]);
  Rows := TDBEngine.Inst().Query(Sql);
end;

procedure TDBRecordOfItem.Update();
var
  Sql:String;
  Rows : TMysqlRow;
begin
  Sql := 'SerialNumber = ' + IntToStr(SerialNumber);
  Sql := Sql + ', '+ 'ActorID = ' + IntToStr(ActorID);
  Sql := Sql + ', '+ 'ItemName = ' + Self._Q(ItemName);
  Sql := Sql + ', '+ 'ConfigID = ' + IntToStr(ConfigID);
  Sql := Sql + ', '+ 'Attrs = ' + Self._Q(Attrs);
  Sql := Sql + ', '+ 'ExpireTime = ' + FormatDateTime('YYYY-MM-DD HH:NN:SS', Self.ExpireTime);;
  Sql := Format('UPDATE %s SET %s WHERE %s = %s ;', ['Item', Sql ,'SerialNumber',IntToStr(SerialNumber)]);
  Rows := TDBEngine.Inst().Query(Sql);
end;

class function TDBRecordOfItem.GetCreateTableSql():TStringList;
begin
  Result := TStringList.Create;
  Result.Add('CREATE TABLE `Item` (');
  Result.Add('`SerialNumber` bigint(20) unsigned ,');
  Result.Add('`ActorID` bigint(20) unsigned comment ''所属角色ID'',');
  Result.Add('`ItemName` text comment ''物品名称'',');
  Result.Add('`ConfigID` int(11) comment ''配置的物品ID'',');
  Result.Add('`Attrs` json comment ''物品属性'',');
  Result.Add('`ExpireTime` datetime comment ''物品属性 过期时间'',');
  Result.Add('PRIMARY KEY (`SerialNumber`)');
  Result.Add(')');
end;

class function TDBRecordOfItem.FeatchToList(MysqlRows:TMysqlRow):TList<TDBRecordOfItem>;
var
   V:TDBRecordOfItem;
   I:Integer;
   Str:String;
begin
  Result := TList<TDBRecordOfItem>.Create;
  for I := 0 to MysqlRows.Count - 1 do 
  begin
    V := TDBRecordOfItem.Create();
    V.SerialNumber := MysqlRows.FieldByNameAsInteger('SerialNumber');
    V.ActorID := MysqlRows.FieldByNameAsInteger('ActorID');
    MysqlRows.FieldByName('ItemName',V.ItemName);
    V.ConfigID := MysqlRows.FieldByNameAsInteger('ConfigID');
    MysqlRows.FieldByName('Attrs',V.Attrs);
    MysqlRows.FieldByName('ExpireTime',Str);
    V.ExpireTime := MysqlDateTimeStrToDateTime(Str);
    Result.Add(V);
    MysqlRows.Next();
  end;
end;

class function TDBRecordOfItem.QueryAnd_ActorID(ActorID : UInt64):TList<TDBRecordOfItem>;
var
  Rows : TMysqlRow;
  Sql:String;
begin;
  Sql := 'select * from Item where ' + 'ActorID = ' + IntToStr(ActorID) +;
  Rows := TDBEngine.Inst().Query(Sql);
  Result := FeatchToList(Rows);
end;


initialization
  TDBEngine.RegisterClass('Item' , TDBRecordOfItem );
end.
