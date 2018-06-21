unit DB_Actor;

interface
uses DBRecordBase,System.Classes,AttrType,System.SysUtils,CheTek.SerialObject;
Type
  TDBRecordOfActor = class(TDBRecordBase)
    ActorID:UInt64;
    ActorID:UInt64;
    Account:string;
    Account:string;
    ActorName:string;
    ActorName:string;
    Attr:TAttrs;
    procedure Insert();
    procedure Update();
    class function GetCreateTableSql():TStringList;override;
    class function FeatchToList(MysqlRows:TMysqlRow):TList<TDBRecordOfActor>;override;
  end;


implementation
uses CheTek.DBEngine;
procedure TDBRecordOfActor.Insert();
var
  Sql:String;
  V : Array[1..7] of String;
begin
  V[1] := Self._Q(IntToStr(ActorID));
  V[2] := Self._Q(IntToStr(ActorID));
  V[3] := Self._Q(Account);
  V[4] := Self._Q(Account);
  V[5] := Self._Q(ActorName);
  V[6] := Self._Q(ActorName);
  V[7] := Self._Q(TSerialObject.RecordToJson<TAttrs>(Attr));
  Sql := format('INSERT INTO Actor (ActorID,ActorID,Account,Account,ActorName,ActorName,Attr) VALUES (%s,%s,%s,%s,%s,%s,%s);',[ V[1], V[2], V[3], V[4], V[5], V[6], V[7]]);
end;

procedure TDBRecordOfActor.Update();
var
  Sql:String;
begin
  Sql := 'ActorID = ' + Self._Q(IntToStr(ActorID));
  Sql := Sql + ', '+ 'ActorID = ' + Self._Q(IntToStr(ActorID));
  Sql := Sql + ', '+ 'Account = ' + Self._Q(Account);
  Sql := Sql + ', '+ 'Account = ' + Self._Q(Account);
  Sql := Sql + ', '+ 'ActorName = ' + Self._Q(ActorName);
  Sql := Sql + ', '+ 'ActorName = ' + Self._Q(ActorName);
  Sql := Sql + ', '+ 'Attr = ' + Self._Q(TSerialObject.RecordToJson<TAttrs>(Attr));
  Sql := Format('UPDATE %s SET %s WHERE %s = %s ;', ['Actor', Sql ,'ActorID',Self._Q(IntToStr(ActorID))]);
end;

class function TDBRecordOfActor.GetCreateTableSql():TStringList;
begin
  Result := TStringList.Create;
  Result.Add('CREATE TABLE `Actor` (');
  Result.Add('`ActorID` bigint(20) unsigned comment ''角色ID'',');
  Result.Add('`ActorID` bigint(20) unsigned comment ''角色ID'',');
  Result.Add('`Account` varchar(20) comment ''所属账号'',');
  Result.Add('`Account` text comment ''所属账号'',');
  Result.Add('`ActorName` text comment ''角色名'',');
  Result.Add('`ActorName` text comment ''角色名'',');
  Result.Add('`Attr` json ,');
  Result.Add('PRIMARY KEY (`ActorID`,`Account`)');
  Result.Add(')');
end;

class function TDBRecordOfActor.FeatchToList(MysqlRows:TMysqlRow):TList<TDBRecordOfActor>;override;
var
   V:TDBRecordOfActor;
   I:Integer;
begin
  Result := TList<TDBRecordOfActor>.Create;
  for I := 0 to MysqlRows.Count - 1 do 
  begin
    V := TDBRecordOfActor.Create();
    V.ActorID := FieldByNameAsInteger('ActorID');
    V.ActorID := FieldByNameAsInteger('ActorID');
    FieldByName('Account',V.Account);
    FieldByName('Account',V.Account);
    FieldByName('ActorName',V.ActorName);
    FieldByName('ActorName',V.ActorName);
    FieldByName('Attr',V.Attr);
    Result.Add(V);
    MysqlRows.Next();
  end;
end

class function TDBRecordOfActor.QueryAnd_Account_ActorName(Account : string;ActorName : string;):TList<TDBRecordOfActor>
var
Rows : MysqlRows
Sql:String;
Sql := 'select * from Actor where ' + 'Account = ' + Self._Q(Account) + 'and' + 'ActorName = ' + Self._Q(ActorName) + 'and' + ;
Rows := TDBEngine.Inst().Query(Sql);


initialization
  TDBEngine.RegisterClass('Actor' , TDBRecordOfActor );
end.
