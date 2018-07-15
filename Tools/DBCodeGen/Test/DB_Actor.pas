unit DB_Actor;

interface
uses DBRecordBase,System.Classes,AttrType,System.SysUtils,CheTek.SerialObject,CheTek.Mysql,System.Generics.Collections, Classes  ;
Type
  TDBRecordOfActor = class(TDBRecordBase)
    ActorID:UInt64;
    Account:string;
    ActorName:string;
    ServerID:Word;
    OrginalServerID:Word;
    procedure Insert();
    procedure Update();
    class function GetCreateTableSql():TStringList;override;
    class function FeatchToList(MysqlRows:TMysqlRow):TList<TDBRecordOfActor>;
  class function QueryOr_ActorID_Account(ActorID : UInt64;Account : string):TList<TDBRecordOfActor>;
  class function QueryAnd_Account_ActorName(Account : string;ActorName : string):TList<TDBRecordOfActor>;
  end;


implementation
uses CheTek.DBEngine;
procedure TDBRecordOfActor.Insert();
var
  Sql:String;
  Fields:String;
  Values:String;
  Rows : TMysqlRow;
  V : Array[1..5] of String;
begin
   Fields := Fields + 'ActorID';
  V[1] := IntToStr(ActorID);
   Fields := Fields + 'Account';
  V[2] := Self._Q(Account);
   Fields := Fields + 'ActorName';
  V[3] := Self._Q(ActorName);
   Fields := Fields + 'ServerID';
  V[4] := IntToStr(ServerID);
   Fields := Fields + 'OrginalServerID';
  V[5] := IntToStr(OrginalServerID);
  Sql := format('INSERT INTO Actor (Fields) VALUES (Values);',[ V[1], V[2], V[3], V[4], V[5]]);
  Rows := TDBEngine.Inst().Query(Sql);
end;

procedure TDBRecordOfActor.Update();
var
  Sql:String;
  Rows : TMysqlRow;
begin
  Sql := 'ActorID = ' + IntToStr(ActorID);
  Sql := Sql + ', '+ 'Account = ' + Self._Q(Account);
  Sql := Sql + ', '+ 'ActorName = ' + Self._Q(ActorName);
  Sql := Sql + ', '+ 'ServerID = ' + IntToStr(ServerID);
  Sql := Sql + ', '+ 'OrginalServerID = ' + IntToStr(OrginalServerID);
  Sql := Format('UPDATE %s SET %s WHERE %s = %s ;', ['Actor', Sql ,'ActorID',IntToStr(ActorID)]);
  Rows := TDBEngine.Inst().Query(Sql);
end;

class function TDBRecordOfActor.GetCreateTableSql():TStringList;
begin
  Result := TStringList.Create;
  Result.Add('CREATE TABLE `Actor` (');
  Result.Add('`ActorID` bigint(20) unsigned comment ''角色ID'',');
  Result.Add('`Account` varchar(20) comment ''所属账号'',');
  Result.Add('`ActorName` text comment ''角色名'',');
  Result.Add('`ServerID` smallint(6) unsigned comment ''服务器ID'',');
  Result.Add('`OrginalServerID` smallint(6) unsigned comment ''原始服务器ID'',');
  Result.Add('PRIMARY KEY (`ActorID`,`Account`)');
  Result.Add(')');
end;

class function TDBRecordOfActor.FeatchToList(MysqlRows:TMysqlRow):TList<TDBRecordOfActor>;
var
   V:TDBRecordOfActor;
   I:Integer;
   Str:String;
begin
  Result := TList<TDBRecordOfActor>.Create;
  for I := 0 to MysqlRows.Count - 1 do 
  begin
    V := TDBRecordOfActor.Create();
    V.ActorID := MysqlRows.FieldByNameAsInteger('ActorID');
    MysqlRows.FieldByName('Account',V.Account);
    MysqlRows.FieldByName('ActorName',V.ActorName);
    V.ServerID := MysqlRows.FieldByNameAsInteger('ServerID');
    V.OrginalServerID := MysqlRows.FieldByNameAsInteger('OrginalServerID');
    Result.Add(V);
    MysqlRows.Next();
  end;
end;

class function TDBRecordOfActor.QueryOr_ActorID_Account(ActorID : UInt64;Account : string):TList<TDBRecordOfActor>;
var
  Rows : TMysqlRow;
  Sql:String;
begin;
  Sql := 'select * from Actor where ' + 'ActorID = ' + IntToStr(ActorID) + '  or ' + 'Account = ' + Self._Q(Account) +;
  Rows := TDBEngine.Inst().Query(Sql);
  Result := FeatchToList(Rows);
end;
class function TDBRecordOfActor.QueryAnd_Account_ActorName(Account : string;ActorName : string):TList<TDBRecordOfActor>;
var
  Rows : TMysqlRow;
  Sql:String;
begin;
  Sql := 'select * from Actor where ' + 'Account = ' + Self._Q(Account) + ' and ' + 'ActorName = ' + Self._Q(ActorName) +;
  Rows := TDBEngine.Inst().Query(Sql);
  Result := FeatchToList(Rows);
end;


initialization
  TDBEngine.RegisterClass('Actor' , TDBRecordOfActor );
end.
