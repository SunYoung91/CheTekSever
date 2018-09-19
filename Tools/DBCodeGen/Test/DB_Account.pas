unit DB_Account;

interface
uses DBRecordBase,System.Classes,AttrType,System.SysUtils,CheTek.SerialObject,CheTek.Mysql,System.Generics.Collections, Classes  ;
Type
  TDBRecordOfAccount = class(TDBRecordBase)
    Account:string;
    Password:string;
    ChannelID:Integer;
    ChannelName:String;
    RegisterTime:TDateTime;
    MobilePhoneNumber:String;
    EMail:string;
    IdentificationNumber:String;
    Question1:String;
    Question2:String;
    Answer1:String;
    Answer2:String;
    Recharge:Int64;
    procedure Insert();
    procedure Update();
    class function GetCreateTableSql():TStringList;override;
    class function FeatchToList(MysqlRows:TMysqlRow):TList<TDBRecordOfAccount>;
  end;


implementation
uses CheTek.DBEngine;
procedure TDBRecordOfAccount.Insert();
var
  Sql:String;
  Fields:String;
  Values:String;
  Rows : TMysqlRow;
  V : Array[1..13] of String;
begin
   Fields := Fields + 'Account';
  V[1] := Self._Q(Account);
   Fields := Fields + 'Password';
  V[2] := Self._Q(Password);
   Fields := Fields + 'ChannelID';
  V[3] := IntToStr(ChannelID);
   Fields := Fields + 'ChannelName';
  V[4] := Self._Q(ChannelName);
   Fields := Fields + 'RegisterTime';
  V[5] := FormatDateTime('YYYY-MM-DD HH:NN:SS', Self.RegisterTime);;
   Fields := Fields + 'MobilePhoneNumber';
  V[6] := Self._Q(MobilePhoneNumber);
   Fields := Fields + 'EMail';
  V[7] := Self._Q(EMail);
   Fields := Fields + 'IdentificationNumber';
  V[8] := Self._Q(IdentificationNumber);
   Fields := Fields + 'Question1';
  V[9] := Self._Q(Question1);
   Fields := Fields + 'Question2';
  V[10] := Self._Q(Question2);
   Fields := Fields + 'Answer1';
  V[11] := Self._Q(Answer1);
   Fields := Fields + 'Answer2';
  V[12] := Self._Q(Answer2);
   Fields := Fields + 'Recharge';
  V[13] := IntToStr(Recharge);
  Sql := format('INSERT INTO Account (Fields) VALUES (Values);',[ V[1], V[2], V[3], V[4], V[5], V[6], V[7], V[8], V[9], V[10], V[11], V[12], V[13]]);
  Rows := TDBEngine.Inst().Query(Sql);
end;

procedure TDBRecordOfAccount.Update();
var
  Sql:String;
  Rows : TMysqlRow;
begin
  Sql := 'Account = ' + Self._Q(Account);
  Sql := Sql + ', '+ 'Password = ' + Self._Q(Password);
  Sql := Sql + ', '+ 'ChannelID = ' + IntToStr(ChannelID);
  Sql := Sql + ', '+ 'ChannelName = ' + Self._Q(ChannelName);
  Sql := Sql + ', '+ 'RegisterTime = ' + FormatDateTime('YYYY-MM-DD HH:NN:SS', Self.RegisterTime);;
  Sql := Sql + ', '+ 'MobilePhoneNumber = ' + Self._Q(MobilePhoneNumber);
  Sql := Sql + ', '+ 'EMail = ' + Self._Q(EMail);
  Sql := Sql + ', '+ 'IdentificationNumber = ' + Self._Q(IdentificationNumber);
  Sql := Sql + ', '+ 'Question1 = ' + Self._Q(Question1);
  Sql := Sql + ', '+ 'Question2 = ' + Self._Q(Question2);
  Sql := Sql + ', '+ 'Answer1 = ' + Self._Q(Answer1);
  Sql := Sql + ', '+ 'Answer2 = ' + Self._Q(Answer2);
  Sql := Sql + ', '+ 'Recharge = ' + IntToStr(Recharge);
  Sql := Format('UPDATE %s SET %s WHERE %s = %s ;', ['Account', Sql ,'Account',Self._Q(Account)]);
  Rows := TDBEngine.Inst().Query(Sql);
end;

class function TDBRecordOfAccount.GetCreateTableSql():TStringList;
begin
  Result := TStringList.Create;
  Result.Add('CREATE TABLE `Account` (');
  Result.Add('`Account` varchar(20) comment ''所属账号'',');
  Result.Add('`Password` text comment ''密码'',');
  Result.Add('`ChannelID` int(11) comment ''渠道ID'',');
  Result.Add('`ChannelName` text comment ''渠道名称'',');
  Result.Add('`RegisterTime` datetime comment ''注册时间'',');
  Result.Add('`MobilePhoneNumber` text comment ''手机号码'',');
  Result.Add('`EMail` text comment ''电子邮箱地址'',');
  Result.Add('`IdentificationNumber` text comment ''身份证号码'',');
  Result.Add('`Question1` text comment ''问题1'',');
  Result.Add('`Question2` text comment ''问题2'',');
  Result.Add('`Answer1` text comment ''回答1'',');
  Result.Add('`Answer2` text comment ''回答2'',');
  Result.Add('`Recharge` bigint(20) comment ''累计充值'',');
  Result.Add('PRIMARY KEY (`Account`)');
  Result.Add(')');
end;

class function TDBRecordOfAccount.FeatchToList(MysqlRows:TMysqlRow):TList<TDBRecordOfAccount>;
var
   V:TDBRecordOfAccount;
   I:Integer;
   Str:String;
begin
  Result := TList<TDBRecordOfAccount>.Create;
  for I := 0 to MysqlRows.Count - 1 do 
  begin
    V := TDBRecordOfAccount.Create();
    MysqlRows.FieldByName('Account',V.Account);
    MysqlRows.FieldByName('Password',V.Password);
    V.ChannelID := MysqlRows.FieldByNameAsInteger('ChannelID');
    MysqlRows.FieldByName('ChannelName',V.ChannelName);
    MysqlRows.FieldByName('RegisterTime',Str);
    V.RegisterTime := MysqlDateTimeStrToDateTime(Str);
    MysqlRows.FieldByName('MobilePhoneNumber',V.MobilePhoneNumber);
    MysqlRows.FieldByName('EMail',V.EMail);
    MysqlRows.FieldByName('IdentificationNumber',V.IdentificationNumber);
    MysqlRows.FieldByName('Question1',V.Question1);
    MysqlRows.FieldByName('Question2',V.Question2);
    MysqlRows.FieldByName('Answer1',V.Answer1);
    MysqlRows.FieldByName('Answer2',V.Answer2);
    V.Recharge := MysqlRows.FieldByNameAsInteger('Recharge');
    Result.Add(V);
    MysqlRows.Next();
  end;
end;



initialization
  TDBEngine.RegisterClass('Account' , TDBRecordOfAccount );
end.
