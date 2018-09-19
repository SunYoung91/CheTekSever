unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, IdBaseComponent, IdComponent,
  IdTCPConnection, IdTCPClient, IdHTTP,CheTek.Mysql,StringUtils,CheTek.DBEngine,{DB_Actor,DB_Item,DB_Account,}DBRecordBase,AttrType,System.Generics.Collections;

type
  TForm1 = class(TForm)
    mmo_Log: TMemo;
    btn3: TButton;
    btn_showCreateTable: TButton;
    btn_EngineStart: TButton;
    btn_mysqlDateTime: TButton;
    btn_insert: TButton;
    btn_query: TButton;
    btn_create_danyao_alter: TButton;
    btn_checkdata: TButton;
    procedure btn3Click(Sender: TObject);
    procedure btn_showCreateTableClick(Sender: TObject);
    procedure btn_EngineStartClick(Sender: TObject);
    procedure btn_mysqlDateTimeClick(Sender: TObject);
    procedure btn_insertClick(Sender: TObject);
    procedure btn_queryClick(Sender: TObject);
    procedure btn_create_danyao_alterClick(Sender: TObject);
    procedure btn_checkdataClick(Sender: TObject);
  private
    { Private declarations }
  public
    mysql : TMySql;
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses md5,mysql;
{$R *.dfm}

procedure MysqlError(sText : String;nType : Integer);
begin
  Form1.mmo_Log.Lines.Add(sText);
end;

procedure TForm1.btn3Click(Sender: TObject);
var
  I:Integer;
  TableList : TStrings;
  DataBase : TStrings;
  Str :String;
begin
  TMySql.SetErrorProc(MysqlError);

  mysql := TMySql.Create;
  if not mysql.Connect('rm-bp11b3ieo1m1msg6vdo.mysql.rds.aliyuncs.com',3306,'xmsy_game','ChangYu_xmsy_dba1@china_east','mysql') then
  begin
    mysql.Free;
    Exit;
  end;

  TableList := TStringList.Create;
  DataBase := TStringList.Create;

  mysql.GetAllDataBase(DataBase);
  mysql.GetAllTable('mysql',TableList);

  for i := 0 to TableList.Count - 1 do
  begin
    TableList[i] := 'truncate table ' +  TableList[i] + ';';
  end;

  for i := 0 to DataBase.Count - 1 do
  begin
    if TStringUtil.StartWith(DataBase[i],'xmsy_game') then
    begin
      mmo_Log.Lines.Add('truncate table ' + DataBase[i] + '.actors_backup;');
      mmo_Log.Lines.Add('truncate table ' + DataBase[i] + '.roles_backup;');
    end;

  end;

 // mmo_Log.Lines.Assign(TableList);

  TableList.Free;

  mysql.Free;
end;


procedure TForm1.btn_checkdataClick(Sender: TObject);
var
  DataEngine : TDBEngine;
  DataBases : TStrings;
  I , J :Integer;
  Str : String;
  Sql : TStringList;
  Row : TMysqlRow;
  dbid : Int64;
  serverid  : Integer;
  totalCount : Integer;
  Name:String;
begin
  DataEngine := TDBEngine.Create('rm-bp11b3ieo1m1msg6vdo.mysql.rds.aliyuncs.com',3306,'xmsy_dev','2b8db88710cdc4615ce2f41d18793903','');
  DataEngine.Init;
  DataBases := TStringList.Create();
  DataEngine.GetAllDataBase(DataBases);

  Sql := TStringList.Create();
  totalCount := 0;
  for I := 0 to DataBases.Count - 1 do
  begin
    Str := DataBases[i];
    if Str.StartsWith('xmsy_game') then
    begin
      DataEngine.Query('use ' + Str );
      Row := DataEngine.Query('select dbid,serverindex,actorname from actors where level >= 180 and gold >= 20000000000;');
      if Row = nil then
        Continue;
      if Row.Count > 0 then
      begin
        Row.First();
        for J := 0 to Row.Count - 1 do
        begin
           totalCount := totalCount + 1;
           dbid := Row.FieldByNameAsInteger('dbid');
           serverid := Row.FieldByNameAsInteger('serverindex');
           Row.FieldByName('actorname',name);
           mmo_Log.Lines.Add(serverid.ToString() + ',' + dbid.ToString() + ',' );
           Row.Next();
        end;
      end;

    end;
  end;

  mmo_Log.Lines.Add('count:' + totalCount.ToString());

  Sql.SaveToFile('D:\adddanyao.sql');
  Sql.Free;
  DataBases.Free;

end;

procedure TForm1.btn_create_danyao_alterClick(Sender: TObject);
var
  DataEngine : TDBEngine;
  DataBases : TStrings;
  I:Integer;
  Str : String;
  Sql : TStringList;
  DataFile:TStringList;
begin
// œ…ƒß…Ò”Ú
  //ataEngine := TDBEngine.Create('rm-bp11b3ieo1m1msg6vdo.mysql.rds.aliyuncs.com',3306,'xmsy_dev','2b8db88710cdc4615ce2f41d18793903','');

     //œ…ƒß’Ê∞ÆÕÊ
 // DataEngine := TDBEngine.Create('sh-cdb-9mgjct0k.sql.tencentcdb.com',63046,'xmsy_dev','2b8db88710cdc4615ce2f41d18793903','');
  //ÕÚµ¿Œ‰…Ò2
  //DataEngine := TDBEngine.Create('sh-cdb-09a7eius.sql.tencentcdb.com',63081,'wdws2_dev','2b8db88710cdc4615ce2f41d18793903','');

  //DataEngine := TDBEngine.Create('rm-bp1518ev77q9wau01ao.mysql.rds.aliyuncs.com',3306,'wdws_dev','2b8db88710cdc4615ce2f41d18793903','');

  DataEngine := TDBEngine.Create('rm-bp116vd8ic9f43237bo.mysql.rds.aliyuncs.com',3306,'lhxc_dev','2b8db88710cdc4615ce2f41d18793903','');
  DataEngine.Init;

  DataBases := TStringList.Create();
  DataEngine.GetAllDataBase(DataBases);

  Sql := TStringList.Create();
  DataFile := TStringList.Create;
  DataFile.LoadFromFile('C:\Users\kadin\Desktop\¡“ª–«≥Ωtoœ…ƒß…Ò”Ú.sql');
  for I := 0 to DataBases.Count - 1 do
  begin
    Str := DataBases[i];
    if Str.StartsWith('lhxc_game') then
    begin
      Sql.Add('use ' + Str + ';');
      Sql.AddStrings(DataFile);
    end;

  end;

  Sql.SaveToFile('C:\Users\kadin\Desktop\lhxc_to_wdws2.sql');
  Sql.Free;
  DataBases.Free;

end;

procedure TForm1.btn_EngineStartClick(Sender: TObject);
var
  DataEngine : TDBEngine;
begin
  DataEngine := TDBEngine.Create('192.168.2.168',3306,'root','mima00544','liyun_mir');
  //DataEngine.RegisterClass(TDBRecordOfActor);
  //DataEngine.RegisterClass(TDBRecordOfItem);
  //DataEngine.RegisterClass(TDBRecordOfAccount);
  DataEngine.Init;

end;

procedure TForm1.btn_insertClick(Sender: TObject);
var
 // Item : TDBRecordOfActor;
  Str:String;
begin
//  Str := '¬Ë¬Ë¬”¥';
//  Str := #39 + Str + #39;
//  Item := TDBRecordOfActor.Create;
//  Item.ActorID := 12131321;
//  Item.Account := 'ƒ„∂Æ…œ√Ê';
//  Item.ActorName := 'abcdefg';
//  Item.Info := TTestMyList.Create;
//  Item.Info.Name := 'Dad';
//  Item.Info.Age := 18;
//  Item.Update;
end;

procedure TForm1.btn_mysqlDateTimeClick(Sender: TObject);
var
  DateTime : TDateTime;
begin
  DateTime := MysqlDateTimeStrToDateTime('2018-06-22 15:23:16');
  mmo_Log.Lines.Add(FormatDateTime('YYYY_MM_DD_HH_NN_SS',DateTime));
end;

procedure TForm1.btn_queryClick(Sender: TObject);
//var
  //Items : TList<TDBRecordOfActor>;
begin

  //Items := TDBRecordOfActor.QueryAnd_Account_ActorName('ƒ„∂Æ…œ√Ê','abcdefg');
end;

procedure TForm1.btn_showCreateTableClick(Sender: TObject);
var
  I:Integer;
  TableList : TStrings;
  DataBase : TStrings;
  Str :String;
begin
  TMySql.SetErrorProc(MysqlError);

  mysql := TMySql.Create;
  if not mysql.Connect('rm-bp11b3ieo1m1msg6vdo.mysql.rds.aliyuncs.com',3306,'xmsy_game','ChangYu_xmsy_dba1@china_east','xmsy_game_yinghe_1') then
  begin
    mysql.Free;
    Exit;
  end;

  mysql.CharSet := 'UTF8';
  TableList := TStringList.Create;

  mysql.GetCreateTableSql('actors',TableList);

  mmo_Log.Lines.Assign(TableList);

  TableList.Free;

  mysql.Free;
end;
end.
