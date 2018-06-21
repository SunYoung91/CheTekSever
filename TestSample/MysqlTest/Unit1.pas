unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, IdBaseComponent, IdComponent,
  IdTCPConnection, IdTCPClient, IdHTTP,CheTek.Mysql,StringUtils,CheTek.DBEngine,DB_Actor;

type
  TForm1 = class(TForm)
    mmo_Log: TMemo;
    btn3: TButton;
    btn_showCreateTable: TButton;
    btn_EngineStart: TButton;
    procedure btn3Click(Sender: TObject);
    procedure btn_showCreateTableClick(Sender: TObject);
    procedure btn_EngineStartClick(Sender: TObject);
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


procedure TForm1.btn_EngineStartClick(Sender: TObject);
var
  DataEngine : TDBEngine;
begin
  DataEngine := TDBEngine.Create('192.168.2.168',3306,'root','mima00544','liyun_mir');
  DataEngine.RegisterClass(TDBRecordOfActor);
  DataEngine.Init;
  DataEngine.Free;
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
