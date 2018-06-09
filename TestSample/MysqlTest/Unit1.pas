unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, IdBaseComponent, IdComponent,
  IdTCPConnection, IdTCPClient, IdHTTP,CheTek.Mysql;

type
  TForm1 = class(TForm)
    mmo_Log: TMemo;
    btn3: TButton;
    procedure btn3Click(Sender: TObject);
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
begin
  TMySql.SetErrorProc(MysqlError);

  mysql := TMySql.Create;
  if not mysql.Connect('127.0.0.1',3306,'liyun','12356456','mysql') then
  begin
    mysql.Free;
    Exit;
  end;

  TableList := TStringList.Create;
  mysql.GetAllTable('mysql',TableList);

  for i := 0 to TableList.Count - 1 do
  begin
    TableList[i] := 'truncate table ' +  TableList[i] + ';';
  end;

  mmo_Log.Lines.Assign(TableList);

  TableList.Free;

  mysql.Free;
end;


end.
