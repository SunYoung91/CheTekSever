unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, IdBaseComponent, IdComponent,
  IdTCPConnection, IdTCPClient, IdHTTP,IdObjs,uTMySql;

type
  TForm1 = class(TForm)
    lbledtOrderID: TLabeledEdit;
    lbledt_Account: TLabeledEdit;
    lbledt_srvID: TLabeledEdit;
    lbledt_Count: TLabeledEdit;
    btn_Send: TButton;
    mmo_Log: TMemo;
    idhtp1: TIdHTTP;
    btn1: TButton;
    btn2: TButton;
    btn3: TButton;
    procedure btn_SendClick(Sender: TObject);
    procedure btn1Click(Sender: TObject);
    procedure btn2Click(Sender: TObject);
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
uses md5,CA_EDPass,mysql;
{$R *.dfm}

procedure MysqlError(sText : AnsiString;nType : Integer);
begin
  Form1.mmo_Log.Lines.Add(sText);
end;
procedure TForm1.btn1Click(Sender: TObject);
var
  sVer : string;
  Row : TMysqlRow;
  DBList : TStrings;
begin
  uTMySql.SetErrorProc(MysqlError);

  mysql := TMySql.Create;
  if not mysql.Connect('192.168.10.250','root','123456abc','cq_actor',3306) then
  begin
    mmo_Log.Lines.Add(mysql.ErrorDesc);
  end;

  mysql.CharSet := 'utf8';
  {
  DBList := TStringList.Create;
  mysql.GetAllDataBase(DBList);
  mysql.GetAllTable('cq_actor',DBList);
  mmo_Log.Lines.AddStrings(DBList);
  DBList.Free;  }


  if mysql.Query('select * from actors limit 10') then
  begin
    Row := mysql.ResultRow;
    while not Row.EOF() do
    begin
      sVer := Row.FieldByName('actorname');
      sVer := Utf8ToAnsi(sVer);
      mmo_Log.Lines.Add(sVer);
      Row.Next();
    end;
  end else
  begin
    mmo_Log.Lines.Add(mysql.ErrorDesc);
  end;

  mysql.Free;
end;

procedure TForm1.btn2Click(Sender: TObject);
const
   sQuerySQL = 'call Consume(%d,%d,"%s", %d)';
var
   Row : TMysqlRow;
   sSqlText :string;
  nUserID: Integer;
  nCount: Integer;
  sCharName: string;
  nServerID: Integer;
begin
  uTMySql.SetErrorProc(MysqlError);

  mysql := TMySql.Create;
  if not mysql.Connect('192.168.10.250','root','123456abc','amdb',3306) then
  begin
    mmo_Log.Lines.Add(mysql.ErrorDesc);

  end;

  //mysql.CharSet := 'utf8';
  nUserID := 31;
  nCount := 100;
  sCharName := AnsiToUtf8('¿ÓÕı∞Õ');
  nServerID := 1;

  sSqlText := Format(sQuerySQL,[nUserID,nCount,sCharName,nServerID]);

  if mysql.Query(sSqlText) then
  begin
    Row := mysql.ResultRow;
    if Row.FieldCount <= 0 then
    begin
      ShowMessage('-101');
    end;
    mmo_Log.Lines.Add(IntToStr(Row.FieldAsInteger(0)));
    mysql.ResetQuery();
  end else
  begin
    //mmo_Log.Lines.Add(mysql.ErrorDesc);
  end;

  mysql.Free;
end;

procedure TForm1.btn3Click(Sender: TObject);
var
   i : Integer;
   Row : TMysqlRow;
   sSqlText :string;
  nUserID: Integer;
  nCount: Integer;
  sCharName: string;
  nServerID: Integer;
  TableList : TStrings;
begin
  uTMySql.SetErrorProc(MysqlError);

  mysql := TMySql.Create;
  if not mysql.Connect('192.168.10.250','root','123456abc','cq_actor',3306) then
  begin
    mmo_Log.Lines.Add(mysql.ErrorDesc);

  end;
  TableList := TStringList.Create;
  mysql.GetAllTable('cq_actor',TableList);

  for i := 0 to TableList.Count - 1 do
  begin
    TableList[i] := 'truncate table ' +  TableList[i] + ';';
  end;
  mmo_Log.Lines.Assign(TableList);

  mysql.Free;
end;

procedure TForm1.btn_SendClick(Sender: TObject);
const
  EnKey = 'GAmqXI8ao8kGAOIQmjFVqg==';
var
  sCheck : string;
  sOPId: AnsiString;
  sAcc: AnsiString;
  sMoney: AnsiString;
  sServer: AnsiString;
  sRaw : AnsiString;
  sSign : AnsiString;
  ParamList : TStrings;
  sResult : AnsiString;
  sKey : AnsiString;
begin

    ParamList := nil;
    sOPId := lbledtOrderID.Text;
    sAcc := lbledt_Account.Text;
    sMoney := lbledt_Count.Text;
    sServer := lbledt_srvID.Text;
    sRaw := sMoney;
    sKey := DecodePassword(EnKey,GetKeyKey(),true);
    sCheck := Format('opid=%s&account=%s&money=%s&server=%s&key=%s',[sOPId, sAcc, sMoney, sServer, sKey]);
    sSign := md5.MD5EncryptString(sCheck);

    try
    
      sResult := format('opid=%s&account=%s&money=%s&server=%s&sign=%s&ram=%s',[sOpid,sAcc,sMoney,sServer,sSign,sMoney]);
      mmo_Log.Lines.Add(sResult);
      ParamList := TStringList.Create;
      ParamList.add('opid=' + sOpid);
      ParamList.add('account=' + sAcc);
      ParamList.add('money=' + sMoney);
      ParamList.add('server=' + sServer);
      ParamList.add('sign=' + sSign );
      ParamList.add('ram=' + sMoney);
      sResult := idhtp1.Post('http://192.168.10.250:8088/djrm/wyi/pay',ParamList);
      mmo_Log.Lines.Add(sResult);
      
    except
      on E :Exception do
      begin
        ShowMessage('“Ï≥£:' + E.Message);
      end;

    end;

    if ParamList <> nil then ParamList.Free;
    
end;

end.
