(**)
(* delphi Mysql �����򵥷�װ ��Ҫ libmysql.dll *)
(* date : 2015/11/4*)
(* author : һ·���� qq:531662161)
(* �ο�: http://www.cnblogs.com/doorsky/archive/2010/01/05/1639980.html *)

unit uTMySql;

interface

uses
  Windows, Messages, SysUtils, Classes,
  Mysql;
type

  //���в�ѯ�󷵻صĽ����
  TMysqlRow = class
  private
    m_FieldCount: Integer; //��ѯ��¼��������

    m_RowCount: Integer; //��ѯ��¼��������

    m_pMysqlRes: PMYSQL_RES; //����ԭʼ�����

    m_pRows: PMYSQL_ROW; //Mysql ���ؽ����

    m_nRowIndex: Integer; //��ǰ������

    m_nFieldIndex: Integer; //��ǰ������

    m_FieldIndexList: TStringList; //�ֶ����ƶ�Ӧ��Index ����ʵ��FieldByName()

    procedure SetEmpty(); //����Ϊ�����Ϊ��

    procedure FieldNameList();

    function FetchResRow(): Boolean; //������Դ

    procedure CleanUp(); //������Դ

    function FieldIndex(sName: AnsiString): Integer; //�����ֶ��� ��ȡ�ֶ������±�

    destructor Destroy; override; //��һ�л� �� H2269 ����ʾ ������
  public
    constructor Create; //������� �ֶ�Create Ӧ��������
    destructor free; //������� �ֶ�Free Ӧ����������

    function EOF: Boolean; //��ǰ�Ƿ��Ѿ������ݼ�β����(�޷�������ȡ)

    procedure First; //�ƶ�������

    procedure Next; //�ƶ�����һ��
    function FieldAsPointer(nFieldIndex: Integer): PAnsiChar;
    function Field(nFieldIndex: Integer): AnsiString;
    function FieldAsInteger(nFieldIndex: Integer): Integer;
    procedure FieldToBuffer(nFieldIndex: Integer; pPtr: Pointer; nLength: Integer);

    function FieldByName(sFieldName: AnsiString): AnsiString;
    function FieldByNameAsInteger(sFieldName: AnsiString): Integer;
    procedure FieldByNameToBuffer(sFieldName: AnsiString; pPtr: Pointer; nLength: Integer);

    property Count: Integer read m_RowCount;
    property FieldCount: Integer read m_FieldCount;

  end;

  //��������Ļص����� ͨ�� SetErrorProc ��һ���ص����� �����ִ�����������ӡ����
  TMySqlErrorFunction = procedure(sText: AnsiString; nType: Integer);

  TMySql = class
  private
    m_Mysql: PMYSQL;
    m_MysqlRes: PMYSQL_RES;
    m_QueryResult: TMysqlRow;
    m_sCharSet: AnsiString;
    m_sDataBase: AnsiString;
    m_sClientVer: AnsiString;
    m_sServerVer: AnsiString; //�����DBʹ�õİ汾
    m_sErrorDesc: AnsiString; //��������
    m_sQueryErrorText: AnsiString; //��һ��ִ�� Query ���� Exec ���Sqlָ��
    procedure SetCharSet(const Value: AnsiString);
    procedure OnAfterQuery(nError: integer);
    function OnAfterExec(nError: integer): Integer; //����ֵ ΪӰ������
    procedure RaiseError(nError: Integer; nType: Integer = 0); overload;
    procedure RaiseError(sDesc: AnsiString; nType: Integer = 0); overload;
    function isConnectd(): Boolean;
    function RealQuery(SqlText: PAnsiChar; boNeedResult: Boolean; var nAffectRow: Integer): Integer; //ִ�в�ѯ
  protected

  public
    constructor Create();
    destructor Destroy(); override;
    //��ȡ���е����ݿ�

    function GetAllDataBase(var DataBaseList: TStrings): Boolean;

    //��ȡĳ��DB�����б�
    function GetAllTable(const DBName: AnsiString; var DataBaseList: TStrings): Boolean;

    //���ӷ�����
    function Connect(host, user, passwd, db: AnsiString; port: LongInt = 3306): Boolean;

    procedure Close(); //�ر��� Mysql ������

    //ִ��Query SQL���
    function Query(SqlText: AnsiString): Boolean; //�����Ƿ�ִ�гɹ�

    //ִ�� SQL���
    function Exec(SqlText: AnsiString): Integer; //�������õ�����

   (* Query �� Exec ���������� һ����Ҫ ���ؽ���� һ������Ҫ���ؽ���� *)
   (* ����ͬ���� ����ѯ��� ����Ҫִ�� ResetQuery �ͷ���Դ *)

    procedure ResetQuery(); //���ò�ѯ�ͷ���Դ

    function SetDataBase(const Value: AnsiString): Boolean; //�޸�DB����
    property CharSet: string read m_sCharSet write SetCharSet; //�����ַ���

    property ClientVer: string read m_sClientVer;
    property ServerVer: string read m_sServerVer;
    property Connected: Boolean read isConnectd;
    property ResultRow: TMysqlRow read m_QueryResult; //�õ���ѯ��ķ��� ���������Զ�����
    property ErrorDesc: AnsiString read m_sErrorDesc;

  end;

procedure SetErrorProc(proc: TMySqlErrorFunction);

implementation

var
  g_ProcError: TMySqlErrorFunction; //�����������
  g_arrFieldData: array of TMYSQL_FIELD; //�洢Mysql ���ֶ����Ե�����

procedure SetErrorProc(proc: TMySqlErrorFunction);
begin
  g_ProcError := proc;
end;

function TMySql.Connect(host, user, passwd, db: AnsiString;
  port: LongInt): Boolean;
var
  PDBName: PAnsiChar;
  ConFlag: LongWord;
begin
  Result := false;
  Close(); //�ȼ���Ƿ���Ҫ�ر�����

  m_Mysql := mysql_init(nil);

  if m_Mysql = nil then
  begin
    RaiseError('Can not Init The MysqlData,Maybe The Memeory is Out of Use!');
    Exit;
  end;

  if db <> '' then
  begin
    PDBName := PAnsiChar(db);
  end else
  begin
    PDBName := nil;
  end;

  //���flag ����Ϊ 0 �Ļ� ִ�� �洢�����ǲ��᷵�ؽ������
  ConFlag := CLIENT_FOUND_ROWS or CLIENT_MULTI_RESULTS;

  if mysql_real_connect(m_Mysql, PAnsiChar(host), PAnsiChar(user), PAnsiChar(passwd), PDBName, port, nil, ConFlag) = nil then
  begin
    RaiseError(0);
    Exit;
  end;

  Result := True;
  m_sDataBase := db;
  m_sServerVer := mysql_get_server_info(m_Mysql);
  m_sCharSet := mysql_character_set_name(m_Mysql);

end;

constructor TMySql.Create;
begin
  //���ض�̬���Լ���ÿͻ��˰汾��Ϣ
  try
    m_sClientVer := mysql_get_client_info;
    m_sDataBase := '';
  except
    on E: Exception do
    begin
      if Assigned(g_ProcError) then
      begin
        g_ProcError('TMySql.Create ' + E.Message, 0);
      end;
    end;
  end;

 // m_QueryResult := TMysqlRow.Create; 

  m_QueryResult := TMysqlRow(TMysqlRow.NewInstance()); //�������� ���ǲ�ִ��Create ���� ȥ��ʼ����
  with m_QueryResult do
  begin
    m_FieldIndexList := TStringList.Create;
    m_FieldIndexList.CaseSensitive := False;
    m_FieldIndexList.Sorted := True;
  end;

end;


function TMySql.isConnectd: Boolean;
begin
  Result := True;
  if m_Mysql = nil then
  begin
    Result := False;
    Exit;
  end;

  if mysql_ping(m_Mysql) <> 0 then
  begin
    Result := False;
    Exit;
  end;

end;

function TMySql.OnAfterExec(nError: integer): Integer;
begin
  m_QueryResult.SetEmpty();

  if nError = 0 then
  begin
    Result := mysql_affected_rows(m_Mysql);
  end else
  begin
    Result := 0;
  end;
end;

procedure TMySql.OnAfterQuery(nError: integer);
begin
  if nError = 0 then
  begin
    //��ý��
    m_QueryResult.m_pMysqlRes := mysql_store_result(m_Mysql);
    if not m_QueryResult.FetchResRow() then
    begin
      m_QueryResult.SetEmpty();
      RaiseError(nError);
    end;

  end else
  begin
    m_MysqlRes := nil;
    m_QueryResult.SetEmpty();
  end;
end;

function TMySql.Query(SqlText: AnsiString): Boolean;
var
  nCount: Integer;
begin
  result := RealQuery(PAnsiChar(SqlText), true, nCount) = 0;
end;

procedure TMySql.RaiseError(nError: Integer; nType: Integer = 0);
var
  sError: Ansistring;
begin
  sError := mysql_error(m_Mysql);
  RaiseError(sError);
end;

procedure TMySql.RaiseError(sDesc: AnsiString; nType: Integer = 0);
begin
  m_sErrorDesc := sDesc;
  if Assigned(g_ProcError) then
  begin
    g_ProcError(sDesc, nType)
  end;
end;

//ִ�� boNeedResult �Ƿ���Ҫ���ؽ���� ����� select ����Ҫ  �����Updata ������ֻ��Ҫ�������õ�����

function TMySql.RealQuery(SqlText: PAnsiChar; boNeedResult: Boolean; var nAffectRow: Integer): Integer;
begin

  Result := mysql_real_query(m_Mysql, SqlText, Length(SqlText));

  if Result <> 0 then
  begin
    m_sQueryErrorText := SqlText;
    RaiseError(Result, 1);
  end;

  if boNeedResult then
  begin
    OnAfterQuery(Result);
  end else
  begin
    OnAfterExec(Result);
  end;

end;

procedure TMySql.ResetQuery;
begin
  if m_Mysql <> nil then
  begin
    m_QueryResult.CleanUp();
     //��ն� DB �� �����ѯ������Է���һ ������ʵ�ʴ�������� һ��Query һ�� Reset
    while (mysql_next_result(m_Mysql) = 0) do
    begin

    end;
  end;

  m_QueryResult.SetEmpty();

end;

procedure TMySql.SetCharSet(const Value: Ansistring);
var
  nError: Integer;
begin
  nError := mysql_set_character_set(m_Mysql, PAnsiChar(Value));
  if nError = 0 then
  begin
    m_sCharSet := Value;
  end else
  begin
    RaiseError(nError);
  end;
end;

procedure TMySql.Close;
begin

  ResetQuery();
  if m_Mysql <> nil then
  begin
    mysql_close(m_Mysql);
    m_Mysql := nil;
  end;

end;

function TMySql.SetDataBase(const Value: Ansistring): Boolean;
var
  nError: Integer;
begin
  nError := mysql_select_db(m_Mysql, PAnsiChar(Value));
  if nError = 0 then
  begin
    m_sDataBase := Value;
    Result := True;
  end else
  begin
    RaiseError(nError);
    Result := False;
  end;
end;


function TMySql.GetAllDataBase(var DataBaseList: TStrings): Boolean;
var
  i: Integer;
begin

  Result := False;
  if not Assigned(DataBaseList) then
  begin
    RaiseError('TMySql.GetAllDataBase , DataBaseList = nil');
    Exit;
  end;

  m_QueryResult.m_pMysqlRes := mysql_list_dbs(m_Mysql, nil);

  if m_QueryResult.m_pMysqlRes = nil then
  begin
    RaiseError('TMySql.GetAllDataBase , mysql_list_dbs , Fail!');
    Exit;
  end;

  if m_QueryResult.FetchResRow() then
  begin
    for I := 0 to m_QueryResult.Count - 1 do
    begin
      DataBaseList.Add(m_QueryResult.Field(0));
      m_QueryResult.Next;
    end;

    ResetQuery();
  end;



  result := True;
end;

function TMySql.GetAllTable(const DBName: AnsiString;
  var DataBaseList: TStrings): Boolean;
var
  I, nError: Integer;

begin
  Result := False;
  nError := mysql_select_db(m_Mysql, PAnsiChar(DBName));

  if nError <> 0 then
  begin
    RaiseError(nError);
    Exit;
  end;

  m_QueryResult.m_pMysqlRes := mysql_list_tables(m_Mysql, nil);

  if m_QueryResult.m_pMysqlRes = nil then
  begin
    RaiseError('TMySql.GetAllTable mysql_list_tables, Fail!');
    Exit;
  end else
  begin
    if m_QueryResult.FetchResRow then
    begin
      for i := 0 to m_QueryResult.Count - 1 do
      begin
        DataBaseList.Add(m_QueryResult.Field(0));
        m_QueryResult.Next();
      end;
    end;

    ResetQuery();
  end;

  Result := True;
end;



destructor TMySql.Destroy;
begin
  if Assigned(m_QueryResult) then
  begin
    Close(); //�ر�����
    m_QueryResult.CleanUp(); //�����Դ
    m_QueryResult.Destroy();
  end;
  inherited;
end;

function TMySql.Exec(SqlText: AnsiString): Integer;
begin
  RealQuery(PAnsiChar(SqlText), false, Result);
end;

{ TMysqlRow }

procedure TMysqlRow.CleanUp;
begin
  if m_pMysqlRes <> nil then
  begin
    mysql_free_result(m_pMysqlRes);
  end;
end;

constructor TMysqlRow.Create;
begin
  raise Exception.Create(' Do not create TMysqlRow by your self! TMysql will manage it');
end;

destructor TMysqlRow.Destroy;
begin
  if Assigned(m_FieldIndexList) then
  begin
    m_FieldIndexList.Free;
  end;
  inherited;
end;

function TMysqlRow.EOF: Boolean;
begin
  result := m_nRowIndex >= m_RowCount;
end;

procedure TMysqlRow.FieldNameList;
var
  pField: PMYSQL_FIELD;
  Field: TMYSQL_FIELD;
  i: Integer;
begin

  m_FieldIndexList.Clear;

  if (m_pMysqlRes <> nil) and (m_FieldCount > 0) then
  begin
    for I := 0 to m_FieldCount - 1 do
    begin
      pField := mysql_fetch_field(m_pMysqlRes);
      Field := UpdateField(pField);
      m_FieldIndexList.AddObject(Field.name, TObject(I));
    end;

  end;
end;

procedure TMysqlRow.FieldToBuffer(nFieldIndex: Integer; pPtr: Pointer;
  nLength: Integer);
var
  P: PAnsiChar;
begin
  P := FieldAsPointer(nFieldIndex);
  if P <> nil then
  begin
    Move(P^, pPtr^, nLength);
  end;
end;

procedure TMysqlRow.First;
begin
  //��� ���� �� ���� ��Ϊ 0 �����Ҫ���� seek
  if ((m_nRowIndex <> 0) or (m_nFieldIndex <> 0))
    and (m_pRows <> nil)
    and (m_pMysqlRes <> nil) then
  begin
    mysql_data_seek(m_pMysqlRes, 0);
    mysql_field_seek(m_pMysqlRes, 0);
    m_pRows := mysql_fetch_row(m_pMysqlRes);
  end;
end;

destructor TMysqlRow.free;
begin
  raise Exception.Create('do not Free TMysqlRow , TMysql will  auto manage this!');
end;

procedure TMysqlRow.Next;
begin
  if m_pMysqlRes <> nil then
  begin
    m_pRows := mysql_fetch_row(m_pMysqlRes);
    Inc(m_nRowIndex);
  end;
end;


function TMysqlRow.FieldAsPointer(nFieldIndex: Integer): PAnsiChar;
begin
  if (nFieldIndex < m_FieldCount) and (nFieldIndex >= 0) then
  begin
    Result := m_pRows[nFieldIndex];
  end else
  begin
    Result := nil;
  end;
end;

function TMysqlRow.Field(nFieldIndex: Integer): AnsiString;
var
  P: PAnsiChar;
begin
  P := FieldAsPointer(nFieldIndex);

  if P <> nil then
  begin
    Result := AnsiString(P);
  end else
  begin
    Result := '';
  end;

end;

function TMysqlRow.FieldAsInteger(nFieldIndex: Integer): Integer;
begin
  Result := StrToIntDef(Field(nFieldIndex), 0);
end;



function TMysqlRow.FieldByName(sFieldName: AnsiString): AnsiString;
var
  nIndex: Integer;
begin

  nIndex := FieldIndex(sFieldName);

  if nIndex >= 0 then
  begin
    nIndex := Integer(m_FieldIndexList.Objects[nIndex]);
    Result := Field(nIndex);
  end else
  begin
    Result := '';
  end;

end;

function TMysqlRow.FieldByNameAsInteger(sFieldName: AnsiString): Integer;
begin
  Result := StrToIntDef(FieldByName(sFieldName), 0);
end;

procedure TMysqlRow.FieldByNameToBuffer(sFieldName: AnsiString; pPtr: Pointer;
  nLength: Integer);
var
  nIndex: Integer;
begin
  nIndex := FieldIndex(sFieldName);

  if nIndex >= 0 then
  begin
    FieldToBuffer(nIndex, pPtr, nLength);
  end;

end;

function TMysqlRow.FieldIndex(sName: AnsiString): Integer;
begin
  if m_FieldIndexList.Count <> m_FieldCount then
  begin
    FieldNameList();
  end;
  Result := m_FieldIndexList.IndexOf(sName);
end;

function TMysqlRow.FetchResRow: Boolean;
begin
  Result := False;

  if m_pMysqlRes <> nil then
  begin
    //�ƶ��α� �� 0 0
    mysql_data_seek(m_pMysqlRes, 0);
    mysql_field_seek(m_pMysqlRes, 0);

    //��¼����
    m_FieldCount := mysql_num_fields(m_pMysqlRes);
    m_RowCount := mysql_num_rows(m_pMysqlRes);

    //ת��
    m_pRows := mysql_fetch_row(m_pMysqlRes);

    if m_pRows <> nil then
      Result := True;
  end;
end;

procedure TMysqlRow.SetEmpty;
begin
  m_FieldCount := 0;
  m_RowCount := 0;
  m_pRows := nil;
  m_pMysqlRes := nil;
end;


initialization
  begin
    libmysql_fast_load(nil);
  end;
end.

