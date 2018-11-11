(**)
(* delphi Mysql 操作简单封装 需要 libmysql.dll *)
(* date : 2015/11/4*)
(* author : 一路随云 qq:531662161)
(* 参考: http://www.cnblogs.com/doorsky/archive/2010/01/05/1639980.html *)


unit CheTek.MySql;

interface

uses
  Windows, Messages, SysUtils, Classes,System.Generics.Collections,CheTek.BaseObject,
  Mysql;
type

  TMysql = class;
  //进行查询后返回的结果集
  TMysqlRow = class
  private

    m_Owner : TMysql;

    m_FieldCount: Integer; //查询记录集的列数

    m_RowCount: Integer; //查询记录集的行数

    m_pMysqlRes: PMYSQL_RES; //返回原始结果集

    m_pRows: PMYSQL_ROW; //Mysql 返回结果集

    m_nRowIndex: Integer; //当前行索引

    m_FieldIndexList: TDictionary<String,Integer>; //字段名称对应的Index 用来实现FieldByName()

    procedure SetEmpty(); //设置为结果集为空

    procedure FieldNameList();

    function FetchResRow(): Boolean; //整理资源

    procedure CleanUp(); //清理资源

    function FieldIndex(sName: String): Integer; //根据字段名 获取字段名的下标

    destructor Destroy; override; //这一行会 报 H2269 的提示 忽略他
  public

    constructor Create; //如果尝试 手动Create 应当立马报错
    destructor free; //如果尝试 手动Free 应该立即报错
    function EOF: Boolean; //当前是否已经到数据集尾部了(无法继续读取)

    procedure First; //移动到行首

    procedure Next; //移动到下一行
    function FieldAsPointer(FieldIndex: Integer; out Ptr:PAnsiChar):Boolean;
    function Field(FieldIndex: Integer ; out Value:String):Boolean;
    function FieldAsInteger(FieldIndex: Integer ;out Value : Int64):Boolean;
    function FieldToBuffer(FieldIndex: Integer; pPtr: Pointer; nLength: Integer):Boolean;
    function FieldByName(const FieldName: String ;out Value:String): Boolean;
    function FieldByNameAsInteger(const FieldName: string ; out Value:Int64):Boolean;overload;
    function FieldByNameAsInteger(const FieldName: string ):Int64;overload;
    function FieldByNameToBuffer(const FieldName: string; pPtr: Pointer; nLength: Integer):Boolean;
    function FieldByNameAsDouble(const FieldName:String):Double;
    function FieldByNameAsBoolean(const FieldName:String):Boolean;
    property Count: Integer read m_RowCount;
    property FieldCount: Integer read m_FieldCount;

  end;

  //发生错误的回调函数 通过 SetErrorProc 绑定一个回调函数 当出现错误可以立马打印出来
  TMySqlErrorFunction = procedure(const Error: String; ErrorCode: Integer);

  TMySql = class(TBaseObject)
  private
    m_Mysql: PMYSQL;
    m_MysqlRes: PMYSQL_RES;
    m_QueryResult: TMysqlRow;
    m_sCharSet: String;
    m_sDataBase: String;
    m_sClientVer: String;
    m_sServerVer: String; //服务端DB使用的版本
    m_sErrorDesc: String; //错误描述
    m_sQueryErrorText: String; //上一次执行 Query 或者 Exec 类的Sql指令
    m_boIsUtf8ChrSet:Boolean;//是否utf8编码

    procedure SetCharSet(const Value: String);
    procedure OnAfterQuery(nError: integer);
    function OnAfterExec(nError: integer): Integer; //返回值 为影响行数
    procedure RaiseError(nError: Integer; nType: Integer = 0); overload;
    procedure RaiseError(sDesc: string; nType: Integer = 0); overload;
    function isConnectd(): Boolean;
    function RealQuery(SqlText: String; boNeedResult: Boolean; var nAffectRow: Integer): Integer; //执行查询
  protected

  public
    constructor Create();
    destructor Destroy(); override;

    //获取所有的数据库

    function GetAllDataBase(var DataBaseList: TStrings): Boolean;

    //获取某个DB的所有表
    function GetAllTable(const DBName: string; var DataBaseList: TStrings): Boolean;

    //连接服务器
    function Connect(const Host:String; Port :Integer; const UserName, Password : String; const DataBase: String = ''):Boolean;

    function GetCreateTableSql(const TableName :String ; var Str:TStrings):Boolean;

    procedure Close(); //关闭与 Mysql 的连接

    //执行Query SQL语句
    function Query(const SqlText: string): TMysqlRow; //返回是否执行成功 为nil 则失败

    //执行 SQL语句
    function Exec(const SqlText: string ; out EffectRows:Integer ): Integer; //返回作用的行数

   (* Query 和 Exec 的区别在于 一个需要 返回结果集 一个不需要返回结果集 *)
   (* 但是同样的 当查询完毕 都需要执行 ResetQuery 释放资源 *)

    procedure ResetQuery(); //重置查询释放资源

    procedure OutLog(ErrorCode:Integer;const Desc:String);

    function SetDataBase(const Value: string): Boolean; //修改DB连接
    property CharSet: string read m_sCharSet write SetCharSet; //设置字符集
    property ClientVer: string read m_sClientVer;
    property ServerVer: string read m_sServerVer;
    property Connected: Boolean read isConnectd;
    property ResultRow: TMysqlRow read m_QueryResult; //得到查询后的返回 这个对象会自动管理
    property ErrorDesc: string read m_sErrorDesc;

  private
    class var ErrorProc : TMySqlErrorFunction;
   public
    class procedure SetErrorProc(proc: TMySqlErrorFunction);
    class Function Quote(const Str:String):String;
  end;



implementation

//g_arrFieldData: array of TMYSQL_FIELD; //存储Mysql 的字段属性的数组

class procedure TMySql.SetErrorProc(proc: TMySqlErrorFunction);
begin
  ErrorProc := proc;
end;

function TMySql.GetCreateTableSql(const TableName :String; var Str: TStrings):Boolean;
var
  Sql ,TableText : String;
begin
  Result := False;
  Sql := Format('SHOW CREATE TABLE `%s`',[TableName]);
  if Query(Sql) <> nil then
  begin
    if ResultRow.Count > 0 then
    begin
      ResultRow.First();
      if ResultRow.FieldByName('Create Table',TableText) then
      begin
        Str.Text := TableText;
      end;
      Result := True;
    end;
  end;
end;

function _String(const PStr:PAnsiChar):String;
begin
  Result := string(AnsiString(PStr));
end;

function TMySql.Connect(const Host:String; Port :Integer; const UserName, Password, DataBase: String): Boolean;
var
  PDBName: PAnsiChar;
  ConFlag: LongWord;
begin
  Result := false;
  Close(); //先检查是否需要关闭连接

  m_Mysql := mysql_init(nil);

  if m_Mysql = nil then
  begin
    RaiseError('Can not Init The MysqlData,Maybe The Memeory is Out of Use!');
    Exit;
  end;

  if DataBase <> '' then
  begin
    PDBName := PAnsiChar(AnsiString(DataBase));
  end else
  begin
    PDBName := nil;
  end;

  //如果flag 设置为 0 的话 执行 存储过程是不会返回结果集的
  ConFlag := CLIENT_FOUND_ROWS or CLIENT_MULTI_RESULTS;

 if mysql_real_connect(m_Mysql, PAnsiChar(AnsiString(host)), PAnsiChar(AnsiString(UserName)), PAnsiChar(AnsiString(Password)), PDBName, port, nil, ConFlag) = nil then
  begin
    RaiseError(0);
    Exit;
  end;

  Result := True;
  m_sDataBase := DataBase;
  m_sServerVer := _String((mysql_get_server_info(m_Mysql)));
  m_sCharSet := _String(mysql_character_set_name(m_Mysql));

end;

constructor TMySql.Create;
begin
  //加载动态库以及获得客户端版本信息
  try
    m_sClientVer := _String(mysql_get_client_info);
    m_sDataBase := '';
  except
    on E: Exception do
    begin
        OutLog(0, 'TMySql.Create ' + E.Message);
    end;
  end;

 // m_QueryResult := TMysqlRow.Create; 

  m_QueryResult := TMysqlRow(TMysqlRow.NewInstance()); //创建对象 但是不执行Create 函数 去初始对象
  with m_QueryResult do
  begin
    m_QueryResult.m_Owner := Self;
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
    //获得结果
    m_QueryResult.m_pMysqlRes := mysql_store_result(m_Mysql);
    if not m_QueryResult.FetchResRow() then
    begin
      m_QueryResult.SetEmpty();
    end;
  end else
  begin
    m_MysqlRes := nil;
    m_QueryResult.SetEmpty();
    RaiseError(nError);
  end;
end;

procedure TMySql.OutLog(ErrorCode: Integer; const Desc: String);
begin
  if Assigned(ErrorProc) then
  begin
    ErrorProc(Desc, ErrorCode)
  end;
end;

function TMySql.Query(const SqlText: string): TMysqlRow;
var
  nCount: Integer;
begin
   if RealQuery(SqlText, true, nCount) = 0 then
   begin
      Result := m_QueryResult;
   end else
   begin
     Result := nil;
   end;
end;

class function TMySql.Quote(const Str: String): String;
begin
  Result := '''' + Str + '''';
end;

procedure TMySql.RaiseError(nError: Integer; nType: Integer = 0);
var
  sError: String;
begin
  sError := UTF8ToString(mysql_error(m_Mysql));
  RaiseError(sError);
end;

procedure TMySql.RaiseError(sDesc: string; nType: Integer = 0);
begin
  m_sErrorDesc := sDesc;
  if Assigned(ErrorProc) then
  begin
    ErrorProc(sDesc, nType)
  end;
end;


//执行 boNeedResult 是否需要返回结果集 如果是 select 则需要  如果是Updata 这种则只需要返回作用的行数
function TMySql.RealQuery(SqlText: String; boNeedResult: Boolean; var nAffectRow: Integer): Integer;
var
  Utf8:AnsiString;
begin

  if m_boIsUtf8ChrSet then
  begin
    Utf8 := UTF8Encode(SqlText);
    Result := mysql_real_query(m_Mysql, PAnsiChar(Utf8), Length(Utf8));
  end else
  begin
    Result := mysql_real_query(m_Mysql, PAnsiChar(AnsiString(SqlText)), Length(SqlText));
  end;

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
    nAffectRow := OnAfterExec(Result);
  end;

end;

procedure TMySql.ResetQuery;
begin
  if m_Mysql <> nil then
  begin
    m_QueryResult.CleanUp();
     //清空对 DB 的 多个查询结果。以防万一 但是在实际代码中最好 一个Query 一个 Reset
    while (mysql_next_result(m_Mysql) = 0) do
    begin

    end;
  end;

  m_QueryResult.SetEmpty();

end;

procedure TMySql.SetCharSet(const Value: string);
var
  nError: Integer;
begin
  nError := mysql_set_character_set(m_Mysql, PAnsiChar(AnsiString(Value)));
  if nError = 0 then
  begin
    m_sCharSet := Value;
    if LowerCase(value) = 'utf8' then
    begin
      m_boIsUtf8ChrSet := True;
    end else
    begin
      m_boIsUtf8ChrSet := False;
    end;
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

function TMySql.SetDataBase(const Value: string): Boolean;
var
  nError: Integer;
begin
  nError := mysql_select_db(m_Mysql, PAnsiChar(AnsiString(Value)));
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
  DataBaseName:String;
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
      if m_QueryResult.Field(0,DataBaseName) then
      begin
        DataBaseList.Add(DataBaseName);
      end;
      m_QueryResult.Next;
    end;

    ResetQuery();
  end;

  result := True;
end;

function TMySql.GetAllTable(const DBName: string;
  var DataBaseList: TStrings): Boolean;
var
  I, nError: Integer;
  TableName:string;
begin
  Result := False;
  nError := mysql_select_db(m_Mysql, PAnsiChar(AnsiString((DBName))));

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

        if m_QueryResult.Field(0,TableName) then
        begin
          DataBaseList.Add(TableName);
        end;

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
    Close(); //关闭连接
    m_QueryResult.CleanUp(); //清空资源
    m_QueryResult.Destroy();
  end;
  inherited;
end;

function TMySql.Exec(const SqlText: string ; out EffectRows:Integer): Integer;
begin
  Result := RealQuery(SqlText, false, EffectRows);
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

  if m_FieldIndexList = nil then
  begin
    m_FieldIndexList := TDictionary<String,Integer>.Create;
  end;

  m_FieldIndexList.Clear;

  if (m_pMysqlRes <> nil) and (m_FieldCount > 0) then
  begin
    for I := 0 to m_FieldCount - 1 do
    begin
      pField := mysql_fetch_field(m_pMysqlRes);
      Field := UpdateField(pField);
      m_FieldIndexList.AddOrSetValue(_string(Field.name),i);
    end;
  end;
end;

function TMysqlRow.FieldToBuffer(FieldIndex: Integer; pPtr: Pointer; nLength: Integer):Boolean;
var
  P: PAnsiChar;
begin
  Result := False;
  if FieldAsPointer(FieldIndex,P) then
  begin
    Move(P^, pPtr^, nLength);
    Result := True;
  end;
end;

procedure TMysqlRow.First;
begin
  //如果 行数 和 列数 不为 0 则才需要重新 seek
  if ((m_nRowIndex <> 0))
    and (m_pRows <> nil)
    and (m_pMysqlRes <> nil) then
  begin
    mysql_data_seek(m_pMysqlRes, 0);
    mysql_field_seek(m_pMysqlRes, 0);
    m_pRows := mysql_fetch_row(m_pMysqlRes);
    m_nRowIndex := 0;
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


function TMysqlRow.FieldAsPointer(FieldIndex: Integer;out Ptr:PAnsiChar):Boolean;
begin
  if (FieldIndex < m_FieldCount) and (FieldIndex >= 0) then
  begin
    Ptr := m_pRows[FieldIndex];
    Result := True;
  end else
  begin
    Ptr := nil;
    Result:= False;
  end;
end;

function TMysqlRow.Field(FieldIndex: Integer ; Out Value:String): Boolean;
var
  P: PAnsiChar;
begin

  if FieldAsPointer(FieldIndex,P) then
  begin
    if m_Owner.m_boIsUtf8ChrSet then
      Value := UTF8ToString(AnsiString(P))
    else
      Value := _String(P);

    Result := True;
  end else
  begin
    Result := False;
  end;

end;

function TMysqlRow.FieldAsInteger(FieldIndex: Integer; out Value:Int64): Boolean;
var
  Str : String;
begin
  if Field(FieldIndex , Str) then
  begin
    Value := StrToInt64Def(Str,0);
    Result := True;
  end else
  begin
    Result := False;
  end;
end;



function TMysqlRow.FieldByName(const FieldName: String ; out Value:String): Boolean;
var
  Index: Integer;
begin
  Result := False;
  if m_FieldIndexList.TryGetValue(FieldName,Index) then
  begin
     if Field(Index , Value) then
     begin
        Result := True;
     end;
  end else
  begin
    Result := False;
  end;

end;


function TMysqlRow.FieldByNameAsInteger(const FieldName: string): Int64;
begin
  Result := 0;
  FieldByNameAsInteger(FieldName,Result);
end;

function TMysqlRow.FieldByNameAsInteger(const FieldName: string ; Out Value:Int64):Boolean;
var
  Str:String;
begin
  Result := False;
  if FieldByName(FieldName,Str) then
  begin
    Value := StrToInt64Def(Str,0);
    Result := True;
  end;
end;

function TMysqlRow.FieldByNameAsBoolean(const FieldName: String): Boolean;
var
  Str : String;
begin
  if FieldByName(FieldName,Str) then
  begin
    Result  := StrToBool(Str);
    Result := True;
  end else begin
    Result := False;
  end;
end;

function TMysqlRow.FieldByNameAsDouble(const FieldName: String): Double;
var
  Str : String;
begin
  if FieldByName(FieldName,Str) then
  begin
    Result := StrToFloatDef(Str,0);
  end else
  begin
    Result := 0;
  end;
end;

function TMysqlRow.FieldByNameToBuffer(const FieldName: string; pPtr: Pointer;
  nLength: Integer):Boolean;
var
  nIndex: Integer;
begin
  Result := False;
  nIndex := FieldIndex(FieldName);
  if nIndex >= 0 then
  begin
    if FieldToBuffer(nIndex, pPtr, nLength) then
    begin
      Result := True;
    end;
  end;
end;

function TMysqlRow.FieldIndex(sName: String): Integer;
begin
  Result := -1;
  if m_FieldIndexList.Count <> m_FieldCount then
  begin
    FieldNameList();
  end;

  m_FieldIndexList.TryGetValue(sName,Result);
end;

function TMysqlRow.FetchResRow: Boolean;
begin
  Result := False;

  if m_pMysqlRes <> nil then
  begin
    //移动游标 到 0 0
    mysql_data_seek(m_pMysqlRes, 0);
    mysql_field_seek(m_pMysqlRes, 0);
    m_nRowIndex := 0;

    //记录数量
    m_FieldCount := mysql_num_fields(m_pMysqlRes);
    m_RowCount := mysql_num_rows(m_pMysqlRes);

    //转换
    m_pRows := mysql_fetch_row(m_pMysqlRes);

    if m_pRows <> nil then
      Result := True;
  end;

  FieldNameList();
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

