unit MysqlOP;

interface
uses System.SysUtils;
type
  TMysqlOP = (opNone,UpdateKey,AsJson,PrimaryKey,Unikey,IndexHash,IndexBTree);
  TMysqlOPSets = set of TMysqlOP;

  TFieldCodeOp = (fcOpNone,fcOpIntToStr);
function GetDelphiTypeOP(const DelphiOPType:String):TFieldCodeOp;
function DelphiTypeToMysql(const DelphiType:String):string;
function MySqlTypeToMysql(const MysqlType:String):String;
function GetMysqlOP(const Str:String ):TMysqlOP;
implementation
uses System.Generics.Collections , DoubleDict;
var
  DelphiToMysqlMap : TDoubleDict<String,String>;
  MySqlOPMap : TDictionary<string,TMysqlOP>;
  FieldCodeOpMap : TDictionary<string,TFieldCodeOp>;

function DelphiTypeToMysql(const DelphiType:String):string;
begin
  Result := '';
  DelphiToMysqlMap.TryGetValue(LowerCase(DelphiType),Result);
end;

function MySqlTypeToMysql(const MysqlType:String):String;
begin
  Result := '';
  DelphiToMysqlMap.TryGetKeyByValue(LowerCase(MysqlType),Result);
end;

function GetDelphiTypeOP(const DelphiOPType:String):TFieldCodeOp;
begin
  Result := fcOpNone;
  FieldCodeOpMap.TryGetValue(LowerCase(DelphiOPType),Result);
end;

procedure InitMap();
begin
  //Mysql 类型对照表
  DelphiToMysqlMap.Add('integer','int');
  DelphiToMysqlMap.Add('int64','bigint');
  DelphiToMysqlMap.Add('string','text');
  DelphiToMysqlMap.Add('byte','tinyint');
  DelphiToMysqlMap.Add('smallint','smallint');

  FieldCodeOpMap.Add('integer',fcOpIntToStr);
  FieldCodeOpMap.Add('int64',fcOpIntToStr);
  FieldCodeOpMap.Add('string',fcOpNone);
  FieldCodeOpMap.Add('byte',fcOpIntToStr);
  FieldCodeOpMap.Add('smallint',fcOpIntToStr);

    //操作控制符定义
  MySqlOPMap.Add('json',AsJson);  //该字段执行序列号到json 再以字符串存储
  MySqlOPMap.Add('pkey',PrimaryKey);
  MySqlOPMap.Add('ukey',Unikey);
  MySqlOPMap.Add('updatekey',UpdateKey);
  MySqlOPMap.Add('hash',IndexHash);
  MySqlOPMap.Add('btree',IndexBTree);
end;



function GetMysqlOP(const Str:String ):TMysqlOP;
begin
  if not MySqlOPMap.TryGetValue(Str,Result) then
  begin
    Result := opNone;
  end;
end;

initialization
  DelphiToMysqlMap := TDoubleDict<String,String>.Create();
  MySqlOPMap := TDictionary<string,TMysqlOP>.Create();
  FieldCodeOpMap := TDictionary<string,TFieldCodeOp>.Create();
  InitMap();
finalization
  FieldCodeOpMap.Free;
  DelphiToMysqlMap.Free;
  MySqlOPMap.Free;

end.
