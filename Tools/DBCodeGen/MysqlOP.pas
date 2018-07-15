unit MysqlOP;

interface
uses System.SysUtils;
type
  TMysqlOP = (mopNone,mopUpdateKey,mopAsJson,mopPrimaryKey,mopUnikey,mopIndexHash,mopIndexBTree,mopJsonClass,mopUID,mopQueryAnd,mopQueryOr,mopQueryAnd1,mopQueryOr1);
  TMysqlOPSets = set of TMysqlOP;

  TFieldCodeOp = (fcOpNone,fcOpIntToStr,fcOpBoolean,fcOpDouble,fcOpToMysqlTime);
function GetDelphiTypeOP(const DelphiOPType:String):TFieldCodeOp;
function DelphiTypeToMysql(const DelphiType:String):string;
//function MySqlTypeToMysql(const MysqlType:String):String;
function GetMysqlOP(const Str:String ):TMysqlOP;
implementation
uses System.Generics.Collections , DoubleDict;
var
  DelphiToMysqlMap : TDictionary<String,String>;
  MySqlOPMap : TDictionary<string,TMysqlOP>;
  FieldCodeOpMap : TDictionary<string,TFieldCodeOp>;

function DelphiTypeToMysql(const DelphiType:String):string;
begin
  Result := '';
  DelphiToMysqlMap.TryGetValue(LowerCase(DelphiType),Result);
end;

{
function MySqlTypeToMysql(const MysqlType:String):String;
begin
  Result := '';
  DelphiToMysqlMap.TryGetKeyByValue(LowerCase(MysqlType),Result);
end;
}

function GetDelphiTypeOP(const DelphiOPType:String):TFieldCodeOp;
begin
  Result := fcOpNone;
  FieldCodeOpMap.TryGetValue(LowerCase(DelphiOPType),Result);
end;

procedure InitMap();
begin
  //Mysql 类型对照表

  DelphiToMysqlMap.Add('byte','tinyint(4) unsigned');
  DelphiToMysqlMap.Add('shortint','tinyint(4)');

  DelphiToMysqlMap.Add('word','smallint(6) unsigned');
  DelphiToMysqlMap.Add('smallint','smallint(6)');

  DelphiToMysqlMap.Add('cardinal','int(11) unsigned');
  DelphiToMysqlMap.Add('integer','int(11)');

  DelphiToMysqlMap.Add('uint64','bigint(20) unsigned');
  DelphiToMysqlMap.Add('int64','bigint(20)');

  DelphiToMysqlMap.Add('tdatetime','datetime');
  DelphiToMysqlMap.Add('string','text');

  DelphiToMysqlMap.Add('boolean','tinyint(1)');
  DelphiToMysqlMap.Add('double','double');

  FieldCodeOpMap.Add('byte',fcOpIntToStr);
  FieldCodeOpMap.Add('shortint',fcOpIntToStr);

  FieldCodeOpMap.Add('word',fcOpIntToStr);
  FieldCodeOpMap.Add('smallint',fcOpIntToStr);

  FieldCodeOpMap.Add('integer',fcOpIntToStr);
  FieldCodeOpMap.Add('cardinal',fcOpIntToStr);

  FieldCodeOpMap.Add('uint64',fcOpIntToStr);
  FieldCodeOpMap.Add('int64',fcOpIntToStr);

  FieldCodeOpMap.Add('tdatetime',fcOpToMysqlTime);
  FieldCodeOpMap.Add('boolean',fcOpBoolean);
  FieldCodeOpMap.Add('double',fcOpDouble);
  FieldCodeOpMap.Add('string',fcOpNone);


    //操作控制符定义
  MySqlOPMap.Add('json',mopAsJson);  //结构体->Json  如果类型获取不到会自动识别为结构体类型。
  MySqlOPMap.Add('class',mopJsonClass); // Object -> json    //如果是 class 需要手动增加参数表示其是一个class
  MySqlOPMap.Add('pkey',mopPrimaryKey);
  MySqlOPMap.Add('ukey',mopUnikey);
  MySqlOPMap.Add('updatekey',mopUpdateKey);
  MySqlOPMap.Add('hash',mopIndexHash);
  MySqlOPMap.Add('btree',mopIndexBTree);
  MySqlOPMap.Add('uid',mopUID);
  MySqlOPMap.Add('queryand',mopQueryAnd);
  MySqlOPMap.Add('queryor',mopQueryOr);
  MySqlOPMap.Add('queryand1',mopQueryAnd1);
  MySqlOPMap.Add('queryor1',mopQueryOr1);

end;

function GetMysqlOP(const Str:String ):TMysqlOP;
begin
  if not MySqlOPMap.TryGetValue(Str,Result) then
  begin
    Result := mopNone;
  end;
end;

initialization
  DelphiToMysqlMap := TDictionary<String,String>.Create();
  MySqlOPMap := TDictionary<string,TMysqlOP>.Create();
  FieldCodeOpMap := TDictionary<string,TFieldCodeOp>.Create();
  InitMap();
finalization
  FieldCodeOpMap.Free;
  DelphiToMysqlMap.Free;
  MySqlOPMap.Free;

end.
