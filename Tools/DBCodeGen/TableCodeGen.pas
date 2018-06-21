unit TableCodeGen;

interface
uses MysqlOP,System.Generics.Collections,System.SysUtils,System.Classes,JclStrings,System.IOUtils;



type


    TFieldData = class
    private
      FFieldName:String;
      FDelphiType:String;
      FMysqlOps : TMysqlOPSets;
      FLength:Integer; //字段长度 对于 Text 做主键的话必须指定长度 这样的话就会变成varchar
      FComment:String;//注释
      FQueryAndGroup:Integer; //生成查询分组ID And 条件
      FQueryOrGroup:Integer; //生成查询分组ID Query 条件
    public
      function GetDelphiFiledString():String;
      function IsNeedQuote():Boolean; //需不需要用单引号 引起来
      function CodeOfValueToString():String;
      function IsUIDField():Boolean;
      function GetUPDATEFieldCodeText():string;
      function GetMysqlCloumnDesc():String;
    end;

    TQueryGroup = Class
      GroupID:Integer;
      GroupFields:TList<TFieldData>;
      QueryType:TMysqlOP;
       constructor Create();
       destructor Destroy();override;
    end;

    TDBTable = class
    private
      FTableName:String;
      FFields : TObjectList<TFieldData>;
      QueryGroups : TObjectList<TQueryGroup>;
      function GetClassName():String;
      function GenInsertProcCodeText():String;
      function GenUpdateProcCodeText():String;
      function GenCreateTableSqlText():String;
      function GetFeatchProcCodeText(): String;
      procedure GetQueryFuncNameDefine(List:TStrings); //添加查询函数声明
      procedure GetQueryFuncCode(List:TStrings); //添加查询函数的代码
    public
      constructor Create(const TableName:String);
      procedure AddField(const FieldName,DelphiType,ControlString:string);overload;
      function GetCreateTableSql():TStringList;
      Procedure SaveToDir(Const Path:String);
      destructor Destroy;override;
    end;



implementation

const LowPreFix = 1;
const HighPreFix = 4;
const LowQuote = 1;
const HighQute = 4;

const  WidthPreFix : Array[LowPreFix..HighPreFix] of string = (' ', '  ', '   ', '    ');
const  Quote: array[LowQuote..HighQute] of String = (#39,#39#39,#39#39#39,#39#39#39#39) ;
var Templelate:TStringList;

function _WD(Width:Integer):string;
begin
   Result := '';
   if (Width >= LowPreFix) and (Width <= HighPreFix) then
   begin
     Result := WidthPreFix[Width];
   end;
end;

function _Q(Count : Integer):String;
begin
   Result := '';
   if (Count >= LowQuote) and (Count <= HighQute) then
   begin
     Result := Quote[Count];
   end;
end;

{ TDBTable }

procedure TDBTable.AddField(const FieldName, DelphiType, ControlString: string);
var
  ControlList:TStringList;
  CText:String;
  I:Integer;
  MysqlOps: TMysqlOPSets;
  OP:TMysqlOP;
  FieldData : TFieldData;
  OpStr,OpType,Comment:string;
begin
  CText := StrBetween(ControlString,'[',']');
  ControlList := TStringList.Create;
  ControlList.Delimiter := ',';
  ControlList.DelimitedText  := CText;
  Comment := Trim(StrAfter('comment',ControlString));
  if Comment.Length > 0 then
  begin
    if Comment[1] = ':' then
      Comment := Trim(Copy(Comment,2,Comment.Length - 1));
  end;

  MysqlOps := [];
  for i := 0 to ControlList.Count - 1 do
  begin

    FieldData := TFieldData.Create;
    With FieldData do
    begin
      FFieldName := FieldName;
      FDelphiType := DelphiType;
      if Comment.Length > 0 then
        FComment := _Q(2) + Comment + _Q(2);
    end;


    OpStr := Trim(ControlList[i]);
    OpType := Trim(StrBefore('(',OpStr));
    OP := GetMysqlOP(OpType);

    //如果String 类型的作为了 主键 那么 必须指定长度 否则 将无法创表 这个是mysql的限制
    if (OP = mopPrimaryKey) and (LowerCase(DelphiType) = 'string') then
    begin
      FieldData.FLength := StrToIntDef(StrBetween(OpStr,'(',')'),-1);
    end;

    if OP = mopQueryAnd then
    begin
      FieldData.FQueryAndGroup := StrToIntDef(StrBetween(OpStr,'(',')'),0);
    end else if OP = mopQueryOr then
    begin
      FieldData.FQueryOrGroup := StrToIntDef(StrBetween(OpStr,'(',')'),0);
    end;

    FieldData.FMysqlOps := FieldData.FMysqlOps + [OP];
    FFields.Add(FieldData);
  end;
  ControlList.Free;


end;

constructor TDBTable.Create(const TableName: String);
begin
  FTableName := TableName;
  FFields := TObjectList<TFieldData>.Create;
  QueryGroups := TObjectList<TQueryGroup>.Create;
end;

destructor TDBTable.Destroy;
begin
  FFields.Free;
  QueryGroups.Free;
  inherited;
end;

function TDBTable.GenCreateTableSqlText: String;
var
  SourceText:TStringList;
  CreateTableText:TStringList;
  Str:String;
begin
  SourceText := TStringList.Create;
  SourceText.Add('begin');
  SourceText.Add(_WD(2) + 'Result := TStringList.Create;');

  CreateTableText := Self.GetCreateTableSql;
  for Str in CreateTableText  do
  begin
    SourceText.Add((_WD(2) + 'Result.Add(' + _Q(1) + Str + _Q(1) + ');'));
  end;
  CreateTableText.Free;
  SourceText.Add('end;');
  Result := SourceText.Text;
  SourceText.Free;
end;

function TDBTable.GenInsertProcCodeText: String;
var
  Fileds,Values,FormatControl,InsertSql:String;
  FieldData : TFieldData;
  IsFirst : Boolean;
  SourceText:TStringList;
  I:Integer;
begin
  SourceText := TStringList.Create;
  SourceText.Add('var');
  SourceText.Add(_WD(2) + 'Sql:String;');
  SourceText.Add(_WD(2) + Format('V : Array[1..%d] of String;',[FFields.Count]));
  SourceText.Add('begin');

  IsFirst := True;
  I := 1;
  for FieldData in FFields do
  begin
    if IsFirst then
    begin
      Fileds := Fileds +  FieldData.FFieldName ;
      if FieldData.IsNeedQuote() then
         Values := Values + '''%s'''
      else
         Values := Values + '%s';
    end
    else
    begin
      Fileds := Fileds + ',' + FieldData.FFieldName ;
      if FieldData.IsNeedQuote() then
         Values := Values + ''',%s'''
      else
         Values := Values + ',%s';
    end;

    SourceText.Add(_WD(2) +  Format('V[%d] := %s;',[i,FieldData.CodeOfValueToString()]));

    if IsFirst then
       FormatControl := FormatControl + Format( ' V[%d]',[i])
    else
       FormatControl := FormatControl + Format( ', V[%d]',[i]);

    IsFirst := False;
    I := I + 1;
  end;

  InsertSql := Format('INSERT INTO %s (%s) VALUES (%s);',[FTableName,Fileds,Values]);


  SourceText.Add(_WD(2) + Format('Sql := format(''%s'',[%s]);',[InsertSql,FormatControl]));
  SourceText.Add('end;');
  Result :=  SourceText.Text;
  SourceText.Free;
end;

function TDBTable.GenUpdateProcCodeText: String;
var
  FieldData : TFieldData;
  UIDField : TFieldData;
  IsFirst : Boolean;
  SourceText:TStringList;
  FormatText:String;
begin
  SourceText := TStringList.Create;
  SourceText.Add('var');
  SourceText.Add(_WD(2) + 'Sql:String;');
  SourceText.Add('begin');

  IsFirst := True;
  UIDField := nil;

  for FieldData in FFields do
  begin

    if FieldData.IsUIDField() then
    begin
      if UIDField = nil then
      begin
        UIDField := FieldData;
      end else
      begin
        raise Exception.Create(FTableName + ' : UID Field not only one!');
      end;
    end;

    if IsFirst then
    begin
        SourceText.Add(_WD(2) + 'Sql := ' + FieldData.GetUPDATEFieldCodeText() + ';' );
        IsFirst := False;
    end
    else
    begin
       SourceText.Add(_WD(2) + 'Sql := Sql + ' + ''', ''' + '+ '  + FieldData.GetUPDATEFieldCodeText() + ';' );
    end;
  end;

  if UIDField = nil then
  begin
    raise Exception.Create(FTableName + ' : UID Field not find!');
  end;

  FormatText := _WD(2) + 'Sql := Format(' + _Q(1) + 'UPDATE %s SET %s WHERE %s = %s ;' + _Q(1) + ', [' + _Q(1) +  FTableName + _Q(1) + ', Sql ,' + _Q(1) +UIDField.FFieldName + _Q(1) + ',' +  UIDField.CodeOfValueToString() + ']);' ;

  SourceText.Add(FormatText);
  SourceText.Add('end;');
  Result :=  SourceText.Text;
  SourceText.Free;
end;

function TDBTable.GetClassName: String;
begin
  Result := 'TDBRecordOf' + FTableName;
end;

function TDBTable.GetCreateTableSql: TStringList;
Var
  FieldData:TFieldData;
  PrimaryKey : String;
  Str:String;
begin
  Result := TStringList.Create;
  Result.Add(Format('CREATE TABLE `%s` (',[FTableName]));
  PrimaryKey := '';
  for FieldData in FFields do
  begin
    Result.Add(FieldData.GetMysqlCloumnDesc());
    if mopPrimaryKey in FieldData.FMysqlOps then
    begin
      PrimaryKey := PrimaryKey + Format('`%s`,',[FieldData.FFieldName]);
    end;
  end;

  //有主键
  if PrimaryKey <> '' then
  begin
    PrimaryKey := Copy(PrimaryKey,1,Length(PrimaryKey) - 1); //删掉最后一个逗号
    Result.Add(Format('PRIMARY KEY (%s),',[PrimaryKey]));
  end;


  for FieldData in FFields do
  begin
    if mopIndexBTree in FieldData.FMysqlOps then
    begin
      Result.Add(Format('KEY `%s`(%s),',['KEY_' + FieldData.FFieldName,FieldData.FFieldName]));
    end else if mopIndexHash in FieldData.FMysqlOps then
    begin
      Result.Add(Format('KEY `%s`(%s) USING HASH,',['KEY_' + FieldData.FFieldName,FieldData.FFieldName]));
    end;
  end;

  Str := Result[Result.Count - 1];
  Result[Result.Count - 1] := Copy(Str,1,Length(Str) -1); //去掉最后一行的逗号

  Result.Add(')');
end;


function TDBTable.GetFeatchProcCodeText(): String;
var
  SourceText : TStringList;
  FieldData:TFieldData;
  OP:TFieldCodeOp;
  Text:String;
begin
   SourceText := TStringList.Create;
   SourceText.Add('var');
   SourceText.Add(_WD(2) + ' V:' + GetClassName() + ';' );
   SourceText.Add(_WD(2) + ' I:Integer;' );
   SourceText.Add('begin');
   SourceText.Add(_WD(2) + 'Result := ' + Format('TList<%s>.Create;',[GetClassName()]));
   SourceText.Add(_WD(2) + 'for I := 0 to MysqlRows.Count - 1 do ');
   SourceText.Add(_WD(2) + 'begin');
   SourceText.Add(_WD(4) + 'V := ' + GetClassName() + '.Create();');
   for FieldData in FFields do
   begin
      OP := GetDelphiTypeOP(FieldData.FDelphiType);
      case OP of
        fcOpNone: Text := Format('FieldByName(%s,V.%s);',[_Q(1) + FieldData.FFieldName + _Q(1),FieldData.FFieldName]);
        fcOpIntToStr: Text := Format('V.%s := FieldByNameAsInteger(%s);',[FieldData.FFieldName ,_Q(1) + FieldData.FFieldName + _Q(1)]);
      end;
      SourceText.Add(_WD(4) + Text);

   end;
   SourceText.Add(_WD(4) + 'Result.Add(V);');
   SourceText.Add(_WD(4) +'MysqlRows.Next();');
   SourceText.Add(_WD(2) + 'end;');

   SourceText.Add('end');
   Result := SourceText.Text;
   SourceText.Free;
end;

procedure TDBTable.GetQueryFuncCode(List: TStrings);
var
  QueryGroup : TQueryGroup;
  FieldData:TFieldData;
  FuncName:String;
  Params:String;
  SqlAnd:String;
begin

  FuncName := '';
  Params := '';
  SqlAnd := '';
  for QueryGroup in QueryGroups do
  begin
    for FieldData in QueryGroup.GroupFields  do
    begin
      FuncName := FuncName + '_' + FieldData.FFieldName;
      Params := Params + Format('%s : %s;',[FieldData.FFieldName,FieldData.FDelphiType]);
      //SqlAnd := SqlAnd + FieldData.GetUPDATEFieldCodeText() + ' + ' +  _Q(1) +  'and' +_Q(1) + ' + ' ;
      SqlAnd := SqlAnd + FieldData.GetUPDATEFieldCodeText() + ' + ' +  _Q(1) +  'and' +_Q(1) + ' + ' ;
    end;

    List.Add(Format('class function %s.%s(%s):TList<%s>',[GetClassName(),'QueryAnd' + FuncName,Params,GetClassName()]));
    List.Add('var');
    List.Add('Rows : MysqlRows');
    List.Add('Sql:String;');
    List.Add('Sql := ' + _Q(1) + 'select * from ' + FTableName + ' where ' + _Q(1) + ' + ' + SqlAnd + ';');
    List.Add(Format('Rows := TDBEngine.Inst().Query(%s);',['Sql']));


  end;

end;

procedure TDBTable.GetQueryFuncNameDefine(List: TStrings);
begin

end;

procedure TDBTable.SaveToDir(const Path: String);
var
  Source : TStringList;
  Define:TStringList;
  implement:TStringList;
  FieldData : TFieldData;
  QueryGroup : TQueryGroup;
  function GetInsertProcName(isDefine:Boolean):string;
  begin
    if isDefine then
      Result := 'procedure Insert();'
    else
      Result := 'procedure ' + GetClassName() + '.Insert();'
  end;


  function GetUpdateProcName(isDefine:Boolean):string;
  begin
    if isDefine then
      Result := 'procedure Update();'
    else
      Result := 'procedure ' + GetClassName() + '.Update();'
  end;

  function GetCreateTableSqlProcName(isDefine:Boolean):string;
  begin
    if isDefine then
      Result := 'class function GetCreateTableSql():TStringList;override;'
    else
      Result := 'class function ' + GetClassName() + '.GetCreateTableSql():TStringList;'
  end;

  function GetFeatchProcName(isDefine:Boolean):string;
  begin
    if isDefine then
      Result := Format('class function FeatchToList(MysqlRows:TMysqlRow):TList<%s>;override;',[GetClassName()])
    else
      Result := Format('class function ' + GetClassName()+'.FeatchToList(MysqlRows:TMysqlRow):TList<%s>;override;',[GetClassName()])
  end;


  function GetFileName():string;
  begin
    Result := 'DB_' + FTableName;
  end;

  function FindQueryGroup(GroupID : Integer;GroupType:TMysqlOP):TQueryGroup;
  var
    I:Integer;
  begin
    Result := nil;
    if GroupType in [mopQueryOr,mopQueryAnd] then
    begin
      for I := 0 to QueryGroups.Count - 1 do
      begin
        if (QueryGroups[i].GroupID = GroupID) and (QueryGroups[i].QueryType = GroupType)  then
        begin
          Result := QueryGroups[i];
        end;
      end;
    end;

    if Result = nil then
    begin
      Result := TQueryGroup.Create;
      QueryGroups.Add(Result);
      Result.GroupID := GroupID;
      Result.QueryType := GroupType;
    end;

  end;

begin
  Source := TStringList.Create;
  Source.Assign(Templelate);
  Source[0] := 'unit ' + GetFileName() + ';';

  Source.Text := StringReplace(Source.Text,'//#AddTableName',_WD(2) + 'TDBEngine.RegisterClass(' + _Q(1) + FTableName + _Q(1) + ' , ' + GetClassName() + ' );',[rfReplaceAll]);

  //类定义头开始
  Define := TStringlist.Create;
  Define.Add('Type');
  Define.Add(_WD(2) + GetClassName() + ' = class(TDBRecordBase)');

  for FieldData in FFields do
  begin
    Define.Add(_WD(4) + FieldData.GetDelphiFiledString());

    if FieldData.FQueryAndGroup > 0 then
    begin
      QueryGroup := FindQueryGroup(FieldData.FQueryAndGroup,mopQueryAnd);
      QueryGroup.GroupFields.Add(FieldData);
    end;

    if FieldData.FQueryOrGroup > 0 then
    begin
      QueryGroup := FindQueryGroup(FieldData.FQueryOrGroup,mopQueryOr);
      QueryGroup.GroupFields.Add(FieldData);
    end;
  end;

  Define.Add(_WD(4) + GetInsertProcName(true));
  Define.Add(_WD(4) + GetUpdateProcName(true));
  Define.Add(_WD(4) + GetCreateTableSqlProcName(true));
  Define.Add(_WD(4) + GetFeatchProcName(true));

  //类定义结束
  Define.Add(_WD(2) + 'end;');
  Source.Text := StringReplace(Source.Text,'//#Type',Define.Text,[rfReplaceAll]);
  Define.Free;

  //功能实现开始
  implement := TStringList.Create;

  //插入函数 Insert
  implement.Add(GetInsertProcName(false));
  implement.Append(GenInsertProcCodeText());


  //更新函数 Update
  implement.Add(GetUpdateProcName(false));
  implement.Append(GenUpdateProcCodeText());

  //加入创表Sql
  implement.Add(GetCreateTableSqlProcName(False));
  implement.Append(GenCreateTableSqlText());

  //加入Featch函数。
  implement.Add(GetFeatchProcName(False));
  implement.Append(GetFeatchProcCodeText());


  //加入Query函数
  GetQueryFuncCode(implement);


  Source.Text := StringReplace(Source.Text,'//#implementation',implement.Text,[rfReplaceAll]);
  implement.Free;
  QueryGroups.Free;

  Source.SaveToFile(Path + '\' + GetFileName()+ '.pas',TEncoding.UTF8);
  Source.Free;
end;

{ TFieldData }

function TFieldData.CodeOfValueToString: String;
var
  OP:TFieldCodeOp;
begin
  OP := GetDelphiTypeOP(Self.FDelphiType);
  case OP of
    fcOpNone: Result :=  'Self._Q(' + Self.FFieldName + ')' ;
    fcOpIntToStr: Result := 'Self._Q(IntToStr(' + Self.FFieldName + '))';
  end;

  if mopAsJson in  Self.FMysqlOps  then
  begin
    Result := Format('Self._Q(TSerialObject.RecordToJson<%s>(%s))',[FDelphiType,FFieldName]);
  end else if mopJsonClass in Self.FMysqlOps then
  begin
    Result := 'Self._Q(' +  FFieldName + '.ToJsonString());';
  end;
end;

function TFieldData.GetDelphiFiledString: String;
begin
  Result := FFieldName + ':' + FDelphiType + ';'
end;

function TFieldData.GetMysqlCloumnDesc: String;
var
  MysqlType:String;
begin
    if (mopAsJson in FMysqlOps) or (mopJsonClass in FMysqlOps) then
    begin
      MysqlType := 'json';
    end else
    begin
      MysqlType := DelphiTypeToMysql(FDelphiType);
      if (mopPrimaryKey in FMysqlOps) and (LowerCase(FDelphiType) = 'string') then
      begin
        if (FLength > (65535 div 4)) or (FLength < 1) then
        begin
          raise Exception.Create( format('string 做为主键 必须指定长度 并且取值范围应当在:[1-%d] FieldName:%s',[65535 div 4,FFieldName]) );
        end;
        MysqlType := Format('varchar(%d)',[FLength]);
      end;
    end;

    if FComment <> '' then
    begin
      Result := Format('`%s` %s comment %s,',[FFieldName,MysqlType,FComment])
    end else
    begin
      Result := Format('`%s` %s ,',[FFieldName,MysqlType])
    end;

end;

function TFieldData.GetUPDATEFieldCodeText: string;
begin
  Result := _Q(1) + FFieldName + ' = ' + _Q(1) +' + ' +  CodeOfValueToString();
end;

function TFieldData.IsNeedQuote: Boolean;
begin
  Result := false;
end;

function TFieldData.IsUIDField: Boolean;
begin
  Result := mopUID in FMysqlOps;
end;

var
 R : TResourceStream;

{ TQueryGroup }

constructor TQueryGroup.Create;
begin
  GroupID := 0;
  GroupFields := TList<TFieldData>.Create;
  QueryType := mopQueryAnd;
end;

destructor TQueryGroup.Destroy;
begin
  GroupFields.Free;
  inherited;
end;

initialization
  Templelate := TStringList.Create;
  R := TResourceStream.Create(HInstance,'DB','RC_DATA');
  Templelate.LoadFromStream(R);
  R.Free;
finalization
  Templelate.Free;


end.
