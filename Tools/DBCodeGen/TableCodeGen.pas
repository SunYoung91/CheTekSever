unit TableCodeGen;

interface
uses MysqlOP,System.Generics.Collections,System.SysUtils,System.Classes,JclStrings,System.IOUtils,FieldData,Utils,JclDateTime;
type

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
      FQueryGroups : TObjectList<TQueryGroup>;
      function GetClassName():String;
      function GenInsertProcCodeText():String;
      function GenUpdateProcCodeText():String;
      function GenCreateTableSqlText():String;
      function GetFeatchProcCodeText(): String;
      procedure GetQueryFuncNameDefine(List:TStrings); //添加查询函数声明
      procedure GetQueryFuncCode(List:TStrings); //添加查询函数的代码
      function GetQueryGroupName(Group:TQueryGroup;isHeaderDefine:Boolean):String;
      procedure GetJsonSerializeCode(List:TStrings); //添加Json序列化代码
    public
      constructor Create(const TableName:String);
      procedure AddField(const FieldName,DelphiType,ControlString:string);overload;
      function GetCreateTableSql():TStringList;
      Procedure SaveToDir(Const Path:String);
      destructor Destroy;override;
    end;



implementation
var Templelate:TStringList;
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
  Group:Integer;
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

  FieldData := TFieldData.Create;

  FieldData.FieldName := FieldName;
  FieldData.DelphiType := DelphiType;
  if Comment.Length > 0 then
    FieldData.Comment := _Q(2) + Comment + _Q(2);


  for i := 0 to ControlList.Count - 1 do
  begin
    OpStr := Trim(ControlList[i]);
    OpType := Trim(StrBefore('(',OpStr));
    OP := GetMysqlOP(OpType);

    //如果String 类型的作为了 主键 那么 必须指定长度 否则 将无法创表 这个是mysql的限制
    if (OP = mopPrimaryKey) and (LowerCase(DelphiType) = 'string') then
    begin
      FieldData.Length := StrToIntDef(StrBetween(OpStr,'(',')'),-1);
    end;

    if OP = mopQueryAnd then
    begin
      Group := StrToIntDef(StrBetween(OpStr,'(',')'),0);
      FieldData.AddQueryAndGroup(Group);
    end else if OP = mopQueryOr then
    begin
      Group := StrToIntDef(StrBetween(OpStr,'(',')'),0);
      FieldData.AddQueryOrGroup(Group);
    end;

    FieldData.MysqlOps := FieldData.MysqlOps + [OP];
  end;

  FFields.Add(FieldData);
  ControlList.Free;


end;

constructor TDBTable.Create(const TableName: String);
begin
  FTableName := TableName;
  FFields := TObjectList<TFieldData>.Create;
  FQueryGroups := TObjectList<TQueryGroup>.Create;
end;

destructor TDBTable.Destroy;
begin
  FFields.Free;
  FQueryGroups.Free;
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
  SourceText.Add(_WD(2) + 'Rows : TMysqlRow;');
  SourceText.Add(_WD(2) + Format('V : Array[1..%d] of String;',[FFields.Count]));
  SourceText.Add('begin');

  IsFirst := True;
  I := 1;
  for FieldData in FFields do
  begin
    if IsFirst then
    begin
      Fileds := Fileds +  FieldData.FieldName ;
      if FieldData.IsNeedQuote() then
         Values := Values + '''%s'''
      else
         Values := Values + '%s';
    end
    else
    begin
      Fileds := Fileds + ',' + FieldData.FieldName ;
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
  SourceText.Add(Format(_WD(2) + 'Rows := TDBEngine.Inst().Query(%s);',['Sql']));

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
  SourceText.Add(_WD(2) + 'Rows : TMysqlRow;');
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

  FormatText := _WD(2) + 'Sql := Format(' + _Q(1) + 'UPDATE %s SET %s WHERE %s = %s ;' + _Q(1) + ', [' + _Q(1) +  FTableName + _Q(1) + ', Sql ,' + _Q(1) +UIDField.FieldName + _Q(1) + ',' +  UIDField.CodeOfValueToString() + ']);' ;

  SourceText.Add(FormatText);

  SourceText.Add(Format(_WD(2) + 'Rows := TDBEngine.Inst().Query(%s);',['Sql']));
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
    if mopPrimaryKey in FieldData.MysqlOps then
    begin
      PrimaryKey := PrimaryKey + Format('`%s`,',[FieldData.FieldName]);
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
    if mopIndexBTree in FieldData.MysqlOps then
    begin
      Result.Add(Format('KEY `%s`(%s),',['KEY_' + FieldData.FieldName,FieldData.FieldName]));
    end else if mopIndexHash in FieldData.MysqlOps then
    begin
      Result.Add(Format('KEY `%s`(%s) USING HASH,',['KEY_' + FieldData.FieldName,FieldData.FieldName]));
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
   SourceText.Add(_WD(2) + ' Str:String;' );
   SourceText.Add('begin');
   SourceText.Add(_WD(2) + 'Result := ' + Format('TList<%s>.Create;',[GetClassName()]));
   SourceText.Add(_WD(2) + 'for I := 0 to MysqlRows.Count - 1 do ');
   SourceText.Add(_WD(2) + 'begin');
   SourceText.Add(_WD(4) + 'V := ' + GetClassName() + '.Create();');
   for FieldData in FFields do
   begin
      OP := GetDelphiTypeOP(FieldData.DelphiType);
      case OP of
        fcOpNone:
        begin
          if mopJsonClass in FieldData.MysqlOPs then
          begin
            Text := Format('MysqlRows.FieldByName(%s,Str);',[_Q(1) + FieldData.FieldName + _Q(1)]);
            SourceText.Add(_WD(4) + Text);

            Text := Format('V.%s.FromJson(Str);',[FieldData.FieldName]);
          end else if mopAsJson in FieldData.MysqlOPs then
          begin
            Text := Format('MysqlRows.FieldByName(%s,Str);',[_Q(1) + FieldData.FieldName + _Q(1)]);
            SourceText.Add(_WD(4) + Text);
            Text := Format('_DBJsonToRtti(Str,@V.%s,TypeInfo(%s));',[FieldData.FieldName,FieldData.DelphiType]);

          end else
          begin
            Text := Format('MysqlRows.FieldByName(%s,V.%s);',[_Q(1) + FieldData.FieldName + _Q(1),FieldData.FieldName]);
          end;
        end;
        fcOpIntToStr: Text := Format('V.%s := MysqlRows.FieldByNameAsInteger(%s);',[FieldData.FieldName ,_Q(1) + FieldData.FieldName + _Q(1)]);
        fcOpBoolean : Text := Format('V.%s := MysqlRows.FieldByNameAsBoolean(%s);',[FieldData.FieldName ,_Q(1) + FieldData.FieldName + _Q(1)]);
        fcOpDouble : Text := Format('V.%s := MysqlRows.FieldByNameAsDouble%s);',[FieldData.FieldName ,_Q(1) + FieldData.FieldName + _Q(1)]);
        fcOpToMysqlTime :
        begin
         Text := Format('MysqlRows.FieldByName(%s,Str);',[_Q(1) + FieldData.FieldName + _Q(1)]);
         SourceText.Add(_WD(4) + Text);
         Text := Format('V.%s := MysqlDateTimeStrToDateTime(Str);',[FieldData.FieldName]);
        end;
      end;
      SourceText.Add(_WD(4) + Text);

   end;
   SourceText.Add(_WD(4) + 'Result.Add(V);');
   SourceText.Add(_WD(4) +'MysqlRows.Next();');
   SourceText.Add(_WD(2) + 'end;');

   SourceText.Add('end;');
   Result := SourceText.Text;
   SourceText.Free;
end;

procedure TDBTable.GetJsonSerializeCode(List: TStrings);
begin

end;

procedure TDBTable.GetQueryFuncCode(List: TStrings);
var
  QueryGroup : TQueryGroup;
  FieldData:TFieldData;
  SqlStr:String;
begin

  for QueryGroup in FQueryGroups do
  begin

    SqlStr := '';
    if QueryGroup.QueryType = mopQueryAnd then
    begin
      for FieldData in QueryGroup.GroupFields  do
      begin
        SqlStr := SqlStr + FieldData.GetUPDATEFieldCodeText() + ' + ' +  _Q(1) +  'and' +_Q(1) + ' + ' ;
      end;
    end else
    begin
      for FieldData in QueryGroup.GroupFields  do
      begin
        SqlStr := SqlStr + FieldData.GetUPDATEFieldCodeText() + ' + ' +  _Q(1) +  ' or' +_Q(1) + ' + ' ;
      end;
    end;


    SqlStr := Copy(SqlStr,1,Length(SqlStr) - 11);
    List.Add(GetQueryGroupName(QueryGroup,False));
    List.Add('var');
    List.Add(_WD(2) + 'Rows : TMysqlRow;');
    List.Add(_WD(2) + 'Sql:String;');
    List.Add('begin;');
    List.Add(_WD(2) + 'Sql := ' + _Q(1) + 'select * from ' + FTableName + ' where ' + _Q(1) + ' + ' + SqlStr + ';');
    List.Add(Format(_WD(2) + 'Rows := TDBEngine.Inst().Query(%s);',['Sql']));
    List.Add(_WD(2) + 'Result := FeatchToList(Rows);');
    List.Add('end;');

  end;

end;

procedure TDBTable.GetQueryFuncNameDefine(List: TStrings);
var
  Group : TQueryGroup;
begin
  for Group in FQueryGroups do
  begin
    List.Add(_WD(2) + GetQueryGroupName(Group,true))
  end;
end;

function TDBTable.GetQueryGroupName(Group:TQueryGroup;isHeaderDefine:Boolean):String;
var
  FieldData : TFieldData;
  Fields:String;
  Params:string;
begin
  Fields := '';
  Params := '';
  for FieldData in Group.GroupFields  do
  begin
    Fields := Fields + FieldData.FieldName + '_';
    Params := Params + Format('%s : %s;',[FieldData.FieldName,FieldData.DelphiType]);
  end;

  Params := Copy(Params,1,Params.Length- 1);
  Fields := Copy(Fields,1,Fields.Length - 1);

  if Group.QueryType = mopQueryOr then
  begin
    Fields := 'QueryOr_' + Fields;
  end else
  begin
    Fields := 'QueryAnd_' + Fields;
  end;


  if isHeaderDefine then
  begin
    Result := Format('class function %s(%s):TList<%s>;',[Fields,Params,GetClassName()])
  end else
  begin
    Result := Format('class function %s.%s(%s):TList<%s>;',[GetClassName(),Fields,Params,GetClassName()])
  end;
end;

procedure TDBTable.SaveToDir(const Path: String);
var
  Source : TStringList;
  Define:TStringList;
  implement:TStringList;
  FieldData : TFieldData;
  QueryGroup : TQueryGroup;
  GroupID:Integer;
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
      Result := Format('class function FeatchToList(MysqlRows:TMysqlRow):TList<%s>;',[GetClassName()])
    else
      Result := Format('class function ' + GetClassName()+'.FeatchToList(MysqlRows:TMysqlRow):TList<%s>;',[GetClassName()])
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
      for I := 0 to FQueryGroups.Count - 1 do
      begin
        if (FQueryGroups[i].GroupID = GroupID) and (FQueryGroups[i].QueryType = GroupType)  then
        begin
          Result := FQueryGroups[i];
        end;
      end;
    end;

    if Result = nil then
    begin
      Result := TQueryGroup.Create;
      FQueryGroups.Add(Result);
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

    for GroupID in FieldData.QueryAndGroup  do
    begin
      QueryGroup := FindQueryGroup(GroupID,mopQueryAnd);
      QueryGroup.GroupFields.Add(FieldData);
    end;

    for GroupID in FieldData.QueryOrGroup do
    begin
      QueryGroup := FindQueryGroup(GroupID,mopQueryOr);
      QueryGroup.GroupFields.Add(FieldData);
    end;

  end;

  Define.Add(_WD(4) + GetInsertProcName(true));
  Define.Add(_WD(4) + GetUpdateProcName(true));
  Define.Add(_WD(4) + GetCreateTableSqlProcName(true));
  Define.Add(_WD(4) + GetFeatchProcName(true));

  GetQueryFuncNameDefine(Define);

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

  FQueryGroups.Clear;

  Source.SaveToFile(Path + '\' + GetFileName()+ '.pas',TEncoding.UTF8);
  Source.Free;
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
