unit TableCodeGen;

interface
uses MysqlOP,System.Generics.Collections,System.SysUtils,System.Classes,JclStrings,System.IOUtils,FieldData,Utils,JclDateTime;
type

    TQueryGroup = Class
      GroupID:Integer;
      GroupFields:TList<TFieldData>;
      QueryType:TMysqlOP;
      Limit1 : Boolean;
       constructor Create();
       destructor Destroy();override;
    end;

    TDBTable = class
    private
      FTableName:String;
      FFields : TObjectList<TFieldData>;
      FQueryGroups : TObjectList<TQueryGroup>;
      FUsesStr:String;
      FType:string; //record or class
      function GetClassName():String;
      function GenInsertProcCodeText():String;
      function GenUpdateProcCodeText():String;
      function GenCreateTableSqlText():String;
      function GetFeatchProcCodeText(): String;
      function GetFeatchRecordCodeText(): String;
      procedure GetQueryFuncNameDefine(List:TStrings); //添加查询函数声明
      procedure GetQueryFuncCode(List:TStrings); //添加查询函数的代码
      function GetQueryGroupName(Group:TQueryGroup;isHeaderDefine:Boolean;Limit1:Boolean):String;
      procedure GetJsonSerializeCode(List:TStrings); //添加Json序列化代码
      function GetRecordInDataBaseCodeText():String;
      function GetInsertOrUpdateCodeText():String;
      function GetAddToEngineCodeText():String;
      function GetAssignProcCodeText():String;
    public
      constructor Create(const TableName:String ;const usesStr ,TypeType:String );
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
    if ((OP = mopPrimaryKey) or (OP = mopUnikey)) and (LowerCase(DelphiType) = 'string') then
    begin
      FieldData.Length := StrToIntDef(StrBetween(OpStr,'(',')'),-1);
    end;

    if (OP = mopQueryAnd) or (OP = mopQueryAnd1) then
    begin
      Group := StrToIntDef(StrBetween(OpStr,'(',')'),0);

      if OP = mopQueryAnd then
        FieldData.AddQueryAndGroup(Group,False)
      else
        FieldData.AddQueryAndGroup(Group,True)

    end else if (OP = mopQueryOr) or (OP = mopQueryOr1)  then
    begin
      Group := StrToIntDef(StrBetween(OpStr,'(',')'),0);
      if OP = mopQueryOr then
        FieldData.AddQueryOrGroup(Group,False)
      else
        FieldData.AddQueryOrGroup(Group,True)
    end;

    FieldData.MysqlOps := FieldData.MysqlOps + [OP];
  end;

  FFields.Add(FieldData);
  ControlList.Free;


end;

constructor TDBTable.Create(const TableName: String ; const usesStr , TypeType:String  );
begin
  FTableName := TableName;
  FFields := TObjectList<TFieldData>.Create;
  FQueryGroups := TObjectList<TQueryGroup>.Create;
  FUsesStr := usesStr;
  FType := TypeType;
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
  InsertSql:String;
  FieldData : TFieldData;
  SourceText:TStringList;
  I:Integer;
begin
  SourceText := TStringList.Create;
  SourceText.Add('var');
  SourceText.Add(_WD(2) + 'Sql:String;');
  SourceText.Add(_WD(2) + 'Fields:String;');
  SourceText.Add(_WD(2) + 'Values:String;');
  SourceText.Add(_WD(2) + 'Rows : TMysqlRow;');
  SourceText.Add(_WD(2) + Format('V : Array[1..%d] of String;',[FFields.Count]));
  SourceText.Add('begin');

  I := 1;
  for FieldData in FFields do
  begin

    SourceText.Add(_WD(2) + Format('Fields := Fields + %s + %s ;' ,[ _Q(1) + FieldData.FieldName + _Q(1) ,_Q(1) + ',' + _Q(1) ]));
    SourceText.Add(_WD(2) +  Format('V[%d] := %s;',[i,FieldData.CodeOfValueToString()]));
    SourceText.Add(_WD(2) +  Format('Values := Values + V[%d] + %s;' ,[I,_Q(1) + ',' + _Q(1)]));
    SourceText.Add('');
    I := I + 1;
  end;

  SourceText.Add(_WD(2) +  'Delete(Values,Length(Values),1);' );
  SourceText.Add(_WD(2) +  'Delete(Fields,Length(Fields),1);' );
  SourceText.Add(_WD(2) +  'Fields := Fields + ' + _Q(1) + ')' + _Q(1) + ';' );

  InsertSql := Format('Sql := ' + _Q(1) + 'INSERT INTO %s' + '( ' + _Q(1) +  ' + Fields ' + '+ ' + _Q(1) + ' VALUES ( ' + _Q(1) + ' + ' + 'Values' + ' + '+ _Q(1) + ')' + _Q(1) + ';',[FTableName]);

  SourceText.Add(_WD(2) + InsertSql);
  SourceText.Add(Format(_WD(2) + 'Rows := Engine.Query(%s);',['Sql']));
  SourceText.Add(_WD(2) + 'if Rows <> nil then');
  SourceText.Add(_WD(4) + 'Result := True');
  SourceText.Add(_WD(2) + 'else');
  SourceText.Add(_WD(4) + 'Result := False');
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

  SourceText.Add(Format(_WD(2) + 'Rows := Engine.Query(%s);',['Sql']));
  SourceText.Add(_WD(2) + 'if Rows <> nil then');
  SourceText.Add(_WD(4) + 'Result := True');
  SourceText.Add(_WD(2) + 'else');
  SourceText.Add(_WD(4) + 'Result := False');
  SourceText.Add('end;');
  Result :=  SourceText.Text;
  SourceText.Free;
end;

function TDBTable.GetAddToEngineCodeText: String;
var
  SourceText : TStringList;
  FieldData:TFieldData;
  OP:TFieldCodeOp;
  Text:String;
begin
   SourceText := TStringList.Create;
   SourceText.Add('begin');
   SourceText.Add(_WD(2) + Format('TDBEngine.CreateSql.AddOrSetValue(%s,%s.GetCreateTableSql());',[_Q(1) + Self.FTableName + _Q(1),Self.GetClassName()]));
   SourceText.Add('end;');
   Result := SourceText.Text;
   SourceText.Free;
end;

function TDBTable.GetAssignProcCodeText: String;

  var
  SourceText : TStringList;
  FieldData:TFieldData;
  OP:TFieldCodeOp;
  Text:String;
begin
   SourceText := TStringList.Create;
   SourceText.Add('begin');
   for FieldData in FFields do
   begin
     SourceText.Add(_WD(2) +  Format('Self.%s := Data.%s;',[FieldData.FieldName,FieldData.FieldName]));
   end;
   SourceText.Add('end;');
   Result := SourceText.Text;
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
  UnqueKey:String;
  Str:String;
begin
  Result := TStringList.Create;
  Result.Add(Format('CREATE TABLE `%s` (',[FTableName]));
  PrimaryKey := '';
  UnqueKey := '';
  for FieldData in FFields do
  begin
    Result.Add(FieldData.GetMysqlCloumnDesc());
    if mopPrimaryKey in FieldData.MysqlOps then
    begin
      PrimaryKey := PrimaryKey + Format('`%s`,',[FieldData.FieldName]);
    end;

    if mopUnikey in FieldData.MysqlOps then
    begin
      UnqueKey := UnqueKey + Format('`%s`,',[FieldData.FieldName]);
    end;

  end;

  //有主键
  if PrimaryKey <> '' then
  begin
    PrimaryKey := Copy(PrimaryKey,1,Length(PrimaryKey) - 1); //删掉最后一个逗号
    Result.Add(Format('PRIMARY KEY (%s),',[PrimaryKey]));
  end;

  if UnqueKey <> '' then
  begin
    UnqueKey := Copy(UnqueKey,1,Length(UnqueKey) - 1); //删掉最后一个逗号
    Result.Add(Format('UNIQUE (%s),',[UnqueKey]));
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
   SourceText.Add(_WD(4) + 'V := Featch(MysqlRows);');
   SourceText.Add(_WD(4) + 'if V <> nil then');
   SourceText.Add(_WD(4) + '  Result.Add(V);');
   SourceText.Add(_WD(4) + 'MysqlRows.Next();');
   SourceText.Add(_WD(2) + 'end;');

   SourceText.Add('end;');
   Result := SourceText.Text;
   SourceText.Free;
end;

function TDBTable.GetFeatchRecordCodeText: String;
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
   SourceText.Add(_WD(2) + 'Result := nil;');
   SourceText.Add(_WD(2) + 'if not MysqlRows.EOF() then');
   SourceText.Add(_WD(2) + 'begin');
   SourceText.Add(_WD(4) + 'V := ' + GetClassName() + '.Create();');
   for FieldData in FFields do
   begin
      OP := GetDelphiTypeOP(FieldData.DelphiType);
      case OP of
        fcOpNone:
        begin
          if (mopJsonClass in FieldData.MysqlOPs) or (mopAsJson in FieldData.MysqlOPs) then
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
        fcOpDouble : Text := Format('V.%s := MysqlRows.FieldByNameAsDouble(%s);',[FieldData.FieldName ,_Q(1) + FieldData.FieldName + _Q(1)]);
        fcOpToMysqlTime :
        begin
         Text := Format('MysqlRows.FieldByName(%s,Str);',[_Q(1) + FieldData.FieldName + _Q(1)]);
         SourceText.Add(_WD(4) + Text);
         Text := Format('V.%s := MysqlDateTimeStrToDateTime(Str);',[FieldData.FieldName]);
        end;
      end;
      SourceText.Add(_WD(4) + Text);

   end;
   SourceText.Add(_WD(4) + 'Result := V;');
   SourceText.Add(_WD(2) + 'end;');
   SourceText.Add('end;');
   Result := SourceText.Text;
   SourceText.Free;
end;

function TDBTable.GetInsertOrUpdateCodeText: String;
var
  SourceText : TStringList;
  FieldData:TFieldData;
  OP:TFieldCodeOp;
  Text:String;
begin
   SourceText := TStringList.Create;
   SourceText.Add('begin');
   SourceText.Add(_WD(2) + 'if InDataBase(Engine) then');
   SourceText.Add(_WD(2) + 'begin');
   SourceText.Add(_WD(4) + 'self.Update(Engine);');
   SourceText.Add(_WD(2) + 'end else');
   SourceText.Add(_WD(2) + 'begin');
   SourceText.Add(_WD(4) + 'self.Insert(Engine);');
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
        SqlStr := SqlStr + FieldData.GetUPDATEFieldCodeText() + ' + ' +  _Q(1) +  ' and ' +_Q(1) + ' + ' ;
      end;
    end else
    begin
      for FieldData in QueryGroup.GroupFields  do
      begin
        SqlStr := SqlStr + FieldData.GetUPDATEFieldCodeText() + ' + ' +  _Q(1) +  '  or ' +_Q(1) + ' + ' ;
      end;
    end;


    SqlStr := Copy(SqlStr,1,Length(SqlStr) - 10);
    if QueryGroup.Limit1 then
    begin
      SqlStr := SqlStr + _Q(1) +  ' limit 1' + _Q(1);
    end;

    List.Add(GetQueryGroupName(QueryGroup,False,QueryGroup.Limit1));
    List.Add('var');
    List.Add(_WD(2) + 'Rows : TMysqlRow;');
    List.Add(_WD(2) + 'Sql:String;');
    List.Add('begin;');
    List.Add(_WD(2) + 'Sql := ' + _Q(1) + 'select * from ' + FTableName + ' where ' + _Q(1) + ' + ' + SqlStr + ';');
    List.Add(Format(_WD(2) + 'Rows := Engine.Query(%s);',['Sql']));

    List.Add(_WD(2) + 'if Rows = nil then');
    List.Add(_WD(2) + 'begin');
    List.Add(_WD(4) + 'Result := False;');
    List.Add(_WD(2) + 'end else begin');
    if QueryGroup.Limit1 then
       List.Add(_WD(4) + 'Data := Featch(Rows);')
    else
      List.Add(_WD(4) + 'Data := FeatchToList(Rows);');
    List.Add(_WD(4) + 'Result := True');
    List.Add(_WD(2) + 'end;');

    List.Add('end;');

  end;

end;

procedure TDBTable.GetQueryFuncNameDefine(List: TStrings);
var
  Group : TQueryGroup;
begin
  for Group in FQueryGroups do
  begin
    List.Add(_WD(2) + GetQueryGroupName(Group,true,Group.Limit1))
  end;
end;

function TDBTable.GetQueryGroupName(Group:TQueryGroup;isHeaderDefine:Boolean;Limit1:Boolean):String;
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
    if Limit1 then
       Result := Format('class function %s(Engine:TDBEngine;%s;var Data:%s):Boolean;',[Fields,Params,GetClassName()])
    else
       Result := Format('class function %s(Engine:TDBEngine;%s;var Data:TList<%s>):Boolean;',[Fields,Params,GetClassName()])
  end else
  begin
    if Limit1 then
      Result := Format('class function %s.%s(Engine:TDBEngine;%s;var Data:%s):Boolean;',[GetClassName(),Fields,Params,GetClassName()])
    else
      Result := Format('class function %s.%s(Engine:TDBEngine;%s;var Data:TList<%s>):Boolean;',[GetClassName(),Fields,Params,GetClassName()])
  end;
end;

function TDBTable.GetRecordInDataBaseCodeText: String;
var
  SourceText : TStringList;
  FieldData:TFieldData;
  OP:TFieldCodeOp;
  Text:String;
  Sql : String;
begin
   SourceText := TStringList.Create;
   SourceText.Add('var');
   SourceText.Add(_WD(2) + 'Sql:String;');
   SourceText.Add(_WD(2) + 'Rows : TMysqlRow;');
   SourceText.Add('begin');

   for FieldData in FFields do
   begin
      if FieldData.IsUIDField() then
      begin
        Sql := 'sql := ' + _Q(1) +'select ' + FieldData.FieldName + ' from ' + FTableName + ' where ' + FieldData.FieldName + ' = ' + _Q(1)  + ' + '+  FieldData.CodeOfValueToString() + ';';
        Break;
      end;
   end;

   SourceText.Add(_WD(2) + Sql);
   SourceText.Add(Format(_WD(2) + 'Rows := Engine.Query(%s);',['Sql']));
   SourceText.Add(_WD(2) + 'if Rows <> nil then');
   SourceText.Add(_WD(2) + 'begin');
   SourceText.Add(_WD(4) + 'if Rows.Count > 0 then');
   SourceText.Add(_WD(4) + _WD(2) +  'Result := true ');
   SourceText.Add(_WD(4) + 'else');
   SourceText.Add(_WD(4) + _WD(2) +  'Result := false ');
   SourceText.Add(_WD(2) + 'end else');
   SourceText.Add(_WD(2) + 'begin');
   SourceText.Add(_WD(4) + 'result := false;');
   SourceText.Add(_WD(2) + 'end;');
   SourceText.Add('end;');
   Result := SourceText.Text;
   SourceText.Free;
end;


procedure TDBTable.SaveToDir(const Path: String);
var
  Source : TStringList;
  Define:TStringList;
  implement:TStringList;
  FieldData : TFieldData;
  QueryGroup : TQueryGroup;
  GroupData:TGroupData;
  function GetInsertProcName(isDefine:Boolean):string;
  begin
    if isDefine then
      Result := 'function Insert(Engine:TDBEngine):Boolean;'
    else
      Result := 'function ' + GetClassName() + '.Insert(Engine:TDBEngine):Boolean;'
  end;


  function GetUpdateProcName(isDefine:Boolean):string;
  begin
    if isDefine then
      Result := 'function Update(Engine:TDBEngine):Boolean;'
    else
      Result := 'function ' + GetClassName() + '.Update(Engine:TDBEngine):Boolean;'
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

  function GetFeatchRecordProcName(isDefine:Boolean):string;
  begin
    if isDefine then
      Result := Format('class function Featch(MysqlRows:TMysqlRow):%s;',[GetClassName()])
    else
      Result := Format('class function ' + GetClassName()+'.Featch(MysqlRows:TMysqlRow):%s;',[GetClassName()])
  end;

  function GetRecordInDataBaseProcName(isDefine:Boolean):string;
  begin
    if isDefine then
      Result := Format('function InDataBase(Engine:TDBEngine):Boolean;',[GetClassName()])
    else
      Result := Format('function ' + GetClassName()+'.InDataBase(Engine:TDBEngine):Boolean;',[GetClassName()])
  end;

  function GetInsertOrUpDateProcName(isDefine:Boolean):string;
  begin
    if isDefine then
      Result := Format('function InsertOrUpdate(Engine:TDBEngine):Boolean;',[GetClassName()])
    else
      Result := Format('function ' + GetClassName()+'.InsertOrUpdate(Engine:TDBEngine):Boolean;',[GetClassName()])
  end;

  function GetAddToDBEngineProcName(isDefine:Boolean):string;
  begin
    if isDefine then
      Result := 'class procedure AddToDBEngine();'
    else
      Result := 'class procedure ' + GetClassName()+'.AddToDBEngine();'
  end;

  function GetAssignProcName(isDefine : Boolean):String;
  begin
    if isDefine then
      Result := Format('procedure Assign( data : %s);',[GetClassName()])
    else
      Result := Format('procedure %s.Assign( data : %s);',[GetClassName(),GetClassName()]);
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

  Source.Text := StringReplace(Source.Text,'{uses}',FUsesStr,[rfReplaceAll]);
  //Source.Text := StringReplace(Source.Text,'//#AddTableName',_WD(2) + 'TDBEngine.RegisterClass(' + _Q(1) + FTableName + _Q(1) + ' , ' + GetClassName() + ' );',[rfReplaceAll]);

  //类定义头开始
  Define := TStringlist.Create;
  Define.Add('Type');
  if LowerCase(FType) = 'record' then
    Define.Add(_WD(2) + GetClassName() + ' = record')
  else
    Define.Add(_WD(2) + GetClassName() + ' = class(TDBRecordBase)');

  for FieldData in FFields do
  begin
    Define.Add(_WD(4) + FieldData.GetDelphiFiledString());

    for GroupData in FieldData.QueryAndGroup  do
    begin
      QueryGroup := FindQueryGroup(GroupData.GroupID,mopQueryAnd);
      QueryGroup.Limit1 := GroupData.Limit1;
      QueryGroup.GroupFields.Add(FieldData);
    end;

    for GroupData in FieldData.QueryOrGroup do
    begin
      QueryGroup := FindQueryGroup(GroupData.GroupID,mopQueryOr);
      QueryGroup.Limit1 := GroupData.Limit1;
      QueryGroup.GroupFields.Add(FieldData);
    end;

  end;

  Define.Add(_WD(4) + GetInsertProcName(true));
  Define.Add(_WD(4) + GetUpdateProcName(true));
  Define.Add(_WD(4) + GetCreateTableSqlProcName(true));
  Define.Add(_WD(4) + GetFeatchProcName(true));
  Define.Add(_WD(4) + GetFeatchRecordProcName(true));
  Define.Add(_WD(4) + GetRecordInDataBaseProcName(true));
  Define.Add(_WD(4) + GetInsertOrUpDateProcName(true));
  Define.Add(_WD(4) + GetAddToDBEngineProcName(true));
  Define.Add(_WD(4) + GetAssignProcName(true));



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

  implement.Add( GetAssignProcName(False));
  implement.Append(GetAssignProcCodeText());

  //加入Featch函数。
  implement.Add(GetFeatchRecordProcName(False));
  implement.Append(GetFeatchRecordCodeText());

  //检测数据是否在DB
  implement.Add(GetRecordInDataBaseProcName(False));
  implement.Append(GetRecordInDataBaseCodeText());

  //insert or update
  implement.Add(GetInsertOrUpDateProcName(False));
  implement.Append(GetInsertOrUpdateCodeText());

  //AddToEngine
  implement.Add(GetAddToDBEngineProcName(False));
  implement.Add(GetAddToEngineCodeText());

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
