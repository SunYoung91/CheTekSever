unit CheTek.DBEngine;

interface
uses DBRecordBase,System.Classes,System.Generics.Collections,CheTek.Mysql,JclStrings,System.SysUtils,System.Types;

type
  TFieldInfo = record
    FieldType : String;
    Comment : string;
  end;

  TDBEngine = class(TMysql)
  private
    FHost:String;
    FPort:Integer;
    FUserName:String;
    FPassword:string;
    FDataBase:String;
    FThreadPool:TObjectList<TThread>;
    procedure CreateNewTable(const TableName:String);
    procedure CheckTableStruct(const TableName:String);
    procedure CheckConnect();
    function GetCreateTableSqlMap(List:TStrings):TDictionary<String,TFieldInfo>;
  public
    constructor Create(const Host:string;Port:Integer;const UserName , Password ,DataBase : String ; ThreadCount:Integer = 1 );
    destructor Destroy;override;
    procedure Init();
    procedure RegisterClass(&Class:TDBRecordClass);overload;
  public
    class var TableClasses : TDictionary<String,TDBRecordClass> ;
    class var MysqlTypeLevel : TDictionary<String,Integer>;
    class function GetAllTables():TArray<String>;
    class procedure RegisterClass(const TableName:string ; &Class:TDBRecordClass);overload;
  end;

implementation

{ TDBEngine }

procedure TDBEngine.CheckConnect;
begin
   if not Connected then
   begin
     if Connect(FHost,FPort,FUserName,FPassword) then
     begin
       CharSet := 'utf8';
       if not SetDataBase(FDataBase) then
       begin
          if Query(Format('CREATE DATABASE IF NOT EXISTS %s default charset utf8mb4 COLLATE utf8mb4_unicode_ci',[FDataBase])) then
          begin
            OutPutInfo(Format('Create DataBase :%s sucess ',[FDataBase]));
            if SetDataBase(FDataBase) then
            begin
              OutPutInfo(Format('Change DataBase :%s sucess ',[FDataBase]));
            end else
            begin
              OutPutError(Format('Create Data Base: %s , can not change to DataBase :%s',[FDataBase,FDataBase]));
            end;
          end else
          begin
            OutPutError(Format('Can not Create Data Base: %s , maybe permission is not allow ',[FDataBase]));
          end;
       end;

     end;
   end;
end;

procedure TDBEngine.CheckTableStruct(const TableName: String);
var
  DBRecord : TDBRecordClass;
  CreateTableText:TStringList;
  DBMysqlList:TStrings;
  FieldName,Sql:String;
  RealDBFieldType,RealGenDBFieldType :String;
  RealDBFieldLength,ReadGenFieldLength:Integer;
  DBFieldLevel,GenFieldLevel:Integer;
  DBFieldInfo,GenFieldInfo:TFieldInfo;
  GenCreateTableSqlMap,DBCreateTableSqlMap:TDictionary<String,TFieldInfo>;
  AllowChange:Boolean;

begin
  if TableClasses.TryGetValue(TableName,DBRecord) then
  begin
    CreateTableText := DBRecord.GetCreateTableSql();
    if CreateTableText = nil then
    begin
      OutPutError('CheckTableStruct DBRecord.GetCreateTableSql = nil');
      Exit;
    end;

    DBMysqlList := TStringList.Create;
    if not GetCreateTableSql(TableName,DBMysqlList) then
    begin
      DBMysqlList.Free;
      Exit;
    end;

    //转换为Map
    DBCreateTableSqlMap := GetCreateTableSqlMap(DBMysqlList);
    GenCreateTableSqlMap := GetCreateTableSqlMap(CreateTableText);

    DBMysqlList.Free;
    CreateTableText.Free;

    Try
      for FieldName in GenCreateTableSqlMap.Keys.ToArray() do
      begin
        //当前数据库不存在此字段 需要新增
        if not DBCreateTableSqlMap.ContainsKey(FieldName) then
        begin
          if GenCreateTableSqlMap.TryGetValue(FieldName,GenFieldInfo) then
          begin
            Sql := Format('ALTER TABLE `%s` ADD COLUMN `%s` %s;',[TableName,FieldName,GenFieldInfo.FieldType]);
            if not Query(Sql) then
            begin
              OutPutError(Format('CheckTableStruct Cant Add Field :%s , Sql Error , %s , %s ' ,[FieldName,Sql,ErrorDesc] ));
            end else
            begin
              OutPutInfo(Format(' %s Add  Field : %s ,Type: %s SUCESS!',[TableName,FieldName,GenFieldInfo.FieldType] ));
            end;
          end else
          begin
            OutPutError( Format('CheckTableStruct Can not Get Filed Mysql Desc, Field : %s'  , [FieldName] ));
          end;
        end else
        begin

          DBCreateTableSqlMap.TryGetValue(FieldName,DBFieldInfo);
          GenCreateTableSqlMap.TryGetValue(FieldName,GenFieldInfo);

          //当前数据库结构和目前的不一致
          if DBFieldInfo.FieldType <> GenFieldInfo.FieldType then
          begin

            AllowChange := False;

            RealDBFieldType := Trim(StrBefore('(',DBFieldInfo.FieldType));
            RealGenDBFieldType := Trim(StrBefore('(',GenFieldInfo.FieldType));
            RealDBFieldLength  := StrToIntDef(StrBetween(DBFieldInfo.FieldType,'(',')'),0);
            ReadGenFieldLength  := StrToIntDef(StrBetween(GenFieldInfo.FieldType,'(',')'),0);

            if RealDBFieldType = RealGenDBFieldType then
            begin
              //一样的类型 长度不一样。
              if ReadGenFieldLength >= RealDBFieldLength then
              begin
                AllowChange := True;
              end else
              begin
                OutPutError( Format('CheckTableStruct FieldLength Not Allow Change To Samller , Field : %s' ,[FieldName] ));
              end;

            end else
            begin

              //类型变了 可能是 int 变成 String 也可能是 tiny int 等变成其他的
              if not MysqlTypeLevel.TryGetValue(RealDBFieldType,DBFieldLevel) then
              begin
                OutPutError( Format('CheckTableStruct Cant find mysql Level Define , Field : %d Type : %s' ,[FieldName,RealDBFieldType] ));
                Exit;
              end;

              if not MysqlTypeLevel.TryGetValue(RealGenDBFieldType,GenFieldLevel) then
              begin
                OutPutError( Format('CheckTableStruct Cant find mysql Level Define , Field : %d Type : %s' ,[FieldName,RealGenDBFieldType] ));
                Exit;
              end;


              if (GenFieldLevel div 100) <> (DBFieldLevel div 100) then
              begin
                OutPutError( Format('CheckTableStruct Cant  mysql Level type not the same , %d , %d ' ,[GenFieldLevel,DBFieldLevel] ));
                Exit;
              end;

              if (GenFieldLevel mod 100) <= (DBFieldLevel mod 100) then
              begin
                OutPutError( Format('CheckTableStruct Cant  mysql Level type not allow down level , %d , %d ' ,[GenFieldLevel,DBFieldLevel] ));
                Exit;
              end;

              AllowChange := True;
            end;

            if AllowChange then
            begin
              Sql := Format('ALTER TABLE `%s` MODIFY COLUMN `%s` %s;',[TableName,FieldName,GenFieldInfo.FieldType]);
              if not Query(Sql) then
              begin
                OutPutError(Format('CheckTableStruct Cant Query Modify Sql Error , %s , %d ' ,[Sql,ErrorDesc] ));
              end else
              begin
                OutPutInfo(Format(' %s Update Field : %s ,Type:  %s ->%s SUCESS!',[TableName,FieldName,DBFieldInfo.FieldType,GenFieldInfo.FieldType] ));
              end;
            end;

          end else if DBFieldInfo.Comment <> GenFieldInfo.Comment then
          begin
              Sql := Format('ALTER TABLE `%s` MODIFY COLUMN `%s` %s comment %s ;',[TableName,FieldName,GenFieldInfo.FieldType,GenFieldInfo.Comment]);
              if not Query(Sql) then
              begin
                OutPutError(Format('CheckTableStruct Cant Query Modify Sql Error , %s , %d ' ,[Sql,ErrorDesc] ));
              end else
              begin
                OutPutInfo(Format(' %s Update Field %s , comment : %s ->%s SUCESS!',[TableName,FieldName,DBFieldInfo.Comment,GenFieldInfo.Comment] ));
              end;
          end;

        end;

      end;
    Finally
      GenCreateTableSqlMap.Free;
      DBCreateTableSqlMap.Free;
    End;
  end else
  begin
    OutPutError('CheckTableStruct Can not find tableclass :' + TableName );
  end;
end;

constructor TDBEngine.Create(const Host: string; Port: Integer; const UserName,
  Password , DataBase: String ; ThreadCount:Integer );
begin
    inherited Create();
    FHost := Host;
    FPort := Port;
    FUserName := UserName;
    FPassword := Password;
    FDataBase := DataBase;
    FThreadPool := TObjectList<TThread>.Create;
end;

procedure TDBEngine.CreateNewTable(const TableName: String);
var
  DBRecord : TDBRecordClass;
  CreateTableSql:TStringList;
begin
  if TableClasses.TryGetValue(TableName,DBRecord) then
  begin

    CreateTableSql := DBRecord.GetCreateTableSql();
    if CreateTableSql <> nil then
    begin
      //CreateTableSql.SaveToFile('D:\Sql.sql');
      Exec(CreateTableSql.Text);
      CreateTableSql.Free;
    end else
    begin
      OutPutError('Can not get CreateTableSqlText, Class : ' + TableName);
    end;

  end else
  begin
    OutPutError('Can not get Table Class : ' + TableName);
  end;
end;

destructor TDBEngine.Destroy;
var
  Thread:TThread;
begin
  for Thread in FThreadPool do
  begin
    Thread.Start;
    Thread.Terminate;
    Thread.WaitFor;
  end;
  FThreadPool.Free;
  inherited;
end;

class function TDBEngine.GetAllTables: TArray<String>;
begin
  if TableClasses = nil then
    TableClasses := TDictionary<String,TDBRecordClass>.Create;

  Result := TableClasses.Keys.ToArray();
end;

function TDBEngine.GetCreateTableSqlMap(
  List: TStrings): TDictionary<String, TFieldInfo>;
var
  Str,Str2:String;
  FiledName:String;
  MysqlType:String;
  Info: TFieldInfo;
begin
  Result := TDictionary<String, TFieldInfo>.Create;
  for Str in List do
  begin
    Str2 := Trim(Str);
    if Length(Str2) >= 1 then
    begin
      if Str2[1] = '`' then
      begin
        FiledName := StrBetween(Str2,'`','`');
        MysqlType :=  StrBetween(Str2,' ',' ');

        if StrMatch('unsigned',Str2) > 0 then
        begin
          MysqlType := MysqlType + ' unsigned';
        end;

        Info.FieldType := MysqlType;
        info.Comment := StrAfter('comment',LowerCase(Str2));
        info.Comment := StrBefore(',',Info.Comment);
        Result.Add( FiledName , info );
      end;
    end;

  end;

end;

procedure TDBEngine.Init;
var
  DBTableList:TStrings;
  DefineTableList:TArray<String>;
  Str:string;
begin
  CheckConnect();
  DBTableList := TStringList.Create;
  try
    if GetAllTable(FDataBase,DBTableList) then
    begin
        DefineTableList := GetAllTables();
        for Str in DefineTableList do
        begin
          if DBTableList.IndexOf(Str) >= 0 then
          begin
            CheckTableStruct(Str); //更新数据库结构
          end else
          begin
            CreateNewTable(Str);   //创建新数据库
          end;
        end;
    end;
  finally
    DBTableList.Free;
  end;
end;

class procedure TDBEngine.RegisterClass(const TableName: string;
  &Class: TDBRecordClass);
begin
    if TableClasses = nil then
      TableClasses := TDictionary<String,TDBRecordClass>.Create();
    TableClasses.Add(TableName,&Class);
end;

procedure TDBEngine.RegisterClass(&Class: TDBRecordClass);
begin
  //啥也不做 这里只是用来防止编译器优化文件会将有的生成的 DB 操作源码文件不会编译进来的问题
end;

procedure InitMysqlLevelConst();
begin
  //用于在数据库结构变更时候判断 mod 100 后的数字 如果 升级后的数字更大 才允许升级
  with TDBEngine.MysqlTypeLevel do
  begin
    Add('tinyint',1);
    Add('smallint',2);
    Add('int',3);
    Add('bigint',4);
    Add('varchar',101);
    Add('text',102);
  end;
end;

initialization
begin
  TDBEngine.MysqlTypeLevel := TDictionary<String,Integer>.Create;
  InitMysqlLevelConst();
end;

finalization

FreeAndNil(TDBEngine.MysqlTypeLevel);
if TDBEngine.TableClasses <> nil then
begin
  FreeAndNil(TDBEngine.TableClasses);
end;


end.
