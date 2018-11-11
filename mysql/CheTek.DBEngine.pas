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
    procedure CreateNewTable(const TableName:String);
    procedure CheckTableStruct(const TableName:String);
    procedure CheckConnect();
    function GetCreateTableSqlMap(List:TStrings):TDictionary<String,TFieldInfo>;
  private
    class var FSingleton : TDBEngine;
  public
    constructor Create(const Host:string;Port:Integer;const UserName , Password ,DataBase : String );
    destructor Destroy;override;
    procedure Init();virtual;
  public
    class var CreateSql:TDictionary<String,TStringList>;
    class var MysqlTypeLevel : TDictionary<String,Integer>;
    class function GetAllTables():TArray<String>;
    class function Inst():TDBEngine;
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
          if Query(Format('CREATE DATABASE IF NOT EXISTS %s default charset utf8mb4 COLLATE utf8mb4_unicode_ci',[FDataBase])) <> nil then
          begin
            OutLog(0,Format('Create DataBase :%s sucess ',[FDataBase]));
            if SetDataBase(FDataBase) then
            begin
               OutLog(0,Format('Change DataBase :%s sucess ',[FDataBase]));
            end else
            begin
               OutLog(0,Format('Create Data Base: %s , can not change to DataBase :%s',[FDataBase,FDataBase]));
            end;
          end else
          begin
            OutLog(0,Format('Can not Create Data Base: %s , maybe permission is not allow ',[FDataBase]));
          end;
       end;

     end;
   end;
end;

procedure TDBEngine.CheckTableStruct(const TableName: String);
var
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
  if CreateSql.TryGetValue(TableName,CreateTableText) then
  begin

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
            if  Query(Sql) = nil then
            begin
              OutLog(0 , Format('CheckTableStruct Cant Add Field :%s , Sql Error , %s , %s ' ,[FieldName,Sql,ErrorDesc] ));
            end else
            begin
              OutLog(0 , Format(' %s Add  Field : %s ,Type: %s SUCESS!',[TableName,FieldName,GenFieldInfo.FieldType] ));
            end;
          end else
          begin
            OutLog(0 , Format('CheckTableStruct Can not Get Filed Mysql Desc, Field : %s'  , [FieldName] ));
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
                OutLog( 0 ,  Format('CheckTableStruct FieldLength Not Allow Change To Samller , Field : %s' ,[FieldName] ));
              end;

            end else
            begin

              //类型变了 可能是 int 变成 String 也可能是 tiny int 等变成其他的
              if not MysqlTypeLevel.TryGetValue(RealDBFieldType,DBFieldLevel) then
              begin
                OutLog( 0 ,  Format('CheckTableStruct Cant find mysql Level Define , Field : %d Type : %s' ,[FieldName,RealDBFieldType] ));
                Exit;
              end;

              if not MysqlTypeLevel.TryGetValue(RealGenDBFieldType,GenFieldLevel) then
              begin
                OutLog( 0 ,  Format('CheckTableStruct Cant find mysql Level Define , Field : %d Type : %s' ,[FieldName,RealGenDBFieldType] ));
                Exit;
              end;


              if (GenFieldLevel div 100) <> (DBFieldLevel div 100) then
              begin
                OutLog( 0 ,  Format('CheckTableStruct Cant  mysql Level type not the same , %d , %d ' ,[GenFieldLevel,DBFieldLevel] ));
                Exit;
              end;

              if (GenFieldLevel mod 100) <= (DBFieldLevel mod 100) then
              begin
                //varchar 跟text 的转换总应该是允许的
                if (GenFieldLevel <> 101) and (DBFieldLevel <> 101) then
                begin
                  OutLog( 0 ,  Format('CheckTableStruct Cant  mysql Level type not allow down level , %d , %d ' ,[GenFieldLevel,DBFieldLevel] ));
                  Exit;
                end;
              end;

              AllowChange := True;
            end;

            if AllowChange then
            begin
              Sql := Format('ALTER TABLE `%s` MODIFY COLUMN `%s` %s;',[TableName,FieldName,GenFieldInfo.FieldType]);
              if  Query(Sql) = nil then
              begin
                OutLog( 0 , Format('CheckTableStruct Cant Query Modify Sql Error , %s , %d ' ,[Sql,ErrorDesc] ));
              end else
              begin
                OutPutInfo(Format(' %s Update Field : %s ,Type:  %s ->%s SUCESS!',[TableName,FieldName,DBFieldInfo.FieldType,GenFieldInfo.FieldType] ));
              end;
            end;

          end else if DBFieldInfo.Comment <> GenFieldInfo.Comment then
          begin
              Sql := Format('ALTER TABLE `%s` MODIFY COLUMN `%s` %s comment %s ;',[TableName,FieldName,GenFieldInfo.FieldType,GenFieldInfo.Comment]);
              if Query(Sql) = nil then
              begin
                OutLog( 0 , Format('CheckTableStruct Cant Query Modify Sql Error , %s , %d ' ,[Sql,ErrorDesc] ));
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
    OutLog( 0 , 'CheckTableStruct Can not find tableclass :' + TableName );
  end;
end;

constructor TDBEngine.Create(const Host: string; Port: Integer; const UserName,
  Password , DataBase: String );
begin
    inherited Create();
    FHost := Host;
    FPort := Port;
    FUserName := UserName;
    FPassword := Password;
    FDataBase := DataBase;
    FSingleton := Self;
end;

procedure TDBEngine.CreateNewTable(const TableName: String);
var
  CreateTableSql:TStringList;
begin
  if CreateSql.TryGetValue(TableName,CreateTableSql) then
  begin
      //CreateTableSql.SaveToFile('D:\Sql.sql');
      if Query(CreateTableSql.Text) = nil then
      begin
        OutLog( 0 , 'CreateTableSql Error:, Class : ' + TableName + ' , MysqlError:' + ErrorDesc);
      end;
      CreateTableSql.Free;
  end else
  begin
    OutLog( 0 , 'Can not get Table Class : ' + TableName);
  end;
end;

destructor TDBEngine.Destroy;
begin
  inherited;
end;

class function TDBEngine.GetAllTables: TArray<String>;
begin
  Result := CreateSql.Keys.ToArray();
end;

function TDBEngine.GetCreateTableSqlMap(
  List: TStrings): TDictionary<String, TFieldInfo>;
var
  Str,Str2:String;
  FiledName:String;
  MysqlType:String;
  Info: TFieldInfo;
  I:Integer;
begin
  Result := TDictionary<String, TFieldInfo>.Create;
  for  I := 0 to List.Count - 1  do
  begin
    Str2 := TrimLeft(List[i]);
    if Length(Str2) >= 1 then
    begin
      if Str2[1] = '`' then
      begin
        FiledName := Trim(StrBetween(Str2,'`','`'));
        MysqlType :=  StrBetween(Str2,' ',' ');
        if StrMatch('unsigned',Str2) > 0 then
        begin
          MysqlType := MysqlType + ' unsigned';
        end;

        Info.FieldType := MysqlType;
        info.Comment := StrAfter('comment',LowerCase(Str2));
        info.Comment := StrBefore(',',Info.Comment);
        if FiledName <> '' then
        begin
         Result.Add( FiledName , info );
        end else
        begin
          Raise Exception.Create('can not get GetCreateTableSqlMap : ' + Str2);
        end;
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

class function TDBEngine.Inst: TDBEngine;
begin
  if FSingleton = nil then
  begin
    raise Exception.Create('TDBEngine Inst is nil Need Init!');
  end;

  Result := FSingleton;
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
    Add('json',200);
  end;
end;

initialization
begin
  TDBEngine.MysqlTypeLevel := TDictionary<String,Integer>.Create;
  TDBEngine.CreateSql := TDictionary<String,TStringList>.Create;
  InitMysqlLevelConst();
end;

finalization

FreeAndNil(TDBEngine.MysqlTypeLevel);

if TDBEngine.CreateSql <> nil then
begin
  FreeAndNil(TDBEngine.CreateSql);
end;


end.
