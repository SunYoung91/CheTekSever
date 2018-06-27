program DBCodeGen;

{$APPTYPE CONSOLE}

{$R *.res}


{$R 'DBCodeGenResource.res' 'DBCodeGenResource.rc'}

uses
  System.SysUtils,
  GenCode in 'GenCode.pas',
  DB_Schema in 'Test\DB_Schema.pas',
  AttrType in 'Test\AttrType.pas',
  MysqlOP in 'MysqlOP.pas',
  DoubleDict in 'DoubleDict.pas',
  DBTemplelate in 'DBTemplelate.pas',
  TableCodeGen in 'TableCodeGen.pas',
  DBRecordBase in '..\..\mysql\DBRecordBase.pas',
  CheTek.DBEngine in '..\..\mysql\CheTek.DBEngine.pas',
  CheTek.Mysql in '..\..\mysql\CheTek.Mysql.pas',
  FieldData in 'FieldData.pas',
  Utils in 'Utils.pas';

begin
  try
    { TODO -oUser -cConsole Main : Insert code here }
    With TCodeGen.Create(ParamStr(1),ParamStr(2)) do
    begin
      Gen();
      Free;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
