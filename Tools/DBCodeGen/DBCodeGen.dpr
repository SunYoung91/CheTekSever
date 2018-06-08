program DBCodeGen;

{$APPTYPE CONSOLE}

{$R *.res}


{$R 'DBCodeGenResource.res' 'DBCodeGenResource.rc'}

uses
  System.SysUtils,
  GenCode in 'GenCode.pas',
  Actor in 'Test\Actor.pas',
  AttrType in 'Test\AttrType.pas',
  MysqlOP in 'MysqlOP.pas',
  DoubleDict in 'DoubleDict.pas',
  DBTemplelate in 'DBTemplelate.pas',
  TableCodeGen in 'TableCodeGen.pas';

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
