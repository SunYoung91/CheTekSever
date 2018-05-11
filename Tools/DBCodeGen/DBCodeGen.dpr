program DBCodeGen;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  GenCode in 'GenCode.pas';

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
