program ProtocolGen;

{$APPTYPE CONSOLE}

{$R *.res}


{$R 'DBCodeGenResource.res' 'DBCodeGenResource.rc'}

uses
  System.SysUtils,
  GenCode in 'GenCode.pas',
  RecordProtocol in 'RecordProtocol.pas';

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
