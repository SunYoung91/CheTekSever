program ProtocolGen;

{$APPTYPE CONSOLE}

{$R *.res}


{$R 'DBCodeGenResource.res' 'DBCodeGenResource.rc'}

uses
  System.SysUtils,
  Vcl.Dialogs,
  GenCode in 'GenCode.pas',
  RecordProtocol in 'RecordProtocol.pas',
  CheTek.Pool in '..\..\CheTekCommon\CheTek.Pool.pas';

begin
  try
    { TODO -oUser -cConsole Main : Insert code here }
    With TCodeGen.Create(ParamStr(1),ParamStr(2),ParamStr(3)) do
    begin
      Gen();
      Free;
    end;
  except
    on E: Exception do
      ShowMessage(E.ClassName +  ': ' + E.Message);
  end;
end.
