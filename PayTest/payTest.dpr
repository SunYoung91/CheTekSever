program payTest;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  uTMySql in 'mysql\uTMySql.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
