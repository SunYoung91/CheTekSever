program MysqlTest;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  CheTek.Mysql in '..\..\mysql\CheTek.Mysql.pas',
  mysql in '..\..\mysql\mysql.pas',
  CheTek.MysqlPool in '..\..\mysql\CheTek.MysqlPool.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
