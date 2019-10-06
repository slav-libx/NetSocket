program netsocket;

uses
  System.StartUpCopy,
  FMX.Forms,
  Form.AppMain in 'Form.AppMain.pas' {Form12},
  Net.HTTPSocket in 'net\Net.HTTPSocket.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:=True;
  Application.Initialize;
  Application.CreateForm(TForm12, Form12);
  Application.Run;
end.
