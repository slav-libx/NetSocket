program netsocket;

uses
  System.StartUpCopy,
  FMX.Forms,
  Form.AppMain in 'Form.AppMain.pas' {Form12},
  Lib.HTTPConsts in 'http\Lib.HTTPConsts.pas',
  Lib.HTTPContent in 'http\Lib.HTTPContent.pas',
  Lib.HTTPHeaders in 'http\Lib.HTTPHeaders.pas',
  Lib.HTTPUtils in 'http\Lib.HTTPUtils.pas',
  Net.Socket in 'Net.Socket.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:=True;
  Application.Initialize;
  Application.CreateForm(TForm12, Form12);
  Application.Run;
end.
