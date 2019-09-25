unit Form.AppMain;

interface

uses
  System.SysUtils,
  System.Types,
  System.UIConsts,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.IOUtils,
  System.Net.Socket,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Controls.Presentation,
  FMX.ScrollBox,
  FMX.Memo,
  Lib.HTTPConsts,
  Lib.HTTPContent,
  FMX.StdCtrls,
  FMX.Objects,
  Net.Socket;

type
  TForm12 = class(TForm)
    Memo1: TMemo;
    Circle1: TCircle;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
    TCPSocket: TTCPSocket;
    Request: TRequest;
    Response: TResponse;
    procedure OnConnect(Sender: TObject);
    procedure OnClose(Sender: TObject);
    procedure OnExcept(Sender: TObject; E: Exception);
    procedure OnReceived(Sender: TObject; const Bytes: TBytes);
    procedure SetConnect(Active: Boolean);
    procedure OnReadComplete(Sender: TObject);
    procedure ToLog(const Message: string);
    procedure DoHTTPGet;
  public
  end;

var
  Form12: TForm12;

implementation

{$R *.fmx}

// https://docs.microsoft.com/ru-ru/dotnet/framework/network-programming/asynchronous-client-socket-example?view=netframework-4.8

procedure TForm12.FormCreate(Sender: TObject);
begin

  SetConnect(False);

  Request:=TRequest.Create;

  Response:=TResponse.Create;
  Response.OnReadComplete:=OnReadComplete;

  Request.Method:=METHOD_GET;
  Request.Protocol:=PROTOCOL_HTTP11;
//  Request.Resource:='/api/transactions/2000';
  Request.Resource:='/2.jpg';
  Request.Headers.AddValue('Host','185.182.193.15');
  Request.Headers.SetConnection(True,0);

  TCPSocket:=TTCPSocket.Create;
  TCPSocket.OnConnect:=OnConnect;
  TCPSocket.OnClose:=OnClose;
  TCPSocket.OnReceived:=OnReceived;
  TCPSocket.OnExcept:=OnExcept;

end;

procedure TForm12.FormDestroy(Sender: TObject);
begin
  TCPSocket.Free;
  Request.Free;
  Response.Free;
end;

procedure TForm12.ToLog(const Message: string);
begin
  if not Application.Terminated then
    Memo1.Lines.Add(Message);
end;

procedure TForm12.SetConnect(Active: Boolean);
begin
  if not Application.Terminated then
  if Active then
    Circle1.Fill.Color:=claGreen
  else
    Circle1.Fill.Color:=claRed;
end;

procedure TForm12.Button3Click(Sender: TObject);
begin
  Memo1.Lines.Clear;
end;

procedure TForm12.Button4Click(Sender: TObject);
begin

  if not (TSocketState.Connected in TCPSocket.State) then
    TCPSocket.Connect(TNetEndpoint.Create(127,0,0,1,80))
  else
    DoHTTPGet;

end;

procedure TForm12.Button5Click(Sender: TObject);
begin
  if TSocketState.Connected in TCPSocket.State then
    TCPSocket.Close;
end;

procedure TForm12.OnConnect(Sender: TObject);
begin

  SetConnect(True);

  ToLog('Connected to '+TCPSocket.RemoteAddress);

  DoHTTPGet;

end;

procedure TForm12.OnClose(Sender: TObject);
begin

  SetConnect(False);

  ToLog('Disconnected');

end;

procedure TForm12.OnExcept(Sender: TObject; E: Exception);
begin
  ToLog(E.Message);
end;

procedure TForm12.OnReceived(Sender: TObject; const Bytes: TBytes);
begin
  Response.DoRead(Bytes);
end;

procedure TForm12.OnReadComplete(Sender: TObject);
begin
  ToLog(Response.ResultCode.ToString+' '+Response.ResultText);
  ToLog(Response.Headers.Text+#13);
  ToLog(Response.LocalResource);
  ToLog(Response.ResourceName);
  TFile.WriteAllBytes('d:\121212.jpg',Response.Content);
//  ToLog(TEncoding.ANSI.GetString(Response.Content));
end;

procedure TForm12.DoHTTPGet;
var C: Integer;
begin

  C:=TCPSocket.Send(Request.Compose);

  ToLog('Send '+C.ToString+' bytes');

end;

end.
