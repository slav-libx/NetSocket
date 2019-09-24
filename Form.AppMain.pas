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
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  System.Net.Socket,
  FMX.Controls.Presentation,
  FMX.ScrollBox,
  FMX.Memo,
  Lib.HTTPConsts,
  Lib.HTTPContent,
  FMX.StdCtrls,
  FMX.Objects;

type
  TForm12 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    Circle1: TCircle;
    Button3: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    Socket: TSocket;
    Request: TRequest;
    Response: TResponse;
    procedure CloseSocket;
    procedure ConnectSocket;
    procedure SetConnect(Active: Boolean);
    procedure OnReadComplete(Sender: TObject);
    procedure ToLog(const Message: string);
    procedure ConnectEvent(const ASyncResult: IAsyncResult);
    procedure SendEvent(const ASyncResult: IAsyncResult);
    procedure ReceiveEvent(const ASyncResult: IAsyncResult);
  public
    { Public declarations }
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
  Request.Resource:='/api/transactions/2000';
//  Request.Resource:='/2.jpg';
  Request.Headers.AddValue('Host','185.182.193.15');
  Request.Headers.SetConnection(True,0);

//      'GET /api/transactions/2000 HTTP/1.1'#13#10+
//      'Host: 185.182.193.15'#13#10+
//      'Connection: keep-alive'#13#10+
//      #13#10,SendEvent);

//      'GET /2.jpg HTTP/1.1'#13#10+
//      'Host: 185.182.193.15'#13#10+
//      'Connection: keep-alive'#13#10+
//      #13#10

  Socket:=TSocket.Create(TSocketType.TCP);

end;

procedure TForm12.FormDestroy(Sender: TObject);
begin
  Socket.Free;
  Request.Free;
  Response.Free;
end;

procedure TForm12.ToLog(const Message: string);
begin
  TThread.Synchronize(nil,
  procedure
  begin
    Memo1.Lines.Add(Message);
  end);
end;

procedure TForm12.SetConnect(Active: Boolean);
begin
  TThread.Synchronize(nil,
  procedure
  begin
    if Active then
      Circle1.Fill.Color:=claGreen
    else
      Circle1.Fill.Color:=claRed;
  end);

end;

procedure TForm12.Button1Click(Sender: TObject);
begin
  ConnectSocket;
end;

procedure TForm12.Button2Click(Sender: TObject);
begin
  CloseSocket;
end;

procedure TForm12.Button3Click(Sender: TObject);
begin
  Memo1.Lines.Clear;
end;

procedure TForm12.ConnectSocket;
begin
  if TSocketState.Connected in Socket.State then
    Socket.BeginSend(Request.Compose,SendEvent)
  else
  Socket.BeginConnect(ConnectEvent,TNetEndpoint.Create(185,182,193,15,80));
//  Socket.BeginConnect(ConnectEvent,TNetEndpoint.Create(127,0,0,1,80));
end;

procedure TForm12.CloseSocket;
begin
  if TSocketState.Connected in Socket.State then
  begin
    Socket.Close;
    SetConnect(False);
  end;
end;

procedure TForm12.ConnectEvent(const ASyncResult: IAsyncResult);
begin

  try

    Socket.EndConnect(ASyncResult);

    SetConnect(True);

    ToLog('Socket connected to '+Socket.RemoteAddress);

    Socket.BeginSend(Request.Compose,SendEvent);

    Socket.BeginReceive(ReceiveEvent);

  except on E: Exception do
    ToLog(E.Message);
  end;

end;

procedure TForm12.SendEvent(const ASyncResult: IAsyncResult);
var C: Integer;
begin

  try

    C:=Socket.EndSend(ASyncResult);

    ToLog('Sent '+C.ToString+' bytes to server.');

  except on E: Exception do
    ToLog(E.Message);
  end;

end;

procedure TForm12.ReceiveEvent(const ASyncResult: IAsyncResult);
var ReceiveBytes: TBytes;
begin

  try

    ReceiveBytes:=Socket.EndReceiveBytes(ASyncResult);

    if Length(ReceiveBytes)=0 then
      CloseSocket
    else begin

      Response.DoRead(ReceiveBytes);

      Socket.BeginReceive(ReceiveEvent);

    end;

  except on E: Exception do
    ToLog(E.Message);
  end;

end;

procedure TForm12.OnReadComplete(Sender: TObject);
begin
  ToLog(Response.ResultCode.ToString+' '+Response.ResultText);
  ToLog(Response.Headers.Text+#13);
  ToLog(Response.LocalResource);
  ToLog(Response.ResourceName);
//  TFile.WriteAllBytes('d:\121212.jpg',Response.Content);
  ToLog(TEncoding.ANSI.GetString(Response.Content));
end;

end.
