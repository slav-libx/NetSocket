unit Net.HTTPSocket;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Net.Socket,
  System.Net.URLClient,
  Net.Socket,
  Lib.HTTPConsts,
  Lib.HTTPContent;

type
  THTTPSocket = class(TTCPSocket)
  private
    FRequest: TRequest;
    FResponse: TResponse;
    FOnCompleted: TNotifyEvent;
    procedure OnReadComplete(Sender: TObject);
    procedure SendRequest;
  protected
    procedure DoAfterConnect; override;
    procedure DoReceived; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Get(const URL: string);
    property Request: TRequest read FRequest;
    property Response: TResponse read FResponse;
    property OnCompleted: TNotifyEvent read FOnCompleted write FOnCompleted;
  end;

implementation

constructor THTTPSocket.Create;
begin
  inherited;
  FRequest:=TRequest.Create;
  FResponse:=TResponse.Create;
  Response.OnReadComplete:=OnReadComplete;
end;

destructor THTTPSocket.Destroy;
begin
  FRequest.Free;
  FResponse.Free;
  inherited;
end;

procedure THTTPSocket.Get(const URL: string);
var URI: TURI;
begin

  URI.Create(URL);

  Request.Reset;

  Request.Method:=METHOD_GET;
  Request.Protocol:=PROTOCOL_HTTP11;
  Request.Resource:=URI.Path;
  Request.Headers.AddValue('Host',URI.Host);
  Request.Headers.SetConnection(True,0);

  if not Connected then
    ConnectTo(URI.Host,URI.Port)
  else
    SendRequest;

end;

procedure THTTPSocket.DoAfterConnect;
begin
  inherited;
  SendRequest;
end;

procedure THTTPSocket.SendRequest;
begin
  Send(Request.Compose);
end;

procedure THTTPSocket.DoReceived;
begin
  inherited;
  Response.DoRead(ReceivedBytes);
end;

procedure THTTPSocket.OnReadComplete(Sender: TObject);
begin
  Response.Merge(Request);
  if Assigned(FOnCompleted) then FOnCompleted(Self);
end;

end.
