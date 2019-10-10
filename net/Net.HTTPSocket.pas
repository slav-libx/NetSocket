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
  THTTPClient = class(TTCPSocket)
  private
    FRequest: TRequest;
    FResponse: TResponse;
    FOnCompleted: TNotifyEvent;
    procedure OnReadComplete(Sender: TObject);
  protected
    procedure DoConnected; override;
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

constructor THTTPClient.Create;
begin
  inherited;
  FRequest:=TRequest.Create;
  FResponse:=TResponse.Create;
  Response.OnReadComplete:=OnReadComplete;
end;

destructor THTTPClient.Destroy;
begin
  FRequest.Free;
  FResponse.Free;
  inherited;
end;

procedure THTTPClient.Get(const URL: string);
var URI: TURI;
begin

  URI.Create(URL);

  Request.Reset;

  Request.Method:=METHOD_GET;
  Request.Protocol:=PROTOCOL_HTTP11;
  Request.Resource:=URI.Path;
  Request.Headers.AddValue('Host',URI.Host);
  Request.Headers.SetConnection(True,0);

  Connect(URI.Host,URI.Port);

end;

procedure THTTPClient.DoConnected;
begin
  inherited;
  Response.Reset;
end;

procedure THTTPClient.DoAfterConnect;
begin
  Socket.Send(Request.Compose);
end;

procedure THTTPClient.DoReceived;
var Bytes: TBytes;
begin
  inherited;
  Socket.Receive(Bytes);
  Response.DoRead(Bytes);
end;

procedure THTTPClient.OnReadComplete(Sender: TObject);
begin
  Response.Merge(Request);
  if Assigned(FOnCompleted) then FOnCompleted(Self);
end;

end.
