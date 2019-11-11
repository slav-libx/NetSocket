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
    FOnResponse: TNotifyEvent;
    procedure OnReadComplete(Sender: TObject);
  protected
    procedure DoConnected; override;
    procedure DoAfterConnect; override;
    procedure DoReceived; override;
  public
    constructor Create(Socket: TSocket); override;
    destructor Destroy; override;
    procedure Get(const URL: string);
    property Request: TRequest read FRequest;
    property Response: TResponse read FResponse;
    property OnResponse: TNotifyEvent read FOnResponse write FOnResponse;
  end;

  THTTPServerClient = class(TTCPSocket)
  private
    FRequest: TRequest;
    FResponse: TResponse;
    FOnRequest: TNotifyEvent;
    procedure OnReadComplete(Sender: TObject);
  protected
    procedure DoConnected; override;
    procedure DoReceived; override;
  public
    constructor Create(Socket: TSocket); override;
    destructor Destroy; override;
    property Request: TRequest read FRequest;
    property Response: TResponse read FResponse;
    property OnRequest: TNotifyEvent read FOnRequest write FOnRequest;
  end;

implementation

constructor THTTPClient.Create(Socket: TSocket);
begin
  inherited Create(Socket);
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
  Send(Request.Compose);
end;

procedure THTTPClient.DoReceived;
begin
  Response.DoRead(Receive);
  inherited;
end;

procedure THTTPClient.OnReadComplete(Sender: TObject);
begin
  Response.Merge(Request);
  if Assigned(FOnResponse) then
  TThread.Synchronize(SyncThread,
  procedure
  begin
    FOnResponse(Self);
  end);
end;

{ THTTPServerClient }

constructor THTTPServerClient.Create(Socket: TSocket);
begin
  inherited Create(Socket);
  FRequest:=TRequest.Create;
  FResponse:=TResponse.Create;
  FRequest.OnReadComplete:=OnReadComplete;
end;

destructor THTTPServerClient.Destroy;
begin
  FRequest.Free;
  FResponse.Free;
  inherited;
end;

procedure THTTPServerClient.DoConnected;
begin
  inherited;
  Request.Reset;
end;

procedure THTTPServerClient.DoReceived;
begin
  Request.DoRead(Receive);
  inherited;
end;

procedure THTTPServerClient.OnReadComplete(Sender: TObject);
begin

  Request.Merge;

  Response.Reset;
  Response.Protocol:=PROTOCOL_HTTP11;
  Response.Headers.SetConnection(False,0);

  if Request.Protocol<>PROTOCOL_HTTP11 then
  begin

    Response.SetResult(HTTPCODE_NOT_SUPPORTED,'HTTP Version Not Supported')

  end else

    if Request.Method=METHOD_GET then
    begin

      if Assigned(FOnRequest) then
      begin

        if Assigned(FOnRequest) then
        TThread.Synchronize(SyncThread,
        procedure
        begin
          FOnRequest(Self);
        end);

      end else begin

        Response.SetResult(HTTPCODE_NOT_FOUND,'Not Found');

        Response.AddContentText(content_404,'text/html');

      end;

    end else

      Response.SetResult(HTTPCODE_METHOD_NOT_ALLOWED,'Method Not Allowed');

  Send(Response.Compose);
  Send(Response.Content);

end;

end.
