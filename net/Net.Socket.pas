unit Net.Socket;

interface

uses
  System.Types,
  System.SysUtils,
  System.Classes,
  System.Threading,
  System.SyncObjs,
  System.Net.Socket,
  System.Net.URLClient;

type

  ICancellationToken = interface
    procedure SetCancelled(Value: Boolean);
    function GetCancelled: Boolean;
    property Cancelled: Boolean read GetCancelled write SetCancelled;
  end;

  TTCPSocket = class
  private
    FSocket: TSocket;
    FAcceptSocket: TSocket;
    FOnConnect: TNotifyEvent;
    FOnClose: TNotifyEvent;
    FOnAccept: TNotifyEvent;
    FOnReceived: TNotifyEvent;
    FOnExcept: TNotifyEvent;
    FException: Exception;
    FLock: TCriticalSection;
    FCancellationToken: ICancellationToken;
    FCloseForce: Boolean;
    function GetHandle: TSocketHandle;
    function GetAddress: string;
    function GetRemoteAddress: string;
  protected
    function Connected: Boolean;
    procedure DoConnect(const NetEndpoint: TNetEndpoint);
    procedure DoAfterConnect; virtual;
    procedure DoConnected; virtual;
    procedure DoReceived; virtual;
    procedure DoClose;
    procedure DoExcept(E: Exception); virtual;
    procedure DoAccept;
    property Socket: TSocket read FSocket;
  public
    constructor Create(Socket: TSocket); overload; virtual;
    constructor Create; overload; virtual;
    destructor Destroy; override;
    procedure Connect(const Address: string; Port: Word); overload;
    procedure Connect(const URL: string); overload;
    procedure Connect; overload;
    procedure Start(Port: Word);
    procedure Disconnect;
    function GetAcceptSocket(Take: Boolean=True): TSocket;
    function ReceiveString: string;
    property Handle: TSocketHandle read GetHandle;
    property Address: string read GetAddress;
    property RemoteAddress: string read GetRemoteAddress;
    property E: Exception read FException;
    property OnConnect: TNotifyEvent read FOnConnect write FOnConnect;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnReceived: TNotifyEvent read FOnReceived write FOnReceived;
    property OnExcept: TNotifyEvent read FOnExcept write FOnExcept;
    property OnAccept: TNotifyEvent read FOnAccept write FOnAccept;
  end;

implementation

// https://docs.microsoft.com/ru-ru/dotnet/framework/network-programming/asynchronous-client-socket-example?view=netframework-4.8

type
  TCancellationToken = class(TInterfacedObject,ICancellationToken)
  private
    FCancelled: Boolean;
  public
    procedure SetCancelled(Value: Boolean);
    function GetCancelled: Boolean;
  end;

function TCancellationToken.GetCancelled: Boolean;
begin
  Result:=FCancelled;
end;

procedure TCancellationToken.SetCancelled(Value: Boolean);
begin
  FCancelled:=Value;
end;

constructor TTCPSocket.Create(Socket: TSocket);
begin
  FCloseForce:={$IFDEF POSIX}True{$ELSE}False{$ENDIF};
  FSocket:=Socket;
  FSocket.Encoding:=TEncoding.ANSI;
  FLock:=TCriticalSection.Create;
  FCancellationToken:=TCancellationToken.Create;
end;

constructor TTCPSocket.Create;
begin
  Create(TSocket.Create(TSocketType.TCP));
end;

destructor TTCPSocket.Destroy;
begin
  FCancellationToken.Cancelled:=True;
  FCancellationToken:=nil;
  Disconnect;
  FAcceptSocket.Free;
  FSocket.Free;
  FLock.Free;
end;

function TTCPSocket.GetAddress: string;
begin
  Result:=Socket.Endpoint.Address.Address;
end;

function TTCPSocket.GetHandle: TSocketHandle;
begin
  Result:=Socket.Handle;
end;

function TTCPSocket.GetRemoteAddress: string;
begin
  Result:=Socket.RemoteAddress;
end;

function TTCPSocket.ReceiveString: string;
begin
  Result:=Socket.ReceiveString;
end;

procedure TTCPSocket.Disconnect;
begin
  if Connected then Socket.Close(FCloseForce);
end;

function TTCPSocket.Connected: Boolean;
begin
  Result:=Assigned(Socket) and (TSocketState.Connected in Socket.State);
end;

function CompareEndpoints(const EndPoint1,EndPoint2: TNetEndpoint): Boolean;
begin
  Result:=(EndPoint1.Address.Address=EndPoint2.Address.Address) and
    (EndPoint1.Port=EndPoint2.Port);
end;

procedure TTCPSocket.Connect(const Address: string; Port: Word);
begin

  TTask.Run(

  procedure
  var NetEndpoint: TNetEndpoint;
  begin

    try

      NetEndpoint:=TNetEndpoint.Create(TIPAddress.Create(Address),Port);

      TThread.Synchronize(nil,

      procedure
      begin

        if Connected then
          if CompareEndpoints(NetEndpoint,Socket.Endpoint) then
            DoAfterConnect
          else begin
            Disconnect;
            DoConnect(NetEndpoint);
          end
        else
          DoConnect(NetEndpoint);

      end);

    except
      on E: Exception do DoExcept(E);
    end;

  end);

end;

procedure TTCPSocket.Connect(const URL: string);
var URI: TURI;
begin
  URI.Create('://'+URL);
  Connect(URI.Host,URI.Port);
end;

procedure TTCPSocket.Connect;
begin
  DoConnect(Socket.Endpoint);
end;

type
  TSocketAccess = class(TSocket);

procedure TTCPSocket.DoConnect(const NetEndpoint: TNetEndpoint);
begin

  TTask.Run(

  procedure
  var
    Lost: Boolean;
    CancellationToken: ICancellationToken;
  begin

    FLock.Enter;

    CancellationToken:=FCancellationToken;

    try

      if not Connected then Socket.Connect(NetEndpoint);

      TThread.Synchronize(nil,

      procedure
      begin
        DoConnected;
        DoAfterConnect;
      end);

      Lost:=False;

      while not Lost and Connected do
      begin

        {$IFDEF POSIX}
        if TSocketAccess(Socket).WaitForData(250)=wrTimeout then Continue;
        {$ELSE}
        TSocketAccess(Socket).WaitForData;
        {$ENDIF}

        TThread.Synchronize(nil,
        procedure
        begin
          if Connected and (Socket.ReceiveLength>0) then
            DoReceived
          else
            Lost:=True;
        end);

      end;

    except
      on E: Exception do DoExcept(E);
    end;

    if Connected then

    TThread.Synchronize(nil,

    procedure
    begin
      if Connected then
      begin
        Disconnect;
        DoClose;
      end;
    end);

    if not CancellationToken.Cancelled then FLock.Leave;

  end);

end;

procedure TTCPSocket.DoAfterConnect;
begin
end;

procedure TTCPSocket.DoConnected;
begin
  if Assigned(FOnConnect) then FOnConnect(Self);
end;

procedure TTCPSocket.DoReceived;
begin
  if Assigned(FOnReceived) then FOnReceived(Self);
end;

procedure TTCPSocket.DoClose;
begin
  if Assigned(FOnClose) then FOnClose(Self);
end;

procedure TTCPSocket.DoExcept(E: Exception);
begin

  if (E is ESocketError) and Assigned(FOnExcept) then

  TThread.Synchronize(nil,

  procedure
  begin
    FException:=E;
    FOnExcept(Self);
    FException:=nil;
  end)

  else ApplicationHandleException(E);

end;

procedure TTCPSocket.DoAccept;
begin
  FOnAccept(Self);
end;

procedure TTCPSocket.Start(Port: Word);
begin

  FCloseForce:=True;

  try

    FSocket.Listen('','',Port);

    DoConnected;

    TTask.Run(

    procedure
    begin

      try

      while Connected do
      begin

        {$IFDEF POSIX}
        if TSocketAccess(Socket).WaitForData(250)=wrTimeout then Continue;
        {$ENDIF}

        FAcceptSocket.Free;
        FAcceptSocket:=nil;

        if not Connected then Break;

        FAcceptSocket:=FSocket.Accept;

        TThread.Synchronize(nil,DoAccept);

      end;

      except
        on E: Exception do DoExcept(E);
      end;

      if Connected then

      TThread.Synchronize(nil,

      procedure
      begin
        Disconnect;
        DoClose;
      end);

    end);

  except
    on E: Exception do DoExcept(E);
  end;

end;

function TTCPSocket.GetAcceptSocket(Take: Boolean=True): TSocket;
begin
  Result:=FAcceptSocket;
  if Take then FAcceptSocket:=nil;
end;

end.
