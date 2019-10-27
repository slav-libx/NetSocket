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

  ILock = interface
    procedure Enter;
    procedure Leave;
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
    FLock: ILock;
    FCloseForce: Boolean;
    [weak]FSyncThread: TThread;
    function GetHandle: TSocketHandle;
    function GetAddress: string;
    function GetRemoteAddress: string;
    function GetLocalHost: string;
    function GetHostAddress: string;
  protected
    procedure DoConnect(const NetEndpoint: TNetEndpoint; Accepted: Boolean);
    procedure DoAfterConnect; virtual;
    procedure DoConnected; virtual;
    procedure DoReceived; virtual;
    procedure DoClose; virtual;
    procedure DoHandleException(E: Exception); virtual;
    procedure DoExcept; virtual;
    procedure DoAccept;
    procedure HandleUIException(E: Exception);
    property Socket: TSocket read FSocket;
  public
    constructor Create(Socket: TSocket); overload; virtual;
    constructor Create; overload; virtual;
    destructor Destroy; override;
    procedure Terminate;
    procedure Connect(const Address: string; Port: Word); overload;
    procedure Connect(const URL: string); overload;
    procedure Connect; overload;
    procedure Start(Port: Word);
    procedure Disconnect;
    function Connected: Boolean;
    function GetAcceptSocket(Take: Boolean=True): TSocket;
    function Receive: TBytes;
    function ReceiveString: string;
    function Send(const Buf; Count: Integer): Integer;
    property Handle: TSocketHandle read GetHandle;
    property Address: string read GetAddress;
    property RemoteAddress: string read GetRemoteAddress;
    property LocalHost: string read GetLocalHost;
    property HostAddress: string read GetHostAddress;
    property E: Exception read FException;
    property SyncThread: TThread read FSyncThread write FSyncThread;
    property OnConnect: TNotifyEvent read FOnConnect write FOnConnect;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnReceived: TNotifyEvent read FOnReceived write FOnReceived;
    property OnExcept: TNotifyEvent read FOnExcept write FOnExcept;
    property OnAccept: TNotifyEvent read FOnAccept write FOnAccept;
  end;

implementation

type
  TLock = class(TInterfacedObject,ILock)
  private
    FLock: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Enter;
    procedure Leave;
  end;

constructor TLock.Create;
begin
  FLock:=TCriticalSection.Create;
end;

destructor TLock.Destroy;
begin
  FLock.Free;
  inherited;
end;

procedure TLock.Enter;
begin
  FLock.Enter;
end;

procedure TLock.Leave;
begin
  FLock.Leave;
end;

constructor TTCPSocket.Create(Socket: TSocket);
begin
  FCloseForce:={$IFDEF POSIX}True{$ELSE}False{$ENDIF};
  FSocket:=Socket;
  FSocket.Encoding:=TEncoding.ANSI;
  FLock:=TLock.Create;
end;

constructor TTCPSocket.Create;
begin
  Create(TSocket.Create(TSocketType.TCP));
end;

destructor TTCPSocket.Destroy;
begin
  Terminate;
  FAcceptSocket.Free;
  FSocket.Free;
end;

procedure TTCPSocket.Terminate;
begin
  FLock:=nil;
  Disconnect; // иначе будет вызов Close(False) для сервера
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

function TTCPSocket.GetLocalHost: string;
begin
  Result:=Socket.LocalHost;
end;

function TTCPSocket.GetHostAddress: string;
begin
  Result:=TIPAddress.LookupName(LocalHost).Address;
end;

function TTCPSocket.Receive: TBytes;
begin
  Result:=Socket.Receive;
end;

function TTCPSocket.ReceiveString: string;
begin
  Result:=Socket.ReceiveString;
end;

function TTCPSocket.Send(const Buf; Count: Integer): Integer;
begin
  Result:=Socket.Send(Buf,Count);
end;

procedure TTCPSocket.Disconnect;
begin
  if TSocketState.Connected in Socket.State then
    Socket.Close(FCloseForce);
end;

function TTCPSocket.Connected: Boolean;
begin
  Result:=Assigned(FLock) and Assigned(FSocket) and (TSocketState.Connected in Socket.State);
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

      TThread.Synchronize(SyncThread,

      procedure
      begin

        if Connected then

          if CompareEndpoints(NetEndpoint,Socket.Endpoint) then
            DoAfterConnect
          else begin
            Disconnect;
            DoConnect(NetEndpoint,False);
          end

        else

          DoConnect(NetEndpoint,False);

      end);

    except
      on E: Exception do DoHandleException(E);
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
  DoConnect(Socket.Endpoint,True);
end;

type
  TSocketAccess = class(TSocket);

procedure TTCPSocket.DoConnect(const NetEndpoint: TNetEndpoint; Accepted: Boolean);
begin

  TTask.Run(

  procedure
  var
    Lost: Boolean;
    Lock: ILock;
  begin

    Lock:=FLock;

    Lock.Enter;

    try

      if not Accepted then Socket.Connect(NetEndpoint);

      TThread.Synchronize(SyncThread,

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

        if Connected then

        try

        TThread.Synchronize(SyncThread,
        procedure
        begin
          if Connected and (Socket.ReceiveLength>0) then
            DoReceived
          else
            Lost:=True;
        end);

        // любые ошибки сокета должны приводить к выходу из цикла чтения данных из сокета
        // другие ошибки возбуждают исключение в основном потоке приложения

        except
          on E: ESocketError do raise;
          on E: Exception do HandleUIException(E);
        end;

      end;

    except
      on E: Exception do DoHandleException(E);
    end;

    if Connected then

    TThread.Synchronize(SyncThread,

    procedure
    begin
      if Connected then
      begin
        Disconnect;
        DoClose;
      end;
    end);

    Lock.Leave;

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

procedure TTCPSocket.DoExcept;
begin
  if Assigned(FOnExcept) then FOnExcept(Self);
end;

procedure TTCPSocket.DoHandleException(E: Exception);
begin

  if Assigned(FLock) then

  if (E is ESocketError) and Assigned(FOnExcept) then

  TThread.Synchronize(SyncThread,

  procedure
  begin
    FException:=E;
    DoExcept;
    FException:=nil;
  end)

  else HandleUIException(E);

end;

procedure TTCPSocket.HandleUIException(E: Exception);
begin

  AcquireExceptionObject;

  TThread.Queue(nil,
  procedure
  begin
    raise E;
  end);

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
    var Lock: ILock;
    begin

      Lock:=FLock;

      try

      while Connected do
      begin

//        {$IFDEF POSIX}
//        if TSocketAccess(Socket).WaitForData(250)=wrTimeout then Continue;
//        {$ENDIF}
//
//        FAcceptSocket.Free;
//        FAcceptSocket:=nil;
//
//        if Connected then FAcceptSocket:=FSocket.Accept;
//
//        if Connected then TThread.Synchronize(SyncThread,DoAccept);

        FAcceptSocket:=FSocket.Accept{$IFDEF POSIX}(250){$ENDIF};

        if Assigned(FAcceptSocket) and Connected then
        begin
          TThread.Synchronize(SyncThread,DoAccept);
          FAcceptSocket.Free;
          FAcceptSocket:=nil;
        end;

      end;

      except
        on E: ESocketError do;
        on E: Exception do HandleUIException(E);
      end;

      if Connected then

      TThread.Synchronize(SyncThread,

      procedure
      begin
        Disconnect;
        DoClose;
      end);

    end);

  except
    on E: Exception do DoHandleException(E);
  end;

end;

function TTCPSocket.GetAcceptSocket(Take: Boolean=True): TSocket;
begin
  Result:=FAcceptSocket;
  if Take then FAcceptSocket:=nil;
end;

end.
