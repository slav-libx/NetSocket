unit Net.Socket;

interface

uses
  System.Types,
  System.SysUtils,
  System.Classes,
  System.Math,
  System.SyncObjs,
  System.Net.Socket,
  System.Net.URLClient;

type

  ILock = interface
    procedure Enter;
    procedure Leave;
    procedure SetTerminated(Value: Boolean);
    function GetTerminated: Boolean;
    property Terminated: Boolean read GetTerminated write SetTerminated;
  end;

  TTCPSocket = class
  private
    FSocket: TSocket;
    FName: string;
    FRemoteAddress: string;
    FOnConnect: TNotifyEvent;
    FOnAfterConnect: TNotifyEvent;
    FOnClose: TNotifyEvent;
    FOnAccept: TNotifyEvent;
    FOnReceived: TNotifyEvent;
    FOnExcept: TNotifyEvent;
    FException: Exception;
    FOnLog: TGetStrProc;
    FLock: ILock;
    FCloseForce: Boolean;
    FTag: NativeInt;
    FTagObject: TObject;
    FTagString: string;
    FDisconnecting: Boolean;
    [weak]FSyncThread: TThread;
    function GetHandle: TSocketHandle;
    function GetAddress: string;
    function GetRemoteAddress: string;
    function GetLocalHost: string;
    function GetHostAddress: string;
    procedure Run(Proc: TProc);
  protected
    procedure DoConnect(const NetEndpoint: TNetEndpoint; Accepted: Boolean);
    procedure DoAfterConnect; virtual;
    procedure DoConnected; virtual;
    procedure DoReceived; virtual;
    procedure DoClose; virtual;
    procedure DoHandleException(E: Exception); virtual;
    procedure DoExcept; virtual;
    procedure DoAccept;
    procedure DoLog(const S: string);
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
    function Accept: TSocket;
    function Receive: TBytes;
    function ReceiveString: string;
    function Send(const Buf; Count: Integer): Integer; overload;
    function Send(const Bytes: TBytes): Integer; overload;
    function Send(const S: string): Integer; overload;
    property Handle: TSocketHandle read GetHandle;
    property Address: string read GetAddress;
    property RemoteAddress: string read GetRemoteAddress;
    property LocalHost: string read GetLocalHost;
    property HostAddress: string read GetHostAddress;
    property E: Exception read FException;
    property Name: string read FName write FName;
    property Tag: NativeInt read FTag write FTag default 0;
    property TagObject: TObject read FTagObject write FTagObject;
    property TagString: string read FTagString write FTagString;
    property SyncThread: TThread read FSyncThread write FSyncThread;
    property OnConnect: TNotifyEvent read FOnConnect write FOnConnect;
    property OnAfterConnect: TNotifyEvent read FOnAfterConnect write FOnAfterConnect;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnReceived: TNotifyEvent read FOnReceived write FOnReceived;
    property OnExcept: TNotifyEvent read FOnExcept write FOnExcept;
    property OnAccept: TNotifyEvent read FOnAccept write FOnAccept;
    property OnLog: TGetStrProc read FOnLog write FOnLog;
  end;

implementation

const
  SocketSendError = 'Error sending data: send %d bytes, sending %d bytes';

type
  TLock = class(TInterfacedObject,ILock)
  private
    FLock: TCriticalSection;
    FTerminated: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Enter;
    procedure Leave;
    procedure SetTerminated(Value: Boolean);
    function GetTerminated: Boolean;
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

function TLock.GetTerminated: Boolean;
begin
  Result:=FTerminated;
end;

procedure TLock.SetTerminated(Value: Boolean);
begin
  FTerminated:=Value;
end;

var NameID: Integer=0;

constructor TTCPSocket.Create(Socket: TSocket);
begin
  Inc(NameID);
  FName:=NameID.ToString;
  FCloseForce:={$IFDEF POSIX}True{$ELSE}False{$ENDIF};
  FSocket:=Socket;
  FRemoteAddress:=Socket.RemoteAddress;
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
  FSocket.Free;
end;

procedure TTCPSocket.Terminate;
begin
  if Assigned(FLock) then FLock.Terminated:=True;
  FLock:=nil;
  Disconnect; // else call Close(False) for server
end;

procedure TTCPSocket.Run(Proc: TProc);
begin
  TThread.CreateAnonymousThread(Proc).Start;
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
  Result:=FRemoteAddress;
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
  Result:=Socket.Encoding.GetString(Receive);
end;

function TTCPSocket.Send(const S: string): Integer;
begin
  Result:=Send(Socket.Encoding.GetBytes(S));
end;

function TTCPSocket.Send(const Bytes: TBytes): Integer;
begin
  Result:=Send(Bytes[0],Length(Bytes));
end;

function TTCPSocket.Send(const Buf; Count: Integer): Integer;
//var ResultCount,SendCount: Integer; P: Pointer;
begin

  try
    Result:=Socket.Send(Buf,Count);
    if Result<>Count then raise Exception.CreateFmt(SocketSendError,[Count,Result]);
  except on E: Exception do
    DoHandleException(E);
  end;

//  try
//    Result:=0;
//    while Result<>Count do
//    begin
//      SendCount:=Min(1024*1,Count-Result);
//      P:=Pointer(Integer(@Buf)+Result);
//      ResultCount:=Socket.Send(P^,SendCount);
//      if ResultCount<>SendCount then
//        raise ESocketError.CreateFmt(SocketSendError,[SendCount,ResultCount]);
//      Result:=Result+SendCount;
//    end;
//  except on E: Exception do
//    DoHandleException(E);
//  end;

end;

procedure TTCPSocket.Disconnect;
begin
  if TSocketState.Connected in Socket.State then
  try
    FDisconnecting:=True;
    Socket.Close(FCloseForce);
  finally
    FDisconnecting:=False;
  end;
end;

function TTCPSocket.Connected: Boolean;
begin
  Result:=Assigned(FLock) and Assigned(FSocket) and not FDisconnecting and (TSocketState.Connected in Socket.State);
end;

function CompareEndpoints(const EndPoint1,EndPoint2: TNetEndpoint): Boolean;
begin
  Result:=(EndPoint1.Address.Address=EndPoint2.Address.Address) and
    (EndPoint1.Port=EndPoint2.Port);
end;

procedure TTCPSocket.Connect(const Address: string; Port: Word);
begin

  Run(

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

  DoLog('TTCPSocket.DoConnect('+Name+') Enter');

  Run(

  procedure
  var
    Lock: ILock;
    AName: string;
  begin

    DoLog('TTCPSocket.DoConnect('+Name+') Run()');

    if FLock=nil then Exit;

    AName:=Name;

    Lock:=FLock;

    DoLog('TTCPSocket.DoConnect('+AName+') Lock.Enter Before');

    Lock.Enter;

    DoLog('TTCPSocket.DoConnect('+AName+') Lock.Enter After');

    try

      if not Accepted then
      begin
        Socket.Connect(NetEndpoint);
        FRemoteAddress:=Socket.RemoteAddress;
      end;

      TThread.Synchronize(SyncThread,

      procedure
      begin
        DoConnected;
        DoAfterConnect;
      end);

      while Connected do
      begin

        DoLog('TTCPSocket.DoConnect('+AName+') Wait receive data');

        {$IFDEF POSIX}
        if TSocketAccess(Socket).WaitForData(250)=wrTimeout then Continue;
        {$ELSE}
        TSocketAccess(Socket).WaitForData;
        {$ENDIF}

        if Lock.Terminated then Exit;

        if Connected then

        try

        if Socket.ReceiveLength=0 then
          Break
        else

        TThread.Synchronize(SyncThread,
        procedure
        begin
          DoReceived;
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

    if Lock.Terminated then Exit;

    DoLog('TTCPSocket.DoConnect('+AName+') Receive loop terminated');

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

    DoLog('TTCPSocket.DoConnect('+AName+') Lock.Leave');

    Lock.Leave;

  end);

end;

procedure TTCPSocket.DoLog(const S: string);
begin
  if Assigned(FOnLog) then FOnLog(S);
end;

procedure TTCPSocket.DoAfterConnect;
begin
  if Assigned(FOnAfterConnect) then FOnAfterConnect(Self);
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

    Run(

    procedure
    var Lock: ILock;
    begin

      Lock:=FLock;

      try

      while Connected do
      begin

        {$IFDEF POSIX}
        if TSocketAccess(Socket).WaitForData(250)=wrTimeout then Continue;
        {$ELSE}
        TSocketAccess(Socket).WaitForData;
        {$ENDIF}

        TThread.Synchronize(SyncThread,
        procedure
        begin
          if Connected then DoAccept;
        end);

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

function TTCPSocket.Accept: TSocket;
begin
  Result:=FSocket.Accept;
end;

end.
