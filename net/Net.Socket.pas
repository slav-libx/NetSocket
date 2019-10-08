unit Net.Socket;

interface

uses
  System.Types,
  System.SysUtils,
  //System.RTLConsts,
  System.Classes,
  System.Threading,
  System.SyncObjs,
  System.Net.Socket,
  FMX.Types;

type

  TTCPSocket = class(TSocket)
  private
    FOnConnect: TNotifyEvent;
    FOnClose: TNotifyEvent;
    FOnReceived: TNotifyEvent;
    FOnExcept: TNotifyEvent;
    FException: Exception;
    C,P: TCriticalSection;
  protected
    function Connected: Boolean;
    procedure DoConnect; override;
    procedure DoAfterConnect; virtual;
    procedure DoConnected; virtual;
    procedure DoReceived; virtual;
    procedure DoClose;
    procedure DoExcept(E: Exception); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Connect(const Address: string; Port: Word); overload;
    procedure Disconnect;
    property E: Exception read FException;
    property OnConnect: TNotifyEvent read FOnConnect write FOnConnect;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnReceived: TNotifyEvent read FOnReceived write FOnReceived;
    property OnExcept: TNotifyEvent read FOnExcept write FOnExcept;
  end;

implementation

// https://docs.microsoft.com/ru-ru/dotnet/framework/network-programming/asynchronous-client-socket-example?view=netframework-4.8

constructor TTCPSocket.Create;
begin
  inherited Create(TSocketType.TCP);
  P:=TCriticalSection.Create;
  C:=TCriticalSection.Create;
end;

destructor TTCPSocket.Destroy;
begin
  P.Free;
  C.Free;
  inherited;
end;

procedure TTCPSocket.Disconnect;
begin
  if Connected then Close;
end;

function TTCPSocket.Connected: Boolean;
begin
  Result:=TSocketState.Connected in State;
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

    P.Enter;

    try

      NetEndpoint:=TNetEndpoint.Create(TIPAddress.Create(Address),Port);

      TThread.Synchronize(nil,

      procedure
      begin

        if Connected and CompareEndpoints(NetEndpoint,Endpoint) then
          DoAfterConnect
        else begin
          Disconnect;
          Connect(NetEndpoint);
        end

      end);

    except on E: Exception do

      DoExcept(E);

    end;

    P.Leave;

  end);

end;

procedure TTCPSocket.DoConnect;
begin

  TTask.Run(

  procedure
  var Lost: Boolean;
  begin

    Lost:=False;

    Log.d('C.Enter');
    //TMonitor.Enter(Self);
    C.Enter;

    Log.d('C.Entered');

    try

      inherited;

      TThread.Synchronize(nil,

      procedure
      begin
        DoConnected;
        DoAfterConnect;
      end);

      Log.d('Read loop');

      while not Lost and (WaitForData=wrSignaled) do

      TThread.Synchronize(nil,

      procedure
      begin
        if ReceiveLength>0 then
          DoReceived
        else begin
          Log.d('Lost');
          Disconnect;
          DoClose;
          Lost:=True;
          Log.d('Losted');
        end;
      end);

    except on E: Exception do

      DoExcept(E);

    end;

    Log.d('C.Leave');

    //TMonitor.Exit(Self);
    C.Leave;

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

  if Assigned(FOnExcept) then

  TThread.Synchronize(nil,

  procedure
  begin
    FException:=E;
    FOnExcept(Self);
    FException:=nil;
  end)

  else raise E;

end;

end.
