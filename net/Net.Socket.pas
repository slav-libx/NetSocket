unit Net.Socket;

interface

uses
  System.Types,
  System.SysUtils,
  System.RTLConsts,
  System.Classes,
  System.Threading,
  System.Net.Socket;

type

  TTCPSocket = class(TSocket)
  private
    FOnConnect: TNotifyEvent;
    FOnClose: TNotifyEvent;
    FOnReceived: TNotifyEvent;
    FOnExcept: TNotifyEvent;
    FException: Exception;
  protected
    function ConnectedSync: Boolean;
    function Connected: Boolean;
    procedure DoConnect; override;
    procedure DoAfterConnect; virtual;
    procedure DoConnected; virtual;
    procedure DoReceived; virtual;
    procedure DoClose;
    procedure DoExcept(E: Exception); virtual;
  public
    constructor Create; virtual;
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
end;

procedure TTCPSocket.Disconnect;
begin
  if Connected then Close;
end;

function TTCPSocket.ConnectedSync: Boolean;
var AConnected: Boolean;
begin

  TThread.Synchronize(nil,

  procedure
  begin
    AConnected:=Connected;
  end);

  Result:=AConnected;

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
        end;

      end);

    except on E: Exception do

      DoExcept(E);

    end;

  end);

end;

procedure TTCPSocket.DoConnect;
begin

  TTask.Run(

  procedure
  begin

    try

      inherited;

      TTask.Run(

      procedure
      var ConnectionLost: Boolean;
      begin

        try

          ConnectionLost:=False;

          while not ConnectionLost and ConnectedSync and (WaitForData=wrSignaled) do

          TThread.Synchronize(nil,

          procedure
          begin
            if ReceiveLength>0 then DoReceived else ConnectionLost:=True;
          end);

          TThread.Synchronize(nil,

          procedure
          begin
            DoClose;
          end);

        except on E: Exception do

          DoExcept(E);

        end;

      end);

      TThread.Synchronize(nil,

      procedure
      begin

        DoConnected;
        DoAfterConnect;

      end);

    except on E: Exception do

      DoExcept(E);

    end;

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

//  if Connected then Close;
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
