unit Net.Socket;

interface

uses
  System.Types,
  System.SysUtils,
  System.Classes,
  System.Threading,
  System.Net.Socket;

type

  TTCPSocket = class;

  TReceivedEvent = procedure (Sender: TObject; const Bytes: TBytes) of object;
  TExceptEvent = procedure (Sender: TObject; E: Exception) of object;

  TTCPSocket = class(TSocket)
  private
    FOnConnect: TNotifyEvent;
    FOnClose: TNotifyEvent;
    FOnReceived: TReceivedEvent;
    FOnExcept: TExceptEvent;
    ReceivedBytes: TBytes;
  protected
    procedure DoConnect; override;
    procedure DoExcept(E: Exception);
  public
    constructor Create;
    property OnConnect: TNotifyEvent read FOnConnect write FOnConnect;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnReceived: TReceivedEvent read FOnReceived write FOnReceived;
    property OnExcept: TExceptEvent read FOnExcept write FOnExcept;
  end;

implementation

constructor TTCPSocket.Create;
begin
  inherited Create(TSocketType.TCP);
end;

procedure TTCPSocket.DoConnect;
begin

  TTask.Run(
  procedure
  begin

    try

      inherited;

      if Assigned(FOnConnect) then

      TThread.Synchronize(nil,
      procedure
      begin
        FOnConnect(Self);
      end);

      TTask.Run(

      procedure
      begin

        while (TSocketState.Connected in State) and (WaitForData=wrSignaled) do

        try

          TThread.Synchronize(nil,
          procedure
          begin

            Receive(ReceivedBytes);

            if Assigned(FOnReceived) then FOnReceived(Self,ReceivedBytes);

          end);

          if Length(ReceivedBytes)=0 then Break;

        except
        on E: Exception do DoExcept(E);
        end;

        if TSocketState.Connected in State then
          Close;

        if Assigned(FOnClose) then

        TThread.Synchronize(nil,
        procedure
        begin
          FOnClose(Self);
        end);

      end);

    except
    on E: Exception do DoExcept(E);
    end;

  end);

end;

procedure TTCPSocket.DoExcept(E: Exception);
begin

  if Assigned(FOnExcept) then

  TThread.Synchronize(nil,
  procedure
  begin
    FOnExcept(Self,E);
  end)

  else raise E;

end;

end.
