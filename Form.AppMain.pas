unit Form.AppMain;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  System.Net.Socket;

type
  TForm12 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    Socket: TSocket;
  public
    { Public declarations }
  end;

var
  Form12: TForm12;

implementation

{$R *.fmx}

// https://docs.microsoft.com/ru-ru/dotnet/framework/network-programming/asynchronous-client-socket-example?view=netframework-4.8

procedure TForm12.FormCreate(Sender: TObject);
var
LRecesiv: TBytes;
S: string;
begin

  Socket:=TSocket.Create(TSocketType.TCP);

  Socket.Connect(TNetEndpoint.Create(185,182,193,15,80));

  Socket.Send(
    'GET /api/transactions/2000 HTTP/1.1'#13#10+
    'Host: 185.182.193.15'#13#10+
    'Connection: keep-alive'#13#10+
    #13#10);

  Socket.BeginReceive(
  procedure (const ASyncResult: IAsyncResult)
  begin
    //ASyncResult.AsyncContext;

//    TSocket(ASyncResult.AsyncContext).Receive(LRecesiv);

  Socket.Receive(LRecesiv);

  S:=TEncoding.ANSI.GetString(LRecesiv);

  end);


//  LRecesiv);
//
//  Socket.Receive(LRecesiv);
//
//  S:=TEncoding.ANSI.GetString(LRecesiv);



//    );
//
//
//GET /900.jpg HTTP/1.1
//Host: localhost
//User-Agent: Mozilla/5.0 (Windows NT 6.1; rv:61.0) Gecko/20100101 Firefox/61.0
//Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8
//Accept-Language: ru-RU,ru;q=0.8,en-US;q=0.5,en;q=0.3
//Accept-Encoding: gzip, deflate
//Connection: keep-alive
//Upgrade-Insecure-Requests: 1

//LBytes := [233, 123, 001, $FF];
//    socket.Send﻿(LBytes);

//    while True do //
//    Begin
//      if socket.Receive(LRecesiv) = 0 then
//        Continue; // если ничего не пришло - опять запрашиваем инфу
////      if LRecesiv = LBytes then
////        socket.Send([66, 124]);
//    End;

end;

procedure TForm12.FormDestroy(Sender: TObject);
begin
  Socket.Free;
end;

end.
