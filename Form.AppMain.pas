unit Form.AppMain;

interface

uses
  System.SysUtils,
  System.Types,
  System.UIConsts,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.IOUtils,
  System.Net.Socket,
  System.Generics.Collections,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Controls.Presentation,
  FMX.ScrollBox,
  FMX.Memo,
  FMX.StdCtrls,
  FMX.Objects,
  FMX.Layouts,
  FMX.ExtCtrls,
  FMX.TabControl,
  FMX.ListBox,
  Lib.HTTPConsts,
  Net.Socket,
  Net.HTTPSocket;

type
  TForm12 = class(TForm)
    Memo1: TMemo;
    Circle1: TCircle;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    ComboBox1: TComboBox;
    Image1: TImage;
    Layout1: TLayout;
    Splitter1: TSplitter;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    Layout2: TLayout;
    Memo2: TMemo;
    Layout3: TLayout;
    Button1: TButton;
    Circle2: TCircle;
    Button2: TButton;
    Button6: TButton;
    TabItem3: TTabItem;
    Layout4: TLayout;
    Button7: TButton;
    Circle3: TCircle;
    Button8: TButton;
    Button9: TButton;
    Memo3: TMemo;
    Label1: TLabel;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    TabItem4: TTabItem;
    Layout5: TLayout;
    Button10: TButton;
    Circle4: TCircle;
    Label2: TLabel;
    Button11: TButton;
    Button12: TButton;
    Memo4: TMemo;
    ComboBox4: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
  private
    HTTPClient: THTTPClient;
    FResponseIndex: Integer;
    procedure OnConnect(Sender: TObject);
    procedure OnClose(Sender: TObject);
    procedure OnExcept(Sender: TObject);
    procedure OnResponse(Sender: TObject);
    procedure SetConnect(Active: Boolean);
    procedure ShowBitmap;
    procedure HideBitmap;
  private
    TCPSocket: TTCPSocket;
    procedure OnTCPConnect(Sender: TObject);
    procedure OnTCPReceived(Sender: TObject);
    procedure OnTCPClose(Sender: TObject);
    procedure OnTCPExcept(Sender: TObject);
  private
    TCPServer: TTCPSocket;
    TCPClients: TObjectList<TTCPSocket>;
    procedure UpdateClients;
    procedure OnTCPServerListen(Sender: TObject);
    procedure OnTCPServerClose(Sender: TObject);
    procedure OnAccept(Sender: TObject);
    procedure OnClientReceived(Sender: TObject);
    procedure OnClientClose(Sender: TObject);
  private
    HTTPServer: TTCPSocket;
    HTTPClients: TObjectList<THTTPServerClient>;
    procedure UpdateHTTPClients;
    procedure OnHTTPExcept(Sender: TObject);
    procedure OnHTTPAccept(Sender: TObject);
    procedure OnHTTPRequest(Sender: TObject);
    procedure OnHTTPClientClose(Sender: TObject);
  public
  end;

var
  Form12: TForm12;

implementation

{$R *.fmx}

const HTTP_PORT = {$IFDEF ANDROID}8080{$ELSE}80{$ENDIF};

procedure ScrollToBottom(Memo: TMemo);
begin
  Memo.ScrollBy(0,Memo.ContentBounds.Height-Memo.ViewportPosition.Y);
end;

procedure ToMemo(Memo: TMemo; const Message: string);
begin
  if not Application.Terminated then
  begin
    Memo.Lines.Add(Message);
    ScrollToBottom(Memo);
  end;
end;

procedure TForm12.FormCreate(Sender: TObject);
begin

  Button3Click(nil);

  Circle1.Fill.Color:=claRed;
  Circle2.Fill.Color:=claRed;
  Circle3.Fill.Color:=claRed;
  Circle4.Fill.Color:=claRed;

  TCPSocket:=TTCPSocket.Create;

  TCPSocket.OnConnect:=OnTCPConnect;
  TCPSocket.OnReceived:=OnTCPReceived;
  TCPSocket.OnClose:=OnTCPClose;
  TCPSocket.OnExcept:=OnTCPExcept;

  HTTPClient:=THTTPClient.Create;

  HTTPClient.OnConnect:=OnConnect;
  HTTPClient.OnClose:=OnClose;
  HTTPClient.OnExcept:=OnExcept;
  HTTPClient.OnResponse:=OnResponse;

  TCPServer:=TTCPSocket.Create;
  TCPServer.OnConnect:=OnTCPServerListen;
  TCPServer.OnClose:=OnTCPServerClose;
  TCPServer.OnExcept:=OnHTTPExcept;
  TCPServer.OnAccept:=OnAccept;

  TCPClients:=TObjectList<TTCPSocket>.Create;

  UpdateClients;

  HTTPServer:=TTCPSocket.Create;
  HTTPServer.OnAccept:=OnHTTPAccept;

  HTTPClients:=TObjectList<THTTPServerClient>.Create;

  UpdateHTTPClients;

  ComboBox1.Items.Add('http://185.182.193.15/api/node/?identity=BFC9AA5719DE2F25E5E8A7FE5D21C95B');
  ComboBox1.Items.Add('http://www.ancestryimages.com/stockimages/sm0112-Essex-Moule-l.jpg');
  ComboBox1.Items.Add('http://www.ancestryimages.com/stockimages/sm0004-WorldKitchin1777.jpg');
  ComboBox1.Items.Add('http://www.picshare.ru/images/upload_but.png');
  ComboBox1.Items.Add('http://krasivie-kartinki.ru/images/dragocennosti_25_small.jpg');
  ComboBox1.Items.Add('http://i.artfile.ru/1366x768_1477274_[www.ArtFile.ru].jpg');
  ComboBox1.Items.Add('http://zagony.ru/admin_new/foto/2012-4-23/1335176695/chastnye_fotografii_devushek_100_foto_31.jpg');
  ComboBox1.Items.Add('http://localhost/2.jpg');
  ComboBox1.Items.Add('http://localhost:'+HTTP_PORT.ToString+'/2.jpg');
  ComboBox1.Items.Add('http://localhost:'+HTTP_PORT.ToString+'/9.jpg');
  ComboBox1.Items.Add('http://history-maps.ru/pictures/max/0/1764.jpg');
  ComboBox1.Items.Add('http://zagony.ru/admin_new/foto/2019-9-23/1569240641/festival_piva_oktoberfest2019_v_mjunkhene_22_foto_14.jpg');
  ComboBox1.Items.Add('');
  ComboBox1.Items.Add('');
  ComboBox1.Items.Add('');
  ComboBox1.Items.Add('');

  ComboBox1.ItemIndex:=2;

  ComboBox2.Items.Add('185.182.193.15:5555');
  ComboBox2.Items.Add('localhost:5555');

  ComboBox2.ItemIndex:=0;

  ComboBox3.Items.Add('5555');
  ComboBox3.Items.Add('8080');

  ComboBox3.ItemIndex:=0;

end;

procedure TForm12.FormDestroy(Sender: TObject);
begin
  TCPClients.Free;
  TCPSocket.Free;
  HTTPClient.Free;
  TCPServer.Free;
  HTTPServer.Free;
  HTTPClients.Free;
end;

procedure TForm12.ShowBitmap;
begin
  Image1.Visible:=True;
  Splitter1.Visible:=True;
  ScrollToBottom(Memo1);
end;

procedure TForm12.HideBitmap;
begin
  Splitter1.Visible:=False;
  Image1.Bitmap.Assign(nil);
  Image1.Visible:=False;
  ScrollToBottom(Memo1);
end;

procedure TForm12.SetConnect(Active: Boolean);
begin
  if not Application.Terminated then
  if Active then
  begin
    Circle1.Fill.Color:=claGreen;
    ToMemo(Memo1,'Connected ['+HTTPClient.Handle.ToString+'] to '+HTTPClient.Address);
  end else begin
    Circle1.Fill.Color:=claRed;
    ToMemo(Memo1,'Disconnected');
  end;
end;

procedure TForm12.Button3Click(Sender: TObject);
begin
  FResponseIndex:=0;
  Memo1.Lines.Clear;
  HideBitmap;
end;

procedure TForm12.Button4Click(Sender: TObject);
begin
  Image1.Bitmap.Assign(nil);
  HTTPClient.Get(ComboBox1.Items[ComboBox1.ItemIndex]);
  //HTTPSocket.Get(ComboBox1.Items[ComboBox1.ItemIndex]);
end;

procedure TForm12.Button5Click(Sender: TObject);
begin
  HTTPClient.Disconnect;
end;

procedure TForm12.OnConnect(Sender: TObject);
begin
  SetConnect(True);
end;

procedure TForm12.OnClose(Sender: TObject);
begin
  SetConnect(False);
end;

procedure TForm12.OnExcept(Sender: TObject);
begin
  ToMemo(Memo1,HTTPClient.E.Message);
end;

procedure TForm12.OnResponse(Sender: TObject);
begin

  Inc(FResponseIndex);

  ToMemo(Memo1,'---'+FResponseIndex.ToString+'---');
  ToMemo(Memo1,HTTPClient.Response.ResultCode.ToString+' '+HTTPClient.Response.ResultText);
  ToMemo(Memo1,HTTPClient.Response.Headers.Text);

  var ContentType:=HTTPClient.Response.Headers.ContentType;

  if ContentType.StartsWith('image') then
  begin

    var Stream:=TBytesStream.Create(HTTPClient.Response.Content);

    try
      Image1.Bitmap.LoadFromStream(Stream);
      ShowBitmap;
    finally
      Stream.Free;
    end;

  end else begin

    HideBitmap;

    if ContentType.StartsWith('text') or ContentType.EndsWith('json') then

      ToMemo(Memo1,TEncoding.ANSI.GetString(HTTPClient.Response.Content));

  end;

end;

// TCP Client

procedure TForm12.Button1Click(Sender: TObject);
begin
  TCPSocket.Connect(ComboBox2.Items[ComboBox2.ItemIndex]);
end;

procedure TForm12.OnTCPConnect(Sender: TObject);
begin
  Circle2.Fill.Color:=claGreen;
  ToMemo(Memo2,'Connected to '+TCPSocket.RemoteAddress);
end;

procedure TForm12.OnTCPReceived(Sender: TObject);
begin
  ToMemo(Memo2,TCPSocket.ReceiveString);
end;

procedure TForm12.OnTCPClose(Sender: TObject);
begin
  if not Application.Terminated then
  begin
    Circle2.Fill.Color:=claRed;
    ToMemo(Memo2,'Disconnected');
  end;
end;

procedure TForm12.OnTCPExcept(Sender: TObject);
begin
  ToMemo(Memo2,TCPSocket.E.Message);
end;

procedure TForm12.Button2Click(Sender: TObject);
begin
  TCPSocket.Disconnect;
end;

procedure TForm12.Button6Click(Sender: TObject);
begin
  Memo2.Lines.Clear;
end;

// TCP Server

procedure TForm12.UpdateClients;
begin
  Label1.Text:=TCPClients.Count.ToString;
end;

procedure TForm12.Button7Click(Sender: TObject);
begin
  TCPServer.Start(StrToInt(ComboBox3.Items[ComboBox3.ItemIndex]));
end;

procedure TForm12.Button8Click(Sender: TObject);
begin
  TCPServer.Disconnect;
end;

procedure TForm12.Button9Click(Sender: TObject);
begin
  Memo3.Lines.Clear;
end;

procedure TForm12.OnTCPServerListen(Sender: TObject);
begin
  if not Application.Terminated then
  Circle3.Fill.Color:=claGreen;
end;

procedure TForm12.OnTCPServerClose(Sender: TObject);
begin
  if not Application.Terminated then
  Circle3.Fill.Color:=claRed;
end;

procedure TForm12.OnAccept(Sender: TObject);
var Client: TTCPSocket;
begin
  Client:=TTCPSocket.Create(TCPServer.GetAcceptSocket);
  Client.OnReceived:=OnClientReceived;
  Client.OnClose:=OnClientClose;
  Client.Connect;
  TCPClients.Add(Client);
  UpdateClients;
  ToMemo(Memo3,'Connected RemoteAddress: '+Client.RemoteAddress);
end;

procedure TForm12.OnClientReceived(Sender: TObject);
begin
  ToMemo(Memo3,TTCPSocket(Sender).ReceiveString);
end;

procedure TForm12.OnClientClose(Sender: TObject);
begin
  TCPClients.Remove(TTCPSocket(Sender));
  UpdateClients;
  ToMemo(Memo3,'Disconnected');
end;

// HTTP Server

procedure TForm12.UpdateHTTPClients;
begin
  if not Application.Terminated then
  Label2.Text:=HTTPClients.Count.ToString;
end;

procedure TForm12.Button10Click(Sender: TObject);
begin
  HTTPServer.Start(HTTP_PORT);
  Circle4.Fill.Color:=claGreen;
end;

procedure TForm12.Button11Click(Sender: TObject);
begin
  HTTPServer.Disconnect;
  Circle4.Fill.Color:=claRed;
end;

procedure TForm12.Button12Click(Sender: TObject);
begin
  Memo4.Lines.Clear;
end;

procedure TForm12.OnHTTPAccept(Sender: TObject);
var Client: THTTPServerClient;
begin
  Client:=THTTPServerClient.Create(HTTPServer.GetAcceptSocket);
  Client.OnClose:=OnHTTPClientClose;
  Client.OnRequest:=OnHTTPRequest;
  Client.OnExcept:=OnHTTPExcept;
  Client.Connect;
  HTTPClients.Add(Client);
  UpdateHTTPClients;
  ToMemo(Memo4,'Connected RemoteAddress: '+Client.RemoteAddress);
end;

procedure TForm12.OnHTTPRequest(Sender: TObject);
var C: THTTPServerClient;
begin

  C:=THTTPServerClient(Sender);

  ToMemo(Memo4,C.Request.Method+' '+C.Request.Resource+' '+C.Request.Protocol);
  ToMemo(Memo4,C.Request.Headers.Text);

  C.Response.SetResult(HTTPCODE_NOT_FOUND,'Not Found');
  C.Response.AddContentText(content_404,'text/html');

end;

procedure TForm12.OnHTTPClientClose(Sender: TObject);
begin
  HTTPClients.Remove(THTTPServerClient(Sender));
  UpdateHTTPClients;
  ToMemo(Memo4,'Disconnected');
end;

procedure TForm12.OnHTTPExcept(Sender: TObject);
begin
  ToMemo(Memo4,TCPSocket.E.Message);
end;

end.
