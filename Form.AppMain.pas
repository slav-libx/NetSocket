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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
  private
    HTTPSocket: THTTPSocket;
    FResponseIndex: Integer;
    procedure OnConnect(Sender: TObject);
    procedure OnClose(Sender: TObject);
    procedure OnExcept(Sender: TObject);
    procedure OnCompleted(Sender: TObject);
    procedure SetConnect(Active: Boolean);
    procedure ToLog(const Message: string);
    procedure ScrollLogToBottom;
    procedure ShowBitmap;
    procedure HideBitmap;
  private
    TCPSocket: TTCPSocket;
    procedure OnTCPConnect(Sender: TObject);
    procedure OnTCPReceived(Sender: TObject);
    procedure OnTCPClose(Sender: TObject);
    procedure OnTCPExcept(Sender: TObject);
    procedure ToTCPLog(const Message: string);
    procedure ScrollTCPLogToBottom;
  public
  end;

var
  Form12: TForm12;

implementation

{$R *.fmx}

procedure TForm12.FormCreate(Sender: TObject);
begin

  Button3Click(nil);

  Circle1.Fill.Color:=claRed;

//  TCPSocket:=TTCPSocket.Create;
//
//  TCPSocket.Encoding:=TEncoding.ANSI;
//
//  TCPSocket.OnConnect:=OnTCPConnect;
//  TCPSocket.OnReceived:=OnTCPReceived;
//  TCPSocket.OnClose:=OnTCPClose;
//  TCPSocket.OnExcept:=OnTCPExcept;

  HTTPSocket:=THTTPSocket.Create;

  HTTPSocket.OnConnect:=OnConnect;
  HTTPSocket.OnClose:=OnClose;
  HTTPSocket.OnExcept:=OnExcept;
  HTTPSocket.OnCompleted:=OnCompleted;

  ComboBox1.Items.Add('http://185.182.193.15/api/node/?identity=BFC9AA5719DE2F25E5E8A7FE5D21C95B');
  ComboBox1.Items.Add('http://www.ancestryimages.com/stockimages/sm0112-Essex-Moule-l.jpg');
  ComboBox1.Items.Add('http://www.ancestryimages.com/stockimages/sm0004-WorldKitchin1777.jpg');
  ComboBox1.Items.Add('http://www.picshare.ru/images/upload_but.png');
  ComboBox1.Items.Add('http://krasivie-kartinki.ru/images/dragocennosti_25_small.jpg');
  ComboBox1.Items.Add('http://i.artfile.ru/1366x768_1477274_[www.ArtFile.ru].jpg');
  ComboBox1.Items.Add('http://zagony.ru/admin_new/foto/2012-4-23/1335176695/chastnye_fotografii_devushek_100_foto_31.jpg');
  ComboBox1.Items.Add('http://localhost/2.jpg');
  ComboBox1.Items.Add('http://localhost/9.jpg');
  ComboBox1.Items.Add('http://history-maps.ru/pictures/max/0/1764.jpg');
  ComboBox1.Items.Add('http://zagony.ru/admin_new/foto/2019-9-23/1569240641/festival_piva_oktoberfest2019_v_mjunkhene_22_foto_14.jpg');
  ComboBox1.Items.Add('');
  ComboBox1.Items.Add('');
  ComboBox1.Items.Add('');
  ComboBox1.Items.Add('');

  ComboBox1.ItemIndex:=2;

end;

procedure TForm12.FormDestroy(Sender: TObject);
begin
//  TCPSocket.Free;
  HTTPSocket.Free;
end;

procedure TForm12.ScrollLogToBottom;
begin
  Memo1.ScrollBy(0,Memo1.ContentBounds.Height-Memo1.ViewportPosition.Y);
end;

procedure TForm12.ToLog(const Message: string);
begin
  if not Application.Terminated then
  begin
    Memo1.Lines.Add(Message);
    ScrollLogToBottom;
  end;
end;

procedure TForm12.ShowBitmap;
begin
  Image1.Visible:=True;
  Splitter1.Visible:=True;
  ScrollLogToBottom;
end;

procedure TForm12.HideBitmap;
begin
  Splitter1.Visible:=False;
  Image1.Bitmap.Assign(nil);
  Image1.Visible:=False;
  ScrollLogToBottom;
end;

procedure TForm12.SetConnect(Active: Boolean);
begin
  if not Application.Terminated then
  if Active then
  begin
    Circle1.Fill.Color:=claGreen;
    ToLog('Connected ['+HTTPSocket.Handle.ToString+'] to '+HTTPSocket.Endpoint.Address.Address);
  end else begin
    Circle1.Fill.Color:=claRed;
    ToLog('Disconnected');
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
  HTTPSocket.Get(ComboBox1.Items[ComboBox1.ItemIndex]);
  //HTTPSocket.Get(ComboBox1.Items[ComboBox1.ItemIndex]);
end;

procedure TForm12.Button5Click(Sender: TObject);
begin
  HTTPSocket.Disconnect;
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
  ToLog(HTTPSocket.E.Message);
end;

procedure TForm12.OnCompleted(Sender: TObject);
begin

  Inc(FResponseIndex);

  ToLog('---'+FResponseIndex.ToString+'---');
  ToLog(HTTPSocket.Response.ResultCode.ToString+' '+HTTPSocket.Response.ResultText);
  ToLog(HTTPSocket.Response.Headers.Text);

  var ContentType:=HTTPSocket.Response.Headers.ContentType;

  if ContentType.StartsWith('image') then
  begin

    var Stream:=TBytesStream.Create(HTTPSocket.Response.Content);

    try
      Image1.Bitmap.LoadFromStream(Stream);
      ShowBitmap;
    finally
      Stream.Free;
    end;

  end else begin

    HideBitmap;

    if ContentType.StartsWith('text') or ContentType.EndsWith('json') then

      ToLog(TEncoding.ANSI.GetString(HTTPSocket.Response.Content));

  end;

end;

// TCP

procedure TForm12.ToTCPLog(const Message: string);
begin
  if not Application.Terminated then
  begin
    Memo2.Lines.Add(Message);
    ScrollTCPLogToBottom;
  end;
end;

procedure TForm12.ScrollTCPLogToBottom;
begin
  Memo2.ScrollBy(0,Memo2.ContentBounds.Height-Memo2.ViewportPosition.Y);
end;

procedure TForm12.Button1Click(Sender: TObject);
begin
  TCPSocket.Connect('185.182.193.15',5555);
end;

procedure TForm12.OnTCPConnect(Sender: TObject);
begin
  Circle2.Fill.Color:=claGreen;
  ToTCPLog('Connected to '+TCPSocket.RemoteAddress+#13#10);
end;

procedure TForm12.OnTCPReceived(Sender: TObject);
begin
  ToTCPLog(TCPSocket.ReceiveString+#13#10);
end;

procedure TForm12.OnTCPClose(Sender: TObject);
begin
  if not Application.Terminated then
  begin
    Circle2.Fill.Color:=claRed;
    ToTCPLog('Disconnected'#13#10);
  end;
end;

procedure TForm12.OnTCPExcept(Sender: TObject);
begin
  ToTCPLog(TCPSocket.E.Message+#13#10);
end;

procedure TForm12.Button2Click(Sender: TObject);
begin
  TCPSocket.Disconnect;
end;

procedure TForm12.Button6Click(Sender: TObject);
begin
  Memo2.Lines.Clear;
end;

end.
