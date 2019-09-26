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
  Net.HTTPSocket;

type
  TForm12 = class(TForm)
    Memo1: TMemo;
    Circle1: TCircle;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    ComboBox1: TComboBox;
    Image1: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
    HTTPSocket: THTTPSocket;
    procedure OnConnect(Sender: TObject);
    procedure OnClose(Sender: TObject);
    procedure OnExcept(Sender: TObject);
    procedure OnCompleted(Sender: TObject);
    procedure SetConnect(Active: Boolean);
    procedure ToLog(const Message: string);
  public
  end;

var
  Form12: TForm12;

implementation

{$R *.fmx}

procedure TForm12.FormCreate(Sender: TObject);
begin

  SetConnect(False);

  HTTPSocket:=THTTPSocket.Create;

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

  ComboBox1.ItemIndex:=2;

end;

procedure TForm12.FormDestroy(Sender: TObject);
begin
  HTTPSocket.Free;
end;

procedure TForm12.ToLog(const Message: string);
begin
  if not Application.Terminated then
    Memo1.Lines.Add(Message);
end;

procedure TForm12.SetConnect(Active: Boolean);
begin
  if not Application.Terminated then
  if Active then
    Circle1.Fill.Color:=claGreen
  else
    Circle1.Fill.Color:=claRed;
end;

procedure TForm12.Button3Click(Sender: TObject);
begin
  Memo1.Lines.Clear;
  Image1.Bitmap.Assign(nil);
end;

procedure TForm12.Button4Click(Sender: TObject);
begin

  Image1.Bitmap.Assign(nil);
  TabControl1.ActiveTab:=TabItem1;

  HTTPSocket.Get(ComboBox1.Items[ComboBox1.ItemIndex]);

end;

procedure TForm12.Button5Click(Sender: TObject);
begin
  HTTPSocket.Disconnect;
end;

procedure TForm12.OnConnect(Sender: TObject);
begin
  SetConnect(True);
  ToLog('Connected to '+HTTPSocket.RemoteAddress+#13);
end;

procedure TForm12.OnClose(Sender: TObject);
begin
  SetConnect(False);
  ToLog('Disconnected'#13);
end;

procedure TForm12.OnExcept(Sender: TObject);
begin
  ToLog(HTTPSocket.E.Message);
end;

procedure TForm12.OnCompleted(Sender: TObject);
begin

  ToLog(HTTPSocket.Response.ResultCode.ToString+' '+HTTPSocket.Response.ResultText);
  ToLog(HTTPSocket.Response.Headers.Text);

  var ContentType:=HTTPSocket.Response.Headers.ContentType;

  if ContentType.StartsWith('image') then
  begin

    var Stream:=TBytesStream.Create(HTTPSocket.Response.Content);

    try
      Image1.Bitmap.LoadFromStream(Stream);
      TabControl1.ActiveTab:=TabItem2;
    finally
      Stream.Free;
    end;

  end else

  if ContentType.StartsWith('text') or ContentType.EndsWith('json') then

    ToLog(TEncoding.ANSI.GetString(HTTPSocket.Response.Content));

end;

end.
