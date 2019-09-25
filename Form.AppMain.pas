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
    procedure ComboBox1Change(Sender: TObject);
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

  HTTPSocket.Get(ComboBox1.Items[ComboBox1.ItemIndex]); // 'http://185.182.193.15/api/node/?identity=BFC9AA5719DE2F25E5E8A7FE5D21C95B');

//  HTTPSocket.Get('http://www.ancestryimages.com/stockimages/sm0112-Essex-Moule-l.jpg');
//  HTTPSocket.Get('http://65.99.251.252/stockimages/sm0112-Essex-Moule-l.jpg');

end;

procedure TForm12.Button5Click(Sender: TObject);
begin
  HTTPSocket.Disconnect;
end;

procedure TForm12.ComboBox1Change(Sender: TObject);
begin
  HTTPSocket.Disconnect;
end;

procedure TForm12.OnConnect(Sender: TObject);
begin
  SetConnect(True);
  ToLog('Connected to '+HTTPSocket.RemoteAddress);
end;

procedure TForm12.OnClose(Sender: TObject);
begin
  SetConnect(False);
  ToLog('Disconnected');
end;

procedure TForm12.OnExcept(Sender: TObject);
begin
  ToLog(HTTPSocket.E.Message);
end;

procedure TForm12.OnCompleted(Sender: TObject);
begin

  ToLog(HTTPSocket.Response.ResultCode.ToString+' '+HTTPSocket.Response.ResultText);
  ToLog(HTTPSocket.Response.Headers.Text+#13);
//  ToLog(HTTPSocket.Response.LocalResource);
//  ToLog(HTTPSocket.Response.ResourceName);

//  ToLog(TEncoding.ANSI.GetString(HTTPSocket.Response.Content));

//    TFile.WriteAllBytes('d:\121212.jpg',HTTPSocket.Response.Content);

  var S:=TBytesStream.Create(HTTPSocket.Response.Content);
  try
    Image1.Bitmap.LoadFromStream(S);
    TabControl1.ActiveTab:=TabItem2;
  finally
    S.Free;
  end;

end;

end.
