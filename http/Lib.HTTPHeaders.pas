unit Lib.HTTPHeaders;

interface

uses
  System.SysUtils,
  Lib.HTTPUtils;

type
  THeaders = class
  private type
    TLocation = record
      Pos: Integer;
      Len: Integer;
    end;
  private
    FData: string;
    function GetLocation(const Name: string): TLocation;
    procedure SetData(const Value: string);
  public
    constructor Create;
    procedure Assign(Source: TObject);
    procedure AddValue(const Name,Value: string);
    procedure SetValue(const Name,Value: string);
    function GetValue(const Name: string): string;
    procedure Clear;
    function FirstLine: string;
    procedure DeleteFirstLine;
    property Text: string read FData write SetData;
  public
    procedure SetConnection(KeepAlive: Boolean; Timeout: Integer);
    function ConnectionClose: Boolean;
    function ConnectionKeepAlive: Boolean;
    function KeepAliveTimeout: Integer;
    function ContentType: string;
    function ContentTypeCharset: string;
  end;

implementation

const
  CR = #13;
  LF = #10;
  CRLF = CR+LF;

constructor THeaders.Create;
begin
  Clear;
end;

procedure THeaders.Assign(Source: TObject);
begin
  if Source is THeaders then Text:=THeaders(Source).Text;
end;

procedure THeaders.Clear;
begin
  FData:='';
end;

procedure THeaders.SetData(const Value: string);
begin
  FData:=Value.Trim([CR,LF])+CRLF;
end;

function THeaders.GetLocation(const Name: string): TLocation;
var P,L: Integer;
begin

  P:=(CRLF+FData.ToLower).IndexOf(CRLF+Name.ToLower+':');

  L:=P;

  if P<>-1 then
  begin

    L:=(FData+CRLF).IndexOf(CRLF,P);

    while FData.Substring(L+Length(CRLF),1)=' ' do
      L:=(FData+CRLF).IndexOf(CRLF,L+Length(CRLF));

  end;

  Result.Pos:=P;
  Result.Len:=L-P;

end;

procedure THeaders.AddValue(const Name,Value: string);
begin
  FData:=FData+Name+': '+Value+CRLF;
end;

procedure THeaders.SetValue(const Name,Value: string);
var L: TLocation;
begin
  L:=GetLocation(Name);
  if L.Pos=-1 then
    AddValue(Name,Value)
  else
    FData:=FData.Remove(L.Pos,L.Len).Insert(L.Pos,Name+': '+Value);
end;

function THeaders.GetValue(const Name: string): string;
var L: TLocation;
begin
  L:=GetLocation(Name);
  if L.Pos=-1 then
    Result:=''
  else
    Result:=HTTPGetValue(FData.Substring(L.Pos,L.Len));
end;

function THeaders.FirstLine: string;
var P: Integer;
begin
  P:=FData.IndexOf(CRLF);
  if P=-1 then Result:=FData else Result:=FData.Substring(0,P);
end;

procedure THeaders.DeleteFirstLine;
var P: Integer;
begin
  P:=FData.IndexOf(CRLF);
  if P=-1 then FData:='' else FData:=FData.Remove(0,P+Length(CRLF));
end;

procedure THeaders.SetConnection(KeepAlive: Boolean; Timeout: Integer);
begin
  if KeepAlive then
  begin
    SetValue('Connection','keep-alive');
    if Timeout>0 then
      SetValue('Keep-Alive','timeout='+Timeout.ToString);
  end else
    SetValue('Connection','close');
end;

function THeaders.ConnectionClose: Boolean;
begin
  Result:=SameText(GetValue('Connection'),'close');
end;

function THeaders.ConnectionKeepAlive: Boolean;
begin
  Result:=SameText(GetValue('Connection'),'keep-alive');
end;

function THeaders.KeepAliveTimeout: Integer;
var TagTimeout: string;
begin
  Result:=0;
  TagTimeout:=HTTPGetTag(GetValue('Keep-Alive'),'timeout');
  if TagTimeout<>'' then
    Result:=StrToIntDef(HTTPGetTagValue(TagTimeout),0);
end;

function THeaders.ContentType: string;
var P: Integer;
begin
  Result:=GetValue('Content-Type');
  P:=Result.IndexOf(';');
  if P>-1 then Result:=Result.Substring(0,P).Trim;
end;

function THeaders.ContentTypeCharset: string;
begin
  Result:=
    HTTPGetTagValue(
    HTTPGetTag(
    GetValue('Content-Type'),'charset'));
end;

end.
