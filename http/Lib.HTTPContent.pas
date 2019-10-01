unit Lib.HTTPContent;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  Lib.HTTPConsts,
  Lib.HTTPUtils,
  Lib.HTTPHeaders;

type
  TContent = class abstract
  protected type
    TState = (stNone,stHeader,stContentLength,stChunked,stUnknownLength);
  private
    FState: TState;
    FHeaderLength: Integer;
    FContentLength: Integer;
    FContentReaded: Integer;
    FContentType: string;
    FHeaders: THeaders;
    FOnHeader: TNotifyEvent;
    FOnContent: TNotifyEvent;
    FOnComplete: TNotifyEvent;
    procedure DoBeginRead;
  protected
    FComposes: string;
    procedure DoHeader; virtual;
    procedure DoContent; virtual;
    procedure DoComplete; virtual;
    procedure ComposeHeaders;
  public
    Protocol: string;
    Content: TBytes;
    ResourceName: string;
    LocalResource: string;
    Description: string;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure AddContentText(const Text: string); overload;
    procedure AddContentText(const Text,ContentType: string); overload;
    procedure AddContentFile(const FileName: string); overload;
    procedure AddContentFile(const FileName,ContentType: string); overload;
    property Headers: THeaders read FHeaders;
  public
    procedure Reset; virtual;
    procedure Assign(Source: TContent); virtual;
    function DoRead(const B: TBytes): Integer;
    property Composes: string read FComposes;
  public
    property ContentLength: Integer read FContentLength;
    property ContentReaded: Integer read FContentReaded;
    property OnReadHeader: TNotifyEvent write FOnHeader;
    property OnReadContent: TNotifyEvent write FOnContent;
    property OnReadComplete: TNotifyEvent write FOnComplete;
  end;

  TRequest = class(TContent)
  protected
    procedure DoHeader; override;
  public
    Host: string;
    Scheme: string;
    Method: string;
    Resource: string;
    Query: string;
    Fragment: string;
    procedure Reset; override;
    procedure Assign(Source: TContent); override;
    procedure DecomposeURL(const URL: string);
    function Compose: string;
    procedure Merge;
  end;

  TResponse = class(TContent)
  protected
    procedure DoHeader; override;
  public
    ResultCode: Integer;
    ResultText: string;
    procedure Reset; override;
    procedure Assign(Source: TContent); override;
    function Compose: string;
    procedure SetResult(Code: Integer; const Text: string);
    procedure Merge(Request: TRequest);
  end;

implementation

function ContentSizeToString(const Content: TBytes): string;
begin
  Result:=Length(Content).ToString+' bytes';
end;

constructor TContent.Create;
begin
  FHeaders:=THeaders.Create;
  Reset;
end;

destructor TContent.Destroy;
begin
  FHeaders.Free;
  Content:=nil;
  inherited;
end;

procedure TContent.Reset;
begin
  FState:=stNone;
  FHeaderLength:=0;
  FContentReaded:=0;
  FContentLength:=0;
  FContentType:='';
  FComposes:='';
  FHeaders.Clear;
  Content:=nil;
  Protocol:='';
  ResourceName:='';
  LocalResource:='';
  Description:='';
end;

procedure TContent.Assign(Source: TContent);
begin
  Headers.Assign(Source.Headers);
  FContentType:=Source.FContentType;
  Content:=Source.Content;
  Protocol:=Source.Protocol;
  ResourceName:=Source.ResourceName;
  LocalResource:=Source.LocalResource;
  Description:=Source.Description;
end;

procedure TContent.AddContentText(const Text: string);
begin
  AddContentText(Text,HTTPGetMIMEType('.txt'));
end;

procedure TContent.AddContentText(const Text,ContentType: string);
begin
  Content:=TEncoding.UTF8.GetBytes(Text);
  FContentType:=ContentType+'; charset=utf-8';
  Description:=Text;
end;

procedure TContent.AddContentFile(const FileName: string);
begin
  AddContentFile(FileName,HTTPGetMIMEType(ExtractFileExt(FileName)));
end;

procedure TContent.AddContentFile(const FileName,ContentType: string);
begin
  Content:=TFile.ReadAllBytes(FileName);
  FContentType:=ContentType;
  Description:=FileName+' ('+ContentSizeToString(Content)+')';
end;

procedure TContent.DoBeginRead;
begin
  Reset;
  FState:=stHeader;
end;

function TContent.DoRead(const B: TBytes): Integer;
var L: Integer; Over: TBytes;
begin

  Over:=nil;

  Result:=Length(B);

  if FState<>stUnknownLength then
  begin

    if Result=0 then Exit;

    if FState=stNone then DoBeginRead; // начало чтения запроса (ответа)

  end;

  L:=Result;

  SetLength(Content,FContentReaded+L);
  Move(B[0],Content[FContentReaded],L);
  Inc(FContentReaded,L);

  if FState=stHeader then
  begin

    FHeaderLength:=HTTPGetHeaderLength(Content);

    if FHeaderLength>0 then
    begin

      Headers.Text:=TEncoding.ANSI.GetString(Content,0,FHeaderLength);

      DoHeader;

      L:=FContentReaded-FHeaderLength-4;

      Move(Content[FContentReaded-L],Content[0],L);
      SetLength(Content,L);
      FContentReaded:=L;

      if TryStrToInt(Headers.GetValue('Content-Length'),FContentLength) then
        FState:=stContentLength
      else
      if Headers.GetValue('Transfer-Encoding')='chunked' then
        FState:=stChunked
      else
        FState:=stUnknownLength;

    end;

  end;

  if FState=stContentLength then
  begin

    if FContentReaded>=FContentLength then
    begin

      if FContentReaded>FContentLength then
      begin
        Over:=Copy(Content,FContentLength,FContentReaded-FContentLength);
        SetLength(Content,FContentLength);
      end;

      DoContent;
      FState:=stNone;

    end;

  end;

  if FState=stChunked then
  begin

    if HTTPEndedChunked(Content) then
    begin
      Content:=HTTPBytesFromChunked(Content);
      DoContent;
      FState:=stNone;
    end;

  end;

  if FState=stUnknownLength then
  begin

    if L=0 then
    begin
      DoContent;
      FState:=stNone;
    end;

  end;

  if FState=stNone then DoComplete;

  DoRead(Over);

end;

procedure TContent.DoHeader;
begin
  if Assigned(FOnHeader) then FOnHeader(Self);
end;

procedure TContent.DoContent;
begin
  if Assigned(FOnContent) then FOnContent(Self);
end;

procedure TContent.DoComplete;
begin
  if Assigned(FOnComplete) then FOnComplete(Self);
end;

procedure TContent.ComposeHeaders;
begin

  if Length(Content)>0 then
  begin
    if FContentType<>'' then
      Headers.SetValue('Content-Type',FContentType);
    Headers.SetValue('Content-Length',Length(Content).ToString);
  end;

end;

{ TRequest }

procedure TRequest.Reset;
begin
  inherited;
  Host:='';
  Scheme:='';
  Method:='';
  Resource:='';
  Query:='';
  Fragment:='';
end;

procedure TRequest.Assign(Source: TContent);
var S: TRequest;
begin
  inherited;
  if Source is TRequest then
  begin
    S:=TRequest(Source);
    Host:=S.Host;
    Scheme:=S.Scheme;
    Method:=S.Method;
    Resource:=S.Resource;
    Query:=S.Query;
    Fragment:=S.Fragment
  end;
end;

procedure TRequest.DecomposeURL(const URL: string);
begin
  HTTPSplitURL(URL,Scheme,Host,Resource);
end;

function TRequest.Compose: string;
begin
  ComposeHeaders;
  FComposes:=Method+' '+HTTPEncodeResource(Resource)+' '+Protocol+CRLF+
    Headers.Text;
  Result:=FComposes+CRLF;
end;

procedure TRequest.DoHeader;
begin

  FComposes:=Headers.Text;

  if HTTPTrySplitRequest(Headers.FirstLine,Method,Resource,Protocol) then
  begin

    Headers.DeleteFirstLine;

  end;

  inherited;

end;

procedure TRequest.Merge;
begin
  ResourceName:=HTTPExtractResourceName(HTTPDecodeResource(Resource));
  Description:=Headers.ContentType+' ('+ContentSizeToString(Content)+')';
end;

{ TResponse }

procedure TResponse.Reset;
begin
  inherited;
  Protocol:='';
  ResultCode:=0;
  ResultText:='';
end;

procedure TResponse.Assign(Source: TContent);
var S: TResponse;
begin
  inherited;
  if Source is TResponse then
  begin
    S:=TResponse(Source);
    Protocol:=S.Protocol;
    ResultCode:=S.ResultCode;
    ResultText:=S.ResultText;
  end;
end;

procedure TResponse.SetResult(Code: Integer; const Text: string);
begin
  ResultCode:=Code;
  ResultText:=Text;
end;

function TResponse.Compose: string;
begin
  ComposeHeaders;
  FComposes:=Protocol+' '+ResultCode.ToString+' '+ResultText+CRLF+
    Headers.Text;
  Result:=FComposes+CRLF;
end;

procedure TResponse.DoHeader;
begin

  FComposes:=Headers.Text;

  if HTTPTrySplitResponseResult(Headers.FirstLine,Protocol,ResultCode,ResultText) then
  begin

    Headers.DeleteFirstLine;

  end;

  inherited;
end;

procedure TResponse.Merge(Request: TRequest);
var ContentDispositionFileName: string;
begin

  ResourceName:=
    HTTPExtractResourceName(
    HTTPDecodeResource(Request.Resource));

  LocalResource:=ResourceName;

  ContentDispositionFileName:=
    HTTPGetTagValue(
    HTTPGetTag(
    Headers.GetValue('Content-Disposition'),'filename'));

  if ResultCode<>HTTPCODE_SUCCESS then
    LocalResource:='error'+ResultCode.ToString
  else
  if LocalResource='' then
    LocalResource:='file'
  else
  if LocalResource='/' then
    LocalResource:='page';

  LocalResource:=
    HTTPResourceNameToLocal(
    HTTPChangeResourceNameExt(LocalResource,Headers.ContentType));

  if ContentDispositionFileName<>'' then
    LocalResource:=ExtractFilePath(LocalResource)+ContentDispositionFileName;

  Description:=ResourceName+' ('+ContentSizeToString(Content)+')';

end;

end.
