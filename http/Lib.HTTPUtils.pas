unit Lib.HTTPUtils;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Math,
  System.NetEncoding,
  System.IOUtils,
  Lib.HTTPConsts;

type
  THTTPException = class(Exception);

  TRingBuffer<T> = record
    DataSize: Integer;
    Data: array of T;
    StartIndex,EndIndex: Integer;
    procedure Init(ADataSize: Integer=1024);
    function EOF: Boolean;
    procedure Write(const S: T);
    function Read: T;
  end;

function HTTPDecodeResource(const Resource: string): string;
function HTTPEncodeResource(const Resource: string): string;
function HTTPGetContentExt(const ContentType: string): string;
function HTTPGetMIMEType(const FileExt: string): string;
procedure HTTPGetContentTypes(Strings: TStrings);
function HTTPExtractResourceName(const Resource: string): string;
function HTTPFindLocalFile(const Resource: string; const HomePath: string; Aliases: TStrings): string;
function HTTPResourceNameToLocal(const ResourceName: string): string;
function HTTPExtractFileName(const Resource: string): string;
function HTTPChangeResourceNameExt(const ResourceName,ContentType: string): string;
procedure HTTPSplitURL(const URL: string; out Protocol,Host,Resource: string);
procedure HTTPSplitHost(const Host: string; out HostName,Port: string);
procedure HTTPSplitResource(const Resource: string; out ResourceName,Query,Fragment: string);
function HTTPTrySplitResponseResult(const Response: string; out Protocol: string; out Code: Integer; out Text: string): Boolean;
function HTTPTrySplitRequest(const Request: string; out AMethod,AResource,AProtocol: string): Boolean;
function HTTPGetTag(const Value,Tag: string): string;
function HTTPGetTagValue(const Tag: string): string;
function HTTPGetValue(const S: string): string;
function HTTPEndedChunked(const B: TBytes): Boolean;
function HTTPBytesFromChunked(const B: TBytes): TBytes;
function HTTPGetHeaderLength(const B: TBytes): Integer;

implementation

procedure TRingBuffer<T>.Init(ADataSize: Integer=1024);
begin
  DataSize:=ADataSize;
  SetLength(Data,DataSize);
  StartIndex:=0;
  EndIndex:=0;
end;

function TRingBuffer<T>.EOF: Boolean;
begin
  Result:=StartIndex=EndIndex;
end;

procedure TRingBuffer<T>.Write(const S: T);
begin
  Data[EndIndex]:=S;
  EndIndex:=(EndIndex+1) mod DataSize;
  if EOF then raise Exception.Create('buffer_overflow');
end;

function TRingBuffer<T>.Read: T;
begin
  Result:=Data[StartIndex];
  StartIndex:=(StartIndex+1) mod DataSize;
end;

procedure SameMap(const V: array of string; EnumProc: TFunc<string,string,Boolean>);
var I: Integer;
begin
  for I:=0 to High(V) div 2 do
  if EnumProc(V[I*2],V[I*2+1]) then Break;
end;

function HTTPGetContentExt(const ContentType: string): string;
var S: string;
begin
  S:='';
  SameMap(MIME_Types,
  function(Ext,MIMEType: string): Boolean
  begin
    Result:=ContentType.StartsWith(MIMEType);
    if Result then S:=Ext;
  end);
  Result:=S;
end;

function HTTPGetMIMEType(const FileExt: string): string;
var S: string;
begin
  S:='application/octet-stream';
  SameMap(MIME_Types,
  function(Ext,MIMEType: string): Boolean
  begin
    Result:=FileExt.ToLower=Ext;
    if Result then S:=MIMEType;
  end);
  Result:=S;
end;

procedure HTTPGetContentTypes(Strings: TStrings);
var I: Integer;
begin
  Strings.BeginUpdate;
  Strings.Clear;
  SameMap(MIME_Types,
  function(Ext,MIMEType: string): Boolean
  begin
    Result:=False;
    Strings.Add(MIMEType);
  end);
  Strings.EndUpdate;
end;

function HTTPDecodeResource(const Resource: string): string;
begin
  try
    Result:=TNetEncoding.URL.Decode(Resource);
  except
  on E: EConvertError do Exit(Resource);
  else raise;
  end;
end;

function HTTPEncodeResource(const Resource: string): string;
var ResourceName,Query,Fragment: string;
begin
  try

    HTTPSplitResource(Resource,ResourceName,Query,Fragment);

    Result:=
      TNetEncoding.URL.Encode(ResourceName).
        Replace('%2F','/').
        Replace('+','%20');
//        Replace('%3F','?').
//        Replace('%3D','=').
//        Replace('%26','&');
//        Replace('%5B','[').
//        Replace('%5D',']').
//        Replace('%3A',':');

    if Query<>'' then Result:=Result+'?'+
      TNetEncoding.URL.Encode(Query).
//        Replace('%2F','/').
//        Replace('+','%20').
        Replace('%3D','=').
        Replace('%26','&');

    if Fragment<>'' then Result:=Result+'#'+
      TNetEncoding.URL.Encode(Fragment);

  except
  on E: EConvertError do Exit(Resource);
  else raise;
  end;
end;

procedure HTTPSplitURL(const URL: string; out Protocol,Host,Resource: string);
var Index: Integer; S: string;
begin

  S:=URL;

  Index:=S.IndexOf('://');
  if Index<>-1 then
  begin
    Protocol:=S.Substring(0,Index);
    S:=S.Substring(Index+3);
  end else
    Protocol:='';

  Index:=S.IndexOf('/');
  if Index<>-1 then
  begin
    Host:=S.Substring(0,Index);
    Resource:=S.Substring(Index);
  end else begin
    Host:=S;
    Resource:='/';
  end;

end;

procedure HTTPSplitHost(const Host: string; out HostName,Port: string);
var Index: Integer;
begin
  HostName:=Host;
  Index:=HostName.IndexOf(':');
  if Index<>-1 then
  begin
    Port:=HostName.Substring(Index+1);
    HostName:=HostName.Substring(0,Index);
  end else
    Port:='';
end;

procedure HTTPSplitResource(const Resource: string; out ResourceName,Query,Fragment: string);
var P,F: Integer;
begin
  P:=Resource.IndexOf('?');
  if P=-1 then P:=Length(Resource); // MaxInt unacceptably
  F:=Resource.IndexOf('#',P);
  if F=-1 then F:=Length(Resource); // MaxInt unacceptably
  ResourceName:=Resource.Substring(0,P);
  Query:=Resource.Substring(P+1,F-P-1);
  Fragment:=Resource.Substring(F+1);
end;

function HTTPExtractResourceName(const Resource: string): string;
var Query,Fragment: string;
begin
  HTTPSplitResource(Resource,Result,Query,Fragment);
end;

function HTTPExtractFileName(const Resource: string): string;
var P: Integer;
begin
  Result:=HTTPExtractResourceName(Resource);
  P:=Result.LastDelimiter('/');
  if P<>-1 then
    Result:=Result.SubString(P+1);
end;

function HTTPChangeResourceNameExt(const ResourceName,ContentType: string): string;
var
  Extension,ResourceExtension: string;
  P: Integer;
begin

  Result:=ResourceName;

  Extension:=HTTPGetContentExt(ContentType);

  if Extension<>'' then
  begin

    P:=ResourceName.LastDelimiter('./');

    if (P<0) or (ResourceName.Chars[P]<>'.') then

      Result:=ResourceName+Extension

    else begin

      ResourceExtension:=ResourceName.SubString(P);

      if ContentType<>HTTPGetMIMEType(ResourceExtension) then
        Result:=ResourceName.SubString(0,P)+Extension;

    end;

  end;

end;

function CombineString(const S1,S2,S3: string): string;
begin
  if S1.EndsWith(S2) then
    if S3.StartsWith(S2) then
      Result:=S1+S3.Substring(1)
    else
      Result:=S1+S3
  else
    if S3.StartsWith(S2) then
      Result:=S1+S3
    else
      Result:=S1+S2+S3;
end;

function CombinePath(const RootPath,FileName: string): string;
begin
  Result:=FileName;
  if (Result.Substring(0,2)<>'\\') and (Result.Substring(1,1)<>':') then
    Result:=CombineString(RootPath,'\',Result);
end;

function HTTPFindLocalFile(const Resource: string; const HomePath: string; Aliases: TStrings): string;
var
  AliasName,AliasPath: string;
  I: Integer;
begin

  Result:=
    HTTPResourceNameToLocal(
    HTTPExtractResourceName(
    HTTPDecodeResource(Resource)));

  for I:=0 to Aliases.Count-1 do
  begin
    AliasName:=CombineString('','\',Aliases.Names[I]);
    if (AliasName<>'') and Result.StartsWith(AliasName) then
    begin
      AliasPath:=Aliases.ValueFromIndex[I];
      Result:=CombinePath(AliasPath,Result.Substring(Length(AliasName)));
    end;
  end;

  Result:=CombinePath(HomePath,Result);

end;

function HTTPResourceNameToLocal(const ResourceName: string): string;
begin
  Result:=ResourceName.Replace('/','\',[rfReplaceAll]);
end;

function HTTPTrySplitResponseResult(const Response: string; out Protocol: string; out Code: Integer; out Text: string): Boolean;
var Index1,Index2: Integer;
begin
  Index1:=Response.IndexOf(' ',0)+1;
  Index2:=Response.IndexOf(' ',Index1);
  Result:=TryStrToInt(Response.Substring(Index1,Index2-Index1),Code);
  Protocol:=Response.Substring(0,Index1-1);
  Text:=Response.Substring(Index2+1);
end;

function HTTPTrySplitRequest(const Request: string; out AMethod,AResource,AProtocol: string): Boolean;
var
  RequestMethod: string;
  Index1,Index2,Index3: Integer;
begin

  Index3:=Length(Request);

  RequestMethod:=Request.Substring(0,Index3);

  Index1:=RequestMethod.IndexOf(' ');
  Index2:=RequestMethod.LastIndexOf(' ');

  Result:=(Index1<>-1) and (Index2>Index1);

  if Result then
  begin
    AMethod:=RequestMethod.Substring(0,Index1);
    AResource:=RequestMethod.Substring(Index1+1,Index2-Index1-1);
    AProtocol:=RequestMethod.Substring(Index2+1);
  end;

end;

function CompareBytesWith(const B,E: TBytes; StartIndex: Integer=0): Boolean;
var I: Integer;
begin
  Result:=True;
  if not InRange(StartIndex,0,High(B)-High(E)) then Exit(False);
  for I:=0 to High(E) do
    if B[StartIndex+I]<>E[I] then Exit(False);
end;

function BytesEndsWith(const B,E: TBytes): Boolean;
begin
  Result:=CompareBytesWith(B,E,High(B)-High(E));
end;

function BytesIndexOf(const B,E: TBytes; StartIndex: Integer=0): Integer;
begin
  for Result:=StartIndex to High(B)-High(E) do
    if CompareBytesWith(B,E,Result) then Exit;
  Result:=-1;
end;

function HTTPGetHeaderLength(const B: TBytes): Integer;
var Index: Integer;
begin
  Result:=BytesIndexOf(B,[13,10,13,10]);
end;

function HTTPGetTag(const Value,Tag: string): string;
var S: string;
begin
  for S in Value.Split([';']) do
  if S.Trim.StartsWith(Tag+'=') then Exit(S.Trim);
  Result:='';
end;

function HTTPGetTagValue(const Tag: string): string;
var P: Integer;
begin
  Result:='';
  P:=Tag.IndexOf('=');
  if P<>-1 then Result:=Tag.Substring(P+1).Trim([' ','"','''']);
end;

function HTTPGetValue(const S: string): string;
var P: Integer;
begin
  Result:='';
  P:=S.IndexOf(':');
  if P<>-1 then Result:=S.Substring(P+1).Trim([' ']);
end;

function HTTPEndedChunked(const B: TBytes): Boolean;
begin
  Result:=BytesEndsWith(B,[48,13,10,13,10]);
end;

function HTTPBytesFromChunked(const B: TBytes): TBytes;
var Index,ChunkIndex,ResultIndex,ChunkSize: Integer;
begin
  SetLength(Result,Length(B));
  Index:=0;
  ResultIndex:=0;
  while True do
  begin
    ChunkIndex:=BytesIndexOf(B,[13,10],Index);
    if not InRange(ChunkIndex,1,High(B)-2) then Break;
    ChunkSize:=StrToIntDef('$'+TEncoding.ANSI.GetString(B,Index,ChunkIndex-Index),0);
    if not InRange(ChunkSize,1,High(B)-ChunkIndex-6) then Break;
    Move(B[ChunkIndex+2],Result[ResultIndex],ChunkSize);
    Inc(ResultIndex,ChunkSize);
    Index:=ChunkIndex+4+ChunkSize;
  end;
  SetLength(Result,ResultIndex);
end;

end.
