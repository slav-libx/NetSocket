unit Lib.HTTPConsts;

interface

const
  HTTP_PORT = 80;
  HTTPS_PORT = 443;
  CRLF = #13#10;
  CRLF2 = CRLF+CRLF;
  PROTOCOL_HTTP11 = 'HTTP/1.1';
  METHOD_GET = 'GET';
  SCHEME_HTTP = 'http';
  SCHEME_HTTPS = 'https';

  HTTPCODE_SUCCESS = 200;
  HTTPCODE_MOVED_PERMANENTLY = 301;
  HTTPCODE_FOUND = 302;
  HTTPCODE_BAD_REQUEST = 400;
  HTTPCODE_NOT_FOUND = 404;
  HTTPCODE_METHOD_NOT_ALLOWED = 405;
  HTTPCODE_NOT_SUPPORTED = 505;

  content_404 =

    '<html>'+CRLF+
    '<head><title>404 Not Found</title></head>'+CRLF+
    '<body bgcolor="white">'+CRLF+
    '<center><h1>404 Not Found</h1></center>'+CRLF+
    '<hr><center>server</center>'+CRLF+
    '</body>'+CRLF+
    '</html>';

  MIME_Types: array of string = [

    '.txt' ,'text/plain',
    '.html','text/html',
    '.css' ,'text/css',
    '.csv' ,'text/csv',
    '.xml' ,'text/xml',
    '.jpeg','image/jpeg',
    '.jpg' ,'image/jpeg',
    '.gif' ,'image/gif',
    '.svg' ,'image/svg+xml',
    '.png' ,'image/png',
    '.ico' ,'image/vnd.microsoft.icon',
    '.json','application/json',
    '.pdf' ,'application/pdf',
    '.zip' ,'application/zip',
    '.js'  ,'application/javascript',
    ''     ,'application/octet-stream',
    ''     ,'application/x-www-form-urlencoded',
    '.mpeg','video/mpeg',
    '.mp4' ,'video/mp4',
    '.wmv' ,'video/x-ms-wmv',
    '.flv' ,'video/x-flv',
    '.avi' ,'video/x-msvideo'];

implementation

end.
