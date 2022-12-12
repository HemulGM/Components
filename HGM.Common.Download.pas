unit HGM.Common.Download;

interface

uses
  System.SysUtils, System.Classes, System.Net.HttpClient, System.JSON;

type
  TDownload = class;

  TOnReceive = procedure(const Sender: TObject; AContentLength: Int64; AReadCount: Int64; var Abort: Boolean) of object;

  TOnFinish = procedure(const Sender: TDownload; ResponseCode: Integer) of object;

  TOnReceiveRef = reference to procedure(const Sender: TDownload; AContentLength: Int64; AReadCount: Int64; var Abort: Boolean);

  TOnFinishRef = reference to procedure(const Sender: TDownload; ResponseCode: Integer);

  TOnFinishRefStream = reference to procedure(const Sender: TDownload; Stream: TMemoryStream; ResponseCode: Integer);

  TDownload = class(TThread)
  private
    FHTTP: THTTPClient;
    FOnReceive: TOnReceiveRef;
    FOnFinish: TOnFinishRef;
    FResponseCode: Integer;
    FURL: string;
    FFileName: string;
    FLength: Integer;
    FCount: Integer;
    FOnFinishStream: TOnFinishRefStream;
    procedure InternalOnReceive(const Sender: TObject; AContentLength: Int64; AReadCount: Int64; var Abort: Boolean);
    procedure DoNotifyFinish;
    procedure DoNotifyFinishStream(Stream: TMemoryStream);
    procedure SetOnFinishStream(const Value: TOnFinishRefStream);
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean = True); overload;
    constructor CreateAndStart(const AUrl, AFileName: string; OnReceiveProc: TOnReceiveRef = nil; OnFinishProc: TOnFinishRef = nil); overload;
    constructor CreateAndStart(const AUrl: string; OnReceiveProc: TOnReceiveRef = nil; OnFinishProc: TOnFinishRefStream = nil); overload;
    destructor Destroy; override;
    property URL: string read FURL write FURL;
    property FileName: string read FFileName write FFileName;
    property Length: Integer read FLength;
    property Count: Integer read FCount;
    property OnReceive: TOnReceiveRef read FOnReceive write FOnReceive;
    property OnFinish: TOnFinishRef read FOnFinish write FOnFinish;
    property OnFinishStream: TOnFinishRefStream read FOnFinishStream write SetOnFinishStream;
    class function GetRequest(const URL: string): Boolean; overload;
    class function Get(const URL: string; Response: TStream): Boolean; overload;
    class function Get(const URL, FileName: string): Boolean; overload;
    class function Get(const URL: string): TMemoryStream; overload;
    class function GetText(const URL: string; var Response: string): Boolean; overload;
    class function GetText(const URL: string): string; overload;
    class function Post(const URL: string; Stream: TStream; Response: TStream = nil): Boolean; overload;
    class function PostJson(const URL, Json: string; var Response: string): Boolean; overload;
    class function PostJson(const URL, Json: string; Response: TStream): Boolean; overload;
    class function PostJson(const URL: string; Json: TJsonValue; var Response: string): Boolean; overload;
    class function PostJson(const URL: string; Json: TJsonValue; Response: TStream): Boolean; overload;
    class function PostFile(const URL: string; const Field, FileName: TArray<string>; Stream: TArray<TStream>; Response: TStream = nil): Boolean; overload; static;
    class function PostFile(const URL: string; const Field, FileName: TArray<string>; Response: TStream = nil): Boolean; overload; static;
    class function PostFile(const URL: string; const FileName: string; Response: TStream = nil): Boolean; overload; static;
  end;

implementation

uses
  System.Net.Mime, System.Net.URLClient, System.NetConsts;

{ TDownThread }

constructor TDownload.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  FHTTP := THTTPClient.Create;
  FHTTP.OnReceiveData := InternalOnReceive;
end;

constructor TDownload.CreateAndStart(const AUrl, AFileName: string; OnReceiveProc: TOnReceiveRef; OnFinishProc: TOnFinishRef);
begin
  inherited Create(False);
  FreeOnTerminate := True;
  FHTTP := THTTPClient.Create;
  FHTTP.OnReceiveData := InternalOnReceive;
  FHTTP.HandleRedirects := True;
  URL := AUrl;
  FileName := AFileName;
  FOnFinish := OnFinishProc;
  FOnReceive := OnReceiveProc;
end;

class function TDownload.Get(const URL: string; Response: TStream): Boolean;
var
  HTTP: THTTPClient;
begin
  Result := False;
  Response.Size := 0;
  if URL.IsEmpty then
    Exit;
  HTTP := THTTPClient.Create;
  HTTP.HandleRedirects := True;
  try
    try
      Result := (HTTP.Get(URL, Response).StatusCode = 200) and (Response.Size > 0);
      Response.Position := 0;
    finally
      HTTP.Free;
    end;
  except
    Result := False;
  end;
end;

class function TDownload.GetRequest(const URL: string): Boolean;
var
  HTTP: THTTPClient;
begin
  Result := False;
  if URL.IsEmpty then
    Exit;
  HTTP := THTTPClient.Create;
  HTTP.HandleRedirects := True;
  try
    try
      Result := HTTP.Get(URL).StatusCode = 200;
    finally
      HTTP.Free;
    end;
  except
    Result := False;
  end;
end;

class function TDownload.GetText(const URL: string): string;
var
  Stream: TStringStream;
begin
  Stream := TStringStream.Create;
  try
    if Get(URL, Stream) then
      Result := Stream.DataString;
  finally
    Stream.Free;
  end;
end;

class function TDownload.Post(const URL: string; Stream, Response: TStream): Boolean;
var
  HTTP: THTTPClient;
begin
  Result := False;
  if URL.IsEmpty then
    Exit;
  HTTP := THTTPClient.Create;
  try
    HTTP.HandleRedirects := True;
    try
      if Assigned(Response) then
        Result := HTTP.Post(URL, Stream, Response).StatusCode = 200
      else
        Result := HTTP.Post(URL, Stream).StatusCode = 200;
    finally
      HTTP.Free;
    end;
  except
    Result := False;
  end;
end;

class function TDownload.PostFile(const URL: string; const Field, FileName: TArray<string>; Stream: TArray<TStream>; Response: TStream): Boolean;
var
  HTTP: THTTPClient;
  Form: TMultipartFormData;
begin
  Result := False;
  if URL.IsEmpty then
    Exit;
  HTTP := THTTPClient.Create;
  Form := TMultipartFormData.Create;
  try
    HTTP.HandleRedirects := True;
    for var i := Low(Field) to High(Field) do
      Form.AddStream(Field[i], Stream[i], FileName[i]);
    try
      if Assigned(Response) then
        Result := HTTP.Post(URL, Form, Response).StatusCode = 200
      else
        Result := HTTP.Post(URL, Form).StatusCode = 200;
    finally
      Form.Free;
      HTTP.Free;
    end;
  except
    Result := False;
  end;
end;

class function TDownload.PostFile(const URL: string; const Field, FileName: TArray<string>; Response: TStream): Boolean;
var
  HTTP: THTTPClient;
  Form: TMultipartFormData;
begin
  Result := False;
  if URL.IsEmpty then
    Exit;
  HTTP := THTTPClient.Create;
  Form := TMultipartFormData.Create;
  try
    HTTP.HandleRedirects := True;
    for var i := Low(Field) to High(Field) do
      Form.AddFile(Field[i], FileName[i]);
    try
      if Assigned(Response) then
        Result := HTTP.Post(URL, Form, Response).StatusCode = 200
      else
        Result := HTTP.Post(URL, Form).StatusCode = 200;
    finally
      Form.Free;
      HTTP.Free;
    end;
  except
    Result := False;
  end;
end;

class function TDownload.PostJson(const URL: string; Json: TJsonValue; Response: TStream): Boolean;
begin
  Result := PostJson(URL, Json.ToJSON, Response);
end;

class function TDownload.PostJson(const URL: string; Json: TJsonValue; var Response: string): Boolean;
begin
  Result := PostJson(URL, Json.ToJSON, Response);
end;

class function TDownload.GetText(const URL: string; var Response: string): Boolean;
var
  Stream: TStringStream;
begin
  Result := False;
  Stream := TStringStream.Create;
  try
    if Get(URL, Stream) then
    begin
      Response := Stream.DataString;
      Result := True;
    end;
  finally
    Stream.Free;
  end;
end;

class function TDownload.PostJson(const URL, Json: string; Response: TStream): Boolean;
var
  HTTP: THTTPClient;
  Body: TStringStream;
begin
  Result := False;
  if URL.IsEmpty then
    Exit;
  HTTP := THTTPClient.Create;
  Body := TStringStream.Create;
  try
    HTTP.HandleRedirects := True;
    HTTP.ContentType := 'application/json';
    try
      Body.WriteString(Json);
      Body.Position := 0;
      Result := HTTP.Post(URL, Body, Response).StatusCode = 200;
    except
      Result := False;
    end;
  finally
    Body.Free;
    HTTP.Free;
  end;
end;

class function TDownload.PostFile(const URL, FileName: string; Response: TStream): Boolean;
var
  HTTP: THTTPClient;
  Body: TFileStream;
  LKind: TMimeTypes.TKind;
  LType: string;
begin
  Result := False;
  if URL.IsEmpty then
    Exit;
  HTTP := THTTPClient.Create;
  Body := TFileStream.Create(FileName, fmShareDenyWrite);
  try
    HTTP.HandleRedirects := True;
    TMimeTypes.Default.GetFileInfo(FileName, LType, LKind);
    HTTP.ContentType := LType;
    try
      Body.Position := 0;
      Result := HTTP.Post(URL, Body, Response).StatusCode = 200;
    except
      Result := False;
    end;
  finally
    Body.Free;
    HTTP.Free;
  end;

end;

class function TDownload.PostJson(const URL, Json: string; var Response: string): Boolean;
var
  Stream: TStringStream;
begin
  Stream := TStringStream.Create;
  try
    Result := PostJson(URL, Json, Stream);
    if Result then
      Response := Stream.DataString;
  finally
    Stream.Free;
  end;
end;

class function TDownload.Get(const URL: string): TMemoryStream;
var
  HTTP: THTTPClient;
begin
  Result := TMemoryStream.Create;
  if URL.IsEmpty then
    Exit;
  HTTP := THTTPClient.Create;
  HTTP.HandleRedirects := True;
  try
    try
      if (HTTP.Get(URL, Result).StatusCode = 200) and (Result.Size > 0) then
        Result.Position := 0;
    finally
      HTTP.Free;
    end;
  except
    //
  end;
end;

class function TDownload.Get(const URL, FileName: string): Boolean;
var
  HTTP: THTTPClient;
  FS: TFileStream;
begin
  Result := False;
  if URL.IsEmpty then
    Exit;
  HTTP := THTTPClient.Create;
  HTTP.HandleRedirects := True;
  try
    FS := TFileStream.Create(FileName, fmCreate or fmShareDenyNone);
    try
      Result := (HTTP.Get(URL, FS).StatusCode = 200) and (FS.Size > 0);
    except
      Result := False;
    end;
    FS.Free;
  finally
    HTTP.Free;
  end;
end;

constructor TDownload.CreateAndStart(const AUrl: string; OnReceiveProc: TOnReceiveRef; OnFinishProc: TOnFinishRefStream);
begin
  inherited Create(False);
  FreeOnTerminate := True;
  FHTTP := THTTPClient.Create;
  FHTTP.OnReceiveData := InternalOnReceive;
  FHTTP.HandleRedirects := True;
  URL := AUrl;
  FileName := '';
  FOnFinishStream := OnFinishProc;
  FOnReceive := OnReceiveProc;
end;

destructor TDownload.Destroy;
begin
  FHTTP.Free;
  inherited Destroy;
end;

procedure TDownload.Execute;
var
  FS: TFileStream;
  Mem: TMemoryStream;
begin
  FResponseCode := -1;
  if not FURL.IsEmpty then
  begin
    if not FileName.IsEmpty then
    begin
      if not FURL.IsEmpty then
      begin
        try
          FS := TFileStream.Create(FileName, fmCreate or fmShareDenyNone);
          try
            FResponseCode := FHTTP.Get(FURL, FS).StatusCode;
          finally
            FS.Free;
          end;
        except
        end;
      end;
      Synchronize(DoNotifyFinish);
    end
    else
    begin
      if not FURL.IsEmpty then
      begin
        try
          Mem := TMemoryStream.Create;
          try
            FResponseCode := FHTTP.Get(FURL, Mem).StatusCode;
            Mem.Position := 0;
          finally

          end;
        except
          Mem.Free;
        end;
      end;
      TThread.Synchronize(nil,
        procedure
        begin
          DoNotifyFinishStream(Mem);
        end);
      if Assigned(Mem) then
        Mem.Free;
    end;
  end;
end;

procedure TDownload.DoNotifyFinish;
begin
  if Assigned(FOnFinish) then
    FOnFinish(Self, FResponseCode);
end;

procedure TDownload.DoNotifyFinishStream(Stream: TMemoryStream);
begin
  if Assigned(FOnFinishStream) then
    FOnFinishStream(Self, Stream, FResponseCode);
end;

procedure TDownload.InternalOnReceive(const Sender: TObject; AContentLength: Int64; AReadCount: Int64; var Abort: Boolean);
var
  Cancel: Boolean;
begin
  FLength := AContentLength;
  FCount := AReadCount;
  TThread.Synchronize(nil,
    procedure
    begin
      if Assigned(FOnReceive) then
        FOnReceive(Self, AContentLength, AReadCount, Cancel);
    end);
  Abort := Cancel;
end;

procedure TDownload.SetOnFinishStream(const Value: TOnFinishRefStream);
begin
  FOnFinishStream := Value;
end;

end.

