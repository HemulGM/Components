unit HGM.Common.Download;

interface

uses
  System.SysUtils, System.Classes, System.Net.HttpClient;

type
  TDownload = class;

  TOnReceive = procedure(const Sender: TObject; AContentLength: Int64; AReadCount: Int64; var Abort: Boolean) of object;

  TOnFinish = procedure(Sender: TDownload; ResponseCode: Integer) of object;

  TOnReceiveRef = reference to procedure(const Sender: TObject; AContentLength: Int64; AReadCount: Int64; var Abort: Boolean);

  TOnFinishRef = reference to procedure(Sender: TDownload; ResponseCode: Integer);

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
    procedure InternalOnReceive(const Sender: TObject; AContentLength: Int64; AReadCount: Int64; var Abort: Boolean);
    procedure DoNotifyFinish;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean); overload;
    constructor CreateAndStart(AUrl, AFileName: string; OnReceiveProc: TOnReceiveRef = nil; OnFinishProc: TOnFinishRef =
      nil); overload;
    destructor Destroy; override;
    property URL: string read FURL write FURL;
    property FileName: string read FFileName write FFileName;
    property Length: Integer read FLength;
    property Count: Integer read FCount;
    property OnReceive: TOnReceiveRef read FOnReceive write FOnReceive;
    property OnFinish: TOnFinishRef read FOnFinish write FOnFinish;
    class function Get(URL: string; const Mem: TMemoryStream): Boolean; overload;
    class function Get(URL, FileName: string): Boolean; overload;
  end;

implementation

{TDownThread}

constructor TDownload.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  FHTTP := THTTPClient.Create;
  FHTTP.OnReceiveData := InternalOnReceive;
end;

constructor TDownload.CreateAndStart(AUrl, AFileName: string; OnReceiveProc: TOnReceiveRef; OnFinishProc: TOnFinishRef);
begin
  inherited Create(False);
  FreeOnTerminate := True;
  FHTTP := THTTPClient.Create;
  FHTTP.OnReceiveData := InternalOnReceive;
  URL := AUrl;
  FileName := AFileName;
  FOnFinish := OnFinishProc;
  FOnReceive := OnReceiveProc;
end;

class function TDownload.Get(URL: string; const Mem: TMemoryStream): Boolean;
var
  HTTP: THTTPClient;
begin
  Result := False;
  Mem.Clear;
  if URL.IsEmpty then
    Exit;
  HTTP := THTTPClient.Create;
  HTTP.HandleRedirects := True;
  try
    try
      Result := (HTTP.Get(URL, Mem).StatusCode = 200) and (Mem.Size > 0);
      Mem.Position := 0;
    except
      Result := False;
    end;
  finally
    HTTP.Free;
  end;
end;

class function TDownload.Get(URL: string; FileName: string): Boolean;
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

destructor TDownload.Destroy;
begin
  FHTTP.Free;
  inherited Destroy;
end;

procedure TDownload.Execute;
var
  FS: TFileStream;
begin
  FResponseCode := -1;
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
end;

procedure TDownload.DoNotifyFinish;
begin
  if Assigned(FOnFinish) then
    FOnFinish(Self, FResponseCode);
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

end.

