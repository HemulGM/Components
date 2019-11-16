unit HGM.Mail.ServerOptions;

interface

uses
  Classes, System.Types, Generics.Collections, HGM.Common.Settings,
  System.SysUtils, IdPOP3, IdSSLOpenSSL, IdIMAP4;

type
  TServerType = (stIMAP, stPOP, stSMTP);

  TSocketType = (sSSL, sSTARTTLS, sPlain);

  TUserName = (unAddress, unLocalPart, unDomain);

  TPassType = (ptClearText, ptCRAMMD5, ptOther);

  TServerOptions = class
  private
    FServerType: TServerType;
    FSocketType: TSocketType;
    FPort: Cardinal;
    FHostName: string;
    FUserName: TUserName;
    FAuthentication: TPassType;
  public
    constructor Create(AServerType: TServerType; ASocketType: TSocketType; APort: Cardinal; AHostName: string; AUserName: TUserName; AAuthentication: TPassType); overload;
    constructor Create; overload;
    destructor Destroy; override;
    function Save(Settings: TSettings; RelativePath: string): Boolean;
    function Load(Settings: TSettings; RelativePath: string): Boolean;
    procedure Assign(Source: TServerOptions);
    procedure SetSettingsTo(POP3: TIdPOP3); overload;
    procedure SetSettingsTo(IOSSL: TIdSSLIOHandlerSocketOpenSSL); overload;
    procedure SetSettingsTo(IMAP: TIdIMAP4); overload;
    property Authentication: TPassType read FAuthentication write FAuthentication;
    property HostName: string read FHostName write FHostName;
    property UserName: TUserName read FUserName write FUserName;
    property ServerType: TServerType read FServerType write FServerType;
    property SocketType: TSocketType read FSocketType write FSocketType;
    property Port: Cardinal read FPort write FPort;
  end;

  TServerOptionList = TList<TServerOptions>;

  TISPDB = class
  private
    FServers: TServerOptionList;
    function GET(const URL: string; AResponse: TStringStream): Integer;
    function MX(const AEmail: string; AResponse: TStringList): Boolean;
    procedure Parse(AXMLResponse: TStringStream);
  public
    constructor Create;
    destructor Destroy; override;
    function GetDomain(const AEmail: string): string;
    function GetUser(const AEmail: string): string;
    function FindOptions(const AEmail: string): Boolean;
    function FindServer(AServerType: TServerType; var ASocketType: TSocketType): TServerOptions; overload;
    function FindServer(AServerType: TServerType): TServerOptions; overload;
    property Servers: TServerOptionList read FServers;
  end;

  EServerOptionException = class(Exception);

  ESettingsNotAssign = class(EServerOptionException);

  EUnknownError = class(EServerOptionException);

const
  cBaseURL = 'https://autoconfig.thunderbird.net/v1.1/';
  cDefaultDNS = '8.8.8.8';

resourcestring
  rsWrongEmailAddress = 'Неверно задан адрес почты. Домен не определен';
  rsEmptyXML = 'В ответе ISPDB получен пустой XML';

function GetMailServers(const DNSHost, Domain: AnsiString; const Servers: TStrings): Boolean;

implementation

uses
  StrUtils, IdDNSResolver, IdHTTP, Xml.XMLDoc, Xml.XMLIntf;

function GetMailServers(const DNSHost, Domain: AnsiString; const Servers: TStrings): Boolean;
var
  i: integer;
begin
  Result := False;
  with TIdDNSResolver.Create(nil) do
  begin
    QueryType := [qtMX];
    Host := DNSHost;
    WaitingTime := 5000;
    try
      Resolve(Domain);
      if QueryResult.Count > 0 then
      begin
        for i := 0 to QueryResult.Count - 1 do
          Servers.Append(TMXRecord(QueryResult.Items[i]).ExchangeServer);
        Result := True;
      end //ssageDlg('There is no response from the DNS server !', mtInformation, [mbOK], 0);
      else
        Result := False;
    except //ssageDlg('Error resolving domain: ' + e.message, mtInformation, [mbOK], 0);
      on E: Exception do
        Result := False;
    end;
    Free;
  end;
end;

{ TServerOptions }

procedure TServerOptions.Assign(Source: TServerOptions);
begin
  Self.FServerType := Source.ServerType;
  Self.FSocketType := Source.SocketType;
  Self.FPort := Source.Port;
  Self.FHostName := Source.HostName;
  Self.FUserName := Source.UserName;
  Self.FAuthentication := Source.Authentication;
end;

procedure TServerOptions.SetSettingsTo(POP3: TIdPOP3);
begin
  POP3.Host := FHostName;
  POP3.Port := FPort;
end;

constructor TServerOptions.Create;
begin
  inherited Create;
end;

constructor TServerOptions.Create(AServerType: TServerType; ASocketType: TSocketType; APort: Cardinal; AHostName: string; AUserName: TUserName; AAuthentication: TPassType);
begin
  Create;
  FServerType := AServerType;
  FSocketType := ASocketType;
  FPort := APort;
  FHostName := AHostName;
  FUserName := AUserName;
  FAuthentication := AAuthentication;
end;

destructor TServerOptions.Destroy;
begin
  inherited;
end;

function TServerOptions.Load(Settings: TSettings; RelativePath: string): Boolean;
begin
  Result := False;
  if not Assigned(Settings) then
    raise ESettingsNotAssign.Create('Класс параметров "TSettings" не инициализирован');
  try
    FServerType := TServerType(Settings.GetInt(RelativePath, 'ServerType', Ord(TServerType.stSMTP)));
    FSocketType := TSocketType(Settings.GetInt(RelativePath, 'SocketType', Ord(TSocketType.sPlain)));
    FPort := Settings.GetInt(RelativePath, 'Port', 0);
    FHostName := Settings.GetStr(RelativePath, 'HostName', '');
    FUserName := TUserName(Settings.GetInt(RelativePath, 'UserName', Ord(TUserName.unLocalPart)));
    FAuthentication := TPassType(Settings.GetInt(RelativePath, 'Authentication', Ord(TPassType.ptClearText)));
    Result := True;
  except
    raise EServerOptionException.Create('Ошибка при загрузке параметров');
  end;
end;

function TServerOptions.Save(Settings: TSettings; RelativePath: string): Boolean;
begin
  Result := False;
  if not Assigned(Settings) then
    raise ESettingsNotAssign.Create('Класс параметров "TSettings" не инициализирован');
  try
    Settings.SetInt(RelativePath, 'ServerType', Ord(FServerType));
    Settings.SetInt(RelativePath, 'SocketType', Ord(FSocketType));
    Settings.SetInt(RelativePath, 'Port', FPort);
    Settings.SetStr(RelativePath, 'HostName', FHostName);
    Settings.SetInt(RelativePath, 'UserName', Ord(FUserName));
    Settings.SetInt(RelativePath, 'Authentication', Ord(FAuthentication));
    Result := True;
  except
    raise EServerOptionException.Create('Ошибка при сохранении параметров');
  end;
end;

procedure TServerOptions.SetSettingsTo(IMAP: TIdIMAP4);
begin
  IMAP.Host := FHostName;
  IMAP.Port := FPort;
end;

procedure TServerOptions.SetSettingsTo(IOSSL: TIdSSLIOHandlerSocketOpenSSL);
begin
  IOSSL.Destination := FHostName + ':' + IntToStr(FPort);
  IOSSL.Host := FHostName;
  IOSSL.Port := FPort;
  IOSSL.DefaultPort := 0;
  IOSSL.SSLOptions.Method := sslvTLSv1;
  IOSSL.SSLOptions.Mode := sslmUnassigned;
end;

{ TISPDB }

constructor TISPDB.Create;
begin
  inherited;
  FServers := TServerOptionList.Create;
end;

destructor TISPDB.Destroy;
begin
  FServers.Free;
  inherited;
end;

function TISPDB.FindOptions(const AEmail: string): Boolean;
var
  AStream: TStringStream;
  AMX: TStringList;
  Str: TStringDynArray;
  Domain: string;
begin
  FServers.Clear;
  Domain := GetDomain(AEmail);
  if Domain.IsEmpty then
    raise Exception.Create(rsWrongEmailAddress);
  AStream := TStringStream.Create;
  try
    Result := GET(cBaseURL + Domain, AStream) = 200;
    if Result then
      Parse(AStream)
    else
    begin
      AMX := TStringList.Create;
      try
        if MX(AEmail, AMX) then
        begin
          Str := SplitString(AMX[0], '.');
          AStream.Clear;
          Result := GET(cBaseURL + LowerCase(Str[High(Str) - 1] + '.' + Str[High(Str)]), AStream) = 200;
          if Result then
            Parse(AStream);
        end;
      finally
        AMX.Free;
      end;
    end;
  finally
    AStream.Free
  end;
end;

function TISPDB.FindServer(AServerType: TServerType): TServerOptions;
var
  i: Integer;
begin
  for i := 0 to Pred(FServers.Count) do
    if (FServers[i].ServerType = AServerType) then
      Exit(FServers[i]);
  Result := nil;
end;

function TISPDB.FindServer(AServerType: TServerType; var ASocketType: TSocketType): TServerOptions;
var
  i: Integer;
begin
  for i := 0 to Pred(FServers.Count) do
    if (FServers[i].ServerType = AServerType) and (FServers[i].SocketType = ASocketType) then
      Exit(FServers[i]);
  Result := FindServer(AServerType);
  if Assigned(Result) then
    ASocketType := Result.SocketType;
end;

function TISPDB.GET(const URL: string; AResponse: TStringStream): Integer;
var
  HTTP: TIdHTTP;
  IdSSLIOHandlerSocketOpenSSL: TIdSSLIOHandlerSocketOpenSSL;
begin
  HTTP := TIdHTTP.Create;
  IdSSLIOHandlerSocketOpenSSL := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  HTTP.IOHandler := IdSSLIOHandlerSocketOpenSSL;
  try
    HTTP.HandleRedirects := True;
    HTTP.HTTPOptions := HTTP.HTTPOptions + [hoNoProtocolErrorException];
    HTTP.Get(URL, AResponse);
    Result := HTTP.ResponseCode;
  finally
    begin
      IdSSLIOHandlerSocketOpenSSL.Free;
      HTTP.Free;
    end;
  end;
end;

function TISPDB.GetDomain(const AEmail: string): string;
var
  i: integer;
begin
  Result := EmptyStr;
  i := Pos('@', AEmail);
  if i > 0 then
    Result := Trim(Copy(AEmail, i + 1, Length(AEmail) - i));
end;

function TISPDB.GetUser(const AEmail: string): string;
var
  i: integer;
begin
  Result := EmptyStr;
  i := Pos('@', AEmail);
  if i > 0 then
    Result := Trim(Copy(AEmail, 1, i - 1));
end;

function TISPDB.MX(const AEmail: string; AResponse: TStringList): Boolean;
begin
  Result := GetMailServers(cDefaultDNS, GetDomain(AEmail), AResponse);
end;

procedure TISPDB.Parse(AXMLResponse: TStringStream);
const
  cServerNodes: array[0..4] of string = ('hostname', 'port', 'socketType', 'username', 'authentication');
var
  XMLDoc: IXMLDocument;
  SOption: TServerOptions;
  Node, ServerNode: IXMLNode;
  ServerStr: string;
begin
  if not Assigned(AXMLResponse) then
    raise Exception.Create(rsEmptyXML);
  XMLDoc := TXMLDocument.Create(nil);
  try
    XMLDoc.LoadFromStream(AXMLResponse);
    Node := XMLDoc.DocumentElement.ChildNodes.FindNode('emailProvider');
    if Assigned(Node) then
      Node := Node.ChildNodes.First;
    while Assigned(Node) do
    begin
      if SameText(Node.NodeName, 'incomingServer') or SameText(Node.NodeName, 'outgoingServer') then
      begin
        SOption := TServerOptions.Create;
        ServerStr := Node.Attributes['type'];
      // определяем тип сервера
        if SameText(ServerStr, 'imap') then
          SOption.ServerType := TServerType.stIMAP
        else if SameText(ServerStr, 'pop3') then
          SOption.ServerType := TServerType.stPOP
        else
          SOption.ServerType := TServerType.stSMTP;
      // читаем настройки сервера
        ServerNode := Node.ChildNodes.First;
        while Assigned(ServerNode) do
        begin
          case AnsiIndexStr(ServerNode.NodeName, cServerNodes) of
            0:
              SOption.HostName := ServerNode.Text; // hostname
            1:
              SOption.Port := StrToInt(ServerNode.Text); // port
            2:
              begin // socketType
                if SameText(ServerNode.Text, 'SSL') then
                  SOption.SocketType := TSocketType.sSSL
                else if SameText(ServerNode.Text, 'STARTTLS') then
                  SOption.SocketType := TSocketType.sSTARTTLS
                else
                  SOption.SocketType := TSocketType.sPlain;
              end;
            3:
              begin // username
                if SameText(ServerNode.Text, '%EMAILADDRESS%') then
                  SOption.UserName := unAddress
                else
                  SOption.UserName := unLocalPart;
              end;
            4:
              begin // authentication
                if SameText(ServerNode.Text, 'password-cleartext') then
                  SOption.Authentication := ptClearText
                else if SameText(ServerNode.Text, 'password-encrypted') then
                  SOption.Authentication := ptCRAMMD5
                else
                  SOption.Authentication := ptOther;
              end;
          end;
          ServerNode := ServerNode.NextSibling;
        end;
        FServers.Add(SOption);
      end;
      Node := Node.NextSibling;
    end;
  finally
    XMLDoc := nil;
  end;
end;

end.

