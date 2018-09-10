{-----------------------------------------------------------------------------}
{   КОМПОНЕНТ  TDBF  Version 1.11  -  14.06.2004             Freeware         }
{-----------------------------------------------------------------------------}
{   Copyright  2002-2004 Брусникин И.В.                     majar@nm.ru       }
{-----------------------------------------------------------------------------}
{   Компонент предназначен для непосредственного доступа (без использования   }
{ BDE, ODBC) к файлам формата dbf версий dBase III+, dBase IV, dBase V.       }
{ Поддерживаемые типы данных: символьный, дата, числа с плавающей точкой,     }
{ числа с фиксированной точкой, логический.                                   }
{   Позволяет выполнять основные операции с dbf-файлами а также создавать их. }
{ Набор доступных методов и свойств компонента максимально приближен к компо- }
{ ненту TTable, но компонент не является потомком  TTable и взаимодействовать }
{ с визуальными компонентами TDBXXX "не умеет".                               }
{   Работает с Delphi 3..6 под Windows 9X/NT4/2000/XP.                        }
{-----------------------------------------------------------------------------}

unit DBF;

interface

uses Windows, SysUtils, Classes;

type

  DBFHeader = record
    VerDBF      : Byte;     // 0     - версия dbf-файла
    Year        : Byte;     // 1     - год последнего обновления
    Month       : Byte;     // 2     - месяц последнего обновления
    Day         : Byte;     // 3     - день последнего обновления
    RecordCount : Integer;  // 4-7   - количество записей в файле включая удаленные
    HeaderSize  : Word;     // 8-9   - размер заголовка (в байтах)
    RecordSize  : Word;     // 10-11 - размер записи (в байтах)
    Dummy1      : Word;     // 12-13 - зарезервировано
    Transaction : Byte;     // 14    - флаг незавершенной транзакции dBASE IV
    Encrypt     : Byte;     // 15    - флаг зашифрованности таблицы dBASE IV
    MultiUse    : Integer;  // 16-19 - зарезервировано для сетевого
    LastUserID  : Integer;  // 20-23   использования - dBASE IV, dBASE V
    Dummy2      : Integer;  // 24-27 - зарезервировано
    MDXFlag     : Byte;     // 28    - флаг наличия MDX-файла
    Language    : Byte;     // 29    - ID драйвера языка (кодовая страница)
    Dummy3      : Word;     // 30-31 - зарезервировано
  end;

  PField = ^DBField;
  DBField = record
    FieldName      : array[0..10] of Char;  // - имя поля (дополненное нулями)
    FieldType      : Char;     // 11           - тип поля
    FieldOffset    : Integer;  // 12-15        - расположение поля внутри записи
    FieldSize      : Byte;     // 16           - размер поля (в байтах)
    FieldPrecision : Byte;     // 17           - количество знаков после запятой
    Dummy          : array[18..31] of Byte; // - зарезервировано
  end;

  ED = class(Exception);

  TFLDType = (bfBoolean, bfDate, bfFloat, bfNumber, bfString, bfUnkown);

  TCodePage = (ANSI, OEM, NONE);

  TDBF = class(TComponent)
  private
    FTableName: String;
    FExclusive: Boolean;
    FActive: Boolean;
    FCodePage: TCodePage;
    FModified: Boolean;
    FCurrentRecord: LongInt;
    FNumFields: Integer;
    FRecordCount: LongInt;
    DFile: TFileStream;
    FHeader: DBFHeader;
    FieldList: TList;
    RecordBuffer: array[1..4000] of Char;
    procedure ClearHeader;
    function GetDbfDate: TDateTime;
    function GetDeleteStatus: Boolean;
    function IsBof: Boolean;
    function IsEof: Boolean;
    function PadLeft(Text: String; Len: Integer): String;
    function PadRight(Text: String; Len: Integer): String;
    procedure ReadHeader;
    procedure ReadRecord;
    procedure RewriteTable;
    procedure SetActive(Value: Boolean);
    procedure SetDbfDate(Value: TDateTime);
    procedure SetDeleteStatus(Value: Boolean);
    procedure SetExclusive(Value: Boolean);
    procedure SetCodePage(Value: TCodePage);
    procedure SetTableName(Name: String);
    function TrimSpaces(Text: String): String;
    procedure WriteHeader;
    procedure WriteRecord;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddFieldDefs(FldName: String; FldType: TFLDType; FldSize, FldPrecision: Byte);
    procedure Append;          
    procedure Cancel;          
    procedure ClearFields;     
    procedure Close;           
    procedure CreateTable;     
    procedure EmptyTable;      
    procedure First;           
    function GetFieldData(FieldNo: Integer): String;      
    function GetFieldName(FieldNo: Integer): String;      
    function GetFieldPrecision(FieldNo: Integer): Byte;   
    function GetFieldSize(FieldNo: Integer): Byte;        
    function GetFieldType(FieldNo: Integer): TFLDType;    
    procedure GoToRecord(RecordNo: Integer);              
    procedure Insert;          
    function IsEmpty: Boolean; 
    procedure Last;            
    procedure Next;            
    procedure Open;            
    procedure PackTable;       
    procedure Post;            
    procedure Prior;           
    procedure Refresh;         
    procedure SetFieldData(FieldNo: Integer; Data: String);   
    procedure Translate(Src, Dest: String; ToOEM: Boolean);   
    property Active: Boolean read FActive write SetActive;    
    property Bof: Boolean read IsBof;
    property CodePage: TCodePage read FCodePage write SetCodePage;
    property DbfDate: TDateTime read GetDbfDate write SetDbfDate;
    property Deleted: Boolean read GetDeleteStatus write SetDeleteStatus;
    property Eof: Boolean read IsEof;
    property FieldCount: Integer read FNumFields;
    property Modified: Boolean read FModified;
    property RecNo: LongInt read FCurrentRecord;
    property RecordCount: LongInt read FRecordCount;
  published
    property Exclusive: Boolean read FExclusive write SetExclusive;
    property TableName: String read FTableName write SetTableName;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('LKDU', [TDBF]);
end;

{*** Очистка записи заголовка ***}
procedure TDbf.ClearHeader;
begin
  FillChar(FHeader, SizeOf(FHeader), 0);
end;

{*** Получение даты последней модификации dbf-файла ***}
function TDbf.GetDbfDate: TDateTime;
var Y,M,D: Word;
begin
  with FHeader do begin Y:=Year+1900; M:=Month; D:=Day; end;
  Result:=EncodeDate(Y,M,D);
end;

{*** Проверка, помечена ли текущая запись как удаленная (True - удалена) ***}
function TDbf.GetDeleteStatus: Boolean;
begin
  Result:=RecordBuffer[1]='*';
end;

{*** Проверка, является ли текущая запись первой в dbf-файле ***}
function TDbf.IsBof: Boolean;
begin
  Result:=FCurrentRecord=1;
end;

{*** Проверка, является ли текущая запись последней в dbf-файле ***}
function TDbf.IsEof: Boolean;
begin
  Result:=FCurrentRecord=FRecordCount;
end;

{*** Дополнение строки пробелами слева до длины Len ***}
function TDbf.PadLeft(Text: String; Len: Integer): String;
begin
  Result:=Text; while Length(Result)<Len do Result:=' '+Result;
end;

{*** Дополнение строки пробелами справа до длины Len ***}
function TDbf.PadRight(Text: String; Len: Integer): String;
begin
  Result:=Text; while Length(Result)<Len do Result:=Result+' ';
end;

{*** Чтение заголовка dbf-файла ***}
procedure TDbf.ReadHeader;
var
  N: Integer;
  Field: PField;
begin
  try
    DFile.Seek(0, soFromBeginning);
    DFile.ReadBuffer(FHeader, Sizeof(FHeader)); // Читаем заголовок dbf-файла
    FNumFields:=(FHeader.HeaderSize div 32)-1;  // Количество полей в файле
    FRecordCount:=FHeader.RecordCount;          // Количество записей в файле
    if FRecordCount>0 then FCurrentRecord:=1    // Номер текущей физической записи
    else FCurrentRecord:=0;                     // в открытом файле
    case FHeader.Language of                    // Кодовая страница
      87: FCodePage:=ANSI;
      38: FCodePage:=OEM;
    else FCodePage:=NONE; end;
    // Получение информации о полях
    for N:=1 to FNumFields do
    begin
      DFile.Seek(N*32, soFromBeginning);
      New(Field);
      DFile.ReadBuffer(Field^, Sizeof(DBField));
      FieldList.Add(Field);
    end;
  except
    raise ED.Create('ReadHeader: Ошибка чтения заголовка файла');
  end;
end;

{*** Чтение записи из dbf-файла ***}
procedure TDbf.ReadRecord;
begin
  try
    DFile.Seek(FHeader.HeaderSize+((FCurrentRecord-1)*FHeader.RecordSize), soFromBeginning);
    DFile.ReadBuffer(RecordBuffer, FHeader.RecordSize);
  except
    raise ED.Create('ReadRecord: Ошибка чтения записи');
  end;
end;

{*** Перезапись dbf-файла с новым признаком EOF ***}
procedure TDbf.RewriteTable;
var
  Tmp: TFileStream;
  FN : String;
begin
  FN:=FTableName;
  try
    // Если временный файл уже существует, уничтожаем его
    if FileExists('~~Brig~~.tmp') then DeleteFile('~~Brig~~.tmp');
    // Создаем временный файл и открываем его
    Tmp:=TFileStream.Create('~~Brig~~.tmp', fmOpenReadWrite or fmCreate);
    try
      // Копируем данные из dbf-файла во временный файл до EOF включительно
      DFile.Seek(0, soFromBeginning);
      with FHeader do Tmp.CopyFrom(DFile, HeaderSize+RecordCount*RecordSize+1);
    finally
      Tmp.Free;  // Закрываем файлы
    end;
    Close;
    // Удаляем dbf-файл и переименовываем временный файл в dbf-файл
    if FileExists(FN) then DeleteFile(FN);
    if not RenameFile('~~Brig~~.tmp',FN) then
      raise ED.Create('RewriteTable: Не удалось переименовать временный файл')
    else begin FTableName:=FN; Refresh; end; // Открываем перезаписанный файл
  except
    raise ED.Create('RewriteTable: Ошибка при перезаписи файла');
  end;
end;

{*** Установка свойства Active, открытие или закрытие файла ***}
procedure TDbf.SetActive(Value: Boolean);
begin
  if Value then Open else Close;
end;

{*** Установка даты последней модификации dbf-файла ***}
procedure TDbf.SetDbfDate(Value: TDateTime);
var
  Y,M,D: Word;
  C    : array[1..3] of Char;
begin
  try DecodeDate(Value, Y, M, D);
  except raise ED.Create('SetDbfDate: Неверное значение даты'); end;
  with FHeader do
  begin
    Year:=Y-1900; Month:=M; Day:=D;
    C[1]:=Chr(Year); C[2]:=Chr(Month); C[3]:=Chr(Day);
  end;
  try DFile.Seek(1, soFromBeginning); DFile.WriteBuffer(C, 3);
  except raise ED.Create('SetDbfDate: Ошибка записи в заголовок файла'); end;
end;

{*** Пометить текущую запись как удаленную (или наоборот) в dbf-файле ***}
procedure TDbf.SetDeleteStatus(Value: Boolean);
begin
  if Value then RecordBuffer[1]:='*' else RecordBuffer[1]:=' '; WriteRecord;
end;

{*** Установка режима монопольного доступа к файлу ***}
procedure TDbf.SetExclusive(Value: Boolean);
begin
  if not FActive then FExclusive:=Value else
    raise ED.Create('Exclusive: Это свойство нельзя изменять, если файл открыт');
end;

{*** Установка ID драйвера языка (кодовой страницы) ***}
procedure TDbf.SetCodePage(Value: TCodePage);
var C: Char;
begin
  with FHeader do
  case Value of
    ANSI : Language:=87;
    OEM  : Language:=38;
    NONE : Language:=0;
  end;
  C:=Chr(FHeader.Language);
  try DFile.Seek(29, soFromBeginning); DFile.WriteBuffer(C, 1);
  except raise ED.Create('CodePage: Ошибка записи в заголовок файла'); end;
end;

{*** Установка имени файла ***}
procedure TDbf.SetTableName(Name: String);
begin
  if not FActive then
  begin
    if UpperCase(ExtractFileExt(Name))='.DBF' then FTableName:=Name
    else raise ED.Create('TableName: Недопустимый тип файла');
  end else
    raise ED.Create('TableName: Это свойство нельзя изменять, если файл открыт');
end;

{*** Удаление из строки пробелов слева и справа ***}
function TDbf.TrimSpaces(Text: String): String;
begin
  Result:=Text;     
  // Удаление пробелов слева
  while (Length(Result)>0) and (Result[1]=#32) do
    Result:=Copy(Result, 2, Length(Result)-1);
  // Удаление пробелов справа
  while (Length(Result)>0) and (Result[Length(Result)]=#32) do
    SetLength(Result, Length(Result)-1);
end;

{*** Сохранение заголовка dbf-файла ***}
procedure TDbf.WriteHeader;
var
  N: Integer;
  Field: PField;
  Y,M,D: Word;
  C: Char;
begin
  try
    DecodeDate(Now,Y,M,D);                        // Текущая дата
    with FHeader do begin Year:=Y-1900; Month:=M; Day:=D; end;
    DFile.Seek(0, soFromBeginning);               // Сохранение заголовка
    DFile.WriteBuffer(FHeader, Sizeof(FHeader));
    for N:=1 to FNumFields do                     // Сохранение описаний полей
    begin
      Field:=FieldList.Items[N-1];
      DFile.WriteBuffer(Field^, SizeOf(DBField)); // Завершающий символ $0D
    end;
    C:=#13; DFile.WriteBuffer(C, 1);
  except
    raise ED.Create('WriteHeader: Ошибка сохранения заголовка файла');
  end;
end;

{*** Сохранение записи в dbf-файле ***}
procedure TDbf.WriteRecord;
var Y,M,D: Word;
begin
  try
    with FHeader do begin Y:=Year; M:=Month; D:=Day; end; Y:=Y+1900;
    if EncodeDate(Y,M,D)<>Now then SetDbfDate(Now);  // Дата модификации файла изменилась
    DFile.Seek(FHeader.HeaderSize+((FCurrentRecord-1)*FHeader.RecordSize), soFromBeginning);
    DFile.WriteBuffer(RecordBuffer, FHeader.RecordSize);
    FModified:=False;
  except
    raise ED.Create('WriteRecord: Ошибка сохранения записи');
  end;
end;

constructor TDbf.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FieldList:=TList.Create;
end;

destructor TDbf.Destroy;
begin
  FieldList.Free;
  inherited Destroy;
end;

{*** Добавление описаний поля (при создании нового dbf-файла) ***}
procedure TDbf.AddFieldDefs(FldName: String; FldType: TFLDType; FldSize, FldPrecision: Byte);
var
  IField: PField;
  S: String;
begin
  if not FActive then
  begin
    New(IField); FillChar(IField^, SizeOf(DBField), 0);
    S:=UpperCase(FldName); if Length(S)>10 then SetLength(S, 10);
    StrPCopy(IField^.FieldName, S);
    with IField^ do
    case FldType of
      bfBoolean : FieldType:='L';
      bfDate    : FieldType:='D';
      bfFloat   : FieldType:='F';
      bfNumber  : FieldType:='N';
      bfString  : FieldType:='C';
    end;
    if FldSize<1 then
      raise ED.Create('AddFieldDefs: Недопустимый размер поля') else
    with IField^ do
    begin
      case FldType of
        bfBoolean : FieldSize:=1;
        bfDate    : FieldSize:=8
      else FieldSize:=FldSize; end;
      case FldType of bfFloat, bfNumber:
        if (FldPrecision>0) and (FldPrecision>(FldSize-2)) then
          raise ED.Create('AddFieldDefs: Недопустимое значение FldPrecision')
        else FieldPrecision:=FldPrecision;
      end; // case
    end;
    FieldList.Add(IField);
  end else
  raise ED.Create('AddFieldDefs: В открытом файле нельзя изменять описания полей');
end;

{*** Добавляет новую пустую запись в конец файла и делает ее текущей ***}
procedure TDbf.Append;
var
  I: Integer;
  C: Char;
begin
  try
    Inc(FHeader.RecordCount); Inc(FRecordCount); C:=' ';
    DFile.Seek(FHeader.HeaderSize+
                (FHeader.RecordCount-1)*FHeader.RecordSize, soFromBeginning);
    for I:=1 to FHeader.RecordSize do DFile.WriteBuffer(C, 1);
    C:=#26; DFile.WriteBuffer(C, 1);              // Признак конца файла
    DFile.Seek(0, soFromBeginning);               // Сохранение заголовка
    DFile.WriteBuffer(FHeader, Sizeof(FHeader));
    Last;  // Последняя запись - текущая
  except raise ED.Create('Append: Ошибка сохранения записи'); end;
end;

{*** Отменяет любые изменения в текущей записи, не сохраненные еще в файле ***}
procedure TDbf.Cancel;
begin
  ReadRecord; FModified:=False;
end;

{*** Очищает все поля текущей записи ***}
procedure TDbf.ClearFields;
var N: Integer;                        
begin
  for N:=1 to FNumFields do SetFieldData(N, '');
end;

{*** Закрывает открытый файл ***}
procedure TDbf.Close;
begin
  if FActive then
    begin DFile.Free; ClearHeader; FieldList.Clear; FActive:=False; end
  else raise ED.Create('Close: Файл не открыт');
end;

{*** Создает новый dbf-файл по информации, заданной AddFieldDefs ***}
procedure TDbf.CreateTable;
var
  N: Integer;
  C: Char;
  Field: PField;
begin
  if FActive then
    raise ED.Create('CreateTable: Необходимо закрыть открытый файл')
  else if FTableName='' then
    raise ED.Create('CreateTable: Необходимо указать имя файла')
  else if FieldList.Count=0 then
    raise ED.Create('CreateTable: Не заданы описания полей') else
  begin
    // Если файл с таким уменем уже существует, уничтожаем его
    if FileExists(FTableName) then DeleteFile(FTableName);
    // Создаем новый файл
    DFile:=TFileStream.Create(FTableName, fmCreate);
    FActive:=True;  // dbf-файл создан и открыт
    // Подготовеа информации заголовка
    ClearHeader;    // Очистка записи заголовка
    with FHeader do
    begin
      VerDBF:=3;
      HeaderSize:=32*(1+FieldList.Count)+1;
      RecordSize:=1;
      for N:=0 to FieldList.Count-1 do
      begin
        Field:=FieldList.Items[N];
        RecordSize:=RecordSize+Field^.FieldSize;
      end;
      RecordCount:=0; FNumFields:=FieldList.Count;
    end;
    WriteHeader; C:=#26;
    try DFile.WriteBuffer(C, 1); // Физическая запись в файл
    except raise ED.Create('CreateTable: Ошибка записи EOF'); end;
    Refresh; // Закрытие-открытие файла для получения полного доступа
  end;
end;

{*** Удаляет все записи из файла ***}
procedure TDbf.EmptyTable;
var C: Char;
begin
  FHeader.RecordCount:=0; FRecordCount:=0;
  WriteHeader; C:=#26;
  try DFile.WriteBuffer(C, 1);
  except raise ED.Create('EmptyTable: Ошибка записи EOF'); end;
  RewriteTable;
end;

{*** Делает текущей первую физическую запись в открытом файле ***}
procedure TDbf.First;
begin
  if FRecordCount<1 then raise ED.Create('First: В файле нет записей')
  else begin if FModified then WriteRecord; FCurrentRecord:=1; ReadRecord; end;
end;

{*** Возвращает содержимое поля с номером FieldNo из текущей записи ***}
function TDbf.GetFieldData(FieldNo: Integer): String;
var
  IField: PField;
  S: String;
  N,Addr: Integer;
begin
  if (FieldNo<1) or (FieldNo>FNumFields) then
    raise ED.Create('GetFieldData: Неверный номер поля: '+IntToStr(FieldNo)) else
  begin
    Addr:=2; N:=0; IField:=FieldList.Items[N];
    while N<(FieldNo-1) do
      begin Inc(Addr, IField^.FieldSize); Inc(N); IField:=FieldList.Items[N]; end;
    for N:=Addr to Addr+IField^.FieldSize-1 do S:=S+RecordBuffer[N];
    S:=TrimSpaces(S);
    case IField^.FieldType of
      'C' : if FHeader.Language=38 then Translate(S,S,False);  // OEM -> ANSI
      'D' : if S<>'' then S:=Copy(S,7,2)+'.'+Copy(S,5,2)+'.'+Copy(S,3,2);
      'L' : if S<>'' then case S[1] of 'Y','y','T','t': S:='True';
                                       'N','n','F','f': S:='False'
                          else S:=' '; end;
      'F', 'N' : if Pos('.', S)>0 then S[Pos('.', S)]:=FormatSettings.DecimalSeparator;
    end;
    Result:=S;
  end;
end;

{*** Возвращает имя поля с номером FieldNo ***}
function TDbf.GetFieldName(FieldNo: Integer): String;
var
  IField: PField;
  N: Integer;
  S: String;
begin
  if (FieldNo<1) or (FieldNo>FNumFields) then
    raise ED.Create('GetFieldName: Неверный номер поля: '+IntToStr(FieldNo)) else
  begin
    IField:=FieldList.Items[FieldNo-1];
    N:=0; S:='';
    while IField^.FieldName[N]<>#0 do begin S:=S+IField^.FieldName[N]; Inc(N); end;
    Result:=UpperCase(S);
  end;
end;

{*** Возвращает количество знаков после запятой поля с номером FieldNo ***}
function TDbf.GetFieldPrecision(FieldNo: Integer): Byte;
var IField: PField;
begin
  if (FieldNo<1) or (FieldNo>FNumFields) then
    raise ED.Create('GetFieldPrecision: Неверный номер поля: '+IntToStr(FieldNo)) else
  begin IField:=FieldList.Items[FieldNo-1]; Result:=IField^.FieldPrecision; end;
end;

{*** Возвращает размер (в байтах) поля с номером FieldNo ***}
function TDbf.GetFieldSize(FieldNo: Integer): Byte;
var IField: PField;
begin
  if (FieldNo<1) or (FieldNo>FNumFields) then
    raise ED.Create('GetFieldSize: Неверный номер поля: '+IntToStr(FieldNo)) else
  begin IField:=FieldList.Items[FieldNo-1]; Result:=IField^.FieldSize; end;
end;

{*** Возвращает тип данных поля с номером FieldNo ***}
function TDbf.GetFieldType(FieldNo: Integer): TFLDType;
var IField: PField;
begin
  if (FieldNo<1) or (FieldNo>FNumFields) then
    raise ED.Create('GetFieldType: Неверный номер поля: '+IntToStr(FieldNo)) else
  begin
    IField:=FieldList.Items[FieldNo-1];
    case IField^.FieldType of
      'C': Result:=bfString;
      'N': Result:=bfNumber;
      'F': Result:=bfFloat;
      'L': Result:=bfBoolean;
      'D': Result:=bfDate;
    else Result:=bfUnkown; end;
  end;
end;

{*** Делает текущей физическую запись с номером RecordNo в открытом файле ***}
procedure TDbf.GoToRecord(RecordNo: Integer);
begin
  if FRecordCount<1 then raise ED.Create('GoToRecord: В файле нет записей') else
  if (RecordNo<1) or (RecordNo>FRecordCount) then
    raise ED.Create('GoToRecord: Неверный номер записи') else
  begin if FModified then WriteRecord; FCurrentRecord:=RecordNo; ReadRecord; end;
end;

{*** Вставляет пустую запись на место текущей, сдвигая остальные к концу файла ***}
procedure TDbf.Insert;
var
  CR: LongInt;
begin
  CR:=FCurrentRecord;   // Номер текущей записи
  Append;               // Добавляем пустую запись в конец файла
  if CR=0 then Exit;    // Запись добавлена в ПУСТОЙ файл
  while FCurrentRecord>CR do       // Сдвигаем записи к концу файла
  begin
    Dec(FCurrentRecord); ReadRecord;
    Inc(FCurrentRecord); WriteRecord;
    Dec(FCurrentRecord);
  end;
  ClearFields; Post;    // Очистка полей вставленной записи
end;

{*** Возвращает  True, если в файле нет записей ***}
function TDbf.IsEmpty: Boolean;
begin
  Result:=FRecordCount<1;
end;

{*** Делает текущей последнюю физическую запись в открытом файле ***}
procedure TDbf.Last;
begin
  if FRecordCount<1 then raise ED.Create('Last: В файле нет записей') else
  begin
    if FModified then WriteRecord; FCurrentRecord:=FRecordCount; ReadRecord;
  end;
end;

{*** Делает текущей следующую физическую запись в открытом файле ***}
procedure TDbf.Next;
begin
  if FRecordCount<1 then raise ED.Create('Next: В файле нет записей') else
  if FCurrentRecord<FRecordCount then
  begin
    if FModified then WriteRecord; FCurrentRecord:=FCurrentRecord+1; ReadRecord;
  end;
end;

{*** Открывает указанный (TableName) файл ***}
procedure TDbf.Open;
begin
  if FActive then raise ED.Create('Open: Необходимо закрыть открытый файл') else
  if FileExists(FTableName) then
  begin
    try
      if FExclusive then DFile:=TFileStream.Create(FTableName,
                                     fmOpenReadWrite or fmShareExclusive) else
      DFile:=TFileStream.Create(FTableName, fmOpenReadWrite or fmShareDenyNone);
    except
      raise ED.Create('Файл '+ExtractFileName(FTableName)+
                      ' невозможно открыть в монопольном'+#13+
                      'режиме, т.к. он уже используется другой программой');
    end;
    FActive:=True;  // dbf-файл открыт
    ReadHeader;     // Чтение заголовка dbf-файла
    if FCurrentRecord=1 then begin ReadRecord; FModified:=False; end;
  end else raise ED.Create('Open: Не найден файл '+FTableName);
end;

{*** Удаление из файла пустых записей (упаковка таблицы) ***}
procedure TDbf.PackTable;
var
  R, N, K: LongInt;
  C: Char;
begin
  if FRecordCount<1 then raise ED.Create('PackTable: В файле нет записей') else
  begin
    R:=0; N:=0;
    repeat
      Inc(N); K:=N; FCurrentRecord:=N; ReadRecord;
      if GetDeleteStatus then
      begin
        if K=FRecordCount then Break;
        repeat
          Inc(K); FCurrentRecord:=K; ReadRecord;
        until (K=FRecordCount) or (not GetDeleteStatus);
        if not GetDeleteStatus then
        begin
          R:=N; FCurrentRecord:=R; WriteRecord;
          FCurrentRecord:=K; SetDeleteStatus(True); WriteRecord;
        end;
      end else R:=N;
    until K=FRecordCount;
    if R=0 then EmptyTable else
    begin
      FRecordCount:=R; FHeader.RecordCount:=R;
      WriteHeader; FCurrentRecord:=1;
      try
        C:=#26;
        DFile.Seek(FHeader.HeaderSize+FHeader.RecordCount*FHeader.RecordSize,
                   soFromBeginning);
        DFile.WriteBuffer(C, 1);
      except raise ED.Create('PackTable: Ошибка записи EOF'); end;
    end;
  end;
  RewriteTable;
end;

{*** Сохраняет все изменения текущей записи в файле ***}
// Устанавливает свойство Modified в False
procedure TDbf.Post;
begin
  if FRecordCount>0 then WriteRecord;
end;

{*** Делает текущей предыдущую физическую запись в открытом файле ***}
procedure TDbf.Prior;
begin
  if FRecordCount<1 then raise ED.Create('Prior: В файле нет записей') else
  if FCurrentRecord>1 then
  begin
    if FModified then WriteRecord; FCurrentRecord:=FCurrentRecord-1; ReadRecord;
  end;
end;

{*** Обновление TDbf данными из файла (повторное открытие файла) ***}
procedure TDbf.Refresh;
begin
  if FActive then Close; Open;
end;

{*** В текущей записи замещает содержимое поля с указанным FieldNo данными Data ***}
// Устанавливает свойство Modified в True
procedure TDbf.SetFieldData(FieldNo: Integer; Data: String);
var
  IField: PField;
  S, W: String;
  N, Addr: Longint;
  G, M, D: Word;
begin
  if (FieldNo<1) or (FieldNo>FNumFields) then
    raise ED.Create('SetFieldData: Неверный номер поля: '+IntToStr(FieldNo)) else
  begin
    IField:=FieldList.Items[FieldNo-1];
    W:=TrimRight(Data); // Удаление пробелов и спецсимволов справа
    if (Length(W)<=IField^.FieldSize) or
       ((IField^.FieldType='L') and (Length(W)<=4)) then
    with IField^ do
    begin
      if W<>'' then   // Преобразование данных в зависимости от типа
      try
        case FieldType of
          'L' : if (UpperCase(W)='TRUE') or (UpperCase(W)='YES') then S:='T'
                else if (UpperCase(W)='FALSE') or (UpperCase(W)='NO') then
                        S:='F' else S:=' ';
          'D' : begin
                  G:=StrToInt(Copy(W, 7, 2))+1900; if G<=1950 then G:=G+100;
                  M:=StrToInt(Copy(W, 4, 2)); D:=StrToInt(Copy(W, 1, 2));
                  S:=FormatDateTime('yyyymmdd', EncodeDate(G, M, D));
                end;
      'F','N' : begin
                  if (Pos('.', W)>0) and (FormatSettings.DecimalSeparator=',') then W[Pos('.', W)]:=',';
                  S:=FloatToStrF(StrToFloat(W), ffFixed, FieldSize, FieldPrecision);
                  if (FieldPrecision>0) and (FormatSettings.DecimalSeparator=',') then
                    S[Pos(',', S)]:='.';
                end
        else S:=W; end;
      except raise ED.Create('SetFieldData: Ошибка данных - Ряд: '+
                   IntToStr(FCurrentRecord)+', Поле: '+IntToStr(FieldNo)); end;
      case FieldType of
        'N','F','M': S:=PadLeft(S, FieldSize) // Дополнить пробелами слева
      else
        S:=PadRight(S, FieldSize);            // Дополнить пробелами справа
      end; // case
      Addr:=2; N:=0; IField:=FieldList.Items[N];
      while N<(FieldNo-1) do
      begin Inc(Addr, IField^.FieldSize); Inc(N); IField:=FieldList.Items[N]; end;
      for N:=1 to Length(S) do RecordBuffer[Addr-1+N]:=S[N];
      FModified:=True; // Буфер dbf-файла обновлен
    end else raise ED.Create('SetFieldData: Недопустимый размер данных'+#13+
              'Ряд: '+IntToStr(FCurrentRecord)+', Поле: '+IntToStr(FieldNo));
  end;
end;

{*** Перевод текста Src в текст Dest из формата ANSI в формат OEM и наоборот ***}
procedure TDbf.Translate(Src, Dest: String; ToOEM: Boolean);
var S: String;
begin
  S:=Src;
  if ToOEM then
  begin             // Перекодировка из формата ANSI в формат OEM
    while Pos('І', S)>0 do S[Pos('І', S)]:='I';  // Для корректной работы с украинской
    while Pos('і', S)>0 do S[Pos('і', S)]:='i';  // буквой 'i' (замена на латинскую)
    if Length(S)>0 then CharToOem(PChar(S), PAnsiChar(AnsiString(Dest))) else Dest:='';
  end else
  begin             // Перекодировка из формата OEM в формат ANSI
    if Length(S)>0 then OemToChar(PAnsiChar(AnsiString(S)), PChar(Dest)) else Dest:='';
  end;
end;

end.
