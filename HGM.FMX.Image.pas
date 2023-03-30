unit HGM.FMX.Image;

interface

uses
  System.Classes, System.Types, System.SysUtils, FMX.Forms, FMX.Graphics,
  FMX.Objects, System.Threading, System.Generics.Collections;

type
  TBitmapCacheItem = class
  private
    FImage: TBitmap;
    FUrl: string;
    FLoaded: Boolean;
    procedure SetImage(const Value: TBitmap);
    procedure SetLoaded(const Value: Boolean);
    procedure SetUrl(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;
    property Image: TBitmap read FImage write SetImage;
    property Url: string read FUrl write SetUrl;
    property Loaded: Boolean read FLoaded write SetLoaded;
  end;

  TBitmapCache = TArray<TBitmapCacheItem>;

  TCallbackObject = record
    Owner: TComponent;
    Bitmap: TBitmap;
    Url: string;
  end;

  TObjectOwner = class(TComponent)
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  end;

  TBitmapHelper = class helper for TBitmap
  private
    class var
      Pool: TThreadPool;
      DroppingCache: Boolean;
      FCallbackList: TThreadList<TCallbackObject>;
      FObjectOwner: TComponent;
    class procedure AddCallback(Callback: TCallbackObject);
    class procedure RemoveCallback(const AOwner: TComponent);
    class procedure Ready(const Url: string; Stream: TStream);
    class function AppendCache: TBitmapCacheItem;
    class procedure DropCacheLimit;
    class function ExistsCache(const Url: string; var Target: TBitmap): Boolean;
  public
    class var
      PictureCache: TBitmapCache;
      LoadCounter: Integer;
      LoadCounterLimit: Integer;
      CacheSize: Integer;
      GlobalUseCache: Boolean;
    function LoadFromUrl(const Url: string; UseCache: Boolean = True): Boolean;
    function LoadFromUrlAsync(const Url: string; UseCache: Boolean = True; AfterLoaded: TProc<TBitmap> = nil): Boolean; overload;
    procedure LoadFromUrlAsync(AOwner: TComponent; const Url: string; Cache: Boolean = True); overload;
    procedure LoadFromResource(ResName: string); overload;
    procedure LoadFromResource(Instanse: NativeUInt; ResName: string); overload;
    procedure SaveToStream(Stream: TStream; const Ext: string); overload;
    procedure SaveToFile(const AFileName: string; const Ext: string); overload;
    class procedure DropCache;
    class procedure SetLoaded(Url: string);
    class procedure DeleteCache(Url: string);
    class function CreateFromUrl(const Url: string; UseCache: Boolean = True): TBitmap;
    class function CreateLazy(const Url: string; FirstAsDefault: Boolean = False; UseCache: Boolean = True): TBitmap;
    class function CreateFromResource(ResName: string; Url: string = ''): TBitmap;
  end;

implementation

uses
  HGM.Common.Download, FMX.Surfaces, FMX.Types, FMX.Consts;

{ TBitmapHelper }

class procedure TBitmapHelper.AddCallback(Callback: TCallbackObject);
begin
  Callback.Owner.FreeNotification(FObjectOwner);
  FCallbackList.Add(Callback);
end;

class function TBitmapHelper.AppendCache: TBitmapCacheItem;
begin
  DropCacheLimit;
  Result := TBitmapCacheItem.Create;
  SetLength(PictureCache, Length(PictureCache) + 1);
  PictureCache[High(PictureCache)] := Result;
end;

class procedure TBitmapHelper.DropCache;
var
  i: Integer;
begin
  for i := Low(PictureCache) to High(PictureCache) do
    PictureCache[i].Free;
  SetLength(PictureCache, 0);
end;

class procedure TBitmapHelper.DropCacheLimit;
begin
  if GlobalUseCache and not DroppingCache then
  begin
    DroppingCache := True;
    try
      while Length(PictureCache) > CacheSize do
      begin
        PictureCache[0].Free;
        Delete(PictureCache, 0, 1);
      end;
    except
    end;
    DroppingCache := False;
  end;
end;

class function TBitmapHelper.CreateFromResource(ResName, Url: string): TBitmap;
var
  Item: TBitmapCacheItem;
begin
  if GlobalUseCache then
  begin
    Item := AppendCache;
    Item.Image := TBitmap.Create;
    Item.Loaded := True;
    Item.Url := Url;
    Item.Image.LoadFromResource(ResName);
    Result := Item.Image;
  end
  else
  begin
    Result := TBitmap.Create;
    Result.LoadFromResource(ResName);
  end;
end;

class function TBitmapHelper.CreateFromUrl(const Url: string; UseCache: Boolean): TBitmap;
var
  Item: TBitmapCacheItem;
begin
  if GlobalUseCache and UseCache and ExistsCache(Url, Result) then
    Exit;
  if GlobalUseCache then
  begin
    Item := AppendCache;
    Item.Image := TBitmap.Create;
    Item.Loaded := True;
    Item.Url := Url;
    Item.Image.LoadFromUrl(Url, False);
    Result := Item.Image;
  end
  else
  begin
    Result := TBitmap.Create;
    Result.LoadFromUrl(Url, False);
  end;
end;

class function TBitmapHelper.CreateLazy(const Url: string; FirstAsDefault: Boolean; UseCache: Boolean): TBitmap;
var
  Item: TBitmapCacheItem;
begin
  if GlobalUseCache and UseCache and ExistsCache(Url, Result) then
    Exit;
  Item := AppendCache;
  Item.Image := TBitmap.Create;
  Item.Url := Url;
  if FirstAsDefault then
  begin
    if Length(PictureCache) > 1 then
      Item.Image.Assign(PictureCache[0].Image);
  end;
  Result := Item.Image;
end;

class procedure TBitmapHelper.DeleteCache(Url: string);
var
  i: Integer;
begin
  for i := Low(PictureCache) to High(PictureCache) do
  begin
    if PictureCache[i].Url = Url then
    begin
      PictureCache[i].Free;
      Delete(PictureCache, i, 1);
      Exit;
    end;
  end;
end;

class function TBitmapHelper.ExistsCache(const Url: string; var Target: TBitmap): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := Low(PictureCache) to High(PictureCache) do
  begin
    if PictureCache[i].Url = Url then
    begin
      Target := PictureCache[i].Image;
      Exit(True);
    end;
  end;
end;

procedure TBitmapHelper.LoadFromResource(ResName: string);
begin
  LoadFromResource(HInstance, ResName);
end;

procedure TBitmapHelper.LoadFromResource(Instanse: NativeUInt; ResName: string);
var
  Mem: TResourceStream;
begin
  Mem := TResourceStream.Create(Instanse, ResName, RT_RCDATA);
  try
    Self.LoadFromStream(Mem);
  finally
    Mem.Free;
  end;
end;

function TBitmapHelper.LoadFromUrl(const Url: string; UseCache: Boolean): Boolean;
var
  Mem: TMemoryStream;
  Item: TBitmapCacheItem;
  Cache: TBitmap;
  FLoaded: Boolean;
begin
  Result := False;
  if GlobalUseCache and UseCache and ExistsCache(Url, Cache) then
  begin
    Self.Assign(Cache);
    Exit(True);
  end;
  Mem := TDownload.Get(Url);
  try
    try
      if Mem.Size > 0 then
      begin
        TThread.Synchronize(nil,
          procedure
          begin
            try
              Self.LoadFromStream(Mem);
              FLoaded := True;
            except
              FLoaded := False;
            end;
          end);
        Result := FLoaded;
        if GlobalUseCache and UseCache then
        begin
          Item := AppendCache;
          Item.Image := Self;
          Item.Url := Url;
          Item.Loaded := True;
        end;
      end;
    finally
      Mem.Free;
    end;
  except
  end;
end;

procedure TBitmapHelper.LoadFromUrlAsync(AOwner: TComponent; const Url: string; Cache: Boolean);
begin
  var Callback: TCallbackObject;
  Callback.Owner := AOwner;
  Callback.Bitmap := Self;
  Callback.Url := Url;
  AddCallback(Callback);
  TTask.Run(
    procedure
    begin
      try
        var Mem := TDownload.Get(Url);
        //if Cache then
        //  AddCache(Url, Mem);
        TThread.ForceQueue(nil,
          procedure
          begin
            Ready(Url, Mem);
          end);
      except
        TThread.ForceQueue(nil,
          procedure
          begin
            Ready(Url, nil);
          end);
      end;
    end, Pool);
end;

class procedure TBitmapHelper.Ready(const Url: string; Stream: TStream);
begin
  if Assigned(Stream) then
  try
    var List := FCallbackList.LockList;
    try
      for var i := List.Count - 1 downto 0 do
        if List[i].Url = Url then
        begin
          try
            Stream.Position := 0;
            List[i].Bitmap.LoadFromStream(Stream);
          except
            //
          end;
          List.Delete(i);
        end;
    finally
      FCallbackList.UnlockList;
    end;
  finally
    Stream.Free;
  end
  else
  begin
    var List := FCallbackList.LockList;
    try
      for var i := List.Count - 1 downto 0 do
        if List[i].Url = Url then
        begin
          try
            List[i].Bitmap.Assign(nil);
          except
            //
          end;
          List.Delete(i);
        end;
    finally
      FCallbackList.UnlockList;
    end;
  end;
end;

class procedure TBitmapHelper.RemoveCallback(const AOwner: TComponent);
begin
  var List := FCallbackList.LockList;
  try
    for var i := List.Count - 1 downto 0 do
      if List[i].Owner = AOwner then
        List.Delete(i);
  finally
    FCallbackList.UnlockList;
  end;
end;

function TBitmapHelper.LoadFromUrlAsync(const Url: string; UseCache: Boolean; AfterLoaded: TProc<TBitmap>): Boolean;
var
  Cache: TBitmap;
begin
  if GlobalUseCache and UseCache and ExistsCache(Url, Cache) then
  begin
    Self.Assign(Cache);
    Exit(True);
  end;
  Result := False;
  TTask.Run(
    procedure
    begin
      while LoadCounter > LoadCounterLimit do
        TThread.Sleep(500);
      Inc(LoadCounter);
      try
        Self.LoadFromUrl(Url, UseCache);
      except
      end;
      Dec(LoadCounter);
      if Assigned(AfterLoaded) then
      begin
        TThread.ForceQueue(nil,
          procedure
          begin
            AfterLoaded(Self);
          end);
      end;
    end);
end;

procedure TBitmapHelper.SaveToFile(const AFileName, Ext: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(Stream, Ext);
  finally
    Stream.Free;
  end;
end;

procedure TBitmapHelper.SaveToStream(Stream: TStream; const Ext: string);
var
  Surf: TBitmapSurface;
begin
  TMonitor.Enter(Self);
  try
    Surf := TBitmapSurface.Create;
    try
      Surf.Assign(Self);
      if not TBitmapCodecManager.SaveToStream(Stream, Surf, Ext) then
        raise EBitmapSavingFailed.Create(SBitmapSavingFailed);
    finally
      Surf.Free;
    end;
  finally
    TMonitor.Exit(Self);
  end;
end;

class procedure TBitmapHelper.SetLoaded(Url: string);
var
  i: Integer;
begin
  for i := Low(PictureCache) to High(PictureCache) do
    if PictureCache[i].Url = Url then
      PictureCache[i].Loaded := True;
end;

{ TBitmapCacheItem }

constructor TBitmapCacheItem.Create;
begin
  inherited;
  Image := nil;
  Loaded := False;
  Url := '';
end;

destructor TBitmapCacheItem.Destroy;
begin
  {$IFNDEF AUTOREFCOUNT}
  if Assigned(Image) then
    Image.Free;
  {$ENDIF}
  inherited;
end;

procedure TBitmapCacheItem.SetImage(const Value: TBitmap);
begin
  FImage := Value;
end;

procedure TBitmapCacheItem.SetLoaded(const Value: Boolean);
begin
  FLoaded := Value;
end;

procedure TBitmapCacheItem.SetUrl(const Value: string);
begin
  FUrl := Value;
end;

{ TObjectOwner }

procedure TObjectOwner.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation <> TOperation.opRemove then
    Exit;
  var List := TBitmap.FCallbackList.LockList;
  try
    for var i := List.Count - 1 downto 0 do
      if List[i].Owner = AComponent then
        List.Delete(i);
  finally
    TBitmap.FCallbackList.UnlockList;
  end;
end;

initialization
  TBitmap.Pool := TThreadPool.Create;
  TBitmap.DroppingCache := False;
  TBitmap.CacheSize := 60;
  TBitmap.LoadCounterLimit := 20;
  TBitmap.GlobalUseCache := False;
  TBitmap.FCallbackList := TThreadList<TCallbackObject>.Create;
  TBitmap.FObjectOwner := TObjectOwner.Create(nil);

finalization
  TBitmap.DropCache;
  TBitmap.Pool.Free;
  TBitmap.FCallbackList.Free;
  TBitmap.FObjectOwner.Free;

end.

