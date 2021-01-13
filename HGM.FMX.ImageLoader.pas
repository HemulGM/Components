unit HGM.FMX.ImageLoader;

interface

uses
  System.Classes, System.Types, System.SysUtils, System.Generics.Collections,
  System.Threading, FMX.ImgList, FMX.Graphics;

type
  TPreview = class(TBitmap)
  public
    Loading: Boolean;
    URL: string;
    ImageName: string;
  end;

  TPreviews = class(TObjectList<TPreview>)
  private
    class var
      Instance: TPreviews;
      CacheSize: Integer;
  private
    FImages: TImageList;
    FLoadingPoll: TThreadPool;
    FPath: string;
    FPreviewSize: TSizeF;
    procedure SetImages(const Value: TImageList);
    procedure Drop;
    procedure SetPath(const Value: string);
    procedure SetPreviewSize(const Value: TSizeF);
  public
    constructor Create;
    destructor Destroy; override;
    property Images: TImageList read FImages write SetImages;
    property Path: string read FPath write SetPath;
    property PreviewSize: TSizeF read FPreviewSize write SetPreviewSize;
    class function GetInstance: TPreviews;
    class function GetByURL(const URL: string): TPreview;
    class function GetByName(const ImageName: string): TPreview;
    class function GetPreview(const URL, ImageName: string; const DefaultIndex: Integer): TBitmap;
    class function GetDefualt(const ImageIndex: Integer): TBitmap;
    class procedure Reset;
  end;

implementation

uses
  System.IOUtils, System.Net.HttpClient;

{ TPreviews }

class procedure TPreviews.Reset;
var
  FOldPool: TThreadPool;
begin
  with GetInstance do
  begin
    if Assigned(FLoadingPoll) then
    begin
      FOldPool := FLoadingPoll;
      TThread.CreateAnonymousThread(
        procedure
        begin
          FOldPool.Free;
        end).Start;
    end;
    FLoadingPoll := TThreadPool.Create;
  end;
end;

constructor TPreviews.Create;
begin
  inherited;
  FPreviewSize := TSizeF.Create(256, 256);
end;

destructor TPreviews.Destroy;
begin
  if Assigned(FLoadingPoll) then
    FLoadingPoll.Free;
  inherited;
end;

procedure TPreviews.Drop;
var
  i, c: Integer;
begin
  if Count > (CacheSize * 2) then
  begin
    i := 0;
    c := 0;
    while i < CacheSize do
    begin
      Inc(i);
      if not Items[c].Loading then
        Delete(c)
      else
        Inc(c);
    end;
  end;
end;

class function TPreviews.GetByName(const ImageName: string): TPreview;
var
  Item: TPreview;
begin
  Result := nil;
  for Item in GetInstance do
    if Item.ImageName = ImageName then
      Exit(Item);
end;

class function TPreviews.GetByURL(const URL: string): TPreview;
var
  Item: TPreview;
begin
  Result := nil;
  for Item in GetInstance do
    if Item.URL = URL then
      Exit(Item);
end;

class function TPreviews.GetDefualt(const ImageIndex: Integer): TBitmap;
begin
  Result := nil;
  with GetInstance do
  begin
    if Assigned(Images) then
    begin
      Result := Images.Bitmap(FPreviewSize, ImageIndex);
    end;
  end;
end;

class function TPreviews.GetInstance: TPreviews;
begin
  if not Assigned(Instance) then
  begin
    Instance := TPreviews.Create;
    Instance.CacheSize := 100;
  end;
  Result := Instance;
end;

class function TPreviews.GetPreview(const URL, ImageName: string; const DefaultIndex: Integer): TBitmap;
var
  Item: TPreview;
  FN: string;
begin
  with GetInstance do
  begin
    Drop;
    //Find to cache
    Item := GetByURL(URL);
    if Assigned(Item) then
      Exit(Item)
    else
    begin    
      //Find to cache
      Item := GetByName(ImageName);
      if Assigned(Item) then
        Exit(Item)
      else
        Item := TPreview.Create;
    end;

    //Find and load from FS
    FN := TPath.Combine(FPath, ImageName);
    if TFile.Exists(FN) then
    begin
      try
        Item.LoadThumbnailFromFile(FN, FPreviewSize.Width, FPreviewSize.Height);
        Item.URL := URL;
        Item.ImageName := ImageName;
        Add(Item);
        Exit(Item);
      except
        TFile.Delete(FN);
      end;
    end;

    //Load from server
    Item.Assign(GetDefualt(DefaultIndex));
    Item.URL := URL;
    Item.Loading := True;
    Add(Item);
    Result := Item;

    //Loading
    TTask.Run(
      procedure
      var
        HTTP: THTTPClient;
        Mem: TMemoryStream;
      begin
        HTTP := THTTPClient.Create;
        try
          Mem := TMemoryStream.Create;
          try
            if HTTP.Get(URL, Mem).StatusCode = 200 then
            begin
              Mem.Position := 0;
              TThread.Synchronize(nil,
                procedure
                begin
                  Item.LoadFromStream(Mem);
                end);
              if TDirectory.Exists(FPath) then
              begin
                try
                  TFile.Create(FN).Free;
                  Mem.SaveToFile(FN);
                except
                  if TFile.Exists(FN) then
                    TFile.Delete(FN);
                end;
              end;
            end;
          finally
            Mem.Free;
          end;
        except
          if TFile.Exists(FN) then
            TFile.Delete(FN);
        end;
        Item.Loading := False;
        HTTP.Free;
      end, FLoadingPoll);
  end;
end;

procedure TPreviews.SetImages(const Value: TImageList);
begin
  FImages := Value;
end;

procedure TPreviews.SetPath(const Value: string);
begin
  FPath := Value;
end;

procedure TPreviews.SetPreviewSize(const Value: TSizeF);
begin
  FPreviewSize := Value;
end;

initialization

finalization
  TPreviews.GetInstance.Free;

end.

