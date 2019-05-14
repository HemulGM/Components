unit HGM.License;

interface
 uses
  Vcl.Controls, System.Classes, Winapi.Messages, Winapi.Windows,
  System.SysUtils, Vcl.Graphics, Vcl.ExtCtrls, Vcl.Themes, Vcl.Forms,
  Vcl.ImgList, Vcl.ActnList, System.SyncObjs, System.Types, System.UITypes,

  HGM.Controls.PanelExt, HGM.Common, HGM.WinAPI;

 type
  ThTrueInstance = class(TThread)
   procedure Execute; override;
   procedure Check;
  end;

  ThTrue = class(TComponent)
    class var Instance:ThTrueInstance;
   private
    FIsCreated: Boolean;
    FAppGUID: string;
    procedure SetAppGUID(const Value: string);
   public
    constructor Create(AOwner: TComponent); override;
    class function GetInstance:ThTrueInstance;
    property IsCreated:Boolean read FIsCreated;
   published
    property AppGUID:string read FAppGUID write SetAppGUID;
  end;


procedure Register;

implementation
 uses Math;

procedure Register;
begin
 RegisterComponents(PackageName, [ThTrue]);
end;

{ ThTrue }

constructor ThTrue.Create(AOwner: TComponent);
begin
 inherited;
 FIsCreated:=True;
end;

class function ThTrue.GetInstance: ThTrueInstance;
begin
// if not Assigned(Instance) then Instance:=ThTrue.Create();

end;

procedure ThTrue.SetAppGUID(const Value:string);
begin
 FAppGUID:=Value;
end;

{ ThTrueInstance }

procedure ThTrueInstance.Check;
begin
 Synchronize(
  procedure
  begin
   Halt;
  end);
end;

procedure ThTrueInstance.Execute;
var TS:Cardinal;
begin
 TS:=GetTickCount + 1000 * 5;//(1000 * 60) * 1;
 while TS > GetTickCount do
  begin
   Sleep(1000);
  end;
 Terminate;
 while not Terminated do Sleep(1000);
 Check;
end;
      {
initialization
 ThTrue.Instance:=ThTrueInstance.Create(False);
 ThTrue.Instance.FreeOnTerminate:=True; }

end.
