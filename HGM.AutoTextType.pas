unit HGM.AutoTextType;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, HGM.Common;

type
  TOnTextChange = procedure(Sender:TObject; Text:string) of object;
  TAutoTypeText = class(TComponent)
   private
    FWait:Integer;
    FText:string;
    i:Integer;
    Str:string;
    DoErase:Boolean;
    DoStop:Boolean;
    Value: string;
    CurrentW:Integer;
    FList:TStringList;
    FTimerNextPhrase: TTimer;
    FTimerNextStep: TTimer;
    FNextPhraseInterval: Cardinal;
    FNextCharInteravl: Cardinal;
    FOnTextChange: TOnTextChange;
    FEditControl: TEdit;
    FMemoControl: TMemo;
    FLabelControl: TLabel;
    procedure TimerNextPhraseTimer(Sender: TObject);
    procedure TimerNextStepTimer(Sender: TObject);
    procedure SetNextPhraseInterval(const Value: Cardinal);
    procedure SetNextCharInteravl(const Value: Cardinal);
    procedure AnimatePhrase;
    procedure AnimateNextStep;
    procedure SetOnTextChange(const Value: TOnTextChange);
    procedure SetText(const Value: string);
    procedure SetEditControl(const Value: TEdit);
    procedure SetLabelControl(const Value: TLabel);
    procedure SetMemoControl(const Value: TMemo);
    procedure SetList(const Value: TStringList);
   public
    constructor Create(AOwner: TComponent); override;
    procedure Start;
    procedure Stop;
   published
    property NextPhraseInterval:Cardinal read FNextPhraseInterval write SetNextPhraseInterval default 2000;
    property NextCharInteravl:Cardinal read FNextCharInteravl write SetNextCharInteravl default 70;
    property OnTextChange:TOnTextChange read FOnTextChange write SetOnTextChange;
    property Text:string read FText;
    property LabelControl:TLabel read FLabelControl write SetLabelControl;
    property EditControl:TEdit read FEditControl write SetEditControl;
    property MemoControl:TMemo read FMemoControl write SetMemoControl;
    property List:TStringList read FList write SetList;
  end;

procedure Register;

implementation
 uses Math;

{ TAutoTypeText }

procedure Register;
begin
 RegisterComponents(PackageName, [TAutoTypeText]);
end;

constructor TAutoTypeText.Create(AOwner: TComponent);
begin
 inherited;
 FList:=TStringList.Create;
 DoStop:=False;
 FNextCharInteravl:=70;
 FNextPhraseInterval:=2000;
 FWait:=FNextCharInteravl;

 FTimerNextPhrase:=TTimer.Create(nil);
 FTimerNextPhrase.Enabled:=False;
 FTimerNextPhrase.Interval:=FNextPhraseInterval;
 FTimerNextPhrase.OnTimer:=TimerNextPhraseTimer;

 FTimerNextStep:=TTimer.Create(nil);
 FTimerNextStep.Enabled:=False;
 FTimerNextStep.Interval:=70;
 FTimerNextStep.OnTimer:=TimerNextStepTimer;

 CurrentW:=-1;
end;

procedure TAutoTypeText.TimerNextPhraseTimer(Sender: TObject);
begin
 FTimerNextPhrase.Enabled:=False;
 Inc(CurrentW);
 if CurrentW > FList.Count-1 then CurrentW:=0;
 Value:=FList[CurrentW];
 AnimatePhrase;
end;

procedure TAutoTypeText.TimerNextStepTimer(Sender: TObject);
begin
 FTimerNextStep.Enabled:=False;
 AnimateNextStep;
end;

procedure TAutoTypeText.SetEditControl(const Value: TEdit);
begin
 FEditControl := Value;
end;

procedure TAutoTypeText.SetLabelControl(const Value: TLabel);
begin
 FLabelControl := Value;
end;

procedure TAutoTypeText.SetList(const Value: TStringList);
begin
 FList := Value;
end;

procedure TAutoTypeText.SetMemoControl(const Value: TMemo);
begin
 FMemoControl := Value;
end;

procedure TAutoTypeText.SetNextCharInteravl(const Value: Cardinal);
begin
 FNextCharInteravl := Value;
 FWait:=Value;
end;

procedure TAutoTypeText.SetNextPhraseInterval(const Value: Cardinal);
begin
 FNextPhraseInterval:=Value;
 FTimerNextPhrase.Interval:=Value;
end;

procedure TAutoTypeText.SetOnTextChange(const Value: TOnTextChange);
begin
  FOnTextChange := Value;
end;

procedure TAutoTypeText.SetText(const Value: string);
begin
 FText:= Value;
 if Assigned(FOnTextChange) then FOnTextChange(Self, FText);
 if Assigned(FEditControl) then FEditControl.Text:=FText;
 if Assigned(FLabelControl) then FLabelControl.Caption:=FText;
 if Assigned(FMemoControl) then FMemoControl.Text:=FText;
end;

procedure TAutoTypeText.Start;
begin
 DoStop:=False;
 FTimerNextPhrase.Enabled:=True;
end;

procedure TAutoTypeText.Stop;
begin
 DoStop:=true;
 FTimerNextPhrase.Enabled:=False;
end;

procedure TAutoTypeText.AnimateNextStep;
var C:Char;
begin
 if (i <= Value.Length) or DoErase then
  begin
   if DoErase then
    begin
     Delete(Str, Str.Length, 1);
     DoErase:=False;
     Dec(i);
    end
   else
    begin
     if Random(10) = 2 then
          C:=Chr(RandomRange(97, 122))
     else C:=Value[i];
     DoErase:=C <> Value[i];
     Str:=Str+C;
     Inc(i);
    end;

   SetText(Str+'|');
   FTimerNextStep.Interval:=FWait;
   if (i > Value.Length) and (not DoErase) then
    FTimerNextStep.Interval:=FWait * 12;
   FTimerNextStep.Enabled:=True;
   Exit;
  end;
 if Str.Length > 0 then
  begin
   Delete(Str, Str.Length, 1);
   SetText(Str+'|');
   FTimerNextStep.Interval:=FWait div 2;
   FTimerNextStep.Enabled:=True;
   Exit;
  end;
 if not DoStop then
  FTimerNextPhrase.Enabled:=True;
end;

procedure TAutoTypeText.AnimatePhrase;
begin
 Str:='';
 DoErase:=False;
 i:=1;
 AnimateNextStep;
end;

end.

