unit HGM.WinAPI.ShellDlg;

interface

uses
  Vcl.Dialogs, System.UITypes;

function AskYesNo(aCaption, aQuestion: string): Boolean;

implementation

function AskYesNo(aCaption, aQuestion: string): Boolean;
begin
  Result := False;
  with TTaskDialog.Create(nil) do
  begin
    Caption := aCaption;
    CommonButtons := [tcbYes, tcbNo];
    DefaultButton := tcbYes;
    Flags := [tfUseHiconMain, tfAllowDialogCancellation, tfUseCommandLinksNoIcon];
    MainIcon := 0;
    Title := aQuestion;
    try
      if Execute then
        Result := ModalResult = idYes
      else
        Result := False;
    finally
      Free;
    end;
  end;
end;

end.

