object Form8: TForm8
  Left = 0
  Top = 0
  Caption = 'Form8'
  ClientHeight = 580
  ClientWidth = 391
  Color = clSilver
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PanelSelection: TPanel
    Left = 0
    Top = 0
    Width = 391
    Height = 50
    Align = alTop
    BevelOuter = bvNone
    Color = 2826519
    ParentBackground = False
    TabOrder = 0
    Visible = False
    object ButtonFlatReply: TButtonFlat
      AlignWithMargins = True
      Left = 10
      Top = 10
      Width = 116
      Height = 30
      Margins.Left = 10
      Margins.Top = 10
      Margins.Right = 10
      Margins.Bottom = 10
      Align = alLeft
      Caption = #1055#1045#1056#1045#1057#1051#1040#1058#1068' 2'
      ColorNormal = 10841647
      ColorOver = 11236916
      ColorPressed = 11631675
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Segoe UI Semibold'
      Font.Style = []
      FontOver.Charset = DEFAULT_CHARSET
      FontOver.Color = clWhite
      FontOver.Height = -13
      FontOver.Name = 'Segoe UI Semibold'
      FontOver.Style = []
      FontDown.Charset = DEFAULT_CHARSET
      FontDown.Color = clWhite
      FontDown.Height = -13
      FontDown.Name = 'Segoe UI Semibold'
      FontDown.Style = []
      IgnorBounds = True
      RoundRectParam = 0
      Shape = stRoundRect
      ShowFocusRect = False
      TabOrder = 0
      TabStop = True
      TextFormat = [tfCenter, tfSingleLine, tfVerticalCenter]
      SubTextFont.Charset = DEFAULT_CHARSET
      SubTextFont.Color = clWhite
      SubTextFont.Height = -13
      SubTextFont.Name = 'Tahoma'
      SubTextFont.Style = []
    end
  end
  object hChat1: ThChat
    Left = 0
    Top = 50
    Width = 391
    Height = 530
    Align = alClient
    Color = 14927223
    OnSelectionStart = hChat1SelectionStart
    OnSelectionChange = hChat1SelectionChange
    OnSelectionEnd = hChat1SelectionEnd
    ColorInfo = 12820815
    ColorOpponent = clWhite
    ColorMe = 16512738
    ColorSelection = 16574402
    ColorScrollInactive = 14927223
    ColorScrollActive = 13414001
    ColorScrollButton = 9335883
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 10000
    OnTimer = Timer1Timer
    Left = 176
    Top = 194
  end
end
