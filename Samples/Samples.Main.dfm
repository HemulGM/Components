object Form8: TForm8
  Left = 0
  Top = 0
  Caption = 'Form8'
  ClientHeight = 517
  ClientWidth = 578
  Color = clSilver
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object ProgressBar1: TProgressBar
    Left = 144
    Top = 195
    Width = 150
    Height = 17
    TabOrder = 0
  end
  object hProgrsssBar1: ThProgrsssBar
    Left = 8
    Top = 16
    Width = 562
    Height = 33
    DoubleBuffered = True
    ColorScale = 10711881
    ColorBackground = 2102799
    ParentBackground = False
    Position = 3
    ParentColor = True
    Kind = pbkRoundRect
    RoundRadius = 15
  end
  object TrackBar1: TTrackBar
    Left = 96
    Top = 144
    Width = 300
    Height = 45
    Max = 100
    Position = 1
    TabOrder = 2
    OnChange = TrackBar1Change
  end
  object Panel1: TPanel
    Left = 184
    Top = 243
    Width = 185
    Height = 41
    Caption = 'Panel1'
    TabOrder = 3
  end
end
