object Form8: TForm8
  Left = 0
  Top = 0
  Caption = 'Form8'
  ClientHeight = 345
  ClientWidth = 617
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
    Left = 88
    Top = 115
    Width = 150
    Height = 17
    TabOrder = 0
  end
  object hProgrsssBar1: ThProgrsssBar
    Left = 0
    Top = 0
    Width = 617
    Height = 33
    Align = alTop
    DoubleBuffered = True
    ColorScale = 10711881
    ColorBackground = 2102799
    ParentBackground = False
    Position = 25
    ParentColor = True
    Kind = pbkRoundRect
    RoundRadius = 15
    ExplicitLeft = 3
    ExplicitTop = 8
  end
  object TrackBar1: TTrackBar
    Left = 309
    Top = 112
    Width = 300
    Height = 45
    Max = 100
    Position = 1
    TabOrder = 2
    OnChange = TrackBar1Change
  end
  object Panel1: TPanel
    Left = 8
    Top = 147
    Width = 185
    Height = 41
    Caption = 'Panel1'
    TabOrder = 3
  end
  object hTrackbar1: ThTrackbar
    AlignWithMargins = True
    Left = 3
    Top = 36
    Width = 611
    Height = 29
    Align = alTop
    Position = 6.560000000000000000
    SecondPosition = 20.000000000000000000
    OnChange = hTrackbar1Change
    Color = 6901811
    ParentColor = False
    HotScroll = True
  end
  object hColorGrid1: ThColorGrid
    Left = 320
    Top = 192
    Width = 100
    Height = 100
    BorderStyle = bsNone
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 5
  end
  object EditEx1: TEditEx
    Left = 8
    Top = 194
    Width = 185
    Height = 23
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    AutoSelect = True
    TabOrder = 6
    ReadOnly = False
    Color = 8736536
    Text = '  .  .    '
    Mode = emDate
    ShowClearButton = True
    ShowEditButton = True
    NumberOnly = False
  end
  object hTrue1: ThTrue
    Left = 104
    Top = 224
  end
end
