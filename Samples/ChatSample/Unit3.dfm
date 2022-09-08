object Form3: TForm3
  Left = 0
  Top = 0
  Caption = 'Form3'
  ClientHeight = 643
  ClientWidth = 819
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object hChat1: ThChat
    Left = 0
    Top = 49
    Width = 819
    Height = 594
    Align = alClient
    ExplicitLeft = -24
    ExplicitTop = 33
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 819
    Height = 49
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = -6
    ExplicitWidth = 735
    object ButtonLAdd100: TButton
      Left = 75
      Top = 0
      Width = 75
      Height = 49
      Align = alLeft
      Caption = 'add 100'
      TabOrder = 0
      OnClick = ButtonLAdd100Click
      ExplicitLeft = 8
      ExplicitTop = 8
      ExplicitHeight = 25
    end
    object ButtonLAdd1: TButton
      Left = 0
      Top = 0
      Width = 75
      Height = 49
      Align = alLeft
      Caption = 'add 1'
      TabOrder = 1
      OnClick = ButtonLAdd1Click
      ExplicitLeft = 89
      ExplicitTop = 8
      ExplicitHeight = 25
    end
    object ButtonRAdd1: TButton
      Left = 669
      Top = 0
      Width = 75
      Height = 49
      Align = alRight
      Caption = 'add 1'
      TabOrder = 2
      OnClick = ButtonRAdd1Click
      ExplicitLeft = 571
      ExplicitTop = 8
      ExplicitHeight = 25
    end
    object ButtonRAdd100: TButton
      Left = 744
      Top = 0
      Width = 75
      Height = 49
      Align = alRight
      Caption = 'add 100'
      TabOrder = 3
      OnClick = ButtonRAdd100Click
      ExplicitLeft = 652
      ExplicitTop = 8
      ExplicitHeight = 25
    end
    object Button1: TButton
      Left = 150
      Top = 0
      Width = 75
      Height = 49
      Align = alLeft
      Caption = 'add info'
      TabOrder = 4
      OnClick = Button1Click
      ExplicitLeft = 336
      ExplicitTop = 8
      ExplicitHeight = 25
    end
  end
end
