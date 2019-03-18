object Form8: TForm8
  Left = 0
  Top = 0
  Caption = 'Form8'
  ClientHeight = 300
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object hColorGrid1: ThColorGrid
    Left = 8
    Top = 8
    Width = 100
    Height = 100
    OnSelect = hColorGrid1Select
    BorderStyle = bsNone
    DoubleBuffered = True
    ParentColor = True
    ParentDoubleBuffered = False
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 8
    Top = 111
    Width = 97
    Height = 41
    Caption = 'Panel1'
    Color = clCream
    ParentBackground = False
    TabOrder = 1
  end
  object Button1: TButton
    Left = 8
    Top = 158
    Width = 97
    Height = 25
    Caption = 'Button1'
    TabOrder = 2
    OnClick = Button1Click
  end
  object ColorGrid1: ThColorGrid
    Left = 207
    Top = 8
    Width = 247
    Height = 248
    OnSelect = hColorGrid1Select
    BevelInner = bvNone
    BorderStyle = bsNone
    DoubleBuffered = True
    ParentColor = True
    ParentDoubleBuffered = False
    TabOrder = 3
  end
  object ColorDialog1: TColorDialog
    Left = 112
    Top = 200
  end
end
