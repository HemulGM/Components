object FormPopup: TFormPopup
  Left = 0
  Top = 0
  BorderStyle = bsNone
  ClientHeight = 532
  ClientWidth = 506
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  Padding.Left = 1
  Padding.Top = 1
  Padding.Right = 1
  Padding.Bottom = 1
  OldCreateOrder = False
  Visible = True
  OnActivate = FormActivate
  OnClick = FormClick
  OnClose = FormClose
  OnHide = FormHide
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object imgUpArrow: TImage
    Left = 1
    Top = 1
    Width = 504
    Height = 12
    Align = alTop
    Center = True
    Picture.Data = {
      0954506E67496D61676589504E470D0A1A0A0000000D49484452000000170000
      000C0806000000893B26F8000000097048597300000B1300000B1301009A9C18
      0000005E4944415478DAB58DC90D00200804A510FA2F8C42343E341E888B072F
      583633141CC3CC514408EDC3C50C2E3B2A804A2DD823D81634302A309F161811
      2C1F08782750430FD8124CC1097825E88E1BB026A8CB0BF028A0D7E056403FC0
      6512EAD82D637D5D06CA0000000049454E44AE426082}
    Visible = False
  end
  object imgDownArrow: TImage
    Left = 1
    Top = 519
    Width = 504
    Height = 12
    Align = alBottom
    Center = True
    Picture.Data = {
      0954506E67496D61676589504E470D0A1A0A0000000D49484452000000170000
      000C0806000000893B26F8000000097048597300000B1300000B1301009A9C18
      0000005D4944415478DAB5CEBB0D00200845511984FD0763100D05C6A8C8F347
      4142732EC4CC397D1AD2F52320224476BC0C285C3F7F193078C06F032D3CC54F
      033DECE2BB8119BCC4D1800787781458C110EE052218C6FB00026FE11640619D
      0244EC2D63CA9DA8B70000000049454E44AE426082}
    Visible = False
    ExplicitLeft = 2
    ExplicitTop = 9
  end
  object Shape1: TShape
    Left = 1
    Top = 13
    Width = 504
    Height = 506
    Align = alClient
    Brush.Color = 1907997
    Pen.Color = 1907997
    ExplicitLeft = 176
    ExplicitTop = 176
    ExplicitWidth = 65
    ExplicitHeight = 65
  end
  object tmrAutoHide: TTimer
    Enabled = False
    Interval = 5000
    OnTimer = tmrAutoHideTimer
    Left = 184
    Top = 184
  end
end
