object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 787
  ClientWidth = 692
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lbledtOrderID: TLabeledEdit
    Left = 48
    Top = 48
    Width = 121
    Height = 21
    EditLabel.Width = 36
    EditLabel.Height = 13
    EditLabel.Caption = #35746#21333#21495
    TabOrder = 0
    Text = '1234567890'
  end
  object lbledt_Account: TLabeledEdit
    Left = 192
    Top = 48
    Width = 121
    Height = 21
    EditLabel.Width = 24
    EditLabel.Height = 13
    EditLabel.Caption = #36134#25143
    TabOrder = 1
    Text = 'wuyitest22'
  end
  object lbledt_srvID: TLabeledEdit
    Left = 336
    Top = 48
    Width = 121
    Height = 21
    EditLabel.Width = 47
    EditLabel.Height = 13
    EditLabel.Caption = #26381#21153#22120'ID'
    TabOrder = 2
    Text = '2'
  end
  object lbledt_Count: TLabeledEdit
    Left = 480
    Top = 48
    Width = 121
    Height = 21
    EditLabel.Width = 24
    EditLabel.Height = 13
    EditLabel.Caption = #25968#37327
    TabOrder = 3
    Text = '50000'
  end
  object btn_Send: TButton
    Left = 272
    Top = 120
    Width = 75
    Height = 25
    Caption = #21457#36865#35746#21333
    TabOrder = 4
    OnClick = btn_SendClick
  end
  object mmo_Log: TMemo
    Left = 8
    Top = 166
    Width = 633
    Height = 595
    Lines.Strings = (
      'mmo_Log')
    ScrollBars = ssBoth
    TabOrder = 5
  end
  object btn1: TButton
    Left = 609
    Top = 8
    Width = 75
    Height = 25
    Caption = 'btn1'
    TabOrder = 6
    OnClick = btn1Click
  end
  object btn2: TButton
    Left = 526
    Top = 112
    Width = 75
    Height = 25
    Caption = #27979#35797#25552#21462
    TabOrder = 7
    OnClick = btn2Click
  end
  object btn3: TButton
    Left = 8
    Top = 8
    Width = 209
    Height = 25
    Caption = #29983#25104#28165#31354#25968#25454#26381'Sql'
    TabOrder = 8
    OnClick = btn3Click
  end
  object idhtp1: TIdHTTP
    AllowCookies = True
    ProxyParams.BasicAuthentication = False
    ProxyParams.ProxyPort = 0
    Request.ContentLength = -1
    Request.Accept = 'text/html, */*'
    Request.BasicAuthentication = False
    Request.UserAgent = 'Mozilla/3.0 (compatible; Indy Library)'
    HTTPOptions = [hoForceEncodeParams]
    Left = 456
    Top = 112
  end
end
