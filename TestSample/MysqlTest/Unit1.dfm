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
  object mmo_Log: TMemo
    Left = 8
    Top = 166
    Width = 633
    Height = 595
    ImeName = #20013#25991'('#31616#20307') - '#25628#29399#25340#38899#36755#20837#27861
    Lines.Strings = (
      'mmo_Log')
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object btn3: TButton
    Left = 8
    Top = 8
    Width = 209
    Height = 25
    Caption = #29983#25104#28165#31354#25968#25454#26381'Sql'
    TabOrder = 1
    OnClick = btn3Click
  end
  object btn_showCreateTable: TButton
    Left = 416
    Top = 8
    Width = 217
    Height = 25
    Caption = #26174#31034#21019#34920'Sql'
    TabOrder = 2
    OnClick = btn_showCreateTableClick
  end
  object btn_EngineStart: TButton
    Left = 8
    Top = 80
    Width = 209
    Height = 25
    Caption = #21551#21160#25968#25454#24341#25806
    TabOrder = 3
    OnClick = btn_EngineStartClick
  end
  object btn_mysqlDateTime: TButton
    Left = 416
    Top = 80
    Width = 217
    Height = 25
    Caption = 'btn_mysqlDateTime'
    TabOrder = 4
    OnClick = btn_mysqlDateTimeClick
  end
  object btn_insert: TButton
    Left = 8
    Top = 120
    Width = 209
    Height = 25
    Caption = 'btn_insert'
    TabOrder = 5
    OnClick = btn_insertClick
  end
end
