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
    Top = 328
    Width = 633
    Height = 433
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
  object btn_query: TButton
    Left = 416
    Top = 120
    Width = 217
    Height = 25
    Caption = 'btn_query'
    TabOrder = 6
    OnClick = btn_queryClick
  end
  object btn_create_danyao_alter: TButton
    Left = 8
    Top = 224
    Width = 161
    Height = 25
    Caption = 'btn_create_danyao_alter'
    TabOrder = 7
    OnClick = btn_create_danyao_alterClick
  end
  object btn_checkdata: TButton
    Left = 376
    Top = 224
    Width = 217
    Height = 25
    Caption = 'checkdata'
    TabOrder = 8
    OnClick = btn_checkdataClick
  end
end
