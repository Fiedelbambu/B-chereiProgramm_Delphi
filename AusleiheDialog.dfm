object AusleihDialog: TAusleihDialog
  Left = 0
  Top = 0
  Width = 640
  Height = 480
  TabOrder = 0
  object lblAusleihformular: TLabel
    Left = 29
    Top = 40
    Width = 102
    Height = 25
    Caption = 'Ausleihform'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object SearchBox: TSearchBox
    Left = 216
    Top = 104
    Width = 225
    Height = 23
    TabOrder = 0
    OnInvokeSearch = SearchBoxInvokeSearch
  end
  object DBGrid: TDBGrid
    Left = 3
    Top = 216
    Width = 622
    Height = 249
    Align = alCustom
    DataSource = DataSource1
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -12
    TitleFont.Name = 'Segoe UI'
    TitleFont.Style = []
  end
  object BtnAusleihen: TButton
    Left = 544
    Top = 168
    Width = 75
    Height = 25
    Caption = 'Ausleihen'
    TabOrder = 2
  end
  object ADOQuery1: TADOQuery
    Parameters = <>
    Left = 568
    Top = 24
  end
  object ADOConnection1: TADOConnection
    Left = 464
    Top = 24
  end
  object DataSource1: TDataSource
    DataSet = ADOQuery1
    OnDataChange = DataSource1DataChange
    Left = 392
    Top = 32
  end
end
