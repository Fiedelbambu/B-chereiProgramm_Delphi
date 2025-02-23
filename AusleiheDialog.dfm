object AusleihDialog: TAusleihDialog
  Left = 0
  Top = 0
  Width = 640
  Height = 480
  TabOrder = 0
  object lblAusleihformular: TLabel
    Left = 16
    Top = 180
    Width = 166
    Height = 30
    Caption = 'Name des Buches'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -21
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object lblKundenName: TLabel
    Left = 336
    Top = 180
    Width = 171
    Height = 30
    Caption = 'Name des Kunden'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -21
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object lblfuer: TLabel
    Left = 256
    Top = 180
    Width = 23
    Height = 30
    Caption = 'an'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -21
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object SearchBox: TSearchBox
    Left = 120
    Top = 24
    Width = 186
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
    Left = 550
    Top = 185
    Width = 75
    Height = 25
    Caption = 'Ausleihen'
    TabOrder = 2
    OnClick = BtnAusleihenClick
  end
  object DateTimePicker1: TDateTimePicker
    Left = 120
    Top = 80
    Width = 186
    Height = 23
    Date = 45710.000000000000000000
    Time = 0.588731724536046400
    TabOrder = 3
  end
  object ADOQuery1: TADOQuery
    Parameters = <>
    Left = 568
    Top = 24
  end
  object DataSource1: TDataSource
    DataSet = ADOQuery1
    OnDataChange = DataSource1DataChange
    Left = 488
    Top = 24
  end
end
