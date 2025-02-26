object AusleiheFormular: TAusleiheFormular
  Left = 0
  Top = 0
  Width = 702
  Height = 441
  Align = alClient
  AutoSize = True
  TabOrder = 0
  object lblAusleihformular: TLabel
    Left = 0
    Top = 0
    Width = 702
    Height = 37
    Align = alTop
    Alignment = taCenter
    Caption = 'Ausleihformular '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -27
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    Layout = tlCenter
    ExplicitWidth = 194
  end
  object lblAusleihen: TLabel
    Left = 0
    Top = 208
    Width = 178
    Height = 30
    Alignment = taCenter
    Caption = 'Aktuelle Ausleihen '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -21
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    Layout = tlCenter
  end
  object DBGrid1: TDBGrid
    Left = 3
    Top = 244
    Width = 686
    Height = 194
    DataSource = DataSource1
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -12
    TitleFont.Name = 'Segoe UI'
    TitleFont.Style = []
  end
  object Sbtn_Rueckgabe: TStyledButton
    Left = 16
    Top = 56
    Width = 75
    Height = 25
    Caption = 'R'#252'ckgabe'
    TabOrder = 1
    StyleElements = [seFont, seBorder]
    OnClick = Sbtn_RueckgabeClick
    StyleDrawType = btRounded
    StyleClass = 'Puerto Rico'
  end
  object DataSource1: TDataSource
    Left = 504
    Top = 48
  end
  object ADOQuery1: TADOQuery
    Parameters = <>
    Left = 576
    Top = 48
  end
end
