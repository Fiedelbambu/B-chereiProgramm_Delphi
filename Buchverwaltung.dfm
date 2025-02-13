object Buchverwalter: TBuchverwalter
  Left = 0
  Top = 0
  Width = 640
  Height = 449
  TabOrder = 0
  DesignSize = (
    640
    449)
  object lblBuchverwaltung: TLabel
    Left = 0
    Top = 0
    Width = 640
    Height = 37
    Align = alTop
    Alignment = taCenter
    Caption = 'Buch suche und Verwaltung'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -27
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    ExplicitWidth = 325
  end
  object PaintBox1: TPaintBox
    Left = 0
    Top = 43
    Width = 637
    Height = 14
  end
  object lblName: TLabel
    Left = 54
    Top = 88
    Width = 38
    Height = 15
    Caption = 'Name :'
  end
  object lblGenre: TLabel
    Left = 288
    Top = 88
    Width = 37
    Height = 15
    Caption = 'Genre :'
  end
  object lblISBN: TLabel
    Left = 61
    Top = 136
    Width = 31
    Height = 15
    Caption = 'ISBN :'
  end
  object lblAutor: TLabel
    Left = 289
    Top = 136
    Width = 36
    Height = 15
    Caption = 'Autor :'
  end
  object lblHerausgeber: TLabel
    Left = 16
    Top = 176
    Width = 76
    Height = 15
    Caption = 'Herausgeber : '
  end
  object lblSucherergebnisse: TLabel
    Left = 3
    Top = 224
    Width = 167
    Height = 32
    Alignment = taCenter
    Anchors = [akTop, akBottom]
    Caption = 'Suchergebnisse'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -24
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    Layout = tlCenter
  end
  object btn_BuchAnlegen: TButton
    Left = 264
    Top = 408
    Width = 115
    Height = 25
    Caption = 'Neues Buch Anlegen'
    TabOrder = 0
    OnClick = btn_BuchAnlegenClick
  end
  object EdtName: TEdit
    Left = 128
    Top = 85
    Width = 121
    Height = 23
    TabOrder = 1
  end
  object EdtGenre: TEdit
    Left = 344
    Top = 85
    Width = 121
    Height = 23
    TabOrder = 2
  end
  object EdtISBN: TEdit
    Left = 128
    Top = 133
    Width = 121
    Height = 23
    TabOrder = 3
  end
  object EdtAutor: TEdit
    Left = 344
    Top = 133
    Width = 121
    Height = 23
    TabOrder = 4
  end
  object EdtHerausgeber: TEdit
    Left = 128
    Top = 173
    Width = 121
    Height = 23
    TabOrder = 5
  end
  object btnSuche: TButton
    Left = 344
    Top = 176
    Width = 75
    Height = 25
    Caption = 'Suchen'
    TabOrder = 6
  end
  object DBGrid1: TDBGrid
    Left = 0
    Top = 256
    Width = 637
    Height = 120
    TabOrder = 7
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -12
    TitleFont.Name = 'Segoe UI'
    TitleFont.Style = []
  end
end
