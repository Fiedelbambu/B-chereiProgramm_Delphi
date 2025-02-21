object Frame1: TFrame1
  Left = 0
  Top = 0
  Width = 725
  Height = 475
  Anchors = [akLeft, akRight]
  TabOrder = 0
  OnEnter = FrameEnter
  object lblKundenverwaltung: TLabel
    Left = 0
    Top = 0
    Width = 725
    Height = 37
    Align = alTop
    Alignment = taCenter
    Caption = 'Kundenverwaltung'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -27
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    ExplicitWidth = 223
  end
  object PaintBox1: TPaintBox
    Left = 0
    Top = 43
    Width = 637
    Height = 8
  end
  object lblSuchfelf: TLabel
    Left = 3
    Top = 76
    Width = 73
    Height = 25
    Caption = 'Suchfeld'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object PaintBox2: TPaintBox
    Left = 0
    Top = 123
    Width = 637
    Height = 8
  end
  object lblKundendaten: TLabel
    Left = 3
    Top = 137
    Width = 144
    Height = 32
    Alignment = taCenter
    Caption = 'Kundendaten'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -24
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object PaintBox3: TPaintBox
    Left = 3
    Top = 395
    Width = 637
    Height = 8
  end
  object PaintBox4: TPaintBox
    Left = 3
    Top = 195
    Width = 637
    Height = 8
  end
  object btn_Suchen: TButton
    Left = 544
    Top = 80
    Width = 75
    Height = 25
    Caption = 'Suchen'
    TabOrder = 0
  end
  object DBGrid1: TDBGrid
    Left = 0
    Top = 201
    Width = 722
    Height = 176
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -12
    TitleFont.Name = 'Segoe UI'
    TitleFont.Style = []
  end
  object btn_KundenAnlegen: TButton
    Left = 240
    Top = 417
    Width = 161
    Height = 25
    Caption = 'Neuen Kunden Anlegen'
    TabOrder = 2
    OnClick = btn_KundenAnlegenClick
  end
  object SBoxKundenverwaltung: TSearchBox
    Left = 82
    Top = 81
    Width = 456
    Height = 23
    TabOrder = 3
    Text = 'Suche'
    OnChange = SBoxKundenverwaltungInvokeSearch
  end
  object ADOQuery1: TADOQuery
    Filtered = True
    Parameters = <>
    Left = 664
    Top = 72
  end
end
