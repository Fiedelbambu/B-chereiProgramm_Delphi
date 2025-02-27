object Frame1: TFrame1
  Left = 0
  Top = 0
  Width = 860
  Height = 475
  Anchors = [akLeft, akRight]
  TabOrder = 0
  OnEnter = FrameEnter
  object lblKundenverwaltung: TLabel
    Left = 0
    Top = 0
    Width = 860
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
  object lblSuchfelf: TLabel
    Left = 123
    Top = 68
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
  object lblKundendaten: TLabel
    Left = 0
    Top = 161
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
  object btn_Suchen: TButton
    Left = 664
    Top = 72
    Width = 75
    Height = 25
    Caption = 'Suchen'
    TabOrder = 0
  end
  object DBGrid1: TDBGrid
    Left = 0
    Top = 209
    Width = 857
    Height = 168
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -12
    TitleFont.Name = 'Segoe UI'
    TitleFont.Style = []
  end
  object btn_KundenAnlegen: TButton
    Left = 344
    Top = 417
    Width = 161
    Height = 25
    Caption = 'Neuen Kunden Anlegen'
    TabOrder = 2
    OnClick = btn_KundenAnlegenClick
  end
  object SBoxKundenverwaltung: TSearchBox
    Left = 202
    Top = 73
    Width = 456
    Height = 23
    TabOrder = 3
    Text = 'Suche'
    OnChange = SBoxKundenverwaltungInvokeSearch
  end
  object ADOQuery1: TADOQuery
    Filtered = True
    Parameters = <>
    Left = 800
    Top = 408
  end
end
