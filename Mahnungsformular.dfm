object Frame3: TFrame3
  Left = 0
  Top = 0
  Width = 640
  Height = 480
  TabOrder = 0
  object lblKundename: TLabel
    Left = 24
    Top = 64
    Width = 40
    Height = 15
    Caption = 'Kunde :'
  end
  object lblNameDemo: TLabel
    Left = 168
    Top = 64
    Width = 93
    Height = 15
    Caption = '{001} M'#252'ller,Hans'
  end
  object lblAusgelieheneBuecher: TLabel
    Left = 24
    Top = 120
    Width = 117
    Height = 15
    Caption = 'Ausgeliehene B'#252'cher :'
  end
  object lblFris: TLabel
    Left = 40
    Top = 288
    Width = 91
    Height = 15
    Caption = 'Frist'#252'berf'#228'lligkeit'
  end
  object lblBemerkung: TLabel
    Left = 40
    Top = 328
    Width = 94
    Height = 15
    Caption = 'Bemerkunungen :'
  end
  object lblAutomatischberechnet: TLabel
    Left = 192
    Top = 288
    Width = 124
    Height = 15
    Caption = 'Automatisch berechnet'
  end
  object lbl: TLabel
    Left = 0
    Top = 0
    Width = 640
    Height = 37
    Align = alTop
    Alignment = taCenter
    Caption = 'Mahnungsformular'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -27
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    ExplicitWidth = 226
  end
  object btnMahnungsSenden: TButton
    Left = 144
    Top = 424
    Width = 117
    Height = 25
    Caption = 'Mahnung Senden'
    TabOrder = 0
  end
  object ListBox1: TListBox
    Left = 3
    Top = 160
    Width = 454
    Height = 97
    ItemHeight = 15
    TabOrder = 1
  end
end
