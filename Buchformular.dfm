object Buchform: TBuchform
  Left = 0
  Top = 0
  Width = 640
  Height = 453
  TabOrder = 0
  DesignSize = (
    640
    453)
  object lblBuchformular: TLabel
    Left = 0
    Top = 0
    Width = 640
    Height = 37
    Align = alTop
    Alignment = taCenter
    Caption = 'Buchformular'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -27
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    ExplicitWidth = 158
  end
  object lblSprache: TLabel
    Left = 46
    Top = 323
    Width = 48
    Height = 15
    Caption = 'Sprache :'
  end
  object lblPlatznummer: TLabel
    Left = 345
    Top = 238
    Width = 77
    Height = 15
    Caption = 'Platznummer :'
  end
  object lblRegal: TLabel
    Left = 59
    Top = 235
    Width = 35
    Height = 15
    Caption = 'Regal :'
  end
  object lblAuflage: TLabel
    Left = 331
    Top = 144
    Width = 47
    Height = 15
    Caption = 'Auflage :'
  end
  object lblHerausgeber: TLabel
    Left = 3
    Top = 144
    Width = 73
    Height = 15
    Caption = 'Herausgeber :'
  end
  object lblAutor: TLabel
    Left = 331
    Top = 104
    Width = 36
    Height = 15
    Caption = 'Autor :'
  end
  object lblISBN: TLabel
    Left = 3
    Top = 104
    Width = 31
    Height = 15
    Caption = 'ISBN :'
  end
  object lblGenre: TLabel
    Left = 331
    Top = 64
    Width = 37
    Height = 15
    Caption = 'Genre :'
  end
  object lblName: TLabel
    Left = 3
    Top = 64
    Width = 38
    Height = 15
    Caption = 'Name :'
  end
  object lblSeitenanzahl: TLabel
    Left = 350
    Top = 323
    Width = 72
    Height = 15
    Caption = 'Seitenanzahl :'
  end
  object lblPublikationsjahr: TLabel
    Left = 3
    Top = 363
    Width = 91
    Height = 15
    Caption = 'Publikationsjahr :'
  end
  object lbl: TLabel
    Left = 0
    Top = 184
    Width = 640
    Height = 31
    Alignment = taCenter
    Anchors = [akTop, akBottom]
    AutoSize = False
    Caption = 'Platzverweis'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -21
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    Layout = tlCenter
  end
  object lblOptional: TLabel
    Left = 0
    Top = 272
    Width = 153
    Height = 30
    Alignment = taCenter
    Anchors = [akTop, akBottom]
    Caption = 'Optionale Felder'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -21
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    Layout = tlCenter
  end
  object EdtPublikationsjahr: TEdit
    Left = 104
    Top = 360
    Width = 121
    Height = 23
    TabOrder = 0
  end
  object EdtSeitenanzahl: TEdit
    Left = 432
    Top = 320
    Width = 121
    Height = 23
    TabOrder = 1
  end
  object EdtSprache: TEdit
    Left = 104
    Top = 320
    Width = 121
    Height = 23
    TabOrder = 2
  end
  object EdtPlatznummer: TEdit
    Left = 432
    Top = 235
    Width = 121
    Height = 23
    TabOrder = 3
  end
  object EdtRegal: TEdit
    Left = 104
    Top = 232
    Width = 121
    Height = 23
    TabOrder = 4
  end
  object EdtAuflage: TEdit
    Left = 432
    Top = 141
    Width = 121
    Height = 23
    TabOrder = 5
  end
  object EdtHerausgeber: TEdit
    Left = 104
    Top = 141
    Width = 121
    Height = 23
    TabOrder = 6
  end
  object EdtAutor: TEdit
    Left = 432
    Top = 104
    Width = 121
    Height = 23
    TabOrder = 7
  end
  object EdtIsbn: TEdit
    Left = 104
    Top = 101
    Width = 121
    Height = 23
    TabOrder = 8
  end
  object EdtGenre: TEdit
    Left = 432
    Top = 61
    Width = 121
    Height = 23
    TabOrder = 9
  end
  object EdtName: TEdit
    Left = 104
    Top = 61
    Width = 121
    Height = 23
    TabOrder = 10
  end
  object btnSpeichern: TButton
    Left = 104
    Top = 408
    Width = 121
    Height = 25
    Caption = 'Speichern'
    TabOrder = 11
    OnClick = btnSpeichernClick
  end
  object btnbearbeiten: TButton
    Left = 264
    Top = 408
    Width = 121
    Height = 25
    Caption = 'Bearbeiten'
    TabOrder = 12
    OnClick = btnBearbeitenClick
  end
  object btnLoeschen: TButton
    Left = 432
    Top = 408
    Width = 121
    Height = 25
    Caption = 'L'#246'schen'
    TabOrder = 13
    OnClick = btnLoeschenClick
  end
  object ADOConnection: TADOConnection
    Left = 608
    Top = 48
  end
  object ADOQuery: TADOQuery
    Parameters = <>
    Left = 608
    Top = 104
  end
end
