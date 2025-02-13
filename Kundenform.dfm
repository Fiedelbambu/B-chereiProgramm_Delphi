object Frame2: TFrame2
  Left = 0
  Top = 0
  Width = 640
  Height = 450
  TabOrder = 0
  object lblName: TLabel
    Left = 3
    Top = 139
    Width = 32
    Height = 15
    Caption = 'Name'
  end
  object lblVorname: TLabel
    Left = 299
    Top = 139
    Width = 47
    Height = 15
    Caption = 'Vorname'
  end
  object lblPLZ: TLabel
    Left = 3
    Top = 363
    Width = 60
    Height = 15
    Caption = 'Postleitzahl'
  end
  object lblWohnort: TLabel
    Left = 299
    Top = 363
    Width = 47
    Height = 15
    Caption = 'Wohnort'
  end
  object lblHausnummer: TLabel
    Left = 299
    Top = 334
    Width = 73
    Height = 15
    Caption = 'Hausnummer'
  end
  object lblEmail: TLabel
    Left = 299
    Top = 184
    Width = 34
    Height = 15
    Caption = 'E-Mail'
  end
  object lblTelefonnummer: TLabel
    Left = 0
    Top = 227
    Width = 85
    Height = 15
    Caption = 'Telefonnummer'
  end
  object lblKundenformular: TLabel
    Left = 0
    Top = 0
    Width = 640
    Height = 37
    Align = alTop
    Alignment = taCenter
    Caption = 'Kundenformular'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -27
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
    ExplicitWidth = 211
  end
  object lblPDaten: TLabel
    Left = 0
    Top = 71
    Width = 192
    Height = 32
    Caption = 'Pers'#246'nliche Daten'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -24
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object pBoxFormular: TPaintBox
    Left = 0
    Top = 40
    Width = 639
    Height = 25
    Margins.Left = 1
    Margins.Right = 1
    Color = clBackground
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGrayText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object PBoxPDaten: TPaintBox
    Left = 3
    Top = 104
    Width = 636
    Height = 17
  end
  object lblGeburtsdatum: TLabel
    Left = 3
    Top = 184
    Width = 76
    Height = 15
    Caption = 'Geburtsdatum'
  end
  object PBoxzwischenDatenundAdresse: TPaintBox
    Left = 0
    Top = 253
    Width = 640
    Height = 20
  end
  object lblAdresse: TLabel
    Left = 0
    Top = 279
    Width = 83
    Height = 32
    Caption = 'Adresse'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -24
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object lblStrasse: TLabel
    Left = 3
    Top = 336
    Width = 33
    Height = 15
    Caption = 'Stra'#223'e'
  end
  object PBoxUntenKunden: TPaintBox
    Left = 3
    Top = 389
    Width = 634
    Height = 15
  end
  object PBoxAdresse: TPaintBox
    Left = 0
    Top = 310
    Width = 639
    Height = 15
  end
  object edtName: TEdit
    Left = 96
    Top = 139
    Width = 186
    Height = 23
    TabOrder = 0
  end
  object edtVorname: TEdit
    Left = 361
    Top = 139
    Width = 186
    Height = 23
    TabOrder = 1
  end
  object edtPostleitzahl: TEdit
    Left = 96
    Top = 360
    Width = 121
    Height = 23
    TabOrder = 2
  end
  object edtWohnort: TEdit
    Left = 394
    Top = 360
    Width = 121
    Height = 23
    TabOrder = 3
  end
  object edtHausnummer: TEdit
    Left = 394
    Top = 331
    Width = 121
    Height = 23
    TabOrder = 4
  end
  object edtEmail: TEdit
    Left = 361
    Top = 181
    Width = 186
    Height = 23
    TabOrder = 5
  end
  object Edit2: TEdit
    Left = 96
    Top = 224
    Width = 186
    Height = 23
    TabOrder = 6
  end
  object DateTimePicker1: TDateTimePicker
    Left = 96
    Top = 184
    Width = 186
    Height = 23
    Date = 45700.000000000000000000
    Time = 0.718569317126821300
    TabOrder = 7
  end
  object EdtStrasse: TEdit
    Left = 96
    Top = 333
    Width = 186
    Height = 23
    TabOrder = 8
  end
  object btn_speichern_KdnForm: TButton
    Left = 96
    Top = 410
    Width = 75
    Height = 25
    Caption = 'Speichern'
    TabOrder = 9
  end
  object btn_bearbeiten_KdnForm: TButton
    Left = 240
    Top = 410
    Width = 75
    Height = 25
    Caption = 'Bearbeiten'
    TabOrder = 10
  end
  object btn_Loeschen_KdnForm: TButton
    Left = 384
    Top = 410
    Width = 75
    Height = 25
    Caption = 'L'#246'schen'
    TabOrder = 11
  end
end
