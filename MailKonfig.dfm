object Konfig_Mail: TKonfig_Mail
  Left = 0
  Top = 0
  Width = 640
  Height = 433
  Align = alClient
  TabOrder = 0
  object lblMailKonfig: TLabel
    Left = 0
    Top = 0
    Width = 640
    Height = 37
    Align = alTop
    Alignment = taCenter
    Caption = 'Mail konfiguration'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -27
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    ExplicitWidth = 219
  end
  object lblPasswort: TLabel
    Left = 288
    Top = 197
    Width = 53
    Height = 15
    Caption = 'Passwort :'
  end
  object lblBenutzername: TLabel
    Left = 288
    Top = 157
    Width = 82
    Height = 15
    Caption = 'Benutzername :'
  end
  object lblPortImap: TLabel
    Left = 288
    Top = 117
    Width = 28
    Height = 15
    Caption = 'Port :'
  end
  object lblHostImap: TLabel
    Left = 288
    Top = 77
    Width = 31
    Height = 15
    Caption = 'Host :'
  end
  object LblOptional: TLabel
    Left = 40
    Top = 200
    Width = 126
    Height = 15
    Caption = '(Optional) IMAP/POP3 :'
  end
  object lblProt: TLabel
    Left = 56
    Top = 160
    Width = 28
    Height = 15
    Caption = 'Port :'
  end
  object lblHost: TLabel
    Left = 53
    Top = 120
    Width = 31
    Height = 15
    Caption = 'Host :'
  end
  object lblSmtp: TLabel
    Left = 16
    Top = 72
    Width = 68
    Height = 15
    Caption = 'SMTP-Server'
  end
  object lblVerschluesselung: TLabel
    Left = 48
    Top = 240
    Width = 90
    Height = 15
    Caption = 'Verschl'#252'sselung :'
  end
  object btnTestEmail: TButton
    Left = 80
    Top = 312
    Width = 118
    Height = 25
    Caption = 'Test-Email-Versand'
    TabOrder = 0
  end
  object EdtBenutzername: TEdit
    Left = 440
    Top = 154
    Width = 121
    Height = 23
    TabOrder = 1
  end
  object EdtPortImap: TEdit
    Left = 440
    Top = 114
    Width = 121
    Height = 23
    TabOrder = 2
  end
  object EdtHostImap: TEdit
    Left = 440
    Top = 69
    Width = 121
    Height = 23
    TabOrder = 3
  end
  object EdtPort: TEdit
    Left = 104
    Top = 157
    Width = 121
    Height = 23
    TabOrder = 4
  end
  object EdtHost: TEdit
    Left = 104
    Top = 117
    Width = 121
    Height = 23
    TabOrder = 5
  end
  object EdtSMTP: TEdit
    Left = 104
    Top = 69
    Width = 121
    Height = 23
    TabOrder = 6
  end
  object EdtPasswort: TEdit
    Left = 440
    Top = 194
    Width = 121
    Height = 23
    TabOrder = 7
  end
  object ComboBox: TComboBox
    Left = 154
    Top = 237
    Width = 145
    Height = 23
    TabOrder = 8
    Text = 'ComboBox1'
  end
  object btnSpeichern: TButton
    Left = 256
    Top = 312
    Width = 75
    Height = 25
    Caption = 'Speichern'
    TabOrder = 9
  end
  object btnBearbeiten: TButton
    Left = 376
    Top = 312
    Width = 75
    Height = 25
    Caption = 'Bearbeiten'
    TabOrder = 10
  end
end
