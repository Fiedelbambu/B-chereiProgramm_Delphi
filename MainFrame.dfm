object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'B'#252'cherei Verwaltung'
  ClientHeight = 513
  ClientWidth = 697
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 15
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 697
    Height = 25
    Anchors = [akLeft, akTop, akRight, akBottom]
    AutoSize = True
    ButtonHeight = 25
    Caption = 'ToolBar1'
    TabOrder = 0
    DesignSize = (
      697
      25)
    object Button1: TButton
      Left = 0
      Top = 0
      Width = 75
      Height = 25
      Align = alTop
      Caption = 'Kunden'
      TabOrder = 0
      OnClick = Button1Click
    end
    object ToolButton1: TToolButton
      Left = 75
      Top = 0
      Width = 14
      Caption = 'ToolButton1'
      Style = tbsSeparator
    end
    object btn_buecher: TButton
      Left = 89
      Top = 0
      Width = 75
      Height = 25
      Align = alTop
      Caption = 'B'#252'cher'
      TabOrder = 1
      OnClick = btn_buecherClick
    end
    object ToolButton2: TToolButton
      Left = 164
      Top = 0
      Width = 13
      Caption = 'ToolButton2'
      ImageIndex = 0
      Style = tbsSeparator
    end
    object btnAusleihe: TButton
      Left = 177
      Top = 0
      Width = 75
      Height = 25
      Align = alTop
      Caption = 'Ausleihe'
      TabOrder = 2
      OnClick = btnAusleiheClick
    end
    object ToolButton3: TToolButton
      Left = 252
      Top = 0
      Width = 13
      Caption = 'ToolButton3'
      ImageIndex = 1
      Style = tbsSeparator
    end
    object btnMahnungsver: TButton
      Left = 265
      Top = 0
      Width = 75
      Height = 25
      Align = alTop
      Caption = 'Mahnungen'
      TabOrder = 3
      OnClick = btnMahnungsverClick
    end
    object ToolButton4: TToolButton
      Left = 340
      Top = 0
      Width = 13
      Caption = 'ToolButton4'
      ImageIndex = 2
      Style = tbsSeparator
    end
    object btnEinstellungen: TButton
      Left = 353
      Top = 0
      Width = 75
      Height = 25
      Align = alTop
      Caption = 'Einstellungen'
      TabOrder = 4
      OnClick = btnEinstellungenClick
    end
    object ToolButton5: TToolButton
      Left = 428
      Top = 0
      Width = 21
      Caption = 'ToolButton5'
      ImageIndex = 3
      Style = tbsSeparator
    end
    object btn_Hauptmanu: TButton
      Left = 449
      Top = 0
      Width = 101
      Height = 25
      Align = alTop
      Caption = 'Hauptmenu'
      TabOrder = 5
      StyleName = 'Windows'
      OnClick = btn_HauptmanuClick
    end
    object ToolButton6: TToolButton
      Left = 550
      Top = 0
      Width = 32
      Caption = 'ToolButton6'
      ImageIndex = 4
      Style = tbsSeparator
    end
    object lblClock: TLabel
      Left = 582
      Top = 0
      Width = 46
      Height = 25
      Alignment = taCenter
      Anchors = [akTop, akBottom]
      BiDiMode = bdRightToLeft
      Caption = 'Clock'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -19
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentBiDiMode = False
      ParentFont = False
    end
  end
  object pnlContainer: TPanel
    Left = 0
    Top = 25
    Width = 697
    Height = 488
    Align = alClient
    AutoSize = True
    TabOrder = 1
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 624
  end
end
