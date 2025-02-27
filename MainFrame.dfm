object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'B'#252'cherei Verwaltung'
  ClientHeight = 541
  ClientWidth = 859
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
    Width = 859
    Height = 25
    Anchors = [akLeft, akTop, akRight, akBottom]
    AutoSize = True
    ButtonHeight = 25
    Caption = 'ToolBar1'
    TabOrder = 0
    ExplicitWidth = 697
    DesignSize = (
      859
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
      Width = 28
      Caption = 'ToolButton1'
      Style = tbsSeparator
    end
    object btn_buecher: TButton
      Left = 103
      Top = 0
      Width = 75
      Height = 25
      Align = alTop
      Caption = 'B'#252'cher'
      TabOrder = 1
      OnClick = btn_buecherClick
    end
    object ToolButton2: TToolButton
      Left = 178
      Top = 0
      Width = 28
      Caption = 'ToolButton2'
      ImageIndex = 0
      Style = tbsSeparator
    end
    object btnAusleihe: TButton
      Left = 206
      Top = 0
      Width = 75
      Height = 25
      Align = alTop
      Caption = 'Ausleihe'
      TabOrder = 2
      OnClick = btnAusleiheClick
    end
    object ToolButton3: TToolButton
      Left = 281
      Top = 0
      Width = 28
      Caption = 'ToolButton3'
      ImageIndex = 1
      Style = tbsSeparator
    end
    object btnMahnungsver: TButton
      Left = 309
      Top = 0
      Width = 75
      Height = 25
      Align = alTop
      Caption = 'Mahnungen'
      TabOrder = 3
      OnClick = btnMahnungsverClick
    end
    object ToolButton4: TToolButton
      Left = 384
      Top = 0
      Width = 28
      Caption = 'ToolButton4'
      ImageIndex = 2
      Style = tbsSeparator
    end
    object btnEinstellungen: TButton
      AlignWithMargins = True
      Left = 412
      Top = 0
      Width = 75
      Height = 25
      Align = alTop
      Caption = 'Einstellungen'
      TabOrder = 4
      OnClick = btnEinstellungenClick
    end
    object ToolButton5: TToolButton
      Left = 487
      Top = 0
      Width = 28
      Caption = 'ToolButton5'
      ImageIndex = 3
      Style = tbsSeparator
    end
    object btn_Hauptmanu: TButton
      Left = 515
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
      Left = 616
      Top = 0
      Width = 20
      Caption = 'ToolButton6'
      ImageIndex = 4
      Style = tbsSeparator
    end
    object lblClock: TLabel
      Left = 636
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
    Width = 859
    Height = 516
    Align = alClient
    AutoSize = True
    TabOrder = 1
    ExplicitWidth = 845
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 624
  end
end
