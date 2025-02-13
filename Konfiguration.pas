unit Konfiguration;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ToolWin, Vcl.ComCtrls,
  Vcl.ExtCtrls, Vcl.StdCtrls,
  DatenbankKonfig,   // Enth‰lt TDBKonfigFrame
  MailKonfig;        // Enth‰lt TMailKonfigFrame

type
  TKonfig = class(TFrame)
    ToolBar1: TToolBar;
    pnlContainerKonfig: TPanel;
    btn_MailKonfig: TButton;
    btn_DBKonfig: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btn_DBKonfigClick(Sender: TObject);
    procedure btn_MailKonfigClick(Sender: TObject);
  private
    FCurrentFrame: TFrame;   // Referenz auf den aktuell angezeigten Frame
  public
    // Diese Methode wechselt den Inhalt im pnlContainerKonfig
    procedure SwitchFrame(FrameType: string);
  end;

var
  FormKonfig: TKonfig;

implementation

{$R *.dfm}

procedure TKonfig.FormCreate(Sender: TObject);
var
  DBKonfig: TKonfig_Datenbank;
begin
  // Standardm‰ﬂig soll beim Start die Datenbank-Konfiguration angezeigt werden
  DBKonfig := TKonfig_Datenbank.Create(Self);
  DBKonfig.Parent := pnlContainerKonfig;
  DBKonfig.Align := alClient;
  DBKonfig.Visible := True;
  FCurrentFrame := DBKonfig;
end;

procedure TKonfig.SwitchFrame(FrameType: string);
var
  NewFrame: TFrame;
begin
  // Vorhandenen Frame im Container freigeben
  if Assigned(FCurrentFrame) then
  begin
    FCurrentFrame.Free;
    FCurrentFrame := nil;
  end;

  if FrameType = 'DBKonfig' then
    NewFrame := TKonfig_Datenbank.Create(Self)
  else if FrameType = 'MailKonfig' then
    NewFrame := TKonfig_Mail.Create(Self)
  else
    Exit;  // Unbekannter Frame-Typ

  NewFrame.Parent := pnlContainerKonfig;
  NewFrame.Align := alClient;
  NewFrame.Visible := True;
  FCurrentFrame := NewFrame;
end;

procedure TKonfig.btn_DBKonfigClick(Sender: TObject);
begin
  SwitchFrame('DBKonfig');
end;

procedure TKonfig.btn_MailKonfigClick(Sender: TObject);
begin
  SwitchFrame('MailKonfig');
end;

end.

