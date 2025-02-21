unit MainFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.ComCtrls, Vcl.ToolWin,
  Main,          // Enthält TMainDashboard
  Kundenform,    // Enthält TFrame2 (Kundenform)
  Kundenverwaltung,  // Enthält TFrame1 (Kundenverwaltung)
  Buchverwaltung,
  Buchformular,
  Ausleihform,
  Mahnungsverwaltung,
  Konfiguration,
  DatenbankKonfig,
  AusleiheDialog;

type
  TForm1 = class(TForm)
    pnlContainer: TPanel;    // Container, in dem die Frames angezeigt werden
    ToolBar1: TToolBar;
    Button1: TButton;
    ToolButton1: TToolButton;
    btn_buecher: TButton;
    ToolButton2: TToolButton;
    btnAusleihe: TButton;
    ToolButton3: TToolButton;
    btnMahnungsver: TButton;
    ToolButton4: TToolButton;
    btnEinstellungen: TButton;
    ToolButton5: TToolButton;
    btn_Hauptmanu: TButton;
    ToolButton6: TToolButton;
    Timer1: TTimer;
    lblClock: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btn_HauptmanuClick(Sender: TObject);
    procedure btn_buecherClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure btnAusleiheClick(Sender: TObject);
    procedure btnMahnungsverClick(Sender: TObject);
    procedure btnEinstellungenClick(Sender: TObject);
  private
    FDBKonfig: TKonfig_Datenbank;  // Zentrale DB-Konfiguration
    FCurrentFrame: TFrame;   // Referenz auf den aktuell angezeigten Frame
    procedure PrüfeDatenbankVerbindung;  // Prüft die Verbindung zur Datenbank
  public
    property DBKonfig: TKonfig_Datenbank read FDBKonfig write FDBKonfig;
    property CurrentFrame: TFrame read FCurrentFrame write FCurrentFrame;
    // Methode zum Framewechsel (hier wird ein neuer Frame erzeugt)
    procedure ShowKundenform;
    procedure SwitchFrame(FrameType: string);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{ Beim Programmstart wird der MainDashboard-Frame angezeigt }
procedure TForm1.FormCreate(Sender: TObject);
var
  Dashboard: TMainDashboard;
begin
  // Erstelle und zeige das Dashboard
  Dashboard := TMainDashboard.Create(Self);
  Dashboard.Parent := pnlContainer;
  Dashboard.Align := alClient;
  Dashboard.Visible := True;
  FCurrentFrame := Dashboard;

  // Erstelle die zentrale DB-Konfiguration
  FDBKonfig := TKonfig_Datenbank.Create(Self);
  FDBKonfig.InitializeConfig;

  // Prüfe die Datenbankverbindung (zeigt eine Meldung, falls Daten fehlen oder Fehler auftreten)
  PrüfeDatenbankVerbindung;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  lblClock.Caption := FormatDateTime('hh:nn', Now);
end;

procedure TForm1.btnAusleiheClick(Sender: TObject);
var
  AusleiheFormular: TAusleiheFormular;
begin
  if Assigned(FCurrentFrame) then
  begin
    FCurrentFrame.Free;
    FCurrentFrame := nil;
  end;
  AusleiheFormular := TAusleiheFormular.Create(Self);
  AusleiheFormular.Parent := pnlContainer;
  AusleiheFormular.Align := alClient;
  AusleiheFormular.Visible := True;
  FCurrentFrame := AusleiheFormular;
end;

procedure TForm1.PrüfeDatenbankVerbindung;
begin
  if (DBKonfig.EdtServeradresse.Text = '') or (DBKonfig.EdtDb.Text = '') then
  begin
    ShowMessage('⚠️ Fehlende Anmeldedaten!' + sLineBreak +
                'Bitte geben Sie Server und Datenbank in den Einstellungen an.');
    Exit;
  end;

  if DBKonfig.TesteDatenbankVerbindung then
  //  ShowMessage('✅ Verbindung zur Datenbank erfolgreich!')
  else
    ShowMessage('❌ Fehler beim Verbinden zur Datenbank.');
end;

procedure TForm1.btn_buecherClick(Sender: TObject);
var
  Buchverwalter: TBuchverwalter;
begin
  if Assigned(FCurrentFrame) then
  begin
    FCurrentFrame.Free;
    FCurrentFrame := nil;
  end;

  // Erzeuge den Buchverwalter-Frame
  Buchverwalter := TBuchverwalter.Create(Self);
  Buchverwalter.Parent := pnlContainer;
  Buchverwalter.Align := alClient;
  Buchverwalter.Visible := True;

  // Weisen der zentralen Connection aus DBKonfig an die ADOQuery im Buchverwalter
  Buchverwalter.ADOQuery1.Connection := DBKonfig.ADOConnection;

  FCurrentFrame := Buchverwalter;
end;

procedure TForm1.btn_HauptmanuClick(Sender: TObject);
var
  Dashboard: TMainDashboard;
begin
  if Assigned(FCurrentFrame) then
  begin
    FCurrentFrame.Free;
    FCurrentFrame := nil;
  end;
  Dashboard := TMainDashboard.Create(Self);
  Dashboard.Parent := pnlContainer;
  Dashboard.Align := alClient;
  Dashboard.Visible := True;
  FCurrentFrame := Dashboard;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  KundenVerwaltungFrame: TFrame1;
begin
  if Assigned(FCurrentFrame) then
  begin
    FCurrentFrame.Free;
    FCurrentFrame := nil;
  end;
  KundenVerwaltungFrame := TFrame1.Create(Self);
  KundenVerwaltungFrame.Parent := pnlContainer;
  KundenVerwaltungFrame.Align := alClient;
  KundenVerwaltungFrame.Visible := True;
  FCurrentFrame := KundenVerwaltungFrame;
end;

procedure TForm1.btnEinstellungenClick(Sender: TObject);
var
  Konfiguration: TKonfig;
begin
  if Assigned(FCurrentFrame) then
  begin
    FCurrentFrame.Free;
    FCurrentFrame := nil;
  end;
  Konfiguration := TKonfig.Create(Self);
  Konfiguration.Parent := pnlContainer;
  Konfiguration.Align := alClient;
  Konfiguration.Visible := True;
  FCurrentFrame := Konfiguration;
end;

procedure TForm1.btnMahnungsverClick(Sender: TObject);
var
  Mahungsverwalt: TMahungsverwalt;
begin
  if Assigned(FCurrentFrame) then
  begin
    FCurrentFrame.Free;
    FCurrentFrame := nil;
  end;
  Mahungsverwalt := TMahungsverwalt.Create(Self);
  Mahungsverwalt.Parent := pnlContainer;
  Mahungsverwalt.Align := alClient;
  Mahungsverwalt.Visible := True;
  FCurrentFrame := Mahungsverwalt;
end;

procedure TForm1.ShowKundenform;
var
  KundenForm: TFrame2;
begin
  if Assigned(FCurrentFrame) then
  begin
    FCurrentFrame.Free;
    FCurrentFrame := nil;
  end;
  KundenForm := TFrame2.Create(Self);
  KundenForm.Parent := pnlContainer;
  KundenForm.Align := alClient;
  KundenForm.Visible := True;
  FCurrentFrame := KundenForm;
end;

procedure TForm1.SwitchFrame(FrameType: string);
var
  NewFrame: TFrame;
begin
  // Alten Frame schließen
  if Assigned(FCurrentFrame) then
  begin
    FCurrentFrame.Free;
    FCurrentFrame := nil;
  end;

  if FrameType = 'Kundenform' then
  begin
    NewFrame := TFrame2.Create(Self);
  end
  else if FrameType = 'Dashboard' then
  begin
    NewFrame := TMainDashboard.Create(Self);
  end
  else if FrameType = 'AusleihDialog' then
  begin
    NewFrame := TAusleihDialog.Create(Self);
  end
  else if FrameType = 'Kundenverwaltung' then
  begin
    NewFrame := TFrame1.Create(Self);
  end
  else if FrameType = 'Buchverwalter' then
  begin
    // Frame erzeugen
    NewFrame := TBuchform.Create(Self);
    // Connection zuweisen
    TBuchform(NewFrame).ADOQuery.Connection := DBKonfig.ADOConnection;
  end
  else
  begin
    ShowMessage('Unbekannter FrameType: ' + FrameType);
    Exit;
  end;

  // Neuen Frame im Panel anzeigen
  NewFrame.Parent := pnlContainer;
  NewFrame.Align := alClient;
  NewFrame.Visible := True;

  // Referenz merken
  FCurrentFrame := NewFrame;
end;


end.

