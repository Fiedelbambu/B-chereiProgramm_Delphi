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
  DatenbankKonfig;

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
    FCurrentFrame: TFrame;   // Referenz auf den aktuell angezeigten Frame
    DBKonfig: TKonfig_Datenbank;  // 🔹 Datenbank-Konfigurationsobjekt hinzugefügt
     procedure PrüfeDatenbankVerbindung;  // 🔹 Neue Methode für Datenbankprüfung

  public
    // Diese Methode wechselt den Containerinhalt auf den Kundenform
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
  Dashboard := TMainDashboard.Create(Self);
  Dashboard.Parent := pnlContainer;
  Dashboard.Align := alClient;
  Dashboard.Visible := True;
  FCurrentFrame := Dashboard;

   // 🔹 Datenbank-Konfiguration initialisieren
  DBKonfig := TKonfig_Datenbank.Create(Self);
  DBKonfig.InitializeConfig;

  // 🔹 Datenbankverbindung prüfen
  PrüfeDatenbankVerbindung;
end;


//TTimer liefert die aktuelle Uhrzeit
procedure TForm1.Timer1Timer(Sender: TObject);
begin
  lblClock.Caption := FormatDateTime('hh:nn', Now);
end;

//HauptmenuButton oder BackButton
procedure TForm1.btnAusleiheClick(Sender: TObject);
var
AusleiheFormular: TAusleiheFormular;
begin
   // Aktuellen Frame freigeben
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
  // 🔹 Prüfen, ob Anmeldedaten in der `dbconfig.ini` vorhanden sind
  if (DBKonfig.EdtServeradresse.Text = '') or (DBKonfig.EdtDb.Text = '') then
  begin
    ShowMessage('⚠️ Fehlende Anmeldedaten!\nBitte geben Sie Server und Datenbank in den Einstellungen an.');
    Exit;
  end;

  // 🔹 Falls Daten vorhanden sind, versuche Verbindung
  if DBKonfig.TesteDatenbankVerbindung then
    ShowMessage('✅ Verbindung zur Datenbank erfolgreich!')
  else
    ShowMessage('❌ Fehler beim Verbinden zur Datenbank.');
end;



procedure TForm1.btn_buecherClick(Sender: TObject);
var
Buchverwalter: TBuchverwalter;
begin
  // Aktuellen Frame freigeben
  if Assigned(FCurrentFrame) then
  begin
    FCurrentFrame.Free;
    FCurrentFrame := nil;
  end;
  // Dashboard (TMainDashboard) erstellen und einbetten
  Buchverwalter := TBuchverwalter.Create(Self);
  Buchverwalter.Parent := pnlContainer;
  Buchverwalter.Align := alClient;
  Buchverwalter.Visible := True;
  FCurrentFrame := Buchverwalter;
end;

procedure TForm1.btn_HauptmanuClick(Sender: TObject);
var
  Dashboard: TMainDashboard;
begin
  // Aktuellen Frame freigeben
  if Assigned(FCurrentFrame) then
  begin
    FCurrentFrame.Free;
    FCurrentFrame := nil;
  end;

  // Dashboard (TMainDashboard) erstellen und einbetten
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
  // Aktuellen Frame freigeben
  if Assigned(FCurrentFrame) then
  begin
    FCurrentFrame.Free;
    FCurrentFrame := nil;
  end;

  // TFrame1 (Kundenverwaltung) erstellen und einbetten
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
      // Dashboard (TMainDashboard) erstellen und einbetten
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
      // Dashboard (TMainDashboard) erstellen und einbetten
  Mahungsverwalt := TMahungsverwalt.Create(Self);
  Mahungsverwalt.Parent := pnlContainer;
  Mahungsverwalt.Align := alClient;
  Mahungsverwalt.Visible := True;
  FCurrentFrame := Mahungsverwalt;
end;

{ Öffentliche Methode, die den aktuellen Frame im Container durch den Kundenform-Frame (TFrame2) ersetzt }
// Diese Methode wurde ersetzt durch SwitchFrame
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
  if Assigned(FCurrentFrame) then
  begin
    FCurrentFrame.Free;
    FCurrentFrame := nil;
  end;

  if FrameType = 'Kundenform' then
    NewFrame := TFrame2.Create(Self)
  else if FrameType = 'Dashboard' then
    NewFrame := TMainDashboard.Create(Self)
  else if FrameType = 'Kundenverwaltung' then
    NewFrame := TFrame1.Create(Self)
  else if FrameType = 'Buchverwalter' then
    NewFrame := TBuchform.Create(Self)  // Hier neuen Fall hinzufügen
  else
    Exit;

  NewFrame.Parent := pnlContainer;
  NewFrame.Align := alClient;
  NewFrame.Visible := True;
  FCurrentFrame := NewFrame;
end;





end.

