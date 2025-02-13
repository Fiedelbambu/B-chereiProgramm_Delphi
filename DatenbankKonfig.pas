unit DatenbankKonfig;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, IniFiles, Data.DB, Data.Win.ADODB;

type
  TKonfig_Datenbank = class(TFrame)
    lblDataKonfig: TLabel;
    lblServeradresse: TLabel;
    lblPort: TLabel;
    lblDatenbank: TLabel;
    lblPasswort: TLabel;
    lblBenutzername: TLabel;
    EdtServeradresse: TEdit;
    EdtPort: TEdit;
    EdtDb: TEdit;
    EdtNutzername: TEdit;
    btnTestVerbindung: TButton;
    CheckBoxWinAuth: TCheckBox;
    lblInfo: TLabel;
    btnSpeichern: TButton;
    ADOConnection: TADOConnection;
    EdtPasswort: TEdit;

    procedure btnTestVerbindungClick(Sender: TObject);
    procedure btnSpeichernClick(Sender: TObject);
  private
    procedure LadeDatenbankKonfiguration;
    procedure SpeichereDatenbankKonfiguration;
    function ErstelleConnectionString: string;

  public
    procedure InitializeConfig;
    function TesteDatenbankVerbindung: Boolean;
  end;

implementation

{$R *.dfm}

const
  CONFIG_DATEI = 'dbconfig.ini';

// Laden der gespeicherten Konfiguration aus der INI-Datei
procedure TKonfig_Datenbank.LadeDatenbankKonfiguration;
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(ExtractFilePath(ParamStr(0)) + CONFIG_DATEI);
  try
    EdtServeradresse.Text := IniFile.ReadString('Database', 'Server', '');
    EdtPort.Text := IniFile.ReadString('Database', 'Port', '3306');
    EdtDb.Text := IniFile.ReadString('Database', 'DatabaseName', '');
    EdtNutzername.Text := IniFile.ReadString('Database', 'Username', '');
    EdtPasswort.Text := IniFile.ReadString('Database', 'Password', '');
    CheckBoxWinAuth.Checked := IniFile.ReadBool('Database', 'WindowsAuth', False);
  finally
    IniFile.Free;
  end;
end;

// Speichern der Konfiguration in die INI-Datei
procedure TKonfig_Datenbank.SpeichereDatenbankKonfiguration;
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(ExtractFilePath(ParamStr(0)) + CONFIG_DATEI);
  try
    IniFile.WriteString('Database', 'Server', EdtServeradresse.Text);
    IniFile.WriteString('Database', 'Port', EdtPort.Text);
    IniFile.WriteString('Database', 'DatabaseName', EdtDb.Text);
    IniFile.WriteString('Database', 'Username', EdtNutzername.Text);
    IniFile.WriteString('Database', 'Password', EdtPasswort.Text);
    IniFile.WriteBool('Database', 'WindowsAuth', CheckBoxWinAuth.Checked);
    ShowMessage('Datenbankkonfiguration gespeichert.');
  finally
    IniFile.Free;
  end;
end;
 // ICh habe aus dem Vorherigen Programm den "DSN-loser ConnectionString unter Verwendung des MySQL ODBC-Treibers" angepasst siehe Github
// Erstellen des Connection-Strings basierend auf den Eingaben
function TKonfig_Datenbank.ErstelleConnectionString: string;
begin
  if CheckBoxWinAuth.Checked then
    Result := Format('Provider=MSDASQL;Driver={MySQL ODBC 9.2 Unicode Driver};Server=%s;Database=%s;Integrated Security=SSPI;',
      [EdtServeradresse.Text, EdtDb.Text])
  else
    Result := Format('Provider=MSDASQL;Driver={MySQL ODBC 9.2 Unicode Driver};Server=%s;Port=%s;Database=%s;UID=%s;PWD=%s;',
      [EdtServeradresse.Text, EdtPort.Text, EdtDb.Text, EdtNutzername.Text, EdtPasswort.Text]);
end;

// Testen der Datenbankverbindung
function TKonfig_Datenbank.TesteDatenbankVerbindung: Boolean;
var
  ConnectionString: string;
begin
  // Falls bereits verbunden, Verbindung trennen
  if ADOConnection.Connected then
    ADOConnection.Connected := False;

  // Connection-String basierend auf gespeicherten Werten erstellen
  ConnectionString := ErstelleConnectionString;

  // Überprüfen, ob notwendige Werte gesetzt sind
  if (EdtServeradresse.Text = '') or (EdtDb.Text = '') then
  begin
    ShowMessage('Fehlende Verbindungsdaten! Bitte stellen Sie sicher, dass Serveradresse und Datenbankname angegeben sind.');
    Exit(False);
  end;

  // Verbindungseinstellungen setzen
  ADOConnection.ConnectionString := ConnectionString;
  ADOConnection.LoginPrompt := False;

  try
    ADOConnection.Connected := True;
    ShowMessage('Verbindung erfolgreich!');
    Result := True;
  except
    on E: Exception do
    begin
      ShowMessage('Fehler bei der Verbindung: ' + E.Message);
      Result := False;
    end;
  end;
end;

// Initialisieren der Konfiguration beim Programmstart
procedure TKonfig_Datenbank.InitializeConfig;
begin
  LadeDatenbankKonfiguration;
end;

// Event-Handler: Speichern-Button
procedure TKonfig_Datenbank.btnSpeichernClick(Sender: TObject);
begin
  SpeichereDatenbankKonfiguration;
end;

// Event-Handler: Testverbindungs-Button
procedure TKonfig_Datenbank.btnTestVerbindungClick(Sender: TObject);
begin
  TesteDatenbankVerbindung;
end;

end.

