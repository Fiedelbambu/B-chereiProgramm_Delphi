unit Kundenform;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.StdCtrls,
  Vcl.ComCtrls, Vcl.ExtCtrls, Data.Win.ADODB;

type
  // Mögliche Modi für das Kundenformular
  TKundenModus = (kmNeu, kmBearbeiten);

  // Record für Kundendaten
  TKundenDaten = record
    ID: Integer;          // => customer_id in DB
    Name: string;         // => last_name
    Vorname: string;      // => first_name
    Geburtsdatum: TDateTime;
    Email: string;
    Telefon: string;
    Strasse: string;
    Hausnummer: string;
    PLZ: string;
    Wohnort: string;
  end;

  TFrame2 = class(TFrame)
    // GUI-Komponenten
    lblName: TLabel;
    edtName: TEdit;
    lblVorname: TLabel;
    edtVorname: TEdit;
    lblPLZ: TLabel;
    edtPostleitzahl: TEdit;
    lblWohnort: TLabel;
    edtWohnort: TEdit;
    lblHausnummer: TLabel;
    edtHausnummer: TEdit;
    lblEmail: TLabel;
    edtEmail: TEdit;
    lblTelefonnummer: TLabel;
    edtTelefon: TEdit;
    lblKundenformular: TLabel;
    lblPDaten: TLabel;
    pBoxFormular: TPaintBox;
    PBoxPDaten: TPaintBox;
    lblGeburtsdatum: TLabel;
    DateTimePicker1: TDateTimePicker;
    PBoxzwischenDatenundAdresse: TPaintBox;
    lblAdresse: TLabel;
    lblStrasse: TLabel;
    EdtStrasse: TEdit;
    PBoxUntenKunden: TPaintBox;
    PBoxAdresse: TPaintBox;
    btn_speichern_KdnForm: TButton;
    btn_bearbeiten_KdnForm: TButton;
    btn_Loeschen_KdnForm: TButton;
    // NEU: Manuell vergebene Kundennummer, die in customer_id geschrieben wird
    EdtKundenNr: TEdit;
    lbl_KundenNr: TLabel;

    // Button-Events
    procedure btn_speichern_KdnFormClick(Sender: TObject);
    procedure btn_bearbeiten_KdnFormClick(Sender: TObject);
    procedure btn_Loeschen_KdnFormClick(Sender: TObject);
    procedure EdtKundenNrEnter(Sender: TObject);

  private
    { Private-Deklarationen }
    FModus: TKundenModus;
    FKundenDaten: TKundenDaten;   // Hier halten wir die aktuellen Kundendaten

    // Laufzeit-ADOQuery, um Insert/Update auszuführen
    ADOQuery: TADOQuery;

    procedure InitializeKundenformular(AModus: TKundenModus; const AData: TKundenDaten);
    procedure ClearFields;
    procedure LoadCustomerData(const AData: TKundenDaten);
    function ValidateKundenForm: Boolean;
    procedure SaveKundenForm;

  public
    { Public-Deklarationen }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GenerateKundenNummer: Integer;


    // Diese Methode rufst du von außen auf, um das Formular im gewünschten Modus zu starten
    procedure StartKundenForm(AModus: TKundenModus; const AData: TKundenDaten);
  end;

implementation

{$R *.dfm}

uses
  MainFrame, // Für Zugriff auf Form1.DBKonfig
  Dialogs,   // Für ShowMessage
  DatenbankKonfig;

{ ---------------------------------------------------------------------------- }
{ Konstruktor / Destruktor }
{ ---------------------------------------------------------------------------- }

constructor TFrame2.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Laufzeit-ADOQuery anlegen
  ADOQuery := TADOQuery.Create(Self);
  ADOQuery.Name := 'ADOQueryKundenform';
  // Connection wird später zur Laufzeit in SaveKundenForm zugewiesen
end;

destructor TFrame2.Destroy;
begin
  // ADOQuery wird vom Owner (Self) automatisch freigegeben
  inherited Destroy;
end;

procedure TFrame2.EdtKundenNrEnter(Sender: TObject);
begin
       // GenerateKundenNummer
  EdtKundenNr.Text := IntToStr(GenerateKundenNummer);
end;

{ ---------------------------------------------------------------------------- }
{ Haupt-Einstieg: StartKundenForm }
{ ---------------------------------------------------------------------------- }

procedure TFrame2.StartKundenForm(AModus: TKundenModus; const AData: TKundenDaten);
begin
  InitializeKundenformular(AModus, AData);
  // Hier kannst du das Frame ggf. anzeigen, z. B. Parent zuweisen, Visible=True, etc.
end;

{ ---------------------------------------------------------------------------- }
{ InitializeKundenformular: Neu / Bearbeiten }
{ ---------------------------------------------------------------------------- }

procedure TFrame2.InitializeKundenformular(AModus: TKundenModus; const AData: TKundenDaten);
begin
  FModus := AModus;

  if FModus = kmNeu then
  begin
    ClearFields;
    lblKundenformular.Caption := 'Neuen Kunden anlegen';
    btn_bearbeiten_KdnForm.Enabled := False;
    btn_Loeschen_KdnForm.Enabled := False;
  end
  else // kmBearbeiten
  begin
    FKundenDaten := AData;
    LoadCustomerData(FKundenDaten);
    lblKundenformular.Caption := 'Kunden bearbeiten';
    btn_bearbeiten_KdnForm.Enabled := True;
    btn_Loeschen_KdnForm.Enabled := True;
  end;
end;

{ ---------------------------------------------------------------------------- }
{ Felder leeren / laden }
{ ---------------------------------------------------------------------------- }

procedure TFrame2.ClearFields;
begin
  // Alle Edit-Felder zurücksetzen
  EdtKundenNr.Text := '0';   // Kundennummer (customer_id)
  edtName.Text := '';
  edtVorname.Text := '';
  DateTimePicker1.Date := Now;
  edtEmail.Text := '';
  edtTelefon.Text := '';
  EdtStrasse.Text := '';
  edtHausnummer.Text := '';
  edtPostleitzahl.Text := '';
  edtWohnort.Text := '';
end;

procedure TFrame2.LoadCustomerData(const AData: TKundenDaten);
begin
  // Daten in die Edit-Felder laden
  EdtKundenNr.Text := IntToStr(AData.ID);
  edtName.Text := AData.Name;
  edtVorname.Text := AData.Vorname;
  DateTimePicker1.Date := AData.Geburtsdatum;
  edtEmail.Text := AData.Email;
  edtTelefon.Text := AData.Telefon;
  EdtStrasse.Text := AData.Strasse;
  edtHausnummer.Text := AData.Hausnummer;
  edtPostleitzahl.Text := AData.PLZ;
  edtWohnort.Text := AData.Wohnort;
end;

{ ---------------------------------------------------------------------------- }
{ Validierung }
{ ---------------------------------------------------------------------------- }

function TFrame2.ValidateKundenForm: Boolean;
begin
  // Einfaches Beispiel: Prüfe Name und Vorname
  if Trim(edtName.Text) = '' then
  begin
    ShowMessage('Bitte einen Nachnamen eingeben!');
    Exit(False);
  end;
  if Trim(edtVorname.Text) = '' then
  begin
    ShowMessage('Bitte einen Vornamen eingeben!');
    Exit(False);
  end;
  // Prüfe Email rudimentär
  if Pos('@', edtEmail.Text) = 0 then
  begin
    ShowMessage('Bitte eine gültige E-Mail-Adresse eingeben!');
    Exit(False);
  end;
  // Weitere Prüfungen (Telefon, PLZ usw.) möglich
  Result := True;
end;

{ ---------------------------------------------------------------------------- }
{ Speichern: Insert / Update }
{ ---------------------------------------------------------------------------- }

procedure TFrame2.SaveKundenForm;
var
  DebugMsg: string;
begin
  // 1) Validierung
  if not ValidateKundenForm then
    Exit;

  // 2) Felder in FKundenDaten übernehmen
  // Hier interpretieren wir EdtKundenNr als customer_id
  FKundenDaten.ID := StrToIntDef(EdtKundenNr.Text, 0);

  FKundenDaten.Name         := edtName.Text;
  FKundenDaten.Vorname      := edtVorname.Text;
  FKundenDaten.Geburtsdatum := DateTimePicker1.Date;
  FKundenDaten.Email        := edtEmail.Text;
  FKundenDaten.Telefon      := edtTelefon.Text;
  FKundenDaten.Strasse      := EdtStrasse.Text;
  FKundenDaten.Hausnummer   := edtHausnummer.Text;
  FKundenDaten.PLZ          := edtPostleitzahl.Text;
  FKundenDaten.Wohnort      := edtWohnort.Text;

  // 3) Verbindung prüfen
  if Assigned(Form1) and Assigned(Form1.DBKonfig) then
    ADOQuery.Connection := Form1.DBKonfig.ADOConnection
  else
  begin
    ShowMessage('Keine gültige Datenbankverbindung!');
    Exit;
  end;

  // 4) SQL-Text (INSERT ... ON DUPLICATE KEY UPDATE)
  ADOQuery.SQL.Text :=
    'INSERT INTO customers ' +
    '(customer_id, first_name, last_name, email, phone, birth_date, street, house_number, postal_code, city) ' +
    'VALUES ' +
    '(:CustomerID, :FirstName, :LastName, :Email, :Phone, :BirthDate, :Street, :HouseNumber, :PostalCode, :City) ' +
    'ON DUPLICATE KEY UPDATE ' +
    'customer_id   = :CustomerID, ' +
    'first_name    = :FirstName, ' +
    'last_name     = :LastName, ' +
    'email         = :Email, ' +
    'phone         = :Phone, ' +
    'birth_date    = :BirthDate, ' +
    'street        = :Street, ' +
    'house_number  = :HouseNumber, ' +
    'postal_code   = :PostalCode, ' +
    'city          = :City';

  // 5) Parameter
  ADOQuery.Parameters.ParamByName('CustomerID').Value := FKundenDaten.ID;
  ADOQuery.Parameters.ParamByName('FirstName').Value  := FKundenDaten.Vorname;
  ADOQuery.Parameters.ParamByName('LastName').Value   := FKundenDaten.Name;
  ADOQuery.Parameters.ParamByName('BirthDate').Value  := FKundenDaten.Geburtsdatum;
  ADOQuery.Parameters.ParamByName('Email').Value      := FKundenDaten.Email;
  ADOQuery.Parameters.ParamByName('Phone').Value      := FKundenDaten.Telefon;
  ADOQuery.Parameters.ParamByName('Street').Value     := FKundenDaten.Strasse;
  ADOQuery.Parameters.ParamByName('HouseNumber').Value:= FKundenDaten.Hausnummer;
  ADOQuery.Parameters.ParamByName('PostalCode').Value := FKundenDaten.PLZ;
  ADOQuery.Parameters.ParamByName('City').Value       := FKundenDaten.Wohnort;

  // 6) Debug-Nachricht vor dem ExecSQL
  DebugMsg := 'ExecSQL wird ausgeführt. Hier das Statement:'#13#10
    + ADOQuery.SQL.Text + #13#10#13#10
    + 'Parameterwerte:'#13#10
    + 'CustomerID = '  + VarToStr(ADOQuery.Parameters.ParamByName('CustomerID').Value)  + #13#10
    + 'FirstName = '   + VarToStr(ADOQuery.Parameters.ParamByName('FirstName').Value)   + #13#10
    + 'LastName = '    + VarToStr(ADOQuery.Parameters.ParamByName('LastName').Value)    + #13#10
    + 'Email = '       + VarToStr(ADOQuery.Parameters.ParamByName('Email').Value)       + #13#10
    + 'Phone = '       + VarToStr(ADOQuery.Parameters.ParamByName('Phone').Value)       + #13#10
    + 'BirthDate = '   + VarToStr(ADOQuery.Parameters.ParamByName('BirthDate').Value)   + #13#10
    + 'Street = '      + VarToStr(ADOQuery.Parameters.ParamByName('Street').Value)      + #13#10
    + 'HouseNumber = ' + VarToStr(ADOQuery.Parameters.ParamByName('HouseNumber').Value) + #13#10
    + 'PostalCode = '  + VarToStr(ADOQuery.Parameters.ParamByName('PostalCode').Value)  + #13#10
    + 'City = '        + VarToStr(ADOQuery.Parameters.ParamByName('City').Value);

  ShowMessage(DebugMsg);

  // 7) ExecSQL
  try
    ADOQuery.ExecSQL;
    ShowMessage('Kundendaten wurden erfolgreich gespeichert!');
  except
    on E: Exception do
      ShowMessage('Fehler beim Speichern: ' + E.Message);
  end;
end;


function TFrame2.GenerateKundenNummer: Integer;
var
  TestNr: Integer;
  Found: Boolean;
begin
  // 1) Prüfen, ob DB-Verbindung vorhanden ist
  if not (Assigned(Form1) and Assigned(Form1.DBKonfig)) then
    raise Exception.Create('Keine DB-Verbindung vorhanden!');

  // 2) ADOQuery so konfigurieren, dass wir suchen können
  ADOQuery.Connection := Form1.DBKonfig.ADOConnection;

  repeat
    // 3) Zufallszahl erzeugen (z. B. 1..99999)
    TestNr := Random(99999) + 1;

    // 4) Prüfen, ob es diese Nummer schon gibt
    ADOQuery.Close;
    ADOQuery.SQL.Text := 'SELECT COUNT(*) AS C FROM customers WHERE customer_id = :CID';
    ADOQuery.Parameters.ParamByName('CID').Value := TestNr;
    ADOQuery.Open;

    Found := (ADOQuery.FieldByName('C').AsInteger > 0);
    ADOQuery.Close;

    // Solange Found=True, wird die Schleife wiederholt und eine neue Nummer probiert
  until not Found;

  Result := TestNr;
end;



{ ---------------------------------------------------------------------------- }
{ Button-Events }
{ ---------------------------------------------------------------------------- }

procedure TFrame2.btn_speichern_KdnFormClick(Sender: TObject);
begin
  SaveKundenForm;
end;

procedure TFrame2.btn_bearbeiten_KdnFormClick(Sender: TObject);
begin
  ShowMessage('Bearbeiten-Logik hier einbauen, falls nötig.');
end;

procedure TFrame2.btn_Loeschen_KdnFormClick(Sender: TObject);
begin
  ShowMessage('Löschen-Logik hier implementieren, z. B. Delete-Query.');
end;

end.

