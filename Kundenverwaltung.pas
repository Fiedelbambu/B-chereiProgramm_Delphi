unit Kundenverwaltung;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, Vcl.Grids,
  Vcl.DBGrids, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.WinXCtrls;

type
  TFrame1 = class(TFrame)
    lblKundenverwaltung: TLabel;
    PaintBox1: TPaintBox;
    lblSuchfelf: TLabel;
    btn_Suchen: TButton;
    PaintBox2: TPaintBox;
    lblKundendaten: TLabel;
    PaintBox3: TPaintBox;
    DBGrid1: TDBGrid;
    PaintBox4: TPaintBox;
    btn_KundenAnlegen: TButton;
    SBoxKundenverwaltung: TSearchBox;
    procedure btn_KundenAnlegenClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  KundenverwaltungFrame: TFrame1;

implementation

{$R *.dfm}

uses
  MainFrame;  // Zugriff auf Form1 und dessen öffentliche Methode ShowKundenform

procedure TFrame1.btn_KundenAnlegenClick(Sender: TObject);
begin
  if Assigned(Form1) then
    Form1.ShowKundenform;
end;

end.



{// Pseudocode für Modul 2: Kundenverwaltung
// Autor: Christian Fiedler

MODULE Kundenverwaltung

    // --- Deklaration der UI-Komponenten ---
    // Kundenformular (für Anlegen/Bearbeiten)
    VARIABLE kundenForm  // Form mit Eingabefeldern: Name, Vorname, E-Mail, Telefonnummer, Geburtsdatum, Adresse, etc.
    VARIABLE inputFelder = {
        "Name": "",
        "Vorname": "",
        "Email": "",
        "Telefonnummer": "",
        "Geburtsdatum": "",
        "Straße": "",
        "PLZ": "",
        "Ort": ""
    }

    // Suchbereich und Datenanzeige
    VARIABLE kundenSuchfeld  // Textfeld für Suchanfragen (z.B. nach Name oder Kundennummer)
    VARIABLE kundenDataGrid  // DataGrid oder ListView zur Anzeige der Kundendaten

    // --- Initialisierung der Kundenverwaltung ---
    FUNCTION InitializeKundenUI()
        // Erstelle und konfiguriere das Kundenformular
        CALL CreateForm("Kundenformular", inputFelder)

        // Erstelle das Suchfeld und das DataGrid für die Kundensuche
        CALL CreateSearchField("KundenSuchfeld")
        CALL CreateDataGrid("KundenDataGrid")

        // Registriere Event-Handler für Formular-Buttons
        SET OnClick("SpeichernButton") TO SaveKunde
        SET OnClick("BearbeitenButton") TO OpenKundenformular("Bearbeiten")
        SET OnClick("LöschenButton") TO DeleteKunde

        // Registriere Event-Handler für den Suchbutton
        SET OnClick("SuchenButton") TO SearchKunden

        // Button „Neuen Kunden anlegen“ in der Übersicht
        SET OnClick("NeuenKundenButton") TO OpenKundenformular("Neu")
    END FUNCTION

    // --- Öffnen des Kundenformulars ---
    // modus kann "Neu" oder "Bearbeiten" sein
    FUNCTION OpenKundenformular(modus)
        IF modus == "Neu" THEN
            // Leere alle Eingabefelder
            CALL ClearInputFields(inputFelder)
        ELSE IF modus == "Bearbeiten" THEN
            VARIABLE ausgewählterKunde = GET SelectedRow(kundenDataGrid)
            IF ausgewählterKunde IS NOT NULL THEN
                // Lade die Daten des ausgewählten Kunden in das Formular
                CALL LoadKundenDaten(ausgewählterKunde, inputFelder)
            ELSE
                CALL ShowError("Bitte wählen Sie einen Kunden aus, um ihn zu bearbeiten.")
                RETURN
            END IF
        END IF
        // Zeige das Kundenformular an
        CALL ShowForm("Kundenformular")
    END FUNCTION

    // --- Speichern von Kundendaten (Neuanlage oder Bearbeitung) ---
    FUNCTION SaveKunde()
        // Validierung der Eingaben (z. B. Pflichtfelder, E-Mail-Format)
        IF NOT ValidateKundenInput(inputFelder) THEN
            CALL ShowError("Ungültige Eingaben. Bitte überprüfen Sie alle Pflichtfelder.")
            RETURN
        END IF

        // Bestimmen, ob es sich um einen neuen Kunden oder eine Bearbeitung handelt
        IF IsNewCustomer(inputFelder) THEN
            CALL Database.AddCustomer(GetInputData(inputFelder))
        ELSE
            CALL Database.UpdateCustomer(GetInputData(inputFelder))
        END IF

        CALL ShowMessage("Kundendaten wurden erfolgreich gespeichert.")
        CALL RefreshKundenDataGrid()
        CALL CloseForm("Kundenformular")
    END FUNCTION

    // --- Löschen eines Kunden ---
    FUNCTION DeleteKunde()
        VARIABLE ausgewählterKunde = GET SelectedRow(kundenDataGrid)
        IF ausgewählterKunde IS NULL THEN
            CALL ShowError("Bitte wählen Sie einen Kunden zum Löschen aus.")
            RETURN
        END IF
        // Bestätigungsabfrage (optional)
        IF Confirm("Soll der Kunde wirklich gelöscht werden?") THEN
            CALL Database.DeleteCustomer(ausgewählterKunde.id)
            CALL ShowMessage("Kunde wurde gelöscht.")
            CALL RefreshKundenDataGrid()
        END IF
    END FUNCTION

    // --- Suchen von Kunden ---
    FUNCTION SearchKunden()
        VARIABLE suchQuery = GET TextFrom("KundenSuchfeld")
        VARIABLE ergebnisse = CALL Database.SearchCustomer(suchQuery)
        CALL PopulateDataGrid(kundenDataGrid, ergebnisse)
    END FUNCTION

    // --- Aktualisieren der Kundenübersicht ---
    FUNCTION RefreshKundenDataGrid()
        VARIABLE alleKunden = CALL Database.GetAllCustomers()
        CALL PopulateDataGrid(kundenDataGrid, alleKunden)
    END FUNCTION

    // --- Validierung der Eingabefelder im Kundenformular ---
    FUNCTION ValidateKundenInput(felder)
        // Beispiel: Überprüfe, ob Name, Vorname und E-Mail gefüllt sind
        IF felder["Name"] IS EMPTY OR felder["Vorname"] IS EMPTY THEN
            RETURN FALSE
        END IF
        // Überprüfe das E-Mail-Format (vereinfachte Prüfung)
        IF NOT Contains(felder["Email"], "@") THEN
            RETURN FALSE
        END IF
        // Weitere Validierungen können hier hinzugefügt werden
        RETURN TRUE
    END FUNCTION

    // --- Hauptfunktion des Kundenverwaltungsmoduls ---
    FUNCTION MainKundenverwaltung()
        CALL InitializeKundenUI()
        CALL RefreshKundenDataGrid()
        // Starte die Event-Schleife für die Kundenverwaltung
        CALL StartEventLoop()
    END FUNCTION

END MODULE
}

