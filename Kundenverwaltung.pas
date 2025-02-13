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
  MainFrame;  // Zugriff auf Form1 und dessen �ffentliche Methode ShowKundenform

procedure TFrame1.btn_KundenAnlegenClick(Sender: TObject);
begin
  if Assigned(Form1) then
    Form1.ShowKundenform;
end;

end.



{// Pseudocode f�r Modul 2: Kundenverwaltung
// Autor: Christian Fiedler

MODULE Kundenverwaltung

    // --- Deklaration der UI-Komponenten ---
    // Kundenformular (f�r Anlegen/Bearbeiten)
    VARIABLE kundenForm  // Form mit Eingabefeldern: Name, Vorname, E-Mail, Telefonnummer, Geburtsdatum, Adresse, etc.
    VARIABLE inputFelder = {
        "Name": "",
        "Vorname": "",
        "Email": "",
        "Telefonnummer": "",
        "Geburtsdatum": "",
        "Stra�e": "",
        "PLZ": "",
        "Ort": ""
    }

    // Suchbereich und Datenanzeige
    VARIABLE kundenSuchfeld  // Textfeld f�r Suchanfragen (z.B. nach Name oder Kundennummer)
    VARIABLE kundenDataGrid  // DataGrid oder ListView zur Anzeige der Kundendaten

    // --- Initialisierung der Kundenverwaltung ---
    FUNCTION InitializeKundenUI()
        // Erstelle und konfiguriere das Kundenformular
        CALL CreateForm("Kundenformular", inputFelder)

        // Erstelle das Suchfeld und das DataGrid f�r die Kundensuche
        CALL CreateSearchField("KundenSuchfeld")
        CALL CreateDataGrid("KundenDataGrid")

        // Registriere Event-Handler f�r Formular-Buttons
        SET OnClick("SpeichernButton") TO SaveKunde
        SET OnClick("BearbeitenButton") TO OpenKundenformular("Bearbeiten")
        SET OnClick("L�schenButton") TO DeleteKunde

        // Registriere Event-Handler f�r den Suchbutton
        SET OnClick("SuchenButton") TO SearchKunden

        // Button �Neuen Kunden anlegen� in der �bersicht
        SET OnClick("NeuenKundenButton") TO OpenKundenformular("Neu")
    END FUNCTION

    // --- �ffnen des Kundenformulars ---
    // modus kann "Neu" oder "Bearbeiten" sein
    FUNCTION OpenKundenformular(modus)
        IF modus == "Neu" THEN
            // Leere alle Eingabefelder
            CALL ClearInputFields(inputFelder)
        ELSE IF modus == "Bearbeiten" THEN
            VARIABLE ausgew�hlterKunde = GET SelectedRow(kundenDataGrid)
            IF ausgew�hlterKunde IS NOT NULL THEN
                // Lade die Daten des ausgew�hlten Kunden in das Formular
                CALL LoadKundenDaten(ausgew�hlterKunde, inputFelder)
            ELSE
                CALL ShowError("Bitte w�hlen Sie einen Kunden aus, um ihn zu bearbeiten.")
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
            CALL ShowError("Ung�ltige Eingaben. Bitte �berpr�fen Sie alle Pflichtfelder.")
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

    // --- L�schen eines Kunden ---
    FUNCTION DeleteKunde()
        VARIABLE ausgew�hlterKunde = GET SelectedRow(kundenDataGrid)
        IF ausgew�hlterKunde IS NULL THEN
            CALL ShowError("Bitte w�hlen Sie einen Kunden zum L�schen aus.")
            RETURN
        END IF
        // Best�tigungsabfrage (optional)
        IF Confirm("Soll der Kunde wirklich gel�scht werden?") THEN
            CALL Database.DeleteCustomer(ausgew�hlterKunde.id)
            CALL ShowMessage("Kunde wurde gel�scht.")
            CALL RefreshKundenDataGrid()
        END IF
    END FUNCTION

    // --- Suchen von Kunden ---
    FUNCTION SearchKunden()
        VARIABLE suchQuery = GET TextFrom("KundenSuchfeld")
        VARIABLE ergebnisse = CALL Database.SearchCustomer(suchQuery)
        CALL PopulateDataGrid(kundenDataGrid, ergebnisse)
    END FUNCTION

    // --- Aktualisieren der Kunden�bersicht ---
    FUNCTION RefreshKundenDataGrid()
        VARIABLE alleKunden = CALL Database.GetAllCustomers()
        CALL PopulateDataGrid(kundenDataGrid, alleKunden)
    END FUNCTION

    // --- Validierung der Eingabefelder im Kundenformular ---
    FUNCTION ValidateKundenInput(felder)
        // Beispiel: �berpr�fe, ob Name, Vorname und E-Mail gef�llt sind
        IF felder["Name"] IS EMPTY OR felder["Vorname"] IS EMPTY THEN
            RETURN FALSE
        END IF
        // �berpr�fe das E-Mail-Format (vereinfachte Pr�fung)
        IF NOT Contains(felder["Email"], "@") THEN
            RETURN FALSE
        END IF
        // Weitere Validierungen k�nnen hier hinzugef�gt werden
        RETURN TRUE
    END FUNCTION

    // --- Hauptfunktion des Kundenverwaltungsmoduls ---
    FUNCTION MainKundenverwaltung()
        CALL InitializeKundenUI()
        CALL RefreshKundenDataGrid()
        // Starte die Event-Schleife f�r die Kundenverwaltung
        CALL StartEventLoop()
    END FUNCTION

END MODULE
}

