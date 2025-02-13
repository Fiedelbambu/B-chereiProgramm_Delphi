﻿unit Ausleihform;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, Vcl.Grids,
  Vcl.DBGrids, Vcl.StdCtrls, Vcl.WinXPickers, Vcl.WinXCtrls;

type
  TAusleiheFormular = class(TFrame)
    lblAusleihformular: TLabel;
    lblKunde: TLabel;
    SearchBox1: TSearchBox;
    lblAusleihtag: TLabel;
    DatePickerAusleihtag: TDatePicker;
    lblRueckgabewert: TLabel;
    DatePicker2: TDatePicker;
    btnAusleihen: TButton;
    btnVerlaengern: TButton;
    btn_Rueckgabe: TButton;
    lblAusleihen: TLabel;
    DBGrid1: TDBGrid;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

implementation

{$R *.dfm}

end.


{// Pseudocode für das Ausleihformular
// Autor: Christian Fiedler

MODULE Ausleihformular

    // --- Deklaration der UI-Komponenten ---
    // Auswahl des Kunden (Dropdown oder Suchfeld)
    VARIABLE kundeAuswahl = ""         // Kunde oder Kundennummer

    // Datumsauswahlfelder
    VARIABLE ausleihtag = ""           // Datumsauswahlfeld für den Ausleihtag
    VARIABLE sollRueckgabedatum = ""   // Datumsauswahlfeld für das Soll-Rückgabedatum (automatisch berechnet)

    // Buttons für die Aktionen
    VARIABLE buttonAusleihen    // Button für "Ausleihen"
    VARIABLE buttonVerlaengern  // Button für "Verlängern"
    VARIABLE buttonRueckgabe    // Button für "Rückgabe"

    // --- Initialisierung des Ausleihformulars ---
    FUNCTION InitializeAusleihformular(modus)
        // modus kann "Neu", "Verlängern" oder "Rückgabe" sein
        IF modus == "Neu" THEN
            CALL ClearField(kundeAuswahl)
            CALL ClearField(ausleihtag)
            // Automatische Berechnung des Soll-Rückgabedatums, z.B. 14 Tage nach Ausleihtag
            sollRueckgabedatum = CalculateSollRueckgabedatum(GetCurrentDate())
        ELSE IF modus == "Verlängern" THEN
            // Lade den bestehenden Ausleihvorgang, um ihn zu verlängern
            VARIABLE bestehenderVorgang = GET SelectedLoan()
            IF bestehenderVorgang IS NULL THEN
                CALL ShowError("Bitte wählen Sie einen Ausleihvorgang aus, den Sie verlängern möchten.")
                RETURN
            END IF
            CALL LoadLoanData(bestehenderVorgang)
            // Berechne das neue Soll-Rückgabedatum (z. B. Verlängerung um weitere 7 Tage)
            sollRueckgabedatum = CalculateNewRueckgabedatum(bestehenderVorgang.ausleihtag, 7)
        ELSE IF modus == "Rückgabe" THEN
            // Lade den bestehenden Ausleihvorgang für die Rückgabe
            VARIABLE bestehenderVorgang = GET SelectedLoan()
            IF bestehenderVorgang IS NULL THEN
                CALL ShowError("Bitte wählen Sie einen Ausleihvorgang für die Rückgabe aus.")
                RETURN
            END IF
            CALL LoadLoanData(bestehenderVorgang)
        END IF

        // Zeige das Ausleihformular an
        CALL ShowForm("Ausleihformular")
    END FUNCTION

    // --- Validierung der Eingaben ---
    FUNCTION ValidateAusleihForm() RETURNS BOOLEAN
        // Überprüfe, ob ein Kunde ausgewählt wurde
        IF kundeAuswahl IS EMPTY THEN
            CALL ShowError("Bitte wählen Sie einen Kunden aus.")
            RETURN FALSE
        END IF

        // Überprüfe, ob ein Ausleihtag gesetzt ist
        IF ausleihtag IS EMPTY THEN
            CALL ShowError("Bitte wählen Sie einen Ausleihtag aus.")
            RETURN FALSE
        END IF

        // Optional: Weitere Prüfungen, z.B. ob das Soll-Rückgabedatum in der Zukunft liegt
        IF NOT IsFutureDate(sollRueckgabedatum) THEN
            CALL ShowError("Das Soll-Rückgabedatum muss in der Zukunft liegen.")
            RETURN FALSE
        END IF

        RETURN TRUE
    END FUNCTION

    // --- Speichern bzw. Bearbeiten des Ausleihvorgangs ---
    FUNCTION SaveAusleihVorgang(aktion)
        // Aktion kann "Ausleihen", "Verlängern" oder "Rückgabe" sein

        IF NOT ValidateAusleihForm() THEN
            RETURN
        END IF

        VARIABLE loanData = {
            "Kunde": kundeAuswahl,
            "Ausleihtag": ausleihtag,
            "SollRueckgabedatum": sollRueckgabedatum
        }

        IF aktion == "Ausleihen" THEN
            // Neuen Ausleihvorgang erfassen
            CALL Database.AddLoan(loanData)
            CALL ShowMessage("Ausleihe erfolgreich erfasst.")
        ELSE IF aktion == "Verlängern" THEN
            // Verlängerung prüfen, z.B. ob keine Reservierung vorliegt
            IF CALL CanExtendLoan(loanData) THEN
                CALL Database.UpdateLoan(loanData)
                CALL ShowMessage("Ausleihe erfolgreich verlängert.")
            ELSE
                CALL ShowError("Verlängerung nicht möglich: Es liegt eine Reservierung vor.")
                RETURN
            END IF
        ELSE IF aktion == "Rückgabe" THEN
            // Rückgabe verarbeiten
            CALL Database.CompleteLoan(loanData)
            CALL ShowMessage("Rückgabe erfolgreich registriert.")
        END IF

        // Nach der Aktion: Übersicht aktualisieren und Formular schließen
        CALL RefreshLoanOverview()
        CALL CloseForm("Ausleihformular")
    END FUNCTION

    // --- Event-Handler für Buttons ---
    FUNCTION OnAusleihenButtonClick()
        CALL SaveAusleihVorgang("Ausleihen")
    END FUNCTION

    FUNCTION OnVerlaengernButtonClick()
        CALL SaveAusleihVorgang("Verlängern")
    END FUNCTION

    FUNCTION OnRueckgabeButtonClick()
        CALL SaveAusleihVorgang("Rückgabe")
    END FUNCTION

    // --- Hilfsfunktionen ---
    FUNCTION CalculateSollRueckgabedatum(startDatum) RETURNS DATE
        // Beispiel: 14 Tage nach dem Ausleihtag
        RETURN startDatum + 14 Tage
    END FUNCTION

    FUNCTION CalculateNewRueckgabedatum(ausleihtag, verlängerungstage) RETURNS DATE
        RETURN ausleihtag + verlängerungstage Tage
    END FUNCTION

    FUNCTION IsFutureDate(datum) RETURNS BOOLEAN
        RETURN datum > GetCurrentDate()
    END FUNCTION

    // --- Haupt-Einstiegspunkt für das Ausleihformular ---
    FUNCTION MainAusleihformular(modus)
        CALL InitializeAusleihformular(modus)
        // Registriere Event-Handler für die Aktions-Buttons
        SET OnClick("AusleihenButton") TO OnAusleihenButtonClick
        SET OnClick("VerlaengernButton") TO OnVerlaengernButtonClick
        SET OnClick("RueckgabeButton") TO OnRueckgabeButtonClick
        // Starte die Event-Schleife für das Ausleihformular
        CALL StartEventLoop()
    END FUNCTION

END MODULE
}

     // Pseudocode für Modul "Aktuelle Ausleihen"
// Autor: Christian Fiedler

MODULE AktuelleAusleihen

    // Deklaration der UI-Komponente: DataGrid zur Anzeige der Ausleihen
    VARIABLE ausleihenDataGrid

    // --- Initialisierung der Ausleihe-Übersicht ---
    FUNCTION InitializeAusleiheÜbersicht()
        // Erstelle das DataGrid und konfiguriere die Spalten (z. B. Kundennr, Name, Ausleihtag, Soll-Rückgabedatum, Status)
        ausleihenDataGrid = CreateDataGrid("AusleihenDataGrid")
        CALL SetupDataGridColumns(ausleihenDataGrid, ["Kundennr", "Name", "Ausleihtag", "Soll-Rückgabedatum", "Status"])

        // Registriere den Event-Handler für Doppelklick auf eine Zeile
        SET OnDoubleClick(ausleihenDataGrid) TO FUNCTION() {
            VARIABLE selectedLoan = GET SelectedRow(ausleihenDataGrid)
            IF selectedLoan IS NOT NULL THEN
                CALL OpenLoanDetails(selectedLoan)
            ELSE
                CALL ShowError("Bitte wählen Sie einen Ausleihvorgang aus.")
            END IF
        }
    END FUNCTION

    // --- Aktualisieren der Ausleihenliste ---
    FUNCTION RefreshAusleihenListe()
        // Lade alle aktiven Ausleihvorgänge aus der Datenbank
        VARIABLE alleAusleihen = Database.GetActiveLoans()

        // Aktualisiere den Status: Falls das Soll-Rückgabedatum bereits vergangen ist, setze den Status auf "Überfällig"
        FOR EACH loan IN alleAusleihen DO
            IF loan.SollRueckgabedatum < GetCurrentDate() THEN
                loan.Status = "Überfällig"
            ELSE
                loan.Status = "OK"
            END IF
        END FOR

        // Fülle das DataGrid mit den aktualisierten Ausleihvorgängen
        CALL PopulateDataGrid(ausleihenDataGrid, alleAusleihen)

        // Wende farbliche Kennzeichnung auf überfällige Ausleihen an
        CALL HighlightOverdueLoans(ausleihenDataGrid)
    END FUNCTION

    // --- Farbliche Kennzeichnung für überfällige Ausleihen ---
    FUNCTION HighlightOverdueLoans(dataGrid)
        FOR EACH row IN dataGrid.Rows DO
            IF row["Status"] == "Überfällig" THEN
                // Setze beispielsweise den Hintergrund der Zeile auf Rot
                row.SetBackgroundColor("red")
            ELSE
                // Standardfarbe, falls nicht überfällig
                row.SetBackgroundColor("white")
            END IF
        END FOR
    END FUNCTION

    // --- Öffnen der Detailansicht eines ausgewählten Ausleihvorgangs ---
    FUNCTION OpenLoanDetails(loan)
        // Öffnet z. B. ein Detailformular, in dem der Benutzer den Vorgang bearbeiten (Rückgabe/Verlängerung) kann
        CALL ShowLoanDetailForm(loan)
    END FUNCTION

    // --- Haupt-Einstiegspunkt für das Modul "Aktuelle Ausleihen" ---
    FUNCTION MainAktuelleAusleihen()
        CALL InitializeAusleiheÜbersicht()
        CALL RefreshAusleihenListe()
        // Starte die Event-Schleife für die Ausleihe-Übersicht
        CALL StartEventLoop()
    END FUNCTION

END MODULE
}

