unit Kundenform;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ComCtrls, Vcl.ExtCtrls;

type
  TFrame2 = class(TFrame)
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
    Edit2: TEdit;
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
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

implementation

{$R *.dfm}

end.


{// Pseudocode für das Kundenformular (Anlegen/Bearbeiten)
// Autor: Christian Fiedler

MODULE Kundenformular

    // Deklaration der Eingabefelder, gruppiert nach persönlichen Daten und Adresse
    VARIABLE persönlicheDaten = {
        "Name": "",
        "Vorname": "",
        "Geburtsdatum": "",  // Kalenderfeld
        "Email": "",
        "Telefonnummer": ""
    }

    VARIABLE adresseDaten = {
        "Straße": "",
        "PLZ": "",
        "Ort": ""
    }

    // Hauptfunktion zum Initialisieren des Formulars
    FUNCTION InitializeKundenformular(modus)
        // modus: "Neu" oder "Bearbeiten"
        IF modus == "Neu" THEN
            // Felder leeren
            CALL ClearFields(persönlicheDaten)
            CALL ClearFields(adresseDaten)
        ELSE IF modus == "Bearbeiten" THEN
            VARIABLE ausgewählterKunde = GET SelectedCustomerFromList()
            IF ausgewählterKunde IS NULL THEN
                CALL ShowError("Kein Kunde ausgewählt. Bitte wählen Sie einen Kunden zur Bearbeitung aus.")
                RETURN
            END IF
            // Lade die Kundendaten in die Eingabefelder
            CALL LoadCustomerData(ausgewählterKunde, persönlicheDaten, adresseDaten)
        END IF

        // Zeige das Kundenformular an
        CALL ShowForm("Kundenformular")
    END FUNCTION

    // Validierung der Eingabefelder
    FUNCTION ValidateKundenForm() RETURNS BOOLEAN
        // Überprüfe Pflichtfelder in persönlichen Daten
        IF persönlicheDaten["Name"] IS EMPTY OR persönlicheDaten["Vorname"] IS EMPTY THEN
            CALL ShowError("Name und Vorname sind Pflichtfelder!")
            RETURN FALSE
        END IF

        // Überprüfe das E-Mail-Format (einfache Prüfung)
        IF NOT Contains(persönlicheDaten["Email"], "@") THEN
            CALL ShowError("Bitte geben Sie eine gültige E-Mail-Adresse ein!")
            RETURN FALSE
        END IF

        // Weitere Validierungen (z.B. Telefonnummer, Geburtsdatum) können hier ergänzt werden
        RETURN TRUE
    END FUNCTION

    // Speichern der Kundendaten (Neuanlage oder Update)
    FUNCTION SaveKundenForm()
        // Validierung der Eingaben
        IF NOT ValidateKundenForm() THEN
            RETURN
        END IF

        // Daten zusammenführen
        VARIABLE kundenDaten = Merge(persönlicheDaten, adresseDaten)

        // Prüfen, ob es sich um einen neuen Kunden handelt
        IF IsNewCustomer(kundenDaten) THEN
            CALL Database.AddCustomer(kundenDaten)
            CALL ShowMessage("Neuer Kunde wurde erfolgreich angelegt.")
        ELSE
            CALL Database.UpdateCustomer(kundenDaten)
            CALL ShowMessage("Kundendaten wurden erfolgreich aktualisiert.")
        END IF

        // Nach dem Speichern: Formular schließen und Übersicht aktualisieren
        CALL CloseForm("Kundenformular")
        CALL RefreshCustomerList()
    END FUNCTION

    // Event-Handler für den "Speichern"-Button
    FUNCTION OnSaveButtonClick()
        CALL SaveKundenForm()
    END FUNCTION

    // Event-Handler für den "Abbrechen"-Button (optional)
    FUNCTION OnCancelButtonClick()
        CALL CloseForm("Kundenformular")
    END FUNCTION

    // Haupt-Einstiegspunkt für das Kundenformular
    FUNCTION MainKundenformular(modus)
        CALL InitializeKundenformular(modus)
        // Registriere Event-Handler für Buttons
        SET OnClick("SpeichernButton") TO OnSaveButtonClick
        SET OnClick("AbbrechenButton") TO OnCancelButtonClick
        // Starte die Event-Schleife für das Formular
        CALL StartEventLoop()
    END FUNCTION

END MODULE
}
