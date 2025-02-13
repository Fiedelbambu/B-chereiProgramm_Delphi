unit Mahnungsformular;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TFrame3 = class(TFrame)
    lblKundename: TLabel;
    lblNameDemo: TLabel;
    lblAusgelieheneBuecher: TLabel;
    lblFris: TLabel;
    lblBemerkung: TLabel;
    lblAutomatischberechnet: TLabel;
    lbl: TLabel;
    btnMahnungsSenden: TButton;
    ListBox1: TListBox;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

implementation

{$R *.dfm}

end.


{// Pseudocode für das Mahnungsformular
// Autor: Christian Fiedler

MODULE Mahnungsformular

    // --- Deklaration der UI-Komponenten und Variablen ---
    VARIABLE kundenDaten        // Kundendaten (z. B. Kundennummer, Name, E-Mail, etc.)
    VARIABLE ausgelieheneBuecher // Liste der aktuell ausgeliehenen Bücher (z. B. als Array oder DataGrid)
    VARIABLE fristUeberfaellig  // Automatisch berechnete Fristüberfälligkeit (z. B. Datum oder Text)
    VARIABLE bemerkungen        // Freitextfeld für zusätzliche Bemerkungen
    VARIABLE buttonMahnungSenden // Button zum Versenden der Mahnung

    // --- Initialisierung des Mahnungsformulars ---
    FUNCTION InitializeMahnungsformular(customerId)
        // Lade Kundendaten anhand der übergebenen customerId
        kundenDaten = Database.GetCustomerDetails(customerId)
        // Lade die Liste der ausgeliehenen Bücher für den Kunden
        ausgelieheneBuecher = Database.GetLoanedBooks(customerId)

        // Optional: Automatische Berechnung der Fristüberfälligkeit (z. B. basierend auf Ausleihdatum)
        fristUeberfaellig = CalculateFristUeberfaelligkeit(ausgelieheneBuecher)

        // Darstellung der Kundendaten und ausgeliehenen Bücher in der UI (z. B. in einem DataGrid oder als Detailansicht)
        CALL DisplayCustomerData(kundenDaten)
        CALL DisplayLoanedBooks(ausgelieheneBuecher)
        CALL DisplayFristUeberfaelligkeit(fristUeberfaellig)

        // Zeige das Mahnungsformular an
        CALL ShowForm("Mahnungsformular")
    END FUNCTION

    // --- Validierung der Eingaben im Mahnungsformular ---
    FUNCTION ValidateMahnungsForm() RETURNS BOOLEAN
        // Prüfe, ob Kundendaten vorhanden sind
        IF kundenDaten IS NULL THEN
            CALL ShowError("Kundendaten konnten nicht geladen werden.")
            RETURN FALSE
        END IF

        // Optional: Weitere Validierungen, z. B. ob das Bemerkungsfeld (falls Pflicht) korrekt ausgefüllt ist
        RETURN TRUE
    END FUNCTION

    // --- Zusammenstellen und Versenden der Mahnung ---
    FUNCTION SendMahnung()
        // Validierung der Eingaben
        IF NOT ValidateMahnungsForm() THEN
            RETURN
        END IF

        // Erstelle den E-Mail-Inhalt basierend auf den Kundendaten, ausgeliehenen Büchern und Fristüberfälligkeit
        VARIABLE emailContent = ComposeEmailContent(kundenDaten, ausgelieheneBuecher, fristUeberfaellig, bemerkungen)

        // Versende die Mahnung per E-Mail unter Verwendung der eingebetteten E-Mail-Konfiguration
        VARIABLE emailSent = EmailService.SendEmail(
            recipient = kundenDaten.Email,
            subject = "Mahnung – Zahlungserinnerung",
            body = emailContent
        )

        IF emailSent THEN
            CALL ShowMessage("Mahnung wurde erfolgreich versendet.")
            // Protokolliere die versendete Mahnung in der Historie
            CALL LogMahnung(kundenDaten.id, GetCurrentDate(), "versendet", bemerkungen)
        ELSE
            CALL ShowError("Fehler beim Versenden der Mahnung. Bitte überprüfen Sie die E-Mail-Konfiguration.")
        END IF

        // Schließe das Mahnungsformular nach dem Versand
        CALL CloseForm("Mahnungsformular")
    END FUNCTION

    // --- Event-Handler für den "Mahnung senden"-Button ---
    FUNCTION OnMahnungSendenButtonClick()
        CALL SendMahnung()
    END FUNCTION

    // --- Haupt-Einstiegspunkt des Mahnungsformulars ---
    // customerId wird übergeben, um die entsprechenden Kundendaten zu laden
    FUNCTION MainMahnungsformular(customerId)
        CALL InitializeMahnungsformular(customerId)
        // Registriere den Event-Handler für den Button
        SET OnClick("MahnungSendenButton") TO OnMahnungSendenButtonClick
        // Starte die Event-Schleife für das Mahnungsformular
        CALL StartEventLoop()
    END FUNCTION

    // --- Hilfsfunktionen (Beispiele) ---
    FUNCTION CalculateFristUeberfaelligkeit(loanedBooks) RETURNS STRING
        // Beispiel: Berechne basierend auf den Ausleihdaten, ob Bücher überfällig sind
        // Hier wird ein fiktiver Text zurückgegeben
        RETURN "Überfällig seit 5 Tagen"
    END FUNCTION

    FUNCTION ComposeEmailContent(customer, books, frist, remarks) RETURNS STRING
        VARIABLE content = ""
        content = "Sehr geehrte/r " + customer.Name + ",\n\n"
        content = content + "bei Ihnen sind folgende Bücher überfällig:\n"
        FOR EACH book IN books DO
            content = content + "- " + book.Name + " (ISBN: " + book.ISBN + ")\n"
        END FOR
        content = content + "\nFristüberfälligkeit: " + frist + "\n"
        IF remarks IS NOT EMPTY THEN
            content = content + "\nBemerkungen: " + remarks + "\n"
        END IF
        content = content + "\nBitte begleichen Sie den fälligen Betrag.\n\nMit freundlichen Grüßen\nIhr Team"
        RETURN content
    END FUNCTION

END MODULE
}
