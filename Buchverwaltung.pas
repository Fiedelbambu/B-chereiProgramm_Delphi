unit Buchverwaltung;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Buchformular,
  Vcl.ExtCtrls, Data.DB, Vcl.Grids, Vcl.DBGrids;

type
  TBuchverwalter = class(TFrame)
    lblBuchverwaltung: TLabel;
    btn_BuchAnlegen: TButton;
    PaintBox1: TPaintBox;
    lblName: TLabel;
    lblGenre: TLabel;
    lblISBN: TLabel;
    lblAutor: TLabel;
    lblHerausgeber: TLabel;
    EdtName: TEdit;
    EdtGenre: TEdit;
    EdtISBN: TEdit;
    EdtAutor: TEdit;
    EdtHerausgeber: TEdit;
    btnSuche: TButton;
    lblSucherergebnisse: TLabel;
    DBGrid1: TDBGrid;
    procedure btn_BuchAnlegenClick(Sender: TObject);


  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

implementation

{$R *.dfm}

uses
MainFrame;




procedure TBuchverwalter.btn_BuchAnlegenClick(Sender: TObject);
begin
  if Assigned(Form1) then
    Form1.SwitchFrame('Buchverwalter');
end;





end.


{
// Pseudocode f�r das Modul Buchverwaltung
// Autor: Christian Fiedler

MODULE Buchverwaltung

    // --- Teil 1: Buchformular (Anlegen/Bearbeiten) ---
    // (Die Funktionen aus dem separaten Modul "Buchformular" k�nnen hier entweder integriert
    //  oder als Untermodul referenziert werden. Im Folgenden wird davon ausgegangen, dass
    //  das Buchformular �ber eigene Funktionen verf�gt, die hier aufgerufen werden.)

    // Funktion zum �ffnen des Buchformulars im "Neu"-Modus
    FUNCTION OpenNeuesBuchformular()
        CALL Buchformular.InitializeBuchformular("Neu")
    END FUNCTION

    // Funktion zum �ffnen des Buchformulars im "Bearbeiten"-Modus
    FUNCTION OpenBuchformularBearbeiten(ausgew�hltesBuch)
        IF ausgew�hltesBuch IS NULL THEN
            CALL ShowError("Bitte w�hlen Sie ein Buch aus, um es zu bearbeiten.")
            RETURN
        END IF
        CALL Buchformular.InitializeBuchformular("Bearbeiten", ausgew�hltesBuch)
    END FUNCTION

    // --- Teil 2: Buchsuche & Filter ---

    // Deklaration der Suchfelder f�r Buchsuche
    VARIABLE buchSuchFelder = {
        "Name": "",
        "Genre": "",
        "ISBN": "",
        "Autor": "",
        "Herausgeber": ""
    }

    // DataGrid f�r die Anzeige der Suchergebnisse
    VARIABLE buchDataGrid

    // Initialisierung der Suchoberfl�che
    FUNCTION InitializeBuchsuche()
        // Erstelle Suchfelder (Textfelder und Dropdowns) f�r die Kriterien
        CALL CreateSearchField("BuchSuchFelder", buchSuchFelder)

        // Erstelle ein DataGrid zur Darstellung der Suchergebnisse
        buchDataGrid = CALL CreateDataGrid("BuchDataGrid")

        // Registriere den Event-Handler f�r den Suchbutton
        SET OnClick("BuchSuchenButton") TO FUNCTION() { ExecuteBuchSuche() }

        // Registriere den Event-Handler f�r Doppelklick auf einen Eintrag im DataGrid
        SET OnDoubleClick(buchDataGrid) TO FUNCTION() {
            VARIABLE buch = GET SelectedRow(buchDataGrid)
            CALL OpenBuchformularBearbeiten(buch)
        }
    END FUNCTION

    // Funktion zum Ausf�hren der Buchsuche
    FUNCTION ExecuteBuchSuche()
        // Lese den Suchbegriff aus den Suchfeldern aus
        VARIABLE suchQuery = BuildSearchQuery(buchSuchFelder)

        // Rufe die Suchfunktion der Datenbank auf (Filterung nach Name, Genre, ISBN, etc.)
        VARIABLE ergebnisse = Database.SearchBooks(suchQuery)

        // F�lle das DataGrid mit den Suchergebnissen
        CALL PopulateDataGrid(buchDataGrid, ergebnisse)
    END FUNCTION

    // Funktion zum Aktualisieren der vollst�ndigen Buchliste (z.B. nach Speichern)
    FUNCTION RefreshBuchList()
        VARIABLE alleB�cher = Database.GetAllBooks()
        CALL PopulateDataGrid(buchDataGrid, alleB�cher)
    END FUNCTION

    // --- Initialisierung der gesamten Buchverwaltung ---
    FUNCTION InitializeBuchverwaltungUI()
        // Initialisiere die Such- und Filter-Komponenten
        CALL InitializeBuchsuche()

        // (Optional) Initialisiere zus�tzliche UI-Komponenten, z.B. Buttons f�r "Neues Buch"
        SET OnClick("NeuesBuchButton") TO OpenNeuesBuchformular
    END FUNCTION

    // --- Hauptfunktion des Moduls Buchverwaltung ---
    FUNCTION MainBuchverwaltung()
        // Initialisiere die Benutzeroberfl�che
        CALL InitializeBuchverwaltungUI()

        // Lade die initiale Buchliste in das DataGrid
        CALL RefreshBuchList()

        // Starte die Event-Schleife der Buchverwaltungsoberfl�che
        CALL StartEventLoop()
    END FUNCTION

END MODULE

}
