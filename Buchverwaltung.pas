unit Buchverwaltung;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls, Data.DB, Vcl.Grids, Vcl.DBGrids, Data.Win.ADODB, Buchformular;

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
    ADOQuery1: TADOQuery;
    DBGrid1: TDBGrid;
    procedure btn_BuchAnlegenClick(Sender: TObject);
    procedure FrameEnter(Sender: TObject);

 private

    { Private-Deklarationen }
    FADOQuery: TADOQuery;
    FDataSource: TDataSource;
    procedure LoadBooks;
    procedure AdjustGridColumns;  // Hier deklarieren wir die Methode


  public
    { Public-Deklarationen }
    property ADOQuery: TADOQuery read FADOQuery;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  end;

implementation

{$R *.dfm}

uses
  MainFrame, DatenbankKonfig;

constructor TBuchverwalter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // Erstellen der Laufzeit-Query (unabhängig von der Design-Komponente ADOQuery1)
  FADOQuery := TADOQuery.Create(Self);
  FADOQuery.Name := 'ADOQueryBuchverwalter';
  // Erstellen einer DataSource, die unsere Query als DataSet nutzt
  FDataSource := TDataSource.Create(Self);
  FDataSource.DataSet := FADOQuery;
  // DBGrid1 an die DataSource binden
  DBGrid1.DataSource := FDataSource;
end;

destructor TBuchverwalter.Destroy;
begin
  // Alle Komponenten, die mit Self als Owner erstellt wurden, werden automatisch freigegeben.
  inherited Destroy;
end;


{ AdjustGridColumns passt die Breite der Spalten automatisch an die längsten Inhalte an }
procedure TBuchverwalter.AdjustGridColumns;
var
  i, MaxWidth, TextWidth: Integer;
  Column: TColumn;
  bmp: TBitmap;
  ds: TDataSet;
begin
  ds := DBGrid1.DataSource.DataSet;
  if ds = nil then Exit;

  // Erstelle ein temporäres Bitmap, um einen Canvas mit gültigem Handle zu bekommen
  bmp := TBitmap.Create;
  try
    bmp.Canvas.Font := DBGrid1.Font;
    for i := 0 to DBGrid1.Columns.Count - 1 do
    begin
      Column := DBGrid1.Columns[i];
      // Starte mit der Breite der Überschrift + etwas Abstand
      MaxWidth := bmp.Canvas.TextWidth(Column.Title.Caption) + 8;
      ds.DisableControls;
      try
        ds.First;
        while not ds.Eof do
        begin
          TextWidth := bmp.Canvas.TextWidth(VarToStr(Column.Field.AsString)) + 8;
          if TextWidth > MaxWidth then
            MaxWidth := TextWidth;
          ds.Next;
        end;
      finally
        ds.First;
        ds.EnableControls;
      end;
      Column.Width := MaxWidth;
    end;
  finally
    bmp.Free;
  end;
end;

//FrameEnter in den Eigenschaften OnEnter hinzufügen damit die Datenbank geladen wird
procedure TBuchverwalter.FrameEnter(Sender: TObject);
begin
  LoadBooks;
end;

{ LoadBooks weist zunächst die zentrale Connection zu,
  öffnet die Query und passt dann die Spaltenbreiten an }
procedure TBuchverwalter.LoadBooks;
begin
  if Assigned(Form1) and Assigned(Form1.DBKonfig) then
  begin
    FADOQuery.Connection := Form1.DBKonfig.ADOConnection;
    FADOQuery.Close;
    FADOQuery.SQL.Text :=
      'SELECT book_id, name, genre, isbn, author, publisher, edition, shelf, position, language, pages, publication_year, inventory_number FROM books';
    try
      FADOQuery.Open;
      AdjustGridColumns; // Automatisch die Spaltenbreiten anpassen
    except
      on E: Exception do
        ShowMessage('Fehler beim Laden der Bücher: ' + E.Message);
    end;
  end
  else
    ShowMessage('DBKonfig oder Form1 nicht verfügbar.');
end;

procedure TBuchverwalter.btn_BuchAnlegenClick(Sender: TObject);
begin
  // Beim Klick wird zuerst versucht, die Bücher-Daten zu laden
  LoadBooks;
  // Anschließend erfolgt ein Frame-Wechsel.
  // Achtung: SwitchFrame('Buchverwalter') erzeugt einen neuen Frame und gibt das aktuelle frei.
  Form1.SwitchFrame('Buchverwalter');
end;

end.


{
// Pseudocode für das Modul Buchverwaltung
// Autor: Christian Fiedler

MODULE Buchverwaltung

    // --- Teil 1: Buchformular (Anlegen/Bearbeiten) ---
    // (Die Funktionen aus dem separaten Modul "Buchformular" können hier entweder integriert
    //  oder als Untermodul referenziert werden. Im Folgenden wird davon ausgegangen, dass
    //  das Buchformular über eigene Funktionen verfügt, die hier aufgerufen werden.)

    // Funktion zum Öffnen des Buchformulars im "Neu"-Modus
    FUNCTION OpenNeuesBuchformular()
        CALL Buchformular.InitializeBuchformular("Neu")
    END FUNCTION

    // Funktion zum Öffnen des Buchformulars im "Bearbeiten"-Modus
    FUNCTION OpenBuchformularBearbeiten(ausgewähltesBuch)
        IF ausgewähltesBuch IS NULL THEN
            CALL ShowError("Bitte wählen Sie ein Buch aus, um es zu bearbeiten.")
            RETURN
        END IF
        CALL Buchformular.InitializeBuchformular("Bearbeiten", ausgewähltesBuch)
    END FUNCTION

    // --- Teil 2: Buchsuche & Filter ---

    // Deklaration der Suchfelder für Buchsuche
    VARIABLE buchSuchFelder = {
        "Name": "",
        "Genre": "",
        "ISBN": "",
        "Autor": "",
        "Herausgeber": ""
    }

    // DataGrid für die Anzeige der Suchergebnisse
    VARIABLE buchDataGrid

    // Initialisierung der Suchoberfläche
    FUNCTION InitializeBuchsuche()
        // Erstelle Suchfelder (Textfelder und Dropdowns) für die Kriterien
        CALL CreateSearchField("BuchSuchFelder", buchSuchFelder)

        // Erstelle ein DataGrid zur Darstellung der Suchergebnisse
        buchDataGrid = CALL CreateDataGrid("BuchDataGrid")

        // Registriere den Event-Handler für den Suchbutton
        SET OnClick("BuchSuchenButton") TO FUNCTION() { ExecuteBuchSuche() }

        // Registriere den Event-Handler für Doppelklick auf einen Eintrag im DataGrid
        SET OnDoubleClick(buchDataGrid) TO FUNCTION() {
            VARIABLE buch = GET SelectedRow(buchDataGrid)
            CALL OpenBuchformularBearbeiten(buch)
        }
    END FUNCTION

    // Funktion zum Ausführen der Buchsuche
    FUNCTION ExecuteBuchSuche()
        // Lese den Suchbegriff aus den Suchfeldern aus
        VARIABLE suchQuery = BuildSearchQuery(buchSuchFelder)

        // Rufe die Suchfunktion der Datenbank auf (Filterung nach Name, Genre, ISBN, etc.)
        VARIABLE ergebnisse = Database.SearchBooks(suchQuery)

        // Fülle das DataGrid mit den Suchergebnissen
        CALL PopulateDataGrid(buchDataGrid, ergebnisse)
    END FUNCTION

    // Funktion zum Aktualisieren der vollständigen Buchliste (z.B. nach Speichern)
    FUNCTION RefreshBuchList()
        VARIABLE alleBücher = Database.GetAllBooks()
        CALL PopulateDataGrid(buchDataGrid, alleBücher)
    END FUNCTION

    // --- Initialisierung der gesamten Buchverwaltung ---
    FUNCTION InitializeBuchverwaltungUI()
        // Initialisiere die Such- und Filter-Komponenten
        CALL InitializeBuchsuche()

        // (Optional) Initialisiere zusätzliche UI-Komponenten, z.B. Buttons für "Neues Buch"
        SET OnClick("NeuesBuchButton") TO OpenNeuesBuchformular
    END FUNCTION

    // --- Hauptfunktion des Moduls Buchverwaltung ---
    FUNCTION MainBuchverwaltung()
        // Initialisiere die Benutzeroberfläche
        CALL InitializeBuchverwaltungUI()

        // Lade die initiale Buchliste in das DataGrid
        CALL RefreshBuchList()

        // Starte die Event-Schleife der Buchverwaltungsoberfläche
        CALL StartEventLoop()
    END FUNCTION

END MODULE

}
