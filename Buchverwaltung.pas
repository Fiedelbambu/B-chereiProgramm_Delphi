unit Buchverwaltung;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls,AusleiheDialog,Ausleihform, Data.DB, Vcl.Grids, Vcl.DBGrids, Data.Win.ADODB, Buchformular;

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
    DataSource1: TDataSource;

    procedure btn_BuchAnlegenClick(Sender: TObject);
    procedure FrameEnter(Sender: TObject);
    procedure btnSucheClick(Sender: TObject);
    procedure DBGrid1DblClick(Sender: TObject);

  private
    FADOQuery: TADOQuery;
    FDataSource: TDataSource;
    procedure LoadBooks;
    procedure AdjustGridColumns;
    procedure ApplyLocalFilter;


  public
    property ADOQuery: TADOQuery read FADOQuery;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

uses
  MainFrame, DatenbankKonfig;

{ --- Konstruktor / Destructor --- }

constructor TBuchverwalter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // Laufzeit-Query erstellen (unabh�ngig von ADOQuery1)
  FADOQuery := TADOQuery.Create(Self);
  FADOQuery.Name := 'ADOQueryBuchverwalter';

  // DataSource erzeugen
  FDataSource := TDataSource.Create(Self);
  FDataSource.DataSet := FADOQuery;

  // DBGrid an die DataSource binden
  DBGrid1.DataSource := FDataSource;

end;




procedure TBuchverwalter.DBGrid1DblClick(Sender: TObject);
var
  SelectedBookName: string;
begin
  if (DBGrid1.DataSource = nil) or DBGrid1.DataSource.DataSet.IsEmpty then
    Exit;

  // Lies den Buchnamen aus dem Dataset
  SelectedBookName := DBGrid1.DataSource.DataSet.FieldByName('name').AsString;

  // Hier den korrekten FrameType �bergeben � exakt wie in SwitchFrame definiert
  Form1.SwitchFrame('AusleihDialog');

  // Pr�fen und Casten: Der aktuelle Frame muss vom Typ TAusleihDialog sein
  if Form1.CurrentFrame is TAusleihDialog then
    TAusleihDialog(Form1.CurrentFrame).SetBookData(SelectedBookName);
end;


destructor TBuchverwalter.Destroy;
begin
  inherited Destroy;
end;

{ --- Beim Betreten des Frames: Alle Datens�tze laden --- }
procedure TBuchverwalter.FrameEnter(Sender: TObject);
begin
  LoadBooks;
end;

{ --- Alle Datens�tze laden (ohne WHERE) --- }
procedure TBuchverwalter.LoadBooks;
begin
  if Assigned(Form1) and Assigned(Form1.DBKonfig) then
  begin
    FADOQuery.Connection := Form1.DBKonfig.ADOConnection;
    FADOQuery.Close;
    FADOQuery.SQL.Text :=
  'SELECT ' +
  '  name AS Name, ' +
  '  genre AS Genre, ' +
  '  isbn AS ISBN, ' +
  '  author AS Author, ' +
  '  publisher AS Herausgeber, ' +
  '  edition AS Edition, ' +
  '  shelf AS Regal, ' +
  '  position AS Fach, ' +
  '  language AS Sprache, ' +
  '  pages AS Seitenanzahl, ' +
  '  publication_year AS Ver�ffentlicht, ' +
  '  inventory_number AS Inventarnummer ' +
  'FROM books';
 try
      FADOQuery.Open;
      AdjustGridColumns;
    except
      on E: Exception do
        ShowMessage('Fehler beim Laden der B�cher: ' + E.Message);
    end;
  end
  else
    ShowMessage('DBKonfig oder Form1 nicht verf�gbar.');
end;

{ --- Spaltenbreiten anpassen --- }
procedure TBuchverwalter.AdjustGridColumns;
var
  i, MaxWidth, TextWidth: Integer;
  Column: TColumn;
  bmp: TBitmap;
  ds: TDataSet;
begin
  ds := DBGrid1.DataSource.DataSet;
  if ds = nil then Exit;

  bmp := TBitmap.Create;
  try
    bmp.Canvas.Font := DBGrid1.Font;
    for i := 0 to DBGrid1.Columns.Count - 1 do
    begin
      Column := DBGrid1.Columns[i];
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

{ --- Button "Buch anlegen": neu laden + Frame-Wechsel --- }
procedure TBuchverwalter.btn_BuchAnlegenClick(Sender: TObject);
var
  BuchForm: TBuchform;
begin
BuchForm := TBuchform.Create(Self);
  Form1.SwitchFrame('Buchverwalter');
end;

{ --- Button "Suche": Lokaler Filter auf Genre, Name, ISBN, Autor, Herausgeber --- }
procedure TBuchverwalter.btnSucheClick(Sender: TObject);
begin
  if not FADOQuery.Active then
    LoadBooks;

  ApplyLocalFilter;
end;

// F�rs Debugen kann  ShowMessage(FADOQuery.SQL.Text); verwendet werden
{ --- Lokaler Filter f�r Genre, Name, ISBN, Autor, Herausgeber --- }
procedure TBuchverwalter.ApplyLocalFilter;
var
  FilterParts: TStringList;
  sGenre, sName, sISBN, sAutor, sPublisher: string;
  i: Integer;
begin
  // Werte holen
  sGenre := Trim(EdtGenre.Text);
  sName := Trim(EdtName.Text);
  sISBN := Trim(EdtISBN.Text);
  sAutor := Trim(EdtAutor.Text);
  sPublisher := Trim(EdtHerausgeber.Text);

  FADOQuery.DisableControls;
  try
    // Filter erst mal ausschalten
    FADOQuery.Filtered := False;
    FADOQuery.Filter := '';

    FilterParts := TStringList.Create;
    try
      // Wenn Felder gef�llt, f�gen wir LIKE-Bedingungen hinzu
      if sGenre <> '' then
        // Teiltreffer: genre LIKE '%sGenre%'
        FilterParts.Add(Format('genre LIKE ''%%%s%%''', [sGenre]));

      if sName <> '' then
        FilterParts.Add(Format('name LIKE ''%%%s%%''', [sName]));

      if sISBN <> '' then
        FilterParts.Add(Format('isbn LIKE ''%%%s%%''', [sISBN]));

      if sAutor <> '' then
        FilterParts.Add(Format('author LIKE ''%%%s%%''', [sAutor]));

      if sPublisher <> '' then
        FilterParts.Add(Format('publisher LIKE ''%%%s%%''', [sPublisher]));

      if FilterParts.Count > 0 then
      begin
        // Wir verbinden alle Teile mit " AND "
        // (TStringList.Text w�rde Zeilenumbr�che einf�gen, deshalb machen wir es manuell)
        FADOQuery.Filter := FilterParts[0];
        for i := 1 to FilterParts.Count - 1 do
          FADOQuery.Filter := FADOQuery.Filter + ' AND ' + FilterParts[i];

        FADOQuery.Filtered := True;
      end
      else
      begin
        // Keine Filter-Bedingungen => alle Datens�tze anzeigen
        FADOQuery.Filter := '';
        FADOQuery.Filtered := False;
      end;
    finally
      FilterParts.Free;
    end;
  finally
    FADOQuery.EnableControls;
  end;
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
