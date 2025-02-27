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
    procedure DBGrid1DrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);

  private
    FADOQuery: TADOQuery;
    FDataSource: TDataSource;
    procedure LoadBooks;
    procedure AdjustGridColumns;
    procedure ApplyLocalFilter;
    function CheckBuchStatus: Boolean;

  public
    property ADOQuery: TADOQuery read FADOQuery;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

uses
  MainFrame, DatenbankKonfig;

{ --- Konstruktor / Destructor besser Lernen --- }

constructor TBuchverwalter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // Laufzeit-Query erstellen (unabhängig von ADOQuery1)
  FADOQuery := TADOQuery.Create(Self);
  FADOQuery.Name := 'ADOQueryBuchverwalter';

  // DataSource erzeugen
  FDataSource := TDataSource.Create(Self);
  FDataSource.DataSet := FADOQuery;

  // DBGrid an die DataSource binden
  DBGrid1.DataSource := FDataSource;
  DBGrid1.OnDblClick := DBGrid1DblClick;


end;

destructor TBuchverwalter.Destroy;
begin
  inherited Destroy;
end;


procedure TBuchverwalter.DBGrid1DblClick(Sender: TObject);
var
  SelectedBookName: string;
  sStatus: string;
begin
  if (DBGrid1.DataSource = nil) or DBGrid1.DataSource.DataSet.IsEmpty then
    Exit;

  sStatus := LowerCase(Trim(DBGrid1.DataSource.DataSet.FieldByName('Status').AsString));
  if sStatus = 'ausgeliehen' then
  begin
    ShowMessage('Buch ist verliehen.');
    Exit;
  end;

  // Lies den Buchnamen aus dem Dataset
  SelectedBookName := DBGrid1.DataSource.DataSet.FieldByName('name').AsString;

  // Hier den korrekten FrameType übergeben – exakt wie in SwitchFrame definiert
  Form1.SwitchFrame('AusleihDialog');

  // Prüfen und Casten: Der aktuelle Frame muss vom Typ TAusleihDialog sein
  if Form1.CurrentFrame is TAusleihDialog then
    TAusleihDialog(Form1.CurrentFrame).SetBookData(SelectedBookName);
end;



procedure TBuchverwalter.DBGrid1DrawColumnCell(Sender: TObject;
  const Rect: TRect; DataCol: Integer; Column: TColumn;
  State: TGridDrawState);
begin
  // Überprüfe, ob der Status "ausgeliehen" ist
  if LowerCase(DBGrid1.DataSource.DataSet.FieldByName('Status').AsString) = 'ausgeliehen' then
    DBGrid1.Canvas.Brush.Color := clRed  // Zeile rot einfärben
  else
    DBGrid1.Canvas.Brush.Color := clWindow; // Standardfarbe

  DBGrid1.DefaultDrawColumnCell(Rect, DataCol, Column, State);
end;


function TBuchverwalter.CheckBuchStatus: Boolean;
var
  sStatus: string;
begin
  Result := True; // Standard: Buch ist verfügbar
  if (DataSource1 = nil) or (DataSource1.DataSet.IsEmpty) then Exit;

  sStatus := Trim(DataSource1.DataSet.FieldByName('Status').AsString);
  ShowMessage('Status des Buches: [' + sStatus + ']'); // Debug-Ausgabe

  if SameText(sStatus, 'ausgeliehen') then
  begin
    ShowMessage('Buch ist verliehen');
    // Beispiel: Deaktiviere Bearbeitungsmodus (falls vorhanden)
    DBGrid1.Options := DBGrid1.Options - [dgEditing];
    Result := False;
  end;
end;



{ --- Beim Betreten des Frames: Alle Datensätze laden --- }
procedure TBuchverwalter.FrameEnter(Sender: TObject);
begin
  LoadBooks;
end;

{ --- Alle Datensätze laden (ohne WHERE) --- }
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
  '  publication_year AS Veröffentlicht, ' +
  '  inventory_number AS Inventarnummer, ' +   // Komma hier
  '  status AS Status ' +                        // Beachte das Leerzeichen am Ende
  'FROM books';
 try
      FADOQuery.Open;
      AdjustGridColumns;
    except
      on E: Exception do
        ShowMessage('Fehler beim Laden der Bücher: ' + E.Message);
    end;
  end
  else
    ShowMessage('DBKonfig oder Form1 nicht verfügbar.');
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

// Fürs Debugen kann  ShowMessage(FADOQuery.SQL.Text); verwendet werden
{ --- Lokaler Filter für Genre, Name, ISBN, Autor, Herausgeber --- }
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
      // Wenn Felder gefüllt, fügen wir LIKE-Bedingungen hinzu
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
        // (TStringList.Text würde Zeilenumbrüche einfügen, deshalb machen wir es manuell)
        FADOQuery.Filter := FilterParts[0];
        for i := 1 to FilterParts.Count - 1 do
          FADOQuery.Filter := FADOQuery.Filter + ' AND ' + FilterParts[i];

        FADOQuery.Filtered := True;
      end
      else
      begin
        // Keine Filter-Bedingungen => alle Datensätze anzeigen
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

