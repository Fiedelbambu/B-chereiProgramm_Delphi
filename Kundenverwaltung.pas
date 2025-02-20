unit Kundenverwaltung;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, Vcl.Grids,
  Vcl.DBGrids, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.WinXCtrls,
  Data.Win.ADODB; // Für TADOQuery und TDataSource

type
  // Du kannst TFrame1 umbenennen, z.B. in TKundenverwalter, falls gewünscht
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
    procedure FrameEnter(Sender: TObject); // <-- Für das automatische Laden
  private
    { Private-Deklarationen }
    FADOQuery: TADOQuery;      // Laufzeit-Query
    FDataSource: TDataSource;  // DataSource für DBGrid

    procedure LoadCustomers;   // Daten laden
    procedure AdjustGridColumns; // Spaltenbreiten anpassen

  public
    { Public-Deklarationen }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

uses
  MainFrame, DatenbankKonfig; // Für Form1 und DBKonfig

{ TFrame1 }

// Konstruktor: Laufzeit-Query & DataSource erstellen
constructor TFrame1.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FADOQuery := TADOQuery.Create(Self);
  FADOQuery.Name := 'ADOQueryKunden'; // optional

  FDataSource := TDataSource.Create(Self);
  FDataSource.DataSet := FADOQuery;

  DBGrid1.DataSource := FDataSource;

  // Falls du Teiltreffer in Delphi-Filtern brauchst:
  // FADOQuery.FilterOptions := [foCaseInsensitive];
end;

// Destruktor: Alles wird vom Owner (Self) automatisch freigegeben
destructor TFrame1.Destroy;
begin
  inherited Destroy;
end;

// Wird aufgerufen, wenn du im Designer OnEnter = FrameEnter verknüpft hast
procedure TFrame1.FrameEnter(Sender: TObject);
begin
  LoadCustomers;
end;

// Laden der Kundendaten aus Tabelle "customers"
procedure TFrame1.LoadCustomers;
begin
  if Assigned(Form1) and Assigned(Form1.DBKonfig) then
  begin
    FADOQuery.Connection := Form1.DBKonfig.ADOConnection;
    FADOQuery.Close;
    // Aliasnamen für deutsche Spaltenbezeichnungen
    FADOQuery.SQL.Text :=
      'SELECT ' +
      '  first_name AS Name, ' +
      '  last_name AS Familienname, ' +
      '  email AS "E-Mail", ' +       // Anführungszeichen nötig, wenn du Sonderzeichen wie "-" nutzen willst
      '  phone AS Telefon, ' +
      '  birth_date AS Geburtsdatum, ' +
      '  street AS Straße, ' +
      '  postal_code AS Postleitzahl, ' +
      '  city AS Stadt ' +
      'FROM customers';

    try
      FADOQuery.Open;
      AdjustGridColumns;
    except
      on E: Exception do
        ShowMessage('Fehler beim Laden der Kunden: ' + E.Message);
    end;
  end
  else
    ShowMessage('DBKonfig oder Form1 nicht verfügbar.');
end;

// Spaltenbreiten anpassen (wie in Buchverwaltung)
procedure TFrame1.AdjustGridColumns;
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
      // Mindestbreite = Breite des Spaltentitels + 8
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

// Button-Klick: Kunde anlegen (z. B. neues Kundenformular)
procedure TFrame1.btn_KundenAnlegenClick(Sender: TObject);
begin
  // Falls du ein separates Formular/Frame für "Kunden anlegen" hast:
  // Form1.SwitchFrame('KundenAnlegen') oder so.
  // Oder du rufst eine Prozedur "ShowKundenform" auf:
  if Assigned(Form1) then
    Form1.ShowKundenform;
end;

end.

