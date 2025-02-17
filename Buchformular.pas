unit Buchformular;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Data.DB, Data.Win.ADODB, IniFiles,
   DatenbankKonfig;

type
  // Record für Buchinformationen
 TBuchDaten = record
  ID: Integer;               // Primärschlüssel (AUTO_INCREMENT) in DB
  Name: string;
  Genre: string;
  ISBN: string;             // UNIQUE KEY in DB
  Autor: string;            // => author
  Herausgeber: string;      // => publisher
  Auflage: string;          // => edition
  Regal: string;            // => shelf
  Platznummer: string;      // => position
  Sprache: string;          // => language
  Seitenanzahl: Integer;    // => pages
  Publikationsjahr: Integer;// => publication_year
end;


  // Frame für das Buchformular
  TBuchform = class(TFrame)
    lblBuchformular: TLabel;
    lblSprache: TLabel;
    lblPlatznummer: TLabel;
    lblRegal: TLabel;
    lblAuflage: TLabel;
    lblHerausgeber: TLabel;
    lblAutor: TLabel;
    lblISBN: TLabel;
    lblGenre: TLabel;
    lblName: TLabel;
    EdtPublikationsjahr: TEdit;
    EdtSeitenanzahl: TEdit;
    EdtSprache: TEdit;
    EdtPlatznummer: TEdit;
    EdtRegal: TEdit;
    EdtAuflage: TEdit;
    EdtHerausgeber: TEdit;
    EdtAutor: TEdit;
    EdtIsbn: TEdit;
    EdtGenre: TEdit;
    EdtName: TEdit;
    btnSpeichern: TButton;
    btnBearbeiten: TButton;
    btnLoeschen: TButton;
    ADOQuery: TADOQuery;
    procedure btnSpeichernClick(Sender: TObject);
    procedure btnBearbeitenClick(Sender: TObject);
    procedure btnLoeschenClick(Sender: TObject);

  private
    FBuchDaten: TBuchDaten;
    procedure LadeBuchDaten(const ABuchDaten: TBuchDaten);
    procedure SpeichereBuchDaten;
    procedure LoescheBuchDaten;
    function LadeSQLQuery(const Key: string): string;
  public
    procedure InitializeBuchformular(const ABuchDaten: TBuchDaten);
  end;

implementation

{$R *.dfm}

// Laden der SQL-Abfrage aus der INI-Datei
function TBuchform.LadeSQLQuery(const Key: string): string;
var
  IniFile: TIniFile;
  FilePath: string;
begin
  FilePath := ExtractFilePath(ParamStr(0)) + 'queries.ini';
  IniFile := TIniFile.Create(FilePath);
  try
    Result := IniFile.ReadString('SQL', Key, '');
  finally
    IniFile.Free;
  end;
end;

// Initialisierung des Formulars
procedure TBuchform.InitializeBuchformular(const ABuchDaten: TBuchDaten);
begin
  FBuchDaten := ABuchDaten;
  LadeBuchDaten(ABuchDaten);
end;

// Buchdaten in die Eingabefelder laden
procedure TBuchform.LadeBuchDaten(const ABuchDaten: TBuchDaten);
begin
  EdtName.Text := ABuchDaten.Name;
  EdtGenre.Text := ABuchDaten.Genre;
  EdtIsbn.Text := ABuchDaten.ISBN;
  EdtAutor.Text := ABuchDaten.Autor;
  EdtHerausgeber.Text := ABuchDaten.Herausgeber;
  EdtAuflage.Text := ABuchDaten.Auflage;
  EdtRegal.Text := ABuchDaten.Regal;
  EdtPlatznummer.Text := ABuchDaten.Platznummer;
  EdtSprache.Text := ABuchDaten.Sprache;
  EdtSeitenanzahl.Text := IntToStr(ABuchDaten.Seitenanzahl);
  EdtPublikationsjahr.Text := IntToStr(ABuchDaten.Publikationsjahr);
end;

{Es kann sinnvoll sein, eine zentrale DataModule-Unit zu verwenden,
 in der der TADOConnection definiert und initialisiert wird.
  So vermeidest du Mehrfachinitialisierungen und hast an einer
  Stelle die komplette Kontrolle über die Datenbankverbindung.}


// Speichern der Buchdaten
procedure TBuchform.SpeichereBuchDaten;
begin


  // 1) Lokale Buchdaten aktualisieren
  FBuchDaten.Name := EdtName.Text;
  FBuchDaten.Genre := EdtGenre.Text;
  FBuchDaten.ISBN := EdtIsbn.Text;
  FBuchDaten.Autor := EdtAutor.Text;
  FBuchDaten.Herausgeber := EdtHerausgeber.Text;
  FBuchDaten.Auflage := EdtAuflage.Text;
  FBuchDaten.Regal := EdtRegal.Text;
  FBuchDaten.Platznummer := EdtPlatznummer.Text;
  FBuchDaten.Sprache := EdtSprache.Text;
  FBuchDaten.Seitenanzahl := StrToIntDef(EdtSeitenanzahl.Text, 0);
  FBuchDaten.Publikationsjahr := StrToIntDef(EdtPublikationsjahr.Text, 0);

  // 2) Query aus der INI laden
  ADOQuery.SQL.Text := LadeSQLQuery('InsertOrUpdate');
  ADOQuery.Parameters.Refresh; // optional, aktualisiert die Param-Liste

  // 3) Parameter mit Werten belegen
  ADOQuery.Parameters.ParamByName('Name').Value := FBuchDaten.Name;
  ADOQuery.Parameters.ParamByName('Genre').Value := FBuchDaten.Genre;
  ADOQuery.Parameters.ParamByName('ISBN').Value := FBuchDaten.ISBN;
  ADOQuery.Parameters.ParamByName('Author').Value := FBuchDaten.Autor;
  ADOQuery.Parameters.ParamByName('Publisher').Value := FBuchDaten.Herausgeber;
  ADOQuery.Parameters.ParamByName('Edition').Value := FBuchDaten.Auflage;
  ADOQuery.Parameters.ParamByName('Shelf').Value := FBuchDaten.Regal;
  ADOQuery.Parameters.ParamByName('Position').Value := FBuchDaten.Platznummer;
  ADOQuery.Parameters.ParamByName('Language').Value := FBuchDaten.Sprache;
  ADOQuery.Parameters.ParamByName('Pages').Value := FBuchDaten.Seitenanzahl;
  ADOQuery.Parameters.ParamByName('PublicationYear').Value := FBuchDaten.Publikationsjahr;

  // 4) SQL ausführen => Fügt ein oder aktualisiert, je nach ISBN
  ADOQuery.ExecSQL;

  ShowMessage('Buchdaten gespeichert!');
end;

// Löschen der Buchdaten
procedure TBuchform.LoescheBuchDaten;
begin
  ADOQuery.SQL.Text := LadeSQLQuery('DeleteBook');
  ADOQuery.Parameters.ParamByName('BookID').Value := FBuchDaten.ID;
  ADOQuery.ExecSQL;

  ShowMessage('Buch wurde gelöscht!');
end;

// Event-Handler für Buttons
procedure TBuchform.btnSpeichernClick(Sender: TObject);
begin
  SpeichereBuchDaten;
end;

procedure TBuchform.btnBearbeitenClick(Sender: TObject);
begin
  ShowMessage('Bearbeiten-Funktion wird später implementiert');
end;

procedure TBuchform.btnLoeschenClick(Sender: TObject);
begin
  LoescheBuchDaten;
end;

end.

