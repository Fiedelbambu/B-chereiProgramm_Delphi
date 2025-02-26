unit Ausleihform;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, Vcl.Grids,
  Vcl.DBGrids, Vcl.StdCtrls, DatenbankKonfig,
  FireDAC.Comp.Client, LoanRepository, System.Generics.Collections,
  Data.Win.ADODB, Vcl.ButtonStylesAttributes, Vcl.StyledButton;

type
  TAusleiheFormular = class(TFrame)
    lblAusleihformular: TLabel;
    DBGrid1: TDBGrid;
    DataSource1: TDataSource;
    ADOQuery1: TADOQuery;
    Sbtn_Rueckgabe: TStyledButton;
    procedure LoadData;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Sbtn_RueckgabeClick(Sender: TObject);
  private
    FMemTable: TFDMemTable;  // Klassenvariable
    procedure DisplayData(Loans: TList<TLoanData>);
    procedure AdjustGridColumns;
    procedure RückgabeDatumGetText(Sender: TField; var Text: string; DisplayText: Boolean);

  public
  end;

implementation

{$R *.dfm}

uses
  MainFrame;


constructor TAusleiheFormular.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMemTable := TFDMemTable.Create(Self);
  FMemTable.FieldDefs.Clear;

  // Felder für das Grid definieren
  with FMemTable.FieldDefs do
  begin
    Add('AusleihID', ftInteger);    // LoanID
      Add('book_id', ftInteger);      // <-- Dieses Feld hinzufügen

    Add('Kunden Name', ftString, 100);
    Add('Buch Titel', ftString, 150);
    Add('Ausleih Datum', ftDate);
    Add('Rückgabe frist', ftDate);
    Add('Rückgabe Datum', ftDate);
  end;

  FMemTable.CreateDataSet;

  // OnGetText für das Feld "Rückgabe Datum" zuweisen
  if FMemTable.FindField('Rückgabe Datum') <> nil then
    TDateTimeField(FMemTable.FieldByName('Rückgabe Datum')).OnGetText := RückgabeDatumGetText;

  DataSource1.DataSet := FMemTable;  // Datenquelle zuweisen
  DBGrid1.DataSource := DataSource1;

  LoadData;  // Daten laden
end;

destructor TAusleiheFormular.Destroy;
begin
  FMemTable.Free;
  inherited Destroy;
end;

procedure TAusleiheFormular.LoadData;
var
  Loans: TList<TLoanData>;
begin
  Loans := TLoanRepository.GetLoans;
  try
    DisplayData(Loans);
  finally
    Loans.Free;
  end;
end;

procedure TAusleiheFormular.DisplayData(Loans: TList<TLoanData>);
var
  Loan: TLoanData;
begin
  FMemTable.Close;
  FMemTable.Open;
  AdjustGridColumns;

  // Daten einfügen
  for Loan in Loans do
begin
  FMemTable.Append;
 FMemTable.FieldByName('AusleihID').AsInteger := Loan.LoanID;
      FMemTable.FieldByName('book_id').AsInteger := Loan.BookID;
  FMemTable.FieldByName('Kunden Name').AsString := Loan.CustomerName;
  FMemTable.FieldByName('Buch Titel').AsString := Loan.BookTitle;
  FMemTable.FieldByName('Ausleih Datum').AsDateTime := Loan.LoanDate;
  FMemTable.FieldByName('Rückgabe frist').AsDateTime := Loan.DueDate;
  if Loan.ReturnDate = 0 then
    FMemTable.FieldByName('Rückgabe Datum').Clear
  else
    FMemTable.FieldByName('Rückgabe Datum').AsDateTime := Loan.ReturnDate;
  FMemTable.Post;
end;

end;

procedure TAusleiheFormular.AdjustGridColumns;
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

// Definition der OnGetText-Methode
procedure TAusleiheFormular.RückgabeDatumGetText(Sender: TField; var Text: string; DisplayText: Boolean);
begin
  if Sender.IsNull or (Sender.AsDateTime = 0) then
    Text := ''
  else
    Text := FormatDateTime('dd.mm.yyyy', Sender.AsDateTime);
end;

procedure TAusleiheFormular.Sbtn_RueckgabeClick(Sender: TObject);
var
  LoanID, BookID: Integer;  // <-- BookID hier deklarieren
  qry: TADOQuery;
  ReturnDate: TDate;
begin
  // Prüfen, ob ein Datensatz im Grid ausgewählt ist
  if (DBGrid1.DataSource = nil) or DBGrid1.DataSource.DataSet.IsEmpty then
  begin
    ShowMessage('Bitte wählen Sie einen Datensatz aus.');
    Exit;
  end;

  // LoanID aus dem ausgewählten Datensatz ermitteln (hier als AusleihID gespeichert)
  LoanID := DBGrid1.DataSource.DataSet.FieldByName('AusleihID').AsInteger;

  // BookID aus dem ausgewählten Datensatz ermitteln
  BookID := DBGrid1.DataSource.DataSet.FieldByName('book_id').AsInteger;

  // Setze das Rückgabedatum auf heute
  ReturnDate := Date;

  qry := TADOQuery.Create(Self);
  try
    qry.Connection := Form1.DBKonfig.ADOConnection;
    Form1.DBKonfig.ADOConnection.BeginTrans;
    try
      // Aktualisiere den Rückgabedatum-Wert in der Tabelle loans
      qry.SQL.Text :=
        'UPDATE loans ' +
        'SET return_date = :ReturnDate ' +
        'WHERE loan_id = :LoanID';
      qry.Parameters.Clear;
      qry.Parameters.CreateParameter('ReturnDate', ftString, pdInput, 8, FormatDateTime('yyyyMMdd', ReturnDate));
      qry.Parameters.CreateParameter('LoanID', ftInteger, pdInput, 0, LoanID);
      qry.ExecSQL;

      // Setze den Status des Buchs in der Tabelle books auf "nicht verliehen"
      qry.SQL.Text :=
        'UPDATE books SET status = ''nicht verliehen'' WHERE book_id = :BookID';
      qry.Parameters.Clear;
      qry.Parameters.CreateParameter('BookID', ftInteger, pdInput, 0, BookID);
      qry.ExecSQL;

      Form1.DBKonfig.ADOConnection.CommitTrans;
      ShowMessage('Buch erfolgreich zurückgegeben.');
    except
      on E: Exception do
      begin
        Form1.DBKonfig.ADOConnection.RollbackTrans;
        ShowMessage('Fehler bei der Rückgabe: ' + E.Message);
      end;
    end;
  finally
    qry.Free;
  end;

  // Daten neu laden, damit der Grid-Eintrag aktualisiert wird
  LoadData;
end;


end.

