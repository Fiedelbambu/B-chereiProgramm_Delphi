unit AusleiheDialog;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Data.DB,Data.Win.ADODB, Vcl.Grids,
  Vcl.DBGrids, Vcl.WinXCtrls;

type
  TAusleihDialog = class(TFrame)
    lblAusleihformular: TLabel;
    ADOQuery1: TADOQuery;
    SearchBox: TSearchBox;
    DBGrid: TDBGrid;  // Dieses Grid zeigt Kundendaten an
    BtnAusleihen: TButton;
    DataSource1: TDataSource;
    procedure LoadCustomers;
    procedure BtnAusleihenClick(Sender: TObject);
    procedure SearchBoxInvokeSearch(Sender: TObject);
    procedure DataSource1DataChange(Sender: TObject; Field: TField);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    BookName: string;
    procedure SetBookData(const ABookName: string);
    procedure PerformLoan;
  end;

implementation

{$R *.dfm}

uses MainFrame;

procedure TAusleihDialog.SearchBoxInvokeSearch(Sender: TObject);
begin
  LoadCustomers;
end;

procedure TAusleihDialog.SetBookData(const ABookName: string);
begin
  BookName := ABookName;
  lblAusleihformular.Caption := 'Verleihe: ' + ABookName;
end;

procedure TAusleihDialog.DataSource1DataChange(Sender: TObject; Field: TField);
begin
  // Beispiel: Wenn das DBGrid einen neuen Datensatz anzeigt,
  // kannst du etwa den Kundennamen in ein Label schreiben:
  if Assigned(DataSource1.DataSet) and (not DataSource1.DataSet.IsEmpty) then
  begin
    // Angenommen, das Dataset hat ein Feld "first_name":
    lblAusleihformular.Caption := 'Ausgewählter Kunde: ' +
      DataSource1.DataSet.FieldByName('first_name').AsString;
  end;
end;


procedure TAusleihDialog.LoadCustomers;
var
  sSearch: string;
begin
  if not Assigned(Form1) or not Assigned(Form1.DBKonfig) then
  begin
    ShowMessage('DBKonfig oder Form1 nicht verfügbar.');
    Exit;
  end;

  sSearch := Trim(SearchBox.Text);

  ADOQuery1.Close;
  ADOQuery1.Connection := Form1.DBKonfig.ADOConnection;
  ADOQuery1.SQL.Text :=
    'SELECT customer_id, first_name, last_name, email, phone, birth_date, street, postal_code, city ' +
    'FROM customers ' +
    'WHERE (first_name LIKE :SearchText OR last_name LIKE :SearchText)';
  ADOQuery1.Parameters.ParamByName('SearchText').Value := '%' + sSearch + '%';

  try
    ADOQuery1.Open;
  except
    on E: Exception do
      ShowMessage('Fehler beim Laden der Kunden: ' + E.Message);
  end;
end;

procedure TAusleihDialog.PerformLoan;
var
  customerId, bookId: Integer;
  loanDate, dueDate: TDate;
  qry: TADOQuery;
begin
  // 1. Prüfe, ob ein Kunde ausgewählt wurde.
  if (DBGrid.DataSource = nil) or DBGrid.DataSource.DataSet.IsEmpty then
  begin
    ShowMessage('Bitte wählen Sie einen Kunden aus.');
    Exit;
  end;
  customerId := DBGrid.DataSource.DataSet.FieldByName('customer_id').AsInteger;

  // 2. Ermittle die book_id anhand des Buchnamens
  qry := TADOQuery.Create(Self);
  try
    qry.Connection := Form1.DBKonfig.ADOConnection;
    qry.SQL.Text := 'SELECT book_id FROM books WHERE name = :BookName';
    qry.Parameters.ParamByName('BookName').Value := BookName;
    qry.Open;
    if qry.IsEmpty then
    begin
      ShowMessage('Kein Buch mit dem Namen "' + BookName + '" gefunden.');
      Exit;
    end;
    bookId := qry.FieldByName('book_id').AsInteger;
  finally
    qry.Free;
  end;

  // 3. Setze das Ausleihdatum (heute) und das Rückgabedatum (14 Tage später)
  loanDate := Date;
  dueDate := Date + 14;

  // 4. Führe den INSERT in die loans-Tabelle durch
  qry := TADOQuery.Create(Self);
  try
    qry.Connection := Form1.DBKonfig.ADOConnection;
    qry.SQL.Text :=
      'INSERT INTO loans (customer_id, book_id, loan_date, due_date, return_date) ' +
      'VALUES (:CustomerID, :BookID, :LoanDate, :DueDate, NULL)';
    qry.Parameters.ParamByName('CustomerID').Value := customerId;
    qry.Parameters.ParamByName('BookID').Value := bookId;
    qry.Parameters.ParamByName('LoanDate').Value := loanDate;
    qry.Parameters.ParamByName('DueDate').Value := dueDate;
    qry.ExecSQL;
    ShowMessage('Buch wurde erfolgreich verliehen.');
  finally
    qry.Free;
  end;
end;





procedure TAusleihDialog.BtnAusleihenClick(Sender: TObject);
begin
  PerformLoan;
end;

end.

