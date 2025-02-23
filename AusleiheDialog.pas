unit AusleiheDialog;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Data.DB, Data.Win.ADODB,
  Vcl.Grids, Vcl.DBGrids, Vcl.WinXCtrls, Vcl.ComCtrls;

type
  TAusleihDialog = class(TFrame)
    lblAusleihformular: TLabel;
    ADOQuery1: TADOQuery;
    SearchBox: TSearchBox;
    DBGrid: TDBGrid;  // Dieses Grid zeigt Kundendaten an
    BtnAusleihen: TButton;
    DataSource1: TDataSource;
    lblKundenName: TLabel;
    lblfuer: TLabel;
    DateTimePicker1: TDateTimePicker;
    procedure SearchBoxInvokeSearch(Sender: TObject);
    procedure BtnAusleihenClick(Sender: TObject);
    procedure DataSource1DataChange(Sender: TObject; Field: TField);
    procedure LoadCustomers;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    BookName: string;
    procedure SetBookData(const ABookName: string);
    procedure SearchCustomer;
    procedure PerformLoan;
  end;

implementation

{$R *.dfm}

uses MainFrame;

{ SetBookData: Übernimmt den übergebenen Buchnamen und aktualisiert das Label }
procedure TAusleihDialog.SetBookData(const ABookName: string);
begin
  BookName := ABookName;
  lblAusleihformular.Caption := 'Verleihe: ' + ABookName;
end;

{ SearchCustomer: Führt eine Suche in der Kundentabelle anhand des Suchbegriffs durch }
procedure TAusleihDialog.SearchCustomer;
var
  sSearch: string;
  FoundCount: Integer;
begin
  sSearch := Trim(SearchBox.Text);
  if sSearch = '' then
  begin
    ShowMessage('Bitte geben Sie einen Suchbegriff ein.');
    Exit;
  end;

  ADOQuery1.Close;
  ADOQuery1.Connection := Form1.DBKonfig.ADOConnection; // Form1.DBKonfig muss verfügbar sein
  ADOQuery1.SQL.Text :=
    'SELECT customer_id, first_name, last_name, email, phone, birth_date, street, postal_code, city ' +
    'FROM customers ' +
    'WHERE customer_id LIKE :search ' +
    '   OR first_name LIKE :search ' +
    '   OR last_name LIKE :search';
  ADOQuery1.Parameters.ParamByName('search').Value := '%' + sSearch + '%';

    try
    ADOQuery1.Open;
    FoundCount := ADOQuery1.RecordCount;
    ShowMessage('Gefundene Kunden: ' + IntToStr(FoundCount));
  except
    on E: Exception do
      ShowMessage('Fehler bei der Suche: ' + E.Message);
  end;
end;

{ LoadCustomers: Alternative Version der Kundensuche, hier verwenden wir andere SQL-Bedingungen }
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

{ PerformLoan: Führt den Ausleihvorgang aus, indem der aktuell ausgewählte Kunde und das Buch zusammengeführt werden }
procedure TAusleihDialog.PerformLoan;
var
  customerId, bookId: Integer;
  loanDate, dueDate: TDate;
  qry: TADOQuery;
begin
  // Prüfe, ob ein Kunde ausgewählt wurde (DBGrid zeigt Kundendaten an)
  if (DBGrid.DataSource = nil) or DBGrid.DataSource.DataSet.IsEmpty then
  begin
    ShowMessage('Bitte wählen Sie einen Kunden aus.');
    Exit;
  end;
  customerId := DBGrid.DataSource.DataSet.FieldByName('customer_id').AsInteger;

  // Ermittle die book_id anhand des Buchnamens (BookName wurde via SetBookData gesetzt)
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

  // Setze das Ausleihdatum (heute) und das Rückgabedatum (14 Tage später)
  loanDate := Date;
  dueDate := Date + 14;

  // Führe den INSERT in die loans-Tabelle durch
  qry := TADOQuery.Create(Self);
  try
    qry.Connection := Form1.DBKonfig.ADOConnection;
    qry.SQL.Text :=
      'INSERT INTO loans (customer_id, book_id, loan_date, due_date, return_date) ' +
      'VALUES (:CustomerID, :BookID, :LoanDate, :DueDate, NULL)';
    qry.Parameters.ParamByName('CustomerID').Value := customerId;
    qry.Parameters.ParamByName('BookID').Value := bookId;
   // qry.Parameters.ParamByName('LoanDate').Value := loanDate;
    qry.Parameters.ParamByName('LoanDate').Value := FormatDateTime('yyyy-mm-dd', Now);
    qry.Parameters.ParamByName('DueDate').Value := dueDate;
    qry.ExecSQL;
    ShowMessage('Buch wurde erfolgreich verliehen.');
  finally
    qry.Free;
  end;
end;

{ Event-Handler: Wenn in der SearchBox eine Suche ausgelöst wird }
procedure TAusleihDialog.SearchBoxInvokeSearch(Sender: TObject);
begin
  SearchCustomer;
end;

{ Event-Handler: Wenn der Button "Ausleihen" geklickt wird }
procedure TAusleihDialog.BtnAusleihenClick(Sender: TObject);
begin
  PerformLoan;
end;

{ Event-Handler: Wenn sich die Daten im DataSource ändern (z. B. beim Wechsel des Datensatzes im DBGrid) }
procedure TAusleihDialog.DataSource1DataChange(Sender: TObject; Field: TField);
var
  firstName, lastName: string;
begin
  if Assigned(DataSource1.DataSet) and (not DataSource1.DataSet.IsEmpty) then
  begin
    firstName := DataSource1.DataSet.FieldByName('first_name').AsString;
    lastName := DataSource1.DataSet.FieldByName('last_name').AsString;
    lblKundenname.Caption := firstName + ' ' + lastName;
  end;
end;



end.

