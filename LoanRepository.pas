unit LoanRepository;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, Data.DB,
  Data.Win.ADODB, Vcl.Dialogs, DatenbankKonfig;

type
  TLoanData = record
    LoanID: Integer;
    CustomerID: Integer;
    CustomerName: string;
    BookID: Integer;
    BookTitle: string;
    LoanDate: TDate;
    DueDate: TDate;
    ReturnDate: TDate;
  end;

  TLoanRepository = class
  public
    class function GetLoans: TList<TLoanData>;
    class function IstBuchAusgeliehen(BookID: Integer): Boolean;

  end;

implementation

uses
  MainFrame;  // Hier wird Form1 erkannt


class function TLoanRepository.IstBuchAusgeliehen(BookID: Integer): Boolean;
var
  Query: TADOQuery;
  SQL: string;
begin
  Result := False;
  Query := TADOQuery.Create(nil);
  try
    Query.Connection := Form1.DBKonfig.ADOConnection;
    SQL := 'SELECT COUNT(*) AS Anzahl FROM loans WHERE book_id = :BookID AND return_date IS NULL;';
    Query.SQL.Text := SQL;
    Query.Parameters.ParamByName('BookID').Value := BookID;
    Query.Open;
    Result := Query.FieldByName('Anzahl').AsInteger > 0;
  finally
    Query.Free;
  end;
end;



class function TLoanRepository.GetLoans: TList<TLoanData>;
var
  Query: TADOQuery;
  Loan: TLoanData;
  SQL: string;
begin
  Result := TList<TLoanData>.Create;
  Query := TADOQuery.Create(nil);
  try
    // Verbindung direkt über DBKonfig herstellen
    Query.Connection := Form1.DBKonfig.ADOConnection;

   // ShowMessage('Verbindung zur Datenbank verwendet: ' + Form1.DBKonfig.ADOConnection.ConnectionString);

    // SQL-Abfrage korrekt zusammenbauen
    SQL := 'SELECT L.loan_id, ' +
           '       CAST(C.customer_id AS CHAR(20)) AS customer_id, ' +
           '       CONCAT(C.first_name, '' '', C.last_name) AS CustomerName, ' +
           '       B.book_id, ' +
           '       B.name AS BookTitle, ' +
           '       CAST(L.loan_date AS DATE) AS LoanDate, ' +
           '       CAST(L.due_date AS DATE) AS DueDate, ' +
           '       L.return_date ' +
           'FROM loans L ' +
           'INNER JOIN customers C ON L.customer_id = C.customer_id ' +
           'INNER JOIN books B ON L.book_id = B.book_id;';

    Query.SQL.Text := SQL;
   // ShowMessage(' SQL-Abfrage: ' + SQL);

    // Versuch, die Query zu öffnen
    try
      Query.Open;
     // ShowMessage(' Query erfolgreich geöffnet!');
    except
      on E: Exception do
      begin
        ShowMessage('❌ Fehler beim Öffnen der Query: ' + E.Message);
        Exit;
      end;
    end;

    // Daten in Record-Liste umwandeln
    while not Query.Eof do
    begin
      Loan.LoanID := Query.FieldByName('loan_id').AsInteger;
      Loan.CustomerID := Query.FieldByName('customer_id').AsInteger;
      Loan.CustomerName := Query.FieldByName('CustomerName').AsString;
      Loan.BookID := Query.FieldByName('book_id').AsInteger;
      Loan.BookTitle := Query.FieldByName('BookTitle').AsString;
      Loan.LoanDate := Query.FieldByName('LoanDate').AsDateTime;
      Loan.DueDate := Query.FieldByName('DueDate').AsDateTime;
      Loan.ReturnDate := Query.FieldByName('return_date').AsDateTime;
      Result.Add(Loan);
      Query.Next;
    end;

    ShowMessage('Daten erfolgreich geladen: ' + IntToStr(Result.Count) + ' Einträge.');

  finally
    Query.Free;
  end;
end;

end.

