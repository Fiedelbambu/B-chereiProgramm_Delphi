unit Ausleihform;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, Vcl.Grids,
  Vcl.DBGrids, Vcl.StdCtrls, DatenbankKonfig,
  FireDAC.Comp.Client, LoanRepository, System.Generics.Collections,
  Data.Win.ADODB;

type
  TAusleiheFormular = class(TFrame)
    lblAusleihformular: TLabel;
    DBGrid1: TDBGrid;
    DataSource1: TDataSource;
    ADOQuery1: TADOQuery;
    procedure LoadData;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  private
    FMemTable: TFDMemTable;  // Klassenvariable
    procedure DisplayData(Loans: TList<TLoanData>);
  public
  end;

implementation

{$R *.dfm}

uses
  MainFrame;  // Hier wird Form1 erkannt

constructor TAusleiheFormular.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMemTable := TFDMemTable.Create(Self);
  FMemTable.FieldDefs.Clear;

  // Felder für das Grid definieren
  with FMemTable.FieldDefs do
  begin
    Add('LoanID', ftInteger);
    Add('CustomerName', ftString, 100);
    Add('BookTitle', ftString, 150);
    Add('LoanDate', ftDate);
    Add('DueDate', ftDate);
    Add('ReturnDate', ftDate);
  end;

  FMemTable.CreateDataSet;
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

  // Daten einfügen
  for Loan in Loans do
  begin
    FMemTable.Append;
    FMemTable.FieldByName('LoanID').AsInteger := Loan.LoanID;
    FMemTable.FieldByName('CustomerName').AsString := Loan.CustomerName;
    FMemTable.FieldByName('BookTitle').AsString := Loan.BookTitle;
    FMemTable.FieldByName('LoanDate').AsDateTime := Loan.LoanDate;
    FMemTable.FieldByName('DueDate').AsDateTime := Loan.DueDate;
    FMemTable.FieldByName('ReturnDate').AsDateTime := Loan.ReturnDate;
    FMemTable.Post;
  end;

  ShowMessage('📊 Daten erfolgreich im Grid angezeigt.');
end;

end.

