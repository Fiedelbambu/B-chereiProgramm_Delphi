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
    procedure AdjustGridColumns;
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
    Add('AusleihID', ftInteger);    //LoanID
    Add('Kunden Name', ftString, 100);
    Add('Buch Titel', ftString, 150);
    Add('Ausleih Datum', ftDate);
    Add('Rückgabe frist', ftDate);
    Add('Rückgabe Datum', ftDate);
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
  AdjustGridColumns;

  // Daten einfügen
  for Loan in Loans do
  begin
    FMemTable.Append;
    FMemTable.FieldByName('AusleihID').AsInteger := Loan.LoanID;
    FMemTable.FieldByName('Kunden Name').AsString := Loan.CustomerName;
    FMemTable.FieldByName('Buch Titel').AsString := Loan.BookTitle;
    FMemTable.FieldByName('Ausleih Datum').AsDateTime := Loan.LoanDate;
    FMemTable.FieldByName('Rückgabe frist').AsDateTime := Loan.DueDate;
    FMemTable.FieldByName('Rückgabe Datum').AsDateTime := Loan.ReturnDate;
    FMemTable.Post;
  end;

end;


{ --- Spaltenbreiten anpassen --- }
procedure TAusleiheformular.AdjustGridColumns;
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


end.

