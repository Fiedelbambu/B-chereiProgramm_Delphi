unit AusleiheDialog;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TAusleihDialog = class(TFrame)
    lblAusleihformular: TLabel;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    BookName: string;
    procedure SetBookData(const ABookName: string);
  end;

implementation

{$R *.dfm}



 procedure TAusleihDialog.SetBookData(const ABookName: string);
begin
  BookName := ABookName;
  lblAusleihformular.Caption := 'Verleihe: ' + ABookName;
end;




end.
