program BuechereiVerwaltung;

uses
  Vcl.Forms,
  MainFrame in 'MainFrame.pas' {Form1},
  Kundenform in 'Kundenform.pas' {Frame2: TFrame},
  Kundenverwaltung in 'Kundenverwaltung.pas' {Frame1: TFrame},
  Main in 'Main.pas' {Dashboard: TFrame},
  Buchverwaltung in 'Buchverwaltung.pas' {Buchverwalter: TFrame},
  Buchformular in 'Buchformular.pas' {Buchform: TFrame},
  Ausleihform in 'Ausleihform.pas' {AusleiheFormular: TFrame},
  Mahnungsverwaltung in 'Mahnungsverwaltung.pas' {Mahungsverwalt: TFrame},
  Konfiguration in 'Konfiguration.pas' {Konfig: TFrame},
  DatenbankKonfig in 'DatenbankKonfig.pas' {Konfig_Datenbank: TFrame},
  MailKonfig in 'MailKonfig.pas' {Konfig_Mail: TFrame},
  Mahnungsformular in 'Mahnungsformular.pas' {Frame3: TFrame},
  AusleiheDialog in 'AusleiheDialog.pas' {AusleihDialog: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  //  Application.CreateForm(TAusleiheDialogForm, AusleiheDialogForm);
  Application.Run;
end.
