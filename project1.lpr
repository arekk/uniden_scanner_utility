program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  fcllaz,
  UnitMain,
  UnitMemoryBanks,
  UnitInfo,
  UnitDebug,
  UnitDatabase,
  UBC00Database,
  UBC125XLT,
  UBC75XLT,
  UBCGeneric1;

{$R *.res}

begin
  Application.Title:='Uniden';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TFormDatabase, FormDatabase);
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.

