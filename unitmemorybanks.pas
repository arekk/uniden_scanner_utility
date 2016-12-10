unit UnitMemoryBanks;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  UnitDatabase, UnitScanner;

type

  { TForm2 }

  TForm2 = class(TForm)
    StringGrid1: TStringGrid;
    procedure FormShow(Sender: TObject);
    procedure StringGrid1EditingDone(Sender: TObject);
    procedure StringGrid1SelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
  private
    { private declarations }
    EditingCol, EditingRow: Longint;
  public
    { public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.lfm}

{ TForm2 }


procedure TForm2.FormShow(Sender: TObject);
var
  i: Integer;
begin
  StringGrid1.BeginUpdate;
  StringGrid1.Clear;
  StringGrid1.RowCount:=1;
  StringGrid1.Cells[0,0]:= 'Bank';
  StringGrid1.Cells[1,0]:= 'Name';
  for i:=1 to Scanner.NumberOfBanks do begin
    StringGrid1.RowCount:=StringGrid1.RowCount+1;
    StringGrid1.Refresh;
    StringGrid1.Cells[0,i]:= IntToStr(i);
    StringGrid1.Cells[1,i]:= FormDatabase.BankNameLookup(Scanner.Model, i);
  end;
  StringGrid1.EndUpdate;
end;

procedure TForm2.StringGrid1EditingDone(Sender: TObject);
begin
  FormDatabase.BankNameUpdate(Scanner.Model, StrToInt(StringGrid1.Cells[0,EditingRow]), StringGrid1.Cells[1,EditingRow]);

end;

procedure TForm2.StringGrid1SelectCell(Sender: TObject; aCol, aRow: Integer;
  var CanSelect: Boolean);
begin
  EditingCol := ACol;
  EditingRow := ARow;
end;

end.

