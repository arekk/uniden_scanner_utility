unit UnitMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls, Grids, Menus, ExtCtrls, StdCtrls, StrUtils, SqlDb, LCLIntf,
  Common, UnitScanner, UnitDatabase, UnitDebug, UnitInfo, UnitMemoryBanks;

type

  { TForm1 }

  TForm1 = class(TForm)
    ComboBox1: TComboBox;
    Edit1: TEdit;
    IdleTimer1: TIdleTimer;
    Label1: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    PopupMenu1: TPopupMenu;
    SaveDialog1: TSaveDialog;
    StatusBar1: TStatusBar;
    StringGrid1: TStringGrid;

    procedure Button1Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure IdleTimer1Timer(Sender: TObject);
    procedure MenuItem10Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure MenuItem9Click(Sender: TObject);
    procedure StringGrid1ContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure StringGrid1EditingDone(Sender: TObject);
    procedure StringGrid1PickListSelect(Sender: TObject);
    procedure StringGrid1PrepareCanvas(sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
    procedure StringGrid1SelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);

    procedure MoveToBank(Sender: TObject);
    procedure RefreshBankNames(Sender: TObject; var CloseAction: TCloseAction);
    procedure DownloadScannerMemory;
    procedure PopulateGrid;
    procedure PopulateGridRow(row: Longint; memory_row: TScannerMemoryRow);
    procedure SearchForScanners;
    procedure PopulateBanksCombo;

    function FindFirstFreeSlotInBank(bank: Integer):Integer;

  private
    { private declarations }
    EditingCol, EditingRow: Longint;
    ScannerMemory: TScannerMemory;
    SnapShotId: Integer;

  public
    { public declarations }
  end;

var
  Form1: TForm1;

const
  COL_BANK = 0;
  COL_BANK_NAME = 1;
  COL_SLOT = 2;
  COL_FRQ = 3;
  COL_MODUL = 4;
  COL_DLY = 5;
  COL_LO = 6;
  COL_DESC = 7;
  COL_SEEN = 8;

implementation



{$R *.lfm}

{ begin od custom procedures and functions }

procedure TForm1.PopulateBanksCombo;
var
  bank_name: String;
  i: Integer;
begin
  ComboBox1.Items.Clear;
  ComboBox1.Items.Add('All');
  for i:=1 to Scanner.NumberOfBanks do begin
    bank_name :=FormDatabase.BankNameLookup(Scanner.ModelAliased, i);
    bank_name :=IfThen(bank_name <> '', ' - ' + bank_name, '');
    ComboBox1.Items.Add(IntToStr(i) + bank_name);
  end;
  ComboBox1.ItemIndex:=0;
end;

procedure TForm1.SearchForScanners;
begin
  if FindAliveScanner = true then begin
    DownloadScannerMemory;
    PopulateGrid;
    PopulateBanksCombo;
    StatusBar1.Panels[0].Text:=IfThen(Scanner.Model <> Scanner.ModelAliased, Scanner.Model + ' of ' + Scanner.ModelAliased, Scanner.Model);
  end;

  if (Scanner <> Nil) and (Scanner.ReadOnly = false) then begin
    MenuItem2.Enabled:= true;
    MenuItem3.Enabled:= true;
    MenuItem4.Enabled:= true;
    MenuItem5.Enabled:= true;
  end else begin
    MenuItem2.Enabled:= false;
    MenuItem3.Enabled:= false;
    MenuItem4.Enabled:= False;
    MenuItem5.Enabled:= False;
  end;
end;

procedure TForm1.DownloadScannerMemory;
var
  i: Integer;
  mr: TScannerMemoryRow;
begin
  StatusBar1.Panels[1].Text:='Receiving scanner data, this can take a while, please wait...';
  StatusBar1.Repaint;

  ScannerMemory:=Scanner.GetMemorySlots;

  StatusBar1.Panels[1].Text:='Updating database please wait...';
  StatusBar1.Repaint;
  FormDebug.Debug('Updating database');

  // Jesli skaner jest urzadzeniem fizycznym, aktualizujemy snapshot w bazie
  FormDatabase.SQLTransAction1.StartTransaction;

  if Scanner.ReadOnly = false then begin
    SnapShotId:=FormDatabase.GetAutoMemorySnapshot(Scanner.Model);
    FormDebug.Debug('Snapshot: ' + IntToStr(SnapShotId));

    for i:=1 to Length(ScannerMemory) do begin
      mr:=ScannerMemory[i];

      FormDatabase.MemSnapshotSlotUpdate(SnapShotId, mr.Bank, mr.Slot, mr.Frq, mr.Modul, mr.Dly, mr.Lo);
      // Skaner wspiera opisy, zostaly one zaladowane z pamieci skanera, tak wiec aktualizuje te, ktore sa w bazie
      if Scanner.SupportsDescription then begin
        FormDatabase.FreqDescriptionUpdate(mr.Frq, mr.FrqDescription);
      end;

      FormDatabase.ConfigSet('last_used_scanner_m', Scanner.Model);
      FormDatabase.ConfigSet('last_used_scanner_s', IntToStr(Scanner.NumberOfSlots));
      FormDatabase.ConfigSet('last_used_scanner_b', IntToStr(Scanner.NumberOfBanks));
      FormDatabase.ConfigSet('last_used_scanner_d', BoolToStr(Scanner.SupportsDescription));
    end;
  end;

  FormDatabase.SQLTransAction1.Commit;

  StatusBar1.Panels[1].Text:='Idle';
  StatusBar1.Repaint;
end;

procedure TForm1.PopulateGrid;
var
  i,n: Integer;
  memory_row: TScannerMemoryRow;
begin
  n:=1;
  StringGrid1.BeginUpdate;
  StringGrid1.Clear;
  StringGrid1.RowCount:=1;

  StringGrid1.Columns[COL_FRQ].ReadOnly:=Scanner.ReadOnly;
  StringGrid1.Columns[COL_MODUL].ReadOnly:=Scanner.ReadOnly;
  StringGrid1.Columns[COL_DLY].ReadOnly:=Scanner.ReadOnly;
  StringGrid1.Columns[COL_LO].ReadOnly:=Scanner.ReadOnly;
  StringGrid1.Columns[COL_DESC].ReadOnly:= ((Scanner.ReadOnly = true) and (StrToBool(FormDatabase.ConfigGet('last_used_scanner_d')) = true));

  for i:=1 to Length(ScannerMemory) do begin
    memory_row:=ScannerMemory[i];
    if (memory_row.Valid) and
       ((Edit1.Text = '') or (memory_row.Contains(Edit1.Text))) and
       ((ComboBox1.ItemIndex = 0) or (memory_row.Bank = ComboBox1.ItemIndex))
    then begin
      StringGrid1.RowCount:=StringGrid1.RowCount+1;
      PopulateGridRow(n, memory_row);
      n:=n+1;
    end;
  end;
  StringGrid1.EndUpdate;
end;

procedure TForm1.PopulateGridRow(row: Longint; memory_row: TScannerMemoryRow);
begin
  StringGrid1.Cells[COL_BANK,      row]:= IntToStr(memory_row.Bank);
  StringGrid1.Cells[COL_BANK_NAME, row]:= memory_row.BankName;
  StringGrid1.Cells[COL_SLOT,      row]:= IntToStr(memory_row.Slot);
  if memory_row.Frq = 0 then begin
    StringGrid1.Cells[COL_FRQ,     row]:= '';
  end else begin
    StringGrid1.Cells[COL_FRQ,     row]:= FloatToFrq(memory_row.Frq);
  end;
  StringGrid1.Cells[COL_MODUL,     row]:= memory_row.Modul;
  StringGrid1.Cells[COL_DLY,       row]:= IntToStr(memory_row.Dly);
  StringGrid1.Cells[COL_LO,        row]:= IntToStr(memory_row.Lo);
  StringGrid1.Cells[COL_DESC,      row]:= memory_row.FrqDescription;
  StringGrid1.Cells[COL_SEEN,      row]:= IfThen(memory_row.SeenCount > 0, IntToStr(memory_row.SeenCount), '');
end;

procedure TForm1.MoveToBank(Sender: TObject);
var
  menu_item: TMenuItem;
  dst_bank,dst_slot_pos,src_slot_pos,i: Integer;
begin
  if (EditingRow > 0) and (EditingRow < StringGrid1.RowCount) then begin
    menu_item:=TMenuItem(sender);
    dst_bank:=menu_item.Tag;

    dst_slot_pos:=FindPosOfSlotInScannerMemory(ScannerMemory, FindFirstFreeSlotInBank(dst_bank));
    src_slot_pos:=FindPosOfSlotInScannerMemory(ScannerMemory, StrToInt(StringGrid1.Cells[COL_SLOT, EditingRow]));

    ScannerMemory[dst_slot_pos].UpdateData(ScannerMemory[src_slot_pos]);
    ScannerMemory[src_slot_pos].Clear;

    Scanner.UpdateSlot(ScannerMemory[dst_slot_pos]);
    Scanner.UpdateSlot(ScannerMemory[src_slot_pos]);

    if SnapShotId > 0 then begin
       FormDatabase.MemSnapshotSlotUpdate(
         SnapShotId,
         ScannerMemory[dst_slot_pos].Bank,
         ScannerMemory[dst_slot_pos].Slot,
         ScannerMemory[dst_slot_pos].Frq,
         ScannerMemory[dst_slot_pos].Modul,
         ScannerMemory[dst_slot_pos].Dly,
         ScannerMemory[dst_slot_pos].Lo
       );
       FormDatabase.MemSnapshotSlotUpdate(
         SnapShotId,
         ScannerMemory[src_slot_pos].Bank,
         ScannerMemory[src_slot_pos].Slot,
         ScannerMemory[src_slot_pos].Frq,
         ScannerMemory[src_slot_pos].Modul,
         ScannerMemory[src_slot_pos].Dly,
         ScannerMemory[src_slot_pos].Lo
       );
    end;

    PopulateGridRow(EditingRow, ScannerMemory[src_slot_pos]);
    for i:=1 to StringGrid1.RowCount-1 do begin
      if StrToInt(StringGrid1.Cells[COL_SLOT, i]) = ScannerMemory[dst_slot_pos].Slot then begin
        PopulateGridRow(i, ScannerMemory[dst_slot_pos]);
      end;
    end;
  end;
end;

procedure TForm1.RefreshBankNames(Sender: TObject; var CloseAction: TCloseAction);
var
  i: Integer;
begin
  for i:=1 to Length(ScannerMemory) do begin
    ScannerMemory[i].SetExtraData;
  end;
  PopulateBanksCombo;
  PopulateGrid;
end;

function TForm1.FindFirstFreeSlotInBank(bank: Integer):Integer;
var
  i: Integer;
  memory_row: TScannerMemoryRow;
begin
  Result:=0;
  for i:=1 to Length(ScannerMemory) do begin
    memory_row:=ScannerMemory[i];
    if (memory_row.Bank = bank) and (memory_row.isFree = true) then begin
      Result:=memory_row.Slot;
      Break;
    end;
  end;
end;

{ end od custom procedures and functions }

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FormDebug:=TFormDebug.Create(Application);
  FormDebug.Debug(GetSerialPortsInfo);
  ScannerInit;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
   StatusBar1.Panels[1].Text:='Please wait, searching for devices...';
end;

procedure TForm1.IdleTimer1Timer(Sender: TObject);
begin
  SearchForScanners;
end;

procedure TForm1.MenuItem10Click(Sender: TObject);
begin
  FormDebug.Show;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
   FormDebug.Close;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  PopulateGrid;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  FormDebug.Debug('ComboBox1: ' + IntToStr(ComboBox1.ItemIndex));
  PopulateGrid;
end;

procedure TForm1.Edit1Change(Sender: TObject);
begin
  PopulateGrid;
end;

procedure TForm1.StringGrid1EditingDone(Sender: TObject);
var
  memory_pos: Integer;
begin
  FormDebug.Debug('EditingDone: row:' + IntToStr(EditingRow) + ', col: ' + IntToStr(EditingCol));
  if (EditingRow > 0) and
     (EditingRow < StringGrid1.RowCount) and
     ((EditingCol = COL_DESC) or (EditingCol = COL_FRQ))
  then begin
    memory_pos:=FindPosOfSlotInScannerMemory(ScannerMemory, StrToInt(StringGrid1.Cells[COL_SLOT, EditingRow]));
    FormDebug.Debug('Memory pos: ' + IntToStr(memory_pos));

    if memory_pos >= 0 then begin
      FormDebug.Debug('New: ' + StringGrid1.Cells[COL_DESC,EditingRow] + ' old: ' + ScannerMemory[memory_pos].FrqDescription);

      if (EditingCol = COL_DESC) and
         (StringGrid1.Cells[COL_DESC,EditingRow] <> ScannerMemory[memory_pos].FrqDescription)
      then begin
        FormDatabase.FreqDescriptionUpdate(
          ScannerMemory[memory_pos].Frq,
          StringGrid1.Cells[COL_DESC,EditingRow]
        );
        ScannerMemory[memory_pos].FrqDescription:=StringGrid1.Cells[COL_DESC,EditingRow];
        if Scanner.SupportsDescription = true then begin
          Scanner.UpdateSlot(ScannerMemory[memory_pos]);
        end;
        FormDebug.Debug('EditingDone: updated DESC');
      end;

      if (EditingCol = COL_FRQ) and
         (FrqToFloat(StringGrid1.Cells[COL_FRQ, EditingRow]) <> ScannerMemory[memory_pos].Frq)
      then begin
        ScannerMemory[memory_pos].UpdateData(
          StringGrid1.Cells[COL_FRQ,  EditingRow],
          StringGrid1.Cells[COL_MODUL,EditingRow],
          StringGrid1.Cells[COL_DLY,  EditingRow],
          StringGrid1.Cells[COL_LO,   EditingRow]
        );
        Scanner.UpdateSlot(ScannerMemory[memory_pos]);
        if SnapShotId > 0 then begin
          FormDatabase.MemSnapshotSlotUpdate(
            SnapShotId,
            ScannerMemory[memory_pos].Bank,
            ScannerMemory[memory_pos].Slot,
            ScannerMemory[memory_pos].Frq,
            ScannerMemory[memory_pos].Modul,
            ScannerMemory[memory_pos].Dly,
            ScannerMemory[memory_pos].Lo
            );
        end;
        FormDebug.Debug('EditingDone: updated FRQ');
      end;

      PopulateGridRow(EditingRow, ScannerMemory[memory_pos]);
    end;
  end;
end;

procedure TForm1.StringGrid1PickListSelect(Sender: TObject);
var
  memory_pos: Integer;
begin
  if (EditingRow > 0) and
     (EditingRow < StringGrid1.RowCount) and
     ((EditingCol = COL_MODUL) or (EditingCol = COL_DLY) or (EditingCol = COL_LO))
  then begin
    memory_pos:=FindPosOfSlotInScannerMemory(ScannerMemory, StrToInt(StringGrid1.Cells[COL_SLOT, EditingRow]));
    if memory_pos >= 0 then begin
      if ((EditingCol = COL_MODUL) and (StringGrid1.Cells[COL_MODUL,EditingRow] <> ScannerMemory[memory_pos].Modul)) or
         ((EditingCol = COL_DLY) and (StrToInt(StringGrid1.Cells[COL_DLY,EditingRow]) <> ScannerMemory[memory_pos].Dly)) or
         ((EditingCol = COL_LO) and (StrToInt(StringGrid1.Cells[COL_LO,EditingRow]) <> ScannerMemory[memory_pos].Lo))
      then begin
        ScannerMemory[memory_pos].UpdateData(
          StringGrid1.Cells[COL_FRQ,   EditingRow],
          StringGrid1.Cells[COL_MODUL, EditingRow],
          StringGrid1.Cells[COL_DLY,   EditingRow],
          StringGrid1.Cells[COL_LO,    EditingRow]
        );
        Scanner.UpdateSlot(ScannerMemory[memory_pos]);
        if SnapShotId > 0 then begin
          FormDatabase.MemSnapshotSlotUpdate(
            SnapShotId,
            ScannerMemory[memory_pos].Bank,
            ScannerMemory[memory_pos].Slot,
            ScannerMemory[memory_pos].Frq,
            ScannerMemory[memory_pos].Modul,
            ScannerMemory[memory_pos].Dly,
            ScannerMemory[memory_pos].Lo
          );
        end;
        PopulateGridRow(EditingRow, ScannerMemory[memory_pos]);
      end;
    end;
  end;
end;

procedure TForm1.StringGrid1PrepareCanvas(sender: TObject; aCol, aRow: Integer;
  aState: TGridDrawState);
begin
  if not (gdfixed in aState) then begin
    if (StringGrid1.Cells[COL_LO, aRow] = '1') and (StringGrid1.Cells[COL_FRQ, aRow] <> '') then begin
      StringGrid1.Canvas.Brush.Color := clGrayText;
      Exit;
    end;
    if StringGrid1.Cells[COL_SEEN, aRow] <> '' then begin
      StringGrid1.Canvas.Brush.Color := clSkyBlue;
    end;
  end;
end;

procedure TForm1.StringGrid1SelectCell(Sender: TObject; aCol, aRow: Integer;
  var CanSelect: Boolean);
begin
  EditingCol := ACol;
  EditingRow := ARow;
end;

procedure TForm1.MenuItem2Click(Sender: TObject);
begin
  DownloadScannerMemory;
  PopulateGrid;
end;

procedure TForm1.MenuItem3Click(Sender: TObject);
begin
  Form2.OnClose:=@RefreshBankNames;
  Form2.Show;
end;

procedure TForm1.MenuItem5Click(Sender: TObject);
var
  memory_pos: Integer;
begin
  if EditingRow > 0 then begin
    memory_pos:=FindPosOfSlotInScannerMemory(ScannerMemory, StrToInt(StringGrid1.Cells[COL_SLOT, EditingRow]));
    ScannerMemory[memory_pos].Clear;
    Scanner.UpdateSlot(ScannerMemory[memory_pos]);
    if SnapShotId > 0 then begin
      FormDatabase.MemSnapshotSlotUpdate(
        SnapShotId,
        ScannerMemory[memory_pos].Bank,
        ScannerMemory[memory_pos].Slot,
        ScannerMemory[memory_pos].Frq,
        ScannerMemory[memory_pos].Modul,
        ScannerMemory[memory_pos].Dly,
        ScannerMemory[memory_pos].Lo
      );
    end;
    PopulateGridRow(EditingRow, ScannerMemory[memory_pos]);
  end;
end;

procedure TForm1.MenuItem6Click(Sender: TObject);
begin
  Form3.ShowModal;
end;

procedure TForm1.MenuItem7Click(Sender: TObject);
var
  memory_pos: Integer;
begin
  if EditingRow > 0 then begin
    memory_pos:=FindPosOfSlotInScannerMemory(ScannerMemory, StrToInt(StringGrid1.Cells[COL_SLOT, EditingRow]));
    FormDatabase.FreqMarkAsHeard(ScannerMemory[memory_pos].Frq);
    ScannerMemory[memory_pos].SetExtraData;
    PopulateGridRow(EditingRow, ScannerMemory[memory_pos]);
  end;
end;

procedure TForm1.MenuItem9Click(Sender: TObject);
var
  memory_pos: Integer;
begin
  if EditingRow > 0 then begin
    memory_pos:=FindPosOfSlotInScannerMemory(ScannerMemory, StrToInt(StringGrid1.Cells[COL_SLOT, EditingRow]));
    if ScannerMemory[memory_pos].Frq > 0 then begin
      OpenURL('http://kosu77.ugu.pl/index.php?q=' + FloatToStr(ScannerMemory[memory_pos].Frq));
    end;
  end;
end;

procedure TForm1.StringGrid1ContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
var
  cell : TPoint;
  n, memory_pos: Integer;
  i: TMenuItem;
  b_name: String;
begin
  cell := StringGrid1.MouseToCell(MousePos);
  StringGrid1.Row:=cell.Y;
  EditingCol := cell.X;
  EditingRow := cell.Y;
  if EditingRow > 0 then begin
    memory_pos:=FindPosOfSlotInScannerMemory(ScannerMemory, StrToInt(StringGrid1.Cells[COL_SLOT, EditingRow]));
    if (ScannerMemory[memory_pos].Frq <> 0) and (Scanner.ReadOnly = false) then begin
      MenuItem4.Enabled:=true;
      MenuItem5.Enabled:=true;

      MenuItem4.Clear;
      for n:=1 to Scanner.NumberOfBanks do begin
        b_name:=FormDatabase.BankNameLookup(Scanner.ModelAliased, n);
        i:=TMenuItem.Create(MenuItem4);
        i.Caption:=IfThen(b_name <> '', IntToStr(n) + ': ' + b_name, IntToStr(n));
        i.Tag:=n;
        i.OnClick:=@MoveToBank;
        if FindFirstFreeSlotInBank(n) < 1 then begin
           i.Enabled:=false;
        end;
        MenuItem4.Add(i);
      end;
    end else begin
      MenuItem4.Enabled:=false;
      MenuItem5.Enabled:=false;
    end;
    if ScannerMemory[memory_pos].Frq <> 0 then begin
      MenuItem7.Enabled:=true;
      MenuItem8.Enabled:=true;
    end else begin
      MenuItem7.Enabled:=false;
      MenuItem8.Enabled:=false;
    end;
  end;
end;

end.

