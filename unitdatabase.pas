unit UnitDatabase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Sqlite3Conn, SqlDb, FileUtil, Forms, Controls, Graphics,
  Dialogs, StrUtils, UnitDebug;

type
  { TFormDatabase }

  TFormDatabase = class(TForm)
    SQLite3Connection1: TSQLite3Connection;
    SQLQuery1: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    function FrqDescriptionLookup(frq: Real; empty_string_if_not_found: Boolean = true):String;
    function BankNameLookup(scanner: String; bank: Integer):String;
    function FreqHeardLookup(frq: Real):TStringList;
    function CreateScannerMemorySnapshot(scanner: String; auto: Boolean = false): Integer;
    function MemCurrentSlotsGet(snapshot_id: Integer):TStringList;
    function ScannerModelFromSnapshot(snapshot_id: Integer):String;
    function GetAutoMemorySnapshot(scanner: String): Integer;

    function CheckForTableExists(table_name: String): Boolean;
    function CheckForColumnExists(table_name, column_name: String): Boolean;

    procedure ConfigSet(opt, value: String);
    function ConfigGet(opt: String): String;

    procedure FreqDescriptionUpdate(frq: Real; description: String);
    procedure BankNameUpdate(scanner: String; bank: Integer; nme: String);
    procedure FreqMarkAsHeard(frq: Real);
    procedure MemSnapshotSlotUpdate(snapshot_id, bank, slot: Integer; frq: Real; modul: String; dly, lo: Integer);
  end;

var
  FormDatabase: TFormDatabase;

const
  NOT_FOUND = '<not-found>';
  DB_DELIM  = '^';

implementation

{$R *.lfm}

procedure TFormDatabase.FormCreate(Sender: TObject);
var
  loc_num: Integer;
begin
  {$ifdef WIN32}
  SQLiteLibraryName:='./sqlite3_32.dll';
  {$else}
  SQLiteLibraryName:='./sqlite3_64.dll';
  {$endif}

  SQLite3Connection1.ExecuteDirect('PRAGMA journal_mode = WAL;');
  SQLite3Connection1.ExecuteDirect('PRAGMA synchronous = NORMAL;');

  SQLTransAction1.Active:=true;

  SQLTransAction1.StartTransaction;
  with SQLQuery1 do begin
    SQL.Text:='SELECT * FROM my_loc WHERE id = 1';
    Open;
    loc_num:= RecordCount;
    Close;
    if loc_num = 0 then begin
      SQL.Text:='INSERT INTO my_loc (id, name) VALUES (1, "Home")';
      ExecSQL;
      Close;
    end;
    if (CheckForTableExists('my_config') = false) then begin
      SQL.Text:='CREATE TABLE [my_config] ([var] VARCHAR, [val] VARCHAR, CONSTRAINT [] PRIMARY KEY ([var])) WITHOUT ROWID; end;';
      ExecSQL;
      Close;
    end;
    if (CheckForColumnExists('my_mem_snapshots', 'lo') = false) then begin
      SQL.Text:='ALTER TABLE my_mem_snapshots ADD COLUMN lo SMALLINT DEFAULT 0';
      ExecSQL;
      Close;
    end;
  end;
  SQLTransAction1.Commit;
end;

procedure TFormDatabase.FreqDescriptionUpdate(frq: Real; description: String);
var
  current: String;
begin
  if frq <= 0 then begin
    Exit;
  end;
  current:=FrqDescriptionLookup(frq, false);
  with SQLQuery1 do begin
    if current = NOT_FOUND then begin
      SQL.Text:= 'INSERT INTO my_frq_aliases (my_loc_id, frq, description) VALUES (1, :frq, :description)';
    end else begin
      SQL.Text:= 'UPDATE my_frq_aliases SET description = :description WHERE  frq = :frq AND my_loc_id = 1';
    end;
    Params.paramByName('frq').AsFloat:= frq;
    Params.paramByName('description').AsString:= Trim(description);
    ExecSQL;
    Close;
    SQLTransAction1.Commit;
  end;
end;

procedure TFormDatabase.FreqMarkAsHeard(frq: Real);
begin
  if frq <= 0 then begin
    Exit;
  end;
  with SQLQuery1 do begin
    SQL.Text:= 'UPDATE my_frq_aliases SET seen = (seen+1), seen_date = datetime() WHERE  frq = :frq AND my_loc_id = 1';
    Params.paramByName('frq').AsFloat:= frq;
    ExecSQL;
    Close;
    SQLTransAction1.Commit;
  end;
end;

function TFormDatabase.FreqHeardLookup(frq: Real):TStringList;
begin
  Result:=TStringList.Create;
  with SQLQuery1 do begin
     SQL.Text:= 'SELECT seen, strftime("%Y-%m-%d %H:%M:%S", seen_date) AS seen_date_f FROM my_frq_aliases WHERE my_loc_id = 1 AND frq = :frq';
     Params.paramByName('frq').AsFloat:= frq;
     Open;
     if RecordCount > 0 then begin
       Result.Add(FieldByName('seen').AsString);
       Result.Add(FieldByName('seen_date_f').AsString);
     end;
     Close;
   end;
end;

procedure TFormDatabase.BankNameUpdate(scanner: String; bank: Integer; nme: String);
begin
  with SQLQuery1 do begin
    SQL.Text:= 'UPDATE my_mem_banks SET deleted = 1 WHERE deleted = 0 AND scanner = :scanner AND bank = :bank';
    Params.paramByName('scanner').AsString:=scanner;
    Params.paramByName('bank').AsInteger:= bank;
    ExecSQL;

    SQL.Text:= 'INSERT INTO my_mem_banks (scanner, bank, name, deleted) VALUES (:scanner, :bank, :name, 0)';
    Params.paramByName('scanner').AsString:=scanner;
    Params.paramByName('bank').AsInteger:= bank;
    Params.paramByName('name').AsString:=nme;
    ExecSQL;
    Close;
    SQLTransAction1.Commit;
  end;
end;

function TFormDatabase.FrqDescriptionLookup(frq: Real; empty_string_if_not_found: Boolean = true):String;
begin
  with SQLQuery1 do begin
     SQL.Text:= 'SELECT description FROM my_frq_aliases WHERE my_loc_id = 1 AND frq = :frq';
     Params.paramByName('frq').AsFloat:= frq;
     Open;
     if RecordCount = 0 then begin
       Result:=IfThen(empty_string_if_not_found, '', NOT_FOUND);
     end else begin
       Result:=Fields[0].AsString;
     end;
     Close;
   end;
end;

function TFormDatabase.BankNameLookup(scanner: String; bank: Integer):String;
begin
  with SQLQuery1 do begin
     SQL.Text:= 'SELECT name FROM my_mem_banks WHERE deleted = 0 AND scanner = :scanner AND bank = :bank';
     Params.paramByName('bank').AsInteger:= bank;
     Params.paramByName('scanner').AsString:= scanner;
     Open;
     Result:=Fields[0].AsString;
     Close;
   end;
end;

function TFormDatabase.GetAutoMemorySnapshot(scanner: String): Integer;
begin
 Result:=CreateScannerMemorySnapshot(scanner, true);
end;

function TFormDatabase.CreateScannerMemorySnapshot(scanner: String; auto: Boolean = false): Integer;
begin
  with SQLQuery1 do begin
    SQL.Text:= 'SELECT id FROM my_mem_snapshots_index WHERE scanner = :scanner AND auto = :auto ORDER BY created DESC LIMIT 1';
    Params.paramByName('scanner').AsString:= scanner;
    Params.ParamByName('auto').AsBoolean:=auto;
    Open;
    if RecordCount = 1 then begin
      Result:=Fields[0].AsInteger;
      Close;
      Exit;
    end;
    Close;
    SQL.Text:= 'INSERT INTO my_mem_snapshots_index (scanner, created, auto) VALUES (:scanner, datetime(), :auto)';
    Params.paramByName('scanner').AsString:=scanner;
    Params.ParamByName('auto').AsBoolean:=auto;
    ExecSQL;
    Close;
    SQLTransAction1.Commit;
  end;

  Result:=CreateScannerMemorySnapshot(scanner, auto);
end;

function TFormDatabase.ScannerModelFromSnapshot(snapshot_id: Integer):String;
begin
  with SQLQuery1 do begin
     SQL.Text:= 'SELECT scanner FROM my_mem_snapshots_index WHERE id = :id';
     Params.paramByName('id').AsInteger:= snapshot_id;
     Open;
     if RecordCount > 0 then begin
       Result:=Fields[0].AsString;
     end;
     Close;
   end;
end;

procedure TFormDatabase.MemSnapshotSlotUpdate(snapshot_id, bank, slot: Integer; frq: Real; modul: String; dly, lo: Integer);
var
  rc: Integer;
begin
  FormDebug.Debug('MemSnapshotSlotUpdate: ' + IntToStr(slot) + ' ' + FloatToStr(frq));

  with SQLQuery1 do begin
    SQL.Text:= 'SELECT * FROM my_mem_snapshots WHERE snapshot_id = :snapshot_id AND slot = :slot';
    Params.paramByName('snapshot_id').AsInteger:= snapshot_id;
    Params.paramByName('slot').AsInteger:= slot;
    Open;
    rc:=RecordCount;
    Close;

    SQL.Text:=IfThen(rc = 0,
      'INSERT INTO my_mem_snapshots (snapshot_id, bank, slot, frq, modul, dly, lo) VALUES (:snapshot_id, :bank, :slot, :frq, :modul, :dly, :lo)',
      'UPDATE my_mem_snapshots SET bank = :bank, frq = :frq, modul = :modul, dly = :dly, lo = :lo WHERE snapshot_id = :snapshot_id AND slot = :slot');
    Params.paramByName('snapshot_id').AsInteger:= snapshot_id;
    Params.paramByName('bank').AsInteger:= bank;
    Params.paramByName('slot').AsInteger:= slot;
    Params.paramByName('frq').AsFloat:= frq;
    Params.paramByName('modul').AsString:= modul;
    Params.paramByName('dly').AsInteger := dly;
    Params.paramByName('lo').AsInteger := lo;
    ExecSQL;
    Close;
  end;
end;

function TFormDatabase.MemCurrentSlotsGet(snapshot_id: Integer):TStringList;
var
  list: TStringList;
  i: Integer;
  line: String;
begin
  list:=TStringList.Create;
  with SQLQuery1 do begin
    SQL.Text:= 'SELECT bank, slot, frq, modul, dly, lo FROM my_mem_snapshots WHERE snapshot_id = :snapshot_id';
    Params.paramByName('snapshot_id').AsInteger:= snapshot_id;
    Open;
    First;
    while (not EOF) do begin
      line:=FieldByName('bank').AsString  + DB_DELIM +
            FieldByName('slot').AsString  + DB_DELIM +
            FieldByName('frq').AsString   + DB_DELIM +
            FieldByName('modul').AsString + DB_DELIM +
            FieldByName('dly').AsString   + DB_DELIM +
            FieldByName('lo').AsString;
      list.Add(line);
      Next;
    end;
    Close;
    SQLTransAction1.Commit;
  end;
  Result:=list;
end;

function TFormDatabase.CheckForColumnExists(table_name, column_name: String): Boolean;
begin
  with SQLQuery1 do begin
    try
     SQL.Text:= 'SELECT '+ column_name +' FROM ' + table_name;
     Open;
     Result:=true;
     Close;
    except
     on E : Exception do begin
       Result:=false;
       Close;
     end;
    end;
  end;
end;

function TFormDatabase.CheckForTableExists(table_name: String): Boolean;
begin
  with SQLQuery1 do begin
     SQL.Text:= 'SELECT name FROM sqlite_master WHERE type="table" AND name=:table_name';
     Params.paramByName('table_name').AsString:= table_name;
     Open;
     if RecordCount > 0 then begin
       Result:=true;
     end else begin
       Result:=false;
     end;
     Close;
     SQLTransAction1.Commit;
   end;
end;

procedure TFormDatabase.ConfigSet(opt, value: String);
begin
  with SQLQuery1 do begin
    SQL.Text:= IfThen(ConfigGet(opt) = NOT_FOUND,
      'INSERT INTO my_config (var, val) VALUES (:opt, :val)',
      'UPDATE my_config SET val=:val WHERE var=:opt');
    Params.paramByName('opt').AsString:= opt;
    Params.paramByName('val').AsString := value;
    ExecSQL;
    Close;
    SQLTransAction1.Commit;
  end;
end;

function TFormDatabase.ConfigGet(opt: String): String;
begin
   with SQLQuery1 do begin
     SQL.Text:= 'SELECT val FROM my_config WHERE var=:opt';
     Params.paramByName('opt').AsString:= opt;
     Open;
     if RecordCount > 0 then begin
       Result:=Fields[0].AsString;
     end else begin
       Result:=NOT_FOUND;
     end;
     Close;
     SQLTransAction1.Commit;
   end;
end;

end.

