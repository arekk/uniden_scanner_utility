unit UBC00Database;

{$mode objfpc}{$H+}

interface


uses
  Classes, SysUtils, Math, StrUtils, UnitDebug, Common, UnitDatabase;

type
  TUbc00database = class(TInterfacedObject, IScanner)
    function UpdateSlot(memory_row: TScannerMemoryRow): Boolean;
    function GetMemorySlots: TScannerMemory;
    function NumberOfBanks: Integer;
    function NumberOfSlots: Integer;
    function Alive: SmallInt;
    function ModelAliased: String;
    function Model: String;
    function PortId: String;
    function ReadOnly: Boolean;
    function Loaded: Boolean;
    function SupportsDescription:Boolean;
    procedure Unload;

    public
      constructor Create;

    private
      MemIsLoaded: Boolean;
      SnapshotId: Integer;
      SnapshotModel: String;
      Slots: Integer;
      Banks: Integer;
  end;

implementation

constructor TUbc00database.Create;
begin
  Unload;
end;

procedure TUbc00database.Unload;
begin
  MemIsLoaded:=false;
  SnapshotModel:=Model;
  Slots:=0;
  Banks:=0;
end;

function TUbc00database.Loaded: Boolean;
begin
   Result:=MemIsLoaded;
end;

function TUbc00database.ReadOnly: Boolean;
begin
  Result:=true;
end;

function TUbc00database.NumberOfBanks: Integer;
begin
  Result:=Banks;
end;

function TUbc00database.NumberOfSlots: Integer;
begin
  Result:=Slots;
end;

function TUbc00database.ModelAliased: String;
begin
  Result:=SnapshotModel;
end;

function TUbc00database.Model: String;
begin
  Result:='Database snapshot';
end;

function TUbc00database.PortId: String;
begin
  Result:='Sqlite';
end;

function TUbc00database.Alive: SmallInt;
begin
  Result:=1;
end;

function TUbc00database.UpdateSlot(memory_row: TScannerMemoryRow): Boolean;
begin

end;

function TUbc00database.GetMemorySlots: TScannerMemory;
var
  i: Integer;
  snapshot_items, item: TStringList;
  row: TScannerMemoryRow;
  rows: TScannerMemory;
  m: String;
begin
  SetLength(rows, 0);

  m:=FormDatabase.ConfigGet('last_used_scanner_m');
  if m <> NOT_FOUND then begin
    SnapshotModel:=m;
    Slots:=StrToInt(FormDatabase.ConfigGet('last_used_scanner_s'));
    Banks:=StrToInt(FormDatabase.ConfigGet('last_used_scanner_b'));

    SnapshotId:=FormDatabase.GetAutoMemorySnapshot(m);

    snapshot_items:=FormDatabase.MemCurrentSlotsGet(SnapshotId);
    SetLength(rows, snapshot_items.Count);

    if snapshot_items.Count > 0 then begin
      for i:=1 to (snapshot_items.Count) do begin
        item:=TStringList.Create;
        item.Delimiter:=DB_DELIM;
        item.StrictDelimiter:=true;
        item.DelimitedText:=snapshot_items[i-1];

        row:=TScannerMemoryRow.Create;
        row.Valid:= true;
        row.Model:=SnapshotModel;
        row.LookupForFrqDescriptions:=true;
        row.SetData(item[0], item[1], item[2], item[3], item[4], item[5]);
        rows[i]:=row;
      end;
    end;
  end;
  MemIsLoaded:=True;
  Result:=rows;
end;

function TUbc00database.SupportsDescription:Boolean;
begin
  Result:=false;
end;

end.

