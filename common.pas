unit Common;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, UnitDatabase, Registry;

type

  TScannerMemoryRow = class
    Slot: Integer;
    Bank: Integer;
    BankName: String;
    Frq: Real;
    Modul: String;
    Dly: Integer;
    Lo: Integer;
    FrqDescription: String;
    SeenCount: Integer;
    SeenDate: TDateTime;

    Valid: boolean;
    Model: String;
    LookupForFrqDescriptions: Boolean;

    function isFree: Boolean;
    function Contains(text: String): Boolean;

    procedure Clear;
    procedure SetData(a_bank, a_slot, a_frq, a_modul, a_dly, a_lo: String);
    procedure SetData(a_bank, a_slot:Integer; a_frq: Real; a_modul: String; a_dly, a_lo: Integer);
    procedure UpdateData(a_frq, a_modul, a_dly, a_lo: String);
    procedure UpdateData(memory_row: TScannerMemoryRow);
    procedure SetExtraData;
  end;

  TScannerMemory = array of TScannerMemoryRow;

  IScanner = interface
    function UpdateSlot(memory_row: TScannerMemoryRow): Boolean;
    function GetMemorySlots: TScannerMemory;
    function NumberOfBanks: Integer;
    function NumberOfSlots: Integer;
    function Alive: SmallInt;
    function ModelAliased: String;
    function Model: String;
    function PortId: String;
    function ReadOnly: Boolean;            // ostrożnie, to mówi czy calosciowy mechanizm jest readonly, nie zas czy w danym momencue jest readonly
    function Loaded: Boolean;
    function SupportsDescription:Boolean;  // to mowi, czy skaner wspiera przechowywanie opisow
    procedure Unload;
  end;

  function FindPosOfSlotInScannerMemory(scanner_memory: TScannerMemory; slot: Integer):Integer;
  function FloatToFrq(frq: Real; sepatator: String = '.'; pad_left: Boolean = false):String;
  function FrqToFloat(frq: String): Real;
  function GetSerialPortFriendlyName(port: string): string;
  function GetSerialPortNames: TStringList;
  function GetSerialPortsInfo:String;

implementation

function TScannerMemoryRow.isFree: Boolean;
begin
  Result:= (Frq <= 0);
end;

function TScannerMemoryRow.Contains(text: String): Boolean;
var
  all_strings: String;
begin
  Result:=false;
  all_strings:=Lowercase(BankName + FloatToFrq(Frq) + FrqDescription);
  text:=Lowercase(AnsiReplaceText(Trim(text),',','.'));
  if text = IntToStr(Slot) then begin
    Result:=true;
  end;
  if NPos(text, all_strings, 1) > 0 then begin
    Result:=True;
  end;
end;

procedure TScannerMemoryRow.Clear;
begin
  Frq:=0;
  Modul:='FM';
  Dly:=0;
  Lo:=0;
  FrqDescription:='';
  SetExtraData;
end;

procedure TScannerMemoryRow.SetData(a_bank, a_slot, a_frq, a_modul, a_dly, a_lo: String);
begin
  Bank:=StrToInt(IfThen(Trim(a_bank) <> '', a_bank, '-1'));
  Slot:=StrToInt(IfThen(Trim(a_slot) <> '', a_slot, '-1'));
  Frq:=StrToFloat(AnsiReplaceText(Trim(a_frq),'.',','));
  Modul:=Trim(a_modul);
  Dly:=StrToInt(IfThen(Trim(a_dly) <> '', a_dly, '-1'));
  Lo:=StrToInt(IfThen(Trim(a_lo) <> '', a_lo, '-1'));
  SetExtraData;
end;

procedure TScannerMemoryRow.SetData(a_bank, a_slot:Integer; a_frq: Real; a_modul: String; a_dly, a_lo: Integer);
begin
  Bank:=a_bank;
  Slot:=a_slot;
  Frq:=a_frq;
  Modul:=Trim(a_modul);
  Dly:=a_dly;
  Lo:=a_lo;
  SetExtraData;
end;

procedure TScannerMemoryRow.UpdateData(a_frq, a_modul, a_dly, a_lo: String);
begin
  Frq:=FrqToFloat(a_frq);
  Modul:=Trim(a_modul);
  Dly:=StrToInt(IfThen(Trim(a_dly) <> '', a_dly, '-1'));
  Lo:=StrToInt(IfThen(Trim(a_lo) <> '', a_lo, '-1'));
  SetExtraData;
end;

procedure TScannerMemoryRow.UpdateData(memory_row: TScannerMemoryRow);
begin
  Frq:=memory_row.Frq;
  Modul:=memory_row.Modul;
  Dly:=memory_row.Dly;
  Lo:=memory_row.Lo;
  SetExtraData;
end;

procedure TScannerMemoryRow.SetExtraData;
var
  seen_data: TStringList;
begin
  BankName:=IfThen(Bank = -1, IntToStr(Bank), FormDatabase.BankNameLookup(Model, Bank));
  if LookupForFrqDescriptions then begin
    FrqDescription:=IfThen(Frq > 0, FormDatabase.FrqDescriptionLookup(Frq), '');
  end;
  SeenCount:=0;
  seen_data:=FormDatabase.FreqHeardLookup(Frq);
  if seen_data.Count = 2 then begin
    SeenCount:=StrToInt(seen_data[0]);
    if (SeenCount > 0) and (seen_data[1] <> '') then begin
      SeenDate:=StrToDateTime(seen_data[1]);
    end;
  end;
end;

function FindPosOfSlotInScannerMemory(scanner_memory: TScannerMemory; slot: Integer):Integer;
var
  i: Integer;
begin
  for i:=1 to Length(scanner_memory) do begin
    if scanner_memory[i].Slot = slot then begin
      Result:=i;
      Exit;
    end;
  end;
  Result:=-1;
end;

 function GetSerialPortFriendlyName(port: string): string;
  function FriendlyName(key: string; port: string): string;
  var
    r : TRegistry;
    k : TStringList;
    i : Integer;
    ck: string;
    rs: string;
  begin
    r := TRegistry.Create;
    k := TStringList.Create;

    r.RootKey := HKEY_LOCAL_MACHINE;
    r.OpenKeyReadOnly(key);
    r.GetKeyNames(k);
    r.CloseKey;

    try
      for i := 0 to k.Count - 1 do
      begin
        ck := key + k[i] + '\'; // current key
        // looking for "PortName" stringvalue in "Device Parameters" subkey
        if r.OpenKeyReadOnly(ck + 'Device Parameters') then
        begin
          if r.ReadString('PortName') = port then
          begin
            //Memo1.Lines.Add('--> ' + ck);
            r.CloseKey;
            r.OpenKeyReadOnly(ck);
            rs := r.ReadString('FriendlyName');
            Break;
          end // if r.ReadString('PortName') = port ...
        end  // if r.OpenKeyReadOnly(ck + 'Device Parameters') ...
        // keep looking on subkeys for "PortName"
        else // if not r.OpenKeyReadOnly(ck + 'Device Parameters') ...
        begin
          if r.OpenKeyReadOnly(ck) and r.HasSubKeys then
          begin
            rs := FriendlyName(ck, port);
            if rs <> '' then Break;
          end; // if not (r.OpenKeyReadOnly(ck) and r.HasSubKeys) ...
        end; // if not r.OpenKeyReadOnly(ck + 'Device Parameters') ...
      end; // for i := 0 to k.Count - 1 ...
      result := rs;
    finally
      r.Free;
      k.Free;
    end; // try ...
  end; // function findFriendlyName ...
begin
   Result:=FriendlyName('\System\CurrentControlSet\Enum\', port);
end;

function GetSerialPortNames: TStringList;
var
  reg  : TRegistry;
  l,v  : TStringList;
  n    : integer;
begin
  v      := TStringList.Create;
  l      := TStringList.Create;
  reg    := TRegistry.Create;
  try
    reg.RootKey := HKEY_LOCAL_MACHINE;
    if reg.OpenKeyReadOnly('HARDWARE\DEVICEMAP\SERIALCOMM') then begin
      reg.GetValueNames(l);
      for n := 0 to l.Count - 1 do begin
        v.Add(reg.ReadString(l[n]));
      end;
    end;
  finally
    reg.Free;
  end;
  Result := v;
end;

function GetSerialPortsInfo:String;
var
  ports, ports_ex: TStringList;
  n: Integer;
begin
   ports_ex:=TStringList.Create;
   ports:=GetSerialPortNames;

   for n := 0 to ports.Count - 1 do begin
     ports_ex.Add(GetSerialPortFriendlyName(ports[n]));
   end;
   Result:=ports_ex.CommaText;
end;

function FrqToFloat(frq: String): Real;
begin
  frq:= IfThen(Trim(frq) = '', '0', Trim(frq));
  frq:= AnsiReplaceText(frq,'.',',');
  Result :=StrToFloat(frq);
end;

function FloatToFrq(frq: Real; sepatator: String = '.'; pad_left: Boolean = false):String;
var
  l_frq: TStringList;
  u_frq: String;
begin
  l_frq:=TStringlist.Create;
  l_frq.Delimiter:=',';
  l_frq.DelimitedText:=FloatToStr(frq);
  if l_frq.Count = 1 then begin
    l_frq.Add('0');
  end;
  u_frq:=IfThen(pad_left, AddChar('0',l_frq[0],4), l_frq[0]);
  Result:= u_frq + sepatator + AddCharR('0',l_frq[1],4);
end;

end.

