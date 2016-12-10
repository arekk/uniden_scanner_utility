unit UBC75XLT;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UBCGeneric1, Common;

type
  TUbc75xlt = class(TUBCGeneric1)
    function NumberOfBanks: Integer; override;
    function NumberOfSlots: Integer; override;
    function Model: String; override;
    function ModelAliased: String; override;
    function PortId: String; override;
    function SupportsDescription:Boolean; override;
    function UpdateSlot(memory_row: TScannerMemoryRow): Boolean; override;
  end;

implementation

function TUbc75xlt.PortId: String;
begin
  Result:='Silicon Labs CP210x USB to UART Bridge';
end;

function TUbc75xlt.NumberOfBanks: Integer;
begin
  Result:=10;
end;

function TUbc75xlt.NumberOfSlots: Integer;
begin
  Result:=300;
end;

function TUbc75xlt.ModelAliased: String;
begin
  Result:='UBC75XLT';
end;

function TUbc75xlt.Model: String;
begin
  Result:='UBC75XLT';
end;

function TUbc75xlt.UpdateSlot(memory_row: TScannerMemoryRow): Boolean;
var
  cmd: String;
begin
  if (memory_row.Slot > 0) and
     (memory_row.Slot <= NumberOfSlots) and
     ((memory_row.Modul = 'AM') or (memory_row.Modul = 'FM')) and
     ((memory_row.Dly = 1) or (memory_row.Dly = 0))
  then begin
    cmd:='CIN,'+IntToStr(memory_row.Slot)+',,'+FloatToFrq(memory_row.Frq, '', true)+','+Trim(memory_row.Modul)+',,'+IntToStr(memory_row.Dly)+','+IntToStr(memory_row.Lo)+',0';

    SendAndRecieve('PRG');
    SendAndRecieve(cmd);
    SendAndRecieve('EPG');
  end;
end;


function TUbc75xlt.SupportsDescription:Boolean;
begin
  Result:=false;
end;

end.

