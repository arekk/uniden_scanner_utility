unit UBC125XLT;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UBCGeneric1, Common;

type
  TUbc125xlt = class(TUBCGeneric1)
    function NumberOfBanks: Integer; override;
    function NumberOfSlots: Integer; override;
    function Model: String; override;
    function ModelAliased: String; override;
    function PortId: String; override;
    function UpdateSlot(memory_row: TScannerMemoryRow): Boolean; override;
    function SupportsDescription:Boolean; override;
  end;

implementation

function TUbc125xlt.PortId: String;
begin
  Result:='BC125AT';
end;

function TUbc125xlt.NumberOfBanks: Integer;
begin
  Result:=10;
end;

function TUbc125xlt.NumberOfSlots: Integer;
begin
  Result:=500;
end;

function TUbc125xlt.ModelAliased: String;
begin
  Result:='UBC125XLT';
end;

function TUbc125xlt.Model: String;
begin
  Result:='UBC125XLT';
end;

function TUbc125xlt.UpdateSlot(memory_row: TScannerMemoryRow): Boolean;
var
  cmd: String;
begin
  if (memory_row.Slot > 0) and
     (memory_row.Slot <= NumberOfSlots) and
     ((memory_row.Modul = 'AM') or (memory_row.Modul = 'FM'))
  then begin
    //  CIN,1,Okecie Approach,01288000,AM,0,2,0,0
    cmd:='CIN,'+IntToStr(memory_row.Slot)+','+memory_row.FrqDescription+','+FloatToFrq(memory_row.Frq, '', true)+','+Trim(memory_row.Modul)+',0,2,0,0';

    SendAndRecieve('PRG');
    SendAndRecieve(cmd);
    SendAndRecieve('EPG');
  end;
end;

function TUbc125xlt.SupportsDescription:Boolean;
begin
  Result:=true;
end;

end.

