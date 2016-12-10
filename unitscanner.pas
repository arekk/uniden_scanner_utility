unit UnitScanner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Common, UBC75XLT, UBC125XLT, UBC00Database, UnitDebug;

var
 Scanners: array of IScanner;
 Scanner: IScanner;
 ScannerOnline: String = 'nil';

 procedure ScannerInit;
 function FindAliveScanner:Boolean;

implementation

procedure ScannerInit;
begin
  SetLength(Scanners, 3);
  Scanners[0]:=TUbc75xlt.Create;
  Scanners[1]:=TUbc125xlt.Create;
  Scanners[2]:=TUBC00Database.Create;
end;

function FindAliveScanner:Boolean;
var
 i, r: Integer;
begin
  Result:=false;
  for i:=0 to Length(Scanners) do begin
    r:=Scanners[i].Alive;
    FormDebug.Debug('Checking ' + Scanners[i].Model + ': ' + IntToStr(r));

    // Pierwszy żywy skaner wychodzi (ważne, aby baza danych byla na koncu listy)
    if (r = 1) or (r = -1) then begin
      if ScannerOnline <> Scanners[i].Model then begin
        FormDebug.Debug('Changing scanners, was: ' + ScannerOnline + ', is: ' + Scanners[i].Model);
        if Scanner <> nil then begin
          Scanner.Unload;
        end;
        ScannerOnline:=Scanners[i].Model;
        Scanner:=Scanners[i];
      end;
      Result:=(Scanner.Loaded = false);
      Break;
    end;
  end;
end;

end.

