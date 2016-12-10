unit UBCGeneric1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, Synaser, StrUtils, UnitDebug, Common;

type
  TUBCGeneric1 = class(TInterfacedObject, IScanner)
    function NumberOfBanks: Integer; virtual;
    function NumberOfSlots: Integer; virtual;
    function ModelAliased: String; virtual;
    function Model: String; virtual;
    function PortId: String; virtual;
    function SupportsDescription:Boolean; virtual;
    function UpdateSlot(memory_row: TScannerMemoryRow): Boolean; virtual;

    function GetMemorySlots: TScannerMemory;
    function Alive: SmallInt;
    function ReadOnly: Boolean;
    function Loaded: Boolean;
    procedure Unload;

    public
      constructor Create;

    protected
      function SendAndRecieve(cmd: String): TStringList;

    public
      ComPort: String;
      Online: Boolean;

    protected
      CommInProgress: Boolean;
      MemIsLoaded: Boolean;
  end;

implementation

function TUBCGeneric1.NumberOfBanks: Integer; begin
  raise Exception.Create('Need to be overloaded');
end;

function TUBCGeneric1.NumberOfSlots: Integer; begin
  raise Exception.Create('Need to be overloaded');
end;

function TUBCGeneric1.ModelAliased: String; begin
  raise Exception.Create('Need to be overloaded');
end;

function TUBCGeneric1.Model: String; begin
  raise Exception.Create('Need to be overloaded');
end;

function TUBCGeneric1.PortId: String; begin
  raise Exception.Create('Need to be overloaded');
end;

function TUBCGeneric1.SupportsDescription:Boolean; begin
  raise Exception.Create('Need to be overloaded');
end;

function TUBCGeneric1.UpdateSlot(memory_row: TScannerMemoryRow): Boolean; begin
  raise Exception.Create('Need to be overloaded');
end;

constructor TUBCGeneric1.Create;
begin
  Online:=false;
  MemIsLoaded:=false;
  CommInProgress:=false;
end;

procedure TUBCGeneric1.Unload;
begin
  MemIsLoaded:=false;
end;

function TUBCGeneric1.Loaded: Boolean;
begin
  Result:=MemIsLoaded;
end;

function TUBCGeneric1.ReadOnly: Boolean;
begin
   Result:=false;
end;

function TUBCGeneric1.Alive: SmallInt;
var
  ports: TStringList;
  n: Integer;
  port: String;
  port_detail : String;
  buf: TStringList;
  m: String;
  r: Boolean;
begin
  if CommInProgress = true then begin
    Result:=-1;
    Exit;
  end;

  r:=false;
  ports:=GetSerialPortNames;
  for n := 0 to ports.Count - 1 do begin
    port:=ports[n];
    port_detail:=GetSerialPortFriendlyName(port);
    FormDebug.Debug(port_detail);
    if (NPos(PortId, port_detail, 1) > 0) then begin
      ComPort:=port;
      buf:=SendAndRecieve('MDL');
      if buf.Count = 2 then begin
        m:=buf[1];
        if m = Model then begin
          r:=true;
          Break;
        end;
      end;
    end;
  end;

  if Online <> r then begin
    Online:=r;
    FormDebug.Debug(Model + ' ' + IfThen(Online, 'is online on port: ' + port, 'gone offline'));
  end;

  Result:=IfThen(r, 1, 0);
end;

function TUBCGeneric1.GetMemorySlots: TScannerMemory;
var
  i: Integer;
  buf: TStringList;
  buf_frq_up, buf_frq_down: String;
  zeros : TSysCharSet;
  row: TScannerMemoryRow;
  rows: TScannerMemory;
begin
   zeros:=['0'];
   SetLength(rows, NumberOfSlots);

   SendAndRecieve('PRG');
   for i:=1 to NumberOfSlots do begin
     buf:=SendAndRecieve('CIN,' + IntToStr(i));
     row:=TScannerMemoryRow.Create;
     row.Valid:=false;

     if buf.Count >= 9 then begin
       buf_frq_up:=TrimLeftSet(Copy(buf[3], 1, 4), zeros);
       buf_frq_down:=Copy(buf[3], 5, Length(buf[3]));

       // Jesli skaner wspiera wlasne opisy, olewam te z lookupa naspisujac je tym, co jest w skanerze
       row.LookupForFrqDescriptions:=(not SupportsDescription);
       row.FrqDescription:=Trim(buf[2]);
       row.Model:=Model;
       row.Valid:= true;
       row.SetData(
         Ceil(StrToInt(buf[1])/(NumberOfSlots/NumberOfBanks)),
         StrToInt(buf[1]),
         StrToFloat(buf_frq_up + ',' + buf_frq_down),
         buf[4],
         StrToInt(buf[6]),
         StrToInt(buf[7])
       );
     end;
     rows[i]:=row;
   end;
   SendAndRecieve('EPG');
   MemIsLoaded:=True;
   Result:=rows;
end;

function TUBCGeneric1.SendAndRecieve(cmd: String): TStringList;
var
  ser: TBlockSerial;
  lne: String;
  buf: TStringList;
  err: Boolean;
begin
  CommInProgress:=true;
  err:=true;

  buf:=TStringlist.Create;
  buf.Delimiter:=',';
  buf.StrictDelimiter:=true;

  ser:=TBlockSerial.Create;
  FormDebug.Debug('-> ' + cmd);
  try
     ser.Connect(ComPort);
     ser.config(57600, 8, 'N', SB1, False, False);
     if ser.LastError = 0 then begin
        ser.SendString(cmd + AnsiString(#13));
        if ser.LastError = 0 then begin
           ser.ConvertLineEnd:= true;
           lne:=ser.Recvstring(5000);
           if ser.LastError = 0 then begin
              FormDebug.Debug('<- ' + lne);
              if (lne <> 'ERR') and (Copy(lne,1,Length(cmd)) = cmd) then begin
                 buf.DelimitedText:= lne;
                 err:=false;
              end;
           end;
        end;
     end;
     if err = true then begin
        FormDebug.Debug('Error: Device: ' + ser.Device + ' Status: ' + ser.LastErrorDesc + ' ' + Inttostr(ser.LastError));
     end;
   finally
     ser.free;
   end;
   CommInProgress:=false;
   Result:=buf;
end;

end.

