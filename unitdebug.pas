unit UnitDebug;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { TFormDebug }

  TFormDebug = class(TForm)
    MemoDebug: TMemo;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    { private declarations }
  public
    { public declarations }
    procedure Debug(msg: string);
  end;

var
  FormDebug: TFormDebug;

implementation

{$R *.lfm}

procedure TFormDebug.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:= caFree;
end;

procedure TFormDebug.Debug(msg: string);
begin
  MemoDebug.Append(IntToStr(Length(msg)) + ': ' + msg);
end;

end.

