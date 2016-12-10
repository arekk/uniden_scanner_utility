unit UnitInfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, LCLIntf;

type

  { TForm3 }

  TForm3 = class(TForm)
    OkButton1: TButton;
    Panel1: TPanel;
    StaticText1: TStaticText;
    StaticText3: TStaticText;
    procedure OkButton1Click(Sender: TObject);
    procedure StaticText3Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.lfm}

{ TForm3 }

procedure TForm3.OkButton1Click(Sender: TObject);
begin
  Form3.Close;
end;

procedure TForm3.StaticText3Click(Sender: TObject);
begin
  OpenURL('https://github.com/arekk/uniden_scanner_utility');
end;

end.

