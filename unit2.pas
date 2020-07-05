unit Unit2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Spin;

type

  { TForm2 }

  TForm2 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    User: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Address: TLabeledEdit;
    Password: TLabeledEdit;
    Port: TSpinEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public    
    DoConnection:Boolean;

  end;

var
  Form2: TForm2;

implementation

{$R *.frm}

{ TForm2 }

procedure TForm2.Button1Click(Sender: TObject);
begin
  DoConnection:=true;
  Close;
end;

procedure TForm2.Button2Click(Sender: TObject);
begin 
  Close;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin

end;

procedure TForm2.FormShow(Sender: TObject);
begin
  DoConnection:=False;
end;

end.