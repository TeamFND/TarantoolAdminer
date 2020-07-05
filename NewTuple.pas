unit NewTuple;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Utils,
  Generics.Collections, TarantoolTypes, msgpack;

type

  { TNewTupleForm }

  TNewTupleForm = class(TForm)
    CancelBtn: TButton;
    CreateBtn: TButton;
    procedure CancelBtnClick(Sender: TObject);
    procedure CreateBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);   
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private             
    Values:TList<TMsgPackValueEditor>;
  public
    //params
    Format:TTSpaceFormat;    
    Edit:Boolean;
    DataRow:IMsgPackObject;

    DoNew:Boolean;
    function AsMsgPack:IMsgPackObject;
  end;

var
  NewTupleForm: TNewTupleForm;

implementation

{$R *.frm}

{ TNewTupleForm }

procedure TNewTupleForm.CancelBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TNewTupleForm.CreateBtnClick(Sender: TObject);
begin
  DoNew:=True;
  Close;
end;

procedure TNewTupleForm.FormCreate(Sender: TObject);
begin
  Values:=TList<TMsgPackValueEditor>.Create();
end;

procedure TNewTupleForm.FormShow(Sender: TObject);
var
  Num,i:Integer;
begin
  DoNew:=False;
  Num:=4+0*26;
  i:=0;                
  with Values.GetEnumerator do
  begin
    while MoveNext do
      Current.Free;
    Free;
  end;
  Values.Clear;
  with Format.GetEnumerator do
  begin
    while MoveNext do
      with Current do
      begin                                  
        if Edit then
          Values.Add(TMsgPackValueEditorWithInit.Create(Self,Self,Num,Name,&Type,IsNullable,DataRow.AsArray.Get(i)))
        else
          Values.Add(TMsgPackValueEditor.Create(Self,Self,Num,Name,&Type,IsNullable));
        inc(i);
      end;
    Free;
  end;
  CreateBtn.Top:=Num;
  CancelBtn.Top:=Num;
  Self.Height:=Num+30;
end;  

procedure TNewTupleForm.FormDestroy(Sender: TObject);
begin
  with Values.GetEnumerator do
  begin
    while MoveNext do
      Current.Free;
    Free;
  end;
  FreeAndNil(Values);
end;

function TNewTupleForm.AsMsgPack:IMsgPackObject;
begin
  Result:=TMsgPackObject.Create(mptArray);
  with Values.GetEnumerator do
  begin
    while MoveNext do
      Result.AsArray.Add(Current.GetMsgPack);
    Free;
  end;
end;

end.