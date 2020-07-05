unit NewSpace;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Generics.Collections, TarantoolTypes, msgpack, NewIndex;

type
  TTFRecord=class;

  { TNewSpaceForm }

  TNewSpaceForm = class(TForm)
    CreateBtn: TButton;
    CancelBtn: TButton;
    AddFormatRecord: TButton;
    EngineSelector: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    SpaceName: TEdit;
    Timer1: TTimer;
    procedure AddFormatRecordClick(Sender: TObject);
    procedure CreateBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    Records:TList<TTFRecord>;
  public
    DoNew:Boolean;
    function AsMsgPack:IMsgPackObject;
  end;

  TTFRecord=class
    private
      &Type:TComboBox;
      Name:TEdit;    
      CanBeNil:TCheckBox;
      Del:TButton;
      Num:Integer;     
      procedure OnDelete(Sender:TObject);
    public
      constructor Create(Owner,Parent:TWinControl;top:integer);
      function GetFormatRecord():TTSpaceFormatRecord;
      procedure Update(top:integer);
      destructor Destroy; override;
  end;

var
  NewSpaceForm: TNewSpaceForm;

implementation

{$R *.frm}

//TTFRecord       

procedure TTFRecord.OnDelete(Sender:TObject);
begin
  NewSpaceForm.Tag:=Num;
  NewSpaceForm.Timer1.Enabled:=True;
end;

constructor TTFRecord.Create(Owner,Parent:TWinControl;top:integer);
begin
  &Type:=TComboBox.Create(Owner);    
  &Type.Parent:=Parent;
  &Type.Items.Text:='unsigned'+sLineBreak+
    'string'+sLineBreak+
    'varbinary'+sLineBreak+
    'integer'+sLineBreak+
    'number'+sLineBreak+
    'double'+sLineBreak+
    'boolean'+sLineBreak+
    'decimal'+sLineBreak+
    'array'+sLineBreak+
    'map'+sLineBreak+
    'scalar';   
  &Type.Text:='unsigned'; 
  &Type.Width:=127;
  &Type.Height:=21;
  &Type.Left:=129;


  Name:=TEdit.Create(Owner);
  Name.Parent:=Parent;
  Name.Text:='Field'+IntToStr(top);
  Name.Width:=118;
  Name.Height:=21;
  Name.Left:=5;

  CanBeNil:=TCheckBox.Create(Owner);
  CanBeNil.Parent:=Parent;
  CanBeNil.Caption:='';
  CanBeNil.Width:=18;
  CanBeNil.Height:=17;
  CanBeNil.Left:=264;

  Del:=TButton.Create(Owner);  
  Del.Parent:=Parent;
  Del.OnClick:=OnDelete;
  Del.Caption:='Del';
  Del.Width:=44;
  Del.Height:=21;
  Del.Left:=264+20;
  Update(top);
end;

function TTFRecord.GetFormatRecord():TTSpaceFormatRecord;
begin
  Result.&Type:=StringToTTType(&Type.Text);   
  Result.Name:=Name.Text;
  Result.IsNullable:=CanBeNil.Checked;
end;

procedure TTFRecord.Update(top:Integer);
begin     
  &Type.Top:=56+top*26;
  Name.Top:=56+top*26;   
  CanBeNil.Top:=58+top*26;
  Del.Top:=56+top*26;
  Num:=top;
end;

destructor TTFRecord.Destroy;
begin
  FreeAndNil(&Type);
  FreeAndNil(Name);   
  FreeAndNil(CanBeNil);
  FreeAndNil(Del);
end;

{ TNewSpaceForm }


function TNewSpaceForm.AsMsgPack:IMsgPackObject;
var
  MPO:TMsgPackObject;
begin
  Result:=TMsgPackObject.Create(mptArray);
  with Records.GetEnumerator do
  begin
    while MoveNext do
    begin
      MPO:=TMsgPackObject.Create(mptMap);
      MPO.AsMap.Put('name',TMsgPackObject.Create(Current.GetFormatRecord().Name));
      MPO.AsMap.Put('type',TMsgPackObject.Create(Current.&Type.Text));
      MPO.AsMap.Put('is_nullable',TMsgPackObject.Create(Current.GetFormatRecord().IsNullable));
      Result.AsArray.Add(MPO);
      Pointer(MPO):=nil
    end;
    Free;
  end;
end;

procedure TNewSpaceForm.CancelBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TNewSpaceForm.FormCreate(Sender: TObject);
begin
  Records:=TList<TTFRecord>.Create();
  AddFormatRecordClick(Sender);
end;

procedure TNewSpaceForm.FormDestroy(Sender: TObject);
begin
  with Records.GetEnumerator do
  begin
    while MoveNext do
      Current.Free;
    Free;
  end;
  FreeAndNil(Records);
end;

procedure TNewSpaceForm.FormShow(Sender: TObject);
begin
  DoNew:=False;
end;

procedure TNewSpaceForm.Timer1Timer(Sender: TObject);
var
  i:Integer;
begin
  Records[Tag].Free;
  Records.Delete(Tag);
  i:=0;
  with Records.GetEnumerator do
  begin
    while MoveNext do
    begin
      Current.Update(i);
      Inc(i);
    end;
    Free;
  end;      
  AddFormatRecord.top:=56+(Records.Count)*26;
  CancelBtn.top:=56+(Records.Count+1)*26;
  CreateBtn.top:=56+(Records.Count+1)*26;
  Height:=56+(Records.Count+2)*26;
  Timer1.Enabled:=false;
end;

procedure TNewSpaceForm.CreateBtnClick(Sender: TObject);
begin
  NewIndexForm.Primary:=True;
  NewIndexForm.SpaceFormat.Clear;
  with Records.GetEnumerator do
  begin
    while MoveNext do
      NewIndexForm.SpaceFormat.Add(Current.GetFormatRecord);
    Free;
  end;
  NewIndexForm.ShowModal;
                           
  if NewIndexForm.DoNew then
  begin
    DoNew:=True;
    Close;
  end;
end;

procedure TNewSpaceForm.AddFormatRecordClick(Sender: TObject);
begin
  Records.Add(TTFRecord.Create(self,self,Records.Count));
  AddFormatRecord.top:=56+(Records.Count)*26;
  CancelBtn.top:=56+(Records.Count+1)*26;
  CreateBtn.top:=56+(Records.Count+1)*26;
  Height:=56+(Records.Count+2)*26;
end;

end.