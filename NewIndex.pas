unit NewIndex;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids, TarantoolTypes, msgpack;

type

  { TNewIndexForm }

  TNewIndexForm = class(TForm)
    AddFormatRecord: TButton;
    CanBeNil: TCheckBox;
    IsUnique: TCheckBox;
    FieldSelector: TComboBox;
    Label3: TLabel;
    RemoveFormatRecord: TButton;
    CancelBtn: TButton;
    CreateBtn: TButton;
    Label1: TLabel;
    Label2: TLabel;
    IndexName: TEdit;
    Grid: TStringGrid;
    procedure AddFormatRecordClick(Sender: TObject);
    procedure CanBeNilChange(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure CreateBtnClick(Sender: TObject);
    procedure FieldSelectorChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RemoveFormatRecordClick(Sender: TObject);     
    procedure UpDateFieldSelector();
  private
  public
    //Parameters
    Primary:Boolean;
    SpaceFormat:TTSpaceFormat;
    //
    DoNew:Boolean;
    function AsMsgPack:IMsgPackObject;
  end;

var
  NewIndexForm: TNewIndexForm;

implementation

{$R *.frm}

{ TNewIndexForm }

function TNewIndexForm.AsMsgPack:IMsgPackObject;
var
  MPO:TMsgPackObject;
  i,i0:Integer;
begin
  Result:=TMsgPackObject.Create(mptArray);
  for i:=1 to Grid.RowCount-1 do
  begin
    MPO:=TMsgPackObject.Create(mptMap);
    with SpaceFormat.GetEnumerator do
    begin
      i0:=0;
      while MoveNext do
      begin
        if Grid.Cells[0,i]=Current.Name then
        begin
          MPO.AsMap.Put('field',TMsgPackObject.Create(i0+1));
          MPO.AsMap.Put('is_nullable',TMsgPackObject.Create('true'=Grid.Cells[1,i]));
          case Current.&Type of
          ttUnsigned:
            MPO.AsMap.Put('type',TMsgPackObject.Create('unsigned'));
          ttString:
            MPO.AsMap.Put('type',TMsgPackObject.Create('string'));
          ttVarbinary:
            MPO.AsMap.Put('type',TMsgPackObject.Create('varbinary'));
          ttInteger:
            MPO.AsMap.Put('type',TMsgPackObject.Create('integer'));
          ttNumber:
            MPO.AsMap.Put('type',TMsgPackObject.Create('number'));
          ttDouble:
            MPO.AsMap.Put('type',TMsgPackObject.Create('double'));
          ttBoolean:
            MPO.AsMap.Put('type',TMsgPackObject.Create('boolean'));
          ttDecimal:
            MPO.AsMap.Put('type',TMsgPackObject.Create('decimal'));
          ttArray:
            MPO.AsMap.Put('type',TMsgPackObject.Create('array'));
          ttMap:
            MPO.AsMap.Put('type',TMsgPackObject.Create('map'));
          ttScalar:
            MPO.AsMap.Put('type',TMsgPackObject.Create('scalar'));
          end;
          Break;
        end;
        inc(i0);
      end;
      Free;
    end;
    Result.AsArray.Add(MPO);
    Pointer(MPO):=nil
  end;
end;

procedure TNewIndexForm.FormCreate(Sender: TObject);
begin
  SpaceFormat:=TTSpaceFormat.Create();
end;   

procedure TNewIndexForm.FormShow(Sender: TObject);
begin            
  DoNew:=False;
  while Grid.RowCount>1 do
    Grid.DeleteRow(1);
  if Primary then
    IndexName.Text:='primary'
  else                    
    IndexName.Text:='NewIndex';
  IndexName.Enabled:=not Primary;
  CanBeNil.Enabled:=not Primary;
  CanBeNil.Checked:=False;
  IsUnique.Checked:=True;
  IsUnique.Enabled:=not Primary;

  UpDateFieldSelector();
  RemoveFormatRecord.Enabled:=False;
  CreateBtn.Enabled:=False;
end;

procedure TNewIndexForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(SpaceFormat);
end;

procedure TNewIndexForm.CanBeNilChange(Sender: TObject);
begin

end;

procedure TNewIndexForm.CancelBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TNewIndexForm.CreateBtnClick(Sender: TObject);
begin
  DoNew:=True;
  Close;
end;

procedure TNewIndexForm.AddFormatRecordClick(Sender: TObject);
begin
  Grid.InsertRowWithValues(Grid.RowCount,[FieldSelector.Text,BoolToStr(CanBeNil.Checked,'true','false')]);
  FieldSelector.Text:='';
  UpDateFieldSelector();
  RemoveFormatRecord.Enabled:=True;
  CreateBtn.Enabled:=True;
end;

procedure TNewIndexForm.FieldSelectorChange(Sender: TObject);
begin
  AddFormatRecord.Enabled:=FieldSelector.Text<>'';
end;

procedure TNewIndexForm.RemoveFormatRecordClick(Sender: TObject);
begin
  Grid.DeleteRow(Grid.RowCount-1);         
  UpDateFieldSelector();
  RemoveFormatRecord.Enabled:=Grid.RowCount>1;   
  CreateBtn.Enabled:=Grid.RowCount>1;
end;               

procedure TNewIndexForm.UpDateFieldSelector();
var
  b:Boolean;
  i:Integer;
begin
  FieldSelector.Items.Clear;
  with SpaceFormat.GetEnumerator do
  begin
    while MoveNext do
    begin
      b:=True;
      for i:=1 to Grid.RowCount-1 do
        if Grid.Cells[0,i]=Current.Name then
        begin
          B:=False;
          Break;
        end;
      if B then
        FieldSelector.Items.Add(Current.Name);
    end;
    Free;
  end;        
  AddFormatRecord.Enabled:=FieldSelector.Text<>'';
end;

end.
