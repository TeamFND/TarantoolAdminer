unit Utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, msgpack, StdCtrls, SpinEx, Controls, TarantoolTypes;

type
  TMsgPackValueEditor=class
    private
      //Config
      BaseType:TTType;
      //Components       
      &Label:TLabel;
      IsNil:TCheckBox; 
      Input:TWinControl;
      procedure UpdateInput(Sender:TObject);
    public
      constructor Create(Owner,Parent:TWinControl;var top:integer;Name:String;ABaseType:TTType;CanBeNil:Boolean);overload;
      function GetMsgPack:IMsgPackObject;
      destructor Destroy; override;
  end;

  TMsgPackValueEditorWithInit=class(TMsgPackValueEditor)
    public
      constructor Create(Owner,Parent:TWinControl;var top:integer;Name:String;ABaseType:TTType;CanBeNil:Boolean;InitValue:IMsgPackObject);
  end;

function BinToStr(B:RawByteString):String;
function MsgPackToStr(MPO:IMsgPackObject):String;

implementation

//TMsgPackValueEditor
procedure TMsgPackValueEditor.UpdateInput(Sender:TObject);
begin
  Input.Enabled:=not IsNil.Checked;
end;

constructor TMsgPackValueEditor.Create(Owner,Parent:TWinControl;var top:integer;Name:String;ABaseType:TTType;CanBeNil:Boolean);
begin
  BaseType:=ABaseType;
  if BaseType<>ttBoolean then
  begin
    &Label:=TLabel.Create(Owner);
    &Label.Parent:=Parent;
    &Label.Caption:=Name+':';
    &Label.Width:=92;
    &Label.Height:=13;
    &Label.Top:=4+top;
    &Label.Left:=8;
  end
  else
    &Label:=nil;

  case BaseType of
    ttUnsigned:
    begin
      Input:=TSpinEditEx.Create(Owner);
      (Input as TSpinEditEx).MinValue:=0;
      (Input as TSpinEditEx).MaxValue:=Int64.MaxValue;
    end;
    ttString:Input:=TEdit.Create(Owner);
    ttInteger:
    begin
      Input:=TSpinEditEx.Create(Owner);
      (Input as TSpinEditEx).MinValue:=Int64.MinValue;
      (Input as TSpinEditEx).MaxValue:=Int64.MaxValue;
    end;
    ttNumber,ttDouble:
    begin
      Input:=TFloatSpinEditEx.Create(Owner);
      (Input as TFloatSpinEditEx).MinValue:=Int64.MinValue;
      (Input as TFloatSpinEditEx).MaxValue:=Int64.MaxValue;
    end;
  end;    
  if BaseType=ttBoolean then
  begin
    Input:=TCheckBox.Create(Owner);
    Input.Parent:=Parent;
    (Input as TCheckBox).Caption:=Name;
    Input.Width:=188;
    Input.Height:=21;
    Input.Top:=top;
    Input.Left:=8;
  end
  else
  begin
    Input.Parent:=Parent;
    Input.Width:=92;
    Input.Height:=21;
    Input.Top:=top;
    Input.Left:=108;
  end;
  inc(top,26);
  if CanBeNil then
  begin
    IsNil:=TCheckBox.Create(Owner);
    IsNil.OnChange:=@UpdateInput;
    IsNil.Parent:=Parent;
    IsNil.Caption:=Name+' is nil';
    IsNil.Width:=188;
    IsNil.Height:=21;
    IsNil.Top:=top;
    IsNil.Left:=8;
    inc(top,26);
  end
  else
    IsNil:=nil;
end;

function TMsgPackValueEditor.GetMsgPack:IMsgPackObject;
begin
  Result:=nil;
  if Assigned(IsNil) and IsNil.Checked then
    Result:=TMsgPackObject.Create(mptNil)
  else
    case BaseType of
      ttUnsigned,ttInteger:
        Result:=TMsgPackObject.Create((Input as TSpinEditEx).Value);
      ttString:
        Result:=TMsgPackObject.Create((Input as TEdit).Text);
      ttNumber,ttDouble:
        Result:=TMsgPackObject.Create((Input as TFloatSpinEditEx).Value);
      ttBoolean:
        Result:=TMsgPackObject.Create((Input as TCheckBox).Checked);       
      ttVarbinary:
        raise Exception.Create('Varbinary type in indexes selection not support.');
      ttDecimal:
        raise Exception.Create('Decimal type in indexes selection not support.');
      ttArray:
        raise Exception.Create('Array type in indexes selection not support.');
      ttScalar:
        raise Exception.Create('Scalar type in indexes selection not support.');
    end;
end;

destructor TMsgPackValueEditor.Destroy;
begin
  FreeAndNil(Input);
  FreeAndNil(IsNil);
  FreeAndNil(&Label);
end;

//TMsgPackValueEditorWithInit
constructor TMsgPackValueEditorWithInit.Create(Owner,Parent:TWinControl;var top:integer;Name:String;ABaseType:TTType;CanBeNil:Boolean;InitValue:IMsgPackObject);
begin
  inherited Create(Owner,Parent,top,Name,ABaseType,CanBeNil);

  if(not CanBeNil)or(Assigned(InitValue)and(InitValue.GetObjectType<>mptNil))then
  begin
    case BaseType of
      ttString:
        (Input as TEdit).Text:=InitValue.AsString;
      ttUnsigned,ttInteger:
        (Input as TSpinEditEx).Value:=InitValue.AsInteger;
      ttNumber,ttDouble:
        (Input as TFloatSpinEditEx).Value:=InitValue.AsDouble;
      ttBoolean:
        (Input as TCheckBox).Checked:=InitValue.AsBoolean;
    end;
    if CanBeNil then
      IsNil.Checked:=False;
  end
  else   
    IsNil.Checked:=True;
end;







function BinToStr(B:RawByteString):String;
var
  i:Integer;
begin
  Result:='';
  for i:=1 to Length(B) do
    Result:=Result+IntToHex(ord(B[i]),2)+' ';
  if Length(B)>0 then
    SetLength(Result,Length(Result)-1);
end;

function MsgPackToStr(MPO:IMsgPackObject):String;
var
  i:Integer;
  MapIterator:TMsgPackMapIterator;
begin
  Result:='';
  case MPO.GetObjectType of
    mptNil:
      Result:='null';
    mptBoolean:
      Result:=BoolToStr(MPO.AsBoolean,'true','fasle');
    mptInteger:
      Result:=IntToStr(MPO.AsInteger);
    mptFloat,mptDouble:
      Result:=FloatToStr(MPO.AsDouble);
    mptString:
      Result:='"'+MPO.AsString+'"';
    mptBytes:
      Result:='"'+BinToStr(MPO.AsBytes)+'"';
    mptArray:
    begin
      Result:='[';
      for i:=0 to MPO.AsArray.Count-1 do
        Result:=Result+MsgPackToStr(MPO.AsArray.Get(i))+', ';
      if MPO.AsArray.Count>0 then
        SetLength(Result,Length(Result)-2);
      Result:=Result+']';
    end;
    mptMap:
    begin
      Result:='{';
      MPO.AsMap.IteratorInit(MapIterator);
      while MPO.AsMap.IteratorAdvance(MapIterator) do
        Result:=Result+MsgPackToStr(MapIterator.Key)+':'+MsgPackToStr(MapIterator.Value)+', ';
      if MPO.AsMap.Count>0 then
        SetLength(Result,Length(Result)-2);
      Result:=Result+'}';
    end;
  end;
end;

end.