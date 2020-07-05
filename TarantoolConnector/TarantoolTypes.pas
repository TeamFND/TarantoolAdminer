unit TarantoolTypes;

{$mode delphi}

interface

uses
  Classes, SysUtils, Generics.Collections, msgpack;

type     
  TTIterator=(
    tiEQ,tiREQ,tiALL,
    tiLT,tiLE,
    tiGE,tiGT,
    tiBitsetAllSet,
    tiBitsetAnySet,
    tiBitsetAllNotSet,
    tiOverlaps,tiNeighbor);

  TTType=(
    ttUnsigned,
    ttString,
    ttVarbinary,
    ttInteger,
    ttNumber,
    ttDouble,
    ttBoolean,
    ttDecimal,
    ttArray,
    ttMap,
    ttScalar
  );

  TTIndexType=(
    titHASH,
    titTREE,
    titBITSET,
    RTREE
  );

  TTSpaceFormatRecord=record
    Name:String;
    &Type:TTType;   
    IsNullable:Boolean;
    constructor Create(ARecord:IMsgPackObject);
  end;
  TTSpaceFormat=TList<TTSpaceFormatRecord>;

  TTIndexFormatRecord=record
    FieldNum:Integer;
    &Type:TTType;
    IsNullable:Boolean;
    constructor Create(ARecord:IMsgPackObject);
  end;
  TTIndexFormat=TList<TTIndexFormatRecord>;

function StringToTTType(s:String):TTType;
function StringToTTIterator(s:String):TTIterator;

implementation

function StringToTTType(s:String):TTType;
begin         
  Result:=TTType.ttScalar;
  if s='unsigned' then
     Result:=TTType.ttUnsigned;
  if s='string' then
     Result:=TTType.ttString;
  if s='varbinary' then
     Result:=TTType.ttVarbinary;
  if s='integer' then
     Result:=TTType.ttInteger;
  if s='number' then
     Result:=TTType.ttNumber;
  if s='double' then
     Result:=TTType.ttDouble;
  if s='boolean' then
     Result:=TTType.ttBoolean;
  if s='decimal' then
     Result:=TTType.ttDecimal;
  if s='array' then
     Result:=TTType.ttArray;
  if s='map' then
     Result:=TTType.ttMap;
  if s='scalar' then
     Result:=TTType.ttScalar;
end;
        
function StringToTTIterator(s:String):TTIterator;
begin
  if s='EQ' then
    Result:=TTIterator.tiEQ;
  if s='REQ' then
    Result:=TTIterator.tiREQ;
  if s='ALL' then
    Result:=TTIterator.tiALL;
  if s='LT' then
    Result:=TTIterator.tiLT;
  if s='LE' then
    Result:=TTIterator.tiLE;
  if s='GE' then
    Result:=TTIterator.tiGE;
  if s='GT' then
    Result:=TTIterator.tiGT;
  if s='BitsetAllSet' then
    Result:=TTIterator.tiBitsetAllSet;
  if s='BitsetAnySet' then
    Result:=TTIterator.tiBitsetAnySet;
  if s='BitsetAllNotSet' then
    Result:=TTIterator.tiBitsetAllNotSet;
  if s='Overlaps' then
    Result:=TTIterator.tiOverlaps;
  if s='Neighbor' then
    Result:=TTIterator.tiNeighbor;
end;

constructor TTSpaceFormatRecord.Create(ARecord:IMsgPackObject);
const
  NameS:UnicodeString='name';
  TypeS:UnicodeString='type';   
  IsNullableS:UnicodeString='is_nullable';
var
  MPO:TMsgPackObject;
begin
  MPO:=TMsgPackObject.Create(NameS);
  Name:=ARecord.AsMap.GetEx(MPO).AsString;
  MPO.Free;           
  MPO:=TMsgPackObject.Create(TypeS);
  &Type:=StringToTTType(ARecord.AsMap.GetEx(MPO).AsString);
  MPO.Free;
  MPO:=TMsgPackObject.Create(IsNullableS);
  if Assigned(ARecord.AsMap.GetEx(MPO)) then
    IsNullable:=ARecord.AsMap.GetEx(MPO).AsBoolean;
  MPO.Free;
end;

constructor TTIndexFormatRecord.Create(ARecord:IMsgPackObject);
const
  FieldS:UnicodeString='field';
  TypeS:UnicodeString='type';
  IsNullableS:UnicodeString='is_nullable';
var
  MPO:TMsgPackObject;
begin
  if ARecord.ObjectType=mptArray then
  begin
    FieldNum:=ARecord.AsArray.Get(0).AsInteger;
    &Type:=StringToTTType(ARecord.AsArray.Get(1).AsString);
    IsNullable:=False;
  end
  else
  begin
    MPO:=TMsgPackObject.Create(FieldS);
    FieldNum:=ARecord.AsMap.GetEx(MPO).AsInteger;
    MPO.Free;
    MPO:=TMsgPackObject.Create(TypeS);
    &Type:=StringToTTType(ARecord.AsMap.GetEx(MPO).AsString);
    MPO.Free; 
    MPO:=TMsgPackObject.Create(IsNullableS);
    IsNullable:=ARecord.AsMap.GetEx(MPO).AsBoolean;
    MPO.Free;
  end;
end;

end.