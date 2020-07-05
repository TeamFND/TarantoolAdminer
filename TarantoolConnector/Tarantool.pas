unit Tarantool;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, TarantoolConsts, TarantoolErrors, TarantoolTypes, IdTCPClient, base64, sha1, Generics.Collections, msgpack;

type
  TTCustomIndex=class;
  TTCustomSpace=class;
  TTSystemSpace=class;
  TT_SpaceSpace=class;  
  TT_IndexSpace=class;
  TTSpace=class;
  TTIndex=class;

  TTSpaces=TDictionary<String,TTSpace>;
  TTIndexes=TDictionary<String,TTIndex>;

  TTConnection=class
    private
      TCPClient:TIdTCPClient;
      ServerName:string;
      Port:Word;
      User,Password:string;
      CurrentUserID:Integer;
      TarantoolVersionString:string;
      _SpaceSpace:TT_SpaceSpace;
      FSpaces:TTSpaces;
      function GetTitleString:String;
      function GetSpaces:TTSpaces;
      function Request(Code:Integer;Body:TMsgPackObject;out OData:TMsgPackObject):Integer;
    public        
      property TitleString:string read GetTitleString;    
      property Spaces:TTSpaces read GetSpaces;

      constructor Create(const AServerName:string;APort:Word;const AUser,APassword:string);
      procedure CheckRequestErrors(Code:Integer;Data:TMsgPackObject);
      procedure HandShake();

      procedure ReloadSpaceList();

      function ReqSelect(Space,Index,Offset,Limit:Integer;Iterator:TTIterator;Key:IMsgPackObject):IMsgPackObject;  
      procedure ReqInsert(Space:Integer;Tuple:IMsgPackObject);
      function ReqEval(Code:String;Args:IMsgPackObject=nil):IMsgPackObject;
      procedure ReqReplace(Space:Integer;NewTuple:IMsgPackObject);         
      procedure ReqDelete(Space,Index:Integer;Key:IMsgPackObject);
      //function ReqCall(&function:String;Args:IMsgPackObject):IMsgPackObject;

      procedure NewSpace(Name,Engine:String;SFormat,IFormat:IMsgPackObject);

      destructor Destroy; override;
  end;

  TTCustomIndex=class abstract
    private
      Con:TTConnection;
    public
      constructor Create(ACon:TTConnection);
      function Select(Offset,Limit:Integer;Iterator:TTIterator;Key:IMsgPackObject):IMsgPackObject;overload;virtual;abstract;
      function Select(Iterator:TTIterator;Key:IMsgPackObject):IMsgPackObject;
      function Select(Offset,Limit:Integer):IMsgPackObject;
      function Select(Key:IMsgPackObject):IMsgPackObject;
      function SelectAll():IMsgPackObject;
  end;

  TTIndex=class (TTCustomIndex)
    private                 
      SpaceID:Integer;
      IndexID:Integer;
    public           
      Format:TTIndexFormat; 
      Name:String; 
      SpaceName:String;
      constructor Create(ACon:TTConnection;ASpaceName,AName:String;ARecord:IMsgPackObject);
      function Select(Offset,Limit:Integer;Iterator:TTIterator;Key:IMsgPackObject):IMsgPackObject;override;
      procedure Drop();
      destructor Destroy; override;
  end;

  TTCustomSpace=class abstract (TTCustomIndex)    
    private
    public
  end;

  TTSystemSpace=class abstract(TTCustomSpace)
    private
    public
  end;

  TT_SpaceSpace=class(TTSystemSpace)
    private
    public
      constructor Create(ACon:TTConnection);
      function Select(Offset,Limit:Integer;Iterator:TTIterator;Key:IMsgPackObject):IMsgPackObject;override;
  end;

  TT_IndexSpace=class(TTSystemSpace)
    private
    public
      constructor Create(ACon:TTConnection);
      function Select(Offset,Limit:Integer;Iterator:TTIterator;Key:IMsgPackObject):IMsgPackObject;override;
  end;

  TTSpace=class(TTCustomSpace)
    private          
      FSpaceID:Integer;
      PrimaryIndexID:Integer;
      _IndexSpace:TT_IndexSpace;  
      FIndexes:TTIndexes;
    public
      Format:TTSpaceFormat;
      Name:String;
      property Indexes:TTIndexes read FIndexes;     
      property SpaceID:Integer read FSpaceID;
      constructor Create(ACon:TTConnection;AName:String;ARecord:IMsgPackObject);
      procedure ReloadIndexList();
      procedure NewIndex(Name:String;Unique:Boolean;Format:IMsgPackObject);
      function Select(Offset,Limit:Integer;Iterator:TTIterator;Key:IMsgPackObject):IMsgPackObject;override;
      procedure Insert(Tuple:IMsgPackObject);
      procedure Replace(NewTuple:IMsgPackObject); 
      procedure Delete(Tuple:IMsgPackObject);
      procedure Drop();
      destructor Destroy; override;
  end;

implementation
               
//TTConnection
//privare

function TTConnection.GetTitleString:String;
begin
  Result:=User+'@'+ServerName+':'+IntToStr(Port);
end;

function TTConnection.GetSpaces:TTSpaces;
begin
  Result:=FSpaces;
end;

function TTConnection.Request(Code:Integer;Body:TMsgPackObject;out OData:TMsgPackObject):Integer;
var
  Head,Len,OHead,MPT:TMsgPackObject;
  LenStream,WriteStream,ReadStream:TStream;
  LastPos:Integer;
begin
  Head:=TMsgPackObject.Create(mptMap);
  try
    Head.AsMap.PutEx(TMsgPackObject.Create(IPROTO_REQUEST_TYPE),TMsgPackObject.Create(Code));
    Head.AsMap.PutEx(TMsgPackObject.Create(IPROTO_SYNC),TMsgPackObject.Create(0));

    WriteStream:=TMemoryStream.Create;
    try
      Head.Write(WriteStream);
      Body.Write(WriteStream);
      Len:=TMsgPackObject.Create(WriteStream.Size);

      try           
        LenStream:=TMemoryStream.Create;   
        try
          Len.Write(LenStream);
          with TCPClient.Socket do
          begin
            Write(LenStream);
            Write(WriteStream);
          end;

          ReadStream:=TMemoryStream.Create;
          try
            ReadStream.Size:=6;
            TCPClient.Socket.ReadStream(ReadStream,6);
            ReadStream.Position:=0;
            FreeAndNil(Len);
            Len:=TMsgPackObject.Parse(ReadStream);

            LastPos:=ReadStream.Position;
            ReadStream.Position:=6;
            TCPClient.Socket.ReadStream(ReadStream,Len.AsInteger+LastPos-6);
            ReadStream.Position:=LastPos;
            OHead:=TMsgPackObject.Parse(ReadStream);

            try
              if ReadStream.Position<>Len.AsInteger+6 then
                OData:=TMsgPackObject.Parse(ReadStream)
              else
                OData:=nil;
              MPT:=TMsgPackObject.Create(0);
              try
                Result:=OHead.AsMap.GetEx(MPT).AsInteger;
              finally
                FreeAndNil(MPT);
              end;
            finally
              FreeAndNil(OHead);
            end;
          finally
            FreeAndNil(ReadStream);
          end;
        finally   
          FreeAndNil(LenStream);
        end;
      finally
        FreeAndNil(Len);
      end;
    finally
      FreeAndNil(WriteStream);
    end;
  finally
    FreeAndNil(Head);
  end;
end;

//public

constructor TTConnection.Create(const AServerName:string;APort:Word;const AUser,APassword:string);
var
  MPO:TMsgPackObject;     
  IMPO:IMsgPackObject;
begin
  ServerName:=AServerName;
  Port:=APort;
  if AUser='' then
    User:='guest'
  else
    User:=AUser;
  Password:=APassword;
  TarantoolVersionString:='';

  TCPClient:=TIdTCPClient.Create(nil);
  TCPClient.Connect(ServerName,Port);
  HandShake();

  FSpaces:=TTSpaces.Create();
  _SpaceSpace:=TT_SpaceSpace.Create(self);

  ReloadSpaceList();

  with Spaces.GetEnumerator do
    try
      while MoveNext do
        if Current.Value.SpaceID=SPACE_USER then
        begin
          MPO:=TMsgPackObject.Create(mptArray);
          try
            MPO.AsArray.Add(TMsgPackObject.Create(Self.User));
            IMPO:=Current.Value.Indexes['name'].Select(tiEQ,MPO);
            CurrentUserID:=IMPO.AsArray.Get(0).AsArray.Get(0).AsInteger;
          finally
            Pointer(MPO):=nil;
          end;
          Break;
        end;
    finally  
      Free;
    end;
end;

procedure TTConnection.CheckRequestErrors(Code:Integer;Data:TMsgPackObject);
var
  IR,IR_index:IMsgPackObject;
begin
    if Code<IPROTO_REQUEST_TYPE_ERROR then
      exit
    else
    begin
      Code:=Code and(IPROTO_REQUEST_TYPE_ERROR-1);
      if Assigned(Data) then                            
        try
          IR_index:=TMsgPackObject.Create(IPROTO_ERROR);
          IR:=Data.AsMap.GetEx(IR_index);
          if Assigned(IR) and(IR.GetObjectType<>mptNil)then
            raise TarantoolException.Create(TTError(Code),IR.AsBytes)
          else
            raise TarantoolException.Create(TTError(Code));
        finally
          //FreeAndNil(IR_index);
        end
      else
        //case Code of
        //else
          raise TarantoolException.Create(TTError(Code));
        //end;
    end;
end;

procedure TTConnection.HandShake();
var
  Buff:array of Byte;
  Base64Salt,Salt:String;
  first,second,last:TSHA1Digest;
  SHAContext:TSHA1Context;
  i:byte;
  ReqData,ReqSubData,OData:TMsgPackObject;
  Scramble:RawByteString;
begin
  SetLength(Buff,0);
  TCPClient.Socket.ReadBytes(Buff,IPROTO_GREETING_SIZE);
  TarantoolVersionString:=PAnsiChar(@Buff[0]);
  if User<>'guest' then
  begin
    SetLength(Base64Salt,44);
    for i:=1 to 44 do
      Base64Salt[i]:=Char(Buff[63+i]);
    Salt:=DecodeStringBase64(Base64Salt);

    (*
    Auth
    salt = base64_decode(encoded_salt);
    step_1 = sha1(password);
    step_2 = sha1(step_1);
    step_3 = sha1(salt, step_2);
    scramble = xor(step_1, step_3);
    return scramble;
    *)
    first:=SHA1String(Password);    
    second:=SHA1Buffer(first,SizeOf(first));

    SHA1Init(SHAContext);
    SHA1Update(SHAContext, Salt[1], 20);
    SHA1Update(SHAContext, second, 20);
    SHA1Final(SHAContext, last);

    SetLength(Scramble,20);
    for i:=0 to 19 do
      Scramble[i+1]:=char(first[i] xor last[i]);

    ReqData:=TMsgPackObject.Create(mptMap);
    try
      ReqData.AsMap.PutEx(TMsgPackObject.Create(IPROTO_USER_NAME),TMsgPackObject.Create(User));
      ReqSubData:=TMsgPackObject.Create(mptArray);       
      ReqSubData.AsArray.Add(TMsgPackObject.Create('chap-sha1'));
      ReqSubData.AsArray.Add(TMsgPackObject.Create(Scramble));
      ReqData.AsMap.PutEx(TMsgPackObject.Create(IPROTO_TUPLE),ReqSubData);
                                                   
      try
        CheckRequestErrors(Request(IPROTO_REQUEST_TYPE_AUTHENTICATE,ReqData,OData),OData);
      finally            
        FreeAndNil(OData);
      end
    finally
      FreeAndNil(ReqData);
    end
  end;
end;

procedure TTConnection.ReloadSpaceList();   
var
  IMPO:IMsgPackObject;
  i:Integer;
begin
  with FSpaces.GetEnumerator do
  begin
    while MoveNext do
      Current.Value.Free;
    Free;
  end;
  FSpaces.Clear;
  IMPO:=_SpaceSpace.SelectAll();
  try
    with IMPO.AsArray do
      for i:=0 to Count-1 do
        FSpaces.Add(Get(i).AsArray.Get(2).AsString, TTSpace.Create(Self,Get(i).AsArray.Get(2).AsString,Get(i)));
  finally
    while IMPO._Release<>0 do;
    Pointer(IMPO):=nil;
  end;
end;

function TTConnection.ReqSelect(Space,Index,Offset,Limit:Integer;Iterator:TTIterator;Key:IMsgPackObject):IMsgPackObject;
var
  ReqData,OData,MPO:TMsgPackObject;
  IMPO:IMsgPackObject;
  IIterator:Integer;
begin
  ReqData:=TMsgPackObject.Create(mptMap);
  try
    ReqData.AsMap.PutEx(TMsgPackObject.Create(IPROTO_SPACE_ID),TMsgPackObject.Create(Space));
    ReqData.AsMap.PutEx(TMsgPackObject.Create(IPROTO_INDEX_ID),TMsgPackObject.Create(Index));
    ReqData.AsMap.PutEx(TMsgPackObject.Create(IPROTO_LIMIT),TMsgPackObject.Create(Limit));
    ReqData.AsMap.PutEx(TMsgPackObject.Create(IPROTO_OFFSET),TMsgPackObject.Create(Offset));
    case Iterator of
      tiEQ:IIterator:=ITERATOR_EQ;
      tiREQ:IIterator:=ITERATOR_REQ;
      tiALL:IIterator:=ITERATOR_ALL;
      tiLT:IIterator:=ITERATOR_LT;
      tiLE:IIterator:=ITERATOR_LE;
      tiGE:IIterator:=ITERATOR_GE;
      tiGT:IIterator:=ITERATOR_GT;
      tiBitsetAllSet:IIterator:=ITERATOR_BITSET_ALL_SET;
      tiBitsetAnySet:IIterator:=ITERATOR_BITSET_ANY_SET;
      tiBitsetAllNotSet:IIterator:=ITERATOR_BITSET_ALL_NOT_SET;
      tiOverlaps:IIterator:=ITERATOR_OVERLAPS;
      tiNeighbor:IIterator:=ITERATOR_NEIGHBOR;
    end;
    ReqData.AsMap.PutEx(TMsgPackObject.Create(IPROTO_ITERATOR),TMsgPackObject.Create(IIterator));
    if Assigned(Key) then
      ReqData.AsMap.PutEx(TMsgPackObject.Create(IPROTO_KEY),Key)
    else
      ReqData.AsMap.PutEx(TMsgPackObject.Create(IPROTO_KEY),TMsgPackObject.Create(mptArray));
    try
      CheckRequestErrors(Request(IPROTO_REQUEST_TYPE_SELECT,ReqData,OData),OData);
      MPO:=TMsgPackObject.Create(IPROTO_DATA);
      try
        IMPO:=OData.AsMap.GetEx(MPO);
      finally
        FreeAndNil(MPO);
      end;
      Result:=IMPO;
    finally
      FreeAndNil(OData);
    end
  finally
    FreeAndNil(ReqData);
  end
end;

procedure TTConnection.ReqInsert(Space:Integer;Tuple:IMsgPackObject);
var
  ReqData,OData:TMsgPackObject;
begin
  ReqData:=TMsgPackObject.Create(mptMap);
  try
    ReqData.AsMap.PutEx(TMsgPackObject.Create(IPROTO_SPACE_ID),TMsgPackObject.Create(Space));
    ReqData.AsMap.PutEx(TMsgPackObject.Create(IPROTO_TUPLE),Tuple);
    try
      CheckRequestErrors(Request(IPROTO_REQUEST_TYPE_INSERT,ReqData,OData),OData);
    finally
      FreeAndNil(OData);
    end
  finally
    FreeAndNil(ReqData);
  end
end;

function TTConnection.ReqEval(Code:String;Args:IMsgPackObject=nil):IMsgPackObject;     
var
  ReqData,OData:TMsgPackObject;
begin 
  ReqData:=TMsgPackObject.Create(mptMap);
  try
    ReqData.AsMap.PutEx(TMsgPackObject.Create(IPROTO_EXPR),TMsgPackObject.Create(Code));
    if Assigned(Args) then
      ReqData.AsMap.PutEx(TMsgPackObject.Create(IPROTO_TUPLE),Args)
    else
      ReqData.AsMap.PutEx(TMsgPackObject.Create(IPROTO_TUPLE),TMsgPackObject.Create(mptArray));
    CheckRequestErrors(Request(IPROTO_REQUEST_TYPE_EVAL,ReqData,OData),OData);
    Result:=OData;
    Pointer(OData):=nil;
  finally
    FreeAndNil(ReqData);
  end;
end;
           
procedure TTConnection.ReqReplace(Space:Integer;NewTuple:IMsgPackObject);
var
  ReqData,OData:TMsgPackObject;
begin
  ReqData:=TMsgPackObject.Create(mptMap);
  try
    ReqData.AsMap.PutEx(TMsgPackObject.Create(IPROTO_SPACE_ID),TMsgPackObject.Create(Space));
    ReqData.AsMap.PutEx(TMsgPackObject.Create(IPROTO_TUPLE),NewTuple);
    try
      CheckRequestErrors(Request(IPROTO_REQUEST_TYPE_REPLACE,ReqData,OData),OData);
    finally
      FreeAndNil(OData);
    end
  finally
    FreeAndNil(ReqData);
  end;
end;

procedure TTConnection.ReqDelete(Space,Index:Integer;Key:IMsgPackObject);
var
  ReqData,OData:TMsgPackObject;
begin
  ReqData:=TMsgPackObject.Create(mptMap);
  try
    ReqData.AsMap.PutEx(TMsgPackObject.Create(IPROTO_SPACE_ID),TMsgPackObject.Create(Space));   
    ReqData.AsMap.PutEx(TMsgPackObject.Create(IPROTO_INDEX_ID),TMsgPackObject.Create(Index));
    ReqData.AsMap.PutEx(TMsgPackObject.Create(IPROTO_KEY),Key);
    try
      CheckRequestErrors(Request(IPROTO_REQUEST_TYPE_DELETE,ReqData,OData),OData);
    finally
      FreeAndNil(OData);
    end
  finally
    FreeAndNil(ReqData);
  end;
end;

{function TTConnection.ReqCall(&function:String;Args:IMsgPackObject):IMsgPackObject;
var
  ReqData:TMsgPackObject;
begin
  ReqData:=TMsgPackObject.Create(mptMap);
  try
    ReqData.AsMap.PutEx(TMsgPackObject.Create(IPROTO_SPACE_ID),TMsgPackObject.Create(Space));
    ReqData.AsMap.PutEx(TMsgPackObject.Create(IPROTO_TUPLE),Tuple);
    CheckRequestErrors(Request(IPROTO_REQUEST_TYPE_INSERT,ReqData,OData),OData);
  finally
    FreeAndNil(ReqData);
  end;
end;}

procedure TTConnection.NewSpace(Name,Engine:String;SFormat,IFormat:IMsgPackObject);
var
  ReqData:TMsgPackObject;
begin               
  ReqData:=TMsgPackObject.Create(mptArray);
  try
    ReqData.AsArray.Add(SFormat);
    ReqData.AsArray.Add(IFormat);
    ReqEval('local SFormat,IFormat=...'+
      'local Space=box.schema.space.create("'+Name+'",{engine="'+Engine+'",format=SFormat})'+
      'Space:create_index("primary",{unique=true,parts=IFormat})',ReqData);      
    Pointer(ReqData):=nil;
  finally
    FreeAndNil(ReqData);
  end;
  ReloadSpaceList();
end;

destructor TTConnection.Destroy;
begin
  FreeAndNil(TCPClient);
  FreeAndNil(_SpaceSpace);
  if Assigned(FSpaces) then
    with FSpaces.GetEnumerator do
    begin
      while MoveNext do
        Current.Value.Free;
      Free;
    end;
  FreeAndNil(FSpaces);
end;

//TTCustomIndex
//public

constructor TTCustomIndex.Create(ACon:TTConnection);
begin
  Con:=ACon;
end;      

function TTCustomIndex.Select(Iterator:TTIterator;Key:IMsgPackObject):IMsgPackObject;
begin
  Result:=Select(0,$7fffffff,Iterator,Key);
end;

function TTCustomIndex.Select(Offset,Limit:Integer):IMsgPackObject;
begin   
  Result:=Select(Offset,Limit,TTIterator.tiALL,nil);
end;

function TTCustomIndex.Select(Key:IMsgPackObject):IMsgPackObject;
begin
  Result:=Select(0,$7fffffff,TTIterator.tiEQ,Key);
end;

function TTCustomIndex.SelectAll():IMsgPackObject;
begin
  Result:=Select(0,$7fffffff,TTIterator.tiALL,nil);
end;

//TTIndex
constructor TTIndex.Create(ACon:TTConnection;ASpaceName,AName:String;ARecord:IMsgPackObject);
var
  i:Integer;
begin
  inherited Create(ACon);
  Name:=AName;
  SpaceName:=ASpaceName;
  SpaceID:=ARecord.AsArray.Get(0).AsInteger;
  IndexID:=ARecord.AsArray.Get(1).AsInteger;
  Format:=TTIndexFormat.Create();
  with ARecord.AsArray.Get(5).AsArray do
    for i:=0 to Count-1 do
      Format.Add(TTIndexFormatRecord.Create(Get(i)));
end;

function TTIndex.Select(Offset,Limit:Integer;Iterator:TTIterator;Key:IMsgPackObject):IMsgPackObject;
begin
  Result:=Con.ReqSelect(SpaceID,IndexID,Offset,Limit,Iterator,Key);
end;

procedure TTIndex.Drop();
begin
  Con.ReqEval('box.space["'+SpaceName+'"].index["'+Name+'"]:drop()');
  Con.ReloadSpaceList();
end;

destructor TTIndex.Destroy;
begin         
  FreeAndNil(Format);
  inherited;
end;

//TT_SpaceSpace
constructor TT_SpaceSpace.Create(ACon:TTConnection);
begin
  inherited;
end;

function TT_SpaceSpace.Select(Offset,Limit:Integer;Iterator:TTIterator;Key:IMsgPackObject):IMsgPackObject;
begin
  Result:=Con.ReqSelect(SPACE_VSPACE,INDEX_SPACE_NAME,Offset,Limit,Iterator,Key);
end;

//TT_IndexSpace
constructor TT_IndexSpace.Create(ACon:TTConnection);
begin 
  inherited;
end;

function TT_IndexSpace.Select(Offset,Limit:Integer;Iterator:TTIterator;Key:IMsgPackObject):IMsgPackObject;
begin 
  Result:=Con.ReqSelect(SPACE_VINDEX,INDEX_INDEX_NAME,Offset,Limit,Iterator,Key);
end;

//TTSpace
constructor TTSpace.Create(ACon:TTConnection;AName:String;ARecord:IMsgPackObject);
var
  i:Integer;
begin
  inherited Create(ACon);
  Name:=AName;
  FSpaceID:=ARecord.AsArray.Get(0).AsInteger;
  Format:=TTSpaceFormat.Create();
  with ARecord.AsArray.Get(6).AsArray do
    for i:=0 to Count-1 do
      Format.Add(TTSpaceFormatRecord.Create(Get(i)));
  _IndexSpace:=TT_IndexSpace.Create(Con);
  FIndexes:=TTIndexes.Create();
  ReloadIndexList();
end;

procedure TTSpace.ReloadIndexList();
var
  MPO:TMsgPackObject;
  IMPO:IMsgPackObject;
  Name:String;
  i:Integer;
begin
  with FIndexes.GetEnumerator do
  begin
    while MoveNext do
      Current.Value.Free;
    Free;
  end;
  FIndexes.Clear;
  MPO:=TMsgPackObject.Create(mptArray);
  try
    MPO.AsArray.Add(TMsgPackObject.Create(FSpaceID));
    IMPO:=_IndexSpace.Select(MPO);
    try
      with IMPO.AsArray do
        for i:=0 to Count-1 do
        begin
          Name:=Get(i).AsArray.Get(2).AsString;
          FIndexes.Add(Name, TTIndex.Create(Con, Self.Name, Name, Get(i)));
          if Name='primary' then
            PrimaryIndexId:=Get(i).AsArray.Get(1).AsInteger;
        end;
    finally
      while IMPO._Release<>0 do;
      Pointer(IMPO):=nil;
    end;
  finally;
    Pointer(MPO):=nil;
  end
end;

procedure TTSpace.NewIndex(Name:String;Unique:Boolean;Format:IMsgPackObject);
begin
  Con.ReqEval('box.space["'+Self.Name+'"]:create_index("'+Name+'",{unique='+BoolToStr(Unique,'true','false')+',parts={...}})',Format);
  ReloadIndexList();
end;

function TTSpace.Select(Offset,Limit:Integer;Iterator:TTIterator;Key:IMsgPackObject):IMsgPackObject;
begin
  Result:=Con.ReqSelect(FSpaceID,PrimaryIndexId,Offset,Limit,Iterator,Key);
end;         

procedure TTSpace.Insert(Tuple:IMsgPackObject);
begin
  Con.ReqInsert(FSpaceID,Tuple);
end;

procedure TTSpace.Replace(NewTuple:IMsgPackObject);
begin
  Con.ReqReplace(FSpaceID,NewTuple);
end;

procedure TTSpace.Delete(Tuple:IMsgPackObject);
var
  Key:IMsgPackObject;
begin
  try
    Key:=TMsgPackObject.Create(mptArray);
    with Indexes['primary'].Format.GetEnumerator do
    begin
      while MoveNext do
        Key.AsArray.Add(Tuple.AsArray.Get(Current.FieldNum));
      Free;
    end;
    Key._AddRef;
    Con.ReqDelete(SpaceID,PrimaryIndexID,Key);
  finally                 
    while Tuple._Release<>0 do;
    Pointer(Tuple):=nil;   
    while Key._Release<>0 do;
    Pointer(Key):=nil;
  end;
end;

procedure TTSpace.Drop();
begin
  Con.ReqEval('box.space["'+Name+'"]:drop()');
  Con.ReloadSpaceList();
end;

destructor TTSpace.Destroy;
begin  
  inherited;
  FreeAndNil(Format);  
  FreeAndNil(_IndexSpace);
  with FIndexes.GetEnumerator do
  begin
    while MoveNext do
      Current.Value.Free;
    Free;
  end;
  FreeAndNil(FIndexes);
end;

end.