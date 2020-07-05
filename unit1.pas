unit Unit1;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, Menus,
  ExtCtrls, StdCtrls, Spin, Grids, ECTabCtrl, ECTypes,
  Tarantool, TarantoolTypes, Generics.Collections, Unit2, NewSpace, NewIndex,
  msgpack, Utils,
  NewTuple;

type

  { TForm1 }

  TForm1 = class(TForm)
    NewTuple: TButton;
    EditTuple: TButton;
    DeleteTuple: TButton;
    DeleteCurrentIndex: TButton;
    GroupBox2: TGroupBox;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    NewIndexBtn: TButton;
    DeleteCurrentSpace: TButton;
    Panel2: TPanel;
    PopupMenu1: TPopupMenu;
    Select: TButton;
    Label3: TLabel;
    Paging: TCheckBox;
    IteratorSelector: TComboBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    GroupBox6: TGroupBox;
    IndexList: TListBox;
    Label1: TLabel;
    Label2: TLabel;
    MenuItem2: TMenuItem;
    ShowSystemSpaces: TMenuItem;
    ReloadSpaces: TButton;
    NewSpace: TButton;
    ECTabCtrl1: TECTabCtrl;
    GroupBox1: TGroupBox;
    Image1: TImage;
    SpacesList: TListBox;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    Panel1: TPanel;
    IndexFields: TSpinEdit;
    SpaceData: TStringGrid;
    ItemsPerPage: TSpinEdit;
    PageNumber: TSpinEdit;
    TaskDialog1: TTaskDialog;
    procedure ReloadSpaceData();
    procedure ReloadIndexFormat();
    procedure ReloadPaggingParams();
    procedure ReloadIndexList();
    procedure ReloadConnectionTab();
                              
    procedure FormCreate(Sender: TObject);

    procedure NewSpaceClick(Sender: TObject);
    procedure DeleteCurrentSpaceClick(Sender: TObject);     
    procedure NewIndexBtnClick(Sender: TObject);
    procedure DeleteCurrentIndexClick(Sender: TObject);
    procedure NewTupleClick(Sender: TObject);       
    procedure EditTupleClick(Sender: TObject);
    procedure DeleteTupleClick(Sender: TObject);

    procedure ECTabCtrl1Change(Sender: TObject);
    procedure ECTabCtrl1CloseQuery(Sender: TObject; AIndex: Integer;
      var CanClose: Boolean);
    procedure IndexFieldsChange(Sender: TObject);
    procedure IndexListClick(Sender: TObject);
    procedure IteratorSelectorChange(Sender: TObject);
    procedure PageNumberChange(Sender: TObject);
    procedure PagingChange(Sender: TObject);
    procedure ECTabCtrl1Add(Sender: TObject; AIndex: Integer);
    procedure FormDestroy(Sender: TObject);
    procedure ReloadSpacesClick(Sender: TObject);
    procedure SelectClick(Sender: TObject);
    procedure ShowSystemSpacesClick(Sender: TObject);
    procedure SpacesListClick(Sender: TObject);
    procedure ItemsPerPageChange(Sender: TObject);
  private
    Connections:TList<TTConnection>;
    IndexFormatControls:TList<TMsgPackValueEditor>;  
    MPOSpaceData:IMsgPackObject;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.frm}

{ TForm1 }

procedure TForm1.ReloadSpaceData();
var
  MPO:TMsgPackObject;
  LIMPO:TMsgPackArray;
  i,i0:Integer;
begin
  if Assigned(MPOSpaceData)then
     while MPOSpaceData._Release<>0 do;
  Pointer(MPOSpaceData):=nil;
  if GroupBox3.Enabled then
    try
      MPO:=TMsgPackObject.Create(mptArray);
      try
       with Connections[ECTabCtrl1.TabIndex].Spaces[SpacesList.GetSelectedText].Indexes[IndexList.GetSelectedText] do
        begin
          for i:=0 to IndexFields.Value-1 do
            MPO.AsArray.Add(IndexFormatControls[i].GetMsgPack);
          if Paging.Checked then
            MPOSpaceData:=Select(PageNumber.Value*ItemsPerPage.Value,(PageNumber.Value+1)*ItemsPerPage.Value,StringToTTIterator(IteratorSelector.Text),MPO)
          else
            MPOSpaceData:=Select(StringToTTIterator(IteratorSelector.Text),MPO);
          while SpaceData.RowCount>1 do
            SpaceData.DeleteRow(1);
          for i:=0 to MPOSpaceData.AsArray.Count-1 do
          begin
            SpaceData.InsertRowWithValues(1,[]);
            LIMPO:=MPOSpaceData.AsArray.Get(i).AsArray;
            try
              for i0:=0 to LIMPO.Count-1 do
                if i0<Connections[ECTabCtrl1.TabIndex].Spaces[SpacesList.GetSelectedText].Format.Count then
                  if LIMPO.Get(i0).GetObjectType=mptNil then
                    SpaceData.Cells[i0,1]:='null'
                  else
                    case Connections[ECTabCtrl1.TabIndex].Spaces[SpacesList.GetSelectedText].Format[i0].&Type of
                      ttUnsigned,ttInteger:
                        SpaceData.Cells[i0,1]:=IntToStr(LIMPO.Get(i0).AsInteger);
                      ttNumber,ttDouble:
                        SpaceData.Cells[i0,1]:=FloatToStr(LIMPO.Get(i0).AsDouble);
                      ttString:
                        SpaceData.Cells[i0,1]:=LIMPO.Get(i0).AsString;
                      ttBoolean:
                        SpaceData.Cells[i0,1]:=BoolToStr(LIMPO.Get(i0).AsBoolean,'true','fasle');
                      ttVarbinary:
                        SpaceData.Cells[i0,1]:=BinToStr(LIMPO.Get(i0).AsBytes);
                      ttArray,ttMap,ttScalar,ttDecimal:
                        SpaceData.Cells[i0,1]:=MsgPackToStr(LIMPO.Get(i0));
                    end
                else
                begin
                  if i0>=SpaceData.ColCount then
                    with SpaceData.Columns.Add do
                      Title.Caption:='';
                  SpaceData.Cells[i0,1]:=MsgPackToStr(LIMPO.Get(i0));
                end;
            finally
              Pointer(LIMPO):=nil;
            end;
          end;
          SpaceData.AutoSizeColumns;
        end;
      finally
        Pointer(MPO):=nil;
      end;
    except      
      on E : Exception do
        ShowMessage(E.Message);
    end;
end;

procedure TForm1.ReloadIndexFormat();
var
  TabIndex,i,Num:Integer;
  Space:TTSpace;
  Index:TTIndex;
begin
  TabIndex:=ECTabCtrl1.TabIndex;
  GroupBox3.Enabled:=(SpacesList.ItemIndex>=0)and(Connections[TabIndex].Spaces[SpacesList.GetSelectedText].Indexes.Count>0);
  GroupBox4.Enabled:=GroupBox3.Enabled;
  Num:=4+4*26;
  with IndexFormatControls.GetEnumerator do
  begin
    while MoveNext do
      Current.Free;
    Free;
  end;
  IndexFormatControls.Clear;
  IndexFields.Enabled:=IteratorSelector.Text<>'ALL';
  if not IndexFields.Enabled then
    IndexFields.Value:=0;
  if GroupBox3.Enabled then
  begin
    Space:=Connections[TabIndex].Spaces[SpacesList.GetSelectedText];
    Index:=Space.Indexes[IndexList.GetSelectedText];
    IndexFields.MaxValue:=Index.Format.Count;
    with Index.Format.GetEnumerator do
    begin
      i:=0;
      while MoveNext and(i<IndexFields.Value)do
      begin
        IndexFormatControls.Add(TMsgPackValueEditor.Create(self,GroupBox6,Num,Space.Format[Current.FieldNum].Name,Current.&Type,Current.IsNullable));
        inc(i);
      end;
      Free;
    end;
    Invalidate;
  end;
  if(IndexList.ItemIndex<0)and(IndexList.Count>0)then
    IndexList.ItemIndex:=0;
  DeleteCurrentIndex.Enabled:=(IndexList.ItemIndex>=0)and(IndexList.GetSelectedText<>'primary');
end;

procedure TForm1.ReloadPaggingParams();
begin
  PageNumber.Enabled:=Paging.Checked;
  ItemsPerPage.Enabled:=Paging.Checked;
end;

procedure TForm1.ReloadIndexList();
var
  TabIndex:Integer;
  Space:TTSpace;
begin
  TabIndex:=ECTabCtrl1.TabIndex;
  GroupBox3.Enabled:=SpacesList.ItemIndex>=0;
  GroupBox4.Enabled:=GroupBox3.Enabled;
  IndexList.Clear;
  SpaceData.Columns.Clear;
  if GroupBox3.Enabled then
  begin
    Space:=Connections[TabIndex].Spaces[SpacesList.GetSelectedText];
    with Space.Indexes.GetEnumerator do
    begin
      while MoveNext do
        IndexList.Items.Add(Current.Key);
      Free;
    end;
    with Space.Format.GetEnumerator do
    begin
      while MoveNext do
        with SpaceData.Columns.Add do
          Title.Caption:=Current.Name;
      Free;
    end;
  end;
  if(IndexList.ItemIndex<0)and(IndexList.Count>0)then
    IndexList.ItemIndex:=0;
  ReloadIndexFormat();
  ReloadSpaceData();
end;

procedure TForm1.ReloadConnectionTab();
var
  Index:Integer;
begin
  Index:=ECTabCtrl1.TabIndex;
  SpacesList.Clear;
  Panel1.Visible:=(Index>=0)and(Index<Connections.Count);
  if Panel1.Visible then
    with Connections[Index].Spaces.GetEnumerator do
    begin
      while MoveNext do
        if(Current.Key[1]<>'_')or(ShowSystemSpaces.Checked)then
          SpacesList.Items.Add(Current.Key);
      Free;
    end;
  if(SpacesList.ItemIndex<0)and(SpacesList.Count>0)then
    SpacesList.ItemIndex:=0;
  DeleteCurrentSpace.Enabled:=SpacesList.ItemIndex>=0;
  ReloadIndexList();
end;






procedure TForm1.FormCreate(Sender: TObject);
begin
  Connections:=TList<TTConnection>.Create();
  IndexFormatControls:=TList<TMsgPackValueEditor>.Create();
  Panel1.Visible:=False;                         
  Pointer(MPOSpaceData):=nil;
end;







procedure TForm1.NewSpaceClick(Sender: TObject);
begin
  NewSpaceForm.ShowModal;
  if NewSpaceForm.DoNew then
  begin
    try
      with NewSpaceForm do
        Connections[ECTabCtrl1.TabIndex].NewSpace(SpaceName.Text,EngineSelector.Text,AsMsgPack(),NewIndexForm.AsMsgPack());
    except
      on E : Exception do
        ShowMessage(E.Message);
    end;
    ReloadConnectionTab();
  end;
end;

procedure TForm1.DeleteCurrentSpaceClick(Sender: TObject);
begin
  TaskDialog1.Caption:='Space deletion';
  TaskDialog1.Text:='Are you sure want to delete this space?';
  if TaskDialog1.Execute and(TaskDialog1.ModalResult=mrYes)then
  begin
    try
      Connections[ECTabCtrl1.TabIndex].Spaces[SpacesList.GetSelectedText].Drop();
    except
      on E : Exception do
        ShowMessage(E.Message);
    end;
    ReloadConnectionTab();
  end;
end;

procedure TForm1.NewIndexBtnClick(Sender: TObject);
var
  Space:TTSpace;
begin
  Space:=Connections[ECTabCtrl1.TabIndex].Spaces[SpacesList.GetSelectedText];
  NewIndexForm.Primary:=False;
  NewIndexForm.SpaceFormat.Clear;
  with Space.Format.GetEnumerator do
  begin
    while MoveNext do
      NewIndexForm.SpaceFormat.Add(Current);
    Free;
  end;
  NewIndexForm.ShowModal;
  if NewIndexForm.DoNew then
  begin
    try
      with NewIndexForm do
        Space.NewIndex(IndexName.Text,IsUnique.Checked,AsMsgPack());
    except
      on E : Exception do
        ShowMessage(E.Message);
    end;
    ReloadIndexList();
  end;
end;                  

procedure TForm1.DeleteCurrentIndexClick(Sender: TObject);
begin
  TaskDialog1.Caption:='Index deletion';
  TaskDialog1.Text:='Are you sure want to delete this index?';
  if TaskDialog1.Execute and(TaskDialog1.ModalResult=mrYes)then
  begin
    try
      Connections[ECTabCtrl1.TabIndex].Spaces[SpacesList.GetSelectedText].Indexes[IndexList.GetSelectedText].Drop();
    except
      on E : Exception do
        ShowMessage(E.Message);
    end;
    ReloadIndexList();
  end;
end;

procedure TForm1.NewTupleClick(Sender: TObject);
begin
  NewTupleForm.Format:=Connections[ECTabCtrl1.TabIndex].Spaces[SpacesList.GetSelectedText].Format;
  NewTupleForm.Edit:=False;
  NewTupleForm.ShowModal;
  if NewTupleForm.DoNew then
  begin
    try
      Connections[ECTabCtrl1.TabIndex].Spaces[SpacesList.GetSelectedText].Insert(NewTupleForm.AsMsgPack);
    except
      on E : Exception do
        ShowMessage(E.Message);
    end;
    ReloadSpaceData();
  end;
end;

procedure TForm1.EditTupleClick(Sender: TObject);
begin        
  NewTupleForm.Format:=Connections[ECTabCtrl1.TabIndex].Spaces[SpacesList.GetSelectedText].Format;
  NewTupleForm.Edit:=True;
  NewTupleForm.DataRow:=MPOSpaceData.AsArray.Get(SpaceData.SelectedRange[0].Top-1);
  NewTupleForm.ShowModal;         
  if NewTupleForm.DoNew then
  begin
    try
      Connections[ECTabCtrl1.TabIndex].Spaces[SpacesList.GetSelectedText].Replace(NewTupleForm.AsMsgPack);
    except
      on E : Exception do
        ShowMessage(E.Message);
    end;
    ReloadSpaceData();
  end;
end;

procedure TForm1.DeleteTupleClick(Sender: TObject);
begin
  try
    Connections[ECTabCtrl1.TabIndex].Spaces[SpacesList.GetSelectedText].Delete(TMsgPackObject.Parse(MPOSpaceData.AsArray.Get(SpaceData.SelectedRange[0].Top-1).AsMsgPack()));
    except
      on E : Exception do
        ShowMessage(E.Message);
    end;
  ReloadIndexList();
end;















procedure TForm1.ECTabCtrl1CloseQuery(Sender: TObject; AIndex: Integer;
  var CanClose: Boolean);
begin
  if AIndex<Connections.Count then
  begin
    Connections[AIndex].Free;
    Connections.Delete(AIndex);
  end;
  CanClose:=True;
end;

procedure TForm1.IndexFieldsChange(Sender: TObject);
begin
  ReloadIndexFormat();
end;

procedure TForm1.IndexListClick(Sender: TObject);
begin
  ReloadIndexFormat();
end;

procedure TForm1.IteratorSelectorChange(Sender: TObject);
begin     
  ReloadIndexFormat();
end;

procedure TForm1.PageNumberChange(Sender: TObject);
begin
  ReloadPaggingParams();
end;

procedure TForm1.PagingChange(Sender: TObject);
begin
  ReloadPaggingParams();
end;

procedure TForm1.ECTabCtrl1Change(Sender: TObject);
begin
  if ECTabCtrl1.Tabs.Count=Connections.Count then
    ReloadConnectionTab();
end;

procedure TForm1.ECTabCtrl1Add(Sender: TObject; AIndex: Integer);
begin              
  ECTabCtrl1.Tabs[AIndex].Text:='Creating...';
  Form2.ShowModal;
  if Form2.DoConnection then   
  begin         
    try
      with Form2 do
        Connections.Insert(AIndex,TTConnection.Create(Address.Text,Port.Value,User.Caption,Password.Text));
      ECTabCtrl1.Tabs[AIndex].Text:=Connections[AIndex].TitleString;
      ECTabCtrl1.Tabs[AIndex].Options:=[etoCloseable,etoCloseBtn,etoVisible];
    except
      on E : Exception do
      begin
        ShowMessage(E.Message);
        ECTabCtrl1.DeleteTab(AIndex);
      end;
    end;
  end
  else
    ECTabCtrl1.DeleteTab(AIndex);
  ReloadConnectionTab();
end;

procedure TForm1.FormDestroy(Sender: TObject);
var
  i:Integer;
begin            
  if Assigned(MPOSpaceData)then
     while MPOSpaceData._Release<>0 do;
  Pointer(MPOSpaceData):=nil;
  for i:=0 to Connections.Count-1 do
    Connections[i].Free;
  Connections.Clear;
  FreeAndNil(Connections);
  with IndexFormatControls.GetEnumerator do
  begin
    while MoveNext do
      Current.Free;
    Free;
  end;
  FreeAndNil(IndexFormatControls);
end;

procedure TForm1.ReloadSpacesClick(Sender: TObject);
begin
  try
    Connections[ECTabCtrl1.TabIndex].ReloadSpaceList();
  except
    on E : Exception do
      ShowMessage(E.Message);
  end;
  ReloadConnectionTab();
end;

procedure TForm1.SelectClick(Sender: TObject);
begin
  ReloadSpaceData();
end;

procedure TForm1.ShowSystemSpacesClick(Sender: TObject);
begin
  ShowSystemSpaces.Checked:=not ShowSystemSpaces.Checked;
  ReloadConnectionTab();
end;

procedure TForm1.SpacesListClick(Sender: TObject);
begin      
  ReloadIndexList();
end;

procedure TForm1.ItemsPerPageChange(Sender: TObject);
begin
  ReloadPaggingParams();
end;

end.