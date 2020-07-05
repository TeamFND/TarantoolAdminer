object NewIndexForm: TNewIndexForm
  Left = 415
  Height = 379
  Top = 227
  Width = 320
  BorderStyle = bsDialog
  Caption = 'New Index'
  ClientHeight = 379
  ClientWidth = 320
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  Position = poMainFormCenter
  LCLVersion = '7.1'
  object CancelBtn: TButton
    Left = 48
    Height = 25
    Top = 344
    Width = 75
    Caption = 'Cancel'
    OnClick = CancelBtnClick
    TabOrder = 0
  end
  object CreateBtn: TButton
    Left = 200
    Height = 25
    Top = 344
    Width = 75
    Caption = 'Create'
    OnClick = CreateBtnClick
    TabOrder = 1
  end
  object IndexName: TEdit
    Left = 48
    Height = 21
    Top = 8
    Width = 200
    TabOrder = 2
    Text = 'NewIndex'
  end
  object Label1: TLabel
    Left = 8
    Height = 13
    Top = 12
    Width = 31
    Caption = 'Name:'
    ParentColor = False
  end
  object Label2: TLabel
    Left = 160
    Height = 1
    Top = 12
    Width = 1
    ParentColor = False
  end
  object AddFormatRecord: TButton
    Left = 216
    Height = 21
    Top = 40
    Width = 92
    Caption = 'Add Record'
    OnClick = AddFormatRecordClick
    TabOrder = 3
  end
  object RemoveFormatRecord: TButton
    Left = 8
    Height = 25
    Top = 312
    Width = 300
    Caption = 'Remove Last Record'
    OnClick = RemoveFormatRecordClick
    TabOrder = 4
  end
  object IsUnique: TCheckBox
    Left = 260
    Height = 17
    Top = 10
    Width = 51
    Caption = 'Unique'
    TabOrder = 5
  end
  object FieldSelector: TComboBox
    Left = 40
    Height = 21
    Top = 40
    Width = 88
    ItemHeight = 13
    OnChange = FieldSelectorChange
    TabOrder = 6
  end
  object Grid: TStringGrid
    Left = 8
    Height = 232
    Top = 72
    Width = 303
    AutoFillColumns = True
    ColCount = 2
    Columns = <    
      item
        Title.Caption = 'Field'
        Width = 149
      end    
      item
        Title.Caption = 'Title'
        Width = 150
      end>
    FixedCols = 0
    TabOrder = 7
    ColWidths = (
      149
      150
    )
  end
  object Label3: TLabel
    Left = 8
    Height = 13
    Top = 44
    Width = 22
    Caption = 'Field'
    ParentColor = False
  end
  object CanBeNil: TCheckBox
    Left = 136
    Height = 17
    Top = 42
    Width = 66
    Caption = 'Can Be Nil'
    TabOrder = 8
  end
end
