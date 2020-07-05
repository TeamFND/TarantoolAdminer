object NewSpaceForm: TNewSpaceForm
  Tag = -1
  Left = 415
  Height = 240
  Top = 227
  Width = 345
  BorderStyle = bsDialog
  Caption = 'NewSpace'
  ClientHeight = 240
  ClientWidth = 345
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  Position = poMainFormCenter
  LCLVersion = '7.1'
  object CancelBtn: TButton
    Left = 48
    Height = 25
    Top = 200
    Width = 75
    Caption = 'Cancel'
    OnClick = CancelBtnClick
    TabOrder = 0
  end
  object CreateBtn: TButton
    Left = 224
    Height = 25
    Top = 200
    Width = 75
    Caption = 'Create'
    OnClick = CreateBtnClick
    TabOrder = 1
  end
  object SpaceName: TEdit
    Left = 48
    Height = 21
    Top = 8
    Width = 104
    TabOrder = 2
    Text = 'NewSpace'
  end
  object Label1: TLabel
    Left = 8
    Height = 13
    Top = 12
    Width = 31
    Caption = 'Name:'
    ParentColor = False
  end
  object EngineSelector: TComboBox
    Left = 200
    Height = 21
    Top = 8
    Width = 140
    ItemHeight = 13
    ItemIndex = 0
    Items.Strings = (
      'memtx'
      'vinyl'
    )
    TabOrder = 3
    Text = 'memtx'
  end
  object Label2: TLabel
    Left = 160
    Height = 13
    Top = 12
    Width = 36
    Caption = 'Engine:'
    ParentColor = False
  end
  object AddFormatRecord: TButton
    Left = 5
    Height = 25
    Top = 160
    Width = 335
    Caption = 'Add Format Record'
    OnClick = AddFormatRecordClick
    TabOrder = 4
  end
  object Label3: TLabel
    Left = 48
    Height = 13
    Top = 36
    Width = 27
    Caption = 'Name'
    ParentColor = False
  end
  object Label4: TLabel
    Left = 144
    Height = 13
    Top = 36
    Width = 24
    Caption = 'Type'
    ParentColor = False
  end
  object Label5: TLabel
    Left = 240
    Height = 13
    Top = 32
    Width = 48
    Caption = 'Can Be Nil'
    ParentColor = False
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 1
    OnTimer = Timer1Timer
    Left = 98
    Top = 72
  end
end
