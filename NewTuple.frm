object NewTupleForm: TNewTupleForm
  Left = 415
  Height = 240
  Top = 227
  Width = 213
  BorderStyle = bsDialog
  Caption = 'New Tuple'
  ClientHeight = 240
  ClientWidth = 213
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  Position = poMainFormCenter
  LCLVersion = '7.1'
  object CreateBtn: TButton
    Left = 128
    Height = 25
    Top = 200
    Width = 75
    Caption = 'Create'
    OnClick = CreateBtnClick
    TabOrder = 0
  end
  object CancelBtn: TButton
    Left = 8
    Height = 25
    Top = 200
    Width = 75
    Caption = 'Cancel'
    OnClick = CancelBtnClick
    TabOrder = 1
  end
end
