object Form2: TForm2
  Left = 415
  Height = 129
  Top = 227
  Width = 324
  BorderStyle = bsDialog
  Caption = 'NewConnection'
  ClientHeight = 129
  ClientWidth = 324
  OnCreate = FormCreate
  OnShow = FormShow
  Position = poMainFormCenter
  LCLVersion = '7.1'
  object Button1: TButton
    Left = 200
    Height = 25
    Top = 96
    Width = 75
    Caption = 'Connect'
    OnClick = Button1Click
    TabOrder = 0
  end
  object Button2: TButton
    Left = 48
    Height = 25
    Top = 96
    Width = 75
    Caption = 'Cancel'
    OnClick = Button2Click
    TabOrder = 1
  end
  object Address: TLabeledEdit
    Left = 8
    Height = 21
    Top = 24
    Width = 144
    EditLabel.Height = 13
    EditLabel.Width = 144
    EditLabel.Caption = 'Address'
    EditLabel.ParentColor = False
    TabOrder = 2
    Text = '127.0.0.1'
  end
  object Port: TSpinEdit
    Left = 160
    Height = 21
    Top = 24
    Width = 152
    MaxValue = 65535
    TabOrder = 3
    Value = 3301
  end
  object Label1: TLabel
    Left = 160
    Height = 13
    Top = 8
    Width = 20
    Caption = 'Port'
    ParentColor = False
  end
  object User: TComboBox
    Left = 9
    Height = 21
    Top = 63
    Width = 143
    ItemHeight = 13
    Items.Strings = (
      'admin'
      'guest'
    )
    TabOrder = 4
  end
  object Label2: TLabel
    Left = 8
    Height = 13
    Top = 48
    Width = 22
    Caption = 'User'
    ParentColor = False
  end
  object Password: TLabeledEdit
    Left = 160
    Height = 21
    Top = 63
    Width = 152
    EchoMode = emPassword
    EditLabel.Height = 13
    EditLabel.Width = 152
    EditLabel.Caption = 'Password'
    EditLabel.ParentColor = False
    PasswordChar = '*'
    TabOrder = 5
  end
end
