object SpritePropertiesDialog: TSpritePropertiesDialog
  Left = 192
  Top = 114
  BorderStyle = bsDialog
  Caption = 'Sprite properties'
  ClientHeight = 433
  ClientWidth = 521
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object lbSpriteList: TLabel
    Left = 8
    Top = 6
    Width = 45
    Height = 13
    Caption = 'Sprite list:'
  end
  object lbSpriteData: TLabel
    Left = 352
    Top = 6
    Width = 54
    Height = 13
    Caption = 'Sprite data:'
  end
  object lbBehavior: TLabel
    Left = 136
    Top = 172
    Width = 45
    Height = 13
    Caption = 'Behavior:'
  end
  object lbKind: TLabel
    Left = 136
    Top = 204
    Width = 24
    Height = 13
    Caption = 'Kind:'
  end
  object lbItemType: TLabel
    Left = 136
    Top = 236
    Width = 46
    Height = 13
    Caption = 'Item type:'
  end
  object lbFileName: TLabel
    Left = 136
    Top = 28
    Width = 48
    Height = 13
    Caption = 'File name:'
  end
  object lbWidth: TLabel
    Left = 136
    Top = 116
    Width = 31
    Height = 13
    Caption = 'Width:'
  end
  object lbHeight: TLabel
    Left = 136
    Top = 140
    Width = 34
    Height = 13
    Caption = 'Height:'
  end
  object lbNumSprites: TLabel
    Left = 136
    Top = 60
    Width = 85
    Height = 13
    Caption = 'Number of sprites:'
  end
  object lbFirstSpriteIndex: TLabel
    Left = 136
    Top = 84
    Width = 78
    Height = 13
    Caption = 'First sprite index:'
  end
  object lbChildSprite: TLabel
    Left = 136
    Top = 292
    Width = 54
    Height = 13
    Caption = 'Child sprite:'
  end
  object lbChildSprite2: TLabel
    Left = 136
    Top = 324
    Width = 63
    Height = 13
    Caption = 'Child sprite 2:'
  end
  object vlSpriteData: TValueListEditor
    Left = 352
    Top = 23
    Width = 161
    Height = 402
    DefaultColWidth = 75
    TabOrder = 0
    TitleCaptions.Strings = (
      'Byte'
      'Value')
    OnStringsChange = vlSpriteDataStringsChange
    ColWidths = (
      75
      80)
  end
  object lstSpriteList: TListBox
    Left = 8
    Top = 24
    Width = 121
    Height = 401
    ItemHeight = 13
    TabOrder = 1
    OnClick = lstSpriteListClick
  end
  object cbxBehavior: TComboBox
    Left = 200
    Top = 168
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 2
    OnChange = SpritePropertyChange
  end
  object cbxKind: TComboBox
    Left = 200
    Top = 200
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 3
    Text = '0 - '
    OnChange = SpritePropertyChange
    Items.Strings = (
      '0 - '
      '1 - '
      '2 - Monster'
      '3 - Hazard / Projectile'
      '4 - Hazard / Projectile'
      '5 - Pick-up item'
      '6 - Exit'
      '7 - '
      '8 - '
      '9 - '
      '10 - Decoration'
      '11 - Stand-on')
  end
  object cbxItemType: TComboBox
    Left = 200
    Top = 232
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 4
    Text = '0 - Weapon'
    OnChange = SpritePropertyChange
    Items.Strings = (
      '0 - Weapon'
      '1 - Healing'
      '2 -'
      '3 - Key'
      '4 -'
      '5 - Checkpoint')
  end
  object edFileName: TEdit
    Left = 200
    Top = 24
    Width = 145
    Height = 21
    MaxLength = 12
    TabOrder = 5
    OnChange = edFileNameChange
  end
  object seWidth: TSpinEdit
    Left = 200
    Top = 112
    Width = 65
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 6
    Value = 0
    OnChange = SpritePropertyChange
  end
  object seHeight: TSpinEdit
    Left = 200
    Top = 136
    Width = 65
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 7
    Value = 0
    OnChange = SpritePropertyChange
  end
  object seNumSprites: TSpinEdit
    Left = 232
    Top = 56
    Width = 65
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 8
    Value = 0
    OnChange = SpritePropertyChange
  end
  object seFirstSpriteIndex: TSpinEdit
    Left = 232
    Top = 80
    Width = 65
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 9
    Value = 0
    OnChange = SpritePropertyChange
  end
  object cbFullHealth: TCheckBox
    Left = 200
    Top = 264
    Width = 97
    Height = 17
    Caption = 'Gives full health'
    TabOrder = 10
    OnClick = SpritePropertyChange
  end
  object cbxChildSprite: TComboBox
    Left = 200
    Top = 288
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 11
    OnChange = SpritePropertyChange
  end
  object btnCopy: TButton
    Left = 136
    Top = 400
    Width = 65
    Height = 25
    Caption = 'Copy'
    TabOrder = 12
    OnClick = btnCopyClick
  end
  object btnPaste: TButton
    Left = 208
    Top = 400
    Width = 65
    Height = 25
    Caption = 'Paste'
    TabOrder = 13
    OnClick = btnPasteClick
  end
  object btnClear: TButton
    Left = 280
    Top = 400
    Width = 65
    Height = 25
    Caption = 'Clear'
    TabOrder = 14
    OnClick = btnClearClick
  end
  object cbxChildSprite2: TComboBox
    Left = 200
    Top = 320
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 15
    OnChange = SpritePropertyChange
  end
end
