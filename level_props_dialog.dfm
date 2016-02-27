object LevelPropertiesDialog: TLevelPropertiesDialog
  Left = 192
  Top = 114
  BorderStyle = bsDialog
  Caption = 'Level properties'
  ClientHeight = 576
  ClientWidth = 881
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
  object gbUsedSprites: TGroupBox
    Left = 8
    Top = 8
    Width = 617
    Height = 185
    Caption = ' Sprite properties '
    TabOrder = 0
    object lstSpriteList: TListBox
      Left = 8
      Top = 24
      Width = 121
      Height = 153
      ItemHeight = 13
      TabOrder = 0
      OnClick = lstSpriteListClick
    end
    object btnClearMonster: TButton
      Left = 528
      Top = 12
      Width = 81
      Height = 25
      Caption = 'Clear monster'
      TabOrder = 1
    end
  end
  object ValueListEditor1: TValueListEditor
    Left = 208
    Top = 8
    Width = 201
    Height = 561
    DefaultColWidth = 75
    TabOrder = 1
    OnStringsChange = ValueListEditor1StringsChange
    ColWidths = (
      75
      120)
  end
  object ValueListEditor2: TValueListEditor
    Left = 424
    Top = 8
    Width = 201
    Height = 561
    DefaultColWidth = 75
    TabOrder = 2
    OnStringsChange = ValueListEditor2StringsChange
    ColWidths = (
      75
      120)
  end
  object ValueListEditor3: TValueListEditor
    Left = 8
    Top = 199
    Width = 161
    Height = 370
    DefaultColWidth = 75
    TabOrder = 3
    OnStringsChange = ValueListEditor3StringsChange
    ColWidths = (
      75
      80)
  end
  object ValueListEditor4: TValueListEditor
    Left = 640
    Top = 7
    Width = 201
    Height = 561
    DefaultColWidth = 75
    TabOrder = 4
    OnStringsChange = ValueListEditor4StringsChange
    ColWidths = (
      75
      120)
  end
end
