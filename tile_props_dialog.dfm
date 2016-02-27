object TilePropertiesDialog: TTilePropertiesDialog
  Left = 192
  Top = 114
  BorderStyle = bsDialog
  Caption = 'Tile properties'
  ClientHeight = 442
  ClientWidth = 793
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnMouseMove = FormMouseMove
  PixelsPerInch = 96
  TextHeight = 13
  object TilePropsImage: TImage
    Left = 312
    Top = 8
    Width = 320
    Height = 400
    OnMouseDown = TilePropsImageMouseDown
    OnMouseMove = TilePropsImageMouseMove
  end
  object lbTileIndex: TLabel
    Left = 312
    Top = 419
    Width = 29
    Height = 13
    Caption = 'Tile: 0'
  end
  object lbUsedAnimations: TLabel
    Left = 152
    Top = 419
    Width = 81
    Height = 13
    Caption = 'Used animations:'
  end
  object PaletteImage: TImage
    Left = 640
    Top = 144
    Width = 144
    Height = 144
    OnMouseDown = PaletteImageMouseDown
    OnMouseMove = PaletteImageMouseMove
  end
  object lbPaletteAnims: TLabel
    Left = 640
    Top = 8
    Width = 89
    Height = 13
    Caption = 'Palette animations:'
  end
  object lbPaletteAnimsNum: TLabel
    Left = 640
    Top = 116
    Width = 81
    Height = 13
    Caption = 'Used animations:'
  end
  object lbPaletteIndex: TLabel
    Left = 640
    Top = 296
    Width = 36
    Height = 13
    Caption = 'Color: 0'
  end
  object TilePropsGrid: TStringGrid
    Left = 8
    Top = 8
    Width = 297
    Height = 402
    ColCount = 4
    DefaultColWidth = 96
    DefaultRowHeight = 18
    RowCount = 37
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRowSelect, goThumbTracking]
    TabOrder = 0
    OnMouseMove = FormMouseMove
    OnSelectCell = TilePropsGridSelectCell
  end
  object cbHurtTilesMode: TCheckBox
    Left = 8
    Top = 417
    Width = 97
    Height = 17
    Caption = 'Edit hurting tiles'
    TabOrder = 1
    OnClick = cbHurtTilesModeClick
  end
  object seUsedAnimations: TSpinEdit
    Left = 240
    Top = 415
    Width = 65
    Height = 22
    MaxValue = 15
    MinValue = 0
    TabOrder = 2
    Value = 0
    OnChange = seUsedAnimationsChange
  end
  object lstPaletteAnims: TListBox
    Left = 640
    Top = 24
    Width = 145
    Height = 81
    ItemHeight = 13
    TabOrder = 3
    OnClick = lstPaletteAnimsClick
  end
  object sePaletteAnims: TSpinEdit
    Left = 728
    Top = 112
    Width = 57
    Height = 22
    MaxValue = 5
    MinValue = 0
    TabOrder = 4
    Value = 0
    OnChange = sePaletteAnimsChange
  end
end
