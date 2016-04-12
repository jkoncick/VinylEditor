object MainWindow: TMainWindow
  Left = 191
  Top = 71
  Width = 950
  Height = 676
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  Caption = 'Vinyl Goddess from Mars Level Editor'
  Color = clBtnFace
  Constraints.MinHeight = 544
  Constraints.MinWidth = 528
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  Menu = AppMenu
  OldCreateOrder = False
  Visible = True
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnDeactivate = FormDeactivate
  OnKeyDown = FormKeyDown
  OnMouseWheelDown = FormMouseWheelDown
  OnMouseWheelUp = FormMouseWheelUp
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object MapCanvas: TImage
    Left = 4
    Top = 4
    Width = 1
    Height = 1
    OnDblClick = MapCanvasDblClick
    OnMouseDown = MapCanvasMouseDown
    OnMouseMove = MapCanvasMouseMove
    OnMouseUp = MapCanvasMouseUp
  end
  object CursorImage: TImage
    Left = 4
    Top = 4
    Width = 1
    Height = 1
    Visible = False
    OnMouseDown = CursorImageMouseDown
    OnMouseMove = CursorImageMouseMove
    OnMouseUp = MapCanvasMouseUp
  end
  object MapScrollH: TScrollBar
    Left = 4
    Top = 456
    Width = 480
    Height = 16
    LargeChange = 4
    PageSize = 0
    TabOrder = 0
    OnChange = MapScrollChange
  end
  object MapScrollV: TScrollBar
    Left = 488
    Top = 4
    Width = 16
    Height = 448
    Kind = sbVertical
    LargeChange = 4
    PageSize = 0
    TabOrder = 1
    OnChange = MapScrollChange
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 603
    Width = 942
    Height = 19
    Panels = <
      item
        Width = 80
      end
      item
        Width = 80
      end
      item
        Text = 'Tileset name'
        Width = 70
      end
      item
        Width = 64
      end
      item
        Text = 'No map loaded'
        Width = 180
      end
      item
        Width = 250
      end
      item
        Width = 100
      end
      item
        Text = 'alpha 0.5'
        Width = 0
      end>
  end
  object EditorMenu: TPanel
    Left = 512
    Top = 0
    Width = 280
    Height = 601
    TabOrder = 3
    object MiniMapFrame: TBevel
      Left = 6
      Top = 6
      Width = 268
      Height = 140
      Shape = bsFrame
      Style = bsRaised
    end
    object MiniMap: TImage
      Left = 8
      Top = 8
      Width = 264
      Height = 136
      OnMouseDown = MiniMapMouseDown
      OnMouseMove = MiniMapMouseMove
    end
    object sbBackgroundLayer: TSpeedButton
      Left = 8
      Top = 152
      Width = 38
      Height = 22
      Hint = 'Background layer'
      AllowAllUp = True
      GroupIndex = 1
      Down = True
      Caption = 'Bgnd'
      ParentShowHint = False
      ShowHint = True
      OnClick = RenderSettingChange
    end
    object sbForegroundLayer: TSpeedButton
      Left = 46
      Top = 152
      Width = 38
      Height = 22
      Hint = 'Foreground layer'
      AllowAllUp = True
      GroupIndex = 2
      Down = True
      Caption = 'Fgnd'
      ParentShowHint = False
      ShowHint = True
      OnClick = RenderSettingChange
    end
    object sbTransparentDraw: TSpeedButton
      Left = 84
      Top = 152
      Width = 38
      Height = 22
      Hint = 'Draw foreground tiles transparently'
      AllowAllUp = True
      GroupIndex = 3
      Down = True
      Caption = 'Tran'
      ParentShowHint = False
      ShowHint = True
      OnClick = RenderSettingChange
    end
    object sbShowObjects: TSpeedButton
      Left = 122
      Top = 152
      Width = 38
      Height = 22
      Hint = 'Show objects'
      AllowAllUp = True
      GroupIndex = 4
      Down = True
      Caption = 'Obj'
      ParentShowHint = False
      ShowHint = True
      OnClick = RenderSettingChange
    end
    object sbMarkTiles: TSpeedButton
      Left = 198
      Top = 152
      Width = 38
      Height = 22
      Hint = 'Mark tiles'
      AllowAllUp = True
      GroupIndex = 6
      Caption = 'Tiles'
      ParentShowHint = False
      ShowHint = True
      OnClick = RenderSettingChange
    end
    object sbShowGrid: TSpeedButton
      Left = 236
      Top = 152
      Width = 38
      Height = 22
      Hint = 'Show grid'
      AllowAllUp = True
      GroupIndex = 7
      Caption = 'Grid'
      ParentShowHint = False
      ShowHint = True
      OnClick = RenderSettingChange
    end
    object sbShowMarkers: TSpeedButton
      Left = 160
      Top = 152
      Width = 38
      Height = 22
      Hint = 'Show markers'
      AllowAllUp = True
      GroupIndex = 5
      Down = True
      Caption = 'Mark'
      ParentShowHint = False
      ShowHint = True
      OnClick = RenderSettingChange
    end
    object EditorPages: TPageControl
      Left = 1
      Top = 176
      Width = 278
      Height = 424
      ActivePage = PageTiles
      Align = alBottom
      TabOrder = 0
      OnChange = EditorPagesChange
      object PageTiles: TTabSheet
        Caption = 'Tiles'
        object TileLayerControlsPanel: TPanel
          Left = 0
          Top = 0
          Width = 270
          Height = 396
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 0
          object BlockFrame: TBevel
            Left = 4
            Top = 4
            Width = 260
            Height = 260
            Shape = bsFrame
            Style = bsRaised
          end
          object BlockImage: TImage
            Left = 6
            Top = 6
            Width = 256
            Height = 256
            OnMouseDown = BlockImageMouseDown
          end
          object cbAllLayers: TCheckBox
            Left = 136
            Top = 320
            Width = 105
            Height = 17
            Caption = 'Copy both layers'
            TabOrder = 3
            Visible = False
            OnClick = EditingModeChange
          end
          object rbTileMode: TRadioButton
            Left = 8
            Top = 272
            Width = 125
            Height = 17
            Caption = 'Tile mode'
            Checked = True
            TabOrder = 0
            TabStop = True
            OnClick = EditingModeChange
          end
          object rbBlockMode: TRadioButton
            Left = 8
            Top = 320
            Width = 77
            Height = 17
            Caption = 'Block mode'
            TabOrder = 2
            OnClick = EditingModeChange
          end
          object rbPatternMode: TRadioButton
            Left = 8
            Top = 296
            Width = 125
            Height = 17
            Caption = 'Pattern mode'
            TabOrder = 1
            OnClick = EditingModeChange
          end
          object pnBrushSize: TPanel
            Left = 140
            Top = 272
            Width = 125
            Height = 81
            BevelOuter = bvNone
            TabOrder = 4
            object lbBrushWidth: TLabel
              Left = 0
              Top = 24
              Width = 11
              Height = 13
              Caption = 'W'
            end
            object lbBrushHeight: TLabel
              Left = 0
              Top = 56
              Width = 8
              Height = 13
              Caption = 'H'
            end
            object lbBrushWidthVal: TLabel
              Left = 112
              Top = 24
              Width = 6
              Height = 13
              Caption = '1'
            end
            object lbBrushHeightVal: TLabel
              Left = 112
              Top = 56
              Width = 6
              Height = 13
              Caption = '1'
            end
            object lbBrushSize: TLabel
              Left = 0
              Top = 0
              Width = 51
              Height = 13
              Caption = 'Brush size:'
            end
            object tbBrushWidth: TTrackBar
              Left = 11
              Top = 19
              Width = 100
              Height = 30
              Max = 8
              Min = 1
              PageSize = 1
              Position = 1
              TabOrder = 0
              ThumbLength = 16
              OnChange = tbBrushSizeChange
            end
            object tbBrushHeight: TTrackBar
              Left = 11
              Top = 51
              Width = 100
              Height = 30
              Max = 8
              Min = 1
              PageSize = 1
              Position = 1
              TabOrder = 1
              ThumbLength = 16
              OnChange = tbBrushSizeChange
            end
          end
          object btnSavePreset: TButton
            Left = 136
            Top = 354
            Width = 121
            Height = 21
            Caption = 'Save pattern as preset'
            TabOrder = 5
            Visible = False
            OnClick = btnSavePresetClick
          end
          object snTilesetOffset: TSpinButton
            Left = 112
            Top = 272
            Width = 20
            Height = 33
            DownGlyph.Data = {
              0E010000424D0E01000000000000360000002800000009000000060000000100
              200000000000D800000000000000000000000000000000000000008080000080
              8000008080000080800000808000008080000080800000808000008080000080
              8000008080000080800000808000000000000080800000808000008080000080
              8000008080000080800000808000000000000000000000000000008080000080
              8000008080000080800000808000000000000000000000000000000000000000
              0000008080000080800000808000000000000000000000000000000000000000
              0000000000000000000000808000008080000080800000808000008080000080
              800000808000008080000080800000808000}
            TabOrder = 6
            UpGlyph.Data = {
              0E010000424D0E01000000000000360000002800000009000000060000000100
              200000000000D800000000000000000000000000000000000000008080000080
              8000008080000080800000808000008080000080800000808000008080000080
              8000000000000000000000000000000000000000000000000000000000000080
              8000008080000080800000000000000000000000000000000000000000000080
              8000008080000080800000808000008080000000000000000000000000000080
              8000008080000080800000808000008080000080800000808000000000000080
              8000008080000080800000808000008080000080800000808000008080000080
              800000808000008080000080800000808000}
            OnDownClick = snTilesetOffsetDownClick
            OnUpClick = snTilesetOffsetUpClick
          end
          object rbChangeTileType: TRadioButton
            Left = 8
            Top = 344
            Width = 129
            Height = 17
            Caption = 'Change tile type'
            TabOrder = 7
            OnClick = EditingModeChange
          end
          object cbxChangeTileType: TComboBox
            Left = 136
            Top = 354
            Width = 125
            Height = 21
            Style = csDropDownList
            ItemHeight = 13
            ItemIndex = 0
            TabOrder = 8
            Text = 'Shadow / Bright tiles'
            Visible = False
            OnChange = cbxChangeTileTypeChange
            Items.Strings = (
              'Shadow / Bright tiles'
              'Hidden passage'
              'Hid. pas. boundary')
          end
        end
      end
      object PageObjects: TTabSheet
        Caption = 'Objects'
        ImageIndex = 3
        object lbObjectNumber: TLabel
          Left = 120
          Top = 4
          Width = 40
          Height = 13
          Caption = 'Object 0'
        end
        object lbObjectPosX: TLabel
          Left = 120
          Top = 56
          Width = 40
          Height = 13
          Caption = 'X = 0 (0)'
        end
        object lbObjectPosY: TLabel
          Left = 120
          Top = 80
          Width = 40
          Height = 13
          Caption = 'Y = 0 (0)'
        end
        object lbObjectBehavior: TLabel
          Left = 120
          Top = 128
          Width = 45
          Height = 13
          Caption = 'Behavior:'
        end
        object lstObjectList: TListBox
          Left = 0
          Top = 0
          Width = 113
          Height = 396
          Align = alLeft
          ItemHeight = 13
          TabOrder = 0
          OnClick = lstObjectListClick
          OnDblClick = lstObjectListDblClick
        end
        object cbxObjectType: TComboBox
          Left = 120
          Top = 24
          Width = 145
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 1
          OnChange = cbxObjectTypeChange
        end
        object cbxObjectBehavior: TComboBox
          Left = 120
          Top = 144
          Width = 145
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 2
          OnChange = cbxObjectBehaviorChange
        end
        object pnObjectVar1: TPanel
          Left = 120
          Top = 168
          Width = 145
          Height = 29
          BevelOuter = bvNone
          TabOrder = 3
          object lbObjectVar1: TLabel
            Left = 0
            Top = 8
            Width = 51
            Height = 13
            Caption = 'Property 1:'
          end
          object seObjectVar1: TSpinEdit
            Left = 80
            Top = 4
            Width = 65
            Height = 22
            MaxValue = 255
            MinValue = 0
            TabOrder = 0
            Value = 0
            OnChange = ObjectPropertyChange
          end
        end
        object pnObjectVar2: TPanel
          Left = 120
          Top = 194
          Width = 145
          Height = 29
          BevelOuter = bvNone
          TabOrder = 5
          object lbObjectVar2: TLabel
            Left = 0
            Top = 8
            Width = 51
            Height = 13
            Caption = 'Property 2:'
          end
          object seObjectVar2: TSpinEdit
            Left = 80
            Top = 4
            Width = 65
            Height = 22
            MaxValue = 255
            MinValue = 0
            TabOrder = 0
            Value = 0
            OnChange = ObjectPropertyChange
          end
        end
        object pnObjectVar3: TPanel
          Left = 120
          Top = 220
          Width = 145
          Height = 29
          BevelOuter = bvNone
          TabOrder = 6
          object lbObjectVar3: TLabel
            Left = 0
            Top = 8
            Width = 51
            Height = 13
            Caption = 'Property 3:'
          end
          object seObjectVar3: TSpinEdit
            Left = 80
            Top = 4
            Width = 65
            Height = 22
            MaxValue = 65535
            MinValue = 0
            TabOrder = 0
            Value = 0
            OnChange = ObjectPropertyChange
          end
        end
        object pnObjectVar4: TPanel
          Left = 120
          Top = 246
          Width = 145
          Height = 29
          BevelOuter = bvNone
          TabOrder = 7
          object lbObjectVar4: TLabel
            Left = 0
            Top = 8
            Width = 51
            Height = 13
            Caption = 'Property 4:'
          end
          object seObjectVar4: TSpinEdit
            Left = 80
            Top = 4
            Width = 65
            Height = 22
            MaxValue = 65535
            MinValue = 0
            TabOrder = 0
            Value = 0
            OnChange = ObjectPropertyChange
          end
        end
        object pnObjectVar5: TPanel
          Left = 120
          Top = 272
          Width = 145
          Height = 29
          BevelOuter = bvNone
          TabOrder = 4
          object lbObjectVar5: TLabel
            Left = 0
            Top = 8
            Width = 51
            Height = 13
            Caption = 'Property 5:'
          end
          object seObjectVar5: TSpinEdit
            Left = 80
            Top = 4
            Width = 65
            Height = 22
            MaxValue = 65535
            MinValue = 0
            TabOrder = 0
            Value = 0
            OnChange = ObjectPropertyChange
          end
        end
        object cbObjectAlignToFloor: TCheckBox
          Left = 120
          Top = 104
          Width = 97
          Height = 17
          Caption = 'Align to floor + '
          TabOrder = 8
        end
        object btnObjectClear: TButton
          Left = 208
          Top = 52
          Width = 57
          Height = 20
          Caption = 'Clear'
          TabOrder = 9
          OnClick = btnObjectClearClick
        end
        object btnObjectCopy: TButton
          Left = 208
          Top = 76
          Width = 57
          Height = 20
          Caption = 'Copy'
          TabOrder = 10
          OnClick = btnObjectCopyClick
        end
        object seObjectAlignToFloor: TSpinEdit
          Left = 208
          Top = 102
          Width = 57
          Height = 22
          MaxValue = 15
          MinValue = -15
          TabOrder = 11
          Value = 0
        end
      end
      object PageDoors: TTabSheet
        Caption = 'Doors'
        ImageIndex = 3
        object lbDoorBorderLeft: TLabel
          Left = 4
          Top = 160
          Width = 95
          Height = 13
          Caption = 'Left scrolling border:'
        end
        object lbDoorBorderRight: TLabel
          Left = 4
          Top = 184
          Width = 102
          Height = 13
          Caption = 'Right scrolling border:'
        end
        object lbDoorBorderTop: TLabel
          Left = 4
          Top = 208
          Width = 96
          Height = 13
          Caption = 'Top scrolling border:'
        end
        object lbDoorBorderBottom: TLabel
          Left = 4
          Top = 232
          Width = 110
          Height = 13
          Caption = 'Bottom scrolling border:'
        end
        object lbDoorNumber: TLabel
          Left = 120
          Top = 4
          Width = 32
          Height = 13
          Caption = 'Door 0'
        end
        object lbDoorEntrance: TLabel
          Left = 120
          Top = 28
          Width = 100
          Height = 13
          Caption = 'Entrance: 0 : 0 , 0 : 0'
        end
        object lbDoorDestination: TLabel
          Left = 120
          Top = 48
          Width = 80
          Height = 13
          Caption = 'Destination: 0 , 0'
        end
        object sbDoorEntrance: TSpeedButton
          Left = 120
          Top = 72
          Width = 73
          Height = 22
          GroupIndex = 4
          Down = True
          Caption = 'Set entrance'
        end
        object sbDoorDestination: TSpeedButton
          Left = 192
          Top = 72
          Width = 73
          Height = 22
          GroupIndex = 4
          Caption = 'Set dest.'
        end
        object lstDoorList: TListBox
          Left = 0
          Top = 0
          Width = 113
          Height = 153
          ItemHeight = 13
          TabOrder = 0
          OnClick = lstDoorListClick
          OnDblClick = lstDoorListDblClick
        end
        object seDoorBorderLeft: TSpinEdit
          Left = 120
          Top = 156
          Width = 73
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 1
          Value = 0
          OnChange = seDoorBorderChange
        end
        object seDoorBorderRight: TSpinEdit
          Left = 120
          Top = 180
          Width = 73
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 2
          Value = 0
          OnChange = seDoorBorderChange
        end
        object seDoorBorderTop: TSpinEdit
          Left = 120
          Top = 204
          Width = 73
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 3
          Value = 0
          OnChange = seDoorBorderChange
        end
        object seDoorBorderBottom: TSpinEdit
          Left = 120
          Top = 228
          Width = 73
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 4
          Value = 0
          OnChange = seDoorBorderChange
        end
        object btnDoorRemove: TButton
          Left = 192
          Top = 128
          Width = 73
          Height = 20
          Caption = 'Remove last'
          TabOrder = 5
          OnClick = btnDoorRemoveClick
        end
        object btnDoorAdd: TButton
          Left = 120
          Top = 128
          Width = 70
          Height = 20
          Caption = 'Add new'
          TabOrder = 6
          OnClick = btnDoorAddClick
        end
        object cbDoorAutoSet: TCheckBox
          Left = 120
          Top = 100
          Width = 145
          Height = 17
          Caption = 'Auto-set opposite dest.'
          Checked = True
          State = cbChecked
          TabOrder = 7
        end
      end
      object PageSwitches: TTabSheet
        Caption = 'Switches'
        ImageIndex = 4
        object lbSwitchNumber: TLabel
          Left = 120
          Top = 4
          Width = 41
          Height = 13
          Caption = 'Switch 0'
        end
        object lbSwitchPosition: TLabel
          Left = 168
          Top = 4
          Width = 67
          Height = 13
          Caption = 'Position: (0, 0)'
        end
        object lbSwitchType: TLabel
          Left = 120
          Top = 28
          Width = 27
          Height = 13
          Caption = 'Type:'
        end
        object lstSwitchList: TListBox
          Left = 0
          Top = 0
          Width = 113
          Height = 153
          ItemHeight = 13
          TabOrder = 0
          OnClick = lstSwitchListClick
          OnDblClick = lstSwitchListDblClick
        end
        object cbxSwitchType: TComboBox
          Left = 152
          Top = 24
          Width = 113
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 1
          OnChange = cbxSwitchTypeChange
          Items.Strings = (
            '0 - (unknown)'
            '1 - Activate object'
            '2 - (unknown)'
            '3 - Transform tiles'
            '4 - Remove fg.tiles'
            '5 - Remove objects')
        end
        object btnSwitchRemove: TButton
          Left = 192
          Top = 128
          Width = 73
          Height = 20
          Caption = 'Remove last'
          TabOrder = 2
          OnClick = btnSwitchRemoveClick
        end
        object btnSwitchAdd: TButton
          Left = 120
          Top = 128
          Width = 70
          Height = 20
          Caption = 'Add new'
          TabOrder = 3
          OnClick = btnSwitchAddClick
        end
        object pnSwitchT1: TPanel
          Left = 116
          Top = 48
          Width = 152
          Height = 33
          BevelOuter = bvNone
          TabOrder = 4
          Visible = False
          object lbSwitchT1ObjNum: TLabel
            Left = 4
            Top = 8
            Width = 72
            Height = 13
            Caption = 'Object number:'
          end
          object seSwitchT1ObjNum: TSpinEdit
            Left = 84
            Top = 4
            Width = 65
            Height = 22
            MaxValue = 149
            MinValue = 0
            TabOrder = 0
            Value = 0
            OnChange = SwitchPropertyChange
          end
        end
        object pnSwitchT3: TPanel
          Left = 116
          Top = 48
          Width = 152
          Height = 49
          BevelOuter = bvNone
          TabOrder = 5
          Visible = False
          object lbSwitchT3Transblock: TLabel
            Left = 4
            Top = 8
            Width = 132
            Height = 13
            Caption = 'Target transformation block:'
          end
          object cbxSwitchT3Transblock: TComboBox
            Left = 4
            Top = 28
            Width = 145
            Height = 21
            Style = csDropDownList
            ItemHeight = 13
            TabOrder = 0
            OnChange = SwitchPropertyChange
          end
        end
        object pnSwitchT4: TPanel
          Left = 116
          Top = 48
          Width = 152
          Height = 49
          BevelOuter = bvNone
          TabOrder = 6
          Visible = False
          object lbSwitchT4Area: TLabel
            Left = 4
            Top = 8
            Width = 79
            Height = 13
            Caption = 'Area: 0 , 0 : 0 , 0'
          end
          object sbSwitchT4Area: TSpeedButton
            Left = 4
            Top = 28
            Width = 141
            Height = 20
            AllowAllUp = True
            GroupIndex = 11
            Caption = 'Set target area'
          end
        end
        object pnSwitchT5: TPanel
          Left = 116
          Top = 48
          Width = 152
          Height = 57
          BevelOuter = bvNone
          TabOrder = 7
          Visible = False
          object lbSwitchT5FirstObj: TLabel
            Left = 4
            Top = 8
            Width = 54
            Height = 13
            Caption = 'First object:'
          end
          object lbSwitchT5LastObj: TLabel
            Left = 4
            Top = 32
            Width = 55
            Height = 13
            Caption = 'Last object:'
          end
          object seSwitchT5FirstObj: TSpinEdit
            Left = 84
            Top = 4
            Width = 65
            Height = 22
            MaxValue = 149
            MinValue = 0
            TabOrder = 0
            Value = 0
            OnChange = SwitchPropertyChange
          end
          object seSwitchT5LastObj: TSpinEdit
            Left = 84
            Top = 28
            Width = 65
            Height = 22
            MaxValue = 149
            MinValue = 0
            TabOrder = 1
            Value = 0
            OnChange = SwitchPropertyChange
          end
        end
      end
      object PageLocks: TTabSheet
        Caption = 'Locks'
        ImageIndex = 5
        object lbLockNumber: TLabel
          Left = 120
          Top = 4
          Width = 33
          Height = 13
          Caption = 'Lock 0'
        end
        object lbLockType: TLabel
          Left = 120
          Top = 28
          Width = 27
          Height = 13
          Caption = 'Type:'
        end
        object lbLockPosition: TLabel
          Left = 168
          Top = 4
          Width = 67
          Height = 13
          Caption = 'Position: (0, 0)'
        end
        object lbLockTransblock: TLabel
          Left = 120
          Top = 56
          Width = 132
          Height = 13
          Caption = 'Target transformation block:'
        end
        object Bevel1: TBevel
          Left = -8
          Top = 156
          Width = 281
          Height = 2
          Shape = bsBottomLine
        end
        object lstLockList: TListBox
          Left = 0
          Top = 0
          Width = 113
          Height = 153
          ItemHeight = 13
          TabOrder = 0
          OnClick = lstLockListClick
          OnDblClick = lstLockListDblClick
        end
        object cbxLockType: TComboBox
          Left = 160
          Top = 24
          Width = 105
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          ItemIndex = 0
          TabOrder = 1
          Text = '0 - Yellow'
          OnChange = cbxLockTypeChange
          Items.Strings = (
            '0 - Yellow'
            '1 - Red'
            '2 - Green'
            '3 - Blue')
        end
        object cbxLockTransblock: TComboBox
          Left = 120
          Top = 76
          Width = 145
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 2
          OnChange = cbxLockTransblockChange
        end
        object pnTansblock: TPanel
          Left = 0
          Top = 160
          Width = 270
          Height = 225
          BevelOuter = bvNone
          TabOrder = 3
          object lbTransblockNumber: TLabel
            Left = 120
            Top = 4
            Width = 108
            Height = 13
            Caption = 'Transformation block 0'
          end
          object imgTransblock: TImage
            Left = 128
            Top = 44
            Width = 128
            Height = 128
          end
          object sbTransblockSelect: TSpeedButton
            Left = 120
            Top = 182
            Width = 70
            Height = 20
            AllowAllUp = True
            GroupIndex = 12
            Caption = 'Select block'
          end
          object sbTransblockMove: TSpeedButton
            Left = 192
            Top = 182
            Width = 73
            Height = 20
            AllowAllUp = True
            GroupIndex = 12
            Caption = 'Move block'
          end
          object lbTransblockSizePos: TLabel
            Left = 120
            Top = 24
            Width = 106
            Height = 13
            Caption = 'Size: [0 x 0] Pos: (0, 0)'
          end
          object lstTransblockList: TListBox
            Left = 0
            Top = 0
            Width = 113
            Height = 225
            ItemHeight = 13
            TabOrder = 0
            OnClick = lstTransblockListClick
            OnDblClick = lstTransblockListDblClick
          end
          object btnTransblockRemove: TButton
            Left = 192
            Top = 204
            Width = 73
            Height = 20
            Caption = 'Remove last'
            TabOrder = 1
            OnClick = btnTransblockRemoveClick
          end
          object btnTransblockAdd: TButton
            Left = 120
            Top = 204
            Width = 70
            Height = 20
            Caption = 'Add new'
            TabOrder = 2
            OnClick = btnTransblockAddClick
          end
        end
        object btnLockAdd: TButton
          Left = 120
          Top = 128
          Width = 70
          Height = 20
          Caption = 'Add new'
          TabOrder = 4
          OnClick = btnLockAddClick
        end
        object btnLockRemove: TButton
          Left = 192
          Top = 128
          Width = 73
          Height = 20
          Caption = 'Remove last'
          TabOrder = 5
          OnClick = btnLockRemoveClick
        end
      end
      object PageOther: TTabSheet
        Caption = 'Other'
        ImageIndex = 5
        object lbCapsuleCount: TLabel
          Left = 4
          Top = 64
          Width = 71
          Height = 13
          Caption = 'Capsule count:'
        end
        object lbBorderLeft: TLabel
          Left = 4
          Top = 104
          Width = 95
          Height = 13
          Caption = 'Left scrolling border:'
        end
        object lbBorderRight: TLabel
          Left = 4
          Top = 128
          Width = 102
          Height = 13
          Caption = 'Right scrolling border:'
        end
        object lbBorderTop: TLabel
          Left = 4
          Top = 152
          Width = 96
          Height = 13
          Caption = 'Top scrolling border:'
        end
        object lbBorderBottom: TLabel
          Left = 4
          Top = 176
          Width = 110
          Height = 13
          Caption = 'Bottom scrolling border:'
        end
        object lbMapFinishPos: TLabel
          Left = 4
          Top = 208
          Width = 247
          Height = 13
          Caption = 'Where you appear on MAP after you finish this level:'
        end
        object lbMapFinishPixelX: TLabel
          Left = 8
          Top = 232
          Width = 10
          Height = 13
          Caption = 'X:'
        end
        object lbMapFinishPixelY: TLabel
          Left = 112
          Top = 232
          Width = 10
          Height = 13
          Caption = 'Y:'
        end
        object seCapsuleCount: TSpinEdit
          Left = 88
          Top = 60
          Width = 73
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 0
          Value = 0
          OnChange = seCapsuleCountChange
        end
        object seBorderLeft: TSpinEdit
          Left = 120
          Top = 100
          Width = 73
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 1
          Value = 0
          OnChange = seBorderChange
        end
        object seBorderRight: TSpinEdit
          Left = 120
          Top = 124
          Width = 73
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 2
          Value = 0
          OnChange = seBorderChange
        end
        object seBorderTop: TSpinEdit
          Left = 120
          Top = 148
          Width = 73
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 3
          Value = 0
          OnChange = seBorderChange
        end
        object seBorderBottom: TSpinEdit
          Left = 120
          Top = 172
          Width = 73
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 4
          Value = 0
          OnChange = seBorderChange
        end
        object seMapFinishPixelX: TSpinEdit
          Left = 32
          Top = 228
          Width = 73
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 5
          Value = 0
          OnChange = seBorderChange
        end
        object seMapFinishPixelY: TSpinEdit
          Left = 136
          Top = 228
          Width = 73
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 6
          Value = 0
          OnChange = seBorderChange
        end
      end
    end
  end
  object AppMenu: TMainMenu
    object File1: TMenuItem
      Caption = 'File'
      object Newmap1: TMenuItem
        Caption = 'New map'
        ShortCut = 16462
        OnClick = Newmap1Click
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object Openmap1: TMenuItem
        Caption = 'Open map'
        ShortCut = 16463
        OnClick = Openmap1Click
      end
      object Reopenmap1: TMenuItem
        Caption = 'Reopen map'
        ShortCut = 16466
        OnClick = Reopenmap1Click
      end
      object Importmap1: TMenuItem
        Caption = 'Import map'
        ShortCut = 16457
        OnClick = Importmap1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Savemap1: TMenuItem
        Caption = 'Save map'
        ShortCut = 16467
        OnClick = Savemap1Click
      end
      object Savemapas1: TMenuItem
        Caption = 'Save map as...'
        OnClick = Savemapas1Click
      end
      object Exportmap1: TMenuItem
        Caption = 'Export map'
        ShortCut = 16453
        OnClick = Exportmap1Click
      end
      object N8: TMenuItem
        Caption = '-'
      end
      object Savemapimage1: TMenuItem
        Caption = 'Save map image'
        OnClick = Savemapimage1Click
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'Exit'
        OnClick = Exit1Click
      end
    end
    object Edit1: TMenuItem
      Caption = 'Edit'
      object Undo1: TMenuItem
        Caption = 'Undo'
        Enabled = False
        ShortCut = 16474
        OnClick = Undo1Click
      end
      object Redo1: TMenuItem
        Caption = 'Redo'
        Enabled = False
        ShortCut = 16473
        OnClick = Redo1Click
      end
      object N10: TMenuItem
        Caption = '-'
      end
      object Copy1: TMenuItem
        Caption = 'Copy'
        ShortCut = 16451
        OnClick = Copy1Click
      end
      object Paste1: TMenuItem
        Caption = 'Paste'
        ShortCut = 16470
        OnClick = Paste1Click
      end
    end
    object ileset1: TMenuItem
      Caption = 'Tileset'
      object Selecttileset1: TMenuItem
        Caption = 'Select tileset...'
        OnClick = Selecttileset1Click
      end
      object Selectnext1: TMenuItem
        Caption = 'Select next'
        ShortCut = 16468
        OnClick = Selectnext1Click
      end
      object Loadcustomimage1: TMenuItem
        Caption = 'Load custom image'
        OnClick = Loadcustomimage1Click
      end
    end
    object Map1: TMenuItem
      Caption = 'Map'
      object Setmapsize1: TMenuItem
        Caption = 'Set map size'
        ShortCut = 116
        OnClick = Setmapsize1Click
      end
      object Shiftmap1: TMenuItem
        Caption = 'Shift map'
        ShortCut = 117
        OnClick = Shiftmap1Click
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object Spriteproperties1: TMenuItem
        Caption = 'Sprite properties...'
        ShortCut = 115
        OnClick = Spriteproperties1Click
      end
      object Tileproperties1: TMenuItem
        Caption = 'Tile properties...'
        ShortCut = 118
        OnClick = Tileproperties1Click
      end
    end
    object Test1: TMenuItem
      Caption = 'Test'
      object LaunchGame1: TMenuItem
        Caption = 'Launch game'
        ShortCut = 119
        OnClick = LaunchGame1Click
      end
      object N7: TMenuItem
        Caption = '-'
      end
      object Easy1: TMenuItem
        AutoCheck = True
        Caption = 'Easy'
        GroupIndex = 5
        RadioItem = True
        OnClick = TestMapDifficultyClick
      end
      object Moderate1: TMenuItem
        Tag = 1
        AutoCheck = True
        Caption = 'Moderate'
        Checked = True
        GroupIndex = 5
        RadioItem = True
        OnClick = TestMapDifficultyClick
      end
      object Hard1: TMenuItem
        Tag = 2
        AutoCheck = True
        Caption = 'Hard'
        GroupIndex = 5
        RadioItem = True
        OnClick = TestMapDifficultyClick
      end
    end
    object Help1: TMenuItem
      Caption = 'Help'
      object KeyShortcuts1: TMenuItem
        Caption = 'Key Shortcuts'
        OnClick = KeyShortcuts1Click
      end
      object Mouseactions1: TMenuItem
        Caption = 'Mouse actions'
        OnClick = Mouseactions1Click
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object About1: TMenuItem
        Caption = 'About...'
        OnClick = About1Click
      end
    end
  end
  object MapImportDialog: TOpenDialog
    DefaultExt = 'vgl'
    Filter = 'Vinyl Goddess from Mars level (*.vgl)|*.vgl|All files (*.*)|*.*'
    Title = 'Import map'
    Left = 32
  end
  object TilesetOpenDialog: TOpenDialog
    DefaultExt = 'bmp'
    Filter = 'Tileset image (*.bmp)|*.bmp'
    InitialDir = '.\tilesets'
    Title = 'Load Tileset image'
    Left = 96
  end
  object MapExportDialog: TSaveDialog
    DefaultExt = 'vgl'
    Filter = 'Vinyl Goddess from Mars level (*.vgl)|*.vgl|All files (*.*)|*.*'
    Title = 'Export map'
    Left = 64
  end
  object XPManifest1: TXPManifest
    Left = 160
  end
  object MapImageSaveDialog: TSaveDialog
    DefaultExt = 'bmp'
    Filter = 'Image (*.bmp)|*.bmp'
    Title = 'Save map image'
    Left = 128
  end
end
