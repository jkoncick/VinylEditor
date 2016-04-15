unit main;

interface

uses
  // System libraries
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, Menus, StdCtrls, XPMan, Math, Spin, Buttons,
  ShellApi, IniFiles, Clipbrd, CheckLst, Grids, ValEdit,
  // Dialogs
  set_dialog, block_preset_dialog, sprite_props_dialog, tile_props_dialog,
  // Units
  _renderer, _map, _tileset, _settings, _archive, _savegame, _objectinfo;

type
  TImage = class(ExtCtrls.TImage)
    protected
      procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  end;

const layer_marker_color: TColor = $C020C0;

const lock_type_letter: array[0..3] of char = ('Y', 'R', 'G', 'B');
const switch_type_letter: array[0..5] of char = ('U', 'A', 'U', 'T', 'R', 'O');

const tile_mode_value: array[0..2,0..1] of word = ((tilemode_shadow, tilemode_bright), (tilemode_hidden_passage, tilemode_impassable), (tilemode_passage_boundary, tilemode_impassable));

type
  SelectedMode = (mTileMode, mPatternMode, mBlockMode, mChangeTileType, mPainting, mSelecting, mRightBtnScroll, mPixelCoords, mSingleTileThing, mTile, mObject, mDoor, mSwitch, mLock, mItem);

type
  TMainWindow = class(TForm)
    MapCanvas: TImage;
    MapScrollH: TScrollBar;
    MapScrollV: TScrollBar;
    AppMenu: TMainMenu;
    File1: TMenuItem;
    StatusBar: TStatusBar;
    EditorMenu: TPanel;
    Importmap1: TMenuItem;
    MapImportDialog: TOpenDialog;
    Savemap1: TMenuItem;
    ileset1: TMenuItem;
    Loadcustomimage1: TMenuItem;
    TilesetOpenDialog: TOpenDialog;
    MapExportDialog: TSaveDialog;
    Selecttileset1: TMenuItem;
    MiniMap: TImage;
    XPManifest1: TXPManifest;
    Reopenmap1: TMenuItem;
    N1: TMenuItem;
    Savemapas1: TMenuItem;
    N2: TMenuItem;
    Exit1: TMenuItem;
    Selectnext1: TMenuItem;
    Map1: TMenuItem;
    Setmapsize1: TMenuItem;
    Shiftmap1: TMenuItem;
    EditorPages: TPageControl;
    PageTiles: TTabSheet;
    rbBlockMode: TRadioButton;
    rbTileMode: TRadioButton;
    BlockImage: TImage;
    Newmap1: TMenuItem;
    N4: TMenuItem;
    Help1: TMenuItem;
    KeyShortcuts1: TMenuItem;
    About1: TMenuItem;
    Mouseactions1: TMenuItem;
    N5: TMenuItem;
    MiniMapFrame: TBevel;
    Savemapimage1: TMenuItem;
    N8: TMenuItem;
    CursorImage: TImage;
    MapImageSaveDialog: TSaveDialog;
    cbAllLayers: TCheckBox;
    Edit1: TMenuItem;
    Undo1: TMenuItem;
    Redo1: TMenuItem;
    Test1: TMenuItem;
    LaunchGame1: TMenuItem;
    N10: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    BlockFrame: TBevel;
    TileLayerControlsPanel: TPanel;
    PageObjects: TTabSheet;
    sbBackgroundLayer: TSpeedButton;
    sbForegroundLayer: TSpeedButton;
    sbTransparentDraw: TSpeedButton;
    sbShowObjects: TSpeedButton;
    sbMarkTiles: TSpeedButton;
    sbShowGrid: TSpeedButton;
    rbPatternMode: TRadioButton;
    tbBrushWidth: TTrackBar;
    tbBrushHeight: TTrackBar;
    pnBrushSize: TPanel;
    lbBrushWidth: TLabel;
    lbBrushHeight: TLabel;
    lbBrushWidthVal: TLabel;
    lbBrushHeightVal: TLabel;
    Openmap1: TMenuItem;
    lbBrushSize: TLabel;
    Exportmap1: TMenuItem;
    btnSavePreset: TButton;
    N6: TMenuItem;
    Spriteproperties1: TMenuItem;
    N7: TMenuItem;
    Easy1: TMenuItem;
    Moderate1: TMenuItem;
    Hard1: TMenuItem;
    lstObjectList: TListBox;
    cbxObjectType: TComboBox;
    lbObjectNumber: TLabel;
    lbObjectPosX: TLabel;
    lbObjectPosY: TLabel;
    Tileproperties1: TMenuItem;
    sbShowMarkers: TSpeedButton;
    PageDoors: TTabSheet;
    PageSwitches: TTabSheet;
    PageLocks: TTabSheet;
    lstDoorList: TListBox;
    lstSwitchList: TListBox;
    lstLockList: TListBox;
    lbLockNumber: TLabel;
    lbLockType: TLabel;
    cbxLockType: TComboBox;
    lbLockPosition: TLabel;
    lbLockTransblock: TLabel;
    cbxLockTransblock: TComboBox;
    pnTansblock: TPanel;
    lstTransblockList: TListBox;
    lbTransblockNumber: TLabel;
    imgTransblock: TImage;
    Bevel1: TBevel;
    PageOther: TTabSheet;
    cbxObjectBehavior: TComboBox;
    lbObjectBehavior: TLabel;
    pnObjectVar1: TPanel;
    lbObjectVar1: TLabel;
    seObjectVar1: TSpinEdit;
    pnObjectVar2: TPanel;
    lbObjectVar2: TLabel;
    seObjectVar2: TSpinEdit;
    pnObjectVar3: TPanel;
    lbObjectVar3: TLabel;
    seObjectVar3: TSpinEdit;
    pnObjectVar4: TPanel;
    lbObjectVar4: TLabel;
    seObjectVar4: TSpinEdit;
    pnObjectVar5: TPanel;
    lbObjectVar5: TLabel;
    seObjectVar5: TSpinEdit;
    cbObjectAlignToFloor: TCheckBox;
    btnObjectClear: TButton;
    btnObjectCopy: TButton;
    btnLockAdd: TButton;
    btnLockRemove: TButton;
    lbCapsuleCount: TLabel;
    seCapsuleCount: TSpinEdit;
    lbBorderLeft: TLabel;
    lbBorderRight: TLabel;
    lbBorderTop: TLabel;
    lbBorderBottom: TLabel;
    seBorderLeft: TSpinEdit;
    seBorderRight: TSpinEdit;
    seBorderTop: TSpinEdit;
    seBorderBottom: TSpinEdit;
    lbDoorBorderLeft: TLabel;
    lbDoorBorderRight: TLabel;
    lbDoorBorderTop: TLabel;
    lbDoorBorderBottom: TLabel;
    seDoorBorderLeft: TSpinEdit;
    seDoorBorderRight: TSpinEdit;
    seDoorBorderTop: TSpinEdit;
    seDoorBorderBottom: TSpinEdit;
    lbDoorNumber: TLabel;
    lbDoorEntrance: TLabel;
    lbDoorDestination: TLabel;
    sbDoorEntrance: TSpeedButton;
    sbDoorDestination: TSpeedButton;
    btnDoorRemove: TButton;
    btnDoorAdd: TButton;
    seObjectAlignToFloor: TSpinEdit;
    lbSwitchNumber: TLabel;
    lbSwitchPosition: TLabel;
    lbSwitchType: TLabel;
    cbxSwitchType: TComboBox;
    btnSwitchRemove: TButton;
    btnSwitchAdd: TButton;
    cbDoorAutoSet: TCheckBox;
    lbMapFinishPos: TLabel;
    seMapFinishPixelX: TSpinEdit;
    seMapFinishPixelY: TSpinEdit;
    lbMapFinishPixelX: TLabel;
    lbMapFinishPixelY: TLabel;
    cbxChangeTileType: TComboBox;
    snTilesetOffset: TSpinButton;
    rbChangeTileType: TRadioButton;
    btnTransblockRemove: TButton;
    btnTransblockAdd: TButton;
    sbTransblockSelect: TSpeedButton;
    sbTransblockMove: TSpeedButton;
    lbTransblockSizePos: TLabel;
    pnSwitchT1: TPanel;
    lbSwitchT1ObjNum: TLabel;
    seSwitchT1ObjNum: TSpinEdit;
    pnSwitchT3: TPanel;
    lbSwitchT3Transblock: TLabel;
    cbxSwitchT3Transblock: TComboBox;
    pnSwitchT4: TPanel;
    lbSwitchT4Area: TLabel;
    sbSwitchT4Area: TSpeedButton;
    pnSwitchT5: TPanel;
    lbSwitchT5FirstObj: TLabel;
    seSwitchT5FirstObj: TSpinEdit;
    lbSwitchT5LastObj: TLabel;
    seSwitchT5LastObj: TSpinEdit;
    // Main form events
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormResize(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
    procedure CMDialogKey(var AMessage: TCMDialogKey); message CM_DIALOGKEY;
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    // Main menu events
    procedure Newmap1Click(Sender: TObject);
    procedure Openmap1Click(Sender: TObject);
    procedure Importmap1Click(Sender: TObject);
    procedure Reopenmap1Click(Sender: TObject);
    procedure Savemap1Click(Sender: TObject);
    procedure Savemapas1Click(Sender: TObject);
    procedure Exportmap1Click(Sender: TObject);
    procedure Savemapimage1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Undo1Click(Sender: TObject);
    procedure Redo1Click(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure Paste1Click(Sender: TObject);
    procedure Selecttileset1Click(Sender: TObject);
    procedure Selectnext1Click(Sender: TObject);
    procedure Loadcustomimage1Click(Sender: TObject);
    procedure Usepredefinedtiles1Click(Sender: TObject);
    procedure Setmapsize1Click(Sender: TObject);
    procedure Shiftmap1Click(Sender: TObject);
    procedure Spriteproperties1Click(Sender: TObject);
    procedure Tileproperties1Click(Sender: TObject);
    procedure LaunchGame1Click(Sender: TObject);
    procedure TestMapDifficultyClick(Sender: TObject);
    procedure KeyShortcuts1Click(Sender: TObject);
    procedure Mouseactions1Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    // Main form components events
    procedure MapScrollChange(Sender: TObject);
    procedure MapCanvasMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure MapCanvasMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CursorImageMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure CursorImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MapCanvasDblClick(Sender: TObject);
    procedure MapCanvasMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ImageMouseLeave(Sender: TObject);
    // Editing menu component events
    procedure MiniMapMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MiniMapMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure RenderSettingChange(Sender: TObject);
    procedure EditorPagesChange(Sender: TObject);
    // Layer editing component events
    procedure BlockImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure EditingModeChange(Sender: TObject);
    procedure snTilesetOffsetUpClick(Sender: TObject);
    procedure snTilesetOffsetDownClick(Sender: TObject);
    procedure cbxChangeTileTypeChange(Sender: TObject);
    procedure tbBrushSizeChange(Sender: TObject);
    procedure btnSavePresetClick(Sender: TObject);
    procedure sbObjectClick(Sender: TObject);
    // Special entities editing component events
    procedure lstObjectListClick(Sender: TObject);
    procedure lstObjectListDblClick(Sender: TObject);
    procedure lstDoorListClick(Sender: TObject);
    procedure lstDoorListDblClick(Sender: TObject);
    procedure lstSwitchListClick(Sender: TObject);
    procedure lstSwitchListDblClick(Sender: TObject);
    procedure lstLockListClick(Sender: TObject);
    procedure lstLockListDblClick(Sender: TObject);
    procedure lstTransblockListClick(Sender: TObject);
    procedure lstTransblockListDblClick(Sender: TObject);
    // Object editing component events
    procedure cbxObjectTypeChange(Sender: TObject);
    procedure cbxObjectBehaviorChange(Sender: TObject);
    procedure ObjectPropertyChange(Sender: TObject);
    procedure btnObjectClearClick(Sender: TObject);
    procedure btnObjectCopyClick(Sender: TObject);
    procedure btnDoorAddClick(Sender: TObject);
    procedure btnDoorRemoveClick(Sender: TObject);
    procedure seDoorBorderChange(Sender: TObject);
    procedure cbxSwitchTypeChange(Sender: TObject);
    procedure SwitchPropertyChange(Sender: TObject);
    procedure btnSwitchAddClick(Sender: TObject);
    procedure btnSwitchRemoveClick(Sender: TObject);
    procedure cbxLockTypeChange(Sender: TObject);
    procedure cbxLockTransblockChange(Sender: TObject);
    procedure btnLockAddClick(Sender: TObject);
    procedure btnLockRemoveClick(Sender: TObject);
    procedure btnTransblockAddClick(Sender: TObject);
    procedure btnTransblockRemoveClick(Sender: TObject);
    procedure seCapsuleCountChange(Sender: TObject);
    procedure seBorderChange(Sender: TObject);

  public

    item_buttons: array[0..Length(itemNames)-1] of TSpeedButton;
    obj_prop_labels: array[1..5] of TLabel;
    obj_prop_spinedits: array[1..5] of TSpinEdit;

    // Map canvas variables
    map_canvas_width: word;
    map_canvas_height: word;
    map_canvas_left: word;
    map_canvas_top: word;

    // Mouse and keyboard related variables
    mouse_old_x: word;
    mouse_old_y: word;
    mouse_old_pixel_x: word;
    mouse_old_pixel_y: word;
    mouse_already_clicked: boolean;
    mouse_last_button: TMouseButton;
    cur_shift_state: TShiftState;

    // Minimap variables
    mmap_border_x: word;
    mmap_border_y: word;
    minimap_buffer: TBitmap;

    // Editing variables
    cur_preset_group: integer;
    cur_tileset_offset: integer;
    cur_tile_index: word;
    cur_selected_preset: array[0..1] of integer;
    cur_block: TSelectionBlock;
    cur_item_type: integer;
    cur_item_speedbtn: TSpeedButton;
    updating: boolean;

    // Selection variables
    selection_started: boolean;
    selection_start_x: word;
    selection_start_y: word;
    selection_end_x: word;
    selection_end_y: word;

    // Clipboard variables
    clipboard_format: cardinal;

    // Rendering procedures
    procedure resize_map_canvas;
    procedure render_map;
    procedure render_minimap;
    procedure render_minimap_position_marker;
    procedure render_editing_marker;
    procedure render_tileset;

    // Level data GUI procedures
    procedure update_level_data;
    procedure update_object_entry(index: integer);
    procedure update_door_entry(index: integer);
    procedure update_switch_entry(index: integer);
    procedure update_lock_entry(index: integer);
    procedure update_transblock_entry(index: integer);

    // Map loading & saving procedures
    procedure load_map_from_archive(index: integer);
    procedure save_map_to_archive(index: integer);
    procedure load_map_from_file(filename: String);
    procedure save_map_to_file(filename: String);
    function check_map_errors: boolean;
    procedure set_window_titles(map_name: String);

    // Map testing procedures
    function check_map_can_be_tested: boolean;
    procedure launch_game;

    // Miscellaneous helper procedures
    function mode(m: SelectedMode): boolean;
    function mouse_over_map_canvas: boolean;
    function get_object_listitem(obj_index: integer; obj_type: word): String;
    function get_object_under_mouse: integer;
    procedure get_object_mouse_position(var pos_x, pos_y, size_x, size_y: word);
    procedure center_map_to(x, y: integer);
    procedure show_statistics;
    procedure update_editing_mode;
    procedure select_current_preset;

    // Procedures related to cursor and block image
    procedure resize_cursor_image;
    procedure draw_cursor_image;
    procedure draw_block_image;

    // Procedures called from other dialog
    procedure set_map_size(new_width, new_height: integer);
    procedure shift_map(direction: TDirection; num_tiles: integer);
  end;

var
  current_dir: String;
  MainWindow: TMainWindow;

implementation

{$R *.dfm}

procedure TImage.CMMouseLeave(var Message: TMessage);
begin
  MainWindow.ImageMouseLeave(self);
end;

procedure TMainWindow.FormCreate(Sender: TObject);
var
  i: integer;
  btn: TSpeedButton;
  behavior_name: String;
begin
  // Miscellaneous initializations
  randomize;
  current_dir := ExtractFilePath(Application.ExeName);
  Application.HintPause := 500;
  Application.HintHidePause:= 10000;
  DragAcceptFiles(Handle, True);
  clipboard_format := RegisterClipboardFormat('VinylEditorBlock');
  top := 60;
  // Load settings
  Settings.load_precreate_editor_settings;
  // Initialize archive
  Archive.init;
  // Load and initialize graphics
  Renderer.init;
  minimap_buffer := TBitmap.Create;
  minimap_buffer.PixelFormat := pf32bit;
  minimap_buffer.Width := MiniMap.Width;
  minimap_buffer.Height := MiniMap.Height;
  // Initialize tilesets
  Tileset.init;
  // Initialize object info
  ObjectInfo.init;
  // Set up behavior list
  for i := 0 to max_behaviors - 1 do
  begin
    behavior_name := ObjectInfo.behaviors[i].name;
    //if behavior_name = '' then
    //  behavior_name := '(unknown)';
    cbxObjectBehavior.Items.Add(inttostr(i) + ' - ' + behavior_name);
  end;
  // Create buttons for placing items
  for i := 0 to Length(itemNames) - 1 do
  begin
    btn := TSpeedButton.Create(self);
    btn.Width := 40;
    btn.Height := 40;
    btn.Glyph.Width := 32;
    btn.Glyph.Height := 32;
    btn.Left := 4 + (i mod 6) * 44;
    btn.Top := 8 + (i div 6) * 44;
    btn.Hint := itemNames[i];
    btn.ShowHint := true;
    btn.GroupIndex := 10;
    btn.Tag := i;
    btn.OnClick := sbObjectClick;
    btn.Parent := PageOther;
    item_buttons[i] := btn;
  end;
  sbObjectClick(item_buttons[0]);
  // Initialize object property labels and spinedits
  obj_prop_labels[1] := lbObjectVar1;
  obj_prop_labels[2] := lbObjectVar2;
  obj_prop_labels[3] := lbObjectVar3;
  obj_prop_labels[4] := lbObjectVar4;
  obj_prop_labels[5] := lbObjectVar5;
  obj_prop_spinedits[1] := seObjectVar1;
  obj_prop_spinedits[2] := seObjectVar2;
  obj_prop_spinedits[3] := seObjectVar3;
  obj_prop_spinedits[4] := seObjectVar4;
  obj_prop_spinedits[5] := seObjectVar5;
  // Set up test map difficulty
  case Settings.TestMapDifficulty of
    0: Easy1.Checked := true;
    1: Moderate1.Checked := true;
    2: Hard1.Checked := true;
  end;
end;

procedure TMainWindow.FormDestroy(Sender: TObject);
begin
  DragAcceptFiles(Handle, False);
end;

procedure TMainWindow.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Map.loaded and Settings.AlwaysAskOnQuit then
  begin
    if Application.MessageBox('Do you really want to quit?','VinylEditor', MB_YESNO or MB_ICONQUESTION) = IDNO then
    begin
      Action := caNone;
      exit;
    end;
  end;
  Settings.save_editor_settings;
  Tileset.save_config;
  MainWindow.OnResize := nil;
end;

procedure TMainWindow.FormResize(Sender: TObject);
begin
  resize_map_canvas;
  EditorMenu.Left := ClientWidth - EditorMenu.Width;
  EditorMenu.Height := ClientHeight - StatusBar.Height;
  EditorPages.Height := EditorMenu.Height - 184;
  //ObjectTypePages.Height := LayerPages.Height - 172;
  StatusBar.Panels[4].Width := ClientWidth - 720;
  if Map.loaded then
  begin
    render_minimap_position_marker;
    render_map;
  end;
end;

procedure TMainWindow.FormDeactivate(Sender: TObject);
begin
  Undo1.ShortCut := 0;
  Redo1.ShortCut := 0;
  Copy1.ShortCut := 0;
  Paste1.ShortCut := 0;
end;

procedure TMainWindow.FormActivate(Sender: TObject);
begin
  Undo1.ShortCut := 16474;
  Redo1.ShortCut := 16473;
  Copy1.ShortCut := 16451;
  Paste1.ShortCut := 16470;
end;

procedure TMainWindow.WMDropFiles(var Msg: TWMDropFiles);
var
  filename: string;
  length: integer;
begin
  length := DragQueryFile(Msg.Drop, 0, nil, 0);
  setlength(filename, length);
  DragQueryFile(Msg.Drop, 0, PChar(filename), length + 1);
  load_map_from_file(filename);
  DragFinish(Msg.Drop);
end;

procedure TMainWindow.CMDialogKey(var AMessage: TCMDialogKey);
begin
  if AMessage.CharCode = VK_TAB then
  begin
    if EditorPages.TabIndex < 2 then
    begin
      // Toggle background/foregrounf layer
      EditorPages.TabIndex := (EditorPages.TabIndex + 1) and 1;
      EditorPagesChange(nil);
    end else
    begin
      // Toggle object editing buttons

    end;
    AMessage.Result := 1;
  end else
    inherited;
end;

procedure TMainWindow.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  index: integer;
  step: integer;
begin
  cur_shift_state := Shift;
  case key of
    27: // Esc
    begin
      rbTileMode.Checked := true;
    end;
    32: // Space: open tileset window
    begin
      if mode(mTile) then
      begin
        if ssShift in shift then
          rbPatternMode.Checked := true;
        if (ssCtrl in shift) or mode(mTileMode) then
          rbBlockMode.Checked := true;
        BlockPresetDialog.Show;
        key := 0;
        exit;
      end;
    end;
    107: // Num+
    begin
      if EditorPages.ActivePageIndex = 0 then
      begin
        tbBrushWidth.Position := tbBrushWidth.Position + 1;
        tbBrushHeight.Position := tbBrushWidth.Position;
        render_editing_marker;
      end else
      if EditorPages.ActivePageIndex = 1 then
      begin
        seObjectAlignToFloor.Value := seObjectAlignToFloor.Value + 1;
        mouse_already_clicked := false;
        render_editing_marker;
      end;
    end;
    109: // Num-
    begin
      if EditorPages.ActivePageIndex = 0 then
      begin
        tbBrushWidth.Position := tbBrushWidth.Position - 1;
        tbBrushHeight.Position := tbBrushWidth.Position;
        render_editing_marker;
      end else
      if EditorPages.ActivePageIndex = 1 then
      begin
        seObjectAlignToFloor.Value := seObjectAlignToFloor.Value - 1;
        mouse_already_clicked := false;
        render_editing_marker;
      end;
    end;
    192: // ` (key under ESC)
    begin
      tbBrushWidth.Position := 1;
      tbBrushHeight.Position := 1;
      render_editing_marker;
    end;
  end;
  // Shift + arrows = same as Num keys
  if ssShift in Shift then
  begin
    case key of
      37: {Left arrow}  begin key := 100; Shift := []; end;
      38: {Up arrow}    begin key := 104; Shift := []; end;
      39: {Right arrow} begin key := 102; Shift := []; end;
      40: {Down arrow}  begin key := 98; Shift := []; end;
    end;
  end else
  // Arrows - scroll map
  if EditorPages.ActivePageIndex = 0 then
  case key of
    37: {Left arrow}  begin MapScrollH.Position := MapScrollH.Position - 1; key := 0; end;
    38: {Up arrow}    begin MapScrollV.Position := MapScrollV.Position - 1; key := 0; end;
    39: {Right arrow} begin MapScrollH.Position := MapScrollH.Position + 1; key := 0; end;
    40: {Down arrow}  begin MapScrollV.Position := MapScrollV.Position + 1; key := 0; end;
  end;
  // F1-F3 - Select page
  if (key >= 112) and (key <= 114) then
  begin
    index := key - 112;
    EditorPages.ActivePageIndex := index;
    EditorPagesChange(nil);
  end;
  // Shift+key
  if ssShift in Shift then
  begin
    // Brush size preset selection
    if (key >= ord('1')) and (key <= ord('8')) then
    begin
      tbBrushWidth.Position := key - ord('0');
      tbBrushHeight.Position := key - ord('0');
      render_editing_marker;
    end;
    // Editing mode selection
    case key of
      ord('E'): begin rbTileMode.Checked := true; end;
      ord('D'): begin rbPatternMode.Checked := true; end;
      ord('C'): begin rbBlockMode.Checked := true; end;
      ord('S'): cbAllLayers.Checked := not cbAllLayers.Checked;
      ord('A'): begin rbChangeTileType.Checked := true; cbxChangeTileType.ItemIndex := 0; cbxChangeTileTypeChange(nil); end;
      ord('Z'): begin rbChangeTileType.Checked := true; cbxChangeTileType.ItemIndex := 1; cbxChangeTileTypeChange(nil); end;
      ord('X'): begin rbChangeTileType.Checked := true; cbxChangeTileType.ItemIndex := 2; cbxChangeTileTypeChange(nil); end;
    end;
    exit;
  end;
  // Ctrl+key
  if ssCtrl in Shift then
  begin
    // View layer toggle
    case key of
      ord('B'): begin sbBackgroundLayer.Down := not sbBackgroundLayer.Down; end;
      ord('F'): begin sbForegroundLayer.Down := not sbForegroundLayer.Down; end;
      ord('A'): begin sbTransparentDraw.Down := not sbTransparentDraw.Down; end;
      ord('J'): begin sbShowObjects.Down := not sbShowObjects.Down; end;
      ord('X'): begin sbMarkTiles.Down := not sbMarkTiles.Down; end;
      ord('M'): begin sbShowMarkers.Down := not sbShowMarkers.Down; end;
      ord('G'): begin sbShowgrid.Down := not sbShowgrid.Down; end;
      else
        exit;
    end;
    RenderSettingChange(nil);
    exit;
  end;
  // Alphanumeric keys
  if (EditorPages.ActivePageIndex = 0) and (rbPatternMode.Checked or rbBlockMode.Checked) then
  begin
    // Block key presets
    if ((key >= ord('0')) and (key <= ord('9'))) or ((key >= ord('A')) and (key <= ord('Z'))) or (key = 186) or (key = 188) or (key = 190) or (key = 191) then
    begin
      if key = 188 then key := ord('<');
      if key = 190 then key := ord('>');
      if key = 186 then key := ord(':');
      if key = 191 then key := ord('?');
      cur_selected_preset[cur_preset_group] := Tileset.block_key_to_index(key);
      update_editing_mode;
      exit;
    end;
  end;
  // Numeric keyboard keys
  if mode(mTileMode) then
  begin
    case key of
      // Change current tile index
      98:  {Num2} step := 16;
      100: {Num4} step := -1;
      102: {Num6} step :=  1;
      104: {Num8} step :=-16;
      else
        exit;
    end;
    cur_tile_index := (cur_tile_index + step + cnt_tileset_tiles) mod cnt_tileset_tiles;
    update_editing_mode;
    exit;
  end else
  if mode(mPatternMode) then
  begin
    case key of
      // Rotate pattern
      98:  {Num2} Map.rotate_pattern(drDown);
      100: {Num4} Map.rotate_pattern(drLeft);
      102: {Num6} Map.rotate_pattern(drRight);
      104: {Num8} Map.rotate_pattern(drUp);
      else
        exit;
    end;
    draw_block_image;
    cur_selected_preset[bpgPatternPreset] := -1;
    exit;
  end else
  if mode(mBlockMode) then
  case key of
    // Move cursor image
    98:  {Num2} begin CursorImage.Top := CursorImage.Top + 32; resize_cursor_image; end;
    100: {Num4} begin CursorImage.Left := CursorImage.Left - 32; resize_cursor_image; end;
    102: {Num6} begin CursorImage.Left := CursorImage.Left + 32; resize_cursor_image; end;
    104: {Num8} begin CursorImage.Top := CursorImage.Top - 32; resize_cursor_image; end;
    end
end;

procedure TMainWindow.FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
var
  key: word;
begin
  if (MousePos.X - left) < (mapCanvas.Width + 30) then
  begin
    if (ssCtrl in Shift) and not selection_started then
    begin
      key := 107;
      FormKeyDown(Sender, key, Shift);
      Handled := true;
      exit;
    end;
    if (MousePos.Y - top) < (mapCanvas.Height - 30)
    then
      MapScrollV.Position := MapScrollV.Position - 2
    else
      MapScrollH.Position := MapScrollH.Position - 2;
    Handled := true;
  end else
  if EditorPages.ActivePageIndex = 0 then
  begin
    snTilesetOffsetUpClick(nil);
    Handled := true;
  end;
end;

procedure TMainWindow.FormMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
var
  key: word;
begin
  if (MousePos.X - left) < (mapCanvas.Width + 30) then
  begin
    if (ssCtrl in Shift) and not selection_started then
    begin
      key := 109;
      FormKeyDown(Sender, key, Shift);
      Handled := true;
      exit;
    end;
    if (MousePos.Y - top) < (mapCanvas.Height - 30)
    then
      MapScrollV.Position := MapScrollV.Position + 2
    else
      MapScrollH.Position := MapScrollH.Position + 2;
    Handled := true;
  end else
  if EditorPages.ActivePageIndex = 0 then
  begin
    snTilesetOffsetDownClick(nil);
    Handled := true;
  end;
end;

procedure TMainWindow.Newmap1Click(Sender: TObject);
var
  tileset_index: integer;
begin
  SetDialog.select_menu(4);
  if SetDialog.ModalResult = mrCancel then
    exit;
  SetDialog.select_menu(5);
  if SetDialog.ModalResult = mrCancel then
    exit;
  tileset_index := SetDialog.TilesetSelection_List.ItemIndex;
  Map.new_map(tileset_index, SetDialog.SetMapSize_Width.Value, SetDialog.SetMapSize_Height.Value);
  // Update status bar and title
  StatusBar.Panels[3].Text := inttostr(Map.width)+' x '+inttostr(Map.height);
  StatusBar.Panels[4].Text := 'Map not saved';
  set_window_titles('Untitled');
  SpritePropertiesDialog.update_contents;
  // Finish it
  resize_map_canvas;
  render_minimap;
  render_map;
  update_level_data;
  TilePropertiesDialog.update_contents;
end;

procedure TMainWindow.Openmap1Click(Sender: TObject);
var
  level_index: integer;
begin
  SetDialog.select_menu(3);
  if SetDialog.ModalResult = mrCancel then
    exit;
  level_index := SetDialog.LevelSelection_List.ItemIndex;
  load_map_from_archive(level_index);
end;

procedure TMainWindow.Importmap1Click(Sender: TObject);
begin
  if MapImportDialog.Execute then
  begin
    load_map_from_file(MapImportDialog.FileName);
  end;
end;


procedure TMainWindow.Reopenmap1Click(Sender: TObject);
begin
  if Map.loaded and (Map.index <> -1) then
    load_map_from_archive(Map.index);
end;

procedure TMainWindow.Savemap1Click(Sender: TObject);
begin
  if not Map.loaded then
    exit;
  if Map.index = -1 then
    Savemapas1Click(Sender)
  else begin
    if Settings.CheckMapErrorsOnSave then
      check_map_errors;
    save_map_to_archive(Map.index);
  end;
end;

procedure TMainWindow.Savemapas1Click(Sender: TObject);
var
  level_index: integer;
begin
  if not Map.loaded then
    exit;
  if Settings.CheckMapErrorsOnSave then
    check_map_errors;
  SetDialog.select_menu(3);
  if SetDialog.ModalResult = mrCancel then
    exit;
  level_index := SetDialog.LevelSelection_List.ItemIndex;
  save_map_to_archive(level_index);
end;

procedure TMainWindow.Exportmap1Click(Sender: TObject);
begin
  if not Map.loaded then
    exit;
  if Settings.CheckMapErrorsOnSave then
    check_map_errors;
  if MapExportDialog.Execute then
    save_map_to_file(MapExportDialog.FileName);
end;

procedure TMainWindow.Savemapimage1Click(Sender: TObject);
var
  tmp_bitmap: TBitmap;
begin
  if not Map.loaded then
    exit;
  if MapImageSaveDialog.Execute then
  begin
    tmp_bitmap := TBitmap.Create;
    tmp_bitmap.Width := Map.width * 32;
    tmp_bitmap.Height := Map.height * 32;
    Renderer.render_map_contents(tmp_bitmap.Canvas, 0, 0, Map.width, Map.height, 0, 0, Addr(Map.data), -1,
      sbBackgroundLayer.Down, sbForegroundLayer.Down, sbTransparentDraw.Down, sbShowObjects.Down, sbShowMarkers.Down, sbMarkTiles.Down, sbShowGrid.Down,
      false);
    tmp_bitmap.SaveToFile(MapImageSaveDialog.FileName);
    tmp_bitmap.Destroy;
  end;
end;

procedure TMainWindow.Exit1Click(Sender: TObject);
begin
  MainWindow.OnResize := nil;
  application.Terminate;
end;

procedure TMainWindow.Undo1Click(Sender: TObject);
begin
  Map.do_undo;
  render_minimap;
  render_map;
  update_level_data;
  mouse_already_clicked := false;
end;

procedure TMainWindow.Redo1Click(Sender: TObject);
begin
  Map.do_redo;
  render_minimap;
  render_map;
  update_level_data;
  mouse_already_clicked := false;
end;

procedure TMainWindow.Copy1Click(Sender: TObject);
var
  handle: THandle;
  pointer: ^TSelectionBlock;
begin
  if not Map.loaded or not mode(mBlockMode) then
    exit;
  OpenClipboard(Application.Handle);
  EmptyClipboard;

  handle := GlobalAlloc(GMEM_DDESHARE or GMEM_MOVEABLE, SizeOf(TSelectionBlock));
  pointer := GlobalLock(handle);

  Move(cur_block, pointer^, sizeof(TSelectionBlock));

  GlobalUnLock(handle);
  SetClipboardData(clipboard_format, handle);
  CloseClipboard;
end;

procedure TMainWindow.Paste1Click(Sender: TObject);
var
  handle: THandle;
  pointer: ^TSelectionBlock;
begin
  if not Map.loaded or not Clipboard.HasFormat(clipboard_format) then
    exit;
  OpenClipboard(Application.Handle);
  handle := GetClipboardData(clipboard_format);
  pointer := GlobalLock(handle);

  RbBlockMode.Checked := true;
  if EditorPages.ActivePageIndex <> 0 then
  begin
    EditorPages.TabIndex := 0;
    EditorPagesChange(nil);
  end;
  Move(pointer^, cur_block, sizeof(TSelectionBlock));
  draw_cursor_image;
  draw_block_image;

  GlobalUnLock(handle);
  CloseClipboard;
end;

procedure TMainWindow.Selecttileset1Click(Sender: TObject);
var
  tileset_index: integer;
begin
  SetDialog.select_menu(4);
  if SetDialog.ModalResult = mrCancel then
    exit;
  tileset_index := SetDialog.TilesetSelection_List.ItemIndex;
  Map.set_tileset(tileset_index);
  // Re-render everything
  render_minimap;
  render_map;
end;

procedure TMainWindow.Selectnext1Click(Sender: TObject);
var
  tileset_index: integer;
begin
  tileset_index := Tileset.current_tileset + 1;
  if tileset_index >= Archive.tileset_count then
    tileset_index := 0;
  Map.set_tileset(tileset_index);    
  // Re-render everything
  render_minimap;
  render_map;
end;

procedure TMainWindow.Loadcustomimage1Click(Sender: TObject);
begin
  if TilesetOpenDialog.Execute then
  begin
    Tileset.use_custom_image(TilesetOpenDialog.FileName);
    // Re-render everything
    render_map;
  end;
end;

procedure TMainWindow.Usepredefinedtiles1Click(Sender: TObject);
begin
  update_editing_mode;
end;

procedure TMainWindow.Setmapsize1Click(Sender: TObject);
begin
  SetDialog.select_menu(1);
  if SetDialog.ModalResult = mrCancel then
    exit;
  set_map_size(SetDialog.SetMapSize_Width.Value, SetDialog.SetMapSize_Height.Value);
end;

procedure TMainWindow.Shiftmap1Click(Sender: TObject);
begin
  SetDialog.select_menu(2);
  if SetDialog.ModalResult = mrCancel then
    exit;
  shift_map(SetDialog.shift_map_direction, SetDialog.ShiftMap_NumTiles.Value);
end;

procedure TMainWindow.Spriteproperties1Click(Sender: TObject);
begin
  SpritePropertiesDialog.Show;
end;

procedure TMainWindow.Tileproperties1Click(Sender: TObject);
begin
  TilePropertiesDialog.Show;
end;

procedure TMainWindow.LaunchGame1Click(Sender: TObject);
begin
  if not check_map_can_be_tested then
    exit;
  launch_game;
end;

procedure TMainWindow.TestMapDifficultyClick(Sender: TObject);
begin
  Settings.TestMapDifficulty := (Sender as TMenuItem).Tag;
end;

procedure TMainWindow.KeyShortcuts1Click(Sender: TObject);
begin
  Application.MessageBox(
              'Space = Open preset window'#13'Shift + Space = Pattern presets'#13'Ctrl + Space = Block presets'#13'Arrows = Scroll map'#13'Tab = Toggle tile / object mode'#13'Num +/- = Adjust brush size / object alignment'#13#13+
              'Num 2/4/6/8  or  Shift + Arrows:'#13'Tile mode: Change selected tile'#13'Pattern mode: Rotate pattern'#13'Block mode: Move block'#13#13+
              'Shift + 1 - 8 = Change brush size'#13'Shift + E = Tile mode'#13'Shift + D = Pattern mode'#13'Shift + C = Block mode'#13'Shift + A/Z/X = Change tile type mode'#13'Shift + S = Toggle Copy both layers'#13#13+
              '0 - 9, A - Z = Select block/pattern preset',
              'Key Shortcuts',
              MB_OK or MB_ICONINFORMATION
              );
end;

procedure TMainWindow.Mouseactions1Click(Sender: TObject);
begin
  Application.MessageBox(
              'Tile mode:'#13'Left = Paint tile'#13'Right = Erase tile (foreground layer only)'#13'Middle = Copy tile (priority fg/bg layer)'#13'Double click = Flood fill'#13'Ctrl + Left-Select = Fill selected area'#13'Ctrl + Right-Select = Erase selected area' + #13'Ctrl + Wheel = Change brush size'#13#13+
              'Pattern mode:'#13'Left = Paint pattern'#13'Right = Erase tile (foreground layer only)'#13'Middle = Copy single tile'#13'Double click = Flood fill'#13'Ctrl + Select = Fill selected area'#13'Shift + Select = Copy pattern from map'#13#13+
              'Block mode:'#13'Left = Place block'#13'Right + Move = Scroll map'#13'Middle = Copy single tile'#13'Shift + Select = Copy block from map'#13#13+
              'Change tile type mode:'#13'Left = Change tile type'#13'Shift + Left = Auto-draw shadows around wall'#13'Right = Reverse change'#13'Middle = Copy single tile'#13#13+
              'Object mode:'#13'Left = Place object / change position'#13'Shift + Left = Copy and place a new object'#13'Right = Erase object'#13'Middle = Select object'#13'Ctrl + Wheel = Adjust alignment to floor'#13#13+
              'Preset selection window:'#13'Left = Select preset'#13'Right = Delete preset'#13'Middle = Show/hide keys'#13#13+
              'Hold right button while selecting to scroll map.',
              'Mouse Actions',
              MB_OK or MB_ICONINFORMATION
              );
end;

procedure TMainWindow.About1Click(Sender: TObject);
begin
  ShowMessage('Vinyl Goddess from Mars Level Editor'#13#13+
              'Made by Hisymak (kozten@seznam.cz)'#13'Alpha 0.5'#13'Date: 2016-02-12'#13#13+
              'Game version: '+ Archive.archive_version +#13#13+
              'Special thanks to Frenkel Smeijers and Malvineous'#13'for reverse-engineering the VGFM archive and tileset format'#13'and providing the information on Modding Wiki.'#13'http://www.shikadi.net/moddingwiki/Vinyl_Goddess_From_Mars');
end;

procedure TMainWindow.MapScrollChange(Sender: TObject);
var
  pos: TPoint;
begin
  map_canvas_left := MapScrollH.Position;
  map_canvas_top := MapScrollV.Position;
  render_map;
  render_minimap_position_marker;
  // Simulate MouseMove event so that editing marker and coordinates are updated
  if mouse_over_map_canvas then
  begin
    pos := MapCanvas.ScreenToClient(Mouse.CursorPos);
    MapCanvasMouseMove(nil, cur_shift_state, pos.X, pos.Y);
  end
end;

procedure TMainWindow.MapCanvasMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  canvas_x, canvas_y: integer;
  map_x, map_y: integer;
  map_pixel_x, map_pixel_y: integer;
  Button: TMouseButton;
begin
  cur_shift_state := Shift;
  // Get tile coordinates
  canvas_x := X div 32;
  canvas_y := Y div 32;
  map_x := Min(Max(canvas_x + map_canvas_left, 0), Map.width - 1);
  map_y := Min(Max(canvas_y + map_canvas_top, 0), Map.height - 1);
  map_pixel_x := X div 2 + map_canvas_left * 16;
  map_pixel_y := Y div 2 + map_canvas_top * 16;
  // If mouse is still inside same tile, exit (for optimization)
  if ((not mode(mPixelCoords)) and (mouse_old_x = map_x) and (mouse_old_y = map_y))
    or ((mode(mPixelCoords)) and (mouse_old_pixel_x = map_pixel_x) and (mouse_old_pixel_y = map_pixel_y)) then
    exit;
  mouse_already_clicked := false;
  // Write coordinates on status bar
  StatusBar.Panels[0].Text := 'x: '+inttostr(map_x)+' y: '+inttostr(map_y);
  // Scroll map while holding right button
  if (ssRight in shift) and mode(mRightBtnScroll) then
  begin
    MapScrollH.Position := map_canvas_left + (mouse_old_x - map_x);
    MapScrollV.Position := map_canvas_top + (mouse_old_y - map_y);
  end else
  begin
    mouse_old_x := map_x;
    mouse_old_y := map_y;
    mouse_old_pixel_x := map_pixel_x;
    mouse_old_pixel_y := map_pixel_y;
  end;
  // Move cursor image and resize if exceeding map canvas border
  CursorImage.Left := canvas_x * 32 + MapCanvas.Left;
  CursorImage.Top := canvas_y * 32 + MapCanvas.Top;
  resize_cursor_image;
  // Move end of block selection
  if selection_started then
  begin
    selection_end_x := map_x;
    selection_end_y := map_y;
  end;
  // Redraw editing marker
  render_editing_marker;
  // If mouse button is held, paint with brush
  if ((ssLeft in Shift) or (ssRight in Shift)) and mode(mPainting) and not selection_started then
  begin
    if ssLeft in Shift then
      button := mbLeft
    else
      button := mbRight;
    MapCanvasMouseDown(sender,Button,Shift,x,y);
  end;
end;

procedure TMainWindow.MapCanvasMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  map_x, map_y: integer;
  i: integer;
  cursor_left: integer;
  cursor_top: integer;
  editing_marker_disabled: boolean;
  index: integer;
  obj: ^TLevelObject;
  door: ^TDoorEntry;
  switch: ^TSwitchEntry;
  lock: ^TLockEntry;
  obj_x, obj_y, obj_width, obj_height: word;
begin
  cur_shift_state := Shift;
  // Right button is used for map scrolling - do nothing
  if (button = mbRight) and mode(mRightBtnScroll) then
    exit;
  // Get map coordinates
  map_x := x div 32 + map_canvas_left;
  map_y := y div 32 + map_canvas_top;
  // Disable multiple clicks in the same tile
  if mouse_already_clicked and (mouse_last_button = Button) then
    exit;
  mouse_already_clicked := true;
  mouse_last_button := Button;
  editing_marker_disabled := false;

  // Select mode - Just start selection, the action will be performed after mouse button is released
  if mode(mSelecting) then
  begin
    // Start selection
    selection_started := true;
    selection_start_x := map_x;
    selection_start_y := map_y;
    selection_end_x := map_x;
    selection_end_y := map_y;
    render_editing_marker;
    exit;
  end;

  // Change tile type mode
  if mode(mChangeTileType) and (ord(Button) <= ord(mbRight)) then
  begin
    if ssShift in Shift then
      Map.smooth_edges(map_x, map_y, 0)
    else
      Map.paint_tile_rect(map_x, map_y, tbBrushWidth.Position, tbBrushHeight.Position, tile_mode_value[cbxChangeTileType.ItemIndex, ord(Button)]);
  end else

  // Middle mouse button
  if Button = mbMiddle then
  begin
    // Select a tile from map
    if mode(mTileMode) then
    begin
      // Get tile index from map
      cur_tile_index := Map.get_tile_index_prio(map_x, map_y);
      update_editing_mode;
    end else
    // Select a single tile as a pattern
    if mode(mPatternMode) then
    begin
      Map.copy_pattern(map_x, map_y, 1, 1);
      cur_selected_preset[bpgPatternPreset] := -1;
      draw_block_image;
    end else
    // Select single tile and switch to tile mode
    if mode(mBlockMode) or mode(mChangeTileType) then
    begin
      cur_tile_index := Map.get_tile_index_prio(map_x, map_y);
      rbTileMode.Checked := true;
    end;
    // Select object
    if mode(mObject) then
    begin
      index := get_object_under_mouse;
      if index = -1 then
        exit;
      lstObjectList.ItemIndex := index;
      lstObjectListClick(nil);
    end;
    // Select switch or lock
    if mode(mSwitch) or mode(mLock) then
    begin
      for i := 0 to Map.leveldata.numSwitches - 1 do
        if (Map.leveldata.switches[i].posX = map_x) and (Map.leveldata.switches[i].posY = map_y) then
        begin
          EditorPages.TabIndex := 3;
          EditorPagesChange(nil);
          lstSwitchList.ItemIndex := i;
          lstSwitchListClick(nil);
          break;
        end;
      for i := 0 to Map.leveldata.numLocks - 1 do
        if (Map.leveldata.locks[i].posX = map_x) and (Map.leveldata.locks[i].posY = map_y) then
        begin
          EditorPages.TabIndex := 4;
          EditorPagesChange(nil);
          lstLockList.ItemIndex := i;
          lstLockListClick(nil);
          break;
        end;
    end;
    // Nothing to render, exit
    exit;
  end else

  // Left mouse button
  if Button = mbLeft then
  begin
    // Paint tile
    if mode(mTileMode) then
    begin
        Map.paint_tile_rect(map_x, map_y, tbBrushWidth.Position, tbBrushHeight.Position, cur_tile_index);
    end else
    // Paint pattern
    if mode(mPatternMode) then
    begin
      Map.paint_tile_rect(map_x, map_y, tbBrushWidth.Position, tbBrushHeight.Position, tilemode_pattern);
    end else
    // Place block
    if mode(mBlockMode) then
    begin
      cursor_left := (CursorImage.Left - MapCanvas.Left) div 32 + map_canvas_left;
      cursor_top := (CursorImage.Top - MapCanvas.Top) div 32 + map_canvas_top;
      if (cursor_left <> map_x) or (cursor_top <> map_y) then
        // Enable additional clicks if cursor image was moved from mouse cursor position
        mouse_already_clicked := false;
      Map.put_block(cursor_left, cursor_top, Addr(cur_block));
    end else
    // Place object
    if mode(mObject) then
    begin
      get_object_mouse_position(obj_x, obj_y, obj_width, obj_height);
      // Place new object as a copy of selected one
      if ssShift in Shift then
      begin
        index := Map.copy_object(lstObjectList.ItemIndex, obj_x, obj_y);
        if index = -1 then
          exit;
        lstObjectList.Items[index] := get_object_listitem(index, Map.leveldata.objects[index].objType);
        lstObjectList.ItemIndex := index;
        update_object_entry(index);
      end else
      // Change position of selected object
      begin
        obj := Addr(Map.leveldata.objects[lstObjectList.ItemIndex]);
        obj.pixelX := obj_x;
        obj.pixelY := obj_y;
        update_object_entry(lstObjectList.ItemIndex);
      end;
    end else
    // Place door destination
    if mode(mDoor) then
    begin
      get_object_mouse_position(obj_x, obj_y, obj_width, obj_height);
      door := Addr(Map.leveldata.doors[lstDoorList.ItemIndex]);
      door.destPixelX := obj_x;
      door.destPixelY := obj_y;
      update_door_entry(lstDoorList.ItemIndex);
    end else
    // Place switch
    if mode(mSwitch) then
    begin
      if lstSwitchList.ItemIndex = -1 then
        exit;
      switch := Addr(Map.leveldata.switches[lstSwitchList.ItemIndex]);
      switch.posX := map_x;
      switch.posY := map_y;
      Map.level_data_update_flags := Map.level_data_update_flags + [ufSwitches];
      update_level_data;
    end else
    // Place lock
    if mode(mLock) then
    begin
      if lstLockList.ItemIndex = -1 then
        exit;
      lock := Addr(Map.leveldata.locks[lstLockList.ItemIndex]);
      lock.keyX := map_x-1;
      lock.keyY := map_y-1;
      lock.posX := map_x;
      lock.posY := map_y;
      Map.level_data_update_flags := Map.level_data_update_flags + [ufLocks];
      update_level_data;
    end else
    // Chane transformation block position
    if sbTransblockMove.Down then
    begin
      sbTransblockMove.Down := false;
      if lstTransblockList.ItemIndex >= 0 then
      begin
        Map.move_transblock(lstTransblockList.ItemIndex, map_x, map_y);
        update_level_data;
      end;
    end else
    // Place item
    if mode(mItem) then
    begin
      index := RandomRange(Map.leveldata.tileFirstItemType[cur_item_type], Map.leveldata.tileLastItemType[cur_item_type] + 1);
      Map.paint_tile_rect(map_x, map_y, 1, 1, index);
    end;
  end else

  // Right mouse button
  if button = mbRight then
  begin
    // Erase tiles
    if mode(mTileMode) or mode(mPatternMode) then
    begin
      Map.paint_tile_rect(map_x, map_y, tbBrushWidth.Position, tbBrushHeight.Position, tilemode_erase);
    end else
    // Erase items
    if mode(mItem) then
    begin
      Map.paint_tile_rect(map_x, map_y, 1, 1, tilemode_erase);
    end else
    // Remove object
    if mode(mObject) then
    begin
      index := get_object_under_mouse;
      if index = -1 then
        exit;
      Map.remove_object(index);
      lstObjectList.Items[index] := get_object_listitem(index, 65535);
      if index = lstObjectList.ItemIndex then
        update_object_entry(lstObjectList.ItemIndex);
    end;
  end;

  // Do not update statistics and level data while painting but at end of painting
  if not mode(mPainting) then
  begin
    Map.compute_statistics;
    update_level_data;
  end;
  // Finally render changes in map
  render_minimap;
  render_map;
  if editing_marker_disabled then
    Renderer.remove_editing_marker(MapCanvas.Canvas);
end;

procedure TMainWindow.MapCanvasDblClick(Sender: TObject);
begin
  // Double click for filling area
  if mode(mPainting) and not mode(mChangeTileType) then
  begin
    if rbTileMode.Checked then
      Map.fill_area_start(mouse_old_x, mouse_old_y, cur_tile_index)
    else if rbPatternMode.Checked then
      Map.fill_area_start(mouse_old_x, mouse_old_y, tilemode_pattern);
    Map.compute_statistics;
    render_minimap;
    render_map;
    update_level_data;
  end;
end;

procedure TMainWindow.MapCanvasMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  min_x, max_x, min_y, max_y: word;
  size_x, size_y: word;
  door: ^TDoorEntry;
  switch: ^TSwitchEntry;
  index: integer;
begin
  cur_shift_state := Shift;
  // Right button is used for map scrolling - do nothing
  if (button = mbRight) and mode(mRightBtnScroll) then
    exit;
  // Finish selection
  if selection_started then
  begin
    selection_started := false;
    StatusBar.Panels[1].Text := '';
    min_x := Min(selection_start_x, selection_end_x);
    max_x := Max(selection_start_x, selection_end_x);
    min_y := Min(selection_start_y, selection_end_y);
    max_y := Max(selection_start_y, selection_end_y);
    size_x := max_x - min_x + 1;
    size_y := max_y - min_y + 1;
    if mode(mTile) then
    begin
      // Use special tile mode
      if mode(mChangeTileType) and (ord(Button) <= ord(mbRight)) then
      begin
        Map.paint_tile_rect(min_x, min_y, size_x, size_y, tile_mode_value[cbxChangeTileType.ItemIndex, ord(Button)]);
      end else
      // Erase tiles
      if (mode(mTileMode) or mode(mPatternMode)) and (Button = mbRight) then
        Map.paint_tile_rect(min_x, min_y, size_x, size_y, tilemode_erase)
      // Paint tiles
      else if mode(mTileMode) and (ssCtrl in Shift) then
      begin
        Map.paint_tile_rect(min_x, min_y, size_x, size_y, cur_tile_index)
      end
      // Paint pattern
      else if mode(mPatternMode) and (ssCtrl in Shift) then
        Map.paint_tile_rect(min_x, min_y, size_x, size_y, tilemode_pattern)
      // Select pattern
      else if mode(mPatternMode) and (ssShift in Shift) then
      begin
        Map.copy_pattern(min_x, min_y, size_x, size_y);
        cur_selected_preset[bpgPatternPreset] := -1;
        btnSavePreset.Caption := 'Save pattern as preset';
        btnSavePreset.Visible := true;
        draw_block_image;
      end
      // Select and copy block
      else if mode(mBlockMode) then
      begin
        Map.copy_block(min_x, min_y, size_x, size_y, cbAllLayers.Checked, Addr(cur_block));
        cur_selected_preset[bpgBlockPreset] := -1;
        if not cbAllLayers.Checked and (size_x <= max_block_preset_size) and (size_y <= max_block_preset_size) then
        begin
          btnSavePreset.Caption := 'Save block as preset';
          btnSavePreset.Visible := true;
        end;
        draw_cursor_image;
        draw_block_image;
        exit;
      end;
    end else
    // Place door entrance
    if mode(mDoor) then
    begin
      door := Addr(Map.leveldata.doors[lstDoorList.ItemIndex]);
      door.minX := min_x;
      door.maxX := max_x;
      door.minY := min_y;
      door.maxY := max_y;
      if cbDoorAutoSet.Checked then
      begin
        index := IfThen((lstDoorList.ItemIndex and 1) = 0, lstDoorList.ItemIndex + 1, lstDoorList.ItemIndex - 1);
        Map.level_data.doors[index].destPixelX := min_x * 16 + 6;
        Map.level_data.doors[index].destPixelY := min_y * 16 + 10;
      end;
      Map.level_data_update_flags := Map.level_data_update_flags + [ufDoors];
      update_level_data;
    end else
    // Select transformation block
    if sbTransblockSelect.Down then
    begin
      sbTransblockSelect.Down := false;
      if lstTransblockList.ItemIndex >= 0 then
      begin
        Map.select_transblock_from_map(lstTransblockList.ItemIndex, min_x, min_y, size_x, size_y);
        update_level_data;
      end;
    end else
    // Select remove-fg-tiles-switch area
    if sbSwitchT4Area.Down then
    begin
      sbSwitchT4Area.Down := false;
      if lstSwitchList.ItemIndex >= 0 then
      begin
        switch := Addr(Map.leveldata.switches[lstSwitchList.ItemIndex]);
        switch.var1 := min_x;
        switch.var2 := min_y;
        switch.var3 := size_x;
        switch.var4 := size_y;
        update_switch_entry(lstSwitchList.ItemIndex);
      end;
    end;
    Map.compute_statistics;
    render_minimap;
    render_map;
  end else
  if mode(mPainting) then
  begin
    Map.compute_statistics;
    update_level_data;
  end;
end;

procedure TMainWindow.CursorImageMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  MapCanvasMouseMove(Sender, Shift, X + CursorImage.Left - MapCanvas.Left, Y + CursorImage.Top - MapCanvas.Top);
end;

procedure TMainWindow.CursorImageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  MapCanvasMousedown(Sender, Button, Shift, X + CursorImage.Left - MapCanvas.Left, Y + CursorImage.Top - MapCanvas.Top);
end;

procedure TMainWindow.ImageMouseLeave(Sender: TObject);
begin
  if (Sender <> MapCanvas) and (Sender <> CursorImage) then
    exit;
  if mouse_over_map_canvas then
    exit;
  StatusBar.Panels[0].Text := '';
  StatusBar.Panels[1].Text := '';
  // Reset mouse position to a value outside of map range
  mouse_old_x := max_map_width;
  mouse_old_y := max_map_height;
  // Remove editing markers
  render_editing_marker;
end;

procedure TMainWindow.MiniMapMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not Map.loaded then
    exit;
  if (x < mmap_border_x) or (y < mmap_border_y) or (x > MiniMap.Width - mmap_border_x) or (y > MiniMap.Height - mmap_border_y) then
    exit;
  MapScrollH.Position := x - mmap_border_x - (map_canvas_width div 2);
  MapScrollV.Position := y - mmap_border_y - (map_canvas_height div 2);
end;

procedure TMainWindow.MiniMapMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if ssLeft in Shift then
    MiniMapMouseDown(Sender, mbLeft, Shift, X, Y);
end;

procedure TMainWindow.RenderSettingChange(Sender: TObject);
begin
  render_map;
end;

procedure TMainWindow.EditorPagesChange(Sender: TObject);
begin
  case EditorPages.ActivePageIndex of
    3: begin pnTansblock.Parent := PageSwitches; update_switch_entry(lstSwitchList.ItemIndex); end;
    4: begin pnTansblock.Parent := PageLocks; pnTansblock.Visible := true; end;
  end;
  sbTransblockSelect.Down := false;
  sbTransblockMove.Down := false;
  sbSwitchT4Area.Down := false;
  update_editing_mode;
  update_level_data;
end;

procedure TMainWindow.BlockImageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  scale, border_x, border_y: integer;
  xx, yy: integer;
begin
  if Button = mbLeft then
  begin
    if rbTileMode.Checked then
    begin
      cur_tile_index := cur_tileset_offset * 256 + (Y div 16) * 16 + (X div 16);
      update_editing_mode;
    end else
    if rbPatternMode.Checked or rbBlockMode.Checked then
    begin
      BlockPresetDialog.Show;
    end;
  end else
  if Button = mbRight then
  begin
    if rbPatternMode.Checked then
    begin
      scale := IfThen((Map.pattern.width > 9) or (Map.pattern.height > 9), 1, 2);
      border_x := (BlockImage.Width - Map.pattern.width * 16 * scale) div 2;
      border_y := (BlockImage.Height - Map.pattern.height * 16 * scale) div 2;
      xx := (x - border_x) div (16 * scale);
      yy := (y - border_y) div (16 * scale);
      if (xx < 0) or (xx >= Map.pattern.width) or (yy < 0) or (yy >= Map.pattern.height) then
        exit;
      Map.cur_pattern.tiles[xx,yy] := tile_no_change;
      draw_block_image;
    end else
    if rbBlockMode.Checked then
    begin
      border_x := (BlockImage.Width - cur_block.width * 32) div 2;
      border_y := (BlockImage.Height - cur_block.height * 32) div 2;
      xx := (x - border_x) div 32;
      yy := (y - border_y) div 32;
      if (xx < 0) or (xx >= cur_block.width) or (yy < 0) or (yy >= cur_block.height) then
        exit;
      cur_block.data[xx,yy].layers[0] := tile_no_change;
      cur_block.data[xx,yy].layers[1] := tile_no_change;
      draw_cursor_image;
      draw_block_image;
    end;
  end;
end;

procedure TMainWindow.EditingModeChange(Sender: TObject);
begin
  if rbPatternMode.Checked then
    cur_preset_group := bpgPatternPreset
  else
    cur_preset_group := bpgBlockPreset;
  snTilesetOffset.Visible := rbTileMode.Checked;
  cbAllLayers.Visible := rbBlockMode.Checked;
  pnBrushSize.Visible := not rbBlockMode.Checked;
  if cbxChangeTileType.Visible and not rbChangeTileType.Checked then
    cbxChangeTileTypeChange(nil);
  cbxChangeTileType.Visible := rbChangeTileType.Checked;
  if rbChangeTileType.Checked then
    cbxChangeTileTypeChange(nil);
  update_editing_mode;
  if MainWindow.Visible then
    EditorPages.SetFocus;
  btnSavePreset.Visible := False;
end;

procedure TMainWindow.snTilesetOffsetUpClick(Sender: TObject);
begin
  cur_tileset_offset := 0;
  draw_block_image;
end;

procedure TMainWindow.snTilesetOffsetDownClick(Sender: TObject);
begin
  cur_tileset_offset := 1;
  draw_block_image;
end;

procedure TMainWindow.cbxChangeTileTypeChange(Sender: TObject);
begin
  sbMarkTiles.Down := (cbxChangeTileType.ItemIndex > 0) and mode(mChangeTileType);
  RenderSettingChange(nil);
end;

procedure TMainWindow.tbBrushSizeChange(Sender: TObject);
begin
  lbBrushWidthVal.Caption := inttostr(tbBrushWidth.Position);
  lbBrushHeightVal.Caption := inttostr(tbBrushHeight.Position);
end;

procedure TMainWindow.btnSavePresetClick(Sender: TObject);
var
  preset_index: integer;
  tmp_preset: TBlockPreset;
  x, y: integer;
begin
  BlockPresetDialog.select_preset_to_save;
  if BlockPresetDialog.ModalResult <> mrOk then
    exit;
  preset_index := BlockPresetDialog.preset_to_save;
  if preset_index = -1 then
    exit;
  if mode(mPatternMode) then
  begin
    Tileset.save_preset(Addr(Map.pattern), bpgPatternPreset, preset_index);
    cur_selected_preset[bpgPatternPreset] := preset_index;
  end else
  if mode(mBlockMode) then
  begin
    tmp_preset.width := cur_block.width;
    tmp_preset.height := cur_block.height;
    for x := 0 to tmp_preset.width - 1 do
      for y := 0 to tmp_preset.height - 1 do
        tmp_preset.tiles[x, y] := IfThen(cur_block.data[x, y].layers[0] <> tile_no_change, cur_block.data[x, y].layers[0], cur_block.data[x, y].layers[1]);
    Tileset.save_preset(Addr(tmp_preset), bpgBlockPreset, preset_index);
    cur_selected_preset[bpgBlockPreset] := preset_index;
  end;
  BlockPresetDialog.update_presets(cur_preset_group);
  btnSavePreset.Visible := false;
end;

procedure TMainWindow.sbObjectClick(Sender: TObject);
begin
  // Clicked on button which is already active - keep it active
  if cur_item_speedbtn = (Sender as TSpeedButton) then
  begin
    cur_item_speedbtn.Down := true;
    exit;
  end;
  // Deactivate active button
  if cur_item_speedbtn <> nil then
  begin
    cur_item_speedbtn.AllowAllUp := true;
    cur_item_speedbtn.Down := false;
  end;
  // Activate the clicked button
  cur_item_speedbtn := (Sender as TSpeedButton);
  cur_item_speedbtn.Down := true;
  cur_item_speedbtn.AllowAllUp := false;
  cur_item_type := cur_item_speedbtn.Tag;
  render_editing_marker;
end;

procedure TMainWindow.lstObjectListClick(Sender: TObject);
begin
  update_object_entry(lstObjectList.ItemIndex);
end;

procedure TMainWindow.lstObjectListDblClick(Sender: TObject);
var
  obj: ^TLevelObject;
begin
  obj := Addr(Map.leveldata.objects[lstObjectList.ItemIndex]);
  center_map_to(obj.pixelX div 16, obj.pixelY div 16);
end;

procedure TMainWindow.lstDoorListClick(Sender: TObject);
begin
  update_door_entry(lstDoorList.ItemIndex);
end;

procedure TMainWindow.lstDoorListDblClick(Sender: TObject);
var
  door: ^TDoorEntry;
begin
  door := Addr(Map.leveldata.doors[lstDoorList.ItemIndex]);
  if sbDoorEntrance.Down then
    center_map_to(door.minX, door.minY)
  else
    center_map_to(door.destPixelX div 16, door.destPixelY div 16);
end;

procedure TMainWindow.lstSwitchListClick(Sender: TObject);
begin
  update_switch_entry(lstSwitchList.ItemIndex);
end;

procedure TMainWindow.lstSwitchListDblClick(Sender: TObject);
var
  switch: ^TSwitchEntry;
begin
  switch := Addr(Map.leveldata.switches[lstSwitchList.ItemIndex]);
  center_map_to(switch.posX, switch.posY);
end;

procedure TMainWindow.lstLockListClick(Sender: TObject);
begin
  update_lock_entry(lstLockList.ItemIndex);
end;

procedure TMainWindow.lstLockListDblClick(Sender: TObject);
var
  lock: ^TLockEntry;
begin
  lock := Addr(Map.leveldata.locks[lstLockList.ItemIndex]);
  center_map_to(lock.posX, lock.posY);
end;

procedure TMainWindow.lstTransblockListClick(Sender: TObject);
begin
  update_transblock_entry(lstTransblockList.ItemIndex);
end;

procedure TMainWindow.lstTransblockListDblClick(Sender: TObject);
var
  transblock: TTransformationBlockPtr;
begin
  transblock := Map.get_transblock(lstTransblockList.ItemIndex);
  center_map_to(transblock.posX, transblock.posY);
end;

procedure TMainWindow.cbxObjectTypeChange(Sender: TObject);
var
  obj_index: integer;
  obj_behavior: word;
  i: integer;
begin
  render_editing_marker;
  if updating then
    exit;
  obj_index := lstObjectList.ItemIndex;
  lstObjectList.Items[obj_index] := get_object_listitem(obj_index, cbxObjectType.ItemIndex);
  // Set default behavior and property values for that object
  obj_behavior := ObjectInfo.get_obj_def_behavior(cbxObjectType.ItemIndex);
  cbxObjectBehavior.ItemIndex := obj_behavior;
  cbxObjectBehaviorChange(nil);
  for i := 1 to 5 do
    obj_prop_spinedits[i].Value := ObjectInfo.get_obj_def_property(cbxObjectType.ItemIndex, obj_behavior, i);
end;

procedure TMainWindow.cbxObjectBehaviorChange(Sender: TObject);
var
  behv: ^TObjBehaviorInfo;
  i: integer;
begin
  behv := Addr(ObjectInfo.behaviors[cbxObjectBehavior.ItemIndex]);
  for i := 1 to 5 do
    if behv.prop_names[i] <> '' then
      obj_prop_labels[i].Caption := behv.prop_names[i] + ':'
    else
      obj_prop_labels[i].Caption := '';
  ObjectPropertyChange(nil);
end;

procedure TMainWindow.ObjectPropertyChange(Sender: TObject);
var
  obj: ^TLevelObject;
begin
  if updating then
    exit;
  obj := Addr(Map.leveldata.objects[lstObjectList.ItemIndex]);
  obj.objType := cbxObjectType.ItemIndex;
  obj.var1 := strtointdef(seObjectVar1.Text, 0);
  obj.var2 := strtointdef(seObjectVar2.Text, 0);
  obj.var3 := strtointdef(seObjectVar3.Text, 0);
  obj.var4 := strtointdef(seObjectVar4.Text, 0);
  obj.var5 := strtointdef(seObjectVar5.Text, 0);
  obj.behavior := cbxObjectBehavior.ItemIndex;
end;

procedure TMainWindow.btnObjectClearClick(Sender: TObject);
begin
  Map.remove_object(lstObjectList.ItemIndex);
  lstObjectList.Items[lstObjectList.ItemIndex] := get_object_listitem(lstObjectList.ItemIndex, 65535);
  update_object_entry(lstObjectList.ItemIndex);
  render_map;
end;

procedure TMainWindow.btnObjectCopyClick(Sender: TObject);
var
  new_index: integer;
begin
  new_index := Map.copy_object(lstObjectList.ItemIndex, 0, 0);
  if new_index = -1 then
    exit;
  lstObjectList.Items[new_index] := get_object_listitem(new_index, Map.leveldata.objects[new_index].objType);
  lstObjectList.ItemIndex := new_index;
  lstObjectListClick(nil);
  render_map;
end;

procedure TMainWindow.btnDoorAddClick(Sender: TObject);
begin
  if Map.leveldata.numDoors = Length(Map.leveldata.doors) then
    exit;
  FillChar(Map.level_data.doors[Map.level_data.numDoors], sizeof(TDoorEntry), 0);
  Map.level_data.doors[Map.level_data.numDoors].borderRight := Map.leveldata.borderRight;
  Map.level_data.doors[Map.level_data.numDoors].borderBottom := Map.leveldata.borderBottom;
  Inc(Map.level_data.numDoors);
  Map.level_data_update_flags := Map.level_data_update_flags + [ufDoors];
  update_level_data;
end;

procedure TMainWindow.btnDoorRemoveClick(Sender: TObject);
begin
  if Map.leveldata.numDoors = 0 then
    exit;
  Dec(Map.level_data.numDoors);
  Map.level_data_update_flags := Map.level_data_update_flags + [ufDoors];
  render_map;
  update_level_data;
end;

procedure TMainWindow.seDoorBorderChange(Sender: TObject);
var
  door: ^TDoorEntry;
begin
  if updating then
    exit;
  door := Addr(Map.leveldata.doors[lstDoorList.itemIndex]);
  door.borderLeft := strToIntDef(seDoorBorderLeft.Text, 0);
  door.borderRight := strToIntDef(seDoorBorderRight.Text, 0);
  door.borderTop := strToIntDef(seDoorBorderTop.Text, 0);
  door.borderBottom := strToIntDef(seDoorBorderBottom.Text, 0);
  render_map;
end;

procedure TMainWindow.cbxSwitchTypeChange(Sender: TObject);
begin
  if lstSwitchList.ItemIndex = -1 then
    exit;
  Map.level_data.switches[lstSwitchList.ItemIndex].switchType := cbxSwitchType.ItemIndex;
  Map.level_data_update_flags := Map.level_data_update_flags + [ufSwitches];
  update_level_data;
end;

procedure TMainWindow.SwitchPropertyChange(Sender: TObject);
var
  switch: ^TSwitchEntry;
begin
  if updating then
    exit;
  if lstSwitchList.ItemIndex = -1 then
    exit;
  switch := Addr(Map.leveldata.switches[lstSwitchList.ItemIndex]);
  case switch.switchType of
    1: switch.var1 := strtointdef(seSwitchT1ObjNum.Text, 0);
    3: begin
        switch.var1 := cbxSwitchT3Transblock.ItemIndex;
        lstTransblockList.ItemIndex := switch.var1;
        lstTransblockListClick(nil);
      end;
    5: begin
        switch.var1 := strtointdef(seSwitchT5FirstObj.Text, 0);
        switch.var2 := strtointdef(seSwitchT5LastObj.Text, 0);
      end;
  end;
end;

procedure TMainWindow.btnSwitchAddClick(Sender: TObject);
begin
  if Map.leveldata.numSwitches = Length(Map.leveldata.switches) then
    exit;
  FillChar(Map.level_data.switches[Map.level_data.numSwitches], sizeof(TSwitchEntry), 0);
  Map.level_data.switches[Map.level_data.numSwitches].initialState := 1;
  Map.level_data.switches[Map.level_data.numSwitches].switchType := 1;
  Inc(Map.level_data.numSwitches);
  Map.level_data_update_flags := Map.level_data_update_flags + [ufSwitches];
  update_level_data;
  lstSwitchList.ItemIndex := Map.leveldata.numSwitches - 1;
  lstSwitchListClick(nil);
end;

procedure TMainWindow.btnSwitchRemoveClick(Sender: TObject);
begin
  if Map.leveldata.numSwitches = 0 then
    exit;
  Dec(Map.level_data.numSwitches);
  Map.level_data_update_flags := Map.level_data_update_flags + [ufSwitches];
  render_map;
  update_level_data;
end;

procedure TMainWindow.cbxLockTypeChange(Sender: TObject);
begin
  if lstLockList.ItemIndex = -1 then
    exit;
  Map.level_data.locks[lstLockList.ItemIndex].messageNumber := cbxLockType.ItemIndex;
  Map.level_data_update_flags := Map.level_data_update_flags + [ufLocks];
  update_level_data;
end;

procedure TMainWindow.cbxLockTransblockChange(Sender: TObject);
begin
  lstTransblockList.ItemIndex := cbxLockTransblock.ItemIndex;
  lstTransblockListClick(nil);
  if lstLockList.ItemIndex = -1 then
    exit;
  Map.level_data.locks[lstLockList.ItemIndex].transblockNumber := cbxLockTransblock.ItemIndex;
end;

procedure TMainWindow.btnLockAddClick(Sender: TObject);
begin
  if Map.leveldata.numLocks = Length(Map.leveldata.locks) then
    exit;
  FillChar(Map.level_data.locks[Map.level_data.numLocks], sizeof(TLockEntry), 0);
  Map.level_data.locks[Map.level_data.numLocks].transblockNumber := 65535;
  Inc(Map.level_data.numLocks);
  Map.level_data_update_flags := Map.level_data_update_flags + [ufLocks];
  update_level_data;
  lstLockList.ItemIndex := Map.leveldata.numLocks - 1;
  lstLockListClick(nil);
end;

procedure TMainWindow.btnLockRemoveClick(Sender: TObject);
begin
  if Map.leveldata.numLocks = 0 then
    exit;
  Dec(Map.level_data.numLocks);
  Map.level_data_update_flags := Map.level_data_update_flags + [ufLocks];
  render_map;
  update_level_data;
end;

procedure TMainWindow.btnTransblockAddClick(Sender: TObject);
begin
  Map.add_transblock;
  update_level_data;
  lstTransblockList.ItemIndex := Map.leveldata.numTransblocks - 1;
  lstTransblockListClick(nil);
  if (mode(mLock)) and (lstLockList.ItemIndex >= 0) then
  begin
    cbxLockTransblock.ItemIndex := lstTransblockList.ItemIndex;
    cbxLockTransblockChange(nil);
  end;
  if (mode(mSwitch)) and (lstSwitchList.ItemIndex >= 0) and (Map.leveldata.switches[lstSwitchList.ItemIndex].switchType = 3) then
  begin
    cbxSwitchT3Transblock.ItemIndex := lstTransblockList.ItemIndex;
    SwitchPropertyChange(nil);
  end;
end;

procedure TMainWindow.btnTransblockRemoveClick(Sender: TObject);
begin
  Map.remove_transblock;
  render_map;
  update_level_data;
end;

procedure TMainWindow.seCapsuleCountChange(Sender: TObject);
begin
  if updating then
    exit;
  Map.level_data.capsuleCount := strToIntDef(seCapsuleCount.Text, 0);
end;

procedure TMainWindow.seBorderChange(Sender: TObject);
begin
  if updating then
    exit;
  Map.level_data.borderLeft := strToIntDef(seBorderLeft.Text, 0);
  Map.level_data.borderRight := strToIntDef(seBorderRight.Text, 0);
  Map.level_data.borderTop := strToIntDef(seBorderTop.Text, 0);
  Map.level_data.borderBottom := strToIntDef(seBorderBottom.Text, 0);
  Map.level_data.mapFinishPixelX := strToIntDef(seMapFinishPixelX.Text, 0) * 16;
  Map.level_data.mapFinishPixelY := strToIntDef(seMapFinishPixelY.Text, 0) * 16;
  render_map;
end;

procedure TMainWindow.resize_map_canvas;
begin
  map_canvas_width := (ClientWidth - 308) div 32;
  if map_canvas_width > Map.width then
    map_canvas_width := Map.width;
  map_canvas_height := (ClientHeight - 50) div 32;
  if map_canvas_height > Map.height then
    map_canvas_height := Map.height;
  MapCanvas.Picture.Bitmap.Width := map_canvas_width * 32;
  MapCanvas.Picture.Bitmap.Height := map_canvas_height * 32;
  MapCanvas.Width := map_canvas_width * 32;
  MapCanvas.Height := map_canvas_height * 32;
  MapScrollH.Top := map_canvas_height * 32 + 8;
  MapScrollH.Width := map_canvas_width * 32;
  MapScrollH.Visible := MapScrollH.Width > 0;
  MapScrollH.Max := Map.width - map_canvas_width;
  if Map.width = map_canvas_width then
    MapScrollH.Enabled := False
  else
    MapScrollH.Enabled := True;
  MapScrollV.Left := map_canvas_width * 32 + 8;
  MapScrollV.Height := map_canvas_height * 32;
  MapScrollV.Visible := MapScrollV.Height > 0;
  MapScrollV.Max := Map.height - map_canvas_height;
  if Map.height = map_canvas_height then
    MapScrollV.Enabled := False
  else
    MapScrollV.Enabled := True;
  mmap_border_x := (max_map_width - Map.width) div 2;
  mmap_border_y := (max_map_height - Map.height) div 2;
end;

procedure TMainWindow.render_map;
begin
  if not Map.loaded then
    exit;
  Renderer.render_map_contents(MapCanvas.Canvas, map_canvas_left, map_canvas_top, map_canvas_width, map_canvas_height, 0, 0, Addr(Map.data), IfThen(EditorPages.ActivePageIndex = 2, lstDoorList.ItemIndex, -1),
    sbBackgroundLayer.Down, sbForegroundLayer.Down, sbTransparentDraw.Down, sbShowObjects.Down, sbShowMarkers.Down, sbMarkTiles.Down, sbShowGrid.Down,
    true);
  render_editing_marker;
end;

procedure TMainWindow.render_minimap;
begin
  if not Map.loaded then
    exit;
  Renderer.render_minimap_contents(minimap_buffer.Canvas, Addr(Map.data), Map.width, Map.height);
  render_minimap_position_marker;
end;

procedure TMainWindow.render_minimap_position_marker;
begin
  MiniMap.Canvas.CopyRect(rect(0,0,MiniMap.Width,MiniMap.Height),minimap_buffer.Canvas,rect(0,0,MiniMap.Width,MiniMap.Height));
  MiniMap.Canvas.Pen.Color:= $00FF00;
  MiniMap.Canvas.Brush.Style := bsClear;
  MiniMap.Canvas.Rectangle(mmap_border_x + map_canvas_left,mmap_border_y + map_canvas_top,mmap_border_x + map_canvas_left + map_canvas_width,mmap_border_y + map_canvas_top + map_canvas_height);
  MiniMap.Canvas.Brush.Style := bsSolid;
end;

procedure TMainWindow.render_editing_marker;
var
  min_x, min_y, max_x, max_y: integer;
  obj_x, obj_y, obj_width, obj_height: word;
  mark_color: TColor;
  transblock: TTransformationBlockPtr;
begin
  if not mouse_over_map_canvas then
  begin
    Renderer.remove_editing_marker(MapCanvas.Canvas);
    CursorImage.Visible := false;
    exit;
  end;

  mark_color := clRed;
  if Settings.DrawObjectBrush and mode(mPixelCoords) then
  begin
    if mode(mDoor) then
      mark_color := $C04080
    else if cbxObjectType.ItemIndex = -1 then
    begin
      Renderer.remove_editing_marker(MapCanvas.Canvas);
      exit;
    end else
      mark_color := Objectinfo.get_obj_mark_color(cbxObjectType.ItemIndex);
    get_object_mouse_position(obj_x, obj_y, obj_width, obj_height);
    StatusBar.Panels[1].Text := format('%d.%.2d , %d.%.2d', [obj_x div 16, obj_x mod 16, obj_y div 16, obj_y mod 16]);
    // Draw object placement marker
    Renderer.draw_editing_marker(MapCanvas.Canvas, map_canvas_left, map_canvas_top, map_canvas_width, map_canvas_height,
      Addr(Map.data), obj_x, obj_y, obj_width, obj_height, psDot, mark_color, '');
  end else
  if Settings.DrawObjectBrush and mode(mSingleTileThing) then
  begin
    if mode(mSwitch) and (lstSwitchList.ItemIndex <> -1) then
      mark_color := $00FFFF
    else if mode(mLock) and (lstLockList.ItemIndex <> -1) then
      mark_color := $40A000
    else if mode(mItem) then
      mark_color := $8080FF;
    if mark_color <> clRed then
      Renderer.draw_editing_marker(MapCanvas.Canvas, map_canvas_left, map_canvas_top, map_canvas_width, map_canvas_height,
        Addr(Map.data), mouse_old_x * 16, mouse_old_y * 16, 16, 16, psDot, mark_color, '');
  end else
  if sbTransblockMove.Down and (lstTransblockList.ItemIndex >= 0) then
  begin
    mark_color := $FFC040;
    transblock := Map.get_transblock(lstTransblockList.ItemIndex);
    Renderer.draw_editing_marker(MapCanvas.Canvas, map_canvas_left, map_canvas_top, map_canvas_width, map_canvas_height,
      Addr(Map.data), mouse_old_x * 16, mouse_old_y * 16, transblock.width*16, transblock.height*16, psDot, mark_color, '');
  end else
  if selection_started then
  begin
    // Draw border around selected block on map
    min_x := min(selection_start_x, selection_end_x);
    max_x := max(selection_start_x, selection_end_x);
    min_y := min(selection_start_y, selection_end_y);
    max_y := max(selection_start_y, selection_end_y);
    mark_color := clMaroon;
    if mode(mDoor) then
      mark_color := $C04080;
    if mode(mTileMode) or (mode(mPatternMode) and not (ssShift in cur_shift_state)) or (mode(mBlockMode) and not cbAllLayers.Checked) then
      mark_color := layer_marker_color;
    if sbTransblockSelect.Down then
      mark_color := $FFC040;
    if sbSwitchT4Area.Down then
      mark_color := $4040FF;
    Renderer.draw_editing_marker(MapCanvas.Canvas, map_canvas_left, map_canvas_top, map_canvas_width, map_canvas_height,
      Addr(Map.data), min_x * 16, min_y * 16, (max_x-min_x+1) * 16, (max_y-min_y+1) * 16, psSolid, mark_color, '');
    StatusBar.Panels[1].Text := inttostr(max_x-min_x+1) + ' x ' + inttostr(max_y-min_y+1);
  end else
  if Settings.DrawPaintBrush and mode(mPainting) then
  begin
    // Draw paint brush marker
    Renderer.draw_editing_marker(MapCanvas.Canvas, map_canvas_left, map_canvas_top, map_canvas_width, map_canvas_height,
      Addr(Map.data), mouse_old_x * 16, mouse_old_y * 16, tbBrushWidth.Position * 16, tbBrushHeight.Position * 16, psDot, layer_marker_color, '');
  end else
  begin
    Renderer.remove_editing_marker(MapCanvas.Canvas);
  end;
  // Set cursor image visibility
  CursorImage.Visible := mode(mBlockMode) and (cur_block.width > 0) and (cur_block.height > 0) and not selection_started;
end;

procedure TMainWindow.render_tileset;
var
  i: integer;
  tile: word;
  tile_x, tile_y: integer;
begin
  for i := 0 to Length(itemNames) - 1 do
  begin
    tile := Map.level_data.tileFirstItemType[i];
    if tile <> 0 then
    begin
      tile_x := tile mod tileset_cols;
      tile_y := tile div tileset_cols;
      item_buttons[i].Glyph.Canvas.CopyRect(Rect(0,0,32,32), Tileset.tileimage.Canvas, Rect(tile_x*16, tile_y*16, tile_x*16+16, tile_y*16+16));
    end else
      item_buttons[i].Glyph.Canvas.Rectangle(0,0,32,32);
  end;
  cur_selected_preset[0] := 0;
  cur_selected_preset[1] := 0;
  SpritePropertiesDialog.update_contents;
  update_editing_mode;
end;

procedure TMainWindow.update_level_data;
var
  i: integer;
  last_item_index: integer;
  tmp_strings: TStringList;
  obj: ^TLevelObject;
  str: string;
  door: ^TDoorEntry;
  switch: ^TSwitchentry;
  lock: ^TLockEntry;
  transblock: TTransformationBlockPtr;
begin
  // Update Object types list
  if ufObjectTypes in Map.leveldata_dirtyflag then
  begin
    tmp_strings := TStringList.Create;
    tmp_strings.Add('0 - START');
    for i := 1 to Length(Map.leveldata.usedSprites) - 1 do
    begin
      if ord(Map.leveldata.usedSprites[i].name[0]) = 0 then
        continue;
      str := inttostr(i) + ' - ' + Map.leveldata.usedSprites[i].name;
      tmp_strings.Add(str);
    end;
    cbxObjectType.Items := tmp_strings;
    tmp_strings.Destroy;
    Map.leveldata_dirtyflag := Map.leveldata_dirtyflag - [ufObjectTypes];
  end;
  // Update object list
  if (EditorPages.ActivePageIndex = 1) and (ufObjects in Map.leveldata_dirtyflag) then
  begin
    tmp_strings := TStringList.Create;
    for i := 0 to Length(Map.leveldata.objects) - 1 do
    begin
      obj := Addr(Map.leveldata.objects[i]);
      tmp_strings.Add(get_object_listitem(i, obj.objType));
    end;
    last_item_index := Max(lstObjectList.ItemIndex, 0);
    lstObjectList.Items := tmp_strings;
    lstObjectList.ItemIndex := last_item_index;
    update_object_entry(last_item_index);
    tmp_strings.Destroy;
    Map.leveldata_dirtyflag := Map.leveldata_dirtyflag - [ufObjects];
  end;
  // Update door list
  if (EditorPages.ActivePageIndex = 2) and (ufDoors in Map.leveldata_dirtyflag) then
  begin
    tmp_strings := TStringList.Create;
    for i := 0 to Map.leveldata.numDoors - 1 do
    begin
      door := Addr(Map.leveldata.doors[i]);
      str := 'Door ' + inttostr(i) + '  (' + inttostr(door.minX) + ' , ' + inttostr(door.minY) + ')';
      tmp_strings.Add(str);
    end;
    last_item_index := Min(Max(lstDoorList.ItemIndex, 0), tmp_strings.Count - 1);
    lstDoorList.Items := tmp_strings;
    lstDoorList.ItemIndex := last_item_index;
    update_door_entry(last_item_index);
    tmp_strings.Destroy;
    Map.leveldata_dirtyflag := Map.leveldata_dirtyflag - [ufDoors];
  end;
  // Update transformation block list
  if (EditorPages.ActivePageIndex = 3) or (EditorPages.ActivePageIndex = 4) and (ufTransblocks in Map.leveldata_dirtyflag) then
  begin
    tmp_strings := TStringList.Create;
    for i := 0 to Map.leveldata.numTransblocks - 1 do
    begin
      transblock := Map.get_transblock(i);
      str := format('B %d  [%d x %d] (%d , %d)', [i, transblock.width, transblock.height, transblock.posX, transblock.posY]);
      tmp_strings.Add(str);
    end;
    cbxLockTransblock.Items := tmp_strings;
    if lstLockList.ItemIndex >= 0 then
      cbxLockTransblock.ItemIndex := Map.leveldata.locks[lstLockList.ItemIndex].transblockNumber;
    cbxSwitchT3Transblock.Items := tmp_strings;
    if lstSwitchList.ItemIndex >= 0 then
      cbxSwitchT3Transblock.ItemIndex := Map.leveldata.switches[lstSwitchList.ItemIndex].var1;
    last_item_index := Min(Max(lstTransblockList.ItemIndex, 0), tmp_strings.Count - 1);
    lstTransblockList.Items := tmp_strings;
    lstTransblockList.ItemIndex := last_item_index;
    update_transblock_entry(last_item_index);
    tmp_strings.Destroy;
    Map.leveldata_dirtyflag := Map.leveldata_dirtyflag - [ufTransblocks];
  end;
  // Update Switch list
  if (EditorPages.ActivePageIndex = 3) and (ufSwitches in Map.leveldata_dirtyflag) then
  begin
    tmp_strings := TStringList.Create;
    for i := 0 to Map.leveldata.numSwitches - 1 do
    begin
      switch := Addr(Map.leveldata.switches[i]);
      str := 'Switch ' + inttostr(i);
      if (switch.switchType < Length(switch_type_letter)) then
        str := str + ' [' + switch_type_letter[switch.switchType] + ']';
      str := str + ' (' + inttostr(switch.posX) + ' , ' + inttostr(switch.posY) + ')';
      tmp_strings.Add(str);
    end;
    last_item_index := Min(Max(lstSwitchList.ItemIndex, 0), tmp_strings.Count - 1);
    lstSwitchList.Items := tmp_strings;
    lstSwitchList.ItemIndex := last_item_index;
    update_switch_entry(last_item_index);
    tmp_strings.Destroy;
    Map.leveldata_dirtyflag := Map.leveldata_dirtyflag - [ufSwitches];
  end;
  // Update Lock list
  if (EditorPages.ActivePageIndex = 4) and (ufLocks in Map.leveldata_dirtyflag) then
  begin
    tmp_strings := TStringList.Create;
    for i := 0 to Map.leveldata.numLocks - 1 do
    begin
      lock := Addr(Map.leveldata.locks[i]);
      str := 'Lock ' + inttostr(i);
      if (lock.messageNumber < Length(switch_type_letter)) then
        str := str + ' [' + lock_type_letter[lock.messageNumber] + ']';
      str := str + ' (' + inttostr(lock.posX) + ' , ' + inttostr(lock.posY) + ')';
      tmp_strings.Add(str);
    end;
    last_item_index := Min(Max(lstLockList.ItemIndex, 0), tmp_strings.Count - 1);
    lstLockList.Items := tmp_strings;
    lstLockList.ItemIndex := last_item_index;
    update_lock_entry(last_item_index);
    tmp_strings.Destroy;
    Map.leveldata_dirtyflag := Map.leveldata_dirtyflag - [ufLocks];
  end;
  // Update other data
  if (EditorPages.ActivePageIndex = 5) and (ufOther in Map.leveldata_dirtyflag) then
  begin
    updating := true;
    seBorderLeft.MaxValue := Map.width;
    seBorderRight.MaxValue := Map.width;
    seBorderTop.MaxValue := Map.height;
    seBorderBottom.MaxValue := Map.height;
    seCapsuleCount.Value := Map.leveldata.capsuleCount;
    seBorderLeft.Value := Map.leveldata.borderLeft;
    seBorderRight.Value := Map.leveldata.borderRight;
    seBorderTop.Value := Map.leveldata.borderTop;
    seBorderBottom.Value := Map.leveldata.borderBottom;
    seMapFinishPixelX.Value := Map.leveldata.mapFinishPixelX div 16;
    seMapFinishPixelY.Value := Map.leveldata.mapFinishPixelY div 16;
    updating := false;
    Map.leveldata_dirtyflag := Map.leveldata_dirtyflag - [ufOther];
  end;
end;

procedure TMainWindow.update_object_entry(index: integer);
var
  obj: ^TLevelObject;
begin
  obj := Addr(Map.leveldata.objects[index]);
  updating := true;
  lbObjectNumber.Caption := 'Object ' + inttostr(index);
  cbxObjectType.ItemIndex := IfThen(obj.objType <> 65535, obj.objType, -1);
  cbxObjectTypeChange(nil);
  lbObjectPosX.Caption := 'X = ' + inttostr(obj.pixelX div 16) + '.' + inttostr(obj.pixelX mod 16);
  lbObjectPosY.Caption := 'Y = ' + inttostr(obj.pixelY div 16) + '.' + inttostr(obj.pixelY mod 16);
  cbxObjectBehavior.ItemIndex := obj.behavior;
  cbxObjectBehaviorChange(nil);
  seObjectVar1.Value := obj.var1;
  seObjectVar2.Value := obj.var2;
  seObjectVar3.Value := obj.var3;
  seObjectVar4.Value := obj.var4;
  seObjectVar5.Value := obj.var5;
  updating := false;
end;

procedure TMainWindow.update_door_entry(index: integer);
var
  door: ^TDoorEntry;
begin
  if (index = -1) or (Map.leveldata.numDoors = 0) then
  begin
    lbDoorNumber.Caption := 'Door';
    lbDoorEntrance.Caption := '';
    lbDoorDestination.Caption := '';
    updating := true;
    seDoorBorderLeft.Value := 0;
    seDoorBorderRight.Value := 0;
    seDoorBorderTop.Value := 0;
    seDoorBorderBottom.Value := 0;
    updating := false;
    exit;
  end;
  door := Addr(Map.leveldata.doors[index]);
  lbDoorNumber.Caption := 'Door ' + inttostr(index);
  lbDoorEntrance.Caption := format('Entrance: %d : %d , %d : %d', [door.minX, door.maxX, door.minY, door.maxY]);
  lbDoorDestination.Caption := format('Destination: %d.%d , %d.%d', [door.destPixelX div 16, door.destPixelX mod 16, door.destPixelY div 16, door.destPixelY mod 16]);
  updating := true;
  seDoorBorderLeft.Value := door.borderLeft;
  seDoorBorderRight.Value := door.borderRight;
  seDoorBorderTop.Value := door.borderTop;
  seDoorBorderBottom.Value := door.borderBottom;
  updating := false;
  render_map;
end;

procedure TMainWindow.update_switch_entry(index: integer);
var
  switch: ^TSwitchEntry;
  tp: integer;
begin
  sbSwitchT4Area.Down := false;
  if (index = -1) or (Map.leveldata.numSwitches = 0) then
  begin
    lbSwitchNumber.Caption := 'Switch';
    lbSwitchPosition.Caption := '';
    cbxSwitchType.ItemIndex := -1;
    pnSwitchT1.Visible := false;
    pnSwitchT3.Visible := false;
    pnSwitchT4.Visible := false;
    pnSwitchT5.Visible := false;
    pnTansblock.Visible := false;
    exit;
  end;
  switch := Addr(Map.leveldata.switches[index]);
  lbSwitchNumber.Caption := 'Switch ' + inttostr(index);
  lbSwitchPosition.Caption := 'Position: (' + inttostr(switch.posX) + ' , ' + inttostr(switch.posY) + ')';
  tp := switch.switchType;
  cbxSwitchType.ItemIndex := tp;
  pnSwitchT1.Visible := tp = 1;
  pnSwitchT3.Visible := tp = 3;
  pnSwitchT4.Visible := tp = 4;
  pnSwitchT5.Visible := tp = 5;
  pnTansblock.Visible := tp = 3;
  if tp = 3 then
  begin
    lstTransblockList.ItemIndex := switch.var1;
    lstTransblockListClick(nil);
  end;
  updating := true;
  seSwitchT1ObjNum.Value := switch.var1;
  cbxSwitchT3Transblock.ItemIndex := switch.var1;
  lbSwitchT4Area.Caption := format('Area [%d x %d] (%d , %d)', [switch.var3, switch.var4, switch.var1, switch.var2]);
  seSwitchT5FirstObj.Value := switch.var1;
  seSwitchT5LastObj.Value := switch.var2;
  updating := false;
end;

procedure TMainWindow.update_lock_entry(index: integer);
var
  lock: ^TLockEntry;
begin
  if (index = -1) or (Map.leveldata.numLocks = 0) then
  begin
    lbLockNumber.Caption := 'Lock';
    lbLockPosition.Caption := '';
    cbxLockType.ItemIndex := -1;
    cbxLockTransblock.ItemIndex := -1;
    exit;
  end;
  lock := Addr(Map.leveldata.locks[index]);
  lbLockNumber.Caption := 'Lock ' + inttostr(index);
  lbLockPosition.Caption := 'Position: (' + inttostr(lock.posX) + ' , ' + inttostr(lock.posY) + ')';
  cbxLockType.ItemIndex := IfThen(lock.messageNumber < cbxLockType.Items.Count, lock.messageNumber, -1);
  cbxLockTransblock.ItemIndex := IfThen(lock.transblockNumber < Map.leveldata.numTransblocks, lock.transblockNumber, -1);
  lstTransblockList.ItemIndex := lock.transblockNumber;
  lstTransblockListClick(nil);
end;

procedure TMainWindow.update_transblock_entry(index: integer);
var
  transblock: TTransformationBlockPtr;
  x, y: integer;
  tile: word;
  tile_x, tile_y: integer;
  sc: integer;
begin
  imgTransblock.Canvas.Pen.Color := clWhite;
  imgTransblock.Canvas.Brush.Color := clWhite;
  imgTransblock.Canvas.Rectangle(0, 0, imgTransblock.Width, imgTransblock.Height);
  if (index = -1) or (Map.leveldata.numTransblocks = 0) then
  begin
    lbTransblockNumber.Caption := 'Transformation Block';
    lbTransblockSizePos.Caption := '';
    exit;
  end;
  transblock := Map.get_transblock(index);
  lbTransblockNumber.Caption := 'Transformation Block ' + inttostr(index);
  lbTransblockSizePos.Caption := format('Size: [%d x %d] Pos: (%d , %d)', [transblock.width, transblock.height, transblock.posX, transblock.posY]);
  sc := 32;
  if (transblock.width > 4) or (transblock.height > 4) then
    sc := 16;
  for x := 0 to transblock.width - 1 do
    for y := 0 to transblock.height - 1 do
    begin
      tile := transblock.tiles[x + y * transblock.width];
      tile_x := tile mod tileset_cols;
      tile_y := tile div tileset_cols;
      imgTransblock.Canvas.CopyRect(Rect(x*sc, y*sc, x*sc+sc, y*sc+sc), Tileset.tileimage.Canvas, Rect(tile_x*16, tile_y*16, tile_x*16+16, tile_y*16+16));
    end;
end;

procedure TMainWindow.load_map_from_archive(index: integer);
begin
  // Load map
  Map.load_map_from_archive(index);
  // Set status bar
  StatusBar.Panels[4].Text := Archive.level_names[index];
  StatusBar.Panels[3].Text := inttostr(Map.width)+' x '+inttostr(Map.height);
  set_window_titles(Archive.level_names[index]);
  SpritePropertiesDialog.update_contents;
  TilePropertiesDialog.update_contents;
  // Rendering
  resize_map_canvas;
  render_minimap;
  render_map;
  update_level_data;
end;

procedure TMainWindow.save_map_to_archive(index: integer);
var
  old_index: integer;
begin
  if not Map.loaded then
    exit;
  // Save map
  old_index := Map.index;
  Map.save_map_to_archive(index);
  // Update map name on status bar if map index has changed
  if Map.index <> old_index then
  begin
    StatusBar.Panels[4].Text := Archive.level_names[Map.index];
    set_window_titles(Archive.level_names[Map.index]);
  end;
end;

procedure TMainWindow.load_map_from_file(filename: String);
begin
  if not FileExists(filename) then
    exit;
  if UpperCase(Copy(filename, Length(filename)-2, 3)) <> 'VGL' then
  begin
    Application.MessageBox('Invalid file type', 'Load map error', MB_ICONERROR);
    exit;
  end;
  // Load map file
  Map.load_map_file(filename);
  // Show map filename on status bar and title
  StatusBar.Panels[4].Text := filename;
  StatusBar.Panels[3].Text := inttostr(Map.width)+' x '+inttostr(Map.height);
  set_window_titles(ExtractFileName(filename));
  SpritePropertiesDialog.update_contents;
  // Initialize settings
  Settings.get_file_paths_from_map_filename;
  // Rendering
  resize_map_canvas;
  render_minimap;
  render_map;
  update_level_data;
  TilePropertiesDialog.update_contents;
end;

procedure TMainWindow.save_map_to_file(filename: String);
begin
  if not Map.loaded then
    exit;
  // Save map file
  Map.save_map_file(filename);
  // Show map filename on status bar and title if map is not loaded from archive
  if Map.index = -1 then
  begin
    StatusBar.Panels[4].Text := filename;
    set_window_titles(ExtractFileName(filename));
  end;
end;

function TMainWindow.check_map_errors: boolean;
var
  errmsg: String;
begin
  errmsg := Map.check_errors;
  result := true;
  if errmsg <> '' then
  begin
    Application.MessageBox(PChar(errmsg), 'Map error', MB_ICONWARNING);
    result := false;
  end;
end;

procedure TMainWindow.set_window_titles(map_name: String);
begin
  Caption := 'Vinyl Goddess from Mars Level Editor';
  SpritePropertiesDialog.Caption := 'Sprite properties';
  if map_name <> '' then
  begin
    Caption := Caption + ' - ' + map_name;
    SpritePropertiesDialog.Caption := SpritePropertiesDialog.Caption + ' - ' + map_name;
  end;
end;

function TMainWindow.check_map_can_be_tested: boolean;
begin
  if not Map.loaded then
  begin
    Application.MessageBox('No map to test.', 'Cannot test map', MB_ICONERROR);
    result := false;
  end else
  if Map.index = -1 then
  begin
    Application.MessageBox('Map is not saved in the game archive.', 'Cannot test map', MB_ICONERROR);
    result := false;
  end else
  if not FileExists(Settings.GameExecutable) then
  begin
    Application.MessageBox(PChar('Cannot find game executable (' + Settings.GameExecutable + ')'), 'Cannot test map', MB_ICONERROR);
    result := false;
  end else
  if not FileExists(Settings.DosboxPath) then
  begin
    Application.MessageBox(PChar('Cannot find Dosbox. Please specify full path to Dosbox in VinylEditor.ini file.'), 'Cannot test map', MB_ICONERROR);
    result := false;
  end else
  if Settings.CheckMapErrorsOnTest then
    result := check_map_errors
  else
    result := true;
end;

procedure TMainWindow.launch_game;
var
  sav_file: file of byte;
  sav_file_contents: TSavFile;
  save_name: string[7];
  entry: ^TSavEntry;
  i: integer;
begin
  // Save the map
  save_map_to_archive(Map.index);
  // Modify DATA.DAT to allow direct testing of this map
  AssignFile(sav_file, Settings.GameFolder + 'DATA.DAT');
  FileMode := fmOpenReadWrite;
  Reset(sav_file);
  BlockRead(sav_file, sav_file_contents, sizeof(sav_file_contents));
  entry := Addr(sav_file_contents.entries[0]);
  FillChar(entry.name, save_name_length, 0);
  save_name := 'TESTMAP';
  for i := 0 to length(save_name) - 1 do
    entry.name[i] := chr(ord(save_name[i+1]) - ord('A') + $1A);
  entry.pixelX := Map.leveldata.mapFinishPixelX;
  entry.pixelY := Map.leveldata.mapFinishPixelY;
  entry.difficulty := Settings.TestMapDifficulty - 1;
  entry.episode := Map.map_index div 9;
  entry.health := 9;
  for i := 0 to length(entry.finishedLevels) - 1 do
    entry.finishedLevels[i] := 0;
  for i := 0 to length(entry.scoreDigits) - 1 do
    entry.scoreDigits[i] := 255;
  entry.scoreDigits[0] := 0;
  Seek(sav_file, 0);
  BlockWrite(sav_file, sav_file_contents, sizeof(sav_file_contents));
  // Launch game
  ShellExecuteA(0, 'open', PChar(Settings.DosboxPath), PChar('"' + Settings.GameExecutable + '" ' + Settings.DosboxParameters), PChar(Settings.GameFolder), SW_SHOWNORMAL);
end;

function TMainWindow.mode(m: SelectedMode): boolean;
begin
  result := false;
  case m of
    mTileMode:        result := (EditorPages.TabIndex = 0) and (rbTileMode.Checked);
    mPatternMode:     result := (EditorPages.TabIndex = 0) and (rbPatternMode.Checked);
    mBlockMode:       result := (EditorPages.TabIndex = 0) and (rbBlockMode.Checked);
    mChangeTileType:  result := (EditorPages.TabIndex = 0) and (rbChangeTileType.Checked);
    mPainting:        result := (mode(mTile) and (rbTileMode.Checked or rbPatternMode.Checked or rbChangeTileType.Checked) and not selection_started) or mode(mItem);
    mSelecting:       result := ((mode(mPatternMode) or mode(mBlockMode)) and (ssShift in cur_shift_state)) or
                                ((mode(mTileMode) or mode(mPatternMode) or mode(mChangeTileType)) and (ssCtrl in cur_shift_state)) or
                                ((EditorPages.TabIndex = 2) and sbDoorEntrance.Down) or
                                (sbTransblockSelect.Down or sbSwitchT4Area.Down);
    mRightBtnScroll:  result := mode(mBlockMode) or (selection_started and (ssLeft in cur_shift_state)) or (mode(mObject) and mode(mSelecting));
    mPixelCoords:     result := mode(mObject) or (mode(mDoor) and sbDoorDestination.Down);
    mSingleTileThing: result := (mode(mSwitch) or mode(mLock) or mode(mItem));
    mTile:            result := (EditorPages.TabIndex = 0);
    mObject:          result := (EditorPages.TabIndex = 1);
    mDoor:            result := (EditorPages.TabIndex = 2);
    mSwitch:          result := (EditorPages.TabIndex = 3) and not sbTransblockMove.Down and not sbTransblockSelect.Down and not sbSwitchT4Area.Down;
    mLock:            result := (EditorPages.TabIndex = 4) and not sbTransblockMove.Down and not sbTransblockSelect.Down;
    mItem:            result := (EditorPages.TabIndex = 5);
  end;
end;

function TMainWindow.mouse_over_map_canvas: boolean;
var
  pos: TPoint;
begin
  pos := MainWindow.ScreenToClient(Mouse.CursorPos);
  result := PtInRect(MapCanvas.BoundsRect, pos);
  result := result and not (BlockPresetDialog.Visible and PtInRect(BlockPresetDialog.BoundsRect, Mouse.CursorPos));
  result := result and not (SpritePropertiesDialog.Visible and PtInRect(SpritePropertiesDialog.BoundsRect, Mouse.CursorPos));
  result := result or selection_started;
end;

function TMainWindow.get_object_listitem(obj_index: integer; obj_type: word): String;
begin
  result := inttostr(obj_index);
  if obj_type = 0 then
    result := result + ' - START'
  else if obj_type <> 65535 then
    result := result + ' - ' + Map.leveldata.usedSprites[obj_type].name;
end;

function TMainWindow.get_object_under_mouse: integer;
var
  i: integer;
  obj: ^TLevelObject;
  obj_width, obj_height: word;
begin
  result := -1;
  for i := 0 to Length(Map.leveldata.objects) - 1 do
  begin
    obj := Addr(Map.leveldata.objects[i]);
    if obj.objType = 65535 then
      continue;
    obj_width := Map.leveldata.usedSprites[obj.objType].width;
    obj_height := Map.leveldata.usedSprites[obj.objType].height;
    if PtInRect(Rect(obj.pixelX, obj.pixelY, obj.pixelX + obj_width, obj.pixelY + obj_height), Point(mouse_old_pixel_x, mouse_old_pixel_y)) then
    begin
      result := i;
      break;
    end;
  end;
end;

procedure TMainWindow.get_object_mouse_position(var pos_x, pos_y, size_x, size_y: word);
begin
  if mode(mDoor) then
  begin
    // Special case - door destination
    size_x := 20;
    size_y := 38;
    pos_x := mouse_old_pixel_x - 10;
    pos_y := mouse_old_pixel_y - 19;
    exit;
  end;
  size_x := Map.leveldata.usedSprites[cbxObjectType.ItemIndex].width + 1;
  size_y := Map.leveldata.usedSprites[cbxObjectType.ItemIndex].height + 1;
  pos_x := mouse_old_pixel_x - size_x div 2;
  if cbObjectAlignToFloor.Checked then
    pos_y := mouse_old_y * 16 - (size_y and 15) - seObjectAlignToFloor.Value
  else
    pos_y := mouse_old_pixel_y - size_y div 2;
end;

procedure TMainWindow.center_map_to(x, y: integer);
begin
  MapScrollH.Position := x - (map_canvas_width div 2);
  MapScrollV.Position := y - (map_canvas_height div 2);
end;

procedure TMainWindow.show_statistics;
begin
  StatusBar.Panels[5].Text := 'Caps: ' + inttostr(Map.stats.cnt_capsules) + '  Tre: ' + inttostr(Map.stats.cnt_treasure) +
    '  Heal: ' + inttostr(Map.stats.cnt_healpots) + '/' + inttostr(Map.stats.cnt_healjugs) + '  Mon: ' + inttostr(Map.stats.cnt_monsters) + '  Obj: ' + inttostr(Map.stats.cnt_objects);
  StatusBar.Panels[6].Text := 'Points: ' + inttostr(Map.stats.total_points);
end;

procedure TMainWindow.update_editing_mode;
begin
  if not mode(mTileMode) then
  begin
    select_current_preset;
  end;
  draw_block_image;
  render_editing_marker;
  mouse_already_clicked := false;
  if (BlockPresetDialog <> nil) then
    BlockPresetDialog.update_presets(cur_preset_group);
end;

procedure TMainWindow.select_current_preset;
var
  preset_index: integer;
  preset: TBlockPresetPtr;
  x, y: integer;
begin
  preset_index := cur_selected_preset[cur_preset_group];
  if preset_index = -1 then
  begin
    draw_cursor_image;
    exit;
  end;
  preset := Addr(Tileset.block_presets[cur_preset_group, preset_index]);
  if cur_preset_group = bpgPatternPreset then
  begin
    Map.set_pattern(preset);
  end else
  begin
    // Reset block data
    for x:= 0 to max_block_preset_size - 1 do
      for y := 0 to max_block_preset_size - 1 do
      begin
        cur_block.data[x,y].layers[0] := tile_no_change;
        cur_block.data[x,y].layers[1] := 0;
      end;
    // Copy block data from the block preset
    cur_block.width := preset.width;
    cur_block.height := preset.height;
    for x:= 0 to cur_block.width - 1 do
      for y := 0 to cur_block.height - 1 do
      begin
        if preset.tiles[x, y] = tile_no_change then
          cur_block.data[x,y].layers[1] := tile_no_change
        else
          cur_block.data[x,y].layers[Tileset.tile_layer[preset.tiles[x,y]]] := preset.tiles[x,y];
      end;
  end;
  btnSavePreset.Visible := false;
  draw_cursor_image;
end;

procedure TMainWindow.resize_cursor_image;
var
  cursor_image_left: integer;
  cursor_image_top: integer;
begin
  cursor_image_left := (CursorImage.Left - MapCanvas.Left) div 32;
  cursor_image_top := (CursorImage.Top - MapCanvas.Top) div 32;
  if (cursor_image_left + cur_block.width) > map_canvas_width then
    CursorImage.Width := (map_canvas_width - cursor_image_left) * 32
  else
    CursorImage.Width := cur_block.width * 32 + 1;
  if (cursor_image_top + cur_block.height) > map_canvas_height then
    CursorImage.Height := (map_canvas_height - cursor_image_top) * 32
  else
    CursorImage.Height := cur_block.height * 32 + 1;
end;

procedure TMainWindow.draw_cursor_image;
begin
  if not mode(mBlockMode) then
    exit;
  CursorImage.Width := cur_block.width * 32 + 1;
  CursorImage.Height := cur_block.height * 32 + 1;
  CursorImage.Picture.Bitmap.Width := cur_block.width * 32 + 1;
  CursorImage.Picture.Bitmap.Height := cur_block.height * 32 + 1;
  // Render cursor image
  Renderer.render_map_contents(CursorImage.Canvas, 0, 0, cur_block.width, cur_block.height, 0, 0, Addr(cur_block.data), 0,
    true, true, true, false, false, false, false,
    false);
  CursorImage.Canvas.Pen.Color := layer_marker_color;
  CursorImage.Canvas.Brush.Style := bsClear;
  CursorImage.Canvas.Rectangle(0, 0, cur_block.width * 32 + 1, cur_block.height * 32 + 1);
  resize_cursor_image;
  render_editing_marker;
end;

procedure TMainWindow.draw_block_image;
var
  i, x, y: integer;
  tile_x, tile_y: integer;
  border_x, border_y: integer;
  src_rect, dest_rect: TRect;
  str: String;
  scale: integer;
begin
  BlockImage.Canvas.Brush.Style := bsSolid;
  BlockImage.Canvas.Pen.Style := psSolid;
  BlockImage.Canvas.Brush.Color := clBtnFace;
  BlockImage.Canvas.Pen.Color := clBtnFace;
  BlockImage.Canvas.Rectangle(0,0,BlockImage.Width,BlockImage.Height);
  if not Map.loaded then
    exit;
  // Tile mode - render tileset
  if mode(mTileMode) then
  begin
    for i := cur_tileset_offset * 256 to cur_tileset_offset * 256 + 255 do
    begin
      x := (i and 255) mod 16;
      y := (i and 255) div 16;
      tile_x := i mod tileset_cols;
      tile_y := i div tileset_cols;
      src_rect := Rect(tile_x*16,tile_y*16,tile_x*16+16,tile_y*16+16);
      dest_rect := Rect(x*16,y*16,x*16+16,y*16+16);
      BlockImage.Canvas.CopyRect(dest_rect, Tileset.tileimage.Canvas, src_rect);
      if (i = cur_tile_index) then
      begin
        BlockImage.Canvas.Brush.Style := bsClear;
        BlockImage.Canvas.Pen.Color := clRed;
        BlockImage.Canvas.Rectangle(dest_rect);
      end;
    end;
  end else
  // Pattern mode - render pattern
  if mode(mPatternMode) then
  begin
    scale := IfThen((Map.pattern.width > 9) or (Map.pattern.height > 9), 1, 2);
    border_x := (BlockImage.Width - Map.pattern.width * 16 * scale) div 2;
    border_y := (BlockImage.Height - Map.pattern.height * 16 * scale) div 2;
    for x:= 0 to Map.pattern.width-1 do
      for y := 0 to Map.pattern.height-1 do
      begin
        tile_x := Map.pattern.tiles[x,y] mod 20;
        tile_y := Map.pattern.tiles[x,y] div 20;
        src_rect := Rect(tile_x*16, tile_y*16, tile_x*16+16, tile_y*16+16);
        dest_rect := Rect(x*16*scale+border_x, y*16*scale+border_y, (x*16+16)*scale+border_x, (y*16+16)*scale+border_y);
        BlockImage.Canvas.CopyRect(dest_rect, Tileset.tileimage.Canvas, src_rect);
      end;
  end else
  // Block mode - render block
  if mode(mBlockMode) then
  begin
    str := '';
    if (cur_block.width = 0) or (cur_block.height = 0) then
      str := 'Empty block is selected.';
    if (cur_block.width > max_block_preset_size) or (cur_block.height > max_block_preset_size) then
      str := 'Block is too big to display it here.';
    if str <> '' then
    begin
      BlockImage.Canvas.TextOut((BlockImage.Width - BlockImage.Canvas.TextWidth(str)) div 2, 122, str);
      exit;
    end;
    border_x := (BlockImage.Width - cur_block.width * 32) div 2;
    border_y := (BlockImage.Height - cur_block.height * 32) div 2;
    Renderer.render_map_contents(BlockImage.Canvas, 0, 0, cur_block.width, cur_block.height, border_x, border_y, Addr(cur_block.data), 0,
      true, true, true, false, false, false, false,
      false);
  end;
end;


procedure TMainWindow.set_map_size(new_width, new_height: integer);
begin
  if (Map.width = new_width) and (Map.height = new_height) then
    exit;
  Map.set_map_size(new_width, new_height);
  StatusBar.Panels[3].Text := inttostr(Map.width)+' x '+inttostr(Map.height);
  resize_map_canvas;
  render_minimap;
  render_map;
end;

procedure TMainWindow.shift_map(direction: TDirection; num_tiles: integer);
begin
  Map.shift_map(direction, num_tiles);
  render_minimap;
  render_map;
  update_level_data;
end;

end.
