unit _map;

interface

uses Classes, _tileset, _objectinfo;

const max_map_width = 264;
const max_map_height = 136;
const max_undo_steps = 65535;

// ============================================================================
// Object types and constants
// ============================================================================

type
  TDirection = (drLeft, drUp, drRight, drDown);

const itemNames: array[0..4] of String =
  ('Capsule', '100 Points', '250 Points', '500 Points', '1000 Points');

// ============================================================================
// Tile-related type definitions
// ============================================================================
type
  TMapTile = record
    layers: array[0..1] of word;
  end;

type
  TMapData = array[0..max_map_width-1, 0..max_map_height-1] of TMapTile;
  TMapDataPtr = ^TMapData;

const empty_tile: TMapTile = (layers:(0, 0););

// ============================================================================
// Level-info type definitions
// ============================================================================


type
  TAnimationEntry = record
    unknown: word;
    lastTile: word;
    firstTile: word;
  end;

  TPaletteAnimEntry = record
    firstIndex: word;
    lastIndex: word;
    startIndex: word;
    unknown: word;
  end;

  TDoorEntry = record
    minX: word;
    maxX: word;
    minY: word;
    maxY: word;
    destPixelX: word;
    destPixelY: word;
    borderLeft: word;
    borderRight: word;
    borderTop: word;
    borderBottom: word;
  end;

  TSwitchEntry = record
    posX: word;
    posY: word;
    initialState: word;
    switchType: word;
    var1: word;
    var2: word;
    unknown2: array[0..6] of word;
  end;

  TLockEntry = record
    keyX: word; // unused
    keyY: word; // unused
    posX: word;
    posY: word;
    unknown1: word;
    transblockNumber: word;
    messageNumber: word;
    showMessage: word;
    unknown2: word;
    unknown3: word;
  end;

  TSpriteDefinition = record
    name: array[0..15] of char;
    width: word;
    height: word;
    unknown: array[0..101] of byte;
  end;

  TLevelObject = record
    objType: word;
    pixelX: word;
    pixelY: word;
    var1: byte;
    var2: byte;
    unused1: word;
    var5: word;
    unused2: array[0..2] of word;
    var3: word;
    var4: word;
    unused3: array[0..9] of word;
    behavior: word;
  end;

  TTransformationBlock = record
    posX: word;
    posY: word;
    width: word;
    height: word;
    numBytes: word;
    unknown1: word;
    unknown2: word;
    tiles: array[0..0] of word;
  end;

  TTransformationBlockPtr = ^TTransformationBlock;

type
  TLevelData = record
    platformTiles: word;
    solidTiles: word;
    tileAnimations: array[0..14] of TAnimationEntry;
    numTileAnimations: word;
    noAnimationTiles: word;
    safeTiles: array[0..499] of byte;
    capsuleCount: word;
    tileFirstItem: word;
    tileLastItem: word;
    tileFirstClimb: word;
    tileLastClimb: word;
    tileFirstLadder: word;
    tileLastLadder: word;
    tileFirstRail: word;
    tileLastRail: word;
    borderLeft: word;
    borderTop: word;
    borderRight: word;
    borderBottom: word;
    doors: array[0..9] of TDoorEntry;
    switches: array[0..24] of TSwitchEntry;
    locks: array[0..14] of TLockEntry;
    numDoors: word;
    numSwitches: word;
    numLocks: word;
    tileFirstTransp: word;
    tileLastTransp: word;
    unknown: array[0..12] of word;
    tilesetName: array[0..12] of char;
    paletteName: array[0..12] of char;
    tileFirstBoundary: word;
    tileLastBoundary: word;
    unknown2: array[0..569] of word;
    mapFinishPixelX: word;
    mapFinishPixelY: word;
    unknown3: word;
    itemAnimations: array[0..4] of TAnimationEntry;
    tileFirstItemAnim: word;
    tileLastItemAnim: word;
    paletteAnims: array[0..4] of TPaletteAnimEntry;
    numPaletteAnims: word;
    tileFirstItemType: array[0..4] of byte;
    tileLastItemType: array[0..4] of byte;
    tileFirstFront: word;
    tileLastFront: word;
    usedSprites: array[0..29] of TSpriteDefinition;
    objects: array[0..149] of TLevelObject;
    numTransblocks: word;
  end;

type
  TLevelDataUpdateFlags = set of (ufObjects, ufObjectTypes, ufDoors, ufSwitches, ufLocks, ufTransblocks, ufOther);

const all_level_data_update_flags: TLevelDataUpdateFlags = [ufObjects, ufObjectTypes, ufDoors, ufSwitches, ufLocks, ufTransblocks, ufOther];

// ============================================================================
// Other type definitions
// ============================================================================
type
  TSelectionBlock = record
    width: word;
    height: word;
    layer: word;
    all_layers: boolean;
    data: TMapData;
  end;
  TSelectionBlockPtr = ^TSelectionBlock;

type
  TUndoEntry = record
    x, y: word;
    data: TMapTile;
    is_first: boolean;
  end;

type
  TMapStats = record
    cnt_capsules: integer;
    cnt_treasure: integer;
    cnt_healpots: integer;
    cnt_healjugs: integer;
    cnt_monsters: integer;
    cnt_objects: integer;
    total_points: integer;
  end;

// ============================================================================
// Map class
// ============================================================================
type
  TMap = class

  public
    // Map variables
    map_loaded: boolean;
    map_index: integer;
    map_data: TMapData;
    map_width: word;
    map_height: word;

    // Level variables
    level_data: TLevelData;
    level_data_update_flags: TLevelDataUpdateFlags;

    // Transformation block variables
    transblock_data: array[0..255] of word;
    transblock_size: integer;
    transblocks: array[0..39] of TTransformationBlockPtr;

    // Statistics variables
    map_stats: TMapStats;

    // Undo variables
    undo_history: array[0..max_undo_steps] of TUndoEntry;
    undo_start: integer;
    undo_max: integer;
    undo_pos: integer;
    undo_block_start: boolean;

    // Pattern variables
    cur_pattern: TBlockPreset;

    // Fill area variables
    tmp_layer: integer;
    tmp_tile_index: word;

  public
    Property loaded: boolean read map_loaded;
    Property index: Integer read map_index;
    Property data: TMapData read map_data;
    Property leveldata: TLevelData read level_data;
    Property leveldata_dirtyflag: TLevelDataUpdateFlags read level_data_update_flags write level_data_update_flags;
    Property width: word read map_width;
    Property height: word read map_height;
    Property stats: TMapStats read map_stats;
    Property pattern: TBlockPreset read cur_pattern;

    // Basic map manipulation procedures
  private
    procedure modify_map_tile(x,y: integer; new_tile: TMapTile);
    procedure paint_tile(x,y: integer; tile_index: word);
  public
    procedure paint_tile_rect(x, y, width, height: integer; tile_index: word);

    procedure set_pattern(new_pattern: TBlockPresetPtr);
    procedure copy_pattern(x, y, width, height, layer: integer);
    procedure rotate_pattern(direction: TDirection);

    procedure copy_block(x, y, width, height, layer: integer; all_layers: boolean; block: TSelectionBlockPtr);
    procedure put_block(x, y, layer: integer; all_layers: boolean; block: TSelectionBlockPtr);

    // Object manipulation methods
    function copy_object(obj_index, x, y: integer): integer;
    procedure remove_object(obj_index: integer);
    // Level data modification methods

    // Fill area procedures
  public
    procedure fill_area_start(x, y: integer; tile_index: word);
  private
    procedure fill_area_step(x,y: integer; area_type: integer);

    // Undo & Redo procedures
  public
    procedure do_undo;
    procedure do_redo;
  private
    procedure reset_undo_history;

    // Procedures related to auto-smoothing edges
  private
    function check_edge_tile(x,y: integer; exact: boolean): boolean;
    procedure put_edge_tile(var xpos, ypos: integer; moveoff_x, moveoff_y: integer; edge_tile: integer);
  public
    procedure smooth_edges(x, y: integer; paint_tile_group: integer);

    // Miscellaneous procedures
    procedure compute_statistics;
    function check_errors: String;

    // Load & Save procedures
    procedure new_map(tileset_index: integer; width, height: integer);
    procedure load_map_from_archive(index: integer);
    procedure save_map_to_archive(index: integer);
    procedure load_map_file(filename: String);
    procedure save_map_file(filename: String);
    procedure init_transblocks;

    // Map actions
    procedure set_map_size(new_width, new_height: integer);
    procedure shift_map(direction: TDirection; num_tiles: integer);

  end;

var
  Map: TMap;


implementation

uses Windows, Forms, SysUtils, Math, _renderer, _settings, _archive, main;


// Modify map tile and save old values into undo history.
// Map data should not be modified outside this or undo/redo methods.
procedure TMap.modify_map_tile(x,y: integer; new_tile: TMapTile);
begin
  // Save old map tile into undo history
  undo_history[undo_pos].x := x;
  undo_history[undo_pos].y := y;
  undo_history[undo_pos].data := map_data[x,y];
  undo_history[undo_pos].is_first := undo_block_start;
  undo_block_start := false;
  undo_pos := (undo_pos + 1) and max_undo_steps;
  if undo_start = undo_pos then
    undo_start := (undo_start + 1) and max_undo_steps;
  undo_max := undo_pos;
  MainWindow.Undo1.Enabled := true;
  MainWindow.Redo1.Enabled := false;
  // Modify the actual tile
  map_data[x,y] := new_tile;
  Renderer.invalidate_map_tile(x, y);
end;

procedure TMap.paint_tile(x,y: integer; tile_index: word);
var
  new_tile: TMapTile;
begin
  new_tile := map_data[x,y];
  // Replace reserved value of 65534 by a tile from current pattern
  if tile_index = 65534 then
  begin
    if (cur_pattern.width = 0) or (cur_pattern.height = 0) then
      exit;
    tile_index := cur_pattern.tiles[x mod cur_pattern.width, y mod cur_pattern.height];
  end;
  // Delete tile in foreground layer
  if tile_index = 65535 then
  begin
    new_tile.layers[1] := 0
  end else
  // Update layer depending on whether a tile is transparent or not
    new_tile.layers[Tileset.tile_layer[tile_index]] := tile_index;
  modify_map_tile(x, y, new_tile);
end;

procedure TMap.paint_tile_rect(x, y, width, height: integer; tile_index: word);
var
  xx, yy: integer;
begin
  undo_block_start := true;
  Renderer.invalidate_init;
  for xx := x to x + width - 1 do
    for yy := y to y + height - 1 do
    begin
      if (xx >= map_width) or (xx < 0) or (yy >= map_height) or (yy < 0) then
        continue;
      paint_tile(xx, yy, tile_index);
    end;
end;

procedure TMap.set_pattern(new_pattern: TBlockPresetPtr);
begin
  cur_pattern := new_pattern^;
end;

procedure TMap.copy_pattern(x, y, width, height, layer: integer);
var
  xx, yy: integer;
  mod_x, mod_y: integer;
  tile: word;
begin
  cur_pattern.width := Min(width, max_block_preset_size);
  cur_pattern.height := Min(height, max_block_preset_size);
  mod_x := x mod cur_pattern.width;
  mod_y := y mod cur_pattern.height;
  for xx := 0 to cur_pattern.width - 1 do
    for yy := 0 to cur_pattern.height - 1 do
    begin
      if (x + xx < map_width) and (y + yy < map_height) then
      begin
        tile := map_data[x + xx, y + yy].layers[layer];
      end else
      begin
        tile := 0;
      end;
      cur_pattern.tiles[(xx + mod_x) mod cur_pattern.width, (yy + mod_y) mod cur_pattern.height] := tile;
    end;
end;

procedure TMap.rotate_pattern(direction: TDirection);
var
  x, y: integer;
  tmp_tile: byte;
begin
  case direction of
    drLeft:
      for y := 0 to cur_pattern.height-1 do
      begin
        tmp_tile := cur_pattern.tiles[0, y];
        for x := 0 to cur_pattern.width-2 do
          cur_pattern.tiles[x, y] := cur_pattern.tiles[x+1, y];
        cur_pattern.tiles[cur_pattern.width-1, y] := tmp_tile;
      end;
    drUp:
      for x := 0 to cur_pattern.width-1 do
      begin
        tmp_tile := cur_pattern.tiles[x, 0];
        for y := 0 to cur_pattern.height-2 do
          cur_pattern.tiles[x, y] := cur_pattern.tiles[x, y+1];
        cur_pattern.tiles[x, cur_pattern.height-1] := tmp_tile;
      end;
    drRight:
      for y := 0 to cur_pattern.height-1 do
      begin
        tmp_tile := cur_pattern.tiles[cur_pattern.width-1, y];
        for x := cur_pattern.width-1 downto 1 do
          cur_pattern.tiles[x, y] := cur_pattern.tiles[x-1, y];
        cur_pattern.tiles[0, y] := tmp_tile;
      end;
    drDown:
      for x := 0 to cur_pattern.width-1 do
      begin
        tmp_tile := cur_pattern.tiles[x, cur_pattern.height-1];
        for y := cur_pattern.height-1 downto 1 do
          cur_pattern.tiles[x, y] := cur_pattern.tiles[x, y-1];
        cur_pattern.tiles[x, 0] := tmp_tile;
      end;
  end;
end;

procedure TMap.copy_block(x, y, width, height, layer: integer; all_layers: boolean; block: TSelectionBlockPtr);
var
  xx, yy: integer;
  map_tile: TMapTile;
begin
  block.width := width;
  block.height := height;
  block.layer := layer;
  block.all_layers := all_layers;
  for xx := 0 to width - 1 do
    for yy := 0 to height - 1 do
    begin
      map_tile := empty_tile;
      if (x + xx < map_width) and (y + yy < map_height) then
      begin
        map_tile := map_data[x + xx, y + yy];
      end;
      block.data[xx,yy] := map_tile;
    end;
end;

procedure TMap.put_block(x, y, layer: integer; all_layers: boolean; block: TSelectionBlockPtr);
var
  xx, yy: integer;
begin
  undo_block_start := true;
  Renderer.invalidate_init;
  for xx := 0 to block.width - 1 do
    for yy := 0 to block.height - 1 do
      if (x + xx < map_width) and (x + xx >= 0) and (y + yy < map_height) and (y + yy >= 0) then
      begin
        if all_layers then
        begin
          modify_map_tile(x + xx, y + yy, block.data[xx, yy])
        end else
          paint_tile(x + xx, y + yy, block.data[xx, yy].layers[layer]);
      end;
end;

function TMap.copy_object(obj_index, x, y: integer): integer;
var
  i: integer;
  new_index: integer;
begin
  // Find free object slot
  new_index := -1;
  for i := 0 to Length(level_data.objects) - 1 do
    if level_data.objects[i].objType = 65535 then
    begin
      new_index := i;
      break;
    end;
  // Copy object
  if new_index <> -1 then
  begin
    move(level_data.objects[obj_index], level_data.objects[new_index], sizeof(TLevelObject));
    level_data.objects[new_index].pixelX := x;
    level_data.objects[new_index].pixelY := y;
    compute_statistics;
  end;
  result := new_index;
end;

procedure TMap.remove_object(obj_index: integer);
begin
  FillChar(level_data.objects[obj_index], sizeof(TLevelObject), 0);
  level_data.objects[obj_index].objType := 65535;
  compute_statistics;
end;

procedure TMap.fill_area_start(x, y: integer; tile_index: word);
var
  tmp_pos: integer;
begin
  // Undo the action which was made by first click
  tmp_pos := undo_pos;
  repeat
    tmp_pos := (tmp_pos - 1) and max_undo_steps
  until undo_history[tmp_pos].is_first;
  if (undo_history[tmp_pos].x = x) and (undo_history[tmp_pos].y = y) then
    do_undo;
  // Fill area
  undo_block_start := true;
  Renderer.invalidate_init;
  if tile_index < cnt_tileset_tiles then
    tmp_layer := Tileset.tile_layer[tile_index]
  else
    tmp_layer := 0;
  tmp_tile_index := tile_index;
  fill_area_step(x, y, map_data[x,y].layers[tmp_layer]);
end;

procedure TMap.fill_area_step(x, y: integer; area_type: integer);
begin
  if map_data[x,y].layers[tmp_layer] <> area_type then
    exit;
  paint_tile(x, y, tmp_tile_index);
  if map_data[x,y].layers[tmp_layer] = area_type then
    exit;
  if x > 0 then
    fill_area_step(x-1, y, area_type);
  if x < (map_width - 1) then
    fill_area_step(x+1, y, area_type);
  if y > 0 then
    fill_area_step(x, y-1, area_type);
  if y < (map_height - 1) then
    fill_area_step(x, y+1, area_type);
end;


procedure TMap.do_undo;
var
  tmp_data: TMapTile;
  x, y: word;
begin
  if undo_pos = undo_start then
    exit;
  repeat
    undo_pos := (undo_pos - 1) and max_undo_steps;
    x := undo_history[undo_pos].x;
    y := undo_history[undo_pos].y;
    tmp_data := map_data[x, y];
    map_data[x, y] := undo_history[undo_pos].data;
    undo_history[undo_pos].data := tmp_data;
    Renderer.invalidate_map_tile(x, y);
  until undo_history[undo_pos].is_first or (undo_pos = undo_start);
  if undo_pos = undo_start then
    MainWindow.Undo1.Enabled := false;
  MainWindow.Redo1.Enabled := true;
  compute_statistics;
end;

procedure TMap.do_redo;
var
  tmp_data: TMapTile;
  x, y: word;
begin
  if undo_pos = undo_max then
    exit;
  repeat
    x := undo_history[undo_pos].x;
    y := undo_history[undo_pos].y;
    tmp_data := map_data[x, y];
    map_data[x, y] := undo_history[undo_pos].data;
    undo_history[undo_pos].data := tmp_data;
    Renderer.invalidate_map_tile(x, y);
    undo_pos := (undo_pos + 1) and max_undo_steps;
  until undo_history[undo_pos].is_first or (undo_pos = undo_max);
  if undo_pos = undo_max then
    MainWindow.Redo1.Enabled := false;
  MainWindow.Undo1.Enabled := true;
  compute_statistics;
end;

procedure TMap.reset_undo_history;
begin
  MainWindow.Undo1.Enabled := false;
  MainWindow.Redo1.Enabled := false;
  undo_start := 0;
  undo_max := 0;
  undo_pos := 0;
end;


function TMap.check_edge_tile(x, y: integer; exact: boolean): boolean;
begin
  if exact then
    result := map.map_data[x, y].layers[0] = 0
  else
    result := map.map_data[x, y].layers[0] <> 255;
end;

procedure TMap.put_edge_tile(var xpos, ypos: integer; moveoff_x, moveoff_y, edge_tile: integer);
var
  tile_index: byte;
begin
  tile_index := Tileset.edge_tiles[edge_tile];
  // We cannot place the edge tile immediately physically into map,
  // because it would interfere with checks for tiles around the following tile.
  // Instead, we exploit undo feature for this purpose: we store all changes
  // into history and in the end we apply the changes (like doing redo)
  undo_history[undo_max].x := xpos;
  undo_history[undo_max].y := ypos;
  undo_history[undo_max].data := map_data[xpos, ypos];
  undo_history[undo_max].data.layers[0] := tile_index;
  undo_history[undo_max].is_first := false;
  undo_max := (undo_max + 1) and max_undo_steps;
  // Finally move to next position (anticlockwise direction)
  xpos := xpos + moveoff_x;
  ypos := ypos + moveoff_y;
end;

procedure TMap.smooth_edges(x, y: integer; paint_tile_group: integer);
var
  start_x, start_y: integer;
  sum: integer;
  steps: integer;
begin
  start_x := x;
  start_y := y;
  Renderer.invalidate_init;
  undo_max := undo_pos;
  steps := 0;
  // Start smoothing edge from starting point (where user shift-clicked)
  while check_edge_tile(x, y, true) do
  begin
    // Check for all 8 tiles around current tile to determine the direction of edge
    sum := 0;
    if check_edge_tile(x,   y-1, false) then sum := sum + 1;   // 16 1 32
    if check_edge_tile(x-1, y  , false) then sum := sum + 2;   //  2 X 4
    if check_edge_tile(x+1, y  , false) then sum := sum + 4;   // 64 8 128
    if check_edge_tile(x  , y+1, false) then sum := sum + 8;
    if check_edge_tile(x-1, y-1, false) then sum := sum + 16;
    if check_edge_tile(x+1, y-1, false) then sum := sum + 32;
    if check_edge_tile(x-1, y+1, false) then sum := sum + 64;
    if check_edge_tile(x+1, y+1, false) then sum := sum + 128;
    // Transform current tile into edge tile and move to following tile
    case (sum and 15) of
       7: begin // down
            put_edge_tile(x,y,1,0,etTop);
        end;
      11: begin // right
            put_edge_tile(x,y,0,-1,etLeft);
        end;
      14: begin // up
            put_edge_tile(x,y,-1,0,etBottom);
        end;
      13: begin // left
            put_edge_tile(x,y,0,1,etRight);
        end;
       3: begin // down-right corner
            put_edge_tile(x,y,0,-1,etTopLeft);
        end;
      10: begin // up-right corner
            put_edge_tile(x,y,-1,0,etBottomLeft);
        end;
      12: begin // up-left corner
            put_edge_tile(x,y,0,1,etBottomRight);
        end;
       5: begin // down-left corner
            put_edge_tile(x,y,1,0,etTopRight);
        end;
      15: begin // inner curves
        case sum of
          239: put_edge_tile(x,y,-1,0,etBottomRight); // down-right curve
          191: put_edge_tile(x,y,0,1,etTopRight);  // up-right curve
          127: put_edge_tile(x,y,1,0,etTopLeft);  // up-left curve
          223: put_edge_tile(x,y,0,-1,etBottomLeft); // down-left curve
          else break; // Invalid combination - end
        end;
        end;
      else break; // Invalid combination - end
    end;
    // End if we got back into starting point
    if (x = start_x) and (y = start_y) then
      break;
    // End if we got outside the map
    if (x < 0) or (y < 0) or (x >= map_width) or (y >= map_height) then
      break;
    // Sometimes the algorithm may end up in infinite loop. This is to prevent it.
    inc(steps);
    if steps > 1000 then
      break;
  end;
  undo_history[undo_pos].is_first := true;
  // Finally put smoothed edges on map - apply all changes we stored into undo history
  do_redo;
end;


procedure TMap.compute_statistics;
var
  x, y: integer;
  i: integer;
  tile: word;
  itemtype: integer;
begin
  // Process map foreground layer and count statistics of several types of objects
  map_stats.cnt_capsules := 0;
  map_stats.cnt_treasure := 0;
  map_stats.cnt_healpots := 0;
  map_stats.cnt_healjugs := 0;
  map_stats.cnt_monsters := 0;
  map_stats.cnt_objects := 0;
  map_stats.total_points := 0;
  for y := 0 to map_height - 1 do
    for x := 0 to map_width -1 do
    begin
      tile := map_data[x,y].layers[1];
      if tile = 0 then
        continue;
      itemtype := -1;
      for i := 0 to 4 do
        if (tile >= level_data.tileFirstItemType[i]) and (tile <= level_data.tileLastItemType[i]) then
          itemtype := i;
      case itemtype of
        0: Inc(map_stats.cnt_capsules);
        1: begin Inc(map_stats.cnt_treasure); Inc(map_stats.total_points, 100); end;
        2: begin Inc(map_stats.cnt_treasure); Inc(map_stats.total_points, 250); end;
        3: begin Inc(map_stats.cnt_treasure); Inc(map_stats.total_points, 500); end;
        4: begin Inc(map_stats.cnt_treasure); Inc(map_stats.total_points, 1000); end;
      end;
    end;
  for i := 0 to Length(level_data.objects) - 1 do
    if level_data.objects[i].objType <> 65535 then
      Inc(map_stats.cnt_objects);
  MainWindow.show_statistics;
end;

function TMap.check_errors: String;
begin
  //if map_stats.cnt_crystals = 0 then
  //begin
  //  result := 'You must place at least one Magic Crystal.';
  //  exit;
  //end;
  result := '';
end;

procedure TMap.new_map(tileset_index: integer; width, height: integer);
var
  x, y: integer;
  i: integer;
  level_index: integer;
begin
  // Load a level using this tileset to retrieve sprite and tile properties
  level_index := Archive.get_first_level_by_tileset(tileset_index);
  load_map_from_archive(level_index);
  // Initialize map layers
  map_width := width;
  map_height := height;
  for x := 0 to map_width - 1 do
    for y := 0 to map_height - 1 do
      map_data[x,y] := empty_tile;
  // Initialize level data
  level_data.capsuleCount := 0;
  level_data.borderLeft := 0;
  level_data.borderTop := 0;
  level_data.borderRight := map_width;
  level_data.borderBottom := map_height;
  FillChar(level_data.doors, sizeof(level_data.doors), 0);
  FillChar(level_data.switches, sizeof(level_data.switches), 0);
  FillChar(level_data.locks, sizeof(level_data.locks), 0);
  level_data.numDoors := 0;
  level_data.numSwitches := 0;
  level_data.numLocks := 0;
  FillChar(level_data.objects, sizeof(level_data.objects), 0);
  for i := 0 to Length(level_data.objects) - 1 do
    level_data.objects[i].objType := 65535;
  level_data.numTransblocks := 0;
  // Finalize it
  map_loaded := true;
  map_index := -1;
  reset_undo_history;
  compute_statistics;
  level_data_update_flags := all_level_data_update_flags;
end;

procedure TMap.load_map_from_archive(index: integer);
var
  level_file_offset: Cardinal;
  level_file_next_offset: Cardinal;
  bg_layer_buffer: array[0..max_map_width*max_map_height-1] of word;
  fg_layer_buffer: array[0..max_map_width*max_map_height-1] of byte;
  x, y: integer;
begin
  level_file_offset := Archive.file_list[index + Archive.level_base_index].offset;
  level_file_next_offset := Archive.file_list[index + Archive.level_base_index + 1].offset;
  Archive.open_archive(fmOpenRead, true);
  // Load map size
  archive.load_data(Addr(map_height), level_file_offset, 2);
  archive.load_data(Addr(map_width), level_file_offset+2, 2);
  Inc(level_file_offset, 4);
  // Load background layer
  Archive.load_data(Addr(bg_layer_buffer), level_file_offset, map_width * map_height * 2);
  for x := 0 to map_width - 1 do
    for y := 0 to map_height - 1 do
      map_data[x, y].layers[0] := bg_layer_buffer[y * map_width + x];
  Inc(level_file_offset, map_width * map_height * 2);
  // Load foreground layer
  Archive.load_data(Addr(fg_layer_buffer), level_file_offset, map_width * map_height);
  for x := 0 to map_width - 1 do
    for y := 0 to map_height - 1 do
      map_data[x, y].layers[1] := fg_layer_buffer[y * map_width + x];
  Inc(level_file_offset, map_width * map_height);
  // Load level data
  Archive.load_data(Addr(level_data), level_file_offset, sizeof(level_data));
  Inc(level_file_offset, sizeof(level_data));
  // Load transformation blocks
  transblock_size := level_file_next_offset - level_file_offset;
  if transblock_size <> 0 then
    Archive.load_data(Addr(transblock_data), level_file_offset, transblock_size);
  init_transblocks;
  Archive.close_archive(true);
  // Finalize it
  map_loaded := true;
  map_index := index;
  reset_undo_history;
  compute_statistics;
  level_data_update_flags := all_level_data_update_flags;
end;

procedure TMap.save_map_to_archive(index: integer);
var
  level_file_offset: Cardinal;
  bg_layer_buffer: array[0..max_map_width*max_map_height-1] of word;
  fg_layer_buffer: array[0..max_map_width*max_map_height-1] of byte;
  x, y: integer;
begin
  level_file_offset := Archive.file_list[index + Archive.level_base_index].offset;
  Archive.open_archive(fmOpenReadWrite, true);
  // Save map size
  archive.save_data(Addr(map_height), level_file_offset, 2);
  archive.save_data(Addr(map_width), level_file_offset+2, 2);
  Inc(level_file_offset, 4);
  // Save background layer
  for x := 0 to map_width - 1 do
    for y := 0 to map_height - 1 do
      bg_layer_buffer[y * map_width + x] := map_data[x, y].layers[0];
  Archive.save_data(Addr(bg_layer_buffer), level_file_offset, map_width * map_height * 2);
  Inc(level_file_offset, map_width * map_height * 2);
  // Save foreground layer
  for x := 0 to map_width - 1 do
    for y := 0 to map_height - 1 do
      fg_layer_buffer[y * map_width + x] := map_data[x, y].layers[1];
  Archive.save_data(Addr(fg_layer_buffer), level_file_offset, map_width * map_height);
  Inc(level_file_offset, map_width * map_height);
  // Save level data
  Archive.save_data(Addr(level_data), level_file_offset, sizeof(level_data));
  Inc(level_file_offset, sizeof(level_data));
  // Save transformation blocks
  if transblock_size <> 0 then
    Archive.save_data(Addr(transblock_data), level_file_offset, transblock_size);
  Archive.close_archive(true);
  map_index := index;
end;

procedure TMap.load_map_file(filename: String);
var
  map_file: file of byte;
  header: array[0..3] of byte;
  bg_layer_buffer: array[0..max_map_width*max_map_height-1] of word;
  fg_layer_buffer: array[0..max_map_width*max_map_height-1] of byte;
  x, y: integer;
begin
  // Read map file
  AssignFile(map_file, filename);
  FileMode := fmOpenRead;
  Reset(map_file);
  BlockRead(map_file, header, sizeof(header));
  // Load map size
  BlockRead(map_file, map_height, sizeof(map_height));
  BlockRead(map_file, map_width, sizeof(map_width));
  // Load background layer
  BlockRead(map_file, bg_layer_buffer, map_width * map_height * 2);
  for x := 0 to map_width - 1 do
    for y := 0 to map_height - 1 do
      map_data[x, y].layers[0] := bg_layer_buffer[y * map_width + x];
  // Load foreground layer
  BlockRead(map_file, fg_layer_buffer, map_width * map_height);
  for x := 0 to map_width - 1 do
    for y := 0 to map_height - 1 do
      map_data[x, y].layers[1] := fg_layer_buffer[y * map_width + x];
  // Load level data
  BlockRead(map_file, level_data, sizeof(level_data));
  CloseFile(map_file);
  // Finalize it
  map_loaded := true;
  map_index := -1;
  reset_undo_history;
  compute_statistics;
  level_data_update_flags := all_level_data_update_flags;
  Tileset.change_tileset(header[3]);
end;

procedure TMap.save_map_file(filename: String);
var
  map_file: file of byte;
  header: array[0..3] of byte;
  bg_layer_buffer: array[0..max_map_width*max_map_height-1] of word;
  fg_layer_buffer: array[0..max_map_width*max_map_height-1] of byte;
  x, y: integer;
begin
  AssignFile(map_file, filename);
  ReWrite(map_file);
  header[0] := ord('V');
  header[1] := ord('G');
  header[2] := ord('L');
  header[3] := Tileset.current_tileset;
  BlockWrite(map_file, header, sizeof(header));
  // Save map size
  BlockWrite(map_file, map_height, sizeof(map_height));
  BlockWrite(map_file, map_width, sizeof(map_width));
  // Save background layer
  for x := 0 to map_width - 1 do
    for y := 0 to map_height - 1 do
      bg_layer_buffer[y * map_width + x] := map_data[x, y].layers[0];
  BlockWrite(map_file, bg_layer_buffer, map_width * map_height * 2);
  // Save foreground layer
  for x := 0 to map_width - 1 do
    for y := 0 to map_height - 1 do
      fg_layer_buffer[y * map_width + x] := map_data[x, y].layers[1];
  BlockWrite(map_file, fg_layer_buffer, map_width * map_height);
  // Save level data
  BlockWrite(map_file, level_data, sizeof(level_data));
  CloseFile(map_file);
end;

procedure TMap.init_transblocks;
var
  i: integer;
  pos: integer;
begin
  pos := 0;
  for i := 0 to level_data.numTransblocks - 1 do
  begin
    transblocks[i] := Addr(transblock_data[pos]);
    pos := pos + (transblocks[i].numBytes div 2) + 7;
  end;
end;

procedure TMap.set_map_size(new_width, new_height: integer);
var
  i, j: integer;
begin
  if (map_width = new_width) and (map_height = new_height) then
    exit;
  if (new_width > max_map_width) or (new_height > max_map_height) then
    exit;
  // Fill additional area with empty tiles
  for i := 0 to new_height - 1 do
    for j := 0 to new_width - 1 do
      if (i >= map_height) or (j >= map_width) then
      begin
        map_data[j,i] := empty_tile;
      end;
  // Set new map size
  map_width := new_width;
  map_height := new_height;
  // Finalize it
  reset_undo_history;
  compute_statistics;
end;

procedure TMap.shift_map(direction: TDirection; num_tiles: integer);
var
  x, y: integer;
  src_x, src_y: integer;
  move_x, move_y: integer;
  i: integer;
begin
  move_x := 0;
  move_y := 0;
  // Shift map tiles
  case direction of
    drLeft:  begin // Left
          for y := 0 to map_height - 1 do
            for x := 0 to map_width - 1 do
            begin
              src_x := x + num_tiles;
              move_x := num_tiles * -1;
              if (src_x < map_width) then
                map_data[x,y] := map_data[src_x,y]
              else
                map_data[x,y] := empty_tile;
            end;
        end;
    drUp:  begin // Up
          for y := 0 to map_height - 1 do
            for x := 0 to map_width - 1 do
            begin
              src_y := y + num_tiles;
              move_y := num_tiles * -1;
              if (src_y < map_height) then
                map_data[x,y] := map_data[x,src_y]
              else
                map_data[x,y] := empty_tile;
            end;
        end;
    drRight:  begin // Right
          for y := map_height - 1 downto 0 do
            for x := map_width - 1 downto 0 do
            begin
              src_x := x - num_tiles;
              move_x := num_tiles;
              if (src_x >= 0) then
                map_data[x,y] := map_data[src_x,y]
              else
                map_data[x,y] := empty_tile;
            end;
        end;
    drDown:  begin // Down
          for y := map_height - 1 downto 0 do
            for x := map_width - 1 downto 0 do
            begin
              src_y := y - num_tiles;
              move_y := num_tiles;
              if (src_y >= 0) then
                map_data[x,y] := map_data[x,src_y]
              else
                map_data[x,y] := empty_tile;
            end;
        end;
  end;
  // Shift objects
  for i := 0 to Length(level_data.objects) - 1 do
  begin
    level_data.objects[i].pixelX := Max(Min(level_data.objects[i].pixelX + move_x * 16, map_width * 16 - 1), 0);
    level_data.objects[i].pixelY := Max(Min(level_data.objects[i].pixelY + move_y * 16, map_height * 16 - 1), 0);
  end;
  // Finalize it
  reset_undo_history;
  compute_statistics;
  level_data_update_flags := all_level_data_update_flags;
end;

end.