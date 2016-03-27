unit _tileset;

interface

uses Graphics, Menus;

// Tileset constants
const tileset_cols = 20;
const cnt_tileset_tiles = 500;
const cnt_defined_tiles = 32;
const cnt_block_preset_keys = 40; // 0-9, A-Z...
const max_block_presets = cnt_block_preset_keys;
const max_block_preset_size = 18;
const block_preset_ini_sections: array[0..1] of String = ('Patterns', 'Blocks');

const bpgPatternPreset = 0;
const bpgBlockPreset = 1;

const num_key_rows = 4;
const num_key_cols = 10;
const block_preset_keys: array[0..num_key_rows-1, 0..num_key_cols-1] of char =
  (
    ('1', '2', '3', '4', '5', '6', '7', '8', '9', '0'),
    ('Q', 'W', 'E', 'R', 'T', 'Y', 'U', 'I', 'O', 'P'),
    ('A', 'S', 'D', 'F', 'G', 'H', 'J', 'K', 'L', ':'),
    ('Z', 'X', 'C', 'V', 'B', 'N', 'M', '<', '>', '?')
  );

// Edge tiles
const etLeft = 0;
const etTop = 1;
const etRight = 2;
const etBottom = 3;
const etTopLeftOut = 4;
const etTopRightOut = 5;
const etBottomLeftOut = 6;
const etBottomRightOut = 7;
const etTopLeftIn = 8;
const etTopRightIn = 9;
const etBottomLeftIn = 10;
const etBottomRightIn = 11;

// Tileset type definitions
type
  TBlockPreset = record
    width: word;
    height: word;
    tiles: array[0..max_block_preset_size-1, 0..max_block_preset_size-1] of word;
  end;
  TBlockPresetPtr = ^TBlockPreset;

const empty_block_preset: TBlockPreset = (width: 0; height: 0;);

// Tileset class
type
  TTileset = class

  private
    config_changed: boolean;

  public
    tileimage_filename: String;
    tileset_config_filename: String;
    tileimage: TBitmap;
    tileimage_mask: TBitmap;
    current_tileset: integer;
    num_tiles: integer;
    tile_layer: array[0..cnt_tileset_tiles] of Byte;

    // Tileset configuration
    base_tiles: array[0..cnt_defined_tiles-1] of word;
    bright_tiles: array[0..cnt_defined_tiles-1] of word;
    shadow_tiles: array[0..cnt_defined_tiles-1] of word;
    block_presets: array[0..1, 0..max_block_presets-1] of TBlockPreset;

  public
    procedure init;
    procedure change_tileset(index: integer);
    procedure next_tileset;
    procedure use_custom_image(filename: String);

    procedure load_config;
    procedure save_config;

    procedure save_preset(preset: TBlockPresetPtr; group, preset_num: integer);
    procedure remove_preset(group, preset_num: integer);

    function block_key_to_index(key: word): integer;

  end;

var
  Tileset: TTileset;

implementation

uses Windows, Forms, SysUtils, Math, main, block_preset_dialog, _settings, IniFiles, Classes, Dialogs, _archive, _renderer;

procedure TTileset.init;
begin
  tileimage := Graphics.TBitmap.Create;
  tileimage.PixelFormat := pf32bit;
  tileimage.Width := 320;
  tileimage.Height := 200;
  tileimage_mask := Graphics.TBitmap.Create;
  current_tileset := -1;
end;

procedure TTileset.change_tileset(index: integer);
begin
  if index = current_tileset then
    exit;
  if index >= Archive.tileset_count then
    exit;
  // Save configuration of previous tileset
  save_config;
  // Change to new tileset
  current_tileset := index;
  MainWindow.StatusBar.Panels[2].Text := Archive.tileset_info[current_tileset].name;
  // Load tileset configuration if it is different
  load_config;
  // Load tileset image
  Archive.load_tileset_image(tileimage, index);
  Renderer.load_or_create_mask(tileimage, tileimage_mask, current_dir + 'tilesets\' + Archive.tileset_info[index].name + '_mask.bmp');
  tileimage_filename := '';
  MainWindow.render_tileset;
end;


procedure TTileset.next_tileset;
var
  new_tileset: integer;
begin
  new_tileset := current_tileset + 1;
  if new_tileset >= Archive.tileset_count then
    new_tileset := 0;
  change_tileset(new_tileset);
end;

procedure TTileset.use_custom_image(filename: String);
begin
  current_tileset := -1;
  MainWindow.StatusBar.Panels[2].Text := 'Custom image';
  tileimage.LoadFromFile(tileimage_filename);
  tileimage_filename := filename;
  MainWindow.render_tileset;
end;

procedure TTileset.load_config;
var
  filename: String;
  ini: TMemIniFile;
  tmp_strings: TStringList;
  decoder, decoder2: TStringList;
  i, j, preset_type, preset_index: integer;
  preset: ^TBlockPreset;
  width, height: integer;
begin
  // Reset all configuration first
  FillChar(base_tiles, sizeof(base_tiles), 255);
  FillChar(bright_tiles, sizeof(bright_tiles), 255);
  FillChar(shadow_tiles, sizeof(shadow_tiles), 255);
  FillChar(block_presets, sizeof(block_presets), 0);
  // Try to open configuration ini file
  filename := current_dir+'/tilesets/'+Archive.tileset_info[current_tileset].name+'.ini';
  tileset_config_filename := filename;
  if not FileExists(filename) then
    exit;
  ini := TMemIniFile.Create(filename);
  tmp_strings := TStringList.Create;
  decoder := TStringList.Create;
  decoder2 := TStringList.Create;
  decoder.Delimiter := ';';
  decoder2.Delimiter := '.';
  // Load basic tiles
  decoder2.DelimitedText := ini.ReadString('Tiles', 'Base_Tiles', '');
  for i := 0 to Min(cnt_defined_tiles, decoder2.Count) - 1 do
    base_tiles[i] := strtoint(decoder2[i]);
  // Load bright tiles
  decoder2.DelimitedText := ini.ReadString('Tiles', 'Bright_Tiles', '');
  for i := 0 to Min(cnt_defined_tiles, decoder2.Count) - 1 do
    bright_tiles[i] := strtoint(decoder2[i]);
  // Load shadow tiles
  decoder2.DelimitedText := ini.ReadString('Tiles', 'Shadow_Tiles', '');
  for i := 0 to Min(cnt_defined_tiles, decoder2.Count) - 1 do
    shadow_tiles[i] := strtoint(decoder2[i]);
  // Load block presets
  for preset_type := 0 to 1 do
  begin
    ini.ReadSection(block_preset_ini_sections[preset_type], tmp_strings);
    for i := 0 to tmp_strings.Count - 1 do
    begin
      preset_index := strtointdef(tmp_strings[i], 0) - 1;
      if (preset_index < 0) or (preset_index >= max_block_presets) then
        continue;
      decoder2.DelimitedText := ini.ReadString(block_preset_ini_sections[preset_type], tmp_strings[i], '');
      if decoder2.Count < 2 then
        continue;
      width := strtoint(decoder2[0]);
      height := strtoint(decoder2[1]);
      if (width > max_block_preset_size) or (height > max_block_preset_size) then
        continue;
      preset := Addr(block_presets[preset_type, preset_index]);
      preset.width := width;
      preset.height := height;
      for j := 2 to decoder2.Count - 1 do
        preset.tiles[(j - 2) mod width, (j - 2) div width] := strtoint(decoder2[j]);
    end;
  end;

  ini.Destroy;
  tmp_strings.Destroy;
  decoder.Destroy;
  decoder2.Destroy;
end;

procedure TTileset.save_config;
var
  filename: String;
  ini: TMemIniFile;
  encoder, encoder2: TStringList;
  i, j, k, preset_type: integer;
  preset: ^TBlockPreset;
begin
  if not config_changed then
    exit;
  if current_tileset = -1 then
    exit;
  filename := tileset_config_filename;
  ini := TMemIniFile.Create(filename);
  encoder := TStringList.Create;
  encoder2 := TStringList.Create;
  encoder.Delimiter := ';';
  encoder2.Delimiter := '.';
  // Save block presets
  for preset_type := 0 to 1 do
  begin
    ini.EraseSection(block_preset_ini_sections[preset_type]);
    for i := 0 to max_block_presets - 1 do
    begin
      encoder2.Clear;
      preset := Addr(block_presets[preset_type, i]);
      if preset.width = 0 then
        continue;
      encoder2.Add(inttostr(preset.width));
      encoder2.Add(inttostr(preset.height));
      for k := 0 to preset.height - 1 do
        for j := 0 to preset.width - 1 do
          encoder2.Add(inttostr(preset.tiles[j, k]));
      ini.WriteString(block_preset_ini_sections[preset_type], inttostr(i + 1), encoder2.delimitedText);
    end;
  end;

  config_changed := false;
  ini.UpdateFile;
  ini.Destroy;
  encoder.Destroy;
  encoder2.Destroy;
end;

procedure TTileset.save_preset(preset: TBlockPresetPtr; group, preset_num: integer);
begin
  block_presets[group, preset_num] := preset^;
  config_changed := true;
end;

procedure TTileset.remove_preset(group, preset_num: integer);
begin
  block_presets[group, preset_num] := empty_block_preset;
  config_changed := true;
end;

function TTileset.block_key_to_index(key: word): integer;
var
  row, col: integer;
begin
  result := -1;
  for row := 0 to num_key_rows - 1 do
    for col := 0 to num_key_cols - 1 do
      if ord(block_preset_keys[row, col]) = key then
      begin
        result := row * num_key_cols + col;
        break;
      end;
end;

end.
