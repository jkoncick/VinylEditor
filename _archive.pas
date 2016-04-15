unit _archive;

interface

uses Forms, Windows, Graphics, Dialogs, Math, _tileset;

type
  TFileEntry = packed record
    hash: word;
    offset: cardinal;
  end;

type
  TTilesetInfo = record
    name: String;
    tileset_file_index: integer;
    palette_file_index: integer;
    tileset_file_name: string;
    palette_file_name: string;
    level_number: integer;
  end;

type
  TTransparentTileData = packed record
    mask: byte;
    index: word;
  end;

type TOpaqueTileDataArray = array[0..63] of word;
type TTransparentTileDataArray = array[0..63] of TTransparentTileData;

type
  TArchive = class

  public
    // Archive file variables
    archive_open_permanent: boolean;
    archive_exists: boolean;
    archive_filename: String;
    archive_filesize: Cardinal;
    archive_version: String;
    archive_file_mode: integer;
    archive_file: File of byte;

  public
    // Palette variables
    palette: array[0..255] of TColor;

    file_count: integer;
    file_list: array of TFileEntry;

    // Tileset variables
    tileset_count: word;
    tileset_info: array of TTilesetInfo;

    // Level variables
    level_base_index: Integer;
    level_count: Integer;
    level_names: array of string;

  public
    procedure init;
    procedure load_config(filename: string);

    procedure open_archive(file_mode: integer; permanent: boolean);
    procedure close_archive(force: boolean);
    procedure load_data(mem: Pointer; offset, size: Cardinal);
    procedure save_data(mem: Pointer; offset, size: Cardinal);
    procedure reserve_space_for_file(file_index: integer; size: Cardinal);

    procedure load_file_list;
    procedure load_palette(file_index: integer);
    procedure load_tileset_image(target: TBitmap; index: integer);
  end;

var
  Archive: TArchive;

implementation

{ TArchive }

uses SysUtils, Classes, IniFiles, _settings, main;

procedure TArchive.init;
var
  ini: TMemIniFile;
  tmp_strings: TStringList;
  i: integer;
  version: String;
  ini_filesize: Cardinal;
  SRec: TSearchRec;
  exe_filename: String;
  exe_filesize: Cardinal;
begin
  // Check if archive exists
  archive_filename := Settings.GameFolder + 'GODDESS.LBR';
  archive_exists := FileExists(archive_filename);
  if not archive_exists then
  begin
    Application.MessageBox('Could not find game data (GODDESS.LBR) file.'#13'Please copy the editor into your VGFM game folder'#13'or specify the game folder in VinylEditor.ini file.', 'Fatal Error', MB_ICONERROR);
    exit;
  end;

  // Check if executable exists
  exe_filename := Settings.GameFolder + 'GODDESS.EXE';
  exe_filesize := 0;
  if not FileExists(exe_filename) then
  begin
    Application.MessageBox('Could not find game executable (GODDESS.EXE) file.'#13'Please copy the editor into your VGFM game folder'#13'or specify the game folder in VinylEditor.ini file.', 'Fatal Error', MB_ICONERROR);
    exit;
  end;
  // Get GODDESS.EXE filesize
  if FindFirst(exe_filename, faAnyfile, SRec) = 0 then
  begin
    exe_filesize := SRec.Size;
    FindClose(SRec);
  end;

  // Detect game version from GODDESS.EXE filesize
  version := '';
  ini := TMemIniFile.Create(current_dir + 'config\versions.ini');
  tmp_strings := TStringList.Create;
  ini.ReadSection('Versions', tmp_strings);
  for i := 0 to tmp_strings.Count -1 do
  begin
    version := tmp_strings[i];
    ini_filesize := ini.ReadInteger('Versions', version, 0);
    if ini_filesize = exe_filesize then
    begin
      archive_version := version;
      break;
    end;
  end;
  tmp_strings.Destroy;
  if archive_version = '' then
  begin
    Application.MessageBox(PChar('Could not detect your game version - the GODDESS.EXE file size does not match any size defined in versions.ini.'#13'Using settings for this version: '+ version), 'Error', MB_ICONWARNING);
  end;

  // Load configuration from ini file
  load_config(current_dir + 'config\' + version + '.ini');

  // Load file list
  load_file_list;
end;

procedure TArchive.load_config(filename: string);
var
  ini: TMemIniFile;
  tmp_strings: TStringList;
  decoder: TStringList;
  i: integer;
begin
  // Load configuration from ini file
  ini := TMemIniFile.Create(filename);
  tmp_strings := TStringList.Create;
  decoder := TStringList.Create;
  decoder.Delimiter := ';';
  // Load basic information
  level_base_index := ini.ReadInteger('Basic', 'First_Level_File_Index', 116);
  // Load tilesets
  ini.ReadSection('Tilesets', tmp_strings);
  tileset_count := tmp_strings.Count;
  SetLength(tileset_info, tileset_count);
  for i := 0 to tileset_count -1 do
  begin
    tileset_info[i].name := tmp_strings[i];
    decoder.DelimitedText := ini.ReadString('Tilesets', tmp_strings[i], '');
    tileset_info[i].tileset_file_index := strtoint(decoder[0]);
    tileset_info[i].palette_file_index := strtoint(decoder[1]);
    tileset_info[i].tileset_file_name := decoder[2];
    tileset_info[i].palette_file_name := decoder[3];
    tileset_info[i].level_number := strtoint(decoder[4]) - 1;
  end;
  // Load levels
  ini.ReadSection('Levels', tmp_strings);
  level_count := tmp_strings.Count;
  SetLength(level_names, level_count);
  for i := 0 to level_count -1 do
    level_names[i] := tmp_strings[i];
  // Free memory
  ini.Destroy;
  tmp_strings.Destroy;
  decoder.Destroy;
end;

procedure TArchive.open_archive(file_mode: integer; permanent: boolean);
begin
  // Archive is already opened with same file mode - do nothing
  if archive_open_permanent and (archive_file_mode >= file_mode) then
    exit;
  // Archive is already opened with different file mode - close it first
  if archive_open_permanent and (archive_file_mode < file_mode) then
    close_archive(true);
  // Open archive
  archive_open_permanent := permanent;
  archive_file_mode := file_mode;
  AssignFile(archive_file, archive_filename);
  FileMode := file_mode;
  Reset(archive_file);
end;

procedure TArchive.close_archive(force: boolean);
begin
  if archive_open_permanent and not force then
    exit;
  archive_open_permanent := false;
  close(archive_file);
end;

procedure TArchive.load_data(mem: Pointer; offset, size: Cardinal);
begin
  open_archive(fmOpenRead, false);
  Seek(archive_file, offset);
  BlockRead(archive_file, mem^, size);
  close_archive(false);
end;

procedure TArchive.save_data(mem: Pointer; offset, size: Cardinal);
begin
  open_archive(fmOpenReadWrite, false);
  Seek(archive_file, offset);
  BlockWrite(archive_file, mem^, size);
  close_archive(false);
end;

procedure TArchive.reserve_space_for_file(file_index: integer; size: Cardinal);
var
  old_size: Cardinal;
  size_diff: integer;
  already_open: boolean;
  buffer: array of byte;
  buffer_size: Cardinal;
  i: integer;
begin
  old_size := file_list[file_index + 1].offset - file_list[file_index].offset;
  if size = old_size then
    exit;
  size_diff := size - old_size;
  already_open := archive_open_permanent and (archive_file_mode = fmOpenReadWrite);
  if not already_open then
    open_archive(fmOpenReadWrite, true);
  // Shift data following the file in question
  buffer_size := archive_filesize - file_list[file_index + 1].offset;
  SetLength(buffer, buffer_size);
  load_data(buffer, file_list[file_index + 1].offset, buffer_size);
  save_data(buffer, file_list[file_index].offset + size, buffer_size);
  // Adjust all offsets in the file list
  for i := file_index + 1 to file_count - 1 do
    file_list[i].offset := integer(file_list[i].offset) + size_diff;
  save_data(file_list, 2, file_count * sizeof(TFileEntry));
  // Truncate archive if the final size is smaller than before
  archive_filesize := integer(archive_filesize) + size_diff;
  if size_diff < 0 then
  begin
    Seek(archive_file, archive_filesize);
    Truncate(archive_file);
  end;
  if not already_open then
    close_archive(true);
end;

procedure TArchive.load_file_list;
begin
  open_archive(fmOpenRead, true);
  archive_filesize := filesize(archive_file);
  load_data(Addr(file_count), 0, 2);
  SetLength(file_list, file_count);
  load_data(file_list, 2, file_count * sizeof(TFileEntry));
  close_archive(true);
end;

procedure TArchive.load_palette(file_index: integer);
var
  tmp_palette: array[0..255, 0..2] of byte;
  i: integer;
begin
  load_data(Addr(tmp_palette), file_list[file_index].offset, 768);
  for i := 0 to 255 do
    palette[i] := (tmp_palette[i,0] shl 0) + (tmp_palette[i,1] shl 8) + (tmp_palette[i,2] shl 16);
end;

procedure TArchive.load_tileset_image(target: TBitmap; index: integer);
var
  file_index: integer;
  file_size: integer;
  buffer: array[0..65535] of word;
  num_tiles: integer;
  tile_data_opaque: ^TOpaqueTileDataArray;
  tile_data_transparent: ^TTransparentTileDataArray;
  pixel_data: array of byte;
  i, j, k, pos: integer;
  tile_x, tile_y: integer;
  pixel_x, pixel_y: integer;
begin
  open_archive(fmOpenRead, true);
  load_palette(tileset_info[index].palette_file_index);
  file_index := tileset_info[index].tileset_file_index;
  file_size := file_list[file_index + 1].offset - file_list[file_index].offset;
  load_data(Addr(buffer), file_list[file_index].offset, file_size);
  num_tiles := buffer[0];
  Tileset.num_tiles := num_tiles;
  // Get position of pixel data (go through all tiles)
  pos := 1;
  for i := 0 to num_tiles - 1 do
  begin
    pos := pos + buffer[pos] div 2 + 1;
  end;
  pixel_data := Addr(buffer[pos + 1]);
  // Load tiles
  tile_x := 0;
  tile_y := 0;
  target.Width := tileset_cols * 16;
  target.Height := ((num_tiles + tileset_cols - 1) div tileset_cols) * 16;
  target.PixelFormat := pf32bit;
  target.Canvas.Pen.Color := $000001;
  target.Canvas.Brush.Color := $000001;
  target.Canvas.Rectangle(Rect(0, 0, target.Width, target.Height));
  pos := 1;
  for i := 0 to num_tiles - 1 do
  begin
    // Load a tile
    pixel_x := 0;
    pixel_y := 0;
    tile_data_opaque := Addr(buffer[pos + 1]);
    tile_data_transparent := Addr(buffer[pos + 1]);
    // Opaque tiles are used only in background layer, transparent only in foreground
    Tileset.tile_layer[i] := IfThen(buffer[pos] = 192, 1, 0);
    // Load pixels for a tile
    for j := 0 to 63 do
    begin
      for k := 0 to 3 do
      begin
        if buffer[pos] <> 192 then
        begin
          // Opaque tile
          target.Canvas.Pixels[tile_x * 16 + pixel_x + k, tile_y * 16 + pixel_y] := palette[pixel_data[tile_data_opaque[j] * 4 + k]];
        end else
        begin
          // Transparent tile
          if (tile_data_transparent[j].mask and (1 shl k)) <> 0 then
            target.Canvas.Pixels[tile_x * 16 + pixel_x + k, tile_y * 16 + pixel_y] := palette[pixel_data[tile_data_transparent[j].index * 4 + k]]
        end;
      end;
      Inc(pixel_x, 4);
      if pixel_x = 16 then
      begin
        Inc(pixel_y);
        pixel_x := 0;
      end;
    end;
    // Increase coordinates
    pos := pos + buffer[pos] div 2 + 1;
    Inc(tile_x);
    if tile_x = tileset_cols then
    begin
      Inc(tile_y);
      tile_x := 0;
    end;
  end;
  close_archive(true);
end;

end.
