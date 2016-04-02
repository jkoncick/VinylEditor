unit _renderer;

interface

uses Graphics, Types, _map;

type EditingMarkerType = (emBrush, emSingleObject, emSelectionArea);

const object_type_colors: array[0..7] of TColor = ($000000, $40A000, $FF80C0, $00FFFF, $FFC040, $4040FF, $0000C0, $C000C0);
const object_type_marktext: array[0..7] of String = ('U', 'It ', 'Tp', 'Sw', 'Iw', 'Rw', 'M ', 'T ');

type
  TRenderer = class

  private
    // Differential rendering variables
    diffrender_old_left: word;
    diffrender_old_top: word;

    // Invalidation variables
    invalidated: boolean;
    inv_nothing: boolean;
    inv_rect: TRect;

    // Editing markers rendering variables
    bkup_bitmap: TBitmap;
    bkup_rect: TRect;
    bkup_valid: boolean;

  public
    procedure init;

    procedure load_or_create_mask(graph: TBitmap; mask: TBitmap; filename: String);

    procedure invalidate_init;
    procedure invalidate_map_tile(x, y: word);

    procedure render_map_contents(cnv_target: TCanvas; cnv_left, cnv_top, cnv_width, cnv_height: word; cnv_off_left, cnv_off_top: integer;
      data: TMapDataPtr; door_area_border: integer;
      o_bglr, o_fglr, o_transp, o_objects, o_markers, o_mark_tiles, o_show_grid,
      o_rendering_optimization: boolean);
    procedure render_minimap_contents(cnv_target: TCanvas; data: TMapDataPtr; data_width, data_height: word);

    procedure remove_editing_marker(cnv_target: TCanvas);
    procedure draw_editing_marker(cnv_target: TCanvas; cnv_left, cnv_top, cnv_width, cnv_height: word;
      data: TMapDataPtr; mark_x, mark_y, mark_width, mark_height: word; mark_style: TPenStyle; mark_color: TColor; mark_text: string);

  end;

var
  Renderer: TRenderer;

implementation

uses SysUtils, Math, main, _tileset, _archive;

procedure TRenderer.init;
begin
  // Init backup image
  bkup_bitmap := TBitmap.Create;
  bkup_bitmap.Width := 128;
  bkup_bitmap.Height := 128;
end;

procedure TRenderer.load_or_create_mask(graph, mask: TBitmap; filename: String);
var
  x, y: integer;
  black: TColor;
begin
  mask.PixelFormat := pf1bit;
  if FileExists(filename) then
    mask.LoadFromFile(filename)
  else begin
    mask.Width := graph.Width;
    mask.Height := graph.Height;
    black := $000001;
    for y := 0 to graph.Height - 1 do
      for x := 0 to graph.Width - 1 do
      begin
        if graph.Canvas.Pixels[x,y] <> black then
          mask.Canvas.Pixels[x,y] := clBlack
        else
          mask.Canvas.Pixels[x,y] := clWhite;
      end;
    mask.SaveToFile(filename);
  end;
end;

procedure TRenderer.invalidate_init;
begin
  if not invalidated then
    inv_nothing := true;
end;

procedure TRenderer.invalidate_map_tile(x, y: word);
begin
  if not invalidated then
  begin
    invalidated := true;
    inv_rect := Rect(x, y, x, y);
  end else
  begin
    inv_rect.Left := Min(inv_rect.Left, x);
    inv_rect.Top := Min(inv_rect.Top, y);
    inv_rect.Right := Max(inv_rect.Right, x);
    inv_rect.Bottom := Max(inv_rect.Bottom, y);
  end;
  inv_nothing := false;
end;

procedure TRenderer.render_map_contents(cnv_target: TCanvas; cnv_left, cnv_top, cnv_width, cnv_height: word; cnv_off_left, cnv_off_top: integer;
  data: TMapDataPtr; door_area_border: integer;
  o_bglr, o_fglr, o_transp, o_objects, o_markers, o_mark_tiles, o_show_grid,
  o_rendering_optimization: boolean);
var
  min_x, min_y, max_x, max_y: integer;
  shift_count: word;
  x, y: integer;
  xx, yy: integer;
  i: integer;
  actual_x, actual_y: integer;
  map_tile: ^TMapTile;
  tile_index, tile_x, tile_y: word;
  dest_rect: TRect;
  src_rect: TRect;
  obj: ^TLevelObject;
  mark_color: TColor;
  mark_text: String;
  index: integer;
  door: ^TDoorEntry;
  switch: ^TSwitchEntry;
  lock: ^TLockEntry;
  transblock: TTransformationBlockPtr;
begin
  if not Map.loaded then
    exit;
  min_x := 0;
  min_y := 0;
  max_x := cnv_width - 1;
  max_y := cnv_height - 1;
  cnv_target.Pen.Color := clBlack;
  cnv_target.Brush.Color := clBlack;
  //cnv_target.Rectangle(0,0,cnv_width*32, cnv_height*32);
  // Rendering optimization
  if o_rendering_optimization then
  begin
    remove_editing_marker(cnv_target);
    // Horizontal scroll
    if (cnv_left <> diffrender_old_left) and (abs(cnv_left - diffrender_old_left) < cnv_width)  then
    begin
      shift_count := abs(cnv_left - diffrender_old_left);
      if cnv_left < diffrender_old_left then
      begin
        // Scrolling left
        max_x := shift_count - 1;
        dest_rect := rect(shift_count*32,0,cnv_width*32,cnv_height*32);
        src_rect := rect(0,0,cnv_width*32-shift_count*32,cnv_height*32);
      end else
      begin
        // Scrolling right
        min_x := max_x - shift_count + 1;
        src_rect := rect(shift_count*32,0,cnv_width*32,cnv_height*32);
        dest_rect := rect(0,0,cnv_width*32-shift_count*32,cnv_height*32);
      end;
      // Shifting part of map canvas
      cnv_target.CopyRect(dest_rect,cnv_target,src_rect);
    end else
    // Vertical scroll
    if (cnv_top <> diffrender_old_top) and (abs(cnv_top - diffrender_old_top) < cnv_height)  then
    begin
      shift_count := abs(cnv_top - diffrender_old_top);
      if cnv_top < diffrender_old_top then
      begin
        // Scrolling up
        max_y := shift_count - 1;
        dest_rect := rect(0,shift_count*32,cnv_width*32,cnv_height*32);
        src_rect := rect(0,0,cnv_width*32,cnv_height*32-shift_count*32);
      end else
      begin
        // Scrolling down
        min_y := max_y - shift_count + 1;
        src_rect := rect(0,shift_count*32,cnv_width*32,cnv_height*32);
        dest_rect := rect(0,0,cnv_width*32,cnv_height*32-shift_count*32);
      end;
      // Shifting part of map canvas
      cnv_target.CopyRect(dest_rect,cnv_target,src_rect);
    end else
    // Invalidated area
    if invalidated then
    begin
      min_x := Min(Max(inv_rect.Left - cnv_left, 0), cnv_width);
      max_x := Min(Max(inv_rect.Right - cnv_left, -1), cnv_width - 1);
      min_y := Min(Max(inv_rect.Top - cnv_top, 0), cnv_height);
      max_y := Min(Max(inv_rect.Bottom - cnv_top, -1), cnv_height - 1);
      // Nothing to render
      if (min_x > max_x) or (min_y > max_y) then
      begin
        invalidated := false;
        exit;
      end;
    end else
    if inv_nothing then
    begin
      // Nothing to render
      inv_nothing := false;
      exit;
    end;
    diffrender_old_left := cnv_left;
    diffrender_old_top := cnv_top;
    invalidated := false;
    inv_nothing := false;
  end;
  // Draw map layers
  cnv_target.Pen.Width := 1;
  for y:= min_y to max_y do
  begin
    for x:= min_x to max_x do
    begin
      map_tile := Addr(data[x + cnv_left, y + cnv_top]);
      dest_rect := Rect(cnv_off_left + x*32, cnv_off_top + y*32, cnv_off_left + x*32+32, cnv_off_top + y*32+32);
      // Background layer
      if o_bglr and (map_tile.layers[0] <> tile_no_change) then
      begin
        tile_index := map_tile.layers[0];
        tile_x := tile_index mod tileset_cols;
        tile_y := tile_index div tileset_cols;
        src_rect := Rect(tile_x * 16, tile_y * 16, tile_x * 16 + 16, tile_y * 16 + 16);
        cnv_target.CopyRect(dest_rect, Tileset.tileimage.Canvas, src_rect);
      end else
      begin
        cnv_target.Brush.Color := $FFE0A0;
        cnv_target.Pen.Width := 1;
        cnv_target.Pen.Color := $FFE0A0;
        cnv_target.Rectangle(dest_rect);
      end;
      // Foreground layer
      if o_fglr and (map_tile.layers[1] <> 0) then
      begin
        tile_index := map_tile.layers[1];
        tile_x := tile_index mod tileset_cols;
        tile_y := tile_index div tileset_cols;
        src_rect := Rect(tile_x * 16, tile_y * 16, tile_x * 16 + 16, tile_y * 16 + 16);
        if o_transp then
        begin
          // Draw foreground tile transparently
          cnv_target.CopyMode := cmSrcAnd;
          cnv_target.CopyRect(dest_rect, Tileset.tileimage_mask.Canvas, src_rect);
          cnv_target.CopyMode := cmSrcPaint;
          cnv_target.CopyRect(dest_rect, Tileset.tileimage.Canvas, src_rect);
          cnv_target.CopyMode := cmSrcCopy;
        end else
          cnv_target.CopyRect(dest_rect, Tileset.tileimage.Canvas, src_rect);
      end;
      if not o_mark_tiles then
        continue;
      tile_index := map_tile.layers[0];
      if (map_tile.layers[1] <> 0) then
        tile_index := map_tile.layers[1];
      mark_color := $0;
      if tile_index < Map.leveldata.platformTiles then
        mark_color := $705070;
      if tile_index < Map.leveldata.solidTiles then
        mark_color := $0000FF;
      if Map.leveldata.safeTiles[tile_index] = 0 then
        mark_color := $006CC0;
      if (tile_index >= Map.leveldata.tileFirstClimb) and (tile_index <= Map.leveldata.tileLastClimb) then
        mark_color := $00D000;
      if (tile_index >= Map.leveldata.tileFirstLadder) and (tile_index <= Map.leveldata.tileLastLadder) then
        mark_color := $C0C0C0;
      if (tile_index >= Map.leveldata.tileFirstRail) and (tile_index <= Map.leveldata.tileLastRail) then
        mark_color := $C0C0C0;
      if (tile_index >= Map.leveldata.tileFirstFront) and (tile_index <= Map.leveldata.tileLastFront) then
        mark_color := $0;
      if (tile_index >= Map.leveldata.tileFirstBoundary) and (tile_index <= Map.leveldata.tileLastBoundary) then
        mark_color := $FF00FF;
      if (tile_index >= Map.leveldata.tileFirstItem) and (tile_index <= Map.leveldata.tileLastItem) then
        mark_color := $00FFFF;
      if mark_color = $0 then
        continue;
      cnv_target.Pen.Width := 2;
      cnv_target.Pen.Color := mark_color;
      cnv_target.MoveTo(x*32, y*32);
      cnv_target.LineTo(x*32+31, y*32+31);
      cnv_target.MoveTo(x*32+31, y*32);
      cnv_target.LineTo(x*32, y*32+31);
    end;
  end;
  cnv_target.Pen.Width := 1;
  cnv_target.Pen.Style := psSolid;
  cnv_target.Brush.Style := bsClear;
  cnv_target.Font.Color := clBlack;
  // Draw grid
  if o_show_grid then
  begin
    cnv_target.Pen.Width := 2;
    for x:= 0 to cnv_width do
    begin
      if (x + cnv_left) and 1 = 1 then
        //cnv_target.Pen.Color := clYellow
        continue
      else
        cnv_target.Pen.Color := clGray;
      cnv_target.MoveTo(x*32,0);
      cnv_target.LineTo(x*32,cnv_height*32);
    end;
    for y:= 0 to cnv_height do
    begin
      if (y + cnv_top) and 1 = 1 then
        //cnv_target.Pen.Color := clYellow
        continue
      else
        cnv_target.Pen.Color := clGray;
      cnv_target.MoveTo(0,y*32);
      cnv_target.LineTo(cnv_width*32,y*32);
    end;
  end;
  // Draw markers
  if o_markers then
  begin
    // Draw boundary markers
    cnv_target.Pen.Width := 2;
    cnv_target.Pen.Color := clRed;
    cnv_target.MoveTo((Map.leveldata.borderLeft - cnv_left) * 32,0);
    cnv_target.LineTo((Map.leveldata.borderLeft - cnv_left) * 32,cnv_height * 32);
    cnv_target.MoveTo((Map.leveldata.borderRight - cnv_left) * 32,0);
    cnv_target.LineTo((Map.leveldata.borderRight - cnv_left) * 32,cnv_height * 32);
    cnv_target.MoveTo(0, (Map.leveldata.borderTop - cnv_top) * 32);
    cnv_target.LineTo(cnv_width * 32, (Map.leveldata.borderTop - cnv_top) * 32);
    cnv_target.MoveTo(0, (Map.leveldata.borderBottom - cnv_top) * 32);
    cnv_target.LineTo(cnv_width * 32, (Map.leveldata.borderBottom - cnv_top) * 32);
    // Draw door markers
    for i := 0 to Map.leveldata.numDoors - 1 do
    begin
      door := Addr(Map.leveldata.doors[i]);
      // Door entrance
      dest_rect := Rect((door.minX - cnv_left)*32, (door.minY - cnv_top)*32, (door.maxX - cnv_left)*32+32, (door.maxY - cnv_top)*32+32);
      cnv_target.Pen.Color := $C04080;
      cnv_target.Rectangle(dest_rect);
      cnv_target.Font.Color := $C04080;
      cnv_target.TextOut(dest_rect.Left + 4, dest_rect.Top + 4, 'Door ' + inttostr(i));
      // Door destination
      cnv_target.TextOut((door.destPixelX - cnv_left * 16)*2, (door.destPixelY - cnv_top * 16)*2, 'Dest ' + inttostr(i));
      // Destination area borders
      if door_area_border = i then
      begin
        cnv_target.Pen.Color := $00FFFF;
        cnv_target.MoveTo((door.borderLeft - cnv_left) * 32, (door.borderTop - cnv_top) * 32);
        cnv_target.LineTo((door.borderLeft - cnv_left) * 32, (door.borderBottom - cnv_top) * 32);
        cnv_target.MoveTo((door.borderRight - cnv_left) * 32, (door.borderTop - cnv_top) * 32);
        cnv_target.LineTo((door.borderRight - cnv_left) * 32, (door.borderBottom - cnv_top) * 32);
        cnv_target.MoveTo((door.borderLeft - cnv_left) * 32, (door.borderTop - cnv_top) * 32);
        cnv_target.LineTo((door.borderRight - cnv_left) * 32, (door.borderTop - cnv_top) * 32);
        cnv_target.MoveTo((door.borderLeft - cnv_left) * 32, (door.borderBottom - cnv_top) * 32);
        cnv_target.LineTo((door.borderRight - cnv_left) * 32, (door.borderBottom - cnv_top) * 32);
      end;
    end;
    // Draw switch markers
    for i := 0 to Map.leveldata.numSwitches - 1 do
    begin
      switch := Addr(Map.leveldata.switches[i]);
      dest_rect := Rect((switch.posX - cnv_left)*32, (switch.posY - cnv_top)*32, (switch.posX - cnv_left)*32+32, (switch.posY - cnv_top)*32+32);
      cnv_target.Pen.Color := $00FFFF;
      cnv_target.Rectangle(dest_rect);
      cnv_target.Font.Color := $00FFFF;
      cnv_target.TextOut(dest_rect.Left + 2, dest_rect.Top + 4, 'Sw ' + inttostr(i));
      // Draw remove wall marker
      if switch.switchType = 4 then
      begin
        dest_rect := Rect((switch.var1 - cnv_left)*32, (switch.var2 - cnv_top)*32, (switch.var1 - cnv_left)*32+switch.unknown2[0]*32, (switch.var2 - cnv_top)*32+switch.unknown2[1]*32);
        cnv_target.Pen.Color := $4040FF;
        cnv_target.Rectangle(dest_rect);
        cnv_target.Font.Color := $4040FF;
        cnv_target.TextOut(dest_rect.Left + 4, dest_rect.Top + 4, 'Rw ' + inttostr(i));
      end;
    end;
    // Draw lock markers
    for i := 0 to Map.leveldata.numLocks - 1 do
    begin
      lock := Addr(Map.leveldata.locks[i]);
      dest_rect := Rect((lock.posX - cnv_left)*32, (lock.posY - cnv_top)*32, (lock.posX - cnv_left)*32+32, (lock.posY - cnv_top)*32+32);
      cnv_target.Pen.Color := $40A000;
      cnv_target.Rectangle(dest_rect);
      cnv_target.Font.Color := $40A000;
      cnv_target.TextOut(dest_rect.Left + 4, dest_rect.Top + 4, 'Lk ' + inttostr(i));
    end;
    // Draw transformation block markers
    for i := 0 to Map.leveldata.numTransblocks - 1 do
    begin
      transblock := Map.get_transblock(i);
      dest_rect := Rect((transblock.posX - cnv_left)*32, (transblock.posY - cnv_top)*32,
        (transblock.posX - cnv_left)*32+transblock.width*32, (transblock.posY - cnv_top)*32+transblock.height*32);
      cnv_target.Pen.Color := $FFC040;
      cnv_target.Rectangle(dest_rect);
      cnv_target.Font.Color := $FFC040;
      cnv_target.TextOut(dest_rect.Left + 4, dest_rect.Top + 4, 'B ' + inttostr(i));
    end;
    cnv_target.Pen.Color := clBlack;
    cnv_target.Pen.Width := 1;
  end;
  if not o_objects then
    exit;
  // Draw objects
  cnv_target.Pen.Style := psSolid;
  cnv_target.Pen.Color := clRed;
  cnv_target.Pen.Width := 1;
  cnv_target.Brush.Style := bsClear;
  cnv_target.Font.Color := clRed;
  for i := 0 to Length(Map.leveldata.objects) - 1 do
  begin
    obj := Addr(Map.leveldata.objects[i]);
    if obj.objType = 65535 then
      continue;
    dest_rect := Rect((obj.pixelX - cnv_left * 16)*2, (obj.pixelY - cnv_top * 16)*2,
      (obj.pixelX - cnv_left * 16)*2+(Map.leveldata.usedSprites[obj.objType].width+1)*2, (obj.pixelY - cnv_top * 16)*2+(Map.leveldata.usedSprites[obj.objType].height+1)*2);
    cnv_target.Rectangle(dest_rect);
    cnv_target.TextOut(dest_rect.Left + 4, dest_rect.Top + 4, inttostr(i));
    if obj.objType = 0 then
      cnv_target.TextOut(dest_rect.Left + 8, dest_rect.Top + 26, 'Start');
  end;
end;

procedure TRenderer.render_minimap_contents(cnv_target: TCanvas; data: TMapDataPtr; data_width, data_height: word);
var
  min_x, min_y, max_x, max_y: integer;
  x, y: integer;
  map_tile: ^TMapTile;
  tile_index: word;
  tile_color: TColor;
  border_x, border_y: integer;
begin
  min_x := 0;
  min_y := 0;
  max_x := data_width - 1;
  max_y := data_height - 1;
  if inv_nothing then
  begin
    // Nothing to render
    exit;
  end else
  if invalidated then
  begin
    // Render only invalidated area
    min_x := inv_rect.Left;
    max_x := inv_rect.Right;
    min_y := inv_rect.Top;
    max_y := inv_rect.Bottom;
  end else
  begin
    // Render whole minimap
    cnv_target.Brush.Color := ClBtnFace;
    cnv_target.Pen.Color := ClBtnFace;
    cnv_target.Rectangle(0,0,max_map_width,max_map_height);
  end;
  border_x := (max_map_width - data_width) div 2;
  border_y := (max_map_height - data_height) div 2;
  // Rendering contents
  for y:= min_y to max_y do
    for x:= min_x to max_x do
    begin
      // Get color according to full/empty tile in particular layer
      map_tile := Addr(data[x, y]);
      tile_index := map_tile.layers[0];
      if map_tile.layers[1] <> 0 then
        tile_index := map_tile.layers[1];
      tile_color := $FFE0A0;
      if (tile_index >= Map.leveldata.tileFirstTransp) and (tile_index <= Map.leveldata.tileLastTransp) and (tile_index = map_tile.layers[1]) then
        tile_color := $C0A060;
      if tile_index < Map.leveldata.platformTiles then
        tile_color := $705070;
      if tile_index < Map.leveldata.solidTiles then
        tile_color := $705010;
      if Map.leveldata.safeTiles[tile_index] = 0 then
        tile_color := $006CC0;
      if (tile_index >= Map.leveldata.tileFirstClimb) and (tile_index <= Map.leveldata.tileLastClimb) then
        tile_color := $00D000;
      if (tile_index >= Map.leveldata.tileFirstLadder) and (tile_index <= Map.leveldata.tileLastLadder) then
        tile_color := $C0C0C0;
      if (tile_index >= Map.leveldata.tileFirstRail) and (tile_index <= Map.leveldata.tileLastRail) then
        tile_color := $C0C0C0;
      if (tile_index >= Map.leveldata.tileFirstFront) and (tile_index <= Map.leveldata.tileLastFront) then
        tile_color := $FF00FF;
      if (tile_index >= Map.leveldata.tileFirstBoundary) and (tile_index <= Map.leveldata.tileLastBoundary) then
        tile_color := $2020D0;
      if (tile_index >= Map.leveldata.tileFirstitem) and (tile_index <= Map.leveldata.tileLastItem) then
        tile_color := $00FFFF;
      cnv_target.Pixels[x+border_x,y+border_y] := tile_color;
    end;
end;

procedure TRenderer.remove_editing_marker(cnv_target: TCanvas);
var
  src_rect: TRect;
begin
  if not bkup_valid then
    exit;
  src_rect := Rect(0, 0, bkup_rect.Right - bkup_rect.Left, bkup_rect.Bottom - bkup_rect.Top);
  cnv_target.CopyRect(bkup_rect, bkup_bitmap.Canvas, src_rect);
  bkup_valid := false;
end;

procedure TRenderer.draw_editing_marker(cnv_target: TCanvas; cnv_left, cnv_top, cnv_width, cnv_height: word;
  data: TMapDataPtr; mark_x, mark_y, mark_width, mark_height: word; mark_style: TPenStyle; mark_color: TColor; mark_text: string);
var
  dest_rect: TRect;
  bkup_width, bkup_height: integer;
begin
  // Restore old backup
  remove_editing_marker(cnv_target);
  // Make new backup
  bkup_rect := Rect(Max((mark_x - cnv_left * 16) * 2, 0), Max((mark_y - cnv_top * 16) * 2, 0), Min((mark_x + mark_width - cnv_left * 16) * 2 + 1, cnv_width * 32), Min((mark_y + mark_height - cnv_top * 16) * 2 + 1, cnv_height * 32));
  bkup_width := bkup_rect.Right - bkup_rect.Left;
  bkup_height := bkup_rect.Bottom - bkup_rect.Top;
  dest_rect := Rect(0, 0, bkup_width, bkup_height);
  bkup_bitmap.Width := Max(bkup_bitmap.Width, bkup_width);
  bkup_bitmap.Height := Max(bkup_bitmap.Height, bkup_height);
  bkup_bitmap.Canvas.CopyRect(dest_rect, cnv_target, bkup_rect);
  bkup_valid := true;
  // Draw actual_marker
  cnv_target.Brush.Style := bsClear;
  cnv_target.Pen.Style := mark_style;
  cnv_target.Pen.Color := mark_color;
  cnv_target.Pen.Width := 1;
  dest_rect := Rect((mark_x - cnv_left * 16)*2, (mark_y - cnv_top * 16)*2, (mark_x + mark_width - cnv_left * 16) * 2, (mark_y + mark_height - cnv_top * 16) * 2);
  cnv_target.Rectangle(dest_rect);
  cnv_target.Font.Color := mark_color;
  if mark_text <> '' then
    cnv_target.TextOut(dest_rect.Left + 3, dest_rect.Top + 3, mark_text);
  cnv_target.Pen.Style := psSolid;
end;

end.
