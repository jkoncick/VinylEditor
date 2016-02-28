unit tile_props_dialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, Grids, StdCtrls, Spin, Math;

type
  TTilePropertiesDialog = class(TForm)
    TilePropsGrid: TStringGrid;
    TilePropsImage: TImage;
    lbTileIndex: TLabel;
    seUsedAnimations: TSpinEdit;
    lbUsedAnimations: TLabel;
    PaletteImage: TImage;
    lstPaletteAnims: TListBox;
    lbPaletteAnims: TLabel;
    sePaletteAnims: TSpinEdit;
    lbPaletteAnimsNum: TLabel;
    lbPaletteIndex: TLabel;
    cbxMode: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure TilePropsGridSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure TilePropsImageMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TilePropsImageMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure cbxModeChange(Sender: TObject);
    procedure seUsedAnimationsChange(Sender: TObject);
    procedure PaletteImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaletteImageMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure lstPaletteAnimsClick(Sender: TObject);
    procedure sePaletteAnimsChange(Sender: TObject);
  private
    cur_row: integer;
    changed: boolean;
  public
    procedure update_contents;
    procedure update_palette_anims;
    procedure render_tileimage;
    procedure render_palette;
  end;

var
  TilePropertiesDialog: TTilePropertiesDialog;

implementation

{$R *.dfm}

uses main, _map, _tileset, _archive;

procedure TTilePropertiesDialog.FormCreate(Sender: TObject);
var
  i: integer;
begin
  TilePropsGrid.ColWidths[1] := 64;
  TilePropsGrid.ColWidths[2] := 64;
  TilePropsGrid.ColWidths[3] := 48;
  TilePropsGrid.Cells[1, 0] := 'First tile';
  TilePropsGrid.Cells[2, 0] := 'Last tile';
  TilePropsGrid.Cells[0, 1] := 'Solid tiles';
  TilePropsGrid.Cells[0, 2] := 'Platform tiles';
  TilePropsGrid.Cells[0, 3] := 'Boundary tiles';
  TilePropsGrid.Cells[0, 4] := 'Front tiles';
  TilePropsGrid.Cells[0, 5] := 'Transparent tiles';
  TilePropsGrid.Cells[0, 6] := 'Item tiles';
  TilePropsGrid.Cells[0, 7] := 'Climbable tiles';
  TilePropsGrid.Cells[0, 8] := 'Ladder tiles';
  TilePropsGrid.Cells[0, 9] := 'Rail tiles';
  TilePropsGrid.Cells[0, 10] := 'Capsule item';
  TilePropsGrid.Cells[0, 11] := '100 Points item';
  TilePropsGrid.Cells[0, 12] := '250 Points item';
  TilePropsGrid.Cells[0, 13] := '500 Points item';
  TilePropsGrid.Cells[0, 14] := '1000 Points item';
  TilePropsGrid.Cells[0, 15] := 'Item animations';
  TilePropsGrid.Cells[0, 16] := 'Capsule anim.';
  TilePropsGrid.Cells[0, 17] := '100 Points anim.';
  TilePropsGrid.Cells[0, 18] := '250 Points anim.';
  TilePropsGrid.Cells[0, 19] := '500 Points anim.';
  TilePropsGrid.Cells[0, 20] := '1000 Points anim.';
  TilePropsGrid.Cells[0, 21] := 'No animation';
  for i := 0 to Length(Map.leveldata.tileAnimations) - 1 do
    TilePropsGrid.Cells[0, 22 + i] := 'Animation ' + inttostr(i);
  cur_row := 1;
end;

procedure TTilePropertiesDialog.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if changed then
  begin
    MainWindow.render_minimap;
    MainWindow.render_map;
    changed := false;
  end;
end;

procedure TTilePropertiesDialog.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case key of
    27: Close;
  end;
end;

procedure TTilePropertiesDialog.FormMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  lbTileIndex.Caption := '';
  lbPaletteIndex.Caption := '';
end;

procedure TTilePropertiesDialog.TilePropsGridSelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
begin
  cur_row := ARow;
  render_tileimage;
end;

procedure TTilePropertiesDialog.TilePropsImageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  tile_index: integer;
begin
  tile_index := X div 16 + (Y div 16) * tileset_cols;
  if cbxMode.ItemIndex >= 2 then
    exit;
  if cbxMode.ItemIndex = 1 then
  begin
    Map.level_data.safeTiles[tile_index] := IfThen(Button = mbLeft, 0, 1);
    render_tileimage;
    changed := true;
    exit;
  end;
  if Button = mbLeft then
  begin
    // Left button - set first tile
    case cur_row of
      1: exit;
      2: exit;
      3: Map.level_data.tileFirstBoundary := tile_index;
      4: Map.level_data.tileFirstFront := tile_index;
      5: Map.level_data.tileFirstTransp := tile_index;
      6: Map.level_data.tileFirstItem := tile_index;
      7: Map.level_data.tileFirstClimb := tile_index;
      8: Map.level_data.tileFirstLadder := tile_index;
      9: Map.level_data.tileFirstRail := tile_index;
      15: Map.level_data.tileFirstItemAnim := tile_index;
      21: exit;
    end;
    if (cur_row >= 10) and (cur_row <= 14) then
      Map.level_data.tileFirstItemType[cur_row - 10] := tile_index;
    if (cur_row >= 16) and (cur_row <= 20) then
      Map.level_data.itemAnimations[cur_row - 16].firstTile := tile_index;
    if (cur_row >= 22) then
      Map.level_data.tileAnimations[cur_row - 22].firstTile := tile_index;
    TilePropsGrid.Cells[1, cur_row] := inttostr(tile_index);
  end else
  begin
    // Right button - set last tile
    case cur_row of
      1:
        begin
          Map.level_data.solidTiles := tile_index + 1;
          TilePropsGrid.Cells[1, 2] := inttostr(Map.leveldata.solidTiles);
        end;
      2: Map.level_data.platformTiles := tile_index + 1;
      3: Map.level_data.tileLastBoundary := tile_index;
      4: Map.level_data.tileLastFront := tile_index;
      5: Map.level_data.tileLastTransp := tile_index;
      6: Map.level_data.tileLastItem := tile_index;
      7: Map.level_data.tileLastClimb := tile_index;
      8: Map.level_data.tileLastLadder := tile_index;
      9: Map.level_data.tileLastRail := tile_index;
      15: Map.level_data.tileLastItemAnim := tile_index;
      21: Map.level_data.noAnimationTiles := tile_index;
    end;
    if (cur_row >= 10) and (cur_row <= 14) then
      Map.level_data.tileLastItemType[cur_row - 10] := tile_index;
    if (cur_row >= 16) and (cur_row <= 20) then
      Map.level_data.itemAnimations[cur_row - 16].lastTile := tile_index;
    if (cur_row >= 22) then
      Map.level_data.tileAnimations[cur_row - 22].lastTile := tile_index;
    TilePropsGrid.Cells[2, cur_row] := inttostr(tile_index);
  end;
  render_tileimage;
  changed := true;
end;

procedure TTilePropertiesDialog.TilePropsImageMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  tile_index: integer;
begin
  tile_index := (X div 16) + (Y div 16) * tileset_cols;
  lbTileIndex.Caption := 'Tile: ' + inttostr(tile_index);
end;

procedure TTilePropertiesDialog.cbxModeChange(Sender: TObject);
begin
  render_tileimage;
  TilePropsGrid.Enabled := cbxMode.ItemIndex = 0;
end;

procedure TTilePropertiesDialog.seUsedAnimationsChange(Sender: TObject);
begin
  Map.level_data.numTileAnimations := seUsedAnimations.Value;
end;

procedure TTilePropertiesDialog.PaletteImageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  color_index: integer;
  entry: ^TPaletteAnimEntry;
begin
  if lstPaletteAnims.ItemIndex = -1 then
    exit;
  color_index := (X div 9) + (Y div 9) * 16;
  entry := Addr(Map.leveldata.paletteAnims[lstPaletteAnims.ItemIndex]);
  if Button = mbLeft then
  begin
    entry.firstIndex := color_index;
    entry.startIndex := color_index;
  end else
  if Button = mbRight then
    entry.lastIndex := color_index
  else
    exit;
  update_palette_anims;
end;

procedure TTilePropertiesDialog.PaletteImageMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  color_index: integer;
begin
  color_index := (X div 9) + (Y div 9) * 16;
  lbPaletteIndex.Caption := 'Color: ' + inttostr(color_index);
end;

procedure TTilePropertiesDialog.lstPaletteAnimsClick(Sender: TObject);
begin
  render_palette;
end;

procedure TTilePropertiesDialog.sePaletteAnimsChange(Sender: TObject);
begin
  Map.level_data.numPaletteAnims := StrToIntDef(sePaletteAnims.Text, 0);
  update_palette_anims;
end;

procedure TTilePropertiesDialog.update_contents;
var
  i: integer;
begin
  TilePropsGrid.Cells[1, 1] := '0';
  TilePropsGrid.Cells[2, 1] := inttostr(Map.leveldata.solidTiles - 1);
  TilePropsGrid.Cells[1, 2] := inttostr(Map.leveldata.solidTiles);
  TilePropsGrid.Cells[2, 2] := inttostr(Map.leveldata.platformTiles - 1);
  TilePropsGrid.Cells[1, 3] := inttostr(Map.leveldata.tileFirstBoundary);
  TilePropsGrid.Cells[2, 3] := inttostr(Map.leveldata.tileLastBoundary);
  TilePropsGrid.Cells[1, 4] := inttostr(Map.leveldata.tileFirstFront);
  TilePropsGrid.Cells[2, 4] := inttostr(Map.leveldata.tileLastFront);
  TilePropsGrid.Cells[1, 5] := inttostr(Map.leveldata.tileFirstTransp);
  TilePropsGrid.Cells[2, 5] := inttostr(Map.leveldata.tileLastTransp);
  TilePropsGrid.Cells[1, 6] := inttostr(Map.leveldata.tileFirstItem);
  TilePropsGrid.Cells[2, 6] := inttostr(Map.leveldata.tileLastItem);
  TilePropsGrid.Cells[1, 7] := inttostr(Map.leveldata.tileFirstClimb);
  TilePropsGrid.Cells[2, 7] := inttostr(Map.leveldata.tileLastClimb);
  TilePropsGrid.Cells[1, 8] := inttostr(Map.leveldata.tileFirstLadder);
  TilePropsGrid.Cells[2, 8] := inttostr(Map.leveldata.tileLastLadder);
  TilePropsGrid.Cells[1, 9] := inttostr(Map.leveldata.tileFirstRail);
  TilePropsGrid.Cells[2, 9] := inttostr(Map.leveldata.tileLastRail);
  for i := 0 to 4 do
  begin
    TilePropsGrid.Cells[1, 10 + i] := inttostr(Map.leveldata.tileFirstItemType[i]);
    TilePropsGrid.Cells[2, 10 + i] := inttostr(Map.leveldata.tileLastItemType[i]);
  end;
  TilePropsGrid.Cells[1, 15] := inttostr(Map.leveldata.tileFirstItemAnim);
  TilePropsGrid.Cells[2, 15] := inttostr(Map.leveldata.tileLastItemAnim);
  for i := 0 to Length(Map.leveldata.itemAnimations) - 1 do
  begin
    TilePropsGrid.Cells[1, 16 + i] := inttostr(Map.leveldata.itemAnimations[i].firstTile);
    TilePropsGrid.Cells[2, 16 + i] := inttostr(Map.leveldata.itemAnimations[i].lastTile);
    TilePropsGrid.Cells[3, 16 + i] := inttostr(Map.leveldata.itemAnimations[i].unknown);
  end;
  TilePropsGrid.Cells[1, 21] := '0';
  TilePropsGrid.Cells[2, 21] := inttostr(Map.leveldata.noAnimationTiles);
  for i := 0 to Length(Map.leveldata.tileAnimations) - 1 do
  begin
    TilePropsGrid.Cells[1, 22 + i] := inttostr(Map.leveldata.tileAnimations[i].firstTile);
    TilePropsGrid.Cells[2, 22 + i] := inttostr(Map.leveldata.tileAnimations[i].lastTile);
    TilePropsGrid.Cells[3, 22 + i] := inttostr(Map.leveldata.tileAnimations[i].unknown);
  end;
  seUsedAnimations.Value := Map.leveldata.numTileAnimations;
  sePaletteAnims.Value := Map.leveldata.numPaletteAnims;
  update_palette_anims;
  render_tileimage;
  render_palette;
  changed := false;
end;

procedure TTilePropertiesDialog.update_palette_anims;
var
  i: integer;
  entry: ^TPaletteAnimEntry;
  last_index: integer;
begin
  last_index := lstPaletteAnims.ItemIndex;
  lstPaletteAnims.Clear;
  for i := 0 to Map.leveldata.numPaletteAnims - 1 do
  begin
    entry := Addr(Map.leveldata.paletteAnims[i]);
    lstPaletteAnims.Items.Add(format('Entry %d: %d -> %d', [i, entry.firstIndex, entry.lastIndex]));
  end;
  lstPaletteAnims.ItemIndex := Min(Max(last_index, 0), Map.leveldata.numPaletteAnims - 1);
  render_palette;
end;

procedure TTilePropertiesDialog.render_tileimage;
var
  min, max: integer;
  tile_x, tile_y: integer;
  i, j, x, y: integer;
  tile_used: array[0..cnt_tileset_tiles-1] of boolean;
  preset: ^TBlockPreset;
begin
  if not Map.loaded then
    exit;
  TilePropsImage.Canvas.Brush.Style := bsSolid;
  TilePropsImage.Canvas.Brush.Color := $000001;
  TilePropsImage.Canvas.Pen.Color := $000001;
  TilePropsImage.Canvas.Rectangle(0, 0, TilePropsImage.Width, TilePropsImage.Height);
  TilePropsImage.Canvas.CopyRect(Rect(0, 0, Tileset.tileimage.Width, Tileset.tileimage.height), Tileset.tileimage.Canvas, Rect(0, 0, Tileset.tileimage.Width, Tileset.tileimage.height));
  TilePropsImage.Canvas.Brush.Style := bsClear;
  TilePropsImage.Canvas.Pen.Color := clRed;
  // Mark tiles which are used in a preset or this level
  if cbxMode.ItemIndex >= 2 then
  begin
    FillChar(tile_used, cnt_tileset_tiles, 0);
    if cbxMode.ItemIndex = 2 then
      for i := 0 to 1 do
        for j := 0 to max_block_presets - 1 do
        begin
          preset := Addr(Tileset.block_presets[i,j]);
          for x := 0 to preset.width - 1 do
            for y := 0 to preset.height - 1 do
              if preset.tiles[x, y] < cnt_tileset_tiles then
                tile_used[preset.tiles[x, y]] := true;
        end;
    if cbxMode.ItemIndex = 3 then
      for x := 0 to Map.width - 1 do
        for y := 0 to Map.height - 1 do
        begin
          tile_used[Map.data[x,y].layers[0]] := true;
          tile_used[Map.data[x,y].layers[1]] := true;
        end;
    for i := 0 to Tileset.num_tiles - 1 do
    begin
      tile_x := i mod tileset_cols;
      tile_y := i div tileset_cols;
      if tile_used[i] then
        TilePropsImage.Canvas.Rectangle(tile_x * 16 + 1, tile_y * 16 + 1, tile_x * 16 + 15, tile_y * 16 + 15);
    end;
    exit;
  end;
  // Mark hurting tiles
  if cbxMode.ItemIndex = 1 then
  begin
    for i := 0 to Tileset.num_tiles - 1 do
    begin
      tile_x := i mod tileset_cols;
      tile_y := i div tileset_cols;
      if Map.level_data.safeTiles[i] = 0 then
        TilePropsImage.Canvas.Rectangle(tile_x * 16, tile_y * 16, tile_x * 16 + 16, tile_y * 16 + 16);
    end;
    exit;
  end;
  // Mark tiles within selected range
  min := strtoint(TilePropsGrid.cells[1, cur_row]);
  max := strtoint(TilePropsGrid.cells[2, cur_row]);
  for i := min to max do
  begin
    tile_x := i mod tileset_cols;
    tile_y := i div tileset_cols;
    TilePropsImage.Canvas.Rectangle(tile_x * 16, tile_y * 16, tile_x * 16 + 16, tile_y * 16 + 16);
  end;
end;

procedure TTilePropertiesDialog.render_palette;
var
  x, y: integer;
  index: integer;
  color: TColor;
  entry: ^TPaletteAnimEntry;
  min, max: integer;
begin
  min := -1;
  max := -1;
  if lstPaletteAnims.ItemIndex <> -1 then
  begin
    entry := Addr(Map.leveldata.paletteAnims[lstPaletteAnims.ItemIndex]);
    min := entry.firstIndex;
    max := entry.lastIndex;
  end;
  for y := 0 to 15 do
    for x := 0 to 15 do
    begin
      index := y * 16 + x;
      color := Archive.palette[index];
      PaletteImage.Canvas.Brush.Color := color;
      PaletteImage.Canvas.Pen.Color := color;
      PaletteImage.Canvas.Rectangle(x*9, y*9, x*9+9, y*9+9);
      if (index >= min) and (index <= max) then
      begin
        PaletteImage.Canvas.Brush.Style := bsClear;
        PaletteImage.Canvas.Pen.Color := clRed;
        PaletteImage.Canvas.Rectangle(x*9, y*9, x*9+9, y*9+9);
        PaletteImage.Canvas.Brush.Style := bsSolid;
      end;
    end;
end;

end.
