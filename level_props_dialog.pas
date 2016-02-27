unit level_props_dialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin, ExtCtrls, Math, Grids, ValEdit;

type
  TLevelPropertiesDialog = class(TForm)
    gbUsedSprites: TGroupBox;
    lstSpriteList: TListBox;
    btnClearMonster: TButton;
    ValueListEditor1: TValueListEditor;
    ValueListEditor2: TValueListEditor;
    ValueListEditor3: TValueListEditor;
    ValueListEditor4: TValueListEditor;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lstSpriteListClick(Sender: TObject);
    procedure ValueListEditor1StringsChange(Sender: TObject);
    procedure ValueListEditor2StringsChange(Sender: TObject);
    procedure ValueListEditor3StringsChange(Sender: TObject);
    procedure ValueListEditor4StringsChange(Sender: TObject);
  private
    updating: boolean;
  public
    procedure update_contents;
    procedure update_sprite_properties;
  end;

var
  LevelPropertiesDialog: TLevelPropertiesDialog;

implementation

uses _archive, _map, _tileset;

{$R *.dfm}

{ TLevelPropertiesDialog }

procedure TLevelPropertiesDialog.FormCreate(Sender: TObject);
var
  i: integer;
begin
  i := 0;
end;

procedure TLevelPropertiesDialog.FormKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if key = 27 then
  begin
    close;
  end;
end;

procedure TLevelPropertiesDialog.lstSpriteListClick(Sender: TObject);
begin
  update_sprite_properties;
end;

procedure TLevelPropertiesDialog.update_contents;
var
  i: integer;
  tmp_strings: TStringList;
begin
  if not Map.loaded then
    exit;
  tmp_strings := TStringList.Create;
  // Update sprite properties
  lstSpriteList.Items.Clear;
  for i := 0 to Length(Map.leveldata.usedSprites) - 1 do
  begin
    tmp_strings.Add(inttostr(i) + ' - ' + Map.leveldata.usedSprites[i].name);
  end;
  lstSpriteList.Items := tmp_strings;
  lstSpriteList.ItemIndex := 0;
  update_sprite_properties;
  tmp_strings.Clear;
  for i := 0 to length(Map.leveldata.unknown) - 1 do
  begin
    tmp_strings.Add(inttostr(i*2)+'='+inttostr(Map.leveldata.unknown[i]));
  end;
  ValueListEditor1.Strings := tmp_strings;
  tmp_strings.Clear;
  for i := 0 to length(Map.leveldata.unknown2) - 1 do
  begin
    tmp_strings.Add(inttostr(i*2)+'='+inttostr(Map.leveldata.unknown2[i]));
  end;
  ValueListEditor2.Strings := tmp_strings;
  tmp_strings.Clear;
  for i := 0 to (Map.transblock_size div 2) - 1 do
  begin
    tmp_strings.Add(inttostr(i*2)+'='+inttostr(Map.transblock_data[i]));
  end;
  ValueListEditor4.Strings := tmp_strings;
  tmp_strings.Destroy;
end;

procedure TLevelPropertiesDialog.update_sprite_properties;
var
  i: integer;
  tmp_strings: TStringList;
begin
  updating := true;
  tmp_strings := TStringList.Create;
  for i := 0 to length(Map.leveldata.usedSprites[0].unknown) - 1 do
  begin
    tmp_strings.Add(inttostr(i)+'='+inttostr(Map.leveldata.usedSprites[lstSpriteList.ItemIndex].unknown[i]));
  end;
  ValueListEditor3.Strings := tmp_strings;
  tmp_strings.Destroy;
  updating := false;
end;

procedure TLevelPropertiesDialog.ValueListEditor1StringsChange(
  Sender: TObject);
var
  leveldata: ^TLevelData;
begin
  leveldata := Addr(Map.leveldata);
  leveldata.unknown[ValueListEditor1.Row-1] := StrToIntDef(ValueListEditor1.Cells[1, ValueListEditor1.Row], 0);
end;

procedure TLevelPropertiesDialog.ValueListEditor2StringsChange(
  Sender: TObject);
var
  leveldata: ^TLevelData;
begin
  leveldata := Addr(Map.leveldata);
  leveldata.unknown2[ValueListEditor2.Row-1] := StrToIntDef(ValueListEditor2.Cells[1, ValueListEditor2.Row], 0);
end;

procedure TLevelPropertiesDialog.ValueListEditor3StringsChange(
  Sender: TObject);
var
  leveldata: ^TLevelData;
begin
  leveldata := Addr(Map.leveldata);
  leveldata.usedSprites[lstSpriteList.ItemIndex].unknown[ValueListEditor3.Row-1] := StrToIntDef(ValueListEditor3.Cells[1, ValueListEditor3.Row], 0);
end;

procedure TLevelPropertiesDialog.ValueListEditor4StringsChange(
  Sender: TObject);
begin
  Map.transblock_data[ValueListEditor4.Row - 1] := StrToIntDef(ValueListEditor4.Cells[1, ValueListEditor4.Row], 0);
end;

end.
