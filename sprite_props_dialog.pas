unit sprite_props_dialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin, ExtCtrls, Math, Grids, ValEdit, Clipbrd;

type
  TSpritePropertiesDialog = class(TForm)
    lstSpriteList: TListBox;
    vlSpriteData: TValueListEditor;
    lbSpriteList: TLabel;
    lbSpriteData: TLabel;
    cbxBehavior: TComboBox;
    lbBehavior: TLabel;
    cbxKind: TComboBox;
    lbKind: TLabel;
    cbxItemType: TComboBox;
    lbItemType: TLabel;
    edFileName: TEdit;
    lbFileName: TLabel;
    lbWidth: TLabel;
    seWidth: TSpinEdit;
    seHeight: TSpinEdit;
    lbHeight: TLabel;
    seNumSprites: TSpinEdit;
    lbNumSprites: TLabel;
    lbFirstSpriteIndex: TLabel;
    seFirstSpriteIndex: TSpinEdit;
    cbFullHealth: TCheckBox;
    lbChildSprite: TLabel;
    cbxChildSprite: TComboBox;
    btnCopy: TButton;
    btnPaste: TButton;
    btnClear: TButton;
    lbChildSprite2: TLabel;
    cbxChildSprite2: TComboBox;
    btnRecompute: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lstSpriteListClick(Sender: TObject);
    procedure edFileNameChange(Sender: TObject);
    procedure SpritePropertyChange(Sender: TObject);
    procedure vlSpriteDataStringsChange(Sender: TObject);
    procedure btnCopyClick(Sender: TObject);
    procedure btnPasteClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnRecomputeClick(Sender: TObject);
  private
    updating: boolean;
    clipboard_format: cardinal;
  public
    procedure update_contents;
    procedure update_sprite_list;
    procedure update_sprite_properties;
    procedure update_mainwindow_object_types;
  end;

var
  SpritePropertiesDialog: TSpritePropertiesDialog;

implementation

uses main, _archive, _map, _tileset;

{$R *.dfm}

{ TSpritePropertiesDialog }

procedure TSpritePropertiesDialog.FormCreate(Sender: TObject);
begin
  cbxBehavior.Items := MainWindow.cbxObjectBehavior.Items;
  clipboard_format := RegisterClipboardFormat('VinylEditorSpriteDefinition');
end;

procedure TSpritePropertiesDialog.FormKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if key = 27 then
  begin
    close;
  end;
end;

procedure TSpritePropertiesDialog.lstSpriteListClick(Sender: TObject);
begin
  update_sprite_properties;
end;

procedure TSpritePropertiesDialog.edFileNameChange(Sender: TObject);
var
  def: ^TSpriteDefinition;
begin
  if updating then
    exit;
  def := Addr(Map.leveldata.usedSprites[lstSpriteList.ItemIndex]);
  StrPLCopy(def.name, edFileName.Text, High(def.name));
  update_mainwindow_object_types;
  update_sprite_list;
end;

procedure TSpritePropertiesDialog.SpritePropertyChange(Sender: TObject);
var
  def: ^TSpriteDefinition;
begin
  if updating then
    exit;
  def := Addr(Map.leveldata.usedSprites[lstSpriteList.ItemIndex]);
  def.width := StrToIntDef(seWidth.Text, 0);
  def.height := StrToIntDef(seHeight.Text, 0);
  def.behavior := cbxBehavior.ItemIndex;
  def.kind := cbxKind.ItemIndex;
  def.numSprites := StrToIntDef(seNumSprites.Text, 0);
  if cbxItemType.ItemIndex <> -1 then
    def.itemType := cbxItemType.ItemIndex;
  def.fullHealth := IfThen(cbFullHealth.Checked, 1, 0);
  def.firstSpriteIndex := StrToIntDef(seFirstSpriteIndex.Text, 0);
  def.childSprite := cbxChildSprite.ItemIndex;
  def.childSprite2 := cbxChildSprite2.ItemIndex;
end;

procedure TSpritePropertiesDialog.vlSpriteDataStringsChange(
  Sender: TObject);
var
  def: ^TSpriteDefinition;
begin
  def := Addr(Map.leveldata.usedSprites[lstSpriteList.ItemIndex]);
  def.unknown2[vlSpriteData.Row-1] := StrToIntDef(vlSpriteData.Cells[1, vlSpriteData.Row], 0);
end;

procedure TSpritePropertiesDialog.btnCopyClick(Sender: TObject);
var
  handle: THandle;
  pointer: ^TSpriteDefinition;
begin
  if not Map.loaded then
    exit;
  OpenClipboard(Application.Handle);
  EmptyClipboard;

  handle := GlobalAlloc(GMEM_DDESHARE or GMEM_MOVEABLE, SizeOf(TSpriteDefinition));
  pointer := GlobalLock(handle);

  Move(Map.leveldata.usedSprites[lstSpriteList.ItemIndex], pointer^, sizeof(TSpriteDefinition));

  GlobalUnLock(handle);
  SetClipboardData(clipboard_format, handle);
  CloseClipboard;
  lstSpriteList.SetFocus;
end;

procedure TSpritePropertiesDialog.btnPasteClick(Sender: TObject);
var
  handle: THandle;
  pointer: ^TSpriteDefinition;
  index: integer;
begin
  if not Map.loaded or not Clipboard.HasFormat(clipboard_format) then
    exit;
  OpenClipboard(Application.Handle);
  handle := GetClipboardData(clipboard_format);
  pointer := GlobalLock(handle);

  index := lstSpriteList.ItemIndex;
  Move(pointer^, Map.level_data.usedSprites[index], sizeof(TSpriteDefinition));
  if index > 0 then
    Map.level_data.usedSprites[index].firstSpriteIndex := Map.level_data.usedSprites[index-1].firstSpriteIndex + Map.level_data.usedSprites[index-1].numSprites;
  if Map.level_data.usedSprites[index].childSprite > 0 then
    Map.level_data.usedSprites[index].childSprite := index + 1;
  if Map.level_data.usedSprites[index].childSprite2 > 0 then
    Map.level_data.usedSprites[index].childSprite2 := index + 2;
  update_mainwindow_object_types;
  update_contents;

  GlobalUnLock(handle);
  CloseClipboard;
  lstSpriteList.SetFocus;
end;

procedure TSpritePropertiesDialog.btnClearClick(Sender: TObject);
var
  def: ^TSpriteDefinition;
begin
  if not Map.loaded then
    exit;
  def := Addr(Map.leveldata.usedSprites[lstSpriteList.ItemIndex]);
  FillChar(def^, sizeof(TSpriteDefinition), 0);
  def.width := 65535;
  update_mainwindow_object_types;
  update_contents;
  lstSpriteList.SetFocus;
end;

procedure TSpritePropertiesDialog.btnRecomputeClick(Sender: TObject);
var
  i: integer;
  last_index: integer;
begin
  last_index := 0;
  for i := 1 to Length(Map.leveldata.usedSprites) - 1 do
  begin
    if Map.level_data.usedSprites[i].name = '' then
      continue;
    Map.level_data.usedSprites[i].firstSpriteIndex := last_index;
    last_index := last_index + Map.level_data.usedSprites[i].numSprites;
  end;
  update_sprite_properties;
end;

procedure TSpritePropertiesDialog.update_contents;
begin
  if not Map.loaded then
    exit;
  // Update sprite properties
  update_sprite_list;
  update_sprite_properties;
end;

procedure TSpritePropertiesDialog.update_sprite_list;
var
  i: integer;
  tmp_strings: TStringList;
  last_index: integer;
begin
  tmp_strings := TStringList.Create;
  for i := 0 to Length(Map.leveldata.usedSprites) - 1 do
  begin
    tmp_strings.Add(inttostr(i) + ' - ' + Map.leveldata.usedSprites[i].name);
  end;
  last_index := lstSpriteList.ItemIndex;
  lstSpriteList.Items := tmp_strings;
  lstSpriteList.ItemIndex := Max(last_index, 0);
  tmp_strings.Clear;
  last_index := cbxChildSprite.ItemIndex;
  cbxChildSprite.Items := lstSpriteList.Items;
  cbxChildSprite.ItemIndex := last_index;
  last_index := cbxChildSprite2.ItemIndex;
  cbxChildSprite2.Items := lstSpriteList.Items;
  cbxChildSprite2.ItemIndex := last_index;
end;

procedure TSpritePropertiesDialog.update_sprite_properties;
var
  i: integer;
  def: ^TSpriteDefinition;
  tmp_strings: TStringList;
begin
  updating := true;
  def := Addr(Map.leveldata.usedSprites[lstSpriteList.ItemIndex]);
  edFileName.Text := def.name;
  seNumSprites.Value := def.numSprites;
  seFirstSpriteIndex.Value := def.firstSpriteIndex;
  seWidth.Value := def.width;
  seHeight.Value := def.height;
  cbxBehavior.ItemIndex := def.behavior;
  cbxKind.ItemIndex := def.kind;
  cbxItemType.ItemIndex := IfThen(def.kind = 5, def.itemType, -1);
  cbFullHealth.Checked := def.fullHealth <> 0;
  cbxChildSprite.ItemIndex := def.childSprite;
  cbxChildSprite2.ItemIndex := def.childSprite2;
  tmp_strings := TStringList.Create;
  for i := 0 to length(Map.leveldata.usedSprites[0].unknown2) - 1 do
  begin
    tmp_strings.Add(inttostr(i)+'='+inttostr(def.unknown2[i]));
  end;
  vlSpriteData.Strings := tmp_strings;
  tmp_strings.Destroy;
  updating := false;
end;

procedure TSpritePropertiesDialog.update_mainwindow_object_types;
begin
  Map.leveldata_dirtyflag := Map.leveldata_dirtyflag + [ufObjectTypes];
  MainWindow.update_level_data;
end;

end.
