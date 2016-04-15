unit _objectinfo;

interface

uses Classes, Graphics;

const max_behaviors = 50;

type
  TObjBehaviorInfo = record
    name: String;
    prop_names: array[1..5] of string;
    prop_defaults: array[1..5] of word;
    ismonster: boolean;
  end;

type
  TObjTypeInfo = record
    behavior: word;
    prop_defaults: array[1..5] of word;
  end;

// Object info class
type
  TObjectInfo = class

  public
    behaviors: array[0..max_behaviors-1] of TObjBehaviorInfo;
    objtypes: array of TObjTypeInfo;
    objtypelist: TStringList;

    procedure init;
    function get_obj_def_behavior(obj_type: word): word;
    function get_obj_def_property(obj_type, behavior: word; prop: integer): word;
    function obj_is_monster(obj_type: word): boolean;
    function obj_is_pickup(obj_type: word): boolean;
    function obj_is_heal_one(obj_type: word): boolean;
    function obj_is_heal_full(obj_type: word): boolean;
    function get_obj_mark_color(obj_type: word): TColor;

  end;

var
  ObjectInfo: TObjectInfo;

implementation

uses main, SysUtils, IniFiles, _map;

procedure TObjectInfo.init;
var
  filename: String;
  ini: TMemIniFile;
  tmp_strings: TStringList;
  i, j, index: integer;
begin
  filename := current_dir+'/config/obj_behaviors.ini';
  ini := TMemIniFile.Create(filename);
  tmp_strings := TStringList.Create;
  ini.ReadSections(tmp_strings);
  for i := 0 to tmp_strings.Count - 1 do
  begin
    index := strtointdef(tmp_strings[i], -1);
    if (index < 0) or (index >= max_behaviors) then
      continue;
    behaviors[index].name := ini.ReadString(tmp_strings[i], 'name', '');
    for j := 1 to 5 do
      behaviors[index].prop_names[j] := ini.ReadString(tmp_strings[i], 'prop' + inttostr(j), '');
    for j := 1 to 5 do
      behaviors[index].prop_defaults[j] := ini.ReadInteger(tmp_strings[i], 'prop' + inttostr(j) + 'def', 0);
    behaviors[index].ismonster := ini.ReadBool(tmp_strings[i], 'ismonster', false);
  end;
  ini.Destroy;
  tmp_strings.Destroy;

  filename := current_dir+'/config/obj_types.ini';
  ini := TMemIniFile.Create(filename);
  objtypelist := TStringList.Create;
  ini.ReadSections(objtypelist);
  SetLength(objtypes, objtypelist.Count);
  for i := 0 to objtypelist.Count - 1 do
  begin
    objtypes[i].behavior := ini.ReadInteger(objtypelist[i], 'behavior', 65535);
    for j := 1 to 5 do
      objtypes[i].prop_defaults[j] := ini.ReadInteger(objtypelist[i], 'prop' + inttostr(j) + 'def', 65535);
  end;
  ini.Destroy;
end;

function TObjectInfo.get_obj_def_behavior(obj_type: word): word;
var
  def: ^TSpriteDefinition;
  index: integer;
begin
  def := Addr(Map.leveldata.usedSprites[obj_type]);
  index := objtypelist.IndexOf(def.name);
  if (index >= 0) and (objtypes[index].behavior <> 65535) then
    result := objtypes[index].behavior
  else
    result := def.behavior;
end;

function TObjectInfo.get_obj_def_property(obj_type, behavior: word; prop: integer): word;
var
  def: ^TSpriteDefinition;
  index: integer;
begin
  def := Addr(Map.leveldata.usedSprites[obj_type]);
  index := objtypelist.IndexOf(def.name);
  if (index >= 0) and (objtypes[index].prop_defaults[prop] <> 65535) then
    result := objtypes[index].prop_defaults[prop]
  else
    result := behaviors[behavior].prop_defaults[prop];
end;

function TObjectInfo.obj_is_monster(obj_type: word): boolean;
begin
  result := behaviors[Map.leveldata.usedSprites[obj_type].behavior].ismonster;
end;

function TObjectInfo.obj_is_pickup(obj_type: word): boolean;
var
  def: ^TSpriteDefinition;
begin
  def := Addr(Map.leveldata.usedSprites[obj_type]);
  result := (def.kind = 5) and (def.itemType <> 5);
end;

function TObjectInfo.obj_is_heal_one(obj_type: word): boolean;
var
  def: ^TSpriteDefinition;
begin
  def := Addr(Map.leveldata.usedSprites[obj_type]);
  result := (def.kind = 5) and (def.itemType = 1) and (def.fullHealth = 0);
end;

function TObjectInfo.obj_is_heal_full(obj_type: word): boolean;
var
  def: ^TSpriteDefinition;
begin
  def := Addr(Map.leveldata.usedSprites[obj_type]);
  result := (def.kind = 5) and (def.itemType = 1) and (def.fullHealth = 1);
end;

function TObjectInfo.get_obj_mark_color(obj_type: word): TColor;
begin
  result := clRed;
  if obj_is_monster(obj_type) then
    result := $0080FF;
  if obj_is_pickup(obj_type) then
    result := $8000FF;
  if obj_is_heal_one(obj_type) or obj_is_heal_full(obj_type) then
    result := $00E6B5;
end;

end.
