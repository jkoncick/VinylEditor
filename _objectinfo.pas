unit _objectinfo;

interface

const max_behaviors = 50;

type
  TObjBehaviorInfo = record
    name: String;
    prop1: String;
    prop2: String;
    prop3: String;
    prop4: String;
    prop5: String;
    prop1def: integer;
    prop2def: integer;
    prop3def: integer;
    prop4def: integer;
    prop5def: integer;
    ismonster: boolean;
  end;

// Object info class
type
  TObjectInfo = class

  public
    behaviors: array[0..max_behaviors-1] of TObjBehaviorInfo;

    procedure init;

  end;

var
  ObjectInfo: TObjectInfo;

implementation

uses main, SysUtils, Classes, IniFiles;

procedure TObjectInfo.init;
var
  filename: String;
  ini: TMemIniFile;
  tmp_strings: TStringList;
  i, index: integer;
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
    behaviors[index].prop1 := ini.ReadString(tmp_strings[i], 'prop1', '');
    behaviors[index].prop2 := ini.ReadString(tmp_strings[i], 'prop2', '');
    behaviors[index].prop3 := ini.ReadString(tmp_strings[i], 'prop3', '');
    behaviors[index].prop4 := ini.ReadString(tmp_strings[i], 'prop4', '');
    behaviors[index].prop5 := ini.ReadString(tmp_strings[i], 'prop5', '');
    behaviors[index].prop1def := ini.ReadInteger(tmp_strings[i], 'prop1def', 0);
    behaviors[index].prop2def := ini.ReadInteger(tmp_strings[i], 'prop2def', 0);
    behaviors[index].prop3def := ini.ReadInteger(tmp_strings[i], 'prop3def', 0);
    behaviors[index].prop4def := ini.ReadInteger(tmp_strings[i], 'prop4def', 0);
    behaviors[index].prop5def := ini.ReadInteger(tmp_strings[i], 'prop5def', 0);
    behaviors[index].ismonster := ini.ReadBool(tmp_strings[i], 'ismonster', false);
  end;

  tmp_strings.destroy;
  ini.Destroy;
end;

end.
