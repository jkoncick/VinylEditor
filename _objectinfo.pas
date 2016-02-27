unit _objectinfo;

interface

const max_behaviors = 50;
const max_objtypes = 50;

type
  TObjBehaviorInfo = record
    name: String;
  end;

type
  TObjTypeInfo = record
    name: String;
  end;

// Object info class
type
  TObjectInfo = class

  public
    behaviors: array[0..max_behaviors-1] of TObjBehaviorInfo;
    objtypes: array[0..max_objtypes-1] of TObjTypeInfo;

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
  end;

  tmp_strings.destroy;
  ini.Destroy;
end;

end.
