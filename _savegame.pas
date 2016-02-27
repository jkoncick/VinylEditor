unit _savegame;

interface

const num_saves = 6;
const save_name_length = 24;

Type
  TSavEntry = packed record
    name: array[0..save_name_length-1] of char;
    pixelX: word;
    pixelY: word;
    difficulty: word;
    numKeys: word;
    unknown: word;
    episode: word;
    health: word;
    finishedLevels: array[0..26] of byte;
    unknown2: array[0..36] of byte;
    scoreDigits: array[0..6] of byte;
  end;

  TSavFile = packed record
    entries: array[0..num_saves-1] of TSavEntry;
  end;

implementation

end.
