Vinyl Goddess from Mars level editor
------------------------------------
Version 1.0 (2016-04-16)
Made by Hisymak (kozten@seznam.cz)


1. Feature set
2. Installation
3. Modding tips
   - How background and foreground layers work
   - How to use pattern mode, block mode and presets
   - How to place objects (monsters, items, platforms etc.)
   - How to use new object types in a level
   - How to remove wall by unlocking a lock or pressing a switch
   - How to use Test Map feature
   - How to make hidden passages (secrets)
   - How to easily draw shadows around wall


1. Feature set
--------------
- Open/save maps directly from/to GODDESS.LBR file or import/export from/to a file
- Read tilesets directly from GODDESS.LBR
- Advanced map editing modes and features, allowing you make maps easily and fast:
  - Four editing modes: Basic Tile mode, Pattern mode, Block mode, Change tile type
  - Predefined Pattern and Block presets for basic tilesets 
  - Ability to add your own presets
  - Undo&Redo feature
  - Copy&Paste feature (works also between different maps)
  - Resize map, Shift map feature
  - Auto-draw shadows feature
- Full support for editing all special objects and events
  (walk-through doors, switches, locks...)
- Editing of available object types and tile properties
- Show/hide specific map layers, show grid feature
- Minimap feature (providing schematic overview of the map)
- Save map image feature
- Simple statistics (number of objects, monsters, points etc. in a map)
- Test map feature (game is directly launched in Dosbox)
- Many keyboard shortcuts and special mouse actions for Ctrl/Shift key combinations


2. Installation
---------------
- Unzip all the files into your VGFM game folder.
- VinylEditor.ini file is created after opening and closing the program for the first 
  time. There you can adust some preferences and set path for Dosbox 
  (must include dosbox.exe filename).
- The editor detects your game version by GODDESS.EXE filesize.
  Supported versions are shareware 1.0 and full.


3. Modding tips
---------------

How background and foreground layers work
-----------------------------------------
VGFM levels use two layers: background and foreground. Almost all the level tiles are
in background layer, and the foreground layer is used just for supplementary tiles.
Background layer always has a tile, while tiles in foreground can be empty.
To see it, you can turn on/off drawing of speific layers.

VGMF tilesets consist of two types of tiles: opaque and transparent tiles. Opaque tiles
can be used only on background and transparent tiles can be used only on foreground.

To make things simple, the editor automatically selects the layer where to place the
tile (so you don't need to manually switch layers). 
With right mouse button you can erase tile from foreground layer.
With middle mouse button you copy a tile under cursor (in priority mode - you select 
a tile from foreground if there is one, background otherwise).


How to use pattern mode, block mode and presets
-----------------------------------------------
Pattern mode makes painting of bricks and other patterns really easy - best is to try 
yourself and see. You can select a pattern directly from a map (by holding Shift and 
selecting an area) or use one of predefined patterns (click into pattern image on
the right side or press Space). All predefined patterns are synchronized in 
2x2, 2x4, 4x4, 9x4, 9x6 etc. grid (according to the background size in a tileset).
You can "rotate" (shift) the pattern with Num2/4/6/8 keys or Shift+Arrow keys.
Tiles from map are selected in priority mode (foreground > background), 
maximum pattern size is 18x18. 

In block mode you can basically copy and paste small or big rectangular parts of a map, 
or transfer whole parts of map into a different map (Ctrl+C, Ctrl+V).
You can select a block from map by selecting an area while holding Shift. There are two
selecting modes: "priority fg>bg" and "both layers". There are also predefined blocks.
You can also make non-rectangular blocks (for trees etc.) by making "holes" in a block
- right-click into specific tiles on the block image on the right side.
Only blocks selected in priority mode up to 18x18 can be saved as presets.

By pressing specific keys on keyboard (1,2,3,4,Q,W,E,R) you can quickly select a 
particular preset. To see keys click with the middle button in preset selection window.
Switch between patterns and blocks in preset selection window by pressing Tab.


How to place objects (monsters, items, platforms etc.)
------------------------------------------------------
Objects can be placed on "Objects" tab. Each level can contain up to 150 objects.
To place an object select a free slot in the list, change the object type to the one you
want (there are sprite filenames in the list) and place the object in a map. Default
behavior and object properties are set automatically so you don't need to adjust them 
in most cases, but some object types (platform) can have different behavior types 
(horizontal/vertical movement) or properties (speed).
The first object (index = 0) is always used as starting point - set object type to 0.

Left button = set object position
Shift + Left button = copy object and place a new object
Right button = remove object
Middle button = select object

Objects are marked with colors: 
orange = monsters, light green = healing items, purple = keys and weapons, red = others


How to use new object types in a level
--------------------------------------
Each level has predefined set of object types that can be used in it - for example one
level can use only Skeleton, Hanging eye and Walking cannon monsters, while other can
only use Big flower, Horn-on-a-stick and Fire-blowing monster.
When creating a new map, available object types are copied from the first level using
the selected tileset (for example E1L5 for Blue tileset).

Available object types can be managed in "Sprite properties" window. You will basically
want to import other object types from different levels.
- In the different level select an object type you want to copy and click Copy
- In your new level select the first empty slot and click Paste
- Remember to copy also all "Child" object types - mostly that's projectiles shot by
  monsters and make sure "Child sprite" property is set correctly
- If you delete or paste an object type in the middle of the list you need to recompute
  sprite indexes - click "Recompute sprite indexes" button.
Example: maneatpl.cmp = big flower monster, bgrensht.cmp = big green ball, 
         lgrensht.cmp = small green ball, sgreene.cmp = green explosion animation
         You must copy all these object types if you want to use Big flower monster.


How to remove wall by unlocking a lock or pressing a switch
-----------------------------------------------------------
The game can change tiles in a level as a response to touching a lock while carrying
the respective key or pressing a switch. 
Transformation blocks are used for this purpose - each lock is associated with one as
well as some switches are. A transformation block is defined with its position in a map
and tiles it contains. The block is "applied" upon activating its trigger.

To create a transformation block go to Switches or Locks tab, click "Add new" to create
an empty one, click "Select block" button and then select the block from a map.
The block is copied.
When making a locked door the process is: Draw as if the door was "open". Select a
transformation block in place where you want to place blocking wall. Then draw the wall.

Transformation blocks are not just limited to doors, but can be also used to spawn 
a platform or anything you like. Just remember that the limitation is 25 tiles per
block (or 24 if trigger is a switch) - making sizes up to 6*4, 3*8 etc. possible.


How to use Test Map feature
---------------------------
Test Map feature allows you to quickly in-game test your most recent modifications.
It can be used only if the map is saved inside game archive (GODDESS.LBR) and path
to Dosbox is configured.
When clicking "Test map", the current level is saved and the game is launched in Dosbox.
A special saved game is created - it is called "TESTMAP" and loading it transfers you
directly into the level you were editing.

Technically the player's position on the "World Map" is set to intersect with the level
marker (blue ring) so that the level is entered immediately after loading the game. The
marker position is determined by "Where you appear on MAP after you finish this level"
setting that can be found on "Other" tab. For most levels this is set to the actual
position of the level marker, but for some special cases (E1L4) it's not, that's why
"Test map" won't work properly here. When creating your custom level, remember to
set this correctly otherwise you will be surprised by appearing in different level than
you wanted to be in, or even on very strange place on world map.


How to make hidden passages (secrets)
-------------------------------------
In the tileset, several tiles (mostly in the ending part of tileset) behave as they are
always rendered in front of all sprites, so they are used for walls with a hidden 
passage. The tiles look same as solid wall tiles you normally use, so practically they 
are redundant.
The edge tiles of a hidden passage are made of "boundary" tiles - they are rendered in
front of all sprites and they are impassable.
To see this, toggle "Tiles" button and impassable tiles are marked red, boundary tiles
are purple, passable tiles are not marked.

To make things easy for you, there is "Change tile type" mode - there are two options
"Hidden passage" and "Hid. pas. boundary" - use this to transform the wall tiles into
hidden passage. Note that only some tiles in a tileset can be transformed this way.
Use right button to transform tiles back to impassable.


How to easily draw shadows around wall
--------------------------------------
The other option in "Change tile type" mode is "Shadow/Bright" tiles. This transtorms
bright tiles to shadow tiles (left button) or back (right button). Note that only
some tiles have its shadow and bright counterparts.

To make things easy for you, the editor can automatically make tiles around all walls
shadow in just one click: Shift-click into any non-wall tile that is adjacent to a wall.
The algorithm works in an enclosed area and can stop if hits any obstacle (one-tile
wide passage).
