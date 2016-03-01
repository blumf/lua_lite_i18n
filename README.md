# Lua Lite i18n

A very simple set of tools for easy handling of translatable texts in Lua source code.

Extracts marked strings that need translations, stores texts with translations in CSV format and builds a module that can be loaded at runtime with translations.

## Quick start:

### Managing the language packs
```lua
local lite_i18n = require"lite_i18n"

-- parse a script file, store it in 'en' region in a new language pack collection
local packs = { en = lite_i18n.parse_file"test.lua" }

-- write a single language pack out for sending to translator
lite_i18n.dump_pack(packs.en, "en", "to_trans.csv")

-- load in the translated texts, add to existing collection
lite_i18n.load_packs("from_trans.csv", packs)

-- store all translation for later user
lite_i18n.dump_packs(packs, "all_trans.csv")

-- write the language module
lite_i18n.write_package(packs, "i18n.lua")
```

### Using the language pack module

```lua
local lang = some_config_option()  -- assume "en", "fr", etc.
local TR = require"i18n"(lang)

print(TR"Some text that should be translated")
```

## Data structures

Translations are held in 'packs', simply a map of orginal text to translated text. All packs are held in a map of their region name/code.

```lua
packs = {
  en = {
    ["Orginal text"] = "Original text",
    ...
  },
  fr = {
    ["Orginal text"] = "Texte original",
    ...
  },
  de = {
    ["Orginal text"] = "Urtext",
    ...
  },
  ...
}
```

These packs are written to CSV in three columns: `<orginal text>,<region>,<translated text>`

## Functions

### `parse_string( src, [pack] )`
Parses a given string `src` and pull out all the `TR` marked pharses, entering them into the given pack (or new empty table if no provided)

### `parse_file( path, [pack] )`
Same as `parse_string` but loads the source from a file

### `dump_pack( pack, lang, path )`
Writes a single langauge pack to a CSV file, marking it as `lang` region

### `dump_packs( packs, path )`
Writes a whole collection of language packs to CSV file

### `load_packs( path, [packs] )`
Loads language packs from CSV file at given path into packs table (or new empty table if no provided)

### `write_package( packs, path )`
Writes a Lua module with all translations given in packs

## Thanks

The source parser was taken from LuaMinify ( https://github.com/stravant/LuaMinify ) under the MIT licence

