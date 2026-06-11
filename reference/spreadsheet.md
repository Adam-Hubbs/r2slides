# R6 Class for Google Sheets Spreadsheets

An R6 class to represent a Google Sheets spreadsheet. Use
[`new_spreadsheet()`](https://adam-hubbs.github.io/r2slides/reference/new_spreadsheet.md)
or
[`register_spreadsheet()`](https://adam-hubbs.github.io/r2slides/reference/register_spreadsheet.md)
to create instances.

## Public fields

- `spreadsheet_id`:

  The Google Sheets spreadsheet ID

- `title`:

  The title of the spreadsheet

- `sheets`:

  A tibble of sheet names and IDs

## Methods

### Public methods

- [`spreadsheet$new()`](#method-spreadsheet-new)

- [`spreadsheet$set_active()`](#method-spreadsheet-set_active)

- [`spreadsheet$set_not_active()`](#method-spreadsheet-set_not_active)

- [`spreadsheet$is_active()`](#method-spreadsheet-is_active)

- [`spreadsheet$get_url()`](#method-spreadsheet-get_url)

- [`spreadsheet$browse()`](#method-spreadsheet-browse)

- [`spreadsheet$print()`](#method-spreadsheet-print)

------------------------------------------------------------------------

### Method `new()`

Create or open a spreadsheet

#### Usage

    spreadsheet$new(id = NULL, title = "Untitled Spreadsheet", set_active = TRUE)

#### Arguments

- `id`:

  Spreadsheet ID, URL, or NULL to create new

- `title`:

  Title for new spreadsheet (only used if id is NULL)

- `set_active`:

  Whether to make this the active spreadsheet

#### Returns

A new `spreadsheet` object

------------------------------------------------------------------------

### Method `set_active()`

Set this spreadsheet as the active one

#### Usage

    spreadsheet$set_active()

#### Returns

Self, invisibly

------------------------------------------------------------------------

### Method `set_not_active()`

Remove this spreadsheet as the active one

#### Usage

    spreadsheet$set_not_active()

#### Returns

Self, invisibly

------------------------------------------------------------------------

### Method `is_active()`

Check if this is the active spreadsheet

#### Usage

    spreadsheet$is_active()

#### Returns

Logical

------------------------------------------------------------------------

### Method `get_url()`

Get the Google Sheets URL for this spreadsheet

#### Usage

    spreadsheet$get_url()

#### Returns

Character URL or NULL

------------------------------------------------------------------------

### Method `browse()`

Open the spreadsheet in a browser

#### Usage

    spreadsheet$browse()

#### Returns

Self, invisibly

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print method for spreadsheet objects

#### Usage

    spreadsheet$print(...)

#### Arguments

- `...`:

  Additional arguments (unused)
