# R6 Class for Google Slides Presentations

An R6 class to represent and manipulate Google Slides presentations.

## Public fields

- `title`:

  The title of the presentation

- `presentation_id`:

  The Google Slides presentation ID

- `slides`:

  List of slide objects

- `page_size`:

  Dimensions of slides (height and width in PTs)

- `revision_id`:

  The current revision ID from Google Drive

- `locale`:

  The presentation locale (e.g., "en")

- `masters`:

  List of master slide metadata

- `layouts`:

  List of layout slide metadata

- `last_refreshed`:

  Timestamp of last refresh from API

## Methods

### Public methods

- [`presentation$new()`](#method-presentation-new)

- [`presentation$refresh()`](#method-presentation-refresh)

- [`presentation$delete()`](#method-presentation-delete)

- [`presentation$copy()`](#method-presentation-copy)

- [`presentation$get_slide()`](#method-presentation-get_slide)

- [`presentation$get_slide_ids()`](#method-presentation-get_slide_ids)

- [`presentation$set_active()`](#method-presentation-set_active)

- [`presentation$set_not_active()`](#method-presentation-set_not_active)

- [`presentation$is_active()`](#method-presentation-is_active)

- [`presentation$get_url()`](#method-presentation-get_url)

- [`presentation$browse()`](#method-presentation-browse)

- [`presentation$print()`](#method-presentation-print)

------------------------------------------------------------------------

### Method `new()`

Create or open a presentation

#### Usage

    presentation$new(id = NULL, title = "Untitled Presentation", set_active = TRUE)

#### Arguments

- `id`:

  Presentation ID, URL, name, or NULL to create new

- `title`:

  Title for new presentation (only used if id is NULL)

- `set_active`:

  Whether to make this the active presentation

#### Returns

A new \`presentation\` object

------------------------------------------------------------------------

### Method `refresh()`

Refresh presentation data from Google Slides API

#### Usage

    presentation$refresh()

#### Returns

Self, invisibly (for method chaining)

------------------------------------------------------------------------

### Method `delete()`

Delete the presentation from Google Drive

#### Usage

    presentation$delete()

#### Arguments

- `permanent`:

  Whether to permanently delete (TRUE) or move to trash (FALSE)

#### Returns

NULL, invisibly

------------------------------------------------------------------------

### Method `copy()`

Create a copy of the presentation

#### Usage

    presentation$copy(name = NULL)

#### Arguments

- `name`:

  Name for the copy (defaults to "Copy of original_name")

#### Returns

A new presentation object for the copy

------------------------------------------------------------------------

### Method `get_slide()`

Get slide object(s) from the presentation

#### Usage

    presentation$get_slide(index = NULL)

#### Arguments

- `index`:

  Optional index/indices of specific slides to return

#### Returns

List of slide objects, single slide object, or NULL

------------------------------------------------------------------------

### Method `get_slide_ids()`

Get slide IDs from the presentation

#### Usage

    presentation$get_slide_ids()

#### Returns

Character vector of slide object IDs

------------------------------------------------------------------------

### Method `set_active()`

Set this presentation as the active one

#### Usage

    presentation$set_active()

#### Returns

Self, invisibly (for method chaining)

------------------------------------------------------------------------

### Method `set_not_active()`

Set this presentation to not active

#### Usage

    presentation$set_not_active()

#### Returns

Self, invisibly (for method chaining)

------------------------------------------------------------------------

### Method `is_active()`

Check if this is the active presentation

#### Usage

    presentation$is_active()

#### Returns

Logical

------------------------------------------------------------------------

### Method `get_url()`

Get the Google Slides URL for this presentation

#### Usage

    presentation$get_url()

#### Returns

Character URL or NULL

------------------------------------------------------------------------

### Method `browse()`

Open the presentation in a browser

#### Usage

    presentation$browse()

#### Returns

Self, invisibly (for method chaining)

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print method for presentation objects

#### Usage

    presentation$print(...)

#### Arguments

- `...`:

  Additional arguments (unused)
