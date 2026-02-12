# Changelog

## Version 0.0.9055

- Element Class
- Increased test coverage from 7% to 30%ish

## Version 0.0.9050

### New Classes

- Presentation R6 class for handeling presentation state
- Slide object S7 class to represent slides
- peek(slide) shows a thumbnail of the slide in the plots pane

### Refactoring

- Refactored and moved files around - internal facing

## Version 0.0.9030

### Style Objects

- Text style objects can style and text with several different styles
- Style rule objects apply text style objects to text which matches a
  selector function

### Add Text

- Added add_text() and add_text_multi() to add text with relevant
  stylings.

## Version 0.0.9025

### Authentication Fixes

- Fixed Authentication flow and exported more helpers

### Slide Position Objects and Helpers

- New system for Slide Position using S7 objects

### Tools for Linked Charts

- Pipeable syntax for adding linked charts to Google Slides

## Version 0.0.9011

#### Built in default OAuth client

- Built in default OAuth client now means that this package will work
  from anyone’s computer (Once their google account has been
  whitelisted)

## Version 0.0.9010

#### Working version

- Moved everything over to query2 to use the discovery document for
  validation
- Now a working package

## Version 0.0.9000

#### New

- Initial Commit
- Basically just put my local functions in a package. It will not build
  yet. (Namespace stuff mainly)
- CI Workflow setup - test, coverage, and documentation website
  automatically runs

#### Bug fixes

- None
