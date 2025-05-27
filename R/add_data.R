### Add data to gs


add_data <- function(data, name, url, spreadsheet_id, sheet_name) {

  data <- tibble::as_tibble(data)

  # Find the spreadsheet - TODO
  # Get sheet id from spreadsheet object. 

  # If not found, create it
  sheet <- googlesheets4::gs4_create(
    name,
    sheets = list(Main = sheet_name) # I'm now sure on this syntax
  )

  sheet <- googlesheets4::gs4_get(sheet)
  # TODO - Extract useful information and pass it to create_spreadsheet_object

}

update_data <- function(data, name, url, spreadsheet_id, sheet_name) {
  data <- tibble::as_tibble(data)

  # Find spreadsheet, error if not found
  # Find sheet_name within spreadsheet, error if not found

  # Push data
  # Need to check if we can push data of different dimensions.

  # If different dimensions, do we need to check for charts and update charts here?

  # Return updated spreadsheet object
}



