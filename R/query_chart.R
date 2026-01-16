### Create chart in gs

query_chart <- function(...) {
  query_chart(...)
  register_chart(...)
}

# Think about first attribute and how we want to pipe it
create_chart <- function(range, ...) {
  # Should Range be created in higher functions and passed here, or should we not use a dynamic range and instead set individual start and stop series in higher functions?
  # Leaning torwards creating range in higher and passing it here

  unpack_range() # TODO Create this function to unpack range into json
  
  # Create rest of json, add unpack_range() in the correct spot

  # Pass json to query
  # Need to check the correct endpoint in mthds for gs chart creation

  
}

register_chart <- function(presentation, spreadsheet, chart_id) {

  # Get presentation object
  # Get current slide

  # Find spreadsheet_id, sheet_name, and chart_id from spreadsheet

  # Create json body for linked chart request
  # Pass to query

  # return updated state of presentation 
  # Should we also update spreadsheet to chow that the chart_id is now linked?
  # Ideally, I think yes, but it could be tricky if users also do it by hand
}


unpack_range <- function(...) {
  # Unpack range (dimensions from sheet witht he data in it) and put it into the json format we need
}