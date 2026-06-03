# sht_id print method snapshot

    Code
      print(x)
    Message
      sht_id:
      Spreadsheet: "abc123"
      Sheet id: "456"

# chart_id print method snapshot

    Code
      print(x)
    Message
      chart_id:
      Spreadsheet: "abc123"
      Sheet id: "456"
      Chart id: "789"

# sht_id() errors on non-scalar spreadsheet_id

    Code
      sht_id(spreadsheet_id = c("a", "b"), sheet_id = "1")
    Condition
      Error:
      ! <r2slides::sht_id> object properties are invalid:
      - @spreadsheet_id @spreadsheet_id must be a single string

# chart_id() errors on non-scalar chart_id

    Code
      chart_id(spreadsheet_id = "abc", sheet_id = "1", chart_id = c("1", "2"))
    Condition
      Error:
      ! <r2slides::chart_id> object properties are invalid:
      - @chart_id @chart_id must be a single string

