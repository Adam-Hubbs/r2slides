# set_replacement_strategy() rejects unknown values

    Code
      set_replacement_strategy("upsert")
    Condition
      Error in `set_replacement_strategy()`:
      ! `strategy` must be one of "add", "replace", or "skip", not "upsert".

# get_replacement_strategy() warns on unknown env var and returns 'add'

    Code
      result <- get_replacement_strategy()
    Condition
      Warning:
      ! Unknown replacement strategy "turbo"; defaulting to "add".
      i Use `set_replacement_strategy()` to set a valid strategy.

# set_match_fn() rejects non-functions

    Code
      set_match_fn("not_a_function")
    Condition
      Error in `set_match_fn()`:
      ! `fn` must be a function.

# match_by_type_and_position() warns and returns NULL on multiple matches

    Code
      result <- matcher(new_spec, existing)
    Condition
      Warning:
      ! 2 existing "TEXT_BOX" elements match the new element.
      i No replacement will be made. Reduce `tolerance` or write a custom match function.

