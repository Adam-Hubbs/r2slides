# set_evaluation_strategy() rejects invalid strategies

    Code
      set_evaluation_strategy("invalid")
    Condition
      Error in `set_evaluation_strategy()`:
      ! `strategy` must be one of "eager" or "lazy", not "invalid".

# get_evaluation_strategy() warns on unknown env var value

    Code
      get_evaluation_strategy()
    Condition
      Warning:
      ! Unknown evaluation strategy "turbo"; defaulting to "eager".
      i Use `set_evaluation_strategy()` to set a valid strategy.
    Output
      [1] "eager"

# execute_requests() reports no pending requests when buffer is empty

    Code
      execute_requests()
    Message
      No pending requests to execute.

