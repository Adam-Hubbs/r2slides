# query errors on unrecognized endpoints

    Code
      query(endpoint = "nonexistent.endpoint", token = test_token)
    Condition
      Error:
      x Endpoint not recognized:
      ! nonexistent.endpoint
      i Endpoint must be valid according to the relevant discovery document.
      i Check that the base matches the requested endpoint.
      i Contact the package developers if you ever incounter this error.

# query errors on missing required parameters

    Code
      query(endpoint = "slides.presentations.batchUpdate", token = test_token, body = list(
        requests = list()))
    Condition
      Error:
      x Missing or malformed argument:
      Caused by error in `gargle::request_develop()`:
      ! These parameters are missing:
      x 'presentationId'
      i API endpoint: 'slides.presentations.batchUpdate'

# query errors when passed a base that conflicts with the endpoint

    Code
      query(endpoint = "slides.presentations.batchUpdate", params = list(
        presentationId = "test123"), body = list(requests = list()), base = "sheets",
      token = test_token)
    Condition
      Error:
      x Endpoint not recognized:
      ! slides.presentations.batchUpdate
      i Endpoint must be valid according to the relevant discovery document.
      i Check that the base matches the requested endpoint.
      i Contact the package developers if you ever incounter this error.

# query errors on wrong parameters

    Code
      query(endpoint = "slides.presentations.batchUpdate", params = test_params,
        body = test_body, base = "slides", token = test_token)
    Condition
      Error:
      x Missing or malformed argument:
      Caused by error in `gargle::request_develop()`:
      ! These parameters are unknown:
      x 'wrong_param'
      i API endpoint: 'slides.presentations.batchUpdate'

