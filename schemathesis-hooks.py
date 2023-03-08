import schemathesis

@schemathesis.check
def my_check(response, case):
  assert response.status_code == 200, "Expected a 200 response code"
