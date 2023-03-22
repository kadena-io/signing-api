{ pkgs
, schemathesis
, kadena-signing-api-mock
}:
rec {
  runMockApi = pkgs.writeScript "run-mock-api" ''
    #! ${pkgs.runtimeShell}
    set -xeuo pipefail

    ${kadena-signing-api-mock}/bin/kadena-signing-api-mock
  '';

  schemathesisHooks = pkgs.writeTextFile {
    name = "schemathesis-hooks";
    destination = "/hooks.py";
    text = ''
      import schemathesis
      @schemathesis.check

      def ok_response_check(response, case):
        assert response.status_code == 200, "Expected a 200 response code"
    '';
  };

  runSchemathesis = pkgs.writeScript "run-schemathesis" ''
    #! ${pkgs.runtimeShell}
    set -xeuo pipefail

    export PYTHONPATH="${schemathesisHooks}"
    export SCHEMATHESIS_HOOKS=hooks
    ${schemathesis}/bin/schemathesis run --checks all ${./swagger.yaml} --base-url http://localhost:8080
  '';

  runSchemaTests = pkgs.writeScript "run-schema-tests" ''
    #! ${pkgs.runtimeShell}
    set -xeuo pipefail

    ${runMockApi} &
    MOCK_API_PID=$!
    function cleanup {
        echo "Stopping server..."
        kill $MOCK_API_PID
    }
    trap cleanup EXIT

    sleep 3

    ${runSchemathesis}
  '';
}
