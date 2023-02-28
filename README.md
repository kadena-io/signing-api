# Kadena Wallet Signing API

This package contains the specification of the Kadena Wallet Signing API. This
API facilitates communication between dapps and wallets. This frees dapp
developers from the complexity of managing private keys, allowing them to focus
on the functionality and business logic of the application.`

Whenever the dapp needs to send a signed transaction, all you have to do is make
an AJAX request to this API on localhost port 9467 and the user's wallet app
will handle all the details of transaction signing for you.

## Who should use this API?

* Dapp authors - Anyone writing dapps on the Kadena blockchain will want to use
  this API to make sure their Dapp works with
  [Chainweaver](https://github.com/kadena-io/chainweaver), the Kadena desktop
  wallet. The easiest way to do that is to use
  [pact-lang-api](https://github.com/kadena-io/pact-lang-api), a JavaScript
  library that handles all the details of the signing API for you.

* Wallet authors - Anyone writing a wallet for the Kadena blockchain will want
  to make their wallet expose this API so they can work seamlessly with the
  Kadena dapp ecosystem.

## API Documentation

The [swagger.yaml](swagger.yaml) file contains the generated swagger
representation of the API. You can use tools like
[swagger-codegen](https://github.com/swagger-api/swagger-codegen) to generate
client or server code in a large number of different languages.

If you would like to get familiar with the API, you can browse the online docs
here:

https://kadena-io.github.io/signing-api/
