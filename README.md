# Haskell Server Template

A minimal template for Haskell HTTP servers using [servant](https://www.servant.dev/),
[wai](https://hackage.haskell.org/package/wai), [warp](https://hackage.haskell.org/package/warp),
[fast-logger](https://hackage.haskell.org/package/fast-logger),
[ekg](https://hackage.haskell.org/package/ekg). The aim of
this repository is to create a minimal template covering some of the
boilerplate like settings and scaffolding, minimally sane middleware,
basic monitoring capabilities, and the start of a custom Prelude for the codebase.

This repository is licensed under the MIT license in order to allow people to
use it in free software as well as proprietary.

# Code Structure

![Module Structure](https://github.com/SamuelSchlesinger/haskell-server-template/blob/main/modules.png)

# Operation

To run this server with the default configuration, run it with `cabal run server -- run`.
If you run `cabal run server -- help`, you will see:

```
usage:
name: server
|
+- subprogram: help
|
+- subprogram: run
|  |
|  +- description: runs the server using the configuration provided
|  |
|  `- option: -config <configuration-file :: FilePath>, filepath of configuration, defaults to config.json
|
`- subprogram: docs
   |
   `- description: prints out documentation for the server
```

The first invocation used the `config.json` file present in the repository,
but we can see that we can pass a custom configuration file if we want to.
After running it, the server will be available at the host, port pair that
is specified in the config, and the EKG server, if present in the config,
will be available at the host, port pair that is specified for it in the
config. If a TLS config is provided, the server will be run using TLS.

Note: The EKG server will never be run using TLS. You should never expose
the port you run it on to the outside world, otherwise people will have
access to all of your metrics. The preferred way to use it remotely in
production would be to direct traffic securely through a TLS enabled proxy,
or to port-forward under the protection of a VPN.

# Generated API Documentation

## GET /health

### Check the health of the server.


### The server will reply with a 204 status code if it is live. This status code means that we should not restart this service, though it may not be yet available for all functionality.


### Response:

- Status code 204
- Headers: []

- No response body

## GET /ready

### Check the readiness of the server.


### The server will reply with a 204 status code if it is ready to receive traffic.


### Response:

- Status code 204
- Headers: []

- No response body


