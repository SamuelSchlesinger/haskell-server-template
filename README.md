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

The code is structured as a library, with the entrypoint in
[Server.Main](https://github.com/SamuelSchlesinger/haskell-server-template/blob/main/src/Server/Main.hs)
module and re-exported from the
[Main](https://github.com/SamuelSchlesinger/haskell-server-template/blob/main/app/Application.hs)
module as is required by Haskell applications. The server's logic is exported from
[Server.Implementation](https://github.com/SamuelSchlesinger/haskell-server-template/blob/main/src/Server/Implementation.hs),
and the tools we used to make that implementation are exported from
[Server.Context](https://github.com/SamuelSchlesinger/haskell-server-template/blob/main/src/Server/Context.hs)
and [Server.Config](https://github.com/SamuelSchlesinger/haskell-server-template/blob/main/src/Server/Config.hs).
The `Config` type is defined in the `Server.Config.*` hierarchy, and we use that to construct a
`Context`. Once we have a `Context`, we use that to run our `App` monad. Both of these types are defined
in [Server.Context](https://github.com/SamuelSchlesinger/haskell-server-template/blob/main/src/Server/Context.hs).
We can think of the `Config` as the information provided by the operator,
such as which host and port to listen on, configuring our minimum printable log
level, or containing filepaths to our TLS certificates. If we wanted to connect
to a database, we would put the connection information in the `Config`.
On the other hand, we can think of the `Context` as the dynamic structures the server
maintains, such as the [ekg](https://hackage.haskell.org/package/ekg)
[Server](https://hackage.haskell.org/package/ekg/docs/System-Remote-Monitoring.html#t:Server)
and the [fast-logger](https://hackage.haskell.org/package/fast-logger) [LoggerSet](https://hackage.haskell.org/package/fast-logger/docs/System-Log-FastLogger-LoggerSet.html#t:LoggerSet).
If we wanted to maintain a TCP connection to some other application, we would
maintain that in the `Context`.

To structure our server's API, we use the [servant](https://www.servant.dev/) library.
First, we define the API in [Servant.API](https://github.com/SamuelSchlesinger/haskell-server-template/blob/main/src/Server/API.hs),
then we implement that API in
[Servant.Implementation](https://github.com/SamuelSchlesinger/haskell-server-template/blob/main/src/Server/Implementation.hs).
Finally, we apply the HTTP and TLS config and actually run the implementation in
[Server.Main](https://github.com/SamuelSchlesinger/haskell-server-template/blob/main/src/Server/Main.hs)
using the [warp](https://hackage.haskell.org/package/warp) HTTP server, or
[warp-tls](https://hackage.haskell.org/package/warp-tls) if there are TLS settings in the configuration file.

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


