# Haskell Server Template

A minimal template for Haskell HTTP servers using [servant](https://www.servant.dev/),
[wai](https://hackage.haskell.org/package/wai), [warp](https://hackage.haskell.org/package/warp),
[fast-logger](https://hackage.haskell.org/package/fast-logger). The aim of
this repository is to create a minimal template covering some of the
boilerplate like settings for HTTP and TLS, minimally sane middleware like a
request logger and an automatic HEAD request injector, and a start on a custom
Prelude.

This repository is licensed under the MIT license in order to allow people to
use it in free software as well as proprietary.

# Code Structure

![Module Structure](https://github.com/SamuelSchlesinger/haskell-server-template/blob/main/modules.png)

# Generated API Documentation

## GET /health

### Check the health of the server.


### The server will reply with a 204 status code if it is live. This form of health means that we should not restart this service, though it may not be available for all functionality.


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


