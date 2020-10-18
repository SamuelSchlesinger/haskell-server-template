# Haskell Server Template

A minimal template for Haskell HTTP servers.
https://github.com/SamuelSchlesinger/haskell-server-template/blob/dev/modules.png

# Code Structure

![Module Structure](https://github.com/SamuelSchlesinger/haskell-server-template/blob/main/modules.png)
Up to date

# Generated API Documentation

## GET /health

### Check the health of the server. The server will reply with a 204 status code if it is live. This form of health means that we should not restart this service, though it may not be available for all functionality.


### Check the health of the server.


### Response:

- Status code 204
- Headers: []

- No response body

## GET /ready

### Check the readiness of the server. The server will reply with a 204 status code if it is ready to receive traffic.


### Check the readiness of the server.


### Response:

- Status code 204
- Headers: []

- No response body


