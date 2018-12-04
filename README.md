# bs-imandra-client

A bucklescript wrapper around Imandra (specifically `imandra-http-server`).

## Prerequisites

- An imandra installation: https://docs.imandra.ai/imandra-docs/notebooks/installation-simple/

Run `imandra-repl` to register/login/start your Imandra trial interactively. After that `imandra-http-server` should also start.

## Installation into a bucklescript project

```
npm install --save https://github.com/aestheticintegration/bs-imandra-client.git
```
Then add `{ ... "bs-dependencies": ['bs-imandra-client'] }` to your `.bsconfig.json`

## Usage from a node project

The client is built with node compatibility in mind, but is not yet packaged for this use case yet - please let us know in Discord https://discord.gg/byQApJ3 and we'll take a look!

### Example

See the tests for a full lifecyle example, and call to verify.
