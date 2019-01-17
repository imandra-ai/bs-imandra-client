# bs-imandra-client

[![Build Status](https://travis-ci.org/AestheticIntegration/bs-imandra-client.svg?branch=master)](https://travis-ci.org/AestheticIntegration/bs-imandra-client)

A bucklescript wrapper around Imandra (specifically `imandra-http-server`).

## Prerequisites

- An imandra installation: https://docs.imandra.ai/imandra-docs/notebooks/installation-simple/

Run `imandra-repl` to register/login/start your Imandra trial interactively. After that `imandra-http-server` should also start.

## Installation into a bucklescript project

```
npm install --save https://github.com/aestheticintegration/bs-imandra-client.git
```
Then add `{ ... "bs-dependencies": ['bs-imandra-client'] }` to your `.bsconfig.json`

## Usage from a vanilla JS node project

The client is built with node compatibility in mind, but is not yet packaged for this use case yet - please let us know in Discord https://discord.gg/byQApJ3 and we'll take a look!

### Example

See the tests for a full lifecyle example, and call to verify.

Also see the [Verified React](https://github.com/AestheticIntegration/verified-react) repo for another example setup - using Jest's `globalSetup` and `globalTeardown` to keep the server alive between test suites, and using `Imandra_client.ServerInfo.from_file` to load the server information in each test suite.

State is preserved between runs in the Imandra process, as it is inherently stateful. To clear out state to maintain isolation between tests, use `Imandra_client.reset` to reset the state.
