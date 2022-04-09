# Re

Simple redirects server.  
Reads a RON file with redirects in the following format:

    {
        "/path": "https://example.com",
        "/another/path": "https://wikipedia.org",
    }

Will automatically reload the config on changes.

## Build

    stack build --ghc-options=-O2 --copy-bins
    # or
    cabal build && cabal install re

## Options

Env:
- `PORT` - default 8040
- `REDIRECTS_FILE` - default `$XDG_CONFIG_DIR/re-server/redirects.ron`

Command line:
- `--port`
- `--config` - same meaning as `REDIRECTS_FILE`

## SystemD unit

An example is provided with the repo. Be sure to replace all the mentions of
'morj' with your desired user.
