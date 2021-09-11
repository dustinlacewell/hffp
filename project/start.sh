#!/usr/bin/env bash

runhaskell Setup.hs configure
ghcid --poll=0.5  -r -c "runhaskell Setup.hs repl"
