#!/bin/bash

erlc -o bin/ src/*.erl

# erl -noshell -pa bin/ -eval "server:start(), init:stop()."
erl -noshell -pa bin/ -eval "client:start(), init:stop()."

rm erl_crash.dump

