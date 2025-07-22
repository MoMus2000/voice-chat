#!/bin/bash

erlc -o bin/ src/*.erl

# erl -noshell -pa bin/ -eval "main:start(), init:stop()."
erl -noshell -pa bin/ -eval "server:start(), init:stop()."

rm erl_crash.dump

