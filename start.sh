#!/bin/sh
erl -pa ebin deps/*/ebin -s adjutant_web \
    -eval "io:format(\"Point your browser at http://localhost:8081/ to use a simple websocket client~n\")."

