#!/bin/sh
# NOTE: mustache templates need \ because they are not awesome.
exec erl -pa ebin edit deps/*/ebin -boot start_sasl \
    -I include \
    -sname kingdom_dev \
    -s kingdom \
    -s reloader
