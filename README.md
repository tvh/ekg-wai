# EKG: Remote monitoring of running processes over HTTP
[![Build status](https://github.com/tvh/ekg-wai/actions/workflows/ci.yml/badge.svg)](https://github.com/tvh/ekg-wai/actions/workflows/ci.yml)

This library lets you remotely monitor a running process over HTTP.
It provides a simple way to integrate a monitoring server into any
application.

# Getting started

Adding monitoring to your application is simple.  Just launch the
monitoring server as soon as your application starts

    import System.Remote.Monitoring.Wai
    
    main = do
         forkServer "localhost" 8000
         ...

and then visit [http://localhost:8000/](http://localhost:8000/) in
your web browser.

To make full use out of this module you must first enable GC
statistics collection in the run-time system. To enable GC
statistics collection, either run your program with

> +RTS -T

or compile it with

> -with-rtsopts=-T

The runtime overhead of `-T` is very small so it's safe to always
leave it enabled.

# JSON API

The monitoring server also lets you to retrieve the stats as JSON.
Simply send the server an HTTP GET request with the Accept header set
to "application/json":

    curl -H "Accept: application/json" http://localhost:8000/

You can use the JSON API to e.g. write applications that monitor other
applications.

# Get involved!

Please report bugs via the
[GitHub issue tracker](https://github.com/tibbe/ekg/issues).

Master [git repository](https://github.com/tvh/ekg-wai):

    git clone https://github.com/tvh/ekg-wai.git


# Authors

This library is maintained by Timo von Holtz, <tvh@tvholtz.net>.
It was originally written using Snap by Johan Tibell, <johan.tibell@gmail.com>.
