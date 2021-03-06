# Synopsis

Test code illustrating various troubleshooting and workarounds for https://github.com/racket/web-server

# Prerequisites

The code was tested with the following software versions:

- racket 8.0 CS
- curl 7.68.0
- timeout, from GNU coreutils 8.30

# Inventory

The programs in from-tcp/ demonstrate handling dropped (half-closed or half-open) TCP connections in racket.  Limitations of racket's web-server requires these to be built from TCP.

| Name | Description |
|---|---|
| [evtserve1.rkt](from-tcp/evtserve1.rkt) |  |
| [evtserve1-handle-half-closed.rkt](from-tcp/evtserve1-handle-half-closed.rkt) | Demonstrate handling tcp half-closed connections. |
| [evtserve1-handle-half-closed-half-open.sh](from-tcp/evtserve1-handle-half-closed-half-open.sh) | Demonstrate handling half-closed and half-open connections (Linux only) |
| [evtserve1-handle-half-closed-half-open.rkt](from-tcp/evtserve1-handle-half-closed-half-open.rkt) | (driven by evtserve1-handle-half-closed-half-open.sh) |

Handling half-open TCP connections is implemented with the help of LD_PRELOAD and libkeepalive.  On Ubuntu, install libkeepalive with:

    sudo apt-get install libkeepalive0


The following examples in racket-web-server/ were created in the process of troubleshooting to rule out the possibility of using racket-web-server for our application.  They demonstrate various behaviors, some sparsely documented elsewhere.

| Name | Description |
|---|---|
| serve1a.rkt | Show how server responds to a dropped TCP connection when serving a long response. |
| serve1b.rkt | Show how server responds to a dropped TCP connection when serving a slow (as opposed to long) response. |
| serve1c.rkt | Show how serve1b.rkt can be worked around by writing whitespace to the output channel in order to cause racket computation to actually get killed during a slow response. |
| serve4.rkt | An experiment to see if the serve/servelet #:servlet-responder parameter can improve error logging. |
| serve6.rkt | Builds upon serve1c.rkt workaround, allowing the dispatch path to be controlled. |
| serve6b.rkt | Builds upon serve6.rkt, logging both requests and responses as JSON, as best we can within the framework. |
| serve6c.rkt | Builds upon serve6b.rkt, adding use of the loosely coupled routine keepalive-and-timeout-via-thread. |
| serve7.rkt | Can the enable-break API somehow resurrect the ability to catch the dropped connection event? |
| serve8.rkt | The response logging mechanism of serve6b.rkt, ala carte. |

Each example shows in a comment at its end a way of exercising the example using curl and timeout.

A few other incomplete examples are provided in the directory incomplete/

