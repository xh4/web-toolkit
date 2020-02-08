<h3 align="center">Lisp Web Toolkit</h3>

<p align="center">
    <a href="https://lisp-web-toolkit.com">lisp-web-toolkit.com</a>
</p>

# Status [![](https://travis-ci.org/xh4/web-toolkit.svg?branch=master)](https://travis-ci.org/xh4/web-toolkit)

# Systems

## HTTP
WT.HTTP provides HTTP construct as is defined in [RFC 2616 Hypertext Transfer Protocol -- HTTP/1.1](https://www.ietf.org/rfc/rfc2616.txt), together with implementation of server and client.

## WebSocket
WT.WEBSOCKET provides WebSocket functionality with interfaces inspired by [JSR 356, Java API for WebSocket](https://www.oracle.com/technetwork/articles/java/jsr356-1937161.html). The system is tested against the [Autobahn WebSocket Testsuite](https://github.com/crossbario/autobahn-testsuite).

## HTML
WT.HTML implements HTML parser, serializer and constructors.

## JSON
WT.JSON is based on [CL-JSON](https://common-lisp.net/project/cl-json/cl-json.html).

## URI
WT.URI provides URI parser and constructor, with support of UTF-8 characters, IPv6 addresses and query parameters handling. It utilize recursive descent [parser combinators](https://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf) to provide a concise implementation that is close to the definition in [RFC 3986 Uniform Resource Identifier (URI): Generic Syntax](https://tools.ietf.org/html/rfc3986).

# Roadmap

* Overall
  * Use in production environments
  * Write [documentation](https://lisp-web-toolkit.com)
  * Carefully design conditions
  * Write more tests
  * Add project build status graph
  * Add code coverage informataion
* HTTP
  * Use Asynchronous I/O in LispWorks
  * Implement server statistics and metrics monitoring
  * Implement [HTTP/2](https://tools.ietf.org/html/rfc7540)
  * Implement common handlers
* WebSocket
  * Use Asynchronous I/O in LispWorks
  * Implement server statistics and metrics monitoring
  * Support [Compression Extensions](https://tools.ietf.org/html/rfc7692)

# Author
[Xiangyu He](https://xh.coobii.com) <[xh@coobii.com](mailto:xh@coobii.com)>
