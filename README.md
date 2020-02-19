<h2 align="center">Lisp Web Toolkit</h2>

<p align="center">
    <a href="https://lisp-web-toolkit.com">lisp-web-toolkit.com</a>
</p>

TODO: Write some introduction here

### Status

<a href="https://lisp-web-toolkit.com/status">
    <img src="https://lisp-web-toolkit.com/status.png" width="750">
</a>

### Systems

#### HTTP
WT.HTTP provides HTTP construct as is defined in [RFC 7231 Hypertext Transfer Protocol (HTTP/1.1)](https://www.ietf.org/rfc/rfc7231.txt), together with implementation of server and client.

#### WebSocket
WT.WEBSOCKET provides WebSocket functionality with interfaces inspired by [JSR 356, Java API for WebSocket](https://www.oracle.com/technetwork/articles/java/jsr356-1937161.html). The system is tested against the [Autobahn WebSocket Testsuite](https://github.com/crossbario/autobahn-testsuite).

#### HTML
WT.HTML implements HTML parser, serializer and constructors.

#### JSON
WT.JSON is based on [CL-JSON](https://common-lisp.net/project/cl-json/cl-json.html).

#### URI
WT.URI provides URI parser and constructor, with support of UTF-8 characters, IPv6 addresses and query parameters handling. It utilize recursive descent [parser combinators](https://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf) to provide a concise implementation that is close to the definition in [RFC 3986 Uniform Resource Identifier (URI): Generic Syntax](https://tools.ietf.org/html/rfc3986).

### Roadmap

* Overall
  * Use in production environments
  * Write [documentation](https://lisp-web-toolkit.com)
  * Carefully design conditions and errors
  * Write more tests
  * <s>Add project build status graph</s>
  * Add code coverage informataion
* HTTP
  * Use [Asynchronous I/O](http://www.lispworks.com/documentation/lw71/LW/html/lw-192.htm) in LispWorks
  * Use [Recursive Event Dispatching (SERVE-EVENT)](https://github.com/sbcl/sbcl/blob/master/src/code/serve-event.lisp) in SBCL 
  * Abandon usocket & bordeaux-threads, write implementation-dependent code
  * Implement server statistics and metrics monitoring
  * Implement traffic throttling and message size limiting
  * Implement authentication, authorization and access control
  * Implement static file serving
  * Implement cache control
  * Implement compression
  * Implement logging
  * Implement [HTTP/2](https://tools.ietf.org/html/rfc7540)
  * Implement common handlers
  * Implement virtual host
  * <s>(Client) Support HTTPS</s>
  * (Client) Support decompression
  * (Client) Support HTTP Proxy
  * (Client) Support SOCKS Proxy
  * (Client) Support Basic Auth
  * (Client) Implement Cookie store
  * (Client) Support caching
* WebSocket
  * Use [Asynchronous I/O](http://www.lispworks.com/documentation/lw71/LW/html/lw-192.htm) in LispWorks
  * Use [Recursive Event Dispatching (SERVE-EVENT)](https://github.com/sbcl/sbcl/blob/master/src/code/serve-event.lisp) in SBCL
  * Implement server statistics and metrics monitoring
  * Implement traffic throttling and message size limiting
  * Support [Compression Extensions](https://tools.ietf.org/html/rfc7692)
  * Support WSS for both client and server
* URI
  * Test against [Web Platform Tests](https://github.com/web-platform-tests/wpt)
* HTML
  * Implement parser
  * Implement pretty print
  * Refine element constractor error report
  * Implement DOM, abandon CXML's DOM implementation
  * Test against [Web Platform Tests](https://github.com/web-platform-tests/wpt)
* JSON
  * Test against [JSONTestSuite](https://github.com/nst/JSONTestSuite)
  * Implement accessors for object
  * Implement identical transformation for JSON values (null, false, [])

### Author
[Xiangyu He](https://xh.coobii.com) <[xh@coobii.com](mailto:xh@coobii.com)>
