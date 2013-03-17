Client library for interacting with the 4store RDF server (http://4store.org/).

Building on the work of Patrick D Logan (https://github.com/patrickdlogan/sbcl-4store) and mon-key (https://github.com/mon-key/sbcl-4store), this is intended to be a general-purpose library for interacting with 4store. It's possible that the SPARQL in use here is compatible with other RDF servers, but I haven't tested against anything else so far.

Dependencies:
-------------

- drakma
- a working 4store server, preferably compiled with the '--enable-dedup-insert' argument to
'configure'


Exported symbols:
-----------------
- sparql-query
- sparql-server-put-data-request
- sparql-update
- insert-triples
- delete-triples
- delete-graph
- sparql-server-status-request


Usage
=====
Most usage will involve these three operations:

```lisp
(insert-triples <server-url> <graph-name> <triples>)
```
- server-url is of the form http://fully-qualified.domain.name:port
- graph-name is required. Conveniently, 4store seems to dynamically create new graphs as required, without the need for the user to explicitly create them.
- triples is a list of three-element lists. No interpolation of qnames is performed, so URIs must be fully-formed and valid.

```lisp
(delete-triples <server-url> <graph-name> <triples>)
```
- much what it looks like: the inverse of 'insert-triples, except removing them.

```lisp
(sparql-query <server-url> <graph> <return-vars> <query-params> [<optional-params>])
```
- this is a fraction more complicated; it's my attempt at a lispy interface to SPARQL
- <server-url> and <graph> work the same as in the previous two
- <return-vars> is a list of strings to be returned, without leading question-marks
- <query-params> is a list of three-element lists, to be converted into a series of triples that form the query. Where the return-vars appear here, you'll need to precede them with question-marks.
- <optional-params> allows you to query for values that might not be present, and for which NIL is a viable substitute
