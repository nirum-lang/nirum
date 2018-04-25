Transport
=========

Nirum currently only has one transport protocol which is based on HTTP and JSON.


Method calls
------------

A method call is represented as an HTTP request.  By default, it's a simple
`POST` request like the following:

~~~~~~~~ http
POST /?method=method_name HTTP/1.1
Content-Type: application/json

{}
~~~~~~~~

The above request is calling a service method named `method-name` with
no arguments.

The `method` parameter of the query string represents a method name to call.
It is a service method name.  A client (which makes a request) has to normalize
the method name.  The normalization rule is the same to serialization format's
[identifier normalization](./serialization.md#identifier).

The request content body is a JSON payload, and it represents arguments to pass.
It's a JSON object that the keys are argument names.  Keys also have to be
normalized in the same
[identifier normalization rule](./serialization.md#identifier) to serialization
format.  Each value is a serialized corresponding Nirum value according to
the [Nirum serialization format](./serialization.md).

Here are some rules that are applied to client and server differently
(it is a kind of [robustness principle]):

| End                | Client (serializer)       | Server (deserializer)      |
| ------------------ | ------------------------- | -------------------------- |
| Argument names     | Have to normalize         | Allow non-normalized names |
| Optional arguments | Do not omit if `null`     | Treat as `null` if omitted |
| Unknown arguments  | Include only known ones   | Simply ignore unknown ones |

The following is another example to call a service method named `notify`
with some arguments.  The former code is a Nirum interface and the latter is
an HTTP request:

~~~~~~~~ nirum
unboxed email-address (text);
unboxed phone-number (text);

union contact = email (email-address address)
              | telephone (phone-number number)
              ;

service notification-service (
    notify ({contact} recipients, text title, text? content)
);
~~~~~~~~

~~~~~~~~ http
POST /?method=notify HTTP/1.1
Content-Type: application/json

{
  "recipients": [
    {
      "_type": "email",
      "address": "john.doe@example.com"
    },
    {
      "_type": "telephone",
      "number": "+1 541-754-3010"
    }
  ],
  "title": "Our product is now 15% cheaper",
  "content": "See also our new pricing table!"
}
~~~~~~~~

[robustness principle]: https://en.wikipedia.org/wiki/Robustness_principle


Return values
-------------

A method may return a value or not.  If it does not have a return type
an HTTP response to an HTTP request that calls it should be simply `null`:

~~~~~~~~ http
HTTP/1.1 200 OK
Content-Type: application/json

null
~~~~~~~~

The status code `200 OK` indicates the method call is successful.

Even if it responds with any other value a client should simply ignore that.

If a method does have a return value an HTTP response to an HTTP request that
calls it should be a JSON payload that serializes a returned value which is
a serializaed Nirum value according to
the [Nirum serialization format](./serialization.md).

The following examples show a method, a request to call it, and its response:

~~~~~~~~ nirum
record product (
    uuid id,
    text name,
    int32 stock,
);

service product-service (
    product? find-product (uuid product-id)
);
~~~~~~~~

~~~~~~~~ http
POST /?method=notify HTTP/1.1
Content-Type: application/json

{
    "product_id": "9926eb5a-3893-4aee-ab19-23ebd1a1292e"
}
~~~~~~~~

~~~~~~~~ http
HTTP/1.1 200 OK
Content-Type: application/json

{
    "id": "9926eb5a-3893-4aee-ab19-23ebd1a1292e",
    "name": "White shirt",
    "stock": 100
}
~~~~~~~~

As `find-product` method's return type is optional (notice the trailing `?`),
it may return `null` as well:

~~~~~~~~ http
HTTP/1.1 200 OK
Content-Type: application/json

null
~~~~~~~~


Error values
------------

A method may `throws` a value or not.  If it has an error type that follows
a `throws` keyword a response should be either a return value or an error value.

If response's status code is `200 OK` a JSON payload in the content contains
a return value.

If the status code starts with `4` or `5` (e.g., `400 Bad Request`) a JSON
payload in the content contains an error value.  Similar to return values,
error values also are a serialized Nirum value according to
the [Nirum serialization format](./serialization.md).

The following examples show a method, a request to call it, and its response
with an error value:

~~~~~~~~ nirum
record product (
    uuid id,
    text name,
    int32 stock,
);

@error
@http-status(code="404")
unboxed product-not-found (text);

service product-service (
    product find-product (uuid product-id) throws product-not-found
);
~~~~~~~~

~~~~~~~~ http
POST /?method=notify HTTP/1.1
Content-Type: application/json

{
    "product_id": "9926eb5a-3893-4aee-ab19-23ebd1a1292e"
}
~~~~~~~~

~~~~~~~~ http
HTTP/1.1 404 Not Found
Content-Type: application/json

"There is no product with an ID \"9926eb5a-3893-4aee-ab19-23ebd1a1292e\"."
~~~~~~~~

Note that the status code is not `200 OK` but `404 Not Found`.  The JSON payload
in the response content serializes a Nirum value of `product-not-found` type.
