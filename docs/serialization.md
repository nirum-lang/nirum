Serialziation format
====================

Identifiers
-----------

All identifiers has to be normalized in JSON representation.  Normalized rules
are:

 -  All uppercase characters become to lower.
 -  All hyphens become to underscore.
 -  All special fields have to start with an underscore prefix.

For example:

    record Payload (
        text FIELD_NAME,
        float64 second-field-name,
    )

It's represented in JSON to:

    {
        "_type": "payload",
        "field_name": "FIELD_NAME becomes to field_name",
        "second_field_name": 3.14
    }


Behind names
------------

Some names can have the behind name which differs from its facial name.
For example:

    record payload (
        text facial-name/behind-name,
    );

It's represented in JSON to:

    {
        "_type": "payload",
        "behind_name": "data goes here."
    }


Enum type
---------

Enum type is equivalent to union type of unary tags in runtime
(although it might differ if the runtime language has native enum types),
but it's equivalent to string in JSON representation.  For example:

    enum gender = male | female;

    record payload (
        gender gender,
    );

It's represented in JSON to:

    {
        "_type": "payload",
        "gender": "female"
    }


Boxed type
----------

Boxed type is equivalent to 1-member record type in runtime, but it's equivalent
to its internal type in JSON representation.  For example:

    boxed offset (float64);

    record payload (
        offset left,
    );

It's represented in JSON as:

    {
        "_type": "payload",
        "left": 3.14
    }

The internal type might be a record type as well:

    record point (
        float64 left,
        float64 top,
    );

    boxed coord (point);

    record payload (
        coord location,
    );

It's represented in JSON as:

    {
        "_type": "payload",
        "coord": {
            "_type": "point",
            "left": 1.23,
            "top": 4.56
        }
    }
