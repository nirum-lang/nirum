Serialziation format
====================

Identifier
----------

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


Behind name
-----------

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

The internal type also can be a option/set/list/map type:

    boxed box-option (text?);

    enum color = red | green | blue;
    boxed box-set ({color});

    boxed box-list ([float64]);

    boxed box-map ({uuid: datetime});

    record payload (
        box-option a,
        box-set b,
        box-list c,
        box-map d
    );

It's represented in JSON to:

    {
        "_type": "payload",
        "a": "box type of an optional type",
        "b": ["red", "green"],
        "c": [1.23, 4.56],
        "d": [
            {
                "key": "4970cd83-541d-40a8-abbc-54d5a8142007",
                "value": "2016-05-10 18:14:08.936767000+09:00"
            },
            {
                "key": "e3c2e2ec-bfb2-46a3-8373-ff0e5dad6f47",
                "value": "2016-05-10 18:15:24.175702000+09:00"
            }
        ]
    }


Option type
-----------

Unless type is optional, its value cannot be `null` in JSON representation.


Set type
--------

Set types are serialized to JSON array.  For example:

    record point (
        float64 left,
        float64 top,
    );

    record payload (
        {text} text-set,
        {point} record-set,
    );

It's represented in JSON to:

    {
        "_type": "payload",
        "text_set": [
            "set of texts",
            "the elements should be sorted"
        ],
        "record_set": [
            {
                "_type": "point",
                "left": 1.23,
                "top": 4.56
            },
            {
                "_type": "point",
                "left": 7.89,
                "top": 0.12
            }
        ]
    }

Note that duplicated elements should be eliminated when it's serialized.
If there are duplicated elements in the set when it's parsed,
only the latest element must be accepted.


List type
---------

List types are similar to set types except its order has to be preserved and
duplicated elements should be possible to exist.

For example:

    record point (
        float64 left,
        float64 top,
    );

    record payload (
        [text] text-list,
        [point] record-list,
    );

It's represented in JSON to:

    {
        "_type": "payload",
        "text_list": [
            "list of texts",
            "duplicated elements are okay",
            "duplicated elements are okay"
        ],
        "record_list": [
            {
                "_type": "point",
                "left": 1.23,
                "top": 4.56
            },
            {
                "_type": "point",
                "left": 7.89,
                "top": 0.12
            }
        ]
    }


Map type
--------

Map types are serialized to array of objects rather than objects, although
it is counterintuitive.  Because unlike JSON's object keys, Nirum's map keys
can be more complex than strings.

For example:

    record point (
        float64 left,
        float64 top,
    );

    record payload (
        {point: text} record-keys-text-values,
        {text: point} text-keys-record-values,
    );

It's represented in JSON to:

    {
        "_type": "payload",
        "record_keys_text_values": [
            {
                "key": {
                    "_type": "point",
                    "left": 1.23,
                    "top": 4.56
                },
                "value": "keys go to 'key' field and values go to 'value' field"
            }
            {
                "key": {
                    "_type": "point",
                    "left": 7.89,
                    "top": 0.12
                },
                "values": "keys are unique but values can be duplicated"
            }
        ],
        "text_keys_record_values": [
            {
                "key": "foo",
                "value": {
                    "_type": "point",
                    "left": 1.23,
                    "top": 4.56
                }
            },
            {
                "key": "bar",
                "value": {
                    "_type": "point",
                    "left": 7.89,
                    "top": 0.12
                }
            }
        ]
    }
