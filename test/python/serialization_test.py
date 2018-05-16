from __future__ import print_function

import functools
import io
import json
import os
import os.path
import pkg_resources
import pprint
from typing import _type_repr

from jsonpath_ng import parse
from pytest import mark


test_suite_dir = os.path.normpath(
    os.path.join(os.path.dirname(__file__), '..', 'serialization')
)


def list_specs(path):
    for f in os.listdir(path):
        filename = os.path.join(path, f)
        if os.path.isdir(filename):
            for subf in list_specs(filename):
                yield subf
        elif f.lower().endswith('.json'):
            yield filename


dump_json = functools.partial(
    json.dumps,
    indent=2,
    ensure_ascii=False,
    sort_keys=True
)


def normalize(path, value):
    parser = parse(path)
    for match in parser.find(value):
        try:
            match.value.sort(key=lambda x: json.dumps(x, sort_keys=True))
        except AttributeError:
            raise AssertionError('$.{!s} is not an array.'.format(
                match.full_path
            ))


@mark.parametrize('spec_file', list_specs(test_suite_dir))
def test_serializer_deserializer(spec_file):
    with io.open(spec_file, 'r', encoding='utf-8') as f:
        spec = json.load(f)
    print()
    print('Spec:', os.path.relpath(spec_file, test_suite_dir))
    print('Description:', spec['description'])
    print('Type:', spec['type'])
    candidates = list(
        pkg_resources.iter_entry_points('nirum.classes', name=spec['type'])
    )
    assert 'normal' in spec or 'errors' in spec, (
        'The specification must have either "normal" or "errors" field.'
    )
    assert candidates, (
        'Failed to resolve a corresponding Python class to the Nirum '
        'type "{0}".'.format(spec['type'])
    )
    assert len(candidates) < 2, (
        'Too many classes are mapped to the Nirum type name "{0}":\n'
        '{1}'.format(spec['type'], '\n'.join(map(str, candidates)))
    )
    cls = candidates[0].resolve()
    print('Class:', _type_repr(cls))
    print('Input:', dump_json(spec['input']))
    if 'normal' in spec:
        deserialized = cls.__nirum_deserialize__(spec['input'])
        print('Deserialized:', pprint.pformat(deserialized))
        print('Normal:', dump_json(spec['normal']))
        serialized = deserialized.__nirum_serialize__()
        print('Serialized:', dump_json(serialized))
        if 'ignoreOrder' in spec:
            normalize(spec['ignoreOrder'], spec['normal'])
            normalize(spec['ignoreOrder'], serialized)
        assert serialized == spec['normal']
    else:
        print('Expected errors:', dump_json(spec['errors']))
        actual_errors = set()
        deserialized = cls.__nirum_deserialize__(
            spec['input'],
            on_error=lambda field, msg: actual_errors.add((field, msg))
        )
        print(
            'Actual errors:',
            dump_json([
                {'path': p, 'message': m} for (p, m) in sorted(actual_errors)
            ])
        )
        if not actual_errors:
            print('Deserialized:', pprint.pformat(deserialized))
        assert actual_errors == frozenset(
            (e['path'], e['message'])
            for e in spec['errors']
        )
    print()
