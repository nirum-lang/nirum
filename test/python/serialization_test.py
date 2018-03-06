from __future__ import print_function

import functools
import io
import json
import os
import os.path
import pkg_resources
import pprint
from typing import _type_repr

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
    deserialized = cls.__nirum_deserialize__(spec['input'])
    print('Deserialized:', pprint.pformat(deserialized))
    print('Normal:', dump_json(spec['normal']))
    serialized = deserialized.__nirum_serialize__()
    print('Serialized:', dump_json(serialized))
    assert serialized == spec['normal']
    print()
