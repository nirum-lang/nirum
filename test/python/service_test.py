import collections
import uuid

from nirum.transport import Transport
from pytest import fixture, raises
from six import PY2

from fixture.foo import (Dog, Gender, PingService, Product, RpcError,
                         SampleService, SampleService_Client, Way)


@fixture
def fx_dog():
    return Dog(name=u'Puppy', age=3), {
        '_type': 'animal',
        '_tag': 'dog',
        'name': 'Puppy',
        'kind': None,
        'age': 3,
        'weight': None,
    }


@fixture
def fx_method_args(fx_dog):
    return (
        collections.OrderedDict([
            ('a', fx_dog[0]),
            ('b', Product(name=u'Product.name', sale=False)),
            ('c', Gender.female),
            ('d', Way(u'way/path/text')),
            ('e', uuid.UUID('F7DB93E3-731E-48EF-80A2-CAC81E02F1AE')),
            ('f', b'binary data'),
            ('g', 1234),
            ('h', u'text data'),
        ]),
        collections.OrderedDict([
            ('a', fx_dog[1]),
            (
                'bb',
                {
                    '_type': 'product',
                    'name': 'Product.name',
                    'price': None,
                    'sale': False,
                    'url': None,
                }
            ),
            ('c', 'yeoseong'),
            ('dd', 'way/path/text'),
            ('e', 'f7db93e3-731e-48ef-80a2-cac81e02f1ae'),
            ('ff', u'YmluYXJ5IGRhdGE='),
            ('g', 1234),
            ('hh', 'text data'),
        ]),
    )


def test_throws_error():
    assert PingService.__nirum_method_error_types__('ping') is RpcError
    assert PingService.__nirum_method_error_types__('does_not_exist') is None
    assert PingService.__nirum_method_error_types__('does_not_exist', 1) == 1


def test_no_return_method():
    methods = PingService.__nirum_service_methods__
    error_types = PingService.__nirum_method_error_types__
    assert methods['no_return_method']['_return']() is None
    assert error_types('no_return_method') is None
    assert methods['no_return_method2']['_return']() is None
    assert error_types('no_return_method2') is RpcError


class DumbTransport(Transport):

    def __init__(self, successful=True, result_serialized=None):
        self.calls = []
        self.successful = successful
        self.result_serialized = result_serialized

    def call(self,
             method_name,
             payload,
             service_annotations,
             method_annotations,
             parameter_annotations):
        self.calls.append((
            method_name,
            payload,
            service_annotations,
            method_annotations,
            parameter_annotations,
        ))
        return self.successful, self.result_serialized

    @property
    def latest_call(self):
        return self.calls[-1]


def test_service_serialize_arguments(fx_method_args):
    args, expected = fx_method_args
    assert SampleService.sample_method.__nirum_serialize_arguments__(
        **args
    ) == expected
    assert SampleService.sample_method.__nirum_serialize_arguments__(
        *args.values()
    ) == expected


def test_service_argument_serializers(fx_method_args):
    args, expected = fx_method_args
    table = SampleService.sample_method.__nirum_argument_serializers__
    assert isinstance(table, collections.Mapping)
    assert len(table) == len(args)
    for (k, arg), expected_value in zip(args.items(), expected.values()):
        assert table[k](arg) == expected_value


def test_service_argument_deserializers(fx_dog, fx_method_args):
    _, payloads = fx_method_args
    table = SampleService.sample_method.__nirum_argument_deserializers__
    assert isinstance(table, collections.Mapping)
    assert frozenset(table) == frozenset(payloads)
    assert all(callable(f) for f in table.values())
    expected, a_payload = fx_dog
    assert table['a'](a_payload) == table['a'](a_payload, None) == expected
    a_invalid_payload = dict(a_payload, age='invalid', weight='invalid')
    with raises(ValueError) as e:
        table['a'](a_invalid_payload)
    assert str(e.value) == '''\
.age: Expected an integral number or a string of decimal digits.
.weight: Expected an integral number or a string of decimal digits.'''
    errors = set()
    assert table['a'](a_payload, lambda *pair: errors.add(pair)) == expected
    assert not errors
    table['a'](a_invalid_payload, lambda *pair: errors.add(pair))
    assert errors == {
        (
            '.age',
            'Expected an integral number or a string of decimal digits.',
        ),
        (
            '.weight',
            'Expected an integral number or a string of decimal digits.',
        ),
    }


def test_service_deserialize_result(fx_dog):
    expected, payload = fx_dog
    f = SampleService.sample_method_that_returns.__nirum_deserialize_result__
    assert f(payload) == expected
    invalid_payload = dict(payload, age='invalid', weight='invalid')
    with raises(ValueError) as e:
        f(invalid_payload)
    assert str(e.value) == '''\
.age: Expected an integral number or a string of decimal digits.
.weight: Expected an integral number or a string of decimal digits.'''
    errors = set()
    assert f(payload, lambda *pair: errors.add(pair)) == expected
    assert not errors
    f(invalid_payload, lambda *pair: errors.add(pair))
    assert errors == {
        (
            '.age',
            'Expected an integral number or a string of decimal digits.',
        ),
        (
            '.weight',
            'Expected an integral number or a string of decimal digits.',
        ),
    }


def test_service_deserialize_error():
    assert SampleService.sample_method.__nirum_deserialize_error__ is None
    f = PingService.ping.__nirum_deserialize_error__
    payload = {
        '_type': 'rpc_error',
        '_tag': 'not_found_error',
        'message': 'An error message.',
    }
    expected = RpcError.NotFoundError(message=u'An error message.')
    assert f(payload) == expected
    invalid_payload = dict(payload, message=['Not a string but a list.'])
    with raises(ValueError) as exc:
        f(invalid_payload)
    assert str(exc.value) == '.message: Expected a string.'
    errors = set()
    assert f(payload, lambda *pair: errors.add(pair)) == expected
    f(invalid_payload, lambda *pair: errors.add(pair))
    assert errors == {('.message', 'Expected a string.')}


def test_service_client_payload_serialization(fx_method_args):
    args, expected = fx_method_args
    t = DumbTransport()
    c = SampleService.Client(t)
    assert SampleService_Client is SampleService.Client
    c.sample_method(**args)
    assert t.latest_call[0] == 'sample_method'
    assert t.latest_call[1] == expected
    c.sample_method(*args.values())
    assert t.latest_call[0] == 'sample_method'
    assert t.latest_call[1] == expected


def test_service_client_validation(fx_method_args):
    """https://github.com/spoqa/nirum/issues/220"""
    args, expected = fx_method_args
    t = DumbTransport()
    c = SampleService.Client(t)
    c.sample_method(**args)  # ok

    # Missing argument raises TypeError
    missing_arg = dict(args)
    del missing_arg['a']
    with raises(TypeError):
        c.sample_method(**missing_arg)

    # Passing a value of unmatched type raises TypeError
    unmatched_type = dict(args, g='not bigint')
    with raises(TypeError):
        c.sample_method(**unmatched_type)

    # Passing None to non-optional parameter raises TypeError
    passing_none = dict(args, a=None)
    with raises(TypeError):
        c.sample_method(**passing_none)

    # Passing integer greater than 2^31-1 raises ValueError
    pc = PingService.Client(t)
    with raises(ValueError):
        pc.no_return_method(0x80000000)


def test_service_client_payload_deserialization():
    c1 = PingService.Client(DumbTransport(True, True))
    assert c1.ping(nonce=u'foo') is True
    c2 = PingService.Client(DumbTransport(True, False))
    assert c2.ping(nonce=u'bar') is False
    c3 = PingService.Client(
        DumbTransport(False, {
            '_type': 'rpc_error',
            '_tag': 'not_found_error',
            'message': 'An error message.',
        })
    )
    with raises(RpcError.NotFoundError) as exc:
        c3.ping(nonce=u'baz')
    assert exc.value.message == 'An error message.'


def test_service_client_payload_deserialization_error():
    c1 = PingService.Client(DumbTransport(True, 'Not a boolean'))
    with raises(ValueError) as exc:
        c1.ping(nonce=u'foo')
    assert str(exc.value) == ': Expected a boolean value; true or false.'
    c2 = PingService.Client(
        DumbTransport(False, {
            '_type': 'rpc_error',
            '_tag': 'not_found_error',
            'message': ['Not a string but a list.'],
        })
    )
    with raises(ValueError) as exc:
        c2.ping(nonce=u'bar')
    assert str(exc.value) == '.message: Expected a string.'


def test_service_client_representation():
    if PY2:
        assert repr(SampleService.Client) == "<class 'fixture.foo.Client'>"
    else:
        assert repr(SampleService.Client) == \
            "<class 'fixture.foo.SampleService.Client'>"
