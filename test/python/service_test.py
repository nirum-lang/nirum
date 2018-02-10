import uuid

from nirum.transport import Transport
from six import PY2

from fixture.foo import (Dog, Gender, PingService, Product, RpcError,
                         SampleService, SampleService_Client, Way)


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

    def __init__(self):
        self.calls = []

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
        return True, None

    @property
    def latest_call(self):
        return self.calls[-1]


def test_service_client_payload_serialization():
    t = DumbTransport()
    c = SampleService.Client(t)
    assert SampleService_Client is SampleService.Client
    c.sample_method(
        a=Dog(name=u'Dog.name', age=3),
        b=Product(name=u'Product.name', sale=False),
        c=Gender.female,
        d=Way(u'way/path/text'),
        e=uuid.UUID('F7DB93E3-731E-48EF-80A2-CAC81E02F1AE'),
        f=b'binary data',
        g=1234,
        h=u'text data'
    )
    assert t.latest_call[0] == 'sample_method'
    assert t.latest_call[1] == {
        'a': {
            '_type': 'animal',
            '_tag': 'dog',
            'name': 'Dog.name',
            'kind': None,
            'age': 3,
            'weight': None,
        },
        'bb': {
            '_type': 'product',
            'name': 'Product.name',
            'price': None,
            'sale': False,
            'url': None,
        },
        'c': 'yeoseong',
        'dd': 'way/path/text',
        'e': 'f7db93e3-731e-48ef-80a2-cac81e02f1ae',
        'ff': u'YmluYXJ5IGRhdGE=',
        'g': 1234,
        'hh': 'text data',
    }


def test_service_client_representation():
    if PY2:
        assert repr(SampleService.Client) == "<class 'fixture.foo.Client'>"
    else:
        assert repr(SampleService.Client) == \
            "<class 'fixture.foo.SampleService.Client'>"
