from fixture.foo import PingService, RpcError
from nirum.datastructures import Map


def test_annotation_as_error():
    assert issubclass(RpcError, Exception)


def test_service_method_annotation_metadata():
    expect = Map({
        'docs': Map({'docs': 'Method docs.'}),
        'http_resource': Map({'method': 'GET', 'path': '/ping'}),
        'quote': Map({'single': "'", 'triple': "'''"})
    })
    assert PingService.__nirum_method_annotations__['ping'] == expect
