from fixture.foo import PingService, RpcError
from nirum.datastructures import Map


def test_annotation_as_error():
    assert issubclass(RpcError, Exception)


def test_service_method_annotation_metadata():
    expect = Map({
        'docs': Map({'docs': u'Method docs.'}),
        'http_resource': Map({'method': u'GET', 'path': u'/ping'}),
        'quote': Map({'single': u"'", 'triple': u"'''"}),
        'unicode': Map({'unicode': u'\uc720\ub2c8\ucf54\ub4dc'}),
    })
    assert PingService.__nirum_method_annotations__['ping'] == expect
