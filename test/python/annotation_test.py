from fixture.datetime import DatetimeService
from fixture.foo import PingService, RpcError, AnnotationTestRecord
from nirum.datastructures import Map


def test_annotation_as_error():
    assert issubclass(RpcError, Exception)


def test_service_method_annotation_metadata():
    expect = Map({
        'docs': Map({'docs': u'Method docs.'}),
        'http_resource': Map({'method': u'GET', 'path': u'/ping'}),
        'quote': Map({'single': u"'", 'triple': u"'''"}),
        'unicode': Map({'unicode': u'\uc720\ub2c8\ucf54\ub4dc'}), })
    assert PingService.__nirum_method_annotations__['ping'] == expect


def test_annotation_int():
    exp = Map({
        'num_constraints': Map({'max': 12, 'min': 1}),
    })
    assert DatetimeService.__nirum_method_annotations__['delta_month'] == exp


def test_record_annotation():
    exp = Map({
        'helloworld': Map({
            'foo': 1,
            'bar': 'foobar',
        }),
    })
    assert AnnotationTestRecord.__nirum_annotations__ == exp
