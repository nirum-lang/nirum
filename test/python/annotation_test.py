from fixture.foo import RpcError


def test_annotation_as_error():
    assert issubclass(RpcError, Exception)
