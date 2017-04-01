from fixture.foo import PingService, RpcError


def test_throws_error():
    assert PingService.__nirum_method_error_types__ == {'ping': RpcError}
