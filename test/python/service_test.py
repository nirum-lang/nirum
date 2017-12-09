from fixture.foo import PingService, RpcError


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
