import inspect

from fixture.foo import PingService, PingService_Async, RpcError
from foo_async import PongService_Async
from six import PY3


def test_throws_error():
    assert PingService.__nirum_method_error_types__('ping') is RpcError
    assert PingService.__nirum_method_error_types__('does_not_exist') is None
    assert PingService.__nirum_method_error_types__('does_not_exist', 1) == 1


def test_async_disabled():
    service = PingService_Async()
    if PY3:
        assert hasattr(service, 'ping')
        assert not inspect.iscoroutinefunction(service.ping)


def test_async_service():
    service = PongService_Async()
    if PY3:
        assert hasattr(service, 'pong')
        assert inspect.iscoroutinefunction(service.pong)
