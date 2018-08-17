import datetime

from pytest import raises

from fixture.foo import Product
from fixture.foo.bar import Point
from fixture.types import DatetimeUnboxed, Int32Unboxed


try:
    UTC = datetime.timezone.utc
except AttributeError:
    from dateutil.tz import tzutc
    UTC = tzutc()


def test_int32_value_error():
    Int32Unboxed(0)
    Int32Unboxed(2 ** 15)
    Int32Unboxed(2 ** 31 - 1)
    Int32Unboxed(-(2 ** 31))
    with raises(ValueError):
        Int32Unboxed(2 ** 31)
    with raises(ValueError):
        Int32Unboxed(-(2 ** 31 + 1))


def test_int64_value_error():
    Point(x=0, y=0)
    Point(x=2 ** 31, y=0)
    Point(x=2 ** 63 - 1, y=0)
    Point(x=-(2 ** 63), y=0)
    with raises(ValueError):
        Point(x=2 ** 63, y=0)
    with raises(ValueError):
        Point(x=-(2 ** 63 + 1), y=0)


def test_datetime_value_error():
    DatetimeUnboxed(datetime.datetime(2018, 3, 11, 5, 27, tzinfo=UTC))
    with raises(ValueError):
        # Naive datetime is disallowed
        DatetimeUnboxed(datetime.datetime(2018, 3, 11, 5, 27))


def test_url_value_error():
    Product(name=u'', sale=True, url=None)  # url field is optional here
    Product(name=u'', sale=True, url='http://example.com/')
    with raises(ValueError):
        # URL cannot contain new lines
        Product(name=u'', sale=True, url='http://example.com/\n')
