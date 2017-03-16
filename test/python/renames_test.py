from renamed.foo import FooTest
from renamed.foo.bar import BarTest


def test_imports_are_renamed():
    assert FooTest.__nirum_field_types__['a'] is BarTest
