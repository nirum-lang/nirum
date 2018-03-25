import pkg_resources

from fixture.foo import FloatUnbox, Irum
from renamed.foo import FooTest


def parse_pkg_info(pkg_name):
    d = pkg_resources.get_distribution(pkg_name)
    r = {}
    for meta in d._get_metadata(d.PKG_INFO):
        k, v = meta.split(': ', 1)
        r.setdefault(k, [])
        r[k].append(v)
    return r


def test_setup_metadata():
    pkg = parse_pkg_info('nirum_fixture')
    assert ['nirum'] == pkg['Author']
    assert ['dev@nirum.org'] == pkg['Author-email']
    assert ['nirum'] == pkg['Requires']
    assert set(pkg['Provides']) == {
        'fixture', 'fixture.foo', 'fixture.foo.bar', 'fixture.qux',
        'fixture.reserved_keyword_enum', 'fixture.reserved_keyword_union',
        'fixture.types',
        'renamed', 'renamed.foo', 'renamed.foo.bar',
    }
    assert ['0.3.0'] == pkg['Version']
    assert ['Package description'] == pkg['Summary']
    assert ['MIT'] == pkg['License']


def test_module_entry_points():
    map_ = pkg_resources.get_entry_map('nirum_fixture', group='nirum.modules')
    assert frozenset(map_) == {
        'fixture.foo', 'fixture.foo.bar', 'fixture.qux',
        'fixture.reserved-keyword-enum', 'fixture.reserved-keyword-union',
        'fixture.types',
        'renames.test.foo', 'renames.test.foo.bar',
    }
    import fixture.foo
    assert map_['fixture.foo'].resolve() is fixture.foo
    import fixture.foo.bar
    assert map_['fixture.foo.bar'].resolve() is fixture.foo.bar
    import renamed.foo
    assert map_['renames.test.foo'].resolve() is renamed.foo


def test_class_entry_points():
    map_ = pkg_resources.get_entry_map('nirum_fixture', group='nirum.classes')
    assert map_['fixture.foo.float-unbox'].resolve() is FloatUnbox
    assert map_['fixture.foo.irum'].resolve() is Irum
    assert map_['renames.test.foo.foo-test'].resolve() is FooTest
