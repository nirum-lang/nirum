import pkg_resources


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
        'renamed', 'renamed.foo', 'renamed.foo.bar',
    }
    assert ['0.3.0'] == pkg['Version']
