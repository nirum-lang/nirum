from fixture.foo import (FloatUnbox, Gender, MixedName, Music, NullService,
                         PingService, Point1, Point2, Point3d, Pop, Rnb, Way)


def test_enum_docs():
    assert Gender.__doc__.strip() == r'Enum docs\.'


def test_record_docs():
    assert Point1.__doc__.strip() == r'''Record docs\.

    .. attribute:: left

       Record field docs\.


    .. attribute:: top'''
    assert Point2.__doc__.strip() == r'''.. attribute:: left

       Record field docs\.


    .. attribute:: top'''
    assert Point3d.__doc__.strip() == r'''.. attribute:: xy


    .. attribute:: z'''


def test_unboxed_type_docs():
    assert FloatUnbox.__doc__.strip() == r'Unboxed type docs\.'
    assert Way.__doc__ is None


def test_union_docs():
    assert Music.__doc__.strip() == r'Union docs\.'
    assert Pop.__doc__.strip() == r'''Tag docs\.

    .. attribute:: country

       Tag field docs\.'''
    assert Rnb.__doc__.strip() == r'.. attribute:: country'
    assert MixedName.__doc__ is None


def test_service_docs():
    assert PingService.__doc__.strip() == r'Service docs\.'
    assert PingService.ping.__doc__.strip() == r'''Method docs\.

        :param nonce: Parameter docs\.'''
    assert NullService.__doc__ is None
