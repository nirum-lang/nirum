# -*- coding: utf-8 -*-
import enum
import uuid

from pytest import raises
from nirum.service import Service
from six import PY3

from fixture.foo import (Album, CultureAgnosticName, Dog,
                         EastAsianName, EvaChar,
                         FloatUnbox, Gender, ImportedTypeUnbox, Irum,
                         Line, MixedName, Music,
                         NameShadowingFieldRecord, NullService,
                         Person, People,
                         Point1, Point2, Point3d, Pop, PingService, Product,
                         RecordWithMap, RecordWithOptionalRecordField,
                         Rnb, RpcError, Run, Song, Status, Stop, Way,
                         WesternName)
from fixture.foo.bar import PathUnbox, IntUnbox, Point
from fixture.qux import Path, Name
from fixture.reserved_keyword_enum import ReservedKeywordEnum
from fixture.reserved_keyword_union import Mro, NoMro, ReservedKeywordUnion
from fixture.types import UuidList


def test_float_unbox():
    float_unbox = FloatUnbox(3.14)
    float1 = FloatUnbox(1.0)
    assert isinstance(FloatUnbox, type)
    assert float_unbox.value == 3.14
    assert float_unbox == FloatUnbox(3.14)
    assert float_unbox != float1
    assert {float_unbox, float_unbox, float1} == {float_unbox, float1}
    assert float_unbox.__nirum_serialize__() == 3.14
    assert FloatUnbox.__nirum_deserialize__(3.14) == float_unbox
    assert hash(float_unbox) == hash(FloatUnbox(3.14))
    assert hash(float_unbox) != hash(float1)
    with raises(ValueError):
        FloatUnbox.__nirum_deserialize__('a')
    with raises(TypeError):
        FloatUnbox('a')


def test_import_type_unbox():
    path = u'/path/string'
    path_unbox = PathUnbox(path)
    imported_unbox = ImportedTypeUnbox(path_unbox)
    assert isinstance(ImportedTypeUnbox, type)
    assert imported_unbox.value.value == u'/path/string'
    assert imported_unbox == ImportedTypeUnbox(PathUnbox(u'/path/string'))
    other_path = ImportedTypeUnbox(PathUnbox(u'/other/path'))
    assert imported_unbox != other_path
    expected = {imported_unbox, other_path}
    assert {imported_unbox, other_path, imported_unbox} == expected
    assert imported_unbox.__nirum_serialize__() == path
    assert ImportedTypeUnbox.__nirum_deserialize__(path) == imported_unbox
    with raises(ValueError):
        ImportedTypeUnbox.__nirum_deserialize__(123)
    with raises(TypeError):
        ImportedTypeUnbox(123)


def test_boxed_alias():
    w = Way(u'.')
    assert w.value == u'.'
    assert Way(Path(u'.')).value == u'.'
    assert Way.__nirum_deserialize__(u'.') == Way(u'.')
    assert Way(u'.').__nirum_serialize__() == u'.'
    assert Name(u'khj') == Irum(u'khj')
    assert Irum.__nirum_deserialize__(u'khj') == Irum(u'khj')
    assert Irum(u'khj').__nirum_serialize__() == u'khj'
    assert Irum.__nirum_deserialize__(u'khj') == Name(u'khj')
    assert Irum.__nirum_deserialize__(u'khj') == Irum(u'khj')


def test_unboxed_list():
    assert list(UuidList([]).value) == []
    uuids = [uuid.uuid1() for _ in range(3)]
    assert list(UuidList(uuids).value) == uuids
    with raises(TypeError) as ei:
        UuidList(['not uuid'])
    assert (str(ei.value) ==
            "expected typing.Sequence[uuid.UUID], not ['not uuid']")
    with raises(TypeError) as ei:
        UuidList(uuids + ['not uuid'])
    assert str(ei.value) == \
        "expected typing.Sequence[uuid.UUID], not %r" % (uuids + ['not uuid'])


def test_enum():
    assert type(Gender) is enum.EnumMeta
    assert set(Gender) == {Gender.male, Gender.female}
    assert Gender.male.value == u'male'
    assert Gender.female.value == u'yeoseong'
    assert Gender.__nirum_deserialize__(u'male') == Gender.male
    assert Gender.__nirum_deserialize__(u'yeoseong') == Gender.female
    with raises(ValueError):
        Gender.__nirum_deserialize__(u'namja')
    assert Gender.male.__nirum_serialize__() == u'male'
    assert Gender.female.__nirum_serialize__() == u'yeoseong'
    assert type(EvaChar) is enum.EnumMeta
    assert set(EvaChar) == {EvaChar.soryu_asuka_langley,
                            EvaChar.ayanami_rei, EvaChar.ikari_shinji,
                            EvaChar.katsuragi_misato, EvaChar.nagisa_kaworu}
    soryu_asuka_langley = EvaChar.soryu_asuka_langley
    asuka = u'soryu_asuka_langley'
    assert soryu_asuka_langley.value == asuka
    assert soryu_asuka_langley.__nirum_serialize__() == asuka
    assert EvaChar.__nirum_deserialize__(asuka) == soryu_asuka_langley
    assert EvaChar.__nirum_deserialize__(asuka) == EvaChar.soryu_asuka_langley


def test_record():
    assert isinstance(Point1, type)
    point = Point1(left=3, top=14)
    assert point.left == 3
    assert point.top == 14
    assert point == Point1(left=3, top=14)
    assert point != Point1(left=3, top=15)
    assert point != Point1(left=4, top=14)
    assert point != Point1(left=4, top=15)
    assert point != 'foo'
    assert hash(point) == hash(Point1(left=3, top=14))
    assert hash(point) != hash(Point1(left=0, top=14))
    assert hash(point) != hash(Point1(left=3, top=0))
    point_serialized = {'_type': 'point1', 'x': 3, 'top': 14}
    assert point.__nirum_serialize__() == point_serialized
    assert Point1.__nirum_deserialize__(point_serialized) == point
    assert Point1.__nirum_deserialize__({'x': 3, 'top': 14}) == point
    with raises(ValueError):
        Point1.__nirum_deserialize__({'_type': 'foo'})
    with raises(ValueError) as e:
        Point1.__nirum_deserialize__({'_type': 'point1',
                                      'x': 'a',
                                      'top': 'b'})
    assert str(e.value) == '''\
.top: Expected a string of decimal digits, but the string consists of other \
than decimal digits.
.x: Expected a string of decimal digits, but the string consists of other \
than decimal digits.'''
    with raises(TypeError):
        Point1(left=1, top='a')
    with raises(TypeError):
        Point1(left='a', top=1)
    with raises(TypeError):
        Point1(left='a', top='b')
    assert repr(point) == 'fixture.foo.Point1(left=3, top=14)'
    assert isinstance(Point2, type)
    int_three = IntUnbox(3)
    int_four_teen = IntUnbox(14)
    point2 = Point2(left=int_three, top=int_four_teen)
    assert point2.left == int_three
    assert point2.top == int_four_teen
    assert point2 == Point2(left=IntUnbox(3), top=IntUnbox(14))
    assert point2 != Point2(left=IntUnbox(3), top=IntUnbox(15))
    assert point2 != Point2(left=IntUnbox(4), top=IntUnbox(14))
    assert point2 != Point2(left=IntUnbox(4), top=IntUnbox(15))
    assert point2 != 'foo'
    assert hash(point2) == hash(Point2(left=IntUnbox(3), top=IntUnbox(14)))
    assert hash(point2) != hash(Point2(left=IntUnbox(0), top=IntUnbox(14)))
    assert hash(point2) != hash(Point2(left=IntUnbox(3), top=IntUnbox(0)))
    point2_serialize = {'_type': 'point2', 'left': 3, 'top': 14}
    assert point2.__nirum_serialize__() == point2_serialize
    assert Point2.__nirum_deserialize__(point2_serialize) == point2
    assert Point2.__nirum_deserialize__({'left': 3, 'top': 14}) == point2
    with raises(ValueError):  # no "left" and "top" fields
        Point2.__nirum_deserialize__({'_type': 'foo'})
    p = Point2.__nirum_deserialize__({
        # extra field does not matter; just ignored
        '_type': 'point2',
        'left': 3,
        'top': 14,
        'extra': 'it does not matter',
    })
    assert p == Point2(left=IntUnbox(3), top=IntUnbox(14))
    with raises(TypeError):
        Point2(left=IntUnbox(1), top='a')
    with raises(TypeError):
        Point2(left=IntUnbox(1), top=2)
    assert repr(point2) == (
        'fixture.foo.Point2(left=fixture.foo.bar.IntUnbox(3), '
        'top=fixture.foo.bar.IntUnbox(14))'
    )
    assert isinstance(Point3d, type)
    point3d = Point3d(xy=Point(x=1, y=2), z=3)
    assert point3d.xy == Point(x=1, y=2)
    assert point3d.xy.x == 1
    assert point3d.xy.y == 2
    assert point3d.z == 3
    point3d_serialize = {
        '_type': 'point3d',
        'xy': {
            '_type': 'point', 'x': 1, 'y': 2
        },
        'z': 3
    }
    assert point3d.__nirum_serialize__() == point3d_serialize
    assert Point3d.__nirum_deserialize__(point3d_serialize) == point3d
    assert (repr(point3d) ==
            'fixture.foo.Point3d(xy=fixture.foo.bar.Point(x=1, y=2), z=3)')

    # Optional fields can be empty
    r = RecordWithOptionalRecordField(f=None)
    assert r.__nirum_serialize__() == {
        '_type': 'record_with_optional_record_field',
        'f': None,
    }


def test_record_with_one_field():
    assert isinstance(Line, type)
    line = Line(length=10)
    assert line.length == 10
    assert Line.__slots__ == ('length', )
    expected = {'_type': 'line', 'length': 3}
    assert Line(length=3).__nirum_serialize__() == expected


def test_record_optional_initializer():
    product = Product(name=u'coffee', sale=False)
    assert product.name == u'coffee'
    assert product.price is None
    assert not product.sale
    assert product.url is None


def test_union():
    assert isinstance(MixedName, type)
    assert MixedName.Tag.western_name.value == 'western_name'
    assert MixedName.Tag.east_asian_name.value == 'east_asian_name'
    assert MixedName.Tag.culture_agnostic_name.value == 'culture_agnostic_name'
    assert WesternName is MixedName.WesternName  # alias
    assert isinstance(WesternName, type)
    assert issubclass(WesternName, MixedName)
    with raises(NotImplementedError):
        MixedName()
    western_name = WesternName(first_name=u'foo', middle_name=u'bar',
                               last_name=u'baz')
    assert western_name.first_name == u'foo'
    assert western_name.middle_name == u'bar'
    assert western_name.last_name == u'baz'
    with raises(TypeError):
        WesternName(first_name=1, middle_name=u'bar', last_name=u'baz')
    with raises(TypeError):
        WesternName(first_name=u'foo', middle_name=1, last_name=u'baz')
    with raises(TypeError):
        WesternName(first_name=u'foo', middle_name=u'bar', last_name=1)
    assert western_name == WesternName(first_name=u'foo', middle_name=u'bar',
                                       last_name=u'baz')
    assert western_name != WesternName(first_name=u'wrong',
                                       middle_name=u'bar', last_name=u'baz')
    assert western_name != WesternName(first_name=u'foo', middle_name=u'wrong',
                                       last_name=u'baz')
    assert western_name != WesternName(first_name=u'foo', middle_name=u'bar',
                                       last_name=u'wrong')
    assert western_name != WesternName(first_name=u'wrong',
                                       middle_name=u'wrong',
                                       last_name=u'wrong')
    assert hash(WesternName(first_name=u'foo', middle_name=u'bar',
                            last_name=u'baz')) == hash(western_name)
    assert hash(western_name) != hash(
        WesternName(first_name=u'', middle_name=u'bar', last_name=u'baz')
    )
    assert hash(western_name) != hash(
        WesternName(first_name=u'foo', middle_name=u'', last_name=u'baz')
    )
    assert hash(western_name) != hash(
        WesternName(first_name=u'foo', middle_name=u'bar', last_name=u'')
    )
    if PY3:
        assert repr(western_name) == (
            "fixture.foo.MixedName.WesternName(first_name='foo', "
            "middle_name='bar', last_name='baz')"
        )
    else:
        assert repr(western_name) == (
            "fixture.foo.MixedName.WesternName(first_name=u'foo', "
            "middle_name=u'bar', last_name=u'baz')"
        )
    assert EastAsianName is MixedName.EastAsianName
    assert isinstance(EastAsianName, type)
    assert issubclass(EastAsianName, MixedName)
    east_asian_name = EastAsianName(family_name=u'foo', given_name=u'baz')
    assert east_asian_name.family_name == u'foo'
    assert east_asian_name.given_name == u'baz'
    assert east_asian_name == EastAsianName(family_name=u'foo',
                                            given_name=u'baz')
    assert east_asian_name != EastAsianName(family_name=u'foo',
                                            given_name=u'wrong')
    assert east_asian_name != EastAsianName(family_name=u'wrong',
                                            given_name=u'baz')
    assert east_asian_name != EastAsianName(family_name=u'wrong',
                                            given_name=u'wrong')
    with raises(TypeError):
        EastAsianName(family_name=1, given_name=u'baz')
    with raises(TypeError):
        EastAsianName(family_name=u'foo', given_name=2)
    assert CultureAgnosticName is MixedName.CultureAgnosticName
    assert isinstance(CultureAgnosticName, type)
    assert issubclass(CultureAgnosticName, MixedName)
    agnostic_name = CultureAgnosticName(fullname=u'foobar')
    assert agnostic_name.fullname == u'foobar'
    assert agnostic_name == CultureAgnosticName(fullname=u'foobar')
    assert agnostic_name != CultureAgnosticName(fullname=u'wrong')
    with raises(TypeError):
        CultureAgnosticName(fullname=1)
    name = MixedName.__nirum_deserialize__({
        '_type': 'mixed_name',
        '_tag': 'east_asian_name',
        'family_name': u'foo',
        'given_name': u'bar',
    })
    assert isinstance(name, MixedName.EastAsianName)
    with raises(ValueError) as e:
        MixedName.__nirum_deserialize__({  # invalid field types
            '_type': 'mixed_name',
            '_tag': 'east_asian_name',
            'family_name': 404,  # not a text
            'given_name': 503,  # not a text
        })
    assert str(e.value) == '''\
.family_name: Expected a string.
.given_name: Expected a string.\
'''  # message can contain multiple errors
    n = MixedName.__nirum_deserialize__({  # invalid field types
        '_type': 'mixed_name',
        '_tag': 'east_asian_name',
        'family_name': u'John',
        'given_name': u'Doe',
        'extra': u'it does not matter',
    })
    assert n == EastAsianName(family_name=u'John', given_name=u'Doe')


def test_union_default_tag():
    n = CultureAgnosticName(fullname=u'foobar')
    serialized = n.__nirum_serialize__()
    print(serialized)
    del serialized['_tag']
    n2 = MixedName.__nirum_deserialize__(serialized)
    assert n2 == n


def test_union_with_special_case():
    kr_pop = Pop(country=u'KR')
    assert kr_pop.country == u'KR'
    assert kr_pop == Pop(country=u'KR')
    assert kr_pop != Pop(country=u'US')
    with raises(TypeError):
        assert Pop(country=1)
    assert Pop.__slots__ == ('country',)
    assert Rnb(country=u'KR').__nirum_tag__.value == 'rhythm_and_ballad'
    assert Run().__nirum_tag__.value == 'run'
    assert Stop().__nirum_tag__.value == 'stop'


def test_union_tags_optional_initializer():
    dog = Dog(name=u"Max", age=10)
    assert dog.name == u"Max"
    assert dog.kind is None
    assert dog.age == 10
    assert dog.weight is None


def test_service():
    assert issubclass(NullService, Service)
    assert issubclass(PingService, Service)
    if PY3:
        import typing
        annots = typing.get_type_hints(PingService.ping)
        assert set(annots) == {'nonce', 'return'}
        assert annots['nonce'] is str
        assert annots['return'] is bool
    with raises(NotImplementedError):
        PingService().ping(u'nonce')
    with raises(NotImplementedError):
        PingService().ping(nonce=u'nonce')
    with raises(TypeError):
        PingService().ping(wrongkwd=u'a')


def test_enum_reserved_keyword_for_member():
    assert hasattr(ReservedKeywordEnum, 'mro_')
    assert hasattr(ReservedKeywordEnum, 'no_mro')
    assert (ReservedKeywordEnum.__nirum_deserialize__(u'mro') ==
            ReservedKeywordEnum.mro_)
    assert (ReservedKeywordEnum.__nirum_deserialize__(u'no-mro') ==
            ReservedKeywordEnum.no_mro)
    assert ReservedKeywordEnum.mro_.__nirum_serialize__() == 'mro'
    assert ReservedKeywordEnum.no_mro.__nirum_serialize__() == 'no_mro'


def test_union_reserved_keyword_for_tag():
    assert ReservedKeywordUnion.Tag.mro_.value == 'mro'
    assert ReservedKeywordUnion.Tag.no_mro.value == 'no_mro'
    assert Mro.__nirum_tag__ is ReservedKeywordUnion.Tag.mro_
    assert NoMro.__nirum_tag__ is ReservedKeywordUnion.Tag.no_mro
    assert ReservedKeywordUnion.__nirum_tag_classes__ == {
        ReservedKeywordUnion.Tag.mro_: Mro,
        ReservedKeywordUnion.Tag.no_mro: NoMro,
    }


def test_nirum_type():
    assert FloatUnbox.__nirum_type__ == 'unboxed'
    assert Way.__nirum_type__ == 'unboxed'
    assert Gender.__nirum_type__ == 'enum'
    assert EvaChar.__nirum_type__ == 'enum'
    assert Point1.__nirum_type__ == 'record'
    assert Point2.__nirum_type__ == 'record'
    assert Point3d.__nirum_type__ == 'record'
    assert Line.__nirum_type__ == 'record'
    assert Product.__nirum_type__ == 'record'
    assert MixedName.__nirum_type__ == 'union'
    assert WesternName.__nirum_type__ == 'union'
    assert EastAsianName.__nirum_type__ == 'union'
    assert CultureAgnosticName.__nirum_type__ == 'union'
    assert Music.__nirum_type__ == 'union'
    assert Pop.__nirum_type__ == 'union'
    assert Rnb.__nirum_type__ == 'union'
    assert Status.__nirum_type__ == 'union'
    assert Run.__nirum_type__ == 'union'
    assert Stop.__nirum_type__ == 'union'
    assert RpcError.__nirum_type__ == 'union'
    assert NullService.__nirum_type__ == 'service'
    assert PingService.__nirum_type__ == 'service'


def test_nirum_tag_classes():
    assert MixedName.__nirum_tag_classes__ == {
        MixedName.Tag.western_name: WesternName,
        MixedName.Tag.east_asian_name: EastAsianName,
        MixedName.Tag.culture_agnostic_name: CultureAgnosticName,
    }
    assert Music.__nirum_tag_classes__ == {
        Music.Tag.pop: Pop,
        Music.Tag.rnb: Rnb,
    }
    assert Status.__nirum_tag_classes__ == {
        Status.Tag.run: Run,
        Status.Tag.stop: Stop,
    }


def test_list_serializer():
    album = Album(name=u'Album title', tracks=[Song(name=u'Song title')])
    assert album.__nirum_serialize__() == {
        '_type': 'album',
        'name': u'Album title',
        'tracks': [
            {'_type': 'song', 'name': u'Song title'},
        ],
    }


def test_set_serializer():
    people = People(people={
        Person(first_name=Name(u'First'), last_name=Name(u'Last')),
    })
    assert people.__nirum_serialize__() == {
        '_type': 'people',
        'people': [
            {
                '_type': 'person',
                'first_name': u'First',
                'last_name': u'Last',
            },
        ]
    }


def test_map_serializer():
    record = RecordWithMap(text_to_text={u'key': u'value'})
    assert record.__nirum_serialize__() == {
        '_type': 'record_with_map',
        'text_to_text': [
            {
                'key': u'key',
                'value': u'value',
            },
        ],
    }


def test_name_shadowing_field():
    NameShadowingFieldRecord(
        typing={u'a': [u'b', u'c']},
        collections={u'a': {u'b', u'c'}},
        numbers=1234,
        uuid=uuid.uuid4(),
        bytes=b'binary',
    )
    with raises(TypeError) as ei:
        NameShadowingFieldRecord(
            typing={u'invalid'},
            collections={u'a': {u'b', u'c'}},
            numbers=1234,
            uuid=uuid.uuid4(),
            bytes=b'binary',
        )
    assert str(ei.value).startswith('typing must be a value of typing.Mapping')
    with raises(TypeError) as ei:
        NameShadowingFieldRecord(
            typing={u'a': [u'b', u'c']},
            collections={u'invalid'},
            numbers=1234,
            uuid=uuid.uuid4(),
            bytes=b'binary',
        )
    assert str(ei.value).startswith(
        'collections must be a value of typing.Mapping['
    )
    with raises(TypeError):
        NameShadowingFieldRecord(
            typing={u'a': [u'b', u'c']},
            collections={u'a': {u'b', u'c'}},
            numbers='invalid',
            uuid=uuid.uuid4(),
            bytes=b'binary',
        )
    with raises(TypeError) as ei:
        NameShadowingFieldRecord(
            typing={u'a': [u'b', u'c']},
            collections={u'a': {u'b', u'c'}},
            numbers=1234,
            uuid='invalid',
            bytes=b'binary',
        )
    assert str(ei.value) == "uuid must be a value of uuid.UUID, not 'invalid'"
    with raises(TypeError) as ei:
        NameShadowingFieldRecord(
            typing={u'a': [u'b', u'c']},
            collections={u'a': {u'b', u'c'}},
            numbers=1234,
            uuid=uuid.uuid4(),
            bytes=['invalid'],
        )
    assert "bytes must be a value of {0}, not ['invalid']".format(
        'bytes' if PY3 else 'str'
    ) == str(ei.value)
