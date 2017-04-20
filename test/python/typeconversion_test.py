from nirum.datastructures import List

from fixture.foo import Album, Name, People, Person, Song


def test_sequence_to_tuple():
    album = Album(name='25', tracks=[
        Song(name=u'Hello')
    ])
    assert isinstance(album.tracks, List)
    assert hash(album)


def test_set_to_frozenset():
    people = People(people={
        Person(first_name=Name(u'hyojun'), last_name=Name(u'kang')),
        Person(first_name=Name(u'minhee'), last_name=Name(u'hong'))
    })
    assert isinstance(people.people, frozenset)
    assert hash(people)
