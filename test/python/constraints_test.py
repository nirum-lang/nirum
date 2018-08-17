# -*- coding: utf-8 -*-
from pytest import raises

from fixture.constraints import Month


def test_numeric_constraints():
    month = Month(1)
    assert month.value == 1

    with raises(ValueError):
        Month(0)

    with raises(ValueError):
        Month(13)
