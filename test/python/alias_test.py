from fixture.types import FloatUnbox as OriginFloat64
from fixture.foo import FloatUnbox as OriginFloat32
from fixture.alias import FloatUnbox as FloatUnboxRecord


def test_aliasing_import():
    record = FloatUnboxRecord(
        f64=OriginFloat64(3.14),
        f32=OriginFloat32(1.59),
    )
    assert record
    alias_mod = __import__('fixture').alias
    assert hasattr(alias_mod, 'Float32Unbox')
    assert alias_mod.Float32Unbox is OriginFloat32
    assert hasattr(alias_mod, 'Float64Unbox')
    assert alias_mod.Float64Unbox is OriginFloat64
