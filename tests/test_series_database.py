import pytest

import renamr.series_database


def test_get_episode_name():
    data = {(5, 16): "Felina"}
    ret = renamr.series_database.get_episode_name(
        renamr.series_database.EpisodeIdentifier(5, 16),
        data)
    assert "Felina" == ret


def test_build_identifier():
    assert "S01E01" == renamr.series_database.EpisodeIdentifier(1, 1).__str__()
    assert "S10E20" == renamr.series_database.EpisodeIdentifier(10, 20).__str__()
    assert "S11E999" == renamr.series_database.EpisodeIdentifier(11, 999).__str__()
    assert "S00E00" == renamr.series_database.EpisodeIdentifier(0, 0).__str__()


def test_build_identifier_errors():
    with pytest.raises(TypeError):
        renamr.series_database.EpisodeIdentifier(None).__str__()
    with pytest.raises(Exception):
        renamr.series_database.EpisodeIdentifier(15).__str__()
    with pytest.raises(ValueError):
        renamr.series_database.EpisodeIdentifier(-1, 5).__str__()
    with pytest.raises(ValueError):
        renamr.series_database.EpisodeIdentifier(1, -5).__str__()

