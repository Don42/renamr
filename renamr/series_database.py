import json
import logging
import pathlib

import requests

logger = logging.getLogger(__name__)

ENCODING = 'utf-8'
SINGLESEARCH_SHOWS_URL = 'http://api.tvmaze.com/singlesearch/shows'

cache = {}


class EpisodeIdentifier:
    def __init__(self, season, episode):
        if season < 0 or episode < 0:
            raise ValueError("Season and Episode can't be negative")
        self.season = season
        self.episode = episode

    def __repr__(self):
        return "EpisodeIdentifier({season:>02}, {episode:>02})".format(season=self.season, episode=self.episode)

    def __str__(self):
        return "S{season:>02}E{episode:>02}".format(season=self.season, episode=self.episode)

    def identifier(self):
        return "S{season:>02}E{episode:>02}".format(season=self.season, episode=self.episode)


class SeriesDatabase:
    """This class implements a cached interface to the api of tvmaze.com

    """
    def __init__(self, cache_file_name=None):
        self._cache = {}
        self.cache_file_name = cache_file_name
        if self.cache_file_name is not None:
            self.load_series_data(self.cache_file_name)

    def __del__(self):
        if self.cache_file_name is not None:
            self.store_series_data(self.cache_file_name)

    def get_episode_name(self, series_name, ident):
        """

        :param series_name: Name of the series
        :param ident: Identifier for the episode
        :return: episode name
        """
        series_data = self._get_series_data(series_name)
        try:
            return series_data[ident.identifier()]
        except KeyError:
            pass

        series_data = self._get_series_data(series_name, refresh=True)
        try:
            return series_data[ident.identifier()]
        except KeyError:
            return ""

    def store_series_data(self, filename):
        file = pathlib.Path(filename)
        with file.open('w') as f:
            json.dump(self._cache, f)

    def load_series_data(self, filename):
        file = pathlib.Path(filename)
        if file.exists():
            with file.open('r') as f:
                self._cache = json.load(f)

    def _get_series_data(self, series_name, refresh=False):
        """

        :param series_name: Name of the series
        :param refresh: Refresh the cache for series
        :return: mapping for the series
        """
        short_name = series_name.replace(' ', '-')
        if short_name not in self._cache or refresh:
            page = self._download_series_page(short_name)
            self._cache[short_name] = self._extract_episode_name_mapping(page)
        return self._cache[short_name]

    @staticmethod
    def _download_series_page(short_name: str) -> str:
        """

        :param short_name: Short name of a series
        :return: Downloaded page
        """
        session = requests.Session()
        payload = {'q': short_name, 'embed': 'episodes'}
        response = session.get(SINGLESEARCH_SHOWS_URL, params=payload)
        if response.status_code == 200:
            response.encoding = ENCODING
            return response.text
        elif response.status_code == 404:
            raise NameError  # TODO Create proper exception
        else:
            logger.error("The server couldn't fulfill the request. Error code: {code}".format(response.status_code))
            raise requests.RequestException

    @staticmethod
    def _extract_episode_name_mapping(series_page):
        """

        :param series_page: Page to parse
        :return: mapping from season/episode to name
        """
        content = json.loads(series_page)
        episodes = content['_embedded']['episodes']
        ident_name_mapping = dict()
        for episode in episodes:
            identifier = EpisodeIdentifier(int(episode['season']), int(episode['number'])).identifier()
            ident_name_mapping[identifier] = episode['name']
        return ident_name_mapping


def download_series_page(short_name: str) -> str:
    """

    :param short_name: Short name of a series
    :return: Downloaded page
    """
    session = requests.Session()
    payload = {'q': short_name, 'embed': 'episodes'}
    response = session.get(SINGLESEARCH_SHOWS_URL, params=payload)
    if response.status_code == 200:
        response.encoding = ENCODING
        return response.text
    elif response.status_code == 404:
        raise NameError  # TODO Create proper exception
    else:
        logger.error("The server couldn't fulfill the request. Error code: {code}".format(response.status_code))
        raise requests.RequestException


def extract_episode_name_mapping(series_page):
    """

    :param series_page: Page to parse
    :return: mapping from season/episode to name
    """
    content = json.loads(series_page)
    episodes = content['_embedded']['episodes']
    ident_name_mapping = dict()
    for episode in episodes:
        ident_name_mapping[(int(episode['season']), int(episode['number']))] = episode['name']
    return ident_name_mapping


def get_series_data(series_name):
    """

    :param series_name: Name of the series
    :return: mapping for the series
    """
    short_name = series_name.replace(' ', '-')
    if short_name not in cache:
        page = download_series_page(short_name)
        cache[short_name] = extract_episode_name_mapping(page)
    return cache[short_name]


def get_episode_name(ident, series_data):
    """

    :param ident: Identifier for the episode
    :param series_data: mapping for the series
    :return: episode name
    """
    try:
        return series_data[ident.identifier()]
    except IndexError:
        return ""

