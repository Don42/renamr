import json
import logging
import requests

ENCODING = 'utf-8'

SINGLESEARCH_SHOWS_URL = 'http://api.tvmaze.com/singlesearch/shows'

logger = logging.getLogger('renamr')
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
        return self.season, self.episode


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

