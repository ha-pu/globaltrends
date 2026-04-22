"""Google Trends Research API helpers.

Thin wrappers around the undocumented v1beta REST API, intended to be sourced
into an R session via reticulate.  Each public function authenticates with
``api_key``, calls one API endpoint, and returns the raw parsed JSON response.

Date strings must use ``"YYYY-MM"`` format (monthly granularity).
Geographic scope is controlled by an ISO 3166-1 alpha-2 country code (e.g.
``"US"``, ``"NO"``) or a region sub-code (e.g. ``"US-CA"``); pass ``None``
for worldwide results.
"""

from apiclient.discovery import build

_SERVER = 'https://trends.googleapis.com'
_API_VERSION = 'v1beta'
_DISCOVERY_URL = f'{_SERVER}/$discovery/rest?version={_API_VERSION}'


def start_service(api_key):
    """Build an authenticated Google Trends API service object.

    Args:
        api_key: Google Cloud API key with the Trends API enabled.

    Returns:
        A ``googleapiclient`` Resource object for the Trends v1beta service.
    """
    return build('trends', _API_VERSION, developerKey=api_key, discoveryServiceUrl=_DISCOVERY_URL)


def _geo_kwargs(geo):
    """Return a ``restrictions_geo`` kwarg dict when a geo filter is given."""
    return {'restrictions_geo': geo} if geo is not None else {}


def query_trend(terms, start_date, end_date, geo, api_key):
    """Fetch a time-series interest graph for one or more search terms.

    Calls the ``getGraph`` endpoint and returns monthly interest values for
    each term over the requested period.

    Args:
        terms: Search term or list of terms (max 5).
        start_date: First month to include, formatted as ``"YYYY-MM"``.
        end_date: Last month to include, formatted as ``"YYYY-MM"``.
        geo: ISO 3166 country or region code, or ``None`` for worldwide.
        api_key: Google Cloud API key.

    Returns:
        Parsed JSON response dict.  ``response["lines"]`` is a list of dicts,
        each with ``"term"`` (str) and ``"points"`` (list of
        ``{"date": "YYYY-MM", "value": float}``).
    """
    service = start_service(api_key)
    return service.getGraph(
        terms=terms,
        restrictions_startDate=start_date,
        restrictions_endDate=end_date,
        **_geo_kwargs(geo)
    ).execute()


def query_region(terms, start_date, end_date, geo, api_key):
    """Fetch regional interest breakdown for a search term.

    Calls the ``regions.list`` endpoint and returns per-region interest values
    for the requested period.  When ``geo`` is ``None`` the breakdown covers
    all countries; when a country code is given it covers sub-national regions.

    Args:
        terms: Single search term string.
        start_date: First month to include, formatted as ``"YYYY-MM"``.
        end_date: Last month to include, formatted as ``"YYYY-MM"``.
        geo: ISO 3166 country code to get sub-national breakdown, or ``None``
            for a worldwide country-level breakdown.
        api_key: Google Cloud API key.

    Returns:
        Parsed JSON response dict.  ``response["regions"]`` is a list of dicts,
        each with ``"regionCode"`` (str), ``"regionName"`` (str), and
        ``"value"`` (float).
    """
    service = start_service(api_key)
    return service.regions().list(
        term=terms,
        restrictions_startDate=start_date,
        restrictions_endDate=end_date,
        **_geo_kwargs(geo)
    ).execute()


def query_terms(terms, start_date, end_date, geo, api_key, topic=False, rising=False):
    """Fetch related queries or topics for a search term.

    Selects one of four API endpoints based on the ``topic`` and ``rising``
    flags:

    +---------+---------+----------------------------+
    | topic   | rising  | Endpoint called            |
    +=========+=========+============================+
    | False   | False   | ``getTopQueries``          |
    +---------+---------+----------------------------+
    | False   | True    | ``getRisingQueries``       |
    +---------+---------+----------------------------+
    | True    | False   | ``getTopTopics``           |
    +---------+---------+----------------------------+
    | True    | True    | ``getRisingTopics``        |
    +---------+---------+----------------------------+

    Args:
        terms: Single search term string.
        start_date: First month to include, formatted as ``"YYYY-MM"``.
        end_date: Last month to include, formatted as ``"YYYY-MM"``.
        geo: ISO 3166 country or region code, or ``None`` for worldwide.
        api_key: Google Cloud API key.
        topic: If ``True``, return topic entities instead of query strings.
        rising: If ``True``, return breakout/rising items instead of top items.

    Returns:
        Parsed JSON response dict.  ``response["item"]`` is a list of dicts,
        each with ``"title"`` (str) and ``"value"`` (float).
    """
    service = start_service(api_key)
    method = {
        (True,  True):  service.getRisingTopics,
        (True,  False): service.getTopTopics,
        (False, True):  service.getRisingQueries,
        (False, False): service.getTopQueries,
    }[(topic, rising)]
    return method(
        term=terms,
        restrictions_startDate=start_date,
        restrictions_endDate=end_date,
        **_geo_kwargs(geo)
    ).execute()
