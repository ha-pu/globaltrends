from apiclient.discovery import build

def query_gtrends(terms, start_date, end_date, geo, api_key):
    SERVER = 'https://trends.googleapis.com'
    API_VERSION = 'v1beta'
    DISCOVERY_URL_SUFFIX = '/$discovery/rest?version=' + API_VERSION
    DISCOVERY_URL = SERVER + DISCOVERY_URL_SUFFIX

    service = build('trends', 'v1beta', developerKey=api_key, discoveryServiceUrl=DISCOVERY_URL)

    response = service.getGraph(terms=terms, restrictions_startDate=start_date, restrictions_endDate=end_date, restrictions_geo = geo).execute()
    return response
