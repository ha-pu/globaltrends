from apiclient.discovery import build

def start_service(api_key):
    SERVER = 'https://trends.googleapis.com'
    API_VERSION = 'v1beta'
    DISCOVERY_URL_SUFFIX = '/$discovery/rest?version=' + API_VERSION
    DISCOVERY_URL = SERVER + DISCOVERY_URL_SUFFIX
    service = build('trends', 'v1beta', developerKey=api_key, discoveryServiceUrl=DISCOVERY_URL)
    return service

def querty_trend(terms, start_date, end_date, geo, api_key):
    service = start_service(api_key)
    
    if (geo is None):
        response = service.getGraph(terms=terms, restrictions_startDate=start_date, restrictions_endDate=end_date).execute()
    else:
        response = service.getGraph(terms=terms, restrictions_startDate=start_date, restrictions_endDate=end_date, restrictions_geo = geo).execute()
    
    return response

def query_region(terms, start_date, end_date, geo, api_key):
    service = start_service(api_key)
    
    if (geo is None):
        response = service.regions().list(term=terms, restrictions_startDate=start_date, restrictions_endDate=end_date).execute()
    else:
        response = service.regions().list(term=terms, restrictions_startDate=start_date, restrictions_endDate=end_date, restrictions_geo = geo).execute()

    return response

def query_terms(terms, start_date, end_date, geo, api_key, topic=False, rising=False):
    service = start_service(api_key)
    
    if (geo is None):
        if (topic is True):
            if (rising is True):
                response = service.getRisingTopics(term=terms, restrictions_startDate=start_date, restrictions_endDate=end_date).execute()
            else:
                response = service.getTopTopics(term=terms, restrictions_startDate=start_date, restrictions_endDate=end_date).execute()
        else:
            if (rising is True):
                response = service.getRisingQueries(term=terms, restrictions_startDate=start_date, restrictions_endDate=end_date).execute()
            else:
                response = service.getTopQueries(term=terms, restrictions_startDate=start_date, restrictions_endDate=end_date).execute()
    else:
        if (topic is True):
            if (rising is True):
                response = service.getRisingTopics(term=terms, restrictions_startDate=start_date, restrictions_endDate=end_date, restrictions_geo = geo).execute()
            else:
                response = service.getTopTopics(term=terms, restrictions_startDate=start_date, restrictions_endDate=end_date, restrictions_geo = geo).execute()
        else:
            if (rising is True):
                response = service.getRisingQueries(term=terms, restrictions_startDate=start_date, restrictions_endDate=end_date, restrictions_geo = geo).execute()
            else:
                response = service.getTopQueries(term=terms, restrictions_startDate=start_date, restrictions_endDate=end_date, restrictions_geo = geo).execute()
    
    return response
