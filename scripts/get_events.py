from datetime import datetime
from dateutil import tz
import pandas as pd
from googleapiclient.discovery import build
from google.oauth2 import service_account

# If modifying these scopes, delete the file token.json.
SCOPES = ['https://www.googleapis.com/auth/calendar.readonly',
          'https://www.googleapis.com/auth/calendar.events.readonly']
SERVICE_ACCOUNT_FILE = '../extended-line-362920-d1d2ea37dd97.json'
credentials = service_account.Credentials.from_service_account_file(
    SERVICE_ACCOUNT_FILE, 
    scopes = SCOPES)

delegated_credentials = credentials.with_subject('jake@rleanalytics.com')

service = build('calendar', 'v3', credentials = delegated_credentials)

# Get the list of calendars available
cal_list = service.calendarList().list().execute()
calendars = pd.json_normalize(cal_list.get('items', []))['id']

# Get today's events
today = datetime.utcnow().date()
start = datetime(today.year, today.month, today.day, 
                 tzinfo = tz.gettz('America/New_York')).isoformat()
end = datetime(today.year, today.month, today.day + 1, 
               tzinfo = tz.gettz('America/New_York')).isoformat()

all_events = pd.DataFrame()

for cal in calendars:
    events_result = service.events().list(calendarId = cal, 
                                          timeMin = start,
                                          timeMax = end,
                                          maxResults = 25, 
                                          singleEvents = True,
                                          orderBy = 'startTime').execute()
    events = pd.json_normalize(events_result.get('items', []))
    
    if(len(events) > 0):
        attendees = events.attendees
    
        resStat = []
        
        for event in range(len(attendees)):
            for att in range(len(attendees[event])):
                if attendees[event][att]['email'] == cal:
                    resStat.append(attendees[event][att]['responseStatus'])
                    break
        
        events = events[['summary', 'start.dateTime', 'end.dateTime']]
        events['response'] = resStat
        events['calendar'] = cal
        all_events = pd.concat([all_events, events], ignore_index = True)

all_events.to_csv('../data/calendar_events.csv', index = False)
