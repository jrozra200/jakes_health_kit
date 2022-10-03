# Libraries

import requests
import json
import pandas as pd
import sys
from datetime import datetime, timedelta

# pd.set_option('display.max_columns', None)

# Get UN and PW from file
whoop_config = pd.read_csv("../whoop.config")

# API URL for Reference
api_url = "https://api-7.whoop.com"

# Get the Refresh Token
refresh = requests.post(
    url = f"{api_url}/oauth/token",
    headers = {
        "Content-Type": "application/json; charset=utf-8",
    },
    data = json.dumps({
        "username": whoop_config["username"][0],
        "password": whoop_config["password"][0],
        "issueRefresh": True,
        "grant_type": "password"
    })
)

refresh_token = refresh.json()['refresh_token']

# Get the access token
response = requests.post(
    url = f"{api_url}/oauth/token",
    headers = {
        "Content-Type": "application/json; charset=utf-8",
    },
    data=json.dumps({
        "grant_type": "refresh_token",
        "refresh_token": refresh_token
    })
)

# Access token, user id, and start of this jawn
access_token = response.json()['access_token']
user_id = response.json()['user']['id']
earliest = response.json()['user']['createdAt']

latest = (datetime.today() + timedelta(days = 1)).strftime('%Y-%m-%dT00:00:00.000Z')

# create a session
s = requests.Session()
s.headers.update({
    'Content-Type': 'application/json; charset=utf-8',
    'Authorization': f'Bearer {access_token}',
})


offset = 0
total_count = sys.maxsize
records = []
while offset < total_count:
    response = s.get(
        url=f'{api_url}/activities-service/v1/cycles/aggregate/range/{user_id}',
        params={
            'startTime': earliest,
            'endTime': latest,
            'limit': 50,
            'offset': offset
        })
    if response.status_code != 200:
        break

    records.extend(response.json()['records'])
    offset = response.json()['offset']
    total_count = response.json()['total_count']

cycles = pd.DataFrame()
sleeps = pd.DataFrame()
recovery = pd.DataFrame()
workouts = pd.DataFrame()

for record in range(len(records)):
    
    # cycles
    if len(records[record]["cycle"]) > 0:    
        tmp = pd.json_normalize(records[record]["cycle"])
        cycles = pd.concat([cycles, tmp], ignore_index = True)
        
    # sleeps
    if len(records[record]["sleeps"]) > 0:
        tmp = pd.json_normalize(records[record]["sleeps"])
        sleeps = pd.concat([sleeps, tmp], ignore_index = True)
        
    # recovery
    if len(records[record]["sleeps"]) > 0:
        tmp = pd.json_normalize(records[record]["recovery"])
        recovery = pd.concat([recovery, tmp], ignore_index = True)
        
    # workouts
    if len(records[record]["workouts"]) > 0:
        tmp = pd.json_normalize(records[record]["workouts"])
        workouts = pd.concat([workouts, tmp], ignore_index = True)
    

response = s.get(f'{api_url}/activities-service/v1/sports')
sports = pd.DataFrame(response.json())

workouts = workouts.merge(sports[["id", "name"]], 
                          left_on = "sport_id", 
                          right_on = "id",
                          how = "left")


cycles.to_csv('../data/cycles.csv', index = False)
sleeps.to_csv('../data/sleeps.csv', index = False)
recovery.to_csv('../data/recovery.csv', index = False)
workouts.to_csv('../data/workouts.csv', index = False)
sports.to_csv('../data/sports.csv', index = False)

# response = s.get(
#     url = f'{api_url}/users/{user_id}/metrics/heart_rate',
#     params = {
#         "start": "2022-09-06T15:00:00.000Z",
#         "end": "2022-09-14T15:00:00.000Z",
#         "step": "6", # every 6 seconds, 6 or 60 or 600
#     }
# )
# hr = pd.DataFrame.from_dict(response.json()['values'])
# hr['time'] = pd.to_datetime(hr['time'], unit = 'ms', utc = True)
# hr = hr.rename(columns={'data': 'bpm'})
# hr = hr.set_index('time')
