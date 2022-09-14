# Libraries

import requests
import json
import pandas as pd
import sys
from datetime import datetime, timedelta

# pd.set_option('display.max_columns', None)

# Get UN and PW from file
whoop_config = pd.read_csv("whoop.config")

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
    

cycles = pd.DataFrame()
sleeps = pd.DataFrame()
recovery = pd.DataFrame()
workouts = pd.DataFrame()

for record in range(len(records)):
    
    # cycles
    tmp = pd.json_normalize(records[record]["cycle"])
    if tmp.shape[0] > 0:
        cycles = pd.concat([cycles, tmp], ignore_index = True)
        
    # sleeps
    tmp = pd.json_normalize(records[record]["sleeps"])
    if tmp.shape[0] > 0:
        sleeps = pd.concat([sleeps, tmp], ignore_index = True)
        
    # recovery
    tmp = pd.json_normalize(records[record]["recovery"])
    if tmp.shape[0] > 0:
        recovery = pd.concat([recovery, tmp], ignore_index = True)
        
    # workouts
    tmp = pd.json_normalize(records[record]["workouts"])
    if tmp.shape[0] > 0:
        workouts = pd.concat([workouts, tmp], ignore_index = True)
    

response = s.get(f'{api_url}/activities-service/v1/sports')
sports = pd.DataFrame(response.json())

workouts = workouts.merge(sports[["id", "name"]], 
                          left_on = "sport_id", 
                          right_on = "id",
                          how = "left")

response = s.get(
    url = f'{api_url}/users/{user_id}/metrics/heart_rate',
    params = {
        "start": "2022-09-06T15:00:00.000Z",
        "end": "2022-09-14T15:00:00.000Z",
        "step": "6", # every 6 seconds, 6 or 60 or 600
    }
)
hr = pd.DataFrame.from_dict(response.json()['values'])
hr['time'] = pd.to_datetime(hr['time'], unit = 'ms', utc = True)
hr = hr.rename(columns={'data': 'bpm'})
hr = hr.set_index('time')
