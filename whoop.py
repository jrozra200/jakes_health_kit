import requests
import json
import pandas as pd
import sys

pd.set_option('display.max_columns', None)

whoop_config = pd.read_csv("whoop.config")

refresh = requests.post(
    url = "https://api-7.whoop.com/oauth/token",
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

response = requests.post(
    url = "https://api-7.whoop.com/oauth/token",
    headers = {
        "Content-Type": "application/json; charset=utf-8",
    },
    data=json.dumps({
        "grant_type": "refresh_token",
        "refresh_token": refresh_token
    })
)

access_token = response.json()['access_token']

s = requests.Session()
s.headers.update({
    'Content-Type': 'application/json; charset=utf-8',
    'Authorization': f'Bearer {access_token}',
})

api_url = "https://api-7.whoop.com"
user_id = "1508418"

response = s.get(f'{api_url}/users/{user_id}')
response.json()


offset = 0
total_count = sys.maxsize
records = []
while offset < total_count:
    response = s.get(
        url=f'{api_url}/activities-service/v1/cycles/aggregate/range/{user_id}',
        params={
            'startTime': '2021-08-11T00:00:00.000Z',
            'endTime': '2022-09-15T00:00:00.000Z',
            'limit': 50,
            'offset': offset
        })
    if response.status_code != 200:
        break

    records.extend(response.json()['records'])
    offset = response.json()['offset']
    total_count = response.json()['total_count']
    print(f'got {offset} of {total_count} items', end='\r')
    

cycles = pd.DataFrame()
sleeps = pd.DataFrame()
recovery = pd.DataFrame()
workouts = pd.DataFrame()
v2_activities = pd.DataFrame()

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
        
    # v2_activities
    tmp = pd.json_normalize(records[record]["v2_activities"])
    if tmp.shape[0] > 0:
        v2_activities = pd.concat([v2_activities, tmp], ignore_index = True)
    

response = s.get(f'{api_url}/activities-service/v1/sports')
sports = pd.DataFrame(response.json()).set_index('id')
