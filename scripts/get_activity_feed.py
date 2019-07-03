#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Jun 23 23:04:14 2019

@author: Julian
"""

# get activity feed data 

from bs4 import BeautifulSoup
import requests
import pandas as pd

page = requests.get("https://widgets.reeltimeapps.com/live/tournaments/61st-annual-big-rock-blue-marlin-tournament/widgets/feed.json?day=0&per=10000&type=scores")

page.status_code # a status code of 200 means the page downloaded successfully 

soup = BeautifulSoup(page.content, 'html.parser')

newsfeed = soup.find_all('h4')

boat_names = []

for feed in range(len(newsfeed)):
    boat_names.append(newsfeed[feed].get_text().strip())

activity = []

for feed in range(len(newsfeed)):
    activity.append(newsfeed[feed].next.next.next.strip())

time = []

for feed in range(len(newsfeed)):
    time.append(newsfeed[feed].next.next.next.next.next.next.get_text().strip())

len(boat_names) == len(activity) == len(time) # check to make sure all have the same length 

d = {'boat_name':boat_names, 'activity':activity, 'time':time}

df = pd.DataFrame(d)

df.to_csv("/Users/Julian/Documents/python/activity.csv")




    
    
    





