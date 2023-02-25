#!/usr/bin/env python3

# -*- coding: utf-8 -*-

"""
Created on Wed Aug  3 22:34:28 2022
@author: JSWART
"""
##### get activity feed data annual 64: new code starting in 2022

from bs4 import BeautifulSoup
import urllib.request
import pandas as pd

number = '64th'

url = 'https://www.reeltimeapps.com/live/tournaments/" + number + "-annual-big-rock-blue-marlin-tournament/activities?day=all&page=all&per_page=1000&today=false&trips=0&type=all'

page = urllib.request.urlopen(url)

page.status_code # a status code of 200 means the page downloaded successfully

soup = BeautifulSoup(page, 'html.parser')

newsfeed = soup.find_all('article', {'class': 'm-b-20'})

boat_names = []

for feed in range(len(newsfeed)):

    boat_names.append(newsfeed[feed].find('h4').get_text().strip())

activity = []

for feed in range(len(newsfeed)):

    activity.append(newsfeed[feed].find('strong').get_text().strip())

time = []

for feed in range(len(newsfeed)):

    time.append(newsfeed[feed].find('p').get_text().strip())

len(boat_names) == len(activity) == len(time) # check to make sure all have the same length

d = {'boat_name':boat_names, 'activity':activity, 'time':time}

df = pd.DataFrame(d)

df.to_csv("/Users/Julian/Documents/projects/big_rock_2019/data/activity/activity" + number + ".csv")

 
