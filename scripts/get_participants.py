#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Jun 25 23:03:30 2019

@author: Julian
"""

from selenium import webdriver
from bs4 import BeautifulSoup
import requests
import pandas as pd
import time

chromepath = "/Users/Julian/Desktop/chromedriver"

driver = webdriver.Chrome(chromepath)

driver.get("https://www.reeltimeapps.com/live/tournaments/61st-annual-big-rock-blue-marlin-tournament/participants")

# helpful stackoverflow link for line of code below: https://stackoverflow.com/questions/12570329/selenium-list-index-out-of-range-error

links = [i.get_attribute('href') for i in driver.find_elements_by_xpath(""".//a""")]

links_subset = links[14:198] # first boat link and last boat link are at link number 14 and 197

len(links_subset) == 184 # if true then good
    
for i in links_subset: 
    print(i)
    
boat_names = []
sizes_and_brands = []
owners = []
ports = []

for link in links_subset:
    
    # slow down the code, website gets overloaded 
     
    time.sleep(1)
    
    # get html
    
    page = requests.get(link, timeout = 5) 
    
    soup = BeautifulSoup(page.content, 'html.parser')
    
    # boat name
    
    tag1 = soup.find_all('h1', class_='m-b-5')
    
    
    if len(tag1) == 1:
        
        boat_names.append(tag1[0].get_text().strip())
        
    else:
        
        boat_names.append('NA')
        
    # boat size and brand
    
    tag2 = soup.find_all('h5')
    
    if len(tag2) == 1:
        
        sizes_and_brands.append(tag2[0].get_text().strip())
        
    else:
        
        sizes_and_brands.append('NA')
    
    # owner/company and port city, state
    
    tag3 = soup.find_all('h4', class_= 'm-0') 
    
    tag4 = soup.find_all('span')
    
    if len(tag4) == 5:
        
        tag4 = tag4[4].get_text().strip() # to help define if there is only a port or only an owner listed
   
    
    if len(tag3) == 2:
        
        owners.append(tag3[0].get_text().strip())
        
        ports.append(tag3[1].get_text().strip())
        
    elif len(tag3) == 1 and tag4 == 'Port':
        
        owners.append('NA')
        
        ports.append(tag3[0].get_text().strip())
        
    elif len(tag3) == 1 and tag4 == 'Owner':
        
        ports.append('NA')
        
        owners.append(tag3[0].get_text().strip())
        
    else:
        
        owners.append('NA')
        
        ports.append('NA')

 
len(boat_names) == len(sizes_and_brands) == len(owners) == len(ports) # check to make sure all have the same length 

d = {'boat_name':boat_names, 'type':sizes_and_brands, 'owner':owners, 'port':ports}

df = pd.DataFrame(d)

df.to_csv("/Users/Julian/Documents/python/participants.csv")
    