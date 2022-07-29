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
import tqdm

number = "64th" #change this each year

chromepath = "/Users/Julian/Desktop/misc/chromedriver"

driver = webdriver.Chrome(chromepath)

driver.get("https://www.reeltimeapps.com/live/tournaments/" + number + "-annual-big-rock-blue-marlin-tournament/participants")

# helpful stackoverflow link for line of code below: https://stackoverflow.com/questions/12570329/selenium-list-index-out-of-range-error

links = [i.get_attribute('href') for i in driver.find_elements_by_xpath(""".//a""")]

# for i in links: 
    # print(i) # find first link and last link for proper subset. Use links that end in a 4 digit #

links_subset = links[17:283] # change this each year - make sure you add +1 to the last link number 

boat_names = []
sizes_and_brands = []
owners = []
ports = []
captains = []

for link in tqdm.tqdm(links_subset):
    
    # slow down the code, website gets overloaded 
     
    time.sleep(3)
    
    # get html
    
    page = requests.get(link, timeout = 10) 
    
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
        
    # captain of the boat (only started listing at the 62nd annual)
    
    table = soup.find_all('table', id = 'lb-table-anglers')

    h4_list = soup.find_all('h4')
    
    no_crew_listed = any([h4_list[x].contents[0].strip() == 'No crew to display' for x in range(len(h4_list))]) # need to see if anything in the list == True to test if there are any captains listed
    
    if no_crew_listed: 
        
        captains.append('NA')
        
    else: 
        
        table_length = len(table)
        
        num_captain_mates_listed = len(table[table_length - 1].find_all('small')) 
        
        captain_mates_list = []
            
        [captain_mates_list.append(table[table_length - 1].find_all('small')[x].get_text()) for x in range(num_captain_mates_listed)]
        
        if 'Captain' in captain_mates_list:
        
            captains.append(table[table_length - 1].find('td').contents[0].strip())
        
        else:
            
            captains.append('NA')
    
    

 
len(boat_names) == len(sizes_and_brands) == len(owners) == len(ports) == len(captains) # check to make sure all have the same length 

d = {'boat_name':boat_names, 'type':sizes_and_brands, 'owner':owners, 'port':ports, 'captain':captains}

df = pd.DataFrame(d)

df.to_csv("/Users/Julian/Documents/projects/big_rock/data/participants/participants" + number + ".csv")







    