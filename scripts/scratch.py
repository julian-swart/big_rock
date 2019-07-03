#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Jul  3 12:10:30 2019

@author: Julian
"""

# attempt to use selenium and grab the whole table at once
# but the times are incorrect on this website

from selenium import webdriver


chromepath = "/Users/Julian/Desktop/chromedriver"

driver = webdriver.Chrome(chromepath)

driver.get("https://www.thebigrock.com/tournament/participants/hookups/")

posts = driver.find_element_by_css_selector("table").text

my_string = posts.split("\n")

######################################

# attempt to use selenium and iteration of clicks and back-clicks to get 
# participants 
# indexing errors kept occuring 

from selenium import webdriver
from bs4 import BeautifulSoup
from selenium.webdriver.common.by import By
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.support.wait import WebDriverWait


chromepath = "/Users/Julian/Desktop/chromedriver"

driver = webdriver.Chrome(chromepath)

driver.get("https://www.thebigrock.com/tournament/participants/")
    

boat_names = []
sizes_and_brands = []
owners = []
ports = []

numbers = list(range(1,185))

for num in numbers:

    WebDriverWait(driver, 5).until(EC.element_to_be_clickable([By.XPATH, """//*[@id="rta-participants"]/div/div[2]/div[""" + str(1) + """]/a"""])).click()
    #driver.find_element_by_xpath("""//*[@id="rta-participants"]/div/div[2]/div[""" + str(1) + """]/a""").click()
    
    html = driver.page_source
    
    soup = BeautifulSoup(html, 'html.parser')
    
    # boat name
    tag1 = soup.find_all('h2', class_='m-b-5')
    boat_names.append(tag1[0].get_text().strip())
    
    # boat size and brand
    tag2 = soup.find_all('h3')
    sizes_and_brands.append(tag2[1].get_text().strip())
    
    # company
    tag3 = soup.find_all('h4')
    owners.append(tag3[0].get_text().strip())
    
    # city, state
    tag4 = soup.find_all('h4')
    ports.append(tag4[1].get_text().strip())
    
    # click back
    #driver.find_element_by_xpath("""//*[@id="rta-participant"]/div/a""").click()
    WebDriverWait(driver, 5).until(EC.element_to_be_clickable([By.XPATH, """//*[@id="rta-participant"]/div/a"""])).click()
    
