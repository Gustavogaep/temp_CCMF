import csv
import re
import pandas as pd
from bs4 import BeautifulSoup
import requests

# @Made by Decong Yu and Jeremy Fan 
df = pd.read_csv("ccmf_data_13Jul2020.csv")

webs = df['article url'] # 

# Publish date pattern 'July 3, 2019' or 'Jul 03, 2019'

#pattern = re.compile(r'(Jan\w*|Feb\w*|Mar\w*|May|Jun\w*|Jul\w*|Aug\w*|Sep\w*|Oct\w*|Nov\w*|Dec\w*)\s(\d{1,2}),\s20\d{2}|')
pattern = re.compile(r'(Jan\w*|Feb\w*|Mar\w*|May\w*|Jun\w*|Jul\w*|Aug\w*|Sep\w*|Oct\w*|Nov\w*|Dec\w*)\s(\d{1,2}),\s20\d{2}')

dates_published = [] # create an empty publish date list
for web in webs:

    # "try" in case there is no web link
    try:
        source = requests.get(web).text # access to the page and only extract text
        soup = BeautifulSoup(source, 'lxml') # soupify the page
        web_str = str(soup) # convert to string for 'regular expression' search
        print(web_str)

    except requests.ConnectionError:
        web_str = "Unable to find URL"

    # "try" in case the page misses the publish date
    try:
        publish_date = pattern.search(web_str).group(0) # use brutal force to find the first match

    except:
        publish_date = "Date Not Available"

    #print(publish_date) # easy to monitor in the console

    
    dates_published.append(publish_date) # append the date list

df['publish_date'] = dates_published # add the publish_date column

df.to_csv('new_ccmf.csv') # write the search results back into csv

## Round of 2 of cleaning (addressing missing dates)

#webs = df['none ']
#webs = df['article url'] # 
#Some have number dates .. in day/month/year
# #Others have Feb. 02 or something like that 