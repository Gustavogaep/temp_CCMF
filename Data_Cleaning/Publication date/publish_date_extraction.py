import csv
import re
import pandas as pd
from bs4 import BeautifulSoup
import requests

def visible_text(soup, remove_list=['[document]', 'noscript', 'header', 'html', 'meta', 'head', 'input', 'script']):
    """Extract all visible text from soup"""
    
    text = soup.find_all(text=True)
    output = ''

    for t in text:
        if t.parent.name not in remove_list:
            output += '{} '.format(t)
    
    return(output)


df = pd.read_csv("../race_relations_raw_consolidated.csv")

webs = df['article_url']

# A few of the article URLs are invalid because they contain two URLs separated by
# white space, so make sure only the first is kept
webs = webs.str.split(expand=True)[0]

# Publish date pattern 'July 3, 2019' or 'Jul 03, 2019'
pattern = re.compile(r'(Jan\w*|Feb\w*|Mar\w*|Apr\w*|May|Jun\w*|Jul\w*|Aug\w*|Sep\w*|Oct\w*|Nov\w*|Dec\w*)\s(\d{1,2}),\s20\d{2}')

dates_published = [] # create an empty publish date list
for web in webs:

    # "try" in case there is no web link
    try:
        source = requests.get(web).text # access to the page and only extrat text
        soup = BeautifulSoup(source, 'lxml') # soupfy the page
        # web_str = str(soup) # convert to string for 'regular expression' search
        
        # Extract visible text only
        web_str = visible_text(soup)
        
    except requests.ConnectionError:
        web_str = None

    # "try" in case the page misses the publish date
    try:
        publish_date = pattern.search(web_str).group(0) # use brutal force to find the first match
    except:
        publish_date = None

    print(publish_date) # easy to monitor in the console


    dates_published.append(publish_date) # append the date list

df['publish_date'] = dates_published # add the publish_date column

cols_out = ['incident_id', 'article_url', 'publish_date']
df[cols_out].to_csv('publish_dates.csv', index=False) # write the search results back into csv