from bs4 import BeautifulSoup
import requests
import pandas as pd
import re


df = pd.read_csv("ccmf_full_data.csv")

headlines = [] # create an empty headline list
contents = [] # create an empty content list

for index, row in df.iterrows():
    url = row['Article URL']
    # use "try" in case of missing pages
    try:
        # to avoid cookie cuased page access error(403), set up user agent
        set_session = requests.session()
        for_cookies = set_session.get(url)
        cookies = for_cookies.cookies
        headers = {'User-Agent': 'Mozilla/5.0 (Windows NT 6.1; Win64; x64; rv:57.0) Gecko/20100101 Firefox/57.0'}
        
        # access to web page
        source = set_session.get(url, headers=headers, cookies=cookies, timeout=5.5)
        soup = BeautifulSoup(source.text, 'html5lib')

        # ---- capture headlines ----
        headline = soup.title.text # extract text in title tage
        headline = headline.strip() # eliminate extra blank lines and space

        if headline == "PressProgress": # handle spacially structured webpage
            headline = soup.h1.span.text.strip()

        if headline is None: # in case the headline is in the <body> <h1> tage, in stead of <header> <title>
            headline = soup.h1.text.strip()

        # ---- capture article content ----
        content = '\n'.join([paragraph.text.strip() for paragraph in soup.find_all('p')]) # looping all paragraphs with <p> tag and add them together

        # special website handling
        # pattern = re.compile(r'https?://(www\.)?(\w+)(\.\w+)/(\w+)')
        check_list = ['aptnnews', 'halifaxexexaminer']
        # publishers = pattern.finditer(web)
        # news_agent = pattern.finditer(url)
        if any(s in str(url) for s in check_list):
            loc_content = soup.find('div', class_='entry-content')
            content = '\n'.join([paragraph.text.strip() for paragraph in loc_content.find_all('p')]) # looping all paragraphs with <p> tag and add them together
        
        content = str(content) # convert to string preparing for DataFrame

    except Exception as e:
        headline = e
        content = "Not Find"
    
    headlines.append(headline)
    contents.append(content)

    # progress monitoring in console/terminal
    id = row['Incident ID']
    print(f'{id}  {headline}')
    print()

# put the article elements into DataFrame
article_archive = pd.DataFrame(columns=['Incident_ID','article_headline','article_content'])
article_archive['Incident_ID'] = df['Incident ID']
article_archive['article_headline'] = headlines
article_archive['article_content'] = contents

# save datafrme into a csv file
article_archive.to_csv('article_archive.csv', index=False)
