import pitchfork
import json
import re
import difflib
import sys
from bs4 import BeautifulSoup
import csv
import numpy as np
import codecs
import string

with open('C:\Users\kevin\Dropbox\Fun\p4k_1000.csv', 'rb') as f:
    reader = csv.reader(f)
    your_list = list(reader)
data = np.asarray(your_list)
authors=list()
texts=list()
scores=np.zeros(len(data))

yay=list()
for i in range(len(data)):
    try:
        p = pitchfork.search(artists[i], albums[i]) 
    except Exception:
        pass
    
    
    scores[i] = p.score()
    d = p.__dict__.copy()
    d['soup'] = d['soup'].prettify()
    author = p.soup.find(class_='info').h4.get_text()
    author = author[:author.index(';')].strip()
    authors.append(author)
    editorial = p.editorial()
    texts.append(editorial)

yay=list()
for i in range(len(texts)): 
    xx=  texts[i].encode('utf-8')
    xx = xx.replace("\'", "")
    xx = xx.replace("\n", " ")
    yay.append(''.join([s if ord(s)>31 and ord(s)<128 else ' ' for s in xx]))

auth=list()
for i in range(len(authors)): 
    xx=  authors[i].encode('utf-8')
    xx = xx.replace("\'", "")
    xx = xx.replace("\n", " ")
    auth.append(''.join([s if ord(s)>31 and ord(s)<128 else ' ' for s in xx]))
results=np.vstack((auth, scores, yay))
tresults=np.transpose(results)
with open('C:\Users\kevin\Dropbox\Fun\\results.csv', 'wb') as f:
    writer = csv.writer(f)
    
    writer.writerows(tresults)
