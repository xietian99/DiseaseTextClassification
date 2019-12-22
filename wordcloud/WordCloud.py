#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Nov 14 11:01:25 2019

@author: tianxie
"""
from nltk.stem import WordNetLemmatizer
from nltk.stem import PorterStemmer 
import pandas as pd
import numpy as np
from wordcloud import WordCloud #, ImageColorGenerator
import matplotlib.pyplot as plt
import re
import string
#from PIL import Image

def grey_color_func(word, font_size, position,orientation,random_state=None, **kwargs):
    return("hsl(200,120%%, %d%%)" % np.random.randint(30,60))


def wf2cloud(wordFreq,tofile):            
    # mymask = np.array(Image.open(pic)) # define     
    wc = WordCloud(background_color='white', 
                             # mask=mymask, 
                             max_words=200, 
                             max_font_size=100,          
                             scale = 8             )     
    wc.generate_from_frequencies(wordFreq)     
    # image_colors = ImageColorGenerator() #    
    # wc.recolor(color_func=grey_color_func)     
    plt.figure(figsize=(12,12))    
    plt.imshow(wc)      
    plt.axis('off')  
    plt.savefig(tofile)
    
def data2wc(textData, stopfile, tofile):
    stopwords = []
    with open(stopfile, "r") as fd:
        for line in fd:
            line = line.strip()
            stopwords.append(line)
    stopwords.extend(['YOM', "YOF"])
    stopwords.extend(list(string.ascii_lowercase))
    
    # regurliratuoan and lower 
    for i in range(len(textData)):
        textData[i] = re.sub('[1-9][0-9]*.*YO[\s|W]*[|M|F]','', textData[i]).lower()
    
    # 
    wnl = WordNetLemmatizer()
    ps = PorterStemmer()
    corpus = [[wnl.lemmatize(ps.stem(word)) for word in document.split() if word not in stopwords]
             for document in textData ]
    
    
    vocab = []  
    for line in corpus:  
        for word in line:
            if (word != u' ' and word not in vocab):  
                vocab.append(word)  
                
    # doc2vec
    CountMatrix = []  
    for line in corpus: 
        count = np.zeros(len(vocab),dtype=np.int)  
        for word in line:  
            if word in vocab:  
                count[vocab.index(word)] += 1  
        if ((count==0).all()) != True :
            CountMatrix.append(count)  
    npDTM = np.array(CountMatrix)
    
    Freq = list(np.sum(npDTM, axis=0))
    
    word_freq = dict(zip(vocab,Freq))
    wf2cloud(word_freq, tofile)
  

Data = pd.read_csv("CDC_Text_ClassificationChallenge_TrainData.csv")
Data1 = Data[(Data["event"] // 10) == 1]
Data4 = Data[(Data["event"] // 10) == 4]
#Data_Event = Data[Data["event"] == 43]
# Data_Event = Data[Data["event"] == 32]
textData1 = list(Data1["text"])
textData4 = list(Data4["text"])

stopfile = "/Users/tianxie/nltk_data/corpora/stopwords/english" 
data2wc(textData1, stopfile, "wc1.png")
data2wc(textData4, stopfile, "wc4.png")
