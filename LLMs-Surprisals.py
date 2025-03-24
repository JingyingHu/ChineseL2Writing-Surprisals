#Title: Modeling Chinese L2 Writing Development: The LLM-Surprisal Perspective 
#Auhtors: Jingying Hu, Yan Cong
#on March 24， 2025

import pandas as pd
import numpy as np
import torch
# !pip install minicons
from minicons import scorer

device = "cuda" if torch.cuda.is_available() else "cpu"
print("Using device:", device)

# load all the LLMs
bloom = scorer.IncrementalLMScorer('bigscience/bloom-7b1', device)  
llamaChinese = scorer.IncrementalLMScorer('hfl/chinese-llama-2-7b', device)
taiwanllm = scorer.IncrementalLMScorer('yentinglin/Taiwan-LLM-7B-v2.1-chat', device)  

# utility functions
def calculate_mean_surprisal(text, model):
    try:
        score = model.sequence_score([text], reduction=lambda x: -x.mean(0).item())
        return round(score[0],2)
    except Exception as e:
        print(f"Error processing text: {text}, Error: {e}")
        return np.nan

# read the data
data = ('/your input folder/')
result= ('/your result folder/')
data = pd.read_csv( data + 'ChineseL2Writing.csv')
data['bloom_surprisal'] = data['text'].apply(lambda x: calculate_mean_surprisal(x, bloom))
data['llamaChinese_surprisal'] = data['text'].apply(lambda x: calculate_mean_surprisal(x, llamaChinese))
data['taiwanllm_surprisal'] = data['text'].apply(lambda x: calculate_mean_surprisal(x, taiwanllm))

# save the result
data.to_csv(result + 'ChineseL2Writing_surprisal.csv')
