
# coding: utf-8

# In[13]:


import pandas as pd
import numpy as np
from ggplot import *
import seaborn as sns
import matplotlib.pyplot as plt
get_ipython().run_line_magic('matplotlib', 'inline')


# In[15]:


df_wine = pd.read_csv("/Users/Jessica/Desktop/winereviews/winemag-data-130k-v2.csv")


# In[3]:


df_wine = df_wine.drop(["Unnamed: 0"], axis='columns')


# In[4]:


df_wine = df_wine.dropna(subset = ['price','country','variety'])


# In[16]:


df_wine.columns


# In[6]:


df_wine.info()


# In[7]:


df_wine.isnull().sum()


# In[8]:


## ggplot using size, shape and color as well as facets.
### in reality, it's not user-friendly to see too many variables in one chart


# In[9]:


df = df_wine[df_wine.variety.isin(df_wine.variety.value_counts().head(9).index)]
df = df[df.country.isin(df.country.value_counts().head(9).index)]
df


# In[20]:


p = ggplot(df,aes(x="points", y="price", shape ="variety", size ="price", color="country")) + geom_point(size=20)
p + facet_wrap('variety', scales="free_y") + xlab("points") + ylab("price") + ggtitle("winery review: country to price")


# In[ ]:


## from above plots, we can infer that France has the most expensive Birdeaux Red Blend 


# In[11]:


## Create a Correlation Heatmap in Seaborn using a public dataset.


# In[14]:


plt.subplots(figsize=(20,15))
ax = plt.axes()
ax.set_title("winery review")
corr = df.corr()
sns.heatmap(corr, 
            xticklabels=corr.columns.values,
            yticklabels=corr.columns.values)


# In[21]:


from sklearn.model_selection import train_test_split

X, y = df.iloc[:, 1:].values, df.iloc[:, 0].values

X_train, X_test, y_train, y_test = train_test_split(X, y, 
                                                    test_size=0.3,
                                                    random_state=0,
                                                    stratify=y)

