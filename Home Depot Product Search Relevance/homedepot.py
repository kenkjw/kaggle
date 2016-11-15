# -*- coding: ISO-8859-1 -*-
import numpy as np
import pandas as pd
from sklearn.ensemble import RandomForestRegressor, BaggingRegressor
from sklearn.base import BaseEstimator, TransformerMixin
from sklearn import pipeline, grid_search
from sklearn.pipeline import FeatureUnion
from nltk.stem.snowball import SnowballStemmer
from sklearn.decomposition import TruncatedSVD
from sklearn.feature_extraction.text import TfidfVectorizer
from time import time
from textclean import str_clean
from spellcheck import spell_check
from sklearn.metrics import mean_squared_error, make_scorer
import os.path
import sys
reload(sys)
sys.setdefaultencoding('utf-8')

t_begin = time()
tstamp = str(int(time()))

def fmean_squared_error(ground_truth, predictions):
    fmean_squared_error_ = mean_squared_error(ground_truth, predictions)**0.5
    return fmean_squared_error_
RMSE  = make_scorer(fmean_squared_error, greater_is_better=False)

class cust_regression_vals(BaseEstimator, TransformerMixin):
    def fit(self, x, y=None):
        return self
    def transform(self, hd_searches):
        d_col_drops=['search_term','product_title','product_description','product_info','brand']
        hd_searches = hd_searches.drop(d_col_drops,axis=1).values
        return hd_searches

class cust_txt_col(BaseEstimator, TransformerMixin):
    def __init__(self, key):
        self.key = key
    def fit(self, x, y=None):
        return self
    def transform(self, data_dict):
        return data_dict[self.key].apply(str)


print "Reading data from files"
df_train = pd.read_csv('in/train.csv', encoding="ISO-8859-1")
df_test = pd.read_csv('in/test.csv', encoding="ISO-8859-1")
df_attr = pd.read_csv('in/attributes.csv', encoding="ISO-8859-1")
df_pro_desc = pd.read_csv('in/product_descriptions.csv')

num_train = df_train.shape[0]

def str_stemmer(s):
    return " ".join([stemmer.stem(word) for word in s.lower().split()])

def str_common_word(str1, str2):
    return sum(int(str2.find(word)>=0) for word in str1.split())

df_brand = df_attr[df_attr.name == "MFG Brand Name"][["product_uid", "value"]].rename(columns={"value": "brand"})
df_all = pd.concat((df_train, df_test), axis=0, ignore_index=True)
df_all = pd.merge(df_all, df_pro_desc, how='left', on='product_uid')
df_all = pd.merge(df_all, df_brand, how='left', on='product_uid')



if (os.path.exists('out/_stemmed_text.csv')):
    print "Stemmed text file exists. Loading from file."
    df_stemmed = pd.read_csv('out/_stemmed_text.csv', encoding="ISO-8859-1")
    df_all = df_all.drop(['search_term','product_title','product_description','brand'],axis=1)
    df_all = pd.concat((df_all,df_stemmed),axis=1)
    #df_all.to_csv('out/_load_stemmed_text.csv',index=False,header=True,encoding="ISO-8859-1")
else:
    print "Stemmed text file does not exist. Stemming text."
    df_all['search_term'] = df_all['search_term'].map(lambda x:str_clean(spell_check(x)))
    df_all['product_title'] = df_all['product_title'].map(lambda x:str_clean(x))
    df_all['product_description'] = df_all['product_description'].map(lambda x:str_clean(x))
    df_all['brand'] = df_all['brand'].map(lambda x:str_clean(x))
    df_all[['search_term','product_title','product_description','brand']].to_csv('out/_stemmed_text.csv',index=False,header=True, encoding="ISO-8859-1")
    #df_all.to_csv('out/_create_stemmed_text.csv',index=False,header=True,encoding="ISO-8859-1")


# TODO: Brand has empty strings that are read as floats.
df_all['brand'] = df_all['brand'].fillna('unbrandedrandf')
df_all['len_of_query'] = df_all['search_term'].map(lambda x:len(x.split())).astype(np.int64)
df_all['len_of_title'] = df_all['product_title'].map(lambda x:len(x.split())).astype(np.int64)
df_all['len_of_description'] = df_all['product_description'].map(lambda x:len(x.split())).astype(np.int64)
df_all['len_of_brand'] = df_all['brand'].map(lambda x:len(x.split())).astype(np.int64)

df_all['product_info'] = df_all['search_term']+"\t"+df_all['product_title']+"\t"+df_all['product_description']


df_all['query_in_title'] = map(lambda (x,y):y.find(x) >= 0,zip(df_all.search_term,df_all.product_title))
df_all['query_in_description'] = map(lambda (x,y):y.find(x) >= 0,zip(df_all.search_term,df_all.product_description))
df_all['query_in_brand'] = map(lambda (x,y):y.find(x) >= 0 if isinstance(y,str) else False,zip(df_all.search_term,df_all.brand))
df_all['brand_in_query'] = map(lambda (x,y):y.find(x) >= 0 if isinstance(x,str) else False,zip(df_all.brand,df_all.search_term))


df_all['word_in_title'] = df_all['product_info'].map(lambda x:str_common_word(x.split('\t')[0],x.split('\t')[1]))
df_all['word_in_description'] = df_all['product_info'].map(lambda x:str_common_word(x.split('\t')[0],x.split('\t')[2]))
df_all['word_in_brand'] = map(lambda (x,y):str_common_word(x,y),zip(df_all.search_term,df_all.brand))

df_all['ratio_title'] = df_all['word_in_title']/df_all['len_of_query']
df_all['ratio_description'] = df_all['word_in_description']/df_all['len_of_query']
df_all['ratio_brand'] = df_all['word_in_brand']/df_all['len_of_brand']


#df_all = df_all.drop(['search_term','product_title','product_description','product_info','brand'],axis=1)
df_train = df_all.iloc[:num_train]
df_test = df_all.iloc[num_train:]
id_test = df_test['id']
y_train = df_train['relevance'].values
X_train = df_train.drop(['id','relevance'],axis=1)
X_test = df_test.drop(['id','relevance'],axis=1)



rfr = RandomForestRegressor(n_estimators = 500, n_jobs = 1, random_state = 4048, verbose = 1)
tfidf = TfidfVectorizer(ngram_range=(1, 1), stop_words='english')
tsvd = TruncatedSVD(n_components=10, random_state = 4048)
clf = pipeline.Pipeline([
        ('union', FeatureUnion(
                    transformer_list = [
                        ('cst',  cust_regression_vals()),  
                        ('txt1', pipeline.Pipeline([('s1', cust_txt_col(key='search_term')), ('tfidf1', tfidf), ('tsvd1', tsvd)])),
                        ('txt2', pipeline.Pipeline([('s2', cust_txt_col(key='product_title')), ('tfidf2', tfidf), ('tsvd2', tsvd)])),
                        ('txt3', pipeline.Pipeline([('s3', cust_txt_col(key='product_description')), ('tfidf3', tfidf), ('tsvd3', tsvd)])),
                        ('txt4', pipeline.Pipeline([('s4', cust_txt_col(key='brand')), ('tfidf4', tfidf), ('tsvd4', tsvd)]))
                        ],
                    transformer_weights = {
                        'cst': 1.0,
                        'txt1': 0.5,
                        'txt2': 0.25,
                        'txt3': 0.0,
                        'txt4': 0.5
                        },
                #n_jobs = -1
                )), 
        ('rfr', rfr)])
param_grid = {'rfr__max_features': [10], 'rfr__max_depth': [20]}
model = grid_search.GridSearchCV(estimator = clf, param_grid = param_grid, n_jobs = 1, cv = 2, verbose = 20, scoring=RMSE)

t0 = time()
print "Begin training"
model.fit(X_train, y_train)
t1 = time()
print "Training complete: ", t1-t0, "s"

print("Best parameters found by grid search:")
print(model.best_params_)
print("Best CV score:")
print(model.best_score_)
print(model.best_score_ + 0.47003199274)

t0 = time()
print "Begin predict"
y_pred = model.predict(X_test)
t1 = time()
print "Predict complete: ", t1-t0, "s"
print "Total time: ", time()-t_begin, "s"

outfile = 'out/_homedepot_submission_'+tstamp+'.csv'
pd.DataFrame({"id": id_test, "relevance": y_pred}).to_csv(outfile,index=False)

'''
df_all.to_csv('out/_features_' + tstamp + '.csv',index=False,header=True,encoding="ISO-8859-1")
rf = RandomForestRegressor(n_estimators=15, max_depth=6, random_state=4048)
clf = BaggingRegressor(rf, n_estimators=45, max_samples=0.1, random_state=4048)
t0 = time()
print "Begin training"
clf.fit(X_train, y_train)
t1 = time()
print "Training complete: ", t1-t0, "s"
t0 = time()
print "Begin predict"
y_pred = clf.predict(X_test)
t1 = time()
print "Predict complete: ", t1-t0, "s"
print "Total time: ", time()-t_begin, "s"
outfile = 'out/_homedepot_submission_'+tstamp+'.csv'
pd.DataFrame({"id": id_test, "relevance": y_pred}).to_csv(outfile,index=False)
'''