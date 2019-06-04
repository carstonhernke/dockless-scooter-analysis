# -*- coding: utf-8 -*-

import pandas as pd
import numpy as np
import sklearn
import pickle

import pdb
from sklearn import linear_model
from sklearn.linear_model import SGDClassifier
from sklearn.model_selection import train_test_split
from sklearn import linear_model
from sklearn.model_selection import cross_val_score
from sklearn.metrics import confusion_matrix
from sklearn.ensemble import GradientBoostingClassifier
from sklearn.ensemble import RandomForestClassifier
from sklearn.tree import DecisionTreeClassifier
from sklearn.svm import LinearSVC
from sklearn.ensemble import AdaBoostClassifier
from time import strftime, localtime
from sklearn.metrics import accuracy_score

from sklearn.preprocessing import OneHotEncoder
from sklearn.preprocessing import Normalizer

from sklearn.neural_network import MLPClassifier
from sklearn import svm

#data = pd.read_csv("scooter_trips_with_attributes.csv")
data = pd.read_csv("basic_trips.csv")
data = data.drop('Unnamed: 0',1)

scooter_y = pd.Series(data['end_hex'])
scooter_X = data.drop('end_hex', 1)

# removing the categorical variables for now (need to do one-hot encoding)
#scooter_X = scooter_X.drop('start_zone_type', 1)
#scooter_X = scooter_X.drop('StartTime', 1)

# just using the hour for now - need to look into how to incorporate minute data into continuous

# one-hot encoding (TO-DO)
#enc = preprocessing.OneHotEncoder()
#enc.fit(scooter_X)

# normalization (TO-DO)
#normalizer = preprocessing.Normalizer().fit(scooter_X)
"""
np.random.seed(0)
indices = np.random.permutation(len(scooter_X))
scoot_X_train = scooter_X[:-20000]
scoot_y_train = scooter_y[:-20000]
scoot_X_test = scooter_X[-20000:]
scoot_y_test = scooter_y[-20000:]
"""

enc = OneHotEncoder(handle_unknown='ignore')
scooter_X_transformed = enc.fit_transform(scooter_X)
scoot_X_train, scoot_X_test, scoot_y_train, scoot_y_test = train_test_split(scooter_X_transformed, scooter_y, test_size=0.3, random_state=0)

# fit a model
clf = SGDClassifier(loss="lasso", penalty="l2", shuffle=True)
clf = neighbors.KNeighborsClassifier(15, weights=weights)
clf = svm.SVC(gamma='scale')
clf = linear_model.Lasso(alpha=0.1)
clf = MLPClassifier(solver='lbfgs', alpha=1e-5, hidden_layer_sizes=(5, 2), random_state=1)
clf = DecisionTreeClassifier(random_state=0)

from sklearn.naive_bayes import GaussianNB
>>> gnb = GaussianNB()

clf.fit(scoot_X_train,scoot_y_train)


# test the fit
preds = clf.predict(scoot_X_test)
accuracy_score(scoot_y_test, preds)

# distance matrix
distance_matrix = pd.read_csv("distance_matrix.csv")
distance_matrix = distance_matrix.drop('Unnamed: 0',1)

def get_distance(start_hex, end_hex):
    return distance_matrix.iat[int(start_hex),int(end_hex)]

# takes distance of ride in meters, gives an approximate cost/ revenue
def Revenue(distance, per_minute = .15):
    duration = (distance * .5)/60
    revenue = 1 + (duration * per_minute)
    return revenue

def build_start_info(Start_Hex_ID, hour):
    return pd.DataFrame({'Start_Hex_ID': [Start_Hex_ID], 'hour': [hour]})

def predicted_revenue(start_info, hour_lag = 2):
    #print(start_info)
    #pdb.set_trace()
    start_hex = start_info.Start_Hex_ID.item()
    start_hour = start_info.hour.item()
    next_hour = start_hour + hour_lag
    transformed_input = enc.transform(build_start_info(start_hex,start_hour))
    prob_vector = clf.predict_proba(transformed_input)
    revenue = 0
    for i in range(0, len(prob_vector[0])):
        probability = prob_vector.item(i)
        if probability > 0.05:
            rev = Revenue(get_distance(start_hex, i)) * probability
            if (next_hour > 20):
                return rev
            else:
                next_start_info = build_start_info(i,next_hour)
                #print(next_start_info)
                revenue = revenue + predicted_revenue(next_start_info) * probability
    return revenue

for i in range(0,600):
    a = build_start_info(i, 8)
    print("Predicted revenue starting in HEX %d = $"%i + str(predicted_revenue(a)))

# for a given location:
sub = scoot_X_test[2:3]
start_hex = sub.Start_Hex_ID
prob_vector = clf.predict_proba(sub)
revenue = 0
for i in range(0, len(prob_vector[0])):
    probability = prob_vector.item(i)
    if probability > 0:
        print("greater than 0")
        rev = Revenue(get_distance(start_hex, i)) * probability
        revenue = revenue + rev
