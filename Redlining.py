import seaborn as sns
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from matplotlib import rcParams
from sklearn.linear_model import LinearRegression
from functools import reduce
rcParams.update({'figure.autolayout': True})
 
 
def powerset(lst):
    return reduce(lambda result, x: result + [subset + [x] for subset in result], lst, [[]])
 
df = pd.read_csv('..\insurance.dat', sep='\s+', header=None, skiprows=1)
df.columns = ['zip', 'fire', 'theft', 'age', 'income', 'race', 'vol', 'invol']
df['age'] = np.power(df['age'], 2)
 
sns.pairplot(df)
plt.show()
 
corr = df.corr()
sns.heatmap(corr, annot=True)
plt.show()
 
vars = ['fire', 'theft', 'age', 'income', 'race']
sets = powerset(vars)
print(powerset(vars))
maxScore = 0
bestSet = 0
 
for x in sets:
    if len(x) >= 1:
        i = df[x]
        d = df['invol']
        model = LinearRegression()
        model.fit(i, d)
        print(x)
        score = model.score(i, d)
        print(score)
        if score > maxScore:
            maxScore = score
            bestSet = x
 
print(bestSet)
print(maxScore)
