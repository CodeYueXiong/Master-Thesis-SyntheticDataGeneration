## shallow learning methods, e.g., logistic regression, Random Forest, SVM, to fit the synthetic CTIS data.

class Imputer(BaseEstimator, TransformerMixin):
    def __init__(self, features, method='constant', value='missing'):
        self.features = features
        self.method = method
        self.value = value
    
    def fit(self, X, y=None):
        if self.method=='mean':
            self.value = X[self.features].mean()
        return self
    
    def transform(self, X):
        X_transformed = X.copy()
        X_transformed[self.features] = X[self.features].fillna(self.value)
        return X_transformed
    
class Scaler(BaseEstimator, TransformerMixin):
    def __init__(self, features):
        self.features = features
    
    def fit(self, X, y=None):
        self.min = X[self.features].min()
        self.range = X[self.features].max()-self.min
        return self
    
    def transform(self, X):
        X_transformed = X.copy()
        X_transformed[self.features] = (X[self.features]-self.min)/self.range
        return X_transformed
  
class Encoder(BaseEstimator, TransformerMixin):
    def __init__(self, features, drop='first'):
        self.features = features
        self.drop = drop
    
    def fit(self, X, y=None):
        self.encoder = OneHotEncoder(sparse=False, drop=self.drop)
        self.encoder.fit(X[self.features])
        return self
    
    def transform(self, X):
        X_transformed = pd.concat([X.drop(columns=self.features).reset_index(drop=True), 
                                   pd.DataFrame(self.encoder.transform(X[self.features]), 
                                                columns=self.encoder.get_feature_names_out(self.features))],
                                  axis=1)
        return X_transformed