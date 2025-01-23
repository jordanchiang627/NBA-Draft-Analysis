import pandas as pd
from sklearn.preprocessing import OneHotEncoder, StandardScaler
from sklearn.compose import ColumnTransformer
from sklearn.pipeline import Pipeline
from sklearn.impute import SimpleImputer
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import accuracy_score, classification_report
import statsmodels.api as sm
import numpy as np

data = pd.read_csv('/Users/jordan/Desktop/NBA Draft Analysis/draft-data-20-years.csv')

# print(data.head(5))

data['SuccessfulCareer'] = (data['Yrs'] > 4).astype(int)

# print(data.head())


def clean_data(data, single_variable):
    data = data.dropna(subset=[single_variable, 'SuccessfulCareer'])
    numerical_features = data.select_dtypes(include=['number']).columns.tolist()
    for feature in numerical_features:
        data[feature].fillna(data[feature].mean(), inplace=True)

    return data


def remove_multicollinear_columns(X):
    variance = np.var(X, axis=0)
    non_zero_variance_columns = np.where(variance > 1e-5)[0]
    X = X[:, non_zero_variance_columns]
    
    if np.linalg.matrix_rank(X) < X.shape[1]:
        raise ValueError("The matrix still has multicollinearity issues or constant columns.")
    
    return X


def NBA_logistical_regression(data, single_variable, test_size=0.2, random_state=42):
    
    print("Variable Tested:", single_variable)

    data = clean_data(data, single_variable)

    if single_variable not in data.columns:
        raise ValueError(f"Column '{single_variable}' not found in data")
    
    X = data[[single_variable]]
    y = data['SuccessfulCareer']

    print(f"Length of X: {len(X)}, Length of y: {len(y)}")

    numerical_features = X.select_dtypes(include=['number']).columns.tolist()
    categorical_features = X.select_dtypes(include=['object', 'category']).columns.tolist()

    numeric_transformer = Pipeline(steps=[
        ('imputer', SimpleImputer(strategy='mean')),
        ('scalar', StandardScaler())
    ])

    categorical_transformer = Pipeline(steps=[
        ('imputer', SimpleImputer(strategy='constant', fill_value='missing')),
        ('onehot', OneHotEncoder(handle_unknown='ignore'))
    ])

    preprocessor = ColumnTransformer(
        transformers=[
            ('num', numeric_transformer, numerical_features),
            ('cat', categorical_transformer, categorical_features)
        ])
    
    X_processed = preprocessor.fit_transform(X)

    X_train, X_test, y_train, y_test = train_test_split(X_processed, y, test_size=test_size, random_state=random_state)

    logistic_model = LogisticRegression(max_iter=1000)
    logistic_model.fit(X_train, y_train)

    y_pred = logistic_model.predict(X_test)

    accuracy = accuracy_score(y_test, y_pred)
    report = classification_report(y_test, y_pred)

    print(f"Accuracy: {accuracy}")
    print("Classification Report:")
    print(report)

    X_train = remove_multicollinear_columns(X_train)
    X_test = X_test[:, :X_train.shape[1]]

    X_train_sm = sm.add_constant(X_train)
    X_test_sm = sm.add_constant(X_test)

    model = sm.Logit(y_train, X_train_sm)
    result = model.fit()

    print("\nStatsmodels Summary:")
    print(result.summary())

    return logistic_model, result


single_variable = 'Pk'
logistic_model, result = NBA_logistical_regression(data, single_variable)

single_variable = 'VORP'
logistic_model, result = NBA_logistical_regression(data, single_variable)

single_variable = 'FG%'
logistic_model, result = NBA_logistical_regression(data, single_variable)

single_variable = 'BPM'
logistic_model, result = NBA_logistical_regression(data, single_variable)