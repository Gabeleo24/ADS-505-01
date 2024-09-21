### Here's how you can approach Assignment 2.1 based on the provided dataset and the use case for Tayko:

---

### **1. Estimate Gross Profit for Random Selection of Remaining 180,000 Names**

Each catalog costs approximately $2 to mail. You will estimate the gross profit if Tayko selects the 180,000 names randomly from the pool, based on the given response rate of 0.053 and the average spending of the purchasers.

#### Rationale:

- **Cost**: $2 per catalog.
- **Response Rate**: 0.053.
- **Spending per purchaser**: You can calculate the average spending from the dataset.

**R Code**:
```python
# Estimate gross profit for 180,000 random selections
response_rate = 0.053
cost_per_catalog = 2
num_names = 180000

# Calculate gross profit
expected_purchasers = num_names * response_rate

# Calculate average spending of purchasers from dataset
avg_spending = tayko_data[tayko_data['Purchase'] == 1]['Spending'].mean()

# Calculate gross profit
total_revenue = expected_purchasers * avg_spending
total_cost = num_names * cost_per_catalog
gross_profit = total_revenue - total_cost
print(f"Estimated Gross Profit: ${gross_profit}")
```

---

### **2. Develop a Model for Classifying Purchasers**

#### 2.1 Partition the Data

You need to randomly partition the data into:
- **Training Set**: 800 records.
- **Validation Set**: 700 records.
- **Test Set**: 500 records.

**Python Code**:
```python
from sklearn.model_selection import train_test_split

# Split the data
train_data, test_data = train_test_split(tayko_data, test_size=0.375, random_state=1)  # 0.375 of the dataset is 700 + 500 records
train_data, validation_data = train_test_split(train_data, test_size=700/1500, random_state=1)  # 700 validation records
```

#### 2.2 Logistic Regression with L2 Penalty

Use logistic regression with L2 penalty (`solver='lbfgs'`, `cv=5`, `max_iter=500`) to classify the data into purchasers and non-purchasers using the training set.

**Python Code**:
```python
from sklearn.linear_model import LogisticRegressionCV
from sklearn.preprocessing import StandardScaler

# Prepare the data
X_train = train_data.drop(columns=['Purchase', 'Spending'])  # predictors
y_train = train_data['Purchase']  # target

# Standardize the data
scaler = StandardScaler()
X_train_scaled = scaler.fit_transform(X_train)

# Logistic Regression with cross-validation
logit_cv = LogisticRegressionCV(cv=5, penalty='l2', solver='lbfgs', max_iter=500)
logit_cv.fit(X_train_scaled, y_train)

# Summary of model performance
print(f"Best C parameter: {logit_cv.C_}")
print(f"Logistic Regression Score: {logit_cv.score(X_train_scaled, y_train)}")
```

---

### **3. Predict Spending Among Purchasers**

#### 3.1 Filter for Purchasers
You will filter the training and validation datasets to include only records where `Purchase = 1`.

**Python Code**:
```python
train_purchasers = train_data[train_data['Purchase'] == 1]
validation_purchasers = validation_data[validation_data['Purchase'] == 1]
```

#### 3.2 Develop Models to Predict Spending

##### 3.2.1 Multiple Linear Regression with Stepwise Selection

You can use a forward or backward stepwise approach using Python's `statsmodels` package.

**Python Code**:
```python
import statsmodels.api as sm

# Prepare data for linear regression
X_train_purchasers = train_purchasers.drop(columns=['Purchase', 'Spending'])
y_train_purchasers = train_purchasers['Spending']

# Add constant to predictor variables
X_train_purchasers = sm.add_constant(X_train_purchasers)

# Fit the model
linear_model = sm.OLS(y_train_purchasers, X_train_purchasers).fit()
print(linear_model.summary())
```

##### 3.2.2 Regression Trees

For regression trees, you can use `DecisionTreeRegressor`.

**Python Code**:
```python
from sklearn.tree import DecisionTreeRegressor
from sklearn.metrics import mean_squared_error

# Fit a regression tree
tree_model = DecisionTreeRegressor(random_state=1)
tree_model.fit(X_train_purchasers, y_train_purchasers)

# Evaluate performance on the validation set
X_validation_purchasers = sm.add_constant(validation_purchasers.drop(columns=['Purchase', 'Spending']))
y_validation_purchasers = validation_purchasers['Spending']
pred_validation_tree = tree_model.predict(X_validation_purchasers)

# Calculate MSE
mse = mean_squared_error(y_validation_purchasers, pred_validation_tree)
print(f"Validation MSE for regression tree: {mse}")
```

#### 3.2.3 Select the Best Model
Compare the performance of the multiple linear regression and regression tree models on the validation data to choose the best model.

---

### **4. Apply Models to the Test Set**

#### 4.1 Add Predicted Scores

Using the logistic regression model, predict the purchase probabilities for the test set.

**Python Code**:
```python
# Predict probabilities for test data
X_test = test_data.drop(columns=['Purchase', 'Spending'])
X_test_scaled = scaler.transform(X_test)

test_data['Predicted_Probability'] = logit_cv.predict_proba(X_test_scaled)[:,1]
```

#### 4.2 Add Predicted Spending

Use the selected model (either linear regression or regression tree) to predict spending for the test set.

**Python Code**:
```python
# Predict spending based on the selected model
test_data['Predicted_Spending'] = tree_model.predict(sm.add_constant(test_data.drop(columns=['Purchase', 'Spending', 'Predicted_Probability'])))
```

#### 4.4 Calculate Expected Spending

Expected spending is the adjusted probability of purchase multiplied by the predicted spending.

**Python Code**:
```python
# Calculate expected spending
adjustment_factor = 0.053 / 0.5
test_data['Expected_Spending'] = test_data['Predicted_Probability'] * adjustment_factor * test_data['Predicted_Spending']
```

#### 4.5 Cumulative Gains Chart

Plot the cumulative gains chart for expected spending.

**Python Code**:
```python
import matplotlib.pyplot as plt
import numpy as np

# Sort test data by expected spending
test_data_sorted = test_data.sort_values(by='Expected_Spending', ascending=False)

# Calculate cumulative expected spending
cumulative_spending = np.cumsum(test_data_sorted['Expected_Spending'])

# Plot cumulative gains chart
plt.plot(range(len(cumulative_spending)), cumulative_spending)
plt.xlabel('Number of Records Targeted')
plt.ylabel('Cumulative Expected Spending')
plt.title('Cumulative Gains Chart')
plt.show()
```

#### 4.6 Estimate Gross Profit

Estimate gross profit based on mailing to 180,000 names using your data mining models.

---

### **5. Explanation for Non-Technical Stakeholders**

In this section, you need to summarize the business objective, the models you used (logistic regression and regression trees), and the key results for your marketing team.

You can refer to the PDF and the dataset for final details, and make sure the code runs without errors. Let me know if you need help with specific parts!