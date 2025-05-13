import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

# Load the CSV file
file_path = "E:/University/Thesis/Thesis_Main/Thesis_Code/Dfs/yearly_indicators.csv"
df = pd.read_csv(file_path)

# Convert all columns to numeric where possible
df = df.apply(pd.to_numeric, errors='coerce')

# Drop rows where any column has null values
df = df.dropna()

# Drop columns with constant values
df = df.loc[:, df.nunique() > 1]

# Compute the correlation matrix
corr_matrix = df.corr()

# Display the correlation matrix
plt.figure(figsize=(10, 8))
sns.heatmap(corr_matrix, annot=True, cmap='coolwarm', fmt='.2f', linewidths=0.5)
plt.title('Correlation Matrix')
plt.show()