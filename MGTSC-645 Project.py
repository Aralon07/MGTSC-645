#!/usr/bin/env python
# coding: utf-8

# In[2]:


import pandas as pd

# File path - Modify this based on your local setup
file_path = r"D:\MBA\SEM 4\MGTSC 645\Project\Sample - Superstore.csv (1)\superstore.csv"

# Load the dataset
df = pd.read_csv(file_path, encoding='latin1')

# Display first few rows
print("First 5 rows of the dataset:")
print(df.head())

# Display dataset info
print("\nDataset Info (Before Cleaning):")
print(df.info())

# Check for duplicate rows
duplicates = df.duplicated().sum()
print(f"\nNumber of duplicate rows: {duplicates}")

# Drop duplicate rows
df = df.drop_duplicates()

# Check for missing values **(but do not fill them)**
print("\nMissing Values per Column:")
print(df.isnull().sum())

# Convert columns to correct data types
date_columns = ['Order Date', 'Ship Date']  # Modify if needed
for col in date_columns:
    df[col] = pd.to_datetime(df[col], errors='coerce')

# Drop columns that are useless for analysis (modify this list as needed)
columns_to_drop = ['Row ID', 'Postal Code']  # Example: Remove identifiers that are not useful
df = df.drop(columns=columns_to_drop, errors='ignore')

# Ensure correct data types
print("\nUpdated Data Types:")
print(df.dtypes)



