#This script is only to be run once for splitting the superstore.csv file into train and test sets
import pandas as pd
from sklearn.model_selection import train_test_split
import os

# Load the original dataset
file_path = os.path.join(".", "superstore.csv")

# Use encoding fix for special characters (based on your earlier work)
df = pd.read_csv(file_path, encoding='ISO-8859-1')

# Split into train and test sets (80/20)
train_df, test_df = train_test_split(df, test_size=0.2, random_state=42)

# Save the splits
train_path = os.path.join(".", "superstore_train.csv")
test_path = os.path.join(".", "superstore_test.csv")

train_df.to_csv(train_path, index=False)
test_df.to_csv(test_path, index=False)

# Confirmation
print(f"✅ Data split completed.")
print(f"Training set saved to: {train_path}")
print(f"Test set saved to: {test_path}")
print(f"Train shape: {train_df.shape}, Test shape: {test_df.shape}")
