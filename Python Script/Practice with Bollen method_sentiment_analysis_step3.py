import pandas as pd

# Load your data (adjust file path as needed)
df = pd.read_csv(r"C:\Users\hoang\Documents\Summer Research\python (locally download file)\tesla_tweets_with_sentiment.csv")

# Convert 'date' column to datetime and truncate to date only (drop time and timezone)
df['date'] = pd.to_datetime(df['date']).dt.date  # Only the date part

# Group by date and average the emotion scores
emotion_cols = ['anger', 'anticipation', 'disgust', 'fear', 'joy', 
                'sadness', 'surprise', 'trust', 'positive', 'negative']  # adjust if your column names differ

daily_sentiment = df.groupby('date')[emotion_cols].mean().reset_index()

# Save the output if needed
daily_sentiment.to_csv(r"C:\Users\hoang\Documents\Summer Research\python (locally download file)\daily_tesla_sentiment.csv", index=False)
print("✅ Daily sentiment time series created and saved.")