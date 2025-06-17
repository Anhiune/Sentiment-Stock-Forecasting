import re
import string
import pandas as pd
import nltk

from nltk.corpus import stopwords

file_path = r"C:\Users\hoang\Documents\Summer Research\python (locally download file)\used_tesla_tweets_extracted_first_version.csv"
df = pd.read_csv(file_path)

# First-time only:
nltk.download('stopwords')
stop_words = set(stopwords.words('english'))

text_column = df['Text'].dropna().reset_index(drop=True)
text_column.head() 

def clean_tweet(text):
    # Return empty string if input is not a string
    if not isinstance(text, str):
        return ""
    
    text = text.lower()
    text = re.sub(r'@\w+', '', text)
    text = re.sub(r'http\S+|www\S+', '', text)
    text = re.sub(r'#', '', text)
    text = text.encode('ascii', 'ignore').decode('ascii')
    text = text.translate(str.maketrans('', '', string.punctuation))
    text = re.sub(r'\d+', '', text)
    text = re.sub(r'\s+', ' ', text).strip()
    text = ' '.join(word for word in text.split() if word not in stop_words)
    
    return text

# Now load the file and apply the function
file_path = r"C:\Users\hoang\Documents\Summer Research\python (locally download file)\used_tesla_tweets_extracted_first_version.csv"
df = pd.read_csv(file_path, encoding='ISO-8859-1')

# Clean the text column
df['clean_text'] = df['Text'].apply(clean_tweet)

# Preview cleaned text
print(df[['Text', 'clean_text']].head())

output_path = r"C:\Users\hoang\Documents\Summer Research\python (locally download file)\cleand_used_tesla_tweets_extracted_first_version.csv"
df.to_csv(output_path, index=False, encoding='utf-8-sig')

print(f"✅ Cleaned file saved to:\n{output_path}")


