import pandas as pd
from sklearn.cluster import AgglomerativeClustering

# Load your data from the Excel file
data = pd.read_csv('H:/WFH/AI/data/HiSeqV2_PANCAN.txt', delimiter='\t', index_col=0)

# Perform hierarchical clustering (adjust parameters as needed)
clustering = AgglomerativeClustering(n_clusters=5)  # You can choose the number of clusters
cluster_labels = clustering.fit_predict(data)

# Add cluster labels to your DataFrame
data['Cluster'] = cluster_labels

# Reorder rows based on the 'Cluster' column (this step can be done in Excel)
data = data.sort_values(by='Cluster')

# Save the reordered data to a text file (tab-delimited)
data.to_csv('H:/WFH/AI/data/Reordered_Data.txt', sep='\t')
