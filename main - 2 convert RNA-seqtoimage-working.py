import numpy as np
from PIL import Image
import os
import pandas as pd
import matplotlib.pyplot as plt  # Import plt for plotting
import matplotlib.cm as cm

# Load your data from the txt file (replace 'data.txt' with your file path)
data = pd.read_csv('H:/WFH/AI/data/HiSeqV2_PANCAN.txt', delimiter='\t', index_col=0)

# Separate patients and controls
patients = data.loc[:, data.columns.str[13] != '1']
controls = data.loc[:, data.columns.str[13] == '1']

# Set the dimensions of the image (adjust these values as needed)
m, n = 205, 101  # Example dimensions, adjust as needed

# Create directories for patients and controls
os.makedirs('H:/WFH/AI/data/patient', exist_ok=True)
os.makedirs('H:/WFH/AI/data/control', exist_ok=True)

# Process patient data
# Define the dimensions of the frame
frame_depth = 20

for i, (column_name, column_data) in enumerate(patients.items()):
    padded_data = np.pad(column_data.values, ((0, m * n - len(column_data))), mode='constant', constant_values=5)
    image_data = padded_data.reshape(m, n)

    # Normalize the data to [0, 1]
    normalized_data = (image_data - image_data.min()) / (image_data.max() - image_data.min())

    # Use a grayscale colormap to map the normalized data to grayscale colors
    grayscale_image = cm.gray_r(normalized_data)  # 'gray_r' for reversed grayscale

    # Create and save the image as a PNG file in the patient folder using plt.imsave
    plt.imsave(f'H:/WFH/AI/data/patient/patient_{i + 1}.png', grayscale_image, cmap='gray')

# Process control data
for j, (column_name, column_data) in enumerate(controls.items()):
    padded_data = np.pad(column_data.values, ((0, m * n - len(column_data))), mode='constant', constant_values=5)
    image_data = padded_data.reshape(m, n)

    # Normalize the data to [0, 1]
    normalized_data = (image_data - image_data.min()) / (image_data.max() - image_data.min())

    # Use a grayscale colormap to map the normalized data to grayscale colors
    grayscale_image = cm.gray_r(normalized_data)  # 'gray_r' for reversed grayscale


    # Create and save the image as a PNG file in the control folder using plt.imsave
    plt.imsave(f'H:/WFH/AI/data/control/control_{j + 1}.png', grayscale_image, cmap='gray')
