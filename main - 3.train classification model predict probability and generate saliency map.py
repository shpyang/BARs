import os
import cv2
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
from tests.tf_keras_vis.attentions_test import saliency
from tf_keras_vis.saliency import Saliency
from tf_keras_vis.utils import normalize
import tensorflow as tf
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Conv2D, MaxPooling2D, Flatten, Dense

def score_function(inputs):
    return tf.reduce_max(inputs, axis=1)

def loadImages(path, urls, target):
    images = []
    labels = []
    for i in range(len(urls)):
        img_path = os.path.join(path, urls[i])
        img = cv2.imread(img_path)
        #img = img * 255.0
        #img = cv2.resize(img, (100, 100))
        images.append(img)
        labels.append(target)
    images = np.asarray(images)
    return images, labels

# Load and preprocess the data
covid_path = "H:/WFH/AI/data/patient"
covid_urls = os.listdir(covid_path)
covidImages, covidTargets = loadImages(covid_path, covid_urls, 1)

normal_path = "H:/WFH/AI/data/control"
normal_urls = os.listdir(normal_path)
normalImages, normalTargets = loadImages(normal_path, normal_urls, 0)

data = np.concatenate((covidImages, normalImages), axis=0)
targets = np.concatenate((covidTargets, normalTargets), axis=0)

x_train, x_test, y_train, y_test = train_test_split(data, targets, test_size=0.8)

#Build the model
model = Sequential([
    Conv2D(32, 3, input_shape=(205, 101, 3), activation='relu'),
    MaxPooling2D(),
    Conv2D(16, 3, activation='relu'),
    MaxPooling2D(),
    Conv2D(16, 3, activation='relu'),
    MaxPooling2D(),
    Flatten(),
    Dense(512, activation='relu'),
    Dense(256, activation='relu'),
    Dense(1, activation='sigmoid')
])
print(len(x_test))
print(len(y_test))


# Compile and train the model
model.compile(optimizer='adam', loss=tf.keras.losses.BinaryCrossentropy(), metrics=['accuracy'])
model.fit(x_train, y_train, batch_size=32, epochs=10, validation_data=(x_test, y_test))

# Save the trained model
model.save("H:/WFH/AI/data/trained_model.keras")


# Load the saved model
loaded_model = tf.keras.models.load_model("H:/WFH/AI/data/trained_model.keras")

# Perform model predictions
predictions = loaded_model.predict(x_test)

from sklearn.metrics import accuracy_score

import matplotlib.pyplot as plt

# Perform model predictions
predictions = loaded_model.predict(x_test)

# Extract the prediction probabilities
prediction_probabilities = predictions.squeeze()  # Squeeze to remove extra dimensions

# Create a histogram of prediction probabilities
plt.hist(prediction_probabilities, bins=20, edgecolor='black')
plt.xlabel('Prediction Probabilities')
plt.ylabel('Frequency')
plt.title('Distribution of Prediction Probabilities')
plt.show()

# Convert probabilities to binary predictions (0 or 1)
binary_predictions = (predictions >= 0.5).astype(int)

# Calculate accuracy
accuracy = accuracy_score(y_test, binary_predictions)

# Print the accuracy percentage
accuracy_percentage = accuracy * 100
print(f"Accuracy: {accuracy_percentage:.2f}%")

# Define the file names of the selected samples
file_names = ['control_1.png', 'control_2.png', 'patient_11.png', 'patient_15.png']

# Create a custom color map with green for important regions and red for unimportant regions
cmap = plt.cm.RdYlGn_r

# Create a subplot for displaying the images and saliency maps
fig, ax = plt.subplots(4, 3, figsize=(16, 24))

# Generate and display the images and saliency maps
# Generate and display the images and saliency maps directly
for i, file_name in enumerate(file_names):
    # Load and preprocess the input image
    img_path = 'H:/WFH/AI/data/' + ('patient' if 'patient' in file_name else 'control') + '/'+file_name
    input_image = cv2.imread(img_path)
    input_image = cv2.cvtColor(input_image, cv2.COLOR_BGR2RGB)
    #input_image = cv2.resize(input_image, (100, 100)) / 255.0
    input_image_preprocessed = np.expand_dims(input_image, axis=0)

    # Create a Saliency instance (assuming you have already defined the score_function)
    saliency = Saliency(loaded_model, model_modifier=None)


    def score_function(inputs):
        # Perform operations to compute the score
        score = tf.reduce_max(inputs, axis=1)
        return score

    input_image_preprocessed = tf.cast(input_image_preprocessed, tf.float32)

    # Generate the saliency map
    saliency_map = saliency(score_function, input_image_preprocessed)
    # Normalize the saliency map
    saliency_map = normalize(saliency_map)

    # Display the original image in the left panel
    ax[i, 0].imshow(input_image)
    ax[i, 0].axis('off')
    ax[i, 0].set_title(file_name)

    # Display the saliency map in the middle panel with custom color map
    ax[i, 1].imshow(saliency_map[0], cmap=cmap)
    ax[i, 1].axis('off')
    ax[i, 1].set_title('Saliency Map')

    # Display the saliency map in the right panel with transparency
    ax[i, 2].imshow(saliency_map[0], cmap=cmap, alpha=0.5)
    ax[i, 2].imshow(input_image, alpha=0.5)
    ax[i, 2].axis('off')
    ax[i, 2].set_title('Overlay')

# Adjust the spacing between subplots
plt.tight_layout()

# Save the plot as a JPEG file
plt.savefig('H:/WFH/AI/Classicses saliency_map_with_overlay.jpg', dpi=600, bbox_inches='tight', pad_inches=0)

# Show the plot
plt.show()







