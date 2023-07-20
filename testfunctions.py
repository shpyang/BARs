import cv2
import numpy as np
import matplotlib.pyplot as plt
from scan.functions import sharpen
import csv
from itertools import combinations
from scipy.stats import linregress
from scipy.optimize import curve_fit
from sklearn.linear_model import RANSACRegressor
from sklearn.preprocessing import PolynomialFeatures
from sklearn.pipeline import make_pipeline

# Load the image
image = cv2.imread('m5.jpg')
height, width, channels = image.shape
# Convert the image to grayscale
gray = cv2.cvtColor(image, cv2.COLOR_RGB2GRAY)
gray = sharpen(gray)
image = cv2.cvtColor(gray, cv2.COLOR_GRAY2RGB)
black_pixel_counts = []
thresholded_image = cv2.adaptiveThreshold(gray, 255, cv2.ADAPTIVE_THRESH_GAUSSIAN_C, cv2.THRESH_BINARY, 51,
                                             15)
_, thresholded_imageo = cv2.threshold(gray, 0, 255, cv2.THRESH_BINARY + cv2.THRESH_OTSU)
# thresholded_image = cv2.bitwise_or(thresholded_image, thresholded_imageo)
kernel = cv2.getStructuringElement(cv2.MORPH_RECT, (1, 3))  # Adjust the kernel size as needed
# Perform the opening operation
thresholded_image = cv2.morphologyEx(thresholded_image, cv2.MORPH_OPEN, kernel)

imageo = cv2.cvtColor(thresholded_image, cv2.COLOR_GRAY2RGB)
# Perform morphological dilation
kernel_size = (2, 1)  # (Width, Height)
# Create the custom kernel
kernel = cv2.getStructuringElement(cv2.MORPH_RECT, kernel_size)
# Perform one-sided dilation with the recalculated weighted center
dilated = 255 - imageo.copy()  # Create a copy of the original image
for _ in range(20):  # Number of iterations for dilation
    # Calculate the weighted center of the current kernel
    kernel_sum = np.sum(kernel)
    weighted_center = (int(np.sum(np.multiply(kernel, np.arange(kernel_size[0]))) / kernel_sum),
                           int(np.sum(np.multiply(kernel, np.arange(kernel_size[1]))) / kernel_sum))

    # Perform dilation with the recalculated anchor point
    dilated = cv2.dilate(dilated, kernel, anchor=weighted_center, iterations=1)
    # Invert the image
inverted = cv2.bitwise_not(dilated)
inverted_gray = cv2.cvtColor(inverted, cv2.COLOR_BGR2GRAY)

# Apply thresholding to create a binary image
# _, binary = cv2.threshold(inverted_gray, 127, 255, cv2.THRESH_BINARY)
_, binary = cv2.threshold(inverted_gray, 0, 255, cv2.THRESH_BINARY + cv2.THRESH_OTSU)
# Find contours of the text regions
contours, _ = cv2.findContours(binary, cv2.RETR_LIST, cv2.CHAIN_APPROX_SIMPLE)
# contours, _ = cv2.findContours(inverted, cv2.RETR_LIST, cv2.CHAIN_APPROX_SIMPLE)
contours = list(reversed(contours))


# Calculate the average of rotated angles of all contours
total_rotated_angle = 0.0
num_contours = 0

# Filter contours based on width, height, and aspect ratio
filtered_contours = []
for cnt in contours:
    x, y, w, h = cv2.boundingRect(cnt)
    aspect_ratio = h / float(w)
    if w > image.shape[1] / 1150 and w < width and h >= 30 and aspect_ratio <= 5 and h < height * 0.9:
       filtered_contours.append(cnt)
       # Calculate the rotated angle of the current contour
       rect = cv2.minAreaRect(cnt)
       rotated_angle = rect[2]  # Extract the angle from the rectangle
       total_rotated_angle += rotated_angle
       num_contours += 1

# Draw the filtered contours on the image
contour_image = imageo.copy()
cv2.drawContours(contour_image, filtered_contours, -1, (0, 0, 255), 2)

# Create a blank image to hold the filled contours
filled_contour_image = np.zeros_like(contour_image)

# Function to check if a contour is completely inside another contour
def is_contour_inside(contour1, contour2):
    result = cv2.pointPolygonTest(contour2, tuple(contour1[0][0]), measureDist=False)
    return result == 1

# Create a list to store indices of nested contours
nested_contour_indices = []

# Check if a contour is inside another and mark it for deletion
for i, contour in enumerate(filtered_contours):
    for j, other_contour in enumerate(filtered_contours):
        if i != j and is_contour_inside(contour.astype(np.float32), other_contour.astype(np.float32)):
            nested_contour_indices.append(i)
            break

# Remove nested contours from the filtered_contours list
filtered_contours = [contour for i, contour in enumerate(filtered_contours) if i not in nested_contour_indices]








# Fill the contours with yellow color
for contour in filtered_contours:
    cv2.drawContours(filled_contour_image, [contour], 0, (0, 255, 255), thickness=cv2.FILLED)


def calculate_mean_pixel_vertical_position_in_contour(contour, image):
    x, y, w, h = cv2.boundingRect(contour)
    mask = np.zeros(image.shape[:2], dtype=np.uint8)
    cv2.drawContours(mask, [contour], 0, 255, thickness=cv2.FILLED)

    # Apply the mask to the image and extract the vertical positions of the pixels within the contour
    masked_image = cv2.bitwise_and(image, image, mask=mask)
    column_positions = []
    for col in range(x, x + w):
        column_pixels = np.argwhere(masked_image[:, col, 0] > 0)[:, 0]
        if column_pixels.any():
            mean_vertical_pos = np.mean(column_pixels)
        else:
            # Set a default value or handle empty column_pixels list as needed
            mean_vertical_pos = y + h // 2  # Center of the contour
        column_positions.append(mean_vertical_pos)

    return column_positions

# Draw the filtered contours on the image
contour_image = imageo.copy()
cv2.drawContours(contour_image, filtered_contours, -1, (0, 0, 255), 2)  # Displaying all filtered contours

# Create a blank image to hold the filled contours
filled_contour_image = np.zeros_like(contour_image)

# Lists to store all column positions from all contours
all_r_positions = []
all_c_positions = []

# Fill the contours with yellow color and plot curves for all filtered contours
for contour in filtered_contours:
    cv2.drawContours(filled_contour_image, [contour], 0, (0, 255, 255), thickness=cv2.FILLED)

    # Get the mean vertical position for each column within the contour
    r_positions = calculate_mean_pixel_vertical_position_in_contour(contour, image)

    # Find the minimum x-coordinate of the contour
    min_x = np.min(contour[:, 0, 0])

    # Create x-axis values relative to the contour's position
    x_values = np.arange(len(r_positions)) + min_x

    # Plot the curve for the current contour on top of the filled contour image
    plt.plot(x_values, r_positions, color='red', linewidth=1)

    # Append the r_positions to the all_r_positions list
    all_r_positions.extend(r_positions)
    c_positions = x_values
    all_c_positions.extend(c_positions)

# Show the figure with both the filled contour image and the overlaid curves for all contours
plt.imshow(filled_contour_image)
plt.xlabel('Column Position (Relative to Contour)')
plt.ylabel('Mean Vertical Position')
plt.title('Curves Overlay on Contours')

# Set high resolution for the saved image (300 dpi, for example)
plt.savefig("out.jpg", dpi=300)

# Show the plot
#plt.show()

# Plot a horizontal histogram of all column positions
plt.figure()
plt.hist(all_r_positions, bins=300, orientation='horizontal', color='blue')
plt.gca().invert_yaxis()
plt.xlabel('Frequency')
plt.ylabel('Mean Vertical Position')
plt.title('Horizontal Histogram of Mean Vertical Positions')
plt.savefig("histogram.jpg", dpi=300)
plt.show()


def find_most_frequent_frequency(histogram):
    # Apply Fourier Transformation to the histogram
    f_transform = np.fft.fft(histogram)
    f_magnitude = np.abs(f_transform)

    # Find the index of the peak frequency
    peak_index = np.argmax(f_magnitude[1:]) + 1

    # Calculate the most frequent frequency value
    most_frequent_frequency = peak_index / len(histogram)

    return most_frequent_frequency


# Calculate the histogram of all column positions
histogram, bin_edges = np.histogram(all_r_positions, bins=256, range=(0, 256))
chistogram, cbin_edges = np.histogram(all_c_positions, bins=256, range=(0, 256))

# Find the most frequent frequency in the histogram
most_frequent_bin = np.argmax(histogram)

# Convert the bin index to the corresponding value
most_frequent_frequency = (bin_edges[most_frequent_bin] + bin_edges[most_frequent_bin + 1]) / 2


# Draw a line on the original image at the position of the most frequent frequency
x_position = int(most_frequent_frequency * width)
cv2.line(image, (x_position, 0), (x_position, height), (0, 0, 255), thickness=2)

# Save the image with the line overlay
cv2.imwrite("image_with_frequency_line.jpg", image)

out = list(zip(all_r_positions, all_c_positions))
with open('most_frequent_frequency.csv', 'w', newline='') as csvfile:
    csv_writer = csv.writer(csvfile)
    csv_writer.writerow(['Row', 'Column'])
    csv_writer.writerows(out)


# ... (previous code)

# Create a blank image to hold the convex hulls of the contours
convex_hulls_image = np.zeros_like(image)

# Draw the convex hulls of each contour on the blank image
for contour in filtered_contours:
    convex_hull = cv2.convexHull(contour)
    cv2.drawContours(convex_hulls_image, [convex_hull], 0, (0, 255, 0), thickness=2)

    # Optionally, you can also draw the original contour in blue
    cv2.drawContours(convex_hulls_image, [contour], 0, (255, 0, 0), thickness=1)

# Show the image with the convex hulls and original contours
plt.imshow(convex_hulls_image)
plt.xlabel('Column Position (Relative to Contour)')
plt.ylabel('Mean Vertical Position')
plt.title('Contours with Convex Hulls')

plt.savefig("Convex Hulls.jpg", dpi=600)
plt.show()



# Lists to store all column positions from all contours
all_r_positions = []
all_c_positions = []

# Fill the contours with yellow color and plot curves for all filtered contours
for contour in filtered_contours:
    cv2.drawContours(filled_contour_image, [contour], 0, (0, 255, 255), thickness=cv2.FILLED)

    # Get the mean vertical position for each column within the contour
    r_positions = calculate_mean_pixel_vertical_position_in_contour(contour, image)

    # Find the minimum x-coordinate of the contour
    min_x = np.min(contour[:, 0, 0])

    # Create x-axis values relative to the contour's position
    x_values = np.arange(len(r_positions)) + min_x

    # Plot the curve for the current contour on top of the filled contour image
    plt.plot(x_values, r_positions, color='red', linewidth=1)

    # Append the r_positions to the all_r_positions list
    all_r_positions.extend(r_positions)
    c_positions = x_values
    all_c_positions.extend(c_positions)

    # Apply quadratic regression to predict y-values (r_positions) from x-values (c_positions)
    x = np.array(c_positions)
    y = np.array(r_positions)

    # Fit a quadratic polynomial to the data
    z = np.polyfit(x, y, 2)

    # Create a function to predict y-values using the quadratic polynomial coefficients
    quadratic_function = np.poly1d(z)

    # Predict y-values using the quadratic function
    y_pred = quadratic_function(x)

    # Append the predicted y-values (y_pred) as the 3rd column to the .csv file
    with open('quadratic_regression_predictions.csv', 'a', newline='') as csvfile:
        csv_writer = csv.writer(csvfile)
        for i, pred in enumerate(y_pred):
            csv_writer.writerow([c_positions[i], r_positions[i], pred])


# ... (previous code)

# Function to perform robust quadratic regression
def robust_quadratic_regression(x, y):
    # Calculate the weights for robust regression (Huber's weights)
    weights = np.where(np.abs(y - np.median(y)) < 3.0 * np.std(y), 1.0, 0.0)

    # Fit a quadratic polynomial using robust regression
    z = np.polyfit(x, y, 2, w=weights)

    return z

# Lists to store all column positions from all contours
all_r_positions = []
all_c_positions = []

# Fill the contours with yellow color and plot curves for all filtered contours
for contour in filtered_contours:
    cv2.drawContours(filled_contour_image, [contour], 0, (0, 255, 255), thickness=cv2.FILLED)

    # Get the mean vertical position for each column within the contour
    r_positions = calculate_mean_pixel_vertical_position_in_contour(contour, image)

    # Find the minimum x-coordinate of the contour
    min_x = np.min(contour[:, 0, 0])

    # Create x-axis values relative to the contour's position
    x_values = np.arange(len(r_positions)) + min_x

    # Plot the curve for the current contour on top of the filled contour image
    plt.plot(x_values, r_positions, color='red', linewidth=1)

    # Append the r_positions to the all_r_positions list
    all_r_positions.extend(r_positions)
    c_positions = x_values
    all_c_positions.extend(c_positions)

    # Apply robust quadratic regression to predict y-values (r_positions) from x-values (c_positions)
    x = np.array(c_positions)
    y = np.array(r_positions)

    # Perform robust quadratic regression
    z = robust_quadratic_regression(x, y)

    # Create a function to predict y-values using the robust quadratic polynomial coefficients
    quadratic_function = np.poly1d(z)

    # Predict y-values using the quadratic function
    y_pred = quadratic_function(x)

    # Append the predicted y-values (y_pred) as the 3rd column to the .csv file
    with open('robust_quadratic_regression_predictions.csv', 'a', newline='') as csvfile:
        csv_writer = csv.writer(csvfile)
        for i, pred in enumerate(y_pred):
            csv_writer.writerow([c_positions[i], r_positions[i], pred])




def fit_quadratic_regression(x, y):
    def quadratic_function(x, a, b, c):
        return a * x ** 2 + b * x + c

    # Perform quadratic regression using curve_fit from scipy
    popt, _ = curve_fit(quadratic_function, x, y)

    return popt
def has_horizontal_overlap(contour1, contour2):
    x1, _, w1, _ = cv2.boundingRect(contour1)
    x2, _, w2, _ = cv2.boundingRect(contour2)
    return abs(x1 - x2) < min(w1, w2) * 0.75
def find_best_model(contours):
    best_model = None
    best_r_squared = -1

    for contour in contours:
        x_positions = [cv2.boundingRect(c)[0] for c in contours if c is not contour]
        y_positions = calculate_mean_pixel_vertical_position_in_contour(contour, image)

        params = fit_quadratic_regression(x_positions, y_positions)
        y_pred = np.polyval(params, x_positions)

        # Calculate R-squared to evaluate the model fit
        r_squared = 1 - np.sum((y_positions - y_pred) ** 2) / np.sum((y_positions - np.mean(y_positions)) ** 2)

        if r_squared > best_r_squared:
            best_model = contour
            best_r_squared = r_squared

    return best_model
def find_contours_in_same_row(contours):
    row_contours = []

    for contour in contours:
        x, y, w, h = cv2.boundingRect(contour)
        centroid_y = y + h // 2

        # Check if there are any existing rows to group the contour
        row_found = False
        for row_contour in row_contours:
            # Find the y-coordinate of the row's centroid
            row_x, row_y, row_w, row_h = cv2.boundingRect(row_contour[0])
            row_centroid_y = row_y + row_h // 2

            # Define a tolerance for grouping contours in the same row
            tolerance = 100

            # If the contour's centroid is close to the row's centroid, add it to the row
            if abs(centroid_y - row_centroid_y) < tolerance:
                row_contour.append(contour)
                row_found = True
                break

        # If no row found, create a new row with the current contour
        if not row_found:
            row_contours.append([contour])

    return row_contours

# Example usage:
row_contours = find_contours_in_same_row(filtered_contours)

# Draw the contours of each row with different colors
output_image = imageo.copy()
for i, row_contour in enumerate(row_contours):
    color = (np.random.randint(0, 256), np.random.randint(0, 256), np.random.randint(0, 256))
    for contour in row_contour:
        cv2.drawContours(output_image, [contour], -1, color, 2)

# Save the original image with the contours grouped by rows
cv2.imwrite("Contours Grouped by Rows.jpg", output_image)


# Function to find contours in the same row
def find_contours_in_same_row(contours):
    row_contours = []

    for contour in contours:
        x, y, w, h = cv2.boundingRect(contour)
        centroid_y = y + h // 2

        # Check if there are any existing rows to group the contour
        row_found = False
        for row_contour in row_contours:
            # Find the y-coordinate of the row's centroid
            row_x, row_y, row_w, row_h = cv2.boundingRect(row_contour[0])
            row_centroid_y = row_y + row_h // 2

            # Define a tolerance for grouping contours in the same row
            tolerance = 100

            # If the contour's centroid is close to the row's centroid, add it to the row
            if abs(centroid_y - row_centroid_y) < tolerance:
                row_contour.append(contour)
                row_found = True
                break

        # If no row found, create a new row with the current contour
        if not row_found:
            row_contours.append([contour])

    return row_contours

# Example usage:
row_contours = find_contours_in_same_row(filtered_contours)

# Draw the contours of each row with different colors
output_image = imageo.copy()
for i, row_contour in enumerate(row_contours):
    color = (np.random.randint(0, 256), np.random.randint(0, 256), np.random.randint(0, 256))
    for contour in row_contour:
        cv2.drawContours(output_image, [contour], -1, color, 2)

# Save the original image with the contours grouped by rows
cv2.imwrite("Contours Grouped by Rows.jpg", output_image)

# Write the contour information to a .csv file
with open("contour_data_with_row_ids.csv", "w", newline="") as csvfile:
    csv_writer = csv.writer(csvfile)
    for i, row_contour in enumerate(row_contours):
        for contour in row_contour:
            x, y, w, h = cv2.boundingRect(contour)
            avg_height = h / len(row_contour)
            csv_writer.writerow([i+1, x, avg_height])


# Function to check if two contours have horizontal overlap
def check_horizontal_overlap(contour1, contour2):
    x1, _, w1, _ = cv2.boundingRect(contour1)
    x2, _, w2, _ = cv2.boundingRect(contour2)
    return abs(x1 - x2) < min(w1, w2) * 0.75


# Function to find the widest contour in a list
def find_widest_contour(contours):
    widest_contour = None
    max_width = 0
    for contour in contours:
        _, _, w, _ = cv2.boundingRect(contour)
        if w > max_width:
            max_width = w
            widest_contour = contour
    return widest_contour


# Function to calculate the model fit AIC for a set of points and a given regression model
def calculate_aic(x, y, model):
    slope, intercept, _, _, _ = linregress(x, y)
    y_pred = [slope * xi + intercept for xi in x]
    residuals = np.subtract(y, y_pred)
    sse = np.sum(residuals ** 2)
    n = len(x)
    k = 2  # Number of parameters in the quadratic regression model
    aic = n * np.log(sse / n) + 2 * k
    return aic


# Example usage:

# Assuming you already have the 'row_contours' list containing the contours grouped into rows
# Let's process the first two rows:

if len(row_contours) >= 2:
    # Process the first row
    original_row1 = row_contours[0]
    new_row1 = [find_widest_contour(original_row1)]

    # Find non-overlapping contours for new row 1
    for contour in original_row1:
        if contour not in new_row1:
            if all(not check_horizontal_overlap(contour, c) for c in new_row1):
                new_row1.append(contour)

    # If there are no other non-overlapping contours, original row 1 is the new row 1
    if len(new_row1) == 1:
        row1_contours = new_row1
        row2_contours = row_contours[1]
    else:
        # Calculate model fit AIC for all possible combinations of contours in new row 1
        best_row1_combination = new_row1
        best_aic = float('inf')
        for r in range(2, len(new_row1) + 1):
            for combination in combinations(new_row1, r):
                x_positions = [cv2.boundingRect(c)[0] for c in combination]
                y_positions = [cv2.boundingRect(c)[1] for c in combination]
                aic = calculate_aic(x_positions, y_positions, 'quadratic')
                if aic < best_aic:
                    best_aic = aic
                    best_row1_combination = combination

        row1_contours = list(best_row1_combination)
        row2_contours = [c for c in original_row1 if c not in row1_contours]

    # Process the second row
    original_row2 = row_contours[1]
    row2_contours.extend(original_row2)

# Now you have the contours assigned to the first two rows (row1_contours and row2_contours)
# You can repeat the above steps for more rows if needed.

def draw_colored_contours(image, row_contours):
    output_image = image.copy()
    for row_id, row in enumerate(row_contours):
        color = (np.random.randint(0, 256), np.random.randint(0, 256), np.random.randint(0, 256))
        for contour in row:
            cv2.drawContours(output_image, [contour], -1, color, 2)
    return output_image

from sklearn.preprocessing import PolynomialFeatures
from sklearn.linear_model import RANSACRegressor

# Function to fit quadratic regression using RANSAC
def fit_quadratic_regression_ransac(x, y):
    x = x.reshape(-1, 1)
    y = y.reshape(-1, 1)

    model = make_pipeline(PolynomialFeatures(2), RANSACRegressor(residual_threshold=5))
    model.fit(x, y)

    return model


# Example usage:
row_contours = find_contours_in_same_row(filtered_contours)

# Create a CSV file to save the contour data with row IDs, X position, Y position, and predicted Y
with open('contour_data_with_row_ids.csv', 'w', newline='') as csvfile:
    csv_writer = csv.writer(csvfile)
    csv_writer.writerow(['Row ID', 'X Position', 'Y Position', 'Predicted Y'])

    for row_id, row_contour in enumerate(row_contours, 1):
        x_positions = []
        y_positions = []
        for contour in row_contour:
            x, _, _, _ = cv2.boundingRect(contour)
            x_positions.append(x)

            # Get the mean vertical position for each column within the contour
            y_positions.append(calculate_mean_pixel_vertical_position_in_contour(contour, image))

        # Apply quadratic regression using RANSAC to predict y-values from x-values
        x = np.array(x_positions)
        y = np.array(y_positions)
        print(x)
        print(f'afa')
        print(y)
        print(f'YYY')
        model = fit_quadratic_regression_ransac(x, y)
        y_pred = model.predict(x.reshape(-1, 1))
        print(y_pred)
        print(f'ZZ')
        # Write row data to the CSV file
        for x, y, y_pred_value in zip(x_positions, y_positions, y_pred):
            csv_writer.writerow([row_id, x, y, y_pred_value])

# Draw the contours of each row with different colors
output_image = image.copy()
for i, row_contour in enumerate(row_contours):
    color = (np.random.randint(0, 256), np.random.randint(0, 256), np.random.randint(0, 256))
    for contour in row_contour:
        cv2.drawContours(output_image, [contour], -1, color, 2)

# Save the original image with the contours grouped by rows
cv2.imwrite("Contours Grouped by Rows.jpg", output_image)

import cv2
import numpy as np
import matplotlib.pyplot as plt
import csv

def calculate_mean_pixel_vertical_position_in_contour(contour, image):
    # Your implementation for calculate_mean_pixel_vertical_position_in_contour
    pass

def find_contours_in_same_row(contours):
    row_contours = []

    for contour in contours:
        x, y, w, h = cv2.boundingRect(contour)
        centroid_y = y + h // 2

        # Check if there are any existing rows to group the contour
        row_found = False
        for row_contour in row_contours:
            # Find the y-coordinate of the row's centroid
            row_x, row_y, row_w, row_h = cv2.boundingRect(row_contour[0])
            row_centroid_y = row_y + row_h // 2

            # Define a tolerance for grouping contours in the same row
            tolerance = 100

            # If the contour's centroid is close to the row's centroid, add it to the row
            if abs(centroid_y - row_centroid_y) < tolerance:
                row_contour.append(contour)
                row_found = True
                break

        # If no row found, create a new row with the current contour
        if not row_found:
            row_contours.append([contour])

    return row_contours

def process_contours(filtered_contours, image):
    all_r_positions = []
    all_c_positions = []

    for contour in filtered_contours:
        cv2.drawContours(filled_contour_image, [contour], 0, (0, 255, 255), thickness=cv2.FILLED)

        # Get the mean vertical position for each column within the contour
        r_positions = calculate_mean_pixel_vertical_position_in_contour(contour, image)

        # Find the minimum x-coordinate of the contour
        min_x = np.min(contour[:, 0, 0])

        # Create x-axis values relative to the contour's position
        x_values = np.arange(len(r_positions)) + min_x

        # Plot the curve for the current contour on top of the filled contour image
        plt.plot(x_values, r_positions, color='red', linewidth=1)

        # Append the r_positions to the all_r_positions list
        all_r_positions.extend(r_positions)
        c_positions = x_values
        all_c_positions.extend(c_positions)

        # Apply quadratic regression to predict y-values (r_positions) from x-values (c_positions)
        x = np.array(c_positions)
        y = np.array(r_positions)

        # Fit a quadratic polynomial to the data
        z = np.polyfit(x, y, 2)

        # Create a function to predict y-values using the quadratic polynomial coefficients
        quadratic_function = np.poly1d(z)

        # Predict y-values using the quadratic function
        y_pred = quadratic_function(x)

        # Append the predicted y-values (y_pred) as the 3rd column to the .csv file
        with open('Cobmined output.csv', 'a', newline='') as csvfile:
            csv_writer = csv.writer(csvfile)
            for i, pred in enumerate(y_pred):
                csv_writer.writerow([c_positions[i], r_positions[i], pred])

    plt.show()  # Show the plotted curves for all contours

# Assuming you have loaded the 'filled_contour_image', 'filtered_contours', and 'image' variables
process_contours(filtered_contours, image)




# Create a CSV file to save the contour data with row IDs, X position, Y position, and predicted Y
with open('contour_data_with_row_ids.csv', 'w', newline='') as csvfile:
    csv_writer = csv.writer(csvfile)
    csv_writer.writerow(['Row ID', 'X Position', 'Y Position', 'Predicted Y'])

    for row_id, row_contour in enumerate(row_contours, 1):
        x_positions = []
        y_positions = []
        for contour in row_contour:
            x, y, w, h = cv2.boundingRect(contour)
            x_positions.append(range(x, x + w))  # Use x instead of x+w

            # Get the mean vertical position for each column within the contour
            y_positions.append(calculate_mean_pixel_vertical_position_in_contour(contour, image))

        # Apply quadratic regression using RANSAC to predict y-values from x-values
        x = np.array(x_positions)
        y = np.array(y_positions)

        model = fit_quadratic_regression_ransac(x, y)
        y_pred = model.predict(x.reshape(-1, 1))

        # Write row data to the CSV file
        for x, y, y_pred_value in zip(x_positions, y_positions, y_pred):
            csv_writer.writerow([row_id, x, y, y_pred_value])
