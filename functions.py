import cv2
from imutils.perspective import four_point_transform
import numpy as np
import pytesseract
import matplotlib as plt
import imageio
import matplotlib.pyplot as plt

def displayf(im_path):
    dpi = 80
    im_data = imageio.imread(im_path)

    height, width = im_data.shape[:2]

    # What size does the figure need to be in inches to fit the image?
    figsize = width/2.5 / float(dpi), height / float(dpi)/1.2

    # Create a figure of the right size with one axes that takes up the full figure
    fig = plt.figure(figsize=figsize)
    ax = fig.add_axes([0, 0, 1, 1])

    # Hide spines, ticks, etc.
    ax.axis('off')

    # Display the image.
    ax.imshow(im_data, cmap='gray')

    plt.show()

def display(im_data):
    dpi = 80
    height, width = im_data.shape[:2]

    # What size does the figure need to be in inches to fit the image?
    figsize = width/2.5 / float(dpi), height / float(dpi)/1.2

    # Create a figure of the right size with one axes that takes up the full figure
    fig = plt.figure(figsize=figsize)
    ax = fig.add_axes([0, 0, 1, 1])

    # Hide spines, ticks, etc.
    ax.axis('off')

    # Display the image.
    ax.imshow(im_data, cmap='gray')

    plt.show()

def contour_image(image_name, backg=10):
    approxa = []
    border_thickness = 1
    image0 = cv2.imread(image_name)  # Read the image

    height, width, channels = image0.shape
    # Calculate the number of pixels
    num_pixels = height * width

    if num_pixels < 1000000:
        factor = 3
        new_size = (image0.shape[1] * factor, image0.shape[0] * factor)
        image = cv2.resize(image0, new_size, interpolation=cv2.INTER_CUBIC)
    else:
        image = image0
    image = cv2.copyMakeBorder(image, border_thickness, border_thickness, border_thickness, border_thickness,
                                cv2.BORDER_CONSTANT, value=(0, 0, 0))

    image_gray = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)

    height, width, channels = image.shape
    gnum_pixels = height * width

    adaptive_thresholded_image = cv2.adaptiveThreshold(image_gray, 255, cv2.ADAPTIVE_THRESH_GAUSSIAN_C,
                                                       cv2.THRESH_BINARY, 21, 0)
    adaptive_thresholded_image0 = cv2.adaptiveThreshold(image_gray, 255, cv2.ADAPTIVE_THRESH_GAUSSIAN_C,
                                                       cv2.THRESH_BINARY, 21, backg)

    contours, _ = cv2.findContours(adaptive_thresholded_image, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
    cnt1 = sorted(contours, key=cv2.contourArea, reverse=True)[0]
    image_with_contours = cv2.cvtColor(adaptive_thresholded_image, cv2.COLOR_GRAY2BGR)
    perimeter = cv2.arcLength(cnt1, True)
    epsilon = 0.02 * perimeter
    approxa = cv2.approxPolyDP(cnt1, epsilon, True)

    if cv2.contourArea(cnt1) >= gnum_pixels * 0.96:
        adaptive_thresholded_image = adaptive_thresholded_image[:adaptive_thresholded_image.shape[0]-5, :]
        contours, _ = cv2.findContours(adaptive_thresholded_image, cv2.RETR_EXTERNAL,
                                       cv2.CHAIN_APPROX_SIMPLE)
        cnt1 = sorted(contours, key=cv2.contourArea, reverse=True)[0]
        image_with_contours = cv2.cvtColor(adaptive_thresholded_image, cv2.COLOR_GRAY2BGR)

    cv2.drawContours(image_with_contours, contours, -1, (0, 255, 0), 2)
    cv2.drawContours(image_with_contours, [cnt1], -1, (0, 0, 255), 2)

    if cv2.contourArea(cnt1) >= gnum_pixels / 5:
        perimeter = cv2.arcLength(cnt1, True)
        epsilon = 0.02 * perimeter
        approxx = cv2.approxPolyDP(cnt1, epsilon, True)
        contoured = four_point_transform(adaptive_thresholded_image0, approxx.reshape(4, 2) * 1)
    else:
        contoured = adaptive_thresholded_image0
        approx = approxa

    f_contourred = "f_contourred.jpg"
    cv2.imwrite(f_contourred, contoured)  # Save the new image

    return contoured, f_contourred, approxx

def unsharp_mask(image, kernel_size=(5, 5), sigma=1.0, amount=1.0, threshold=0):
    """Return a sharpened version of the image, using an unsharp mask."""
    blurred = cv2.GaussianBlur(image, kernel_size, sigma)
    sharpened = float(amount + 1) * image - float(amount) * blurred
    sharpened = np.maximum(sharpened, np.zeros(sharpened.shape))
    sharpened = np.minimum(sharpened, 255 * np.ones(sharpened.shape))
    sharpened = sharpened.round().astype(np.uint8)
    if threshold > 0:
        low_contrast_mask = np.absolute(image - blurred) < threshold
        np.copyto(sharpened, image, where=low_contrast_mask)
    return sharpened

def fcontour_filter(image_path, subtraction=15):
    # Load the image
    image = cv2.imread("image1.jpg")
    height, width, channels = image.shape

    # Convert the image to grayscale
    gray = cv2.cvtColor(image, cv2.COLOR_RGB2GRAY)
    image = cv2.cvtColor(gray, cv2.COLOR_GRAY2RGB)

    black_pixel_counts = []

    thresholded_image = cv2.adaptiveThreshold(gray, 255, cv2.ADAPTIVE_THRESH_GAUSSIAN_C, cv2.THRESH_BINARY, 51,
                                              subtraction)
    _, thresholded_image2 = cv2.threshold(gray, 0, 255, cv2.THRESH_BINARY + cv2.THRESH_OTSU)

    # thresholded_image = cv2.bitwise_or(thresholded_image, thresholded_image2)

    kernel = cv2.getStructuringElement(cv2.MORPH_RECT, (1, 3))  # Adjust the kernel size as needed
    # Perform the opening operation
    thresholded_image = cv2.morphologyEx(thresholded_image, cv2.MORPH_OPEN, kernel)

    image = cv2.cvtColor(thresholded_image, cv2.COLOR_GRAY2RGB)
    # Perform morphological dilation
    kernel_size = (2, 1)  # (Width, Height)

    # Create the custom kernel
    kernel = cv2.getStructuringElement(cv2.MORPH_RECT, kernel_size)

    # Perform one-sided dilation with the recalculated weighted center
    dilated = 255 - image.copy()  # Create a copy of the original image
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
    contour_image = image.copy()
    cv2.drawContours(contour_image, filtered_contours, -1, (0, 0, 255), 2)

    # Load the image and other preprocessing steps...
    # (Your previous code up to the point where you have the filtered_contours)

    # Create a blank image to hold the filled contours
    filled_contour_image = np.zeros_like(contour_image)

    # Fill the contours with yellow color
    for contour in filtered_contours:
        cv2.drawContours(filled_contour_image, [contour], 0, (0, 255, 255), thickness=cv2.FILLED)

    return contour_image, filled_contour_image, image

