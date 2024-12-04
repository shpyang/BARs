 par(mar=c(3,3,3,3))
 
# Load the library
library(eulerr)
 library(grid)

# Define values
fit <- euler(c(
  "BMI_Obesity" = 26070,
  "Abdominal_obesity" = 64932,
  "BMI_Obesity&Abdominal_obesity" = 73264
))

# Total population
total_population <- 239193  # Adjusted for overlap

# Calculate percentages
percentages <- c(
  "BMI\n Obesity\n Only" = (26070) / total_population * 100,
  "Abdominal\n Obesity\n Only" = (64932) / total_population * 100,
  "Both" = 73264 / total_population * 100 
)

# Add labels with percentages
labels <- paste0(names(percentages), "\n", round(percentages, 1), "%")

# Create the Venn diagram
plot(fit,
     fills = list(fill = c("red", "purple" ), alpha = 0.5),
     labels = list(
       labels = labels,
       col = "blue",
       font = 220, cex=2
     ),
#     main = "Venn Diagram of Obesity and Abdominal Obesity"
     main = ""
)

grid.text(
  label = "Both (30.6%)",
  x = 0.46,  # Adjust as needed
  y = 0.5,  # Adjust as needed
  gp = gpar(fontsize = 14, col = "darkred", font = 220, cex=2)
)

