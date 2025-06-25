# Predict the temperature of Szeged, Hungary based on the humidity, wind speed, etc

# List of packages
packages <- c("readxl", "dplyr", "plyr", "PerformanceAnalytics", "reshape2", 
              "arules", "lessR", "rpart", "rpart.plot", "caret", "e1071", 
              "plot3D", "leaps", "caTools")

# Install packages that are not yet installed
installed_packages <- rownames(installed.packages())
for (pkg in packages) {
  if (!(pkg %in% installed_packages)) {
    install.packages(pkg)
  }
}

# Load the packages
lapply(packages, library, character.only = TRUE)

# Read the weather data
data = read_excel("D:\\IVY\\STATS R\\Regression\\Regression Assignment\\weatherHistory.xlsx")

# Basic Exploration of the data
str(data)
summary(data)
dim(data)

#################################################################
# Unique Value Checking and Variable Segregation
#################################################################

# Calculate unique values for each column
unique_counts <- sapply(data, function(x) length(unique(x)))
print(unique_counts)


# Define a threshold to distinguish categorical from continuous variables
# (e.g., if a column has <= 10 unique values, we consider it categorical)
threshold <- 10

# Segregate categorical and continuous variables based on unique counts
categorical_vars <- names(unique_counts[unique_counts <= threshold])
continuous_vars <- names(unique_counts[unique_counts > threshold])

# Print out the variables for each category
print(categorical_vars)
print(continuous_vars)

# Drop unused columns
data = data[, !(colnames(data) %in% c("Loud Cover", "Apparent Temperature (C)"))]

#################################################################
# Missing Value Checking
#################################################################

# Missing values Identification and Treatment
sapply(data,function(x)sum(is.na(x)))#One way to check

as.data.frame(colSums(is.na(data)))#Other way

#################################################################
# Exploratory Data Analysis (EDA)
#################################################################
# We see "Summary" and "Precip Type", "Daily Summary" are categorical variables

precip_counts <- table(data$"Precip Type")
pie(precip_counts, main = "Precipitation Type Distribution", labels = paste(names(precip_counts), round(100 * prop.table(precip_counts), 1), "%"))
# we see there is null value it might be missing value we need to treat it later.

precip_counts <- table(data$"Summary")
pie(precip_counts, main = "Precipitation Type Distribution", labels = paste(names(precip_counts), round(100 * prop.table(precip_counts), 1), "%"))

# Bar plot for better readability
barplot(precip_counts, 
        main = "Precipitation Type Distribution", 
        horiz = TRUE, 
        col = colors, 
        las = 1,         # Rotate labels to be horizontal
        cex.names = 0.7) # Adjust label size

# Bar plot of summary vs average temperature
dataNew <- select(data, c(Summary,"Temperature (C)")) %>% 
  group_by(Summary) %>%
  summarise_at("Temperature (C)", funs(mean(., na.rm=TRUE)))

# Create the bar plot
barplot(dataNew$"Temperature (C)", names.arg = dataNew$Summary, horiz = TRUE,
        xlab = "Temperature (C)", ylab = "Summary", col = "orange", 
        main = "Summary vs Temperature",las = 1, cex.names = 0.7)


# Drop the rows with null class label in column Precip Type
data = data[data$"Precip Type" != 'null', ]

# Map categorical variable in Precip Type to numerical value, 1 = rain, 2 = snow
data$"Precip Type" <- mapvalues(data$"Precip Type", 
                                from=c("rain","snow"), 
                                to=c(1, 2))

# Map categorical variable in Summary to numerical value
data$Summary <- mapvalues(data$Summary, 
                          from=as.vector(unique(data$Summary)), 
                          to=seq(1, length(as.vector(unique(data$Summary))), 1))

# Or you can use this way to get 1=clear, 2=partly cloudy, 3=mostly cloudy, 4=overcast, 5=foggy, 6=others
#data$Processed.Summary <- mapvalues(data$Summary, 
#                                    from=seq(1, length(as.vector(unique(data$Summary))), 1), 
#                                    to=c(2, 3, 4, 5, 3, 1, 3, 4, 3, 2, 5, 4, 5, 2, 6, 2, 3, 2, 6, 6, 4, 6, 6, 6, 3, 6, 6))


# Map categorical variable in Daily Summary to numerical value
data$"Daily Summary" <- mapvalues(data$"Daily Summary", 
                                  from=as.vector(unique(data$"Daily Summary")), 
                                  to=seq(1, length(as.vector(unique(data$"Daily Summary"))), 1))

# Convert variables to numeric variables
columns = c("Precip Type","Summary","Daily Summary")
for (column in columns) {
  data[[column]] = as.numeric(as.character(data[[column]]))
}

#################################################################
# Statistical Test
#################################################################

# Perform factorial ANOVA with multiple categorical variables
anova_result <- aov(`Temperature (C)` ~ `Precip Type` + `Summary` + `Daily Summary`, data = data)

# Display summary of the ANOVA results
summary(anova_result)

# In ANOVA results, the significance symbols (*, **, ***) next to the p-values indicate the level of statistical significance for each factor.

#################################################################
# Continuous Column
#################################################################

names(data)
conti_column=c("Humidity", "Wind Speed (km/h)", "Wind Bearing (degrees)", "Visibility (km)", "Pressure (millibars)")

# Plot histograms for each column
par(mfrow = c(2, 3))  # Arrange plots in a grid
for (col in conti_column) {
  hist(data[[col]], main = paste("Histogram of", col), xlab = col, breaks = 30)
}
par(mfrow = c(1, 1))  # Reset layout

# Distribution plot
par(mfrow = c(2, 3))
for (col in conti_column) {
  plot(density(data[[col]], na.rm = TRUE), main = paste("Density of", col), xlab = col)
}
par(mfrow = c(1, 1))

# Check boxplots on continuous columns to see any outliers present or not
par(mfrow = c(2, 3))
for (col in conti_column) {
  boxplot(data[[col]], main = paste("Boxplot of", col), xlab = col)
}
par(mfrow = c(1, 1))

# Remove outliers from Pressure
data<-data[data$`Pressure (millibars)` > 800, ]
data<-data[data$"Humidity" > .01, ]
data<-data[data$"Wind Speed (km/h)" <50, ]

# Scatter plots
par(mfrow = c(2, 3))
for (col in conti_column) {
  plot(data[[col]], data$`Temperature (C)`, 
       main = paste(col, "vs Temperature (C)"), 
       xlab = col, ylab = "Temperature (C)", 
       pch = 16, col = "blue")
}
par(mfrow = c(1, 1))

#################################################################
# Feature Engineering
#################################################################

# Extract day
data$Day = as.numeric(sapply(data$"Formatted Date", substring, 9, 10))

# Extract month
data$Month = as.numeric(sapply(data$"Formatted Date", substring, 6, 7))

# Extract hour
data$Hour = as.numeric(sapply(data$"Formatted Date", substring, 12, 13))

# Cyclical variables, like day, month, and hour, are special because they wrap around (e.g., 24 hours, 12 months). Directly scaling these variables can be misleading, as the distance between, for example, 23 hours and 0 hours is actually small, even though numerically they are far apart.

# To properly handle cyclical variables, we can transform them into sine and cosine components. This approach allows the model to understand the cyclical nature of these variables, capturing both the period and continuity of the cycle.

# Transform cyclical variables
data <- data %>%
  mutate(
    # Day (assuming it has a cycle of 31 for maximum days in a month)
    Day_sin = sin(2 * pi * Day / 31),
    Day_cos = cos(2 * pi * Day / 31),
    
    # Month (12 months cycle)
    Month_sin = sin(2 * pi * Month / 12),
    Month_cos = cos(2 * pi * Month / 12),
    
    # Hour (24 hours cycle)
    Hour_sin = sin(2 * pi * Hour / 24),
    Hour_cos = cos(2 * pi * Hour / 24)
  )

# Drop unused columns
data = data[, !(colnames(data) %in% c("Formatted Date", "Day", "Month", "Hour"))]
names(data)
#################################################################
# Statistical Test
#################################################################
corr_columns<-c("Wind Speed (km/h)","Wind Bearing (degrees)","Visibility (km)","Pressure (millibars)","Day_sin", "Day_cos", "Month_sin", "Month_cos","Hour_sin")
# Correlation matrix for all selected columns
cor_matrix <- cor(data[corr_columns], method = "spearman")

# Reshape the correlation matrix into long format
cor_matrix_melted <- melt(cor_matrix)

# Plot the heatmap with ggplot2, including annotations
ggplot(cor_matrix_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), 
                       name = "Spearman\nCorrelation") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 4) +  # Annotate correlation values
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 10, hjust = 1)) +
  labs(title = "Annotated Correlation Heatmap") +
  coord_fixed()

names(data)
# Standardize all columns except response column & columns with categorical
excluding_columns = c("Precip Type","Day_sin","Day_cos","Month_sin","Month_cos","Hour_sin","Hour_cos","Temperature (C)")
tmp = as.data.frame(scale(data[, !names(data) %in% excluding_columns]))
tmp[excluding_columns] = data[excluding_columns]
data1 = tmp


# Splitting the data into training and test data set

set.seed(123)#This is used to produce reproducible results, everytime we run the model

spl = sample.split(data1$"Temperature (C)", 0.75)#Splits the overall data into train and test data in 75:25 ratio

original.data = subset(data1, spl == TRUE)
str(original.data)
dim(original.data)

test.data = subset(data1, spl == FALSE)
str(test.data)
dim(test.data)

# Fitting the model
model <- lm(`Temperature (C)`~.,data=original.data)#'.', refers to all variables
summary(model)

model <- lm(`Temperature (C)` ~ `Precip Type`+`Humidity`+`Wind Speed (km/h)`+
              `Wind Bearing (degrees)`+`Visibility (km)`+`Pressure (millibars)`+
              `Daily Summary`+`Day_sin`+`Day_cos`+`Month_sin`+`Month_cos`+
              `Hour_sin`+`Hour_cos`, 
            data = original.data)

summary(model)

#Checking Multicollinearity in the model
## Get the predicted or fitted values
library("car")
vif(model)

model <- lm(`Temperature (C)` ~ `Precip Type`+`Wind Speed (km/h)`+
              `Wind Bearing (degrees)`+`Visibility (km)`+`Pressure (millibars)`+
              `Daily Summary`+`Day_sin`+`Day_cos`+`Month_sin`+`Month_cos`+
              `Hour_sin`+`Hour_cos`, 
            data = original.data)

summary(model)

model <- lm(`Temperature (C)` ~ `Precip Type`+
              `Wind Bearing (degrees)`+`Visibility (km)`+`Pressure (millibars)`+
              `Daily Summary`+`Day_sin`+`Day_cos`+`Month_sin`+`Month_cos`+
              `Hour_sin`+`Hour_cos`, 
            data = original.data)

summary(model)

vif(model)

## Get the predicted or fitted values
fitted(model)
par(mfrow=c(2,2))
plot(model)

## MAPE
original.data$pred <- fitted(model)

#Calculating MAPE on training data
library(MLmetrics)
MAPE(original.data$`Temperature (C)`,original.data$pred)

# Checking AIC and BIC on training data
AIC(model)  
BIC(model) 

# Checking Autocorrelation
durbinWatsonTest(model)

# Finally Predict on test data
test.data$pred <-  predict(model,test.data[,-15])

# Checking MAPE on test data
MAPE(test.data$`Temperature (C)`,test.data$pred)

# Checking R2, RMSE and MAE on test data
data.frame(
  R2 = R2(test.data$pred, test.data$`Temperature (C)`),
  RMSE = RMSE(test.data$pred, test.data$`Temperature (C)`),
  MAE = MAE(test.data$pred, test.data$`Temperature (C)`)
)

