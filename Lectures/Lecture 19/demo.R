# Load the necessary libraries
library(sparklyr)
library(dplyr)
library(ggplot2)

# Initiate the Spark session

# Setting for spark
Sys.setenv(SPARK_HOME="/usr/lib/spark")
config <- spark_config()

# Make a connection. It takes up to a few minutes to make a connection
sc <- spark_connect(master = "yarn-client", config = config, version = '2.3.1')

# We will work with a small data set `mtcars`
mtcars_tbl <- copy_to(sc, mtcars, "mtcars", overwrite = TRUE)

# Check the availability of data sets in Spark
src_tbls(sc)

# transform our data set, and then partition into 'training', 'test'
partitions <- mtcars_tbl %>%
  filter(hp >= 100) %>%
  mutate(cyl8 = cyl == 8) %>%
  sdf_partition(training = 0.5, test = 0.5, seed = 1099)

# Show the partition
partitions %>%head()

# Using SQL
library(DBI)
mtcars_review <- dbGetQuery(sc, "SELECT * FROM mtcars LIMIT 10")
mtcars_review

# fit a linear model to the training dataset
fit <- partitions$training %>%
  ml_linear_regression(response = "mpg", features = c("wt", "cyl"))

# Query the linear regression model
summary(fit)
