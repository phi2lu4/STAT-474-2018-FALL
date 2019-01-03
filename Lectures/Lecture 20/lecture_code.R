# Loading the necessary package
library(sparklyr)
library(dplyr)
library(DBI)

# Get started with a Spark session

## Configurate the Spark parameter
conf <- spark_config()
conf$`sparklyr.shell.driver-memory` <- "512M" 
conf$spark.memory.fraction <- 0.8 

## Start a Spark session
sc <- spark_connect(master = "local", config = conf, version = "2.3.1")

# If you don't remember the version, you can run the following
spark_installed_versions()

# We download the On-time Flight Performance DataSet
if(!file.exists("2008.csv.bz2"))
{download.file("http://stat-computing.org/dataexpo/2009/2008.csv.bz2", "2008.csv.bz2")}
if(!file.exists("2007.csv.bz2"))
{download.file("http://stat-computing.org/dataexpo/2009/2007.csv.bz2", "2007.csv.bz2")}

# We will work with 2008 flights data first
spark_read_csv(sc, "flights_spark_2008", "2008.csv.bz2", memory = FALSE)

# Checkpoint: View the Spark Web UI and comment.

# Get the handler for the data in Spark
sp_flights <- tbl(sc, "flight_spark_2008")

# Take a look at the size of the object
object.size(sp_flights)

# Use SQL to access and query data in Spark
dbGetQuery(sc, "SELECT * FROM flights_spark_2008 LIMIT 10");

# With dplyr
sp_flights %>% head(10)

# Under the hood:
sp_flights %>% head(10) %>% show_query()

# Checkpoint: using SQL, Extract the first 5 flights from Atlanta (`Origin == "ATL"`). What are those destinations?

# Another example
flights_table <- sp_flights %>%
  mutate(DepDelay = as.numeric(DepDelay),
         ArrDelay = as.numeric(ArrDelay),
         DepDelay > 15 , DepDelay < 240,
         ArrDelay > -60 , ArrDelay < 360, 
         Gain = DepDelay - ArrDelay) %>%
  filter(ArrDelay > 0) %>%
  select(Origin, Dest, UniqueCarrier, Distance, DepDelay, ArrDelay, Gain)

flights_table %>% show_query()

# Question: What does the above query do?

# Understanding caching
start_time <- Sys.time()
flights_table %>% tally()
end_time <- Sys.time()

end_time - start_time

# Combining data from both 2007 and 2008 flights
spark_read_csv(sc, "flights_spark_2007" , "2007.csv.bz2", memory = FALSE)
all_flights <- tbl(sc, "flights_spark_2008") %>%
  union(tbl(sc, "flights_spark_2007")) %>%
  group_by(Year, Month) %>%
  tally()

# Checkpoint: Are there more flights in 2008 than in 2007?


# Modeling in Spark
mtcars_tbl <- copy_to(sc, mtcars, "mtcars", overwrite = TRUE)

# To build the predictive model, one split data set into training and test sets
# Use a different seed number to get different partition
partitions <- mtcars_tbl %>%
  filter(hp >= 100) %>%
  mutate(cyl8 = cyl == 8) %>%
  sdf_partition(training = 0.5, test = 0.5, seed = 1099)

partitions %>% head()

# Build a linear regression model based on training set
fit <- partitions$training %>%
  ml_linear_regression(mpg ~ wt + cyl)

summary(fit)

# Evaluate the model using the test set
pred <- partitions$test %>% sdf_predict(fit)
pred %>% summarise(mse = mean((mpg - prediction)^2))

# Checkpoint: When spliting the data 70/30 for training and test set, what is the predictive mean square error?

