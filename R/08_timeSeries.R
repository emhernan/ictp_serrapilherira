library(dplyr)
library(ggplot2)
library(lubridate)
library(zoo) ## library to work with time-series data


covid <- read.csv("data/raw/covid19-dd7bc8e57412439098d9b25129ae6f35.csv")

# First checking the class
class(covid$date)


# Changing to date format
covid$date <- as_date(covid$date)
# Checking the class
class(covid$date)


# Now we can make numeric operations
range(covid$date)

# First, we will create a column containing the number of new cases.
ggplot(covid) +
  geom_line(aes(x = date, y = new_confirmed)) +
  theme_minimal()


# We have negative cases and will substitute the negative values per zero.
covid$new_confirmed[covid$new_confirmed < 0] <- 0 

ggplot(covid) +
  geom_line(aes(x = date, y = new_confirmed)) +
  theme_minimal() +
  scale_x_date(breaks = "4 months", date_labels = "%Y-%m") + # Change how I display the date in my dataset
  labs(x = "Date", y = "New cases")

# Rolling mean

covid$roll_mean <- zoo::rollmean(covid$new_confirmed, 14, fill = NA) 
head(covid)

ggplot(covid) +
  geom_line(aes(x = date, y = new_confirmed)) +
  theme_minimal() +
  scale_x_date(breaks = "4 months", date_labels = "%Y-%m") + # Change how I display the date in my dataset
  labs(x = "Date", y = "New cases") +
  geom_line(aes(x = date, y = roll_mean), col = "red", size = 1.2)

