# importing R libraries
library(DataComputing)

# importing data from local source
data <- read.csv("/Users/drpalan/Desktop/Subu/Berkeley-Crime/Data/raw_data.csv")

# remove unnecessary variables
data_trimmed <- data %>%
  select(date = EVENTDT, day = CVDOW, time = EVENTTM, type = OFFENSE, location = Block_Location, address = BLKADDR) 

# duplicate trimmed data table
data_clean <- data_trimmed

# convert "date" variable into Date format
data_clean$date <- as.Date(mdy(strtrim(data_clean$date, 10)))

# convert "day" variable into factor
data_clean$day <- as.factor(data_clean$day)
levels(data_clean$day) <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

# convert "time" variable into a continuous numeric variable (e.g. 6.30 pm -> 18.5)
data_clean$time <- as.numeric(hour(as.POSIXct(data_clean$time, format = "%H:%M")) + minute(as.POSIXct(data_clean$time, format = "%H:%M")) / 60)

# remove cases without coordinates for location of the crime
data_clean <- data_clean[(grepl("[0-9]+\\.[0-9]+\\, -[0-9]+\\.[0-9]+", data_clean$location)),]

# extract coordinates of the crime from the "location" variable, storing them into two new variables, "lat" and "long"
coord <- unlist(str_extract_all(data_clean$location, "([0-9]+\\.[0-9]+)\\, -[0-9]+\\.[0-9]+"))
coord <- strsplit(coord, ", ")

l <- length(coord)
lat <- vector(length = l)
long <- vector(length = l)
coord <- unlist(coord)

lat[1:l] <- coord[c(TRUE, FALSE)]
long[1:l] <- coord[c(FALSE, TRUE)]

data_clean$lat <- as.numeric(lat)
data_clean$long <- as.numeric(long)
data_clean$location <- NULL

# mutate "wday" variable to specify part of week for each day
data_clean <- data_clean %>%
  mutate(wday = ifelse(day %in% c("Sunday", "Saturday"), "weekend", "weekday"))

# removing cases of crimes involving non-threatening crimes
data_clean <- data_clean[!(data_clean$type %in% c("DOMESTIC VIOLENCE", "MISSING ADULT", "MISSING JUVENILE", "MUNICIPAL CODE", "VEHICLE RECOVERED")),]
data_clean$type <- as.factor(data_clean$type)

# assign severity ratings to type of crimes
types_of_crime <- data_clean %>%
  group_by(type) %>%
  summarise(count = n())
types_of_crime <- types_of_crime$type

severity <- as.factor(c(1, 1, 4, 5, 4, 2, 3, 2, 4, 1, 2, 1, 3, 1, 5, 2, 4, 5, 3, 5, 4, 3, 2, 3, 4, 3))
severity_of_crimes <- data.frame(types_of_crime, severity)

data_clean <- data_clean %>%
  inner_join(severity_of_crimes, by = c("type" = "types_of_crime"))

# group the location of each crime into one of North, South, East or West Berkeley
data_clean <- data_clean %>%
  mutate(x = long + 122.2727, y = lat - 37.8716)
x <- data_clean$x
y <- data_clean$y

area <- vector(length = length(x))

area[y >= 0 & -y < x & x <= y] <- "N"
area[y < 0 & y <= x & x < -y] <- "S"
area[x < 0 & x < y & y <= -x] <- "W"
area[x >= 0 & -x <= y & y < x] <- "E"

data_clean$area <- as.factor(area)
data_clean$x <- NULL
data_clean$y <- NULL

# save cleaned data file locally
write.csv(data_clean, "/Users/drpalan/Desktop/Subu/Berkeley-Crime/Data/clean_data.csv")
