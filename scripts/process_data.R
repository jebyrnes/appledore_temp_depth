library(readxl)
library(dplyr)
library(lubridate)
library(readr)

#get cast data
casts <- read_excel("data/07_16_22_Casts.csv.xlsx") %>%
  mutate(Date_Time = as.Date(`Date & Time`,origin = '1899-12-30',
                             tz = "EST") |> as.POSIXct()) |>
  filter(`Depth(m)`>=0) %>%
  
  #at a turnaround, we are descending (d - lag(d) is positive)
  # after ascending (d - lag(d) is negative)
  mutate(delta_depth = ifelse(`Depth(m)` - lag(`Depth(m)`) < 0 &
                                lead(`Depth(m)`) - `Depth(m)`  >0 &
                                `Depth(m)` < 1,
                                               1, 
                                               0),
         delta_depth = ifelse(is.na(delta_depth), 1, delta_depth), 
         id = cumsum(delta_depth)) %>%
  group_by(id) %>%
  mutate(updown = ifelse(`Depth(m)` < max(`Depth(m)`) &
                           row_number() < which(`Depth(m)`== max(`Depth(m)`))[1],
                         "down",
                         "up")) %>%
  ungroup()

#Check yourself before you wreck yourself
# plot(casts$`Depth(m)`)
# matplot(casts$delta_depth*5, col = "red", add = TRUE, pch = 1)
# plot(casts$`Temp(°C)`)
# 
# ggplot2::ggplot(casts, ggplot2::aes(x = Date_Time, y = `Depth(m)`,
#                   color = as.factor(id))) +
#   ggplot2::geom_point()

#a function to turn different columns into one datetime object
combine_datetime <- function(dateobj, timenum){
  timenum <- gsub("(\\d\\d)(\\d\\d)(\\d\\d)", "\\1:\\2:\\3", timenum)
  as.POSIXct(paste(dateobj, timenum),  # Add hours, minutes & seconds
   format = "%Y-%m-%d %H:%M:%S")
}

#read in and fix up location
loc <- readr::read_csv("data/20220716_AppledoreTempCasts2.csv") %>%
  mutate(Tdeploy = combine_datetime(Date, Tdeploy),
         Tretrieve = combine_datetime(Date, Tretrieve)
  )


joined_dat <- right_join(loc, casts)

write_csv(joined_dat, "data/casts_with_loc.csv")
