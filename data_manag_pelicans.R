library(readxl)
library(tidyverse)

# combine all pelican data sheets into one
file.list <- list.files(pattern = '*.xlsx', recursive = TRUE)
df.list <- lapply(file.list, read_excel)
pelicandata <- as.data.frame(bind_rows(df.list))


######### DATA CLEANING ######### 

# quick check to make sure no errors in FWC copy over
unique(pelicandata$SECT)
unique(pelicandata$TIME)
unique(pelicandata$MONTH)
unique(pelicandata$DAY)
unique(pelicandata$PERIOD)
unique(pelicandata$RESEARCHER)

check <- pelicandata %>%
  group_by(MARK) %>%
  summarise(count = n())
unique(check$count)
# should have 1 value (70)

check <- pelicandata %>%
  group_by(SECT) %>%
  summarise(count = n())
unique(check$count)
# should have 4 values (multiple of A: 26 ; B: 39; C: 35; D: 36)
# 1820 2730 2450 2520

check <- pelicandata %>%
  group_by(DATE, DAY) %>%
  summarise(count = n())
unique(check$count)
# ! 3/6/2024 was surveyed TWICE (morning and mid-day)
# ! 3/10/2024 was surveyed TWICE (morning and morning)
#     The second one was to "make up" for an event on March 3rd that closed the pier
#     We are NOT going to include it as it:
#       (1) Is not representative of the same times as prior morning surveys
#       (2) Cannot be considered an independent observation (high correlation)

# remove second morning survey on 3/10/2024 (row 6665-6800)
pelicandata <- pelicandata[-c(6665:6800),]

# Save the cleaned dataset to use for analyses
#write.csv(pelicandata, file = "data/pelicans/pelican_data.csv", row.names = FALSE)


######### FOR ANGLER COMPARISONS (only need to do once) ######### 

# summarize by date & pier section
totals_for_anglers1 <- pelicandata %>%
  group_by(DATE, SECT) %>%
  summarise(total_pelicans = sum(A_PIER) + sum(A_STRUC) + sum(A_WATER) + sum(J_PIER) + sum(J_STRUC) + sum(J_WATER),
            total_pelicans_injured = sum(A_S1) + sum(A_S2) + sum(A_S3) + sum(J_S1) + sum(J_S2) + sum(J_S3),
            total_pelicans_dead = sum(A_DEAD) + sum(J_DEAD))
#write.csv(totals_for_anglers1, file = "data/anglers/pelicans_date_section.csv", row.names = FALSE)

# summarize by date only
totals_for_anglers2 <- pelicandata %>%
  group_by(DATE) %>%
  summarise(total_pelicans = sum(A_PIER) + sum(A_STRUC) + sum(A_WATER) + sum(J_PIER) + sum(J_STRUC) + sum(J_WATER),
            total_pelicans_injured = sum(A_S1) + sum(A_S2) + sum(A_S3) + sum(J_S1) + sum(J_S2) + sum(J_S3),
            total_pelicans_dead = sum(A_DEAD) + sum(J_DEAD))
#write.csv(totals_for_anglers2, file = "data/anglers/pelicans_date.csv", row.names = FALSE)

