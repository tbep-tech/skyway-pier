library(tidyverse)

# import survey data
anglers <- read.csv("data/anglers/angler_surveys.csv")


######### DATA CLEANING ######### 

#### *- Section ####

anglers$SECTION <- ifelse(anglers$LOCATION == "Section A (markers 147-172)", "A",
                          ifelse(anglers$LOCATION == "Section B (markers 108-146)", "B",
                                 ifelse(anglers$LOCATION == "Section C (markers 73-107)", "C", NA)))

#### *- Time ####

# Create columns citing each time
anglers$TIME_MORNING <- ifelse(is.na(anglers$TIME), NA, 
                               ifelse(grepl("7am - 11am", anglers$TIME) , "Yes", "No"))
anglers$TIME_MIDDAY <- ifelse(is.na(anglers$TIME), NA, 
                              ifelse(grepl("Mid-day", anglers$TIME) , "Yes", "No"))
anglers$TIME_EVENING <- ifelse(is.na(anglers$TIME), NA, 
                               ifelse(grepl("Evening", anglers$TIME) , "Yes", "No"))
anglers$TIME_LATE <- ifelse(is.na(anglers$TIME), NA, 
                            ifelse(grepl("Late", anglers$TIME) , "Yes", "No"))
# Count number of times cited
anglers <- anglers %>%
  mutate(TIME_number = str_count(TIME_MORNING, "Yes") + str_count(TIME_MIDDAY, "Yes") + str_count(TIME_EVENING, "Yes") + str_count(TIME_LATE, "Yes"))


#### *- Reason ####

# Create columns citing each reason
anglers$REASON_FUN <- ifelse(is.na(anglers$REASON), NA, 
                             ifelse(grepl("sport", anglers$REASON) , "Yes", "No"))
anglers$REASON_FOOD <- ifelse(is.na(anglers$REASON), NA, 
                              ifelse(grepl("own", anglers$REASON) , "Yes", "No"))
anglers$REASON_SELL <- ifelse(is.na(anglers$REASON), NA, 
                              ifelse(grepl("sell", anglers$REASON) , "Yes", "No"))
anglers$REASON_WATCH <- ifelse(is.na(anglers$REASON), NA, 
                               ifelse(grepl("Watching", anglers$REASON) , "Yes", "No"))
anglers$REASON_OTHER <- ifelse(is.na(anglers$REASON), NA, 
                               ifelse(grepl("else", anglers$REASON) , "Yes", "No"))
# Count number of reasons cited
anglers <- anglers %>%
  mutate(REASON_number = str_count(REASON_FUN, "Yes") + str_count(REASON_FOOD, "Yes") + str_count(REASON_SELL, "Yes") + str_count(REASON_WATCH, "Yes") + str_count(REASON_OTHER, "Yes"))


#### *- Company ####

# Create columns citing each person joining
anglers$COMPANY_NONE <- ifelse(is.na(anglers$COMPANY), NA, 
                               ifelse(grepl("just me", anglers$COMPANY) , "Yes", "No"))
anglers$COMPANY_FRIEND <- ifelse(is.na(anglers$COMPANY), NA, 
                                 ifelse(grepl("friend", anglers$COMPANY) , "Yes", "No"))
anglers$COMPANY_PARSIB <- ifelse(is.na(anglers$COMPANY), NA, 
                                 ifelse(grepl("parent", anglers$COMPANY) , "Yes", "No"))
anglers$COMPANY_SPOUSE <- ifelse(is.na(anglers$COMPANY), NA, 
                                 ifelse(grepl("spouse", anglers$COMPANY) , "Yes", "No"))
anglers$COMPANY_KIDS <- ifelse(is.na(anglers$COMPANY), NA, 
                               ifelse(grepl("children", anglers$COMPANY) , "Yes", "No"))
anglers$COMPANY_OTHER <- ifelse(is.na(anglers$COMPANY), NA, 
                                ifelse(grepl("else", anglers$COMPANY) , "Yes", "No"))
# Count number of people cited
anglers <- anglers %>%
  mutate(COMPANY_number = str_count(COMPANY_FRIEND, "Yes") + str_count(COMPANY_PARSIB, "Yes") + str_count(COMPANY_SPOUSE, "Yes") + str_count(COMPANY_KIDS, "Yes") + str_count(COMPANY_OTHER, "Yes")) %>%
  mutate(COMPANY_number = ifelse(COMPANY_NONE == "Yes" & COMPANY_number > 0, NA, COMPANY_number))


#### *- Treatment/Control groups ####

# create a variable indicating treatment group
anglers$GROUP <- ifelse(is.na(anglers$TACKLE_C) & is.na(anglers$TTACKLE_C) & !is.na(anglers$TACKLE_T) & !is.na(anglers$TTACKLE_T), "Treatment", 
                        ifelse(!is.na(anglers$TACKLE_C) & !is.na(anglers$TTACKLE_C) & is.na(anglers$TACKLE_T) & is.na(anglers$TTACKLE_T), "Control", NA))

# create single variables for tackle questions
anglers$TACKLE <- ifelse(anglers$GROUP == "Control", anglers$TACKLE_C,
                         ifelse(anglers$GROUP == "Treatment", anglers$TACKLE_T, NA))
anglers$TTACKLE <- ifelse(anglers$GROUP == "Control", anglers$TTACKLE_C,
                          ifelse(anglers$GROUP == "Treatment", anglers$TTACKLE_T, NA))


#### *- Reverse coding ####

# reversed
anglers$ISSUE_CUT <- ifelse(anglers$ISSUE_CUT == "Strongly disagree", "Strongly agree",
                            ifelse(anglers$ISSUE_CUT == "Disagree", "Agree",
                                   ifelse(anglers$ISSUE_CUT == "Somewhat disagree", "Somewhat agree",
                                          ifelse(anglers$ISSUE_CUT == "Somewhat agree", "Somewhat disagree",
                                                 ifelse(anglers$ISSUE_CUT == "Agree", "Disagree",
                                                        ifelse(anglers$ISSUE_CUT == "Strongly agree", "Strongly disagree", NA))))))
# reversed
anglers$REG_AFFECT <- ifelse(anglers$REG_AFFECT.R == "Strongly disagree", "Strongly agree",
                             ifelse(anglers$REG_AFFECT.R == "Disagree", "Agree",
                                    ifelse(anglers$REG_AFFECT.R == "Somewhat disagree", "Somewhat agree",
                                           ifelse(anglers$REG_AFFECT.R == "Somewhat agree", "Somewhat disagree",
                                                  ifelse(anglers$REG_AFFECT.R == "Agree", "Disagree",
                                                         ifelse(anglers$REG_AFFECT.R == "Strongly agree", "Strongly disagree", NA))))))
# reversed
anglers$REG_ENOUGH <- ifelse(anglers$REG_ENOUGH.R == "Strongly disagree", "Strongly agree",
                             ifelse(anglers$REG_ENOUGH.R == "Disagree", "Agree",
                                    ifelse(anglers$REG_ENOUGH.R == "Somewhat disagree", "Somewhat agree",
                                           ifelse(anglers$REG_ENOUGH.R == "Somewhat agree", "Somewhat disagree",
                                                  ifelse(anglers$REG_ENOUGH.R == "Agree", "Disagree",
                                                         ifelse(anglers$REG_ENOUGH.R == "Strongly agree", "Strongly disagree", NA))))))
# reversed
anglers$REG_CAUSE <- ifelse(anglers$REG_CAUSE.R == "Strongly disagree", "Strongly agree",
                            ifelse(anglers$REG_CAUSE.R == "Disagree", "Agree",
                                   ifelse(anglers$REG_CAUSE.R == "Somewhat disagree", "Somewhat agree",
                                          ifelse(anglers$REG_CAUSE.R == "Somewhat agree", "Somewhat disagree",
                                                 ifelse(anglers$REG_CAUSE.R == "Agree", "Disagree",
                                                        ifelse(anglers$REG_CAUSE.R == "Strongly agree", "Strongly disagree", NA))))))
# reversed
anglers$REG_ENFORCE <- ifelse(anglers$REG_ENFORCE.R == "Strongly disagree", "Strongly agree",
                              ifelse(anglers$REG_ENFORCE.R == "Disagree", "Agree",
                                     ifelse(anglers$REG_ENFORCE.R == "Somewhat disagree", "Somewhat agree",
                                            ifelse(anglers$REG_ENFORCE.R == "Somewhat agree", "Somewhat disagree",
                                                   ifelse(anglers$REG_ENFORCE.R == "Agree", "Disagree",
                                                          ifelse(anglers$REG_ENFORCE.R == "Strongly agree", "Strongly disagree", NA))))))


#### *- Response grouping ####

anglers$FULLTIME <- ifelse(anglers$EMPLOY == "Full-time", 1, 0)
anglers$MALE <- ifelse(anglers$GENDER == "Male", 1, 0)
anglers$INCOME3 <- ifelse(anglers$INCOME == "$50,001 - $75,000", "$50,001 - $75,000",
                          ifelse(anglers$INCOME == "$25,000 or less" | anglers$INCOME == "$25,001 - $50,000", "$50,000 or less",
                                 ifelse(anglers$INCOME == "$75,001 - $100,000" | anglers$INCOME == "$100,001 or more", "$75,001 or more", NA)))
anglers$FLRESIDENT <- ifelse(anglers$RESIDENT == "Yes", 1, 0)
anglers$FOOD <- ifelse(anglers$REASON_FOOD == "Yes" | anglers$REASON_SELL == "Yes", 1, 0)
anglers$SOCIAL <- ifelse(anglers$COMPANY_number > 0, 1, 0)
anglers$FREQUENCY3 <- ifelse(anglers$FREQUENCY == "At least once a week", "Frequently",
                             ifelse(anglers$FREQUENCY == "Once or twice a month", "Occasionally", "Rarely"))
anglers$AWARE <- ifelse(anglers$REG_AWARE == "Yes", 1, 0)


#### *- Set scale values ####

anglers$TACKLE.Q <- ifelse(anglers$GROUP == "Treatment" & anglers$TACKLE == "All of them", 5,
                           ifelse(anglers$GROUP == "Control" & anglers$TACKLE == "All of them", 4,
                                  ifelse(anglers$TACKLE == "Four of them", 4,
                                         ifelse(anglers$TACKLE == "Three of them", 3,
                                                ifelse(anglers$TACKLE == "Two of them", 2,
                                                       ifelse(anglers$TACKLE == "One of them", 1, 0))))))
anglers$TTACKLE.Q <- ifelse(anglers$GROUP == "Treatment" & anglers$TTACKLE == "All of them", 5,
                            ifelse(anglers$GROUP == "Control" & anglers$TTACKLE == "All of them", 4,
                                   ifelse(anglers$TTACKLE == "Four of them", 4,
                                          ifelse(anglers$TTACKLE == "Three of them", 3,
                                                 ifelse(anglers$TTACKLE == "Two of them", 2,
                                                        ifelse(anglers$TTACKLE == "One of them", 1, 0))))))
anglers$ISSUE_CONCERN.Q <- ifelse(anglers$ISSUE_CONCERN == "Strongly disagree", 1,
                                  ifelse(anglers$ISSUE_CONCERN == "Disagree", 2,
                                         ifelse(anglers$ISSUE_CONCERN == "Somewhat disagree", 3,
                                                ifelse(anglers$ISSUE_CONCERN == "Somewhat agree", 4,
                                                       ifelse(anglers$ISSUE_CONCERN == "Agree", 5,
                                                              ifelse(anglers$ISSUE_CONCERN == "Strongly agree", 6, NA))))))
anglers$ISSUE_CONFIDENT.Q <- ifelse(anglers$ISSUE_CONFIDENT == "Strongly disagree", 1,
                                    ifelse(anglers$ISSUE_CONFIDENT == "Disagree", 2,
                                           ifelse(anglers$ISSUE_CONFIDENT == "Somewhat disagree", 3,
                                                  ifelse(anglers$ISSUE_CONFIDENT == "Somewhat agree", 4,
                                                         ifelse(anglers$ISSUE_CONFIDENT == "Agree", 5,
                                                                ifelse(anglers$ISSUE_CONFIDENT == "Strongly agree", 6, NA))))))
anglers$ISSUE_CUT.Q <- ifelse(anglers$ISSUE_CUT == "Strongly disagree", 1,
                              ifelse(anglers$ISSUE_CUT == "Disagree", 2,
                                     ifelse(anglers$ISSUE_CUT == "Somewhat disagree", 3,
                                            ifelse(anglers$ISSUE_CUT == "Somewhat agree", 4,
                                                   ifelse(anglers$ISSUE_CUT == "Agree", 5,
                                                          ifelse(anglers$ISSUE_CUT == "Strongly agree", 6, NA))))))
anglers$ISSUE_NORM.Q <- ifelse(anglers$ISSUE_NORM == "Strongly disagree", 1,
                               ifelse(anglers$ISSUE_NORM == "Disagree", 2,
                                      ifelse(anglers$ISSUE_NORM == "Somewhat disagree", 3,
                                             ifelse(anglers$ISSUE_NORM == "Somewhat agree", 4,
                                                    ifelse(anglers$ISSUE_NORM == "Agree", 5,
                                                           ifelse(anglers$ISSUE_NORM == "Strongly agree", 6, NA))))))
anglers$ACTION_GEAR.Q <- ifelse(anglers$ACTION_GEAR == "Not effective at all", 1,
                                ifelse(anglers$ACTION_GEAR == "Slightly effective", 2,
                                       ifelse(anglers$ACTION_GEAR == "Moderately effective", 3,
                                              ifelse(anglers$ACTION_GEAR == "Very effective", 4,
                                                     ifelse(anglers$ACTION_GEAR == "Extremely effective", 5, NA)))))
anglers$ACTION_ZONE.Q <- ifelse(anglers$ACTION_ZONE == "Not effective at all", 1,
                                ifelse(anglers$ACTION_ZONE == "Slightly effective", 2,
                                       ifelse(anglers$ACTION_ZONE == "Moderately effective", 3,
                                              ifelse(anglers$ACTION_ZONE == "Very effective", 4,
                                                     ifelse(anglers$ACTION_ZONE == "Extremely effective", 5, NA)))))
anglers$ACTION_ENFORCE.Q <- ifelse(anglers$ACTION_ENFORCE == "Not effective at all", 1,
                                   ifelse(anglers$ACTION_ENFORCE == "Slightly effective", 2,
                                          ifelse(anglers$ACTION_ENFORCE == "Moderately effective", 3,
                                                 ifelse(anglers$ACTION_ENFORCE == "Very effective", 4,
                                                        ifelse(anglers$ACTION_ENFORCE == "Extremely effective", 5, NA)))))
anglers$ACTION_DEMOLISH.Q <- ifelse(anglers$ACTION_DEMOLISH == "Not effective at all", 1,
                                    ifelse(anglers$ACTION_DEMOLISH == "Slightly effective", 2,
                                           ifelse(anglers$ACTION_DEMOLISH == "Moderately effective", 3,
                                                  ifelse(anglers$ACTION_DEMOLISH == "Very effective", 4,
                                                         ifelse(anglers$ACTION_DEMOLISH == "Extremely effective", 5, NA)))))
anglers$ACTION_VIDEO.Q <- ifelse(anglers$ACTION_VIDEO == "Not effective at all", 1,
                                 ifelse(anglers$ACTION_VIDEO == "Slightly effective", 2,
                                        ifelse(anglers$ACTION_VIDEO == "Moderately effective", 3,
                                               ifelse(anglers$ACTION_VIDEO == "Very effective", 4,
                                                      ifelse(anglers$ACTION_VIDEO == "Extremely effective", 5, NA)))))
anglers$REG_AFFECT.Q <- ifelse(anglers$REG_AFFECT == "Strongly disagree", 1,
                               ifelse(anglers$REG_AFFECT == "Disagree", 2,
                                      ifelse(anglers$REG_AFFECT == "Somewhat disagree", 3,
                                             ifelse(anglers$REG_AFFECT == "Somewhat agree", 4,
                                                    ifelse(anglers$REG_AFFECT == "Agree", 5,
                                                           ifelse(anglers$REG_AFFECT == "Strongly agree", 6, NA))))))
anglers$REG_DIFFICULT.Q <- ifelse(anglers$REG_DIFFICULT == "Strongly disagree", 1,
                                  ifelse(anglers$REG_DIFFICULT == "Disagree", 2,
                                         ifelse(anglers$REG_DIFFICULT == "Somewhat disagree", 3,
                                                ifelse(anglers$REG_DIFFICULT == "Somewhat agree", 4,
                                                       ifelse(anglers$REG_DIFFICULT == "Agree", 5,
                                                              ifelse(anglers$REG_DIFFICULT == "Strongly agree", 6, NA))))))
anglers$REG_ENOUGH.Q <- ifelse(anglers$REG_ENOUGH == "Strongly disagree", 1,
                               ifelse(anglers$REG_ENOUGH == "Disagree", 2,
                                      ifelse(anglers$REG_ENOUGH == "Somewhat disagree", 3,
                                             ifelse(anglers$REG_ENOUGH == "Somewhat agree", 4,
                                                    ifelse(anglers$REG_ENOUGH == "Agree", 5,
                                                           ifelse(anglers$REG_ENOUGH == "Strongly agree", 6, NA))))))
anglers$REG_CAUSE.Q <- ifelse(anglers$REG_CAUSE == "Strongly disagree", 1,
                              ifelse(anglers$REG_CAUSE == "Disagree", 2,
                                     ifelse(anglers$REG_CAUSE == "Somewhat disagree", 3,
                                            ifelse(anglers$REG_CAUSE == "Somewhat agree", 4,
                                                   ifelse(anglers$REG_CAUSE == "Agree", 5,
                                                          ifelse(anglers$REG_CAUSE == "Strongly agree", 6, NA))))))
anglers$REG_SNORM.Q <- ifelse(anglers$REG_SNORM == "Strongly disagree", 1,
                              ifelse(anglers$REG_SNORM == "Disagree", 2,
                                     ifelse(anglers$REG_SNORM == "Somewhat disagree", 3,
                                            ifelse(anglers$REG_SNORM == "Somewhat agree", 4,
                                                   ifelse(anglers$REG_SNORM == "Agree", 5,
                                                          ifelse(anglers$REG_SNORM == "Strongly agree", 6, NA))))))
anglers$REG_REDUCE.Q <- ifelse(anglers$REG_REDUCE == "Strongly disagree", 1,
                               ifelse(anglers$REG_REDUCE == "Disagree", 2,
                                      ifelse(anglers$REG_REDUCE == "Somewhat disagree", 3,
                                             ifelse(anglers$REG_REDUCE == "Somewhat agree", 4,
                                                    ifelse(anglers$REG_REDUCE == "Agree", 5,
                                                           ifelse(anglers$REG_REDUCE == "Strongly agree", 6, NA))))))
anglers$REG_ENFORCE.Q <- ifelse(anglers$REG_ENFORCE == "Strongly disagree", 1,
                                ifelse(anglers$REG_ENFORCE == "Disagree", 2,
                                       ifelse(anglers$REG_ENFORCE == "Somewhat disagree", 3,
                                              ifelse(anglers$REG_ENFORCE == "Somewhat agree", 4,
                                                     ifelse(anglers$REG_ENFORCE == "Agree", 5,
                                                            ifelse(anglers$REG_ENFORCE == "Strongly agree", 6, NA))))))
anglers$REG_DNORM.Q <- ifelse(anglers$REG_DNORM == "Strongly disagree", 1,
                              ifelse(anglers$REG_DNORM == "Disagree", 2,
                                     ifelse(anglers$REG_DNORM == "Somewhat disagree", 3,
                                            ifelse(anglers$REG_DNORM == "Somewhat agree", 4,
                                                   ifelse(anglers$REG_DNORM == "Agree", 5,
                                                          ifelse(anglers$REG_DNORM == "Strongly agree", 6, NA))))))


# distinguish datasets that will be used for (1) qualitative, (2) quantitative, and (3) pelican analyses
anglers_qual <- anglers %>%
  filter(!grepl('X_', ResponseID)) %>%
  select(ResponseID, AGE:VISIT_FROM, REASON_TEXT, COMPANY_TEXT, FREQUENCY, ISSUE_CONCERN:REG_AWARE,
         REG_DIFFICULT, REG_SNORM, REG_REDUCE, REG_DNORM:REG_ENFORCE)

anglers_quant <- anglers %>%
  filter(!grepl('X_', ResponseID)) %>%
  select(ResponseID, AGE, ZIPCODE, SECTION, TIME_number, GROUP, FULLTIME:REG_DNORM.Q)

anglers_pelicans <- anglers %>%
  select(Date:Survey, PELICAN_TOTAL:PELICAN_FED_TYPE_TEXT, SECTION:TIME_number)

#write.csv(anglers_qual, file = "data/anglers/angler_data_qual.csv", row.names = FALSE)
#write.csv(anglers_quant, file = "data/anglers/angler_data_quant.csv", row.names = FALSE)
#write.csv(anglers_pelicans, file = "data/anglers/angler_data_pelicans.csv", row.names = FALSE)



#### ALIGN SUBJECTIVE & OBJECTIVE PELICAN OBSERVATIONS ####


# import survey data
anglers_pelicans <- read.csv("data/anglers/angler_data_pelicans.csv")

# import pelican data
pelicans_date <- read.csv("data/anglers/pelicans_date.csv")
pelicans_date_section <- read.csv("data/anglers/pelicans_date_section.csv")


######### *- Prep angler data ######### 

# set date
anglers_pelicans$Date <- as.Date(anglers_pelicans$Date, "%m/%d/%Y")

# replace NA with 0 when no pelicans sighted
anglers_pelicans <- anglers_pelicans %>%
  mutate(PELICAN_TOTAL_N = ifelse(is.na(PELICAN_TOTAL_N) & PELICAN_TOTAL == "No", 0, PELICAN_TOTAL_N),
         PELICAN_INJURED_N = ifelse(is.na(PELICAN_INJURED_N & PELICAN_INJURED == "No"), 0, PELICAN_INJURED_N),
         PELICAN_DEAD_N = ifelse(is.na(PELICAN_DEAD_N & PELICAN_DEAD == "No"), 0, PELICAN_DEAD_N))

# remove missing or inconsistent responses
anglers_pelicans_reduced <- anglers_pelicans %>%
  filter_at(vars(PELICAN_TOTAL,PELICAN_TOTAL_N,PELICAN_INJURED,PELICAN_INJURED_N,PELICAN_DEAD,PELICAN_DEAD_N), all_vars(!is.na(.))) %>%
  mutate(inconsistent = ifelse(PELICAN_INJURED_N > PELICAN_TOTAL_N | PELICAN_DEAD_N > PELICAN_TOTAL_N, "Yes", 
                               ifelse(PELICAN_TOTAL_N > 0 & PELICAN_TOTAL == "No" | PELICAN_INJURED_N > 0 & PELICAN_INJURED == "No" | PELICAN_DEAD_N > 0 & PELICAN_DEAD == "No", "Yes", "No"))) %>%
  filter(inconsistent == "No")

# focus on people who were at the pier the same time as us
anglers_pelicans_compare <- anglers_pelicans_reduced %>%
  filter(TIME_number == 1)




######### *- Prep pelican data ######### 

# set date
pelicans_date$Date <- as.Date(pelicans_date$DATE)
pelicans_date_section$Date <- as.Date(pelicans_date_section$DATE)

# rename columns
pelicans_date <- pelicans_date %>%
  rename(pelicans_date = total_pelicans,
         injured_date = total_pelicans_injured,
         dead_date = total_pelicans_dead)
pelicans_date_section <- pelicans_date_section %>%
  rename(SECTION = SECT,
         pelicans_section = total_pelicans,
         injured_section = total_pelicans_injured,
         dead_section = total_pelicans_dead)


######### *- Merge data ######### 

angler_pelican_data <- merge(anglers_pelicans_compare, pelicans_date, by = "Date", all.x = TRUE)
angler_pelican_data <- merge(angler_pelican_data, pelicans_date_section, by = c("Date", "SECTION"), all.x = TRUE)


######### *- Align and compare ######### 

# remove outlier
angler_pelican_data <- angler_pelican_data %>%
  filter(PELICAN_TOTAL_N != 100)

# create bins for sightings
angler_pelican_data <- angler_pelican_data %>%
  mutate(subjective_cat = ifelse(PELICAN_TOTAL_N == 0, "0",
                                 ifelse(PELICAN_TOTAL_N >= 1 & PELICAN_TOTAL_N <= 4, "1-4",
                                        ifelse(PELICAN_TOTAL_N >= 5 & PELICAN_TOTAL_N <= 9, "5-9",
                                               ifelse(PELICAN_TOTAL_N >= 10 & PELICAN_TOTAL_N <= 19, "10-19",
                                                      ifelse(PELICAN_TOTAL_N >= 20, "20+", NA))))),
         objectiveSECT_cat = ifelse(pelicans_section == 0, "0",
                                    ifelse(pelicans_section >= 1 & pelicans_section <= 4, "1-4",
                                           ifelse(pelicans_section >= 5 & pelicans_section <= 9, "5-9",
                                                  ifelse(pelicans_section >= 10 & pelicans_section <= 19, "10-19",
                                                         ifelse(pelicans_section >= 20, "20+", NA))))))

angler_pelican_counts <- angler_pelican_data %>%
  group_by(objectiveSECT_cat, subjective_cat) %>%
  summarise(count = n())


plot(angler_pelican_data$pelicans_date, angler_pelican_data$PELICAN_TOTAL_N)
plot(angler_pelican_data$pelicans_section, angler_pelican_data$PELICAN_TOTAL_N)


plot_obj_subj <- ggplot(angler_pelican_data, aes(x=pelicans_section, y=PELICAN_TOTAL_N) ) +
  stat_density_2d(aes(fill = after_stat(density)), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette= "Spectral", direction=-1) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "white", linetype = 3) +
  theme(
    legend.position='none',
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
    legend.box.background = element_rect(fill='transparent'),
    panel.grid  = element_blank())


# Classify angler estimates into over- and under-estimates
angler_pelican_data$pelican_estimate <- ifelse(angler_pelican_data$subjective_cat == "0" & angler_pelican_data$objectiveSECT_cat == "0", "About right",
                                               ifelse(angler_pelican_data$subjective_cat == "0" & angler_pelican_data$objectiveSECT_cat == "1-4", "Underestimate",
                                                      ifelse(angler_pelican_data$subjective_cat == "0" & angler_pelican_data$objectiveSECT_cat == "5-9", "Underestimate",
                                                             ifelse(angler_pelican_data$subjective_cat == "1-4" & angler_pelican_data$objectiveSECT_cat == "0", "Overestimate",
                                                                    ifelse(angler_pelican_data$subjective_cat == "1-4" & angler_pelican_data$objectiveSECT_cat == "1-4", "About right",
                                                                           ifelse(angler_pelican_data$subjective_cat == "1-4" & angler_pelican_data$objectiveSECT_cat == "5-9", "Underestimate",
                                                                                  ifelse(angler_pelican_data$subjective_cat == "5-9" & angler_pelican_data$objectiveSECT_cat == "0", "Overestimate",
                                                                                         ifelse(angler_pelican_data$subjective_cat == "5-9" & angler_pelican_data$objectiveSECT_cat == "1-4", "Overestimate",
                                                                                                ifelse(angler_pelican_data$subjective_cat == "5-9" & angler_pelican_data$objectiveSECT_cat == "5-9", "About right",
                                                                                                       ifelse(angler_pelican_data$subjective_cat == "10-19" & angler_pelican_data$objectiveSECT_cat == "0", "Overestimate",
                                                                                                              ifelse(angler_pelican_data$subjective_cat == "10-19" & angler_pelican_data$objectiveSECT_cat == "1-4", "Overestimate",
                                                                                                                     ifelse(angler_pelican_data$subjective_cat == "10-19" & angler_pelican_data$objectiveSECT_cat == "5-9", "Overestimate",
                                                                                                                            ifelse(angler_pelican_data$subjective_cat == "20+" & angler_pelican_data$objectiveSECT_cat == "0", "Overestimate",
                                                                                                                                   ifelse(angler_pelican_data$subjective_cat == "20+" & angler_pelican_data$objectiveSECT_cat == "1-4", "Overestimate",
                                                                                                                                          ifelse(angler_pelican_data$subjective_cat == "20+" & angler_pelican_data$objectiveSECT_cat == "5-9", "Overestimate",NA)))))))))))))))



estimates <- angler_pelican_data %>%
  group_by(pelican_estimate) %>%
  summarise(count = n())

angler_pelican_data$DATE.x <- NULL
angler_pelican_data$DATE.y <- NULL

#write.csv(angler_pelican_data, file = "data/anglers/angler_data_pelicans_aligned.csv", row.names = FALSE)
