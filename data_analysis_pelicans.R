library(tidyverse)
library(sjPlot)
library(svglite)
library(gridExtra)

# Load cleaned pelican data
pelicandata <- read.csv("data/pelicans/pelican_data.csv")


#### DESCRIPTIVE STATS #### 

#### *- Pelican summaries #### 

sem <- function(x) sd(x)/sqrt(length(x))

# summarize data by period
totals_per_survey <- pelicandata %>%
  group_by(PERIOD, DATE, TIME, DAY) %>%
  summarise(total_pelicans = sum(A_PIER) + sum(A_STRUC) + sum(A_WATER) + sum(J_PIER) + sum(J_STRUC) + sum(J_WATER),
            total_pelicans_injured = sum(A_S1) + sum(A_S2) + sum(A_S3) + sum(J_S1) + sum(J_S2) + sum(J_S3),
            total_pelicans_dead = sum(A_DEAD) + sum(J_DEAD),
            total_adult = sum(A_PIER) + sum(A_STRUC) + sum(A_WATER),
            total_juv = sum(J_PIER) + sum(J_STRUC) + sum(J_WATER),
            total_adult_injured = sum(A_S1) + sum(A_S2) + sum(A_S3),
            total_juv_injured = sum(J_S1) + sum(J_S2) + sum(J_S3),
            total_adult_dead = sum(A_DEAD),
            total_juv_dead = sum(J_DEAD),
            total_adult_pier = sum(A_PIER),
            total_adult_struc = sum(A_STRUC),
            total_adult_water = sum(A_WATER),
            total_juv_pier = sum(J_PIER),
            total_juv_struc = sum(J_STRUC),
            total_juv_water = sum(J_WATER),
            total_adult_s1 = sum(A_S1),
            total_adult_s2 = sum(A_S2),
            total_adult_s3 = sum(A_S3),
            total_juv_s1 = sum(J_S1),
            total_juv_s2 = sum(J_S2),
            total_juv_s3 = sum(J_S3),
            total_anglers = sum(ANGLERS),
            total_lines = sum(LINES)) %>%
  mutate(total_LPA = ifelse(total_anglers == 0, 0, total_lines/total_anglers))


means_PERIOD <- totals_per_survey %>%
  group_by(PERIOD) %>%
  summarise(n = n(),
            mean_pelicans = mean(total_pelicans),
            mean_pelicans_injured = mean(total_pelicans_injured),
            mean_pelicans_dead = mean(total_pelicans_dead),
            mean_adult = mean(total_adult),
            mean_adult_injured = mean(total_adult_injured),
            mean_adult_dead = mean(total_adult_dead),
            mean_juv = mean(total_juv),
            mean_juv_injured = mean(total_juv_injured),
            mean_juv_dead = mean(total_juv_dead),
            sd_pelicans = sd(total_pelicans),
            sd_pelicans_injured = sd(total_pelicans_injured),
            sd_pelicans_dead = sd(total_pelicans_dead),
            sd_adult = sd(total_adult),
            sd_adult_injured = sd(total_adult_injured),
            sd_adult_dead = sd(total_adult_dead),
            sd_juv = sd(total_juv),
            sd_juv_injured = sd(total_juv_injured),
            sd_juv_dead = sd(total_juv_dead),
            sem_pelicans = sem(total_pelicans),
            sem_pelicans_injured = sem(total_pelicans_injured),
            sem_pelicans_dead = sem(total_pelicans_dead),
            sem_adult = sem(total_adult),
            sem_adult_injured = sem(total_adult_injured),
            sem_adult_dead = sem(total_adult_dead),
            sem_juv = sem(total_juv),
            sem_juv_injured = sem(total_juv_injured),
            sem_juv_dead = sem(total_juv_dead))

# by PERIOD
plot_PERIOD_adult <- ggplot(means_PERIOD, aes(x=PERIOD, y=mean_adult, fill=PERIOD)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ggtitle("All adult") +
  geom_errorbar(aes(x = PERIOD, ymin = mean_adult-sd_adult, ymax = mean_adult+sd_adult), 
                colour = "black", width = 0.2, linewidth = 1) +
  ylim(0, 60) +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        panel.grid  = element_blank())
plot_PERIOD_juv <- ggplot(means_PERIOD, aes(x=PERIOD, y=mean_juv, fill=PERIOD)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ggtitle("All juv") +
  geom_errorbar(aes(x = PERIOD, ymin = mean_juv-sd_juv, ymax = mean_juv+sd_juv), 
                colour = "black", width = 0.2, linewidth = 1) +
  ylim(0, 60) +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        panel.grid  = element_blank())



plot_PERIOD_adult_injured <- ggplot(means_PERIOD, aes(x=PERIOD, y=mean_adult_injured, fill=PERIOD)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ggtitle("All adult_injured") +
  geom_errorbar(aes(x = PERIOD, ymin = mean_adult_injured-sd_adult_injured, ymax = mean_adult_injured+sd_adult_injured), 
                colour = "black", width = 0.2, linewidth = 1) +
  ylim(0, 6) +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        panel.grid  = element_blank())
plot_PERIOD_juv_injured <- ggplot(means_PERIOD, aes(x=PERIOD, y=mean_juv_injured, fill=PERIOD)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ggtitle("All juv_injured") +
  geom_errorbar(aes(x = PERIOD, ymin = mean_juv_injured-sd_juv_injured, ymax = mean_juv_injured+sd_juv_injured), 
                colour = "black", width = 0.2, linewidth = 1) +
  ylim(0, 6) +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        panel.grid  = element_blank())


# by time
totals_per_survey <- pelicandata %>%
  group_by(DATE, TIME) %>%
  summarise(total_pelicans = sum(A_PIER) + sum(A_STRUC) + sum(A_WATER) + sum(J_PIER) + sum(J_STRUC) + sum(J_WATER),
            total_pelicans_injured = sum(A_S1) + sum(A_S2) + sum(A_S3) + sum(J_S1) + sum(J_S2) + sum(J_S3),
            total_pelicans_dead = sum(A_DEAD) + sum(J_DEAD),
            total_adult = sum(A_PIER) + sum(A_STRUC) + sum(A_WATER),
            total_juv = sum(J_PIER) + sum(J_STRUC) + sum(J_WATER),
            total_adult_injured = sum(A_S1) + sum(A_S2) + sum(A_S3),
            total_juv_injured = sum(J_S1) + sum(J_S2) + sum(J_S3),
            total_adult_dead = sum(A_DEAD),
            total_juv_dead = sum(J_DEAD),
            total_adult_pier = sum(A_PIER),
            total_adult_struc = sum(A_STRUC),
            total_adult_water = sum(A_WATER),
            total_juv_pier = sum(J_PIER),
            total_juv_struc = sum(J_STRUC),
            total_juv_water = sum(J_WATER),
            total_adult_s1 = sum(A_S1),
            total_adult_s2 = sum(A_S2),
            total_adult_s3 = sum(A_S3),
            total_juv_s1 = sum(J_S1),
            total_juv_s2 = sum(J_S2),
            total_juv_s3 = sum(J_S3),
            total_AJ_ratio = total_adult/total_juv,
            injured_AJ_ratio = total_adult_injured/total_juv_injured)

means_TIME <- totals_per_survey %>%
  group_by(TIME) %>%
  summarise(n = n(),
            mean_pelicans = mean(total_pelicans),
            mean_pelicans_injured = mean(total_pelicans_injured),
            mean_pelicans_dead = mean(total_pelicans_dead),
            mean_adult = mean(total_adult),
            mean_adult_injured = mean(total_adult_injured),
            mean_adult_dead = mean(total_adult_dead),
            mean_juv = mean(total_juv),
            mean_juv_injured = mean(total_juv_injured),
            mean_juv_dead = mean(total_juv_dead),
            sd_pelicans = sd(total_pelicans),
            sd_pelicans_injured = sd(total_pelicans_injured),
            sd_pelicans_dead = sd(total_pelicans_dead),
            sd_adult = sd(total_adult),
            sd_adult_injured = sd(total_adult_injured),
            sd_adult_dead = sd(total_adult_dead),
            sd_juv = sd(total_juv),
            sd_juv_injured = sd(total_juv_injured),
            sd_juv_dead = sd(total_juv_dead),
            sem_pelicans = sem(total_pelicans),
            sem_pelicans_injured = sem(total_pelicans_injured),
            sem_pelicans_dead = sem(total_pelicans_dead),
            sem_adult = sem(total_adult),
            sem_adult_injured = sem(total_adult_injured),
            sem_adult_dead = sem(total_adult_dead),
            sem_juv = sem(total_juv),
            sem_juv_injured = sem(total_juv_injured),
            sem_juv_dead = sem(total_juv_dead))

# by day
totals_per_survey <- pelicandata %>%
  group_by(DATE, TIME, DAY) %>%
  summarise(total_pelicans = sum(A_PIER) + sum(A_STRUC) + sum(A_WATER) + sum(J_PIER) + sum(J_STRUC) + sum(J_WATER),
            total_pelicans_injured = sum(A_S1) + sum(A_S2) + sum(A_S3) + sum(J_S1) + sum(J_S2) + sum(J_S3),
            total_pelicans_dead = sum(A_DEAD) + sum(J_DEAD),
            total_adult = sum(A_PIER) + sum(A_STRUC) + sum(A_WATER),
            total_juv = sum(J_PIER) + sum(J_STRUC) + sum(J_WATER),
            total_adult_injured = sum(A_S1) + sum(A_S2) + sum(A_S3),
            total_juv_injured = sum(J_S1) + sum(J_S2) + sum(J_S3),
            total_adult_dead = sum(A_DEAD),
            total_juv_dead = sum(J_DEAD),
            total_adult_pier = sum(A_PIER),
            total_adult_struc = sum(A_STRUC),
            total_adult_water = sum(A_WATER),
            total_juv_pier = sum(J_PIER),
            total_juv_struc = sum(J_STRUC),
            total_juv_water = sum(J_WATER),
            total_adult_s1 = sum(A_S1),
            total_adult_s2 = sum(A_S2),
            total_adult_s3 = sum(A_S3),
            total_juv_s1 = sum(J_S1),
            total_juv_s2 = sum(J_S2),
            total_juv_s3 = sum(J_S3),
            total_AJ_ratio = total_adult/total_juv,
            injured_AJ_ratio = total_adult_injured/total_juv_injured)

means_DAY <- totals_per_survey %>%
  group_by(DAY) %>%
  summarise(n = n(),
            mean_pelicans = mean(total_pelicans),
            mean_pelicans_injured = mean(total_pelicans_injured),
            mean_pelicans_dead = mean(total_pelicans_dead),
            mean_adult = mean(total_adult),
            mean_adult_injured = mean(total_adult_injured),
            mean_adult_dead = mean(total_adult_dead),
            mean_juv = mean(total_juv),
            mean_juv_injured = mean(total_juv_injured),
            mean_juv_dead = mean(total_juv_dead),
            sd_pelicans = sd(total_pelicans),
            sd_pelicans_injured = sd(total_pelicans_injured),
            sd_pelicans_dead = sd(total_pelicans_dead),
            sd_adult = sd(total_adult),
            sd_adult_injured = sd(total_adult_injured),
            sd_adult_dead = sd(total_adult_dead),
            sd_juv = sd(total_juv),
            sd_juv_injured = sd(total_juv_injured),
            sd_juv_dead = sd(total_juv_dead),
            sem_pelicans = sem(total_pelicans),
            sem_pelicans_injured = sem(total_pelicans_injured),
            sem_pelicans_dead = sem(total_pelicans_dead),
            sem_adult = sem(total_adult),
            sem_adult_injured = sem(total_adult_injured),
            sem_adult_dead = sem(total_adult_dead),
            sem_juv = sem(total_juv),
            sem_juv_injured = sem(total_juv_injured),
            sem_juv_dead = sem(total_juv_dead))

# by section
totals_per_survey <- pelicandata %>%
  group_by(DATE, TIME, SECT) %>%
  summarise(total_pelicans = sum(A_PIER) + sum(A_STRUC) + sum(A_WATER) + sum(J_PIER) + sum(J_STRUC) + sum(J_WATER),
            total_pelicans_injured = sum(A_S1) + sum(A_S2) + sum(A_S3) + sum(J_S1) + sum(J_S2) + sum(J_S3),
            total_pelicans_dead = sum(A_DEAD) + sum(J_DEAD),
            total_adult = sum(A_PIER) + sum(A_STRUC) + sum(A_WATER),
            total_juv = sum(J_PIER) + sum(J_STRUC) + sum(J_WATER),
            total_adult_injured = sum(A_S1) + sum(A_S2) + sum(A_S3),
            total_juv_injured = sum(J_S1) + sum(J_S2) + sum(J_S3),
            total_adult_dead = sum(A_DEAD),
            total_juv_dead = sum(J_DEAD),
            total_adult_pier = sum(A_PIER),
            total_adult_struc = sum(A_STRUC),
            total_adult_water = sum(A_WATER),
            total_juv_pier = sum(J_PIER),
            total_juv_struc = sum(J_STRUC),
            total_juv_water = sum(J_WATER),
            total_adult_s1 = sum(A_S1),
            total_adult_s2 = sum(A_S2),
            total_adult_s3 = sum(A_S3),
            total_juv_s1 = sum(J_S1),
            total_juv_s2 = sum(J_S2),
            total_juv_s3 = sum(J_S3),
            total_AJ_ratio = total_adult/total_juv,
            injured_AJ_ratio = total_adult_injured/total_juv_injured)

means_SECT <- totals_per_survey %>%
  group_by(SECT) %>%
  summarise(n = n(),
            mean_pelicans = mean(total_pelicans),
            mean_pelicans_injured = mean(total_pelicans_injured),
            mean_pelicans_dead = mean(total_pelicans_dead),
            mean_adult = mean(total_adult),
            mean_adult_injured = mean(total_adult_injured),
            mean_adult_dead = mean(total_adult_dead),
            mean_juv = mean(total_juv),
            mean_juv_injured = mean(total_juv_injured),
            mean_juv_dead = mean(total_juv_dead),
            sd_pelicans = sd(total_pelicans),
            sd_pelicans_injured = sd(total_pelicans_injured),
            sd_pelicans_dead = sd(total_pelicans_dead),
            sd_adult = sd(total_adult),
            sd_adult_injured = sd(total_adult_injured),
            sd_adult_dead = sd(total_adult_dead),
            sd_juv = sd(total_juv),
            sd_juv_injured = sd(total_juv_injured),
            sd_juv_dead = sd(total_juv_dead),
            sem_pelicans = sem(total_pelicans),
            sem_pelicans_injured = sem(total_pelicans_injured),
            sem_pelicans_dead = sem(total_pelicans_dead),
            sem_adult = sem(total_adult),
            sem_adult_injured = sem(total_adult_injured),
            sem_adult_dead = sem(total_adult_dead),
            sem_juv = sem(total_juv),
            sem_juv_injured = sem(total_juv_injured),
            sem_juv_dead = sem(total_juv_dead))


# injured pelicans
totals_per_survey <- pelicandata %>%
  group_by(PERIOD, DATE, TIME, DAY) %>%
  summarise(total_pelicans = sum(A_PIER) + sum(A_STRUC) + sum(A_WATER) + sum(J_PIER) + sum(J_STRUC) + sum(J_WATER),
            total_pelicans_injured = sum(A_S1) + sum(A_S2) + sum(A_S3) + sum(J_S1) + sum(J_S2) + sum(J_S3),
            total_pelicans_dead = sum(A_DEAD) + sum(J_DEAD),
            total_adult = sum(A_PIER) + sum(A_STRUC) + sum(A_WATER),
            total_juv = sum(J_PIER) + sum(J_STRUC) + sum(J_WATER),
            total_adult_injured = sum(A_S1) + sum(A_S2) + sum(A_S3),
            total_juv_injured = sum(J_S1) + sum(J_S2) + sum(J_S3),
            total_adult_dead = sum(A_DEAD),
            total_juv_dead = sum(J_DEAD),
            total_adult_pier = sum(A_PIER),
            total_adult_struc = sum(A_STRUC),
            total_adult_water = sum(A_WATER),
            total_juv_pier = sum(J_PIER),
            total_juv_struc = sum(J_STRUC),
            total_juv_water = sum(J_WATER),
            total_adult_s1 = sum(A_S1),
            total_adult_s2 = sum(A_S2),
            total_adult_s3 = sum(A_S3),
            total_juv_s1 = sum(J_S1),
            total_juv_s2 = sum(J_S2),
            total_juv_s3 = sum(J_S3),
            total_anglers = sum(ANGLERS),
            total_lines = sum(LINES)) %>%
  mutate(total_LPA = ifelse(total_anglers == 0, 0, total_lines/total_anglers))

injuries <- totals_per_survey %>%
  group_by(PERIOD) %>%
  summarise(pelicans = sum(total_pelicans),
            injured = sum(total_pelicans_injured),
            s1 = sum(total_adult_s1) + sum(total_juv_s1),
            s2 = sum(total_adult_s2) + sum(total_juv_s2),
            s3 = sum(total_adult_s3) + sum(total_juv_s3))



#### *- Angler summaries #### 

# Correlation between anglers and lines

cordata <- pelicandata %>%
  group_by(DATE, TIME, SECT) %>%
  summarise(anglers = sum(ANGLERS),
            lines = sum(LINES))
cor.test(cordata$anglers, cordata$lines, method = "pearson")


sem <- function(x) sd(x)/sqrt(length(x))

# summarize data by period
totals_per_survey <- pelicandata %>%
  group_by(PERIOD, DATE, TIME, DAY) %>%
  summarise(total_pelicans = sum(A_PIER) + sum(A_STRUC) + sum(A_WATER) + sum(J_PIER) + sum(J_STRUC) + sum(J_WATER),
            total_pelicans_injured = sum(A_S1) + sum(A_S2) + sum(A_S3) + sum(J_S1) + sum(J_S2) + sum(J_S3),
            total_pelicans_dead = sum(A_DEAD) + sum(J_DEAD),
            total_adult = sum(A_PIER) + sum(A_STRUC) + sum(A_WATER),
            total_juv = sum(J_PIER) + sum(J_STRUC) + sum(J_WATER),
            total_adult_injured = sum(A_S1) + sum(A_S2) + sum(A_S3),
            total_juv_injured = sum(J_S1) + sum(J_S2) + sum(J_S3),
            total_adult_dead = sum(A_DEAD),
            total_juv_dead = sum(J_DEAD),
            total_adult_pier = sum(A_PIER),
            total_adult_struc = sum(A_STRUC),
            total_adult_water = sum(A_WATER),
            total_juv_pier = sum(J_PIER),
            total_juv_struc = sum(J_STRUC),
            total_juv_water = sum(J_WATER),
            total_adult_s1 = sum(A_S1),
            total_adult_s2 = sum(A_S2),
            total_adult_s3 = sum(A_S3),
            total_juv_s1 = sum(J_S1),
            total_juv_s2 = sum(J_S2),
            total_juv_s3 = sum(J_S3),
            total_anglers = sum(ANGLERS),
            total_lines = sum(LINES)) %>%
  mutate(total_LPA = ifelse(total_anglers == 0, 0, total_lines/total_anglers))


means_PERIOD <- totals_per_survey %>%
  group_by(PERIOD) %>%
  summarise(n = n(),
            mean_anglers = mean(total_anglers),
            mean_lines = mean(total_lines),
            mean_LPA = mean(total_LPA),
            sd_anglers = sd(total_anglers),
            sd_lines = sd(total_lines),
            sd_LPA = sd(total_LPA),
            sem_anglers = sem(total_anglers),
            sem_lines = sem(total_lines),
            sem_LPA = sem(total_LPA))

# by time
totals_per_survey <- pelicandata %>%
  group_by(DATE, TIME) %>%
  summarise(total_pelicans = sum(A_PIER) + sum(A_STRUC) + sum(A_WATER) + sum(J_PIER) + sum(J_STRUC) + sum(J_WATER),
            total_pelicans_injured = sum(A_S1) + sum(A_S2) + sum(A_S3) + sum(J_S1) + sum(J_S2) + sum(J_S3),
            total_pelicans_dead = sum(A_DEAD) + sum(J_DEAD),
            total_adult = sum(A_PIER) + sum(A_STRUC) + sum(A_WATER),
            total_juv = sum(J_PIER) + sum(J_STRUC) + sum(J_WATER),
            total_adult_injured = sum(A_S1) + sum(A_S2) + sum(A_S3),
            total_juv_injured = sum(J_S1) + sum(J_S2) + sum(J_S3),
            total_adult_dead = sum(A_DEAD),
            total_juv_dead = sum(J_DEAD),
            total_adult_pier = sum(A_PIER),
            total_adult_struc = sum(A_STRUC),
            total_adult_water = sum(A_WATER),
            total_juv_pier = sum(J_PIER),
            total_juv_struc = sum(J_STRUC),
            total_juv_water = sum(J_WATER),
            total_adult_s1 = sum(A_S1),
            total_adult_s2 = sum(A_S2),
            total_adult_s3 = sum(A_S3),
            total_juv_s1 = sum(J_S1),
            total_juv_s2 = sum(J_S2),
            total_juv_s3 = sum(J_S3),
            total_anglers = sum(ANGLERS),
            total_lines = sum(LINES)) %>%
  mutate(total_LPA = ifelse(total_anglers == 0, 0, total_lines/total_anglers))

means_TIME <- totals_per_survey %>%
  group_by(TIME) %>%
  summarise(n = n(),
            mean_anglers = mean(total_anglers),
            mean_lines = mean(total_lines),
            mean_LPA = mean(total_LPA),
            sd_anglers = sd(total_anglers),
            sd_lines = sd(total_lines),
            sd_LPA = sd(total_LPA),
            sem_anglers = sem(total_anglers),
            sem_lines = sem(total_lines),
            sem_LPA = sem(total_LPA))


# by day
totals_per_survey <- pelicandata %>%
  group_by(DATE, TIME, DAY) %>%
  summarise(total_pelicans = sum(A_PIER) + sum(A_STRUC) + sum(A_WATER) + sum(J_PIER) + sum(J_STRUC) + sum(J_WATER),
            total_pelicans_injured = sum(A_S1) + sum(A_S2) + sum(A_S3) + sum(J_S1) + sum(J_S2) + sum(J_S3),
            total_pelicans_dead = sum(A_DEAD) + sum(J_DEAD),
            total_adult = sum(A_PIER) + sum(A_STRUC) + sum(A_WATER),
            total_juv = sum(J_PIER) + sum(J_STRUC) + sum(J_WATER),
            total_adult_injured = sum(A_S1) + sum(A_S2) + sum(A_S3),
            total_juv_injured = sum(J_S1) + sum(J_S2) + sum(J_S3),
            total_adult_dead = sum(A_DEAD),
            total_juv_dead = sum(J_DEAD),
            total_adult_pier = sum(A_PIER),
            total_adult_struc = sum(A_STRUC),
            total_adult_water = sum(A_WATER),
            total_juv_pier = sum(J_PIER),
            total_juv_struc = sum(J_STRUC),
            total_juv_water = sum(J_WATER),
            total_adult_s1 = sum(A_S1),
            total_adult_s2 = sum(A_S2),
            total_adult_s3 = sum(A_S3),
            total_juv_s1 = sum(J_S1),
            total_juv_s2 = sum(J_S2),
            total_juv_s3 = sum(J_S3),
            total_anglers = sum(ANGLERS),
            total_lines = sum(LINES)) %>%
  mutate(total_LPA = ifelse(total_anglers == 0, 0, total_lines/total_anglers))

means_DAY <- totals_per_survey %>%
  group_by(DAY) %>%
  summarise(n = n(),
            mean_anglers = mean(total_anglers),
            mean_lines = mean(total_lines),
            mean_LPA = mean(total_LPA),
            sd_anglers = sd(total_anglers),
            sd_lines = sd(total_lines),
            sd_LPA = sd(total_LPA),
            sem_anglers = sem(total_anglers),
            sem_lines = sem(total_lines),
            sem_LPA = sem(total_LPA))

# by section
totals_per_survey <- pelicandata %>%
  group_by(DATE, TIME, SECT) %>%
  summarise(total_pelicans = sum(A_PIER) + sum(A_STRUC) + sum(A_WATER) + sum(J_PIER) + sum(J_STRUC) + sum(J_WATER),
            total_pelicans_injured = sum(A_S1) + sum(A_S2) + sum(A_S3) + sum(J_S1) + sum(J_S2) + sum(J_S3),
            total_pelicans_dead = sum(A_DEAD) + sum(J_DEAD),
            total_adult = sum(A_PIER) + sum(A_STRUC) + sum(A_WATER),
            total_juv = sum(J_PIER) + sum(J_STRUC) + sum(J_WATER),
            total_adult_injured = sum(A_S1) + sum(A_S2) + sum(A_S3),
            total_juv_injured = sum(J_S1) + sum(J_S2) + sum(J_S3),
            total_adult_dead = sum(A_DEAD),
            total_juv_dead = sum(J_DEAD),
            total_adult_pier = sum(A_PIER),
            total_adult_struc = sum(A_STRUC),
            total_adult_water = sum(A_WATER),
            total_juv_pier = sum(J_PIER),
            total_juv_struc = sum(J_STRUC),
            total_juv_water = sum(J_WATER),
            total_adult_s1 = sum(A_S1),
            total_adult_s2 = sum(A_S2),
            total_adult_s3 = sum(A_S3),
            total_juv_s1 = sum(J_S1),
            total_juv_s2 = sum(J_S2),
            total_juv_s3 = sum(J_S3),
            total_anglers = sum(ANGLERS),
            total_lines = sum(LINES)) %>%
  mutate(total_LPA = ifelse(total_anglers == 0, 0, total_lines/total_anglers))

means_SECT <- totals_per_survey %>%
  group_by(SECT) %>%
  summarise(n = n(),
            mean_anglers = mean(total_anglers),
            mean_lines = mean(total_lines),
            mean_LPA = mean(total_LPA),
            sd_anglers = sd(total_anglers),
            sd_lines = sd(total_lines),
            sd_LPA = sd(total_LPA),
            sem_anglers = sem(total_anglers),
            sem_lines = sem(total_lines),
            sem_LPA = sem(total_LPA))




#### MODELS #### 


#### *- Data prep #### 

# Load cleaned pelican data
pelicandata <- read.csv("data/pelicans/pelican_data.csv")

# Remove Tuesday and Thursday surveys because they were not included in the pre-restriction season surveys
keep <- c("Wednesday","Friday","Saturday","Sunday")
pelicandata <- subset(pelicandata, DAY %in% keep)

# arrange data: unit of observation is Section per Date
countdata <- pelicandata %>%
  group_by(PERIOD, DATE, SECT, TIME, DAY) %>%
  summarise(total_pelicans = sum(A_PIER) + sum(A_STRUC) + sum(A_WATER) + sum(J_PIER) + sum(J_STRUC) + sum(J_WATER),
            total_pelicans_injured = sum(A_S1) + sum(A_S2) + sum(A_S3) + sum(J_S1) + sum(J_S2) + sum(J_S3),
            total_anglers = sum(ANGLERS),
            total_lines = sum(LINES)) %>%
  mutate(total_LPA = ifelse(total_anglers == 0, 0, total_lines/total_anglers))

# set reference levels
countdata <- countdata %>%
  mutate(TIME = factor(TIME, levels = c("Morning","Mid-day")),
         PERIOD = factor(PERIOD, levels = c("Unregulated","Regulated")),
         SECT = factor(SECT, levels = c("D","C","B","A")),
         DAY = factor(DAY, levels = c("Wednesday","Friday","Saturday","Sunday")))



#### *- Total Pelicans #### 

### Zero-inflated Model

# Load packages for using zero-inflated poisson regression
library(pscl)
library(boot)

pelicanmodel <- zeroinfl(total_pelicans ~ PERIOD + TIME + SECT + total_anglers + total_LPA, data = countdata)
summary(pelicanmodel)
exp(cbind("Odds ratio" = coef(pelicanmodel), confint.default(pelicanmodel, level = 0.95)))
# Log-likelihood: -1123 on 16 Df 

# confirm it's better than the null (intercept only model)
nullmodel <- update(pelicanmodel, . ~ 1)
pchisq(2 * (logLik(pelicanmodel) - logLik(nullmodel)), df = 6, lower.tail = FALSE)
# 'log Lik.' 1.157934e-64 (df=16)



#### *- Injured Pelicans #### 

injuredmodel <- zeroinfl(total_pelicans_injured ~ PERIOD + TIME + SECT + total_anglers + total_LPA, data = countdata)
summary(injuredmodel)
exp(cbind("Odds ratio" = coef(injuredmodel), confint.default(injuredmodel, level = 0.95)))
# Log-likelihood: -190.4 on 16 Df 

nullmodel <- update(injuredmodel, . ~ 1)
pchisq(2 * (logLik(injuredmodel) - logLik(nullmodel)), df = 6, lower.tail = FALSE)
# 'log Lik.' 4.502496e-08 (df=16)


#### *- Anglers #### 

# original model
anglermodel <- lm(total_anglers ~ SECT + DAY + TIME + PERIOD, data = countdata)
summary(anglermodel)
# adj R2 = 0.5451  

# check fit
par(mfrow = c(2,2))
plot(anglermodel)
# our total_anglers is very skewed

# apply square root transformation
countdata <- countdata %>%
  mutate(total_anglersSQRT = total_anglers^(1/2))

# transformed model
anglermodel2 <- lm(total_anglersSQRT ~ SECT + DAY + TIME + PERIOD, data = countdata)
summary(anglermodel2)
# adj R2 = 0.5829   

# check fit
plot(anglermodel2)
# better, but some notable outliers

# let's remove outliers from 2/10/2024 (rows 69-71)
countdata_reduced <- countdata[-c(61:64), ]

anglermodel3 <- lm(total_anglersSQRT ~ SECT + DAY + TIME + PERIOD, data = countdata_reduced)
summary(anglermodel3)
# adj R2 = 0.6235   

# check fit
plot(anglermodel3)
# much better

