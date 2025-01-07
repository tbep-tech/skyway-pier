library(tidyverse)
library(rstatix)


#### ANGLER CHARACTERISTICS ####

# import survey data
descriptivestats <- read.csv("data/anglers/angler_data_qual.csv")


#### *- Demographics ####

descriptivestats$AGEcat <- ifelse(descriptivestats$AGE <= 26, "Generation Z",
                                  ifelse(descriptivestats$AGE >= 27 & descriptivestats$AGE <= 42, "Millennial",
                                         ifelse(descriptivestats$AGE >= 43 & descriptivestats$AGE <= 58, "Generation X",
                                                ifelse(descriptivestats$AGE >= 59, "Baby Boomer+", NA))))
descriptivestats$AGEcat <- as.factor(descriptivestats$AGEcat)
descriptivestats$AGEcat <- ordered(descriptivestats$AGEcat, levels = c("Generation Z","Millennial","Generation X","Baby Boomer+"))

descriptivestats %>%
  group_by(RESIDENT) %>%
  summarise(count = n()) %>%
  drop_na() %>%
  mutate(pct = (count/sum(count))*100)
descriptivestats %>%
  group_by(GENDER) %>%
  summarise(count = n()) %>%
  drop_na() %>%
  mutate(pct = (count/sum(count))*100)
descriptivestats %>%
  group_by(AGEcat) %>%
  summarise(count = n()) %>%
  drop_na() %>%
  mutate(pct = (count/sum(count))*100)
descriptivestats %>%
  group_by(EMPLOY) %>%
  summarise(count = n()) %>%
  drop_na() %>%
  mutate(pct = (count/sum(count))*100)
descriptivestats %>%
  group_by(INCOME) %>%
  summarise(count = n()) %>%
  drop_na() %>%
  mutate(pct = (count/sum(count))*100)

#### *- Trip ####

descriptivestats %>%
  group_by(FREQUENCY) %>%
  summarise(count = n()) %>%
  drop_na() %>%
  mutate(pct = (count/sum(count))*100)
descriptivestats %>%
  group_by(REASON_FUN) %>%
  summarise(count = n()) %>%
  drop_na() %>%
  mutate(pct = (count/100)*100)
descriptivestats %>%
  group_by(REASON_FOOD) %>%
  summarise(count = n()) %>%
  drop_na() %>%
  mutate(pct = (count/100)*100)
descriptivestats %>%
  group_by(REASON_SELL) %>%
  summarise(count = n()) %>%
  drop_na() %>%
  mutate(pct = (count/100)*100)
descriptivestats %>%
  group_by(REASON_WATCH) %>%
  summarise(count = n()) %>%
  drop_na() %>%
  mutate(pct = (count/100)*100)
descriptivestats %>%
  group_by(REASON_OTHER) %>%
  summarise(count = n()) %>%
  drop_na() %>%
  mutate(pct = (count/100)*100)
descriptivestats %>%
  group_by(COMPANY_NONE) %>%
  summarise(count = n()) %>%
  drop_na() %>%
  mutate(pct = (count/100)*100)
descriptivestats %>%
  group_by(COMPANY_FRIEND) %>%
  summarise(count = n()) %>%
  drop_na() %>%
  mutate(pct = (count/100)*100)
descriptivestats %>%
  group_by(COMPANY_PARSIB) %>%
  summarise(count = n()) %>%
  drop_na() %>%
  mutate(pct = (count/100)*100)
descriptivestats %>%
  group_by(COMPANY_SPOUSE) %>%
  summarise(count = n()) %>%
  drop_na() %>%
  mutate(pct = (count/100)*100)
descriptivestats %>%
  group_by(COMPANY_KIDS) %>%
  summarise(count = n()) %>%
  drop_na() %>%
  mutate(pct = (count/100)*100)
descriptivestats %>%
  group_by(COMPANY_OTHER) %>%
  summarise(count = n()) %>%
  drop_na() %>%
  mutate(pct = (count/100)*100)
descriptivestats %>%
  group_by(SECTION) %>%
  summarise(count = n()) %>%
  drop_na() %>%
  mutate(pct = (count/sum(count))*100)

#### *- Psychosocial ####

# set factor levels for figures
descriptivestats <- descriptivestats %>%
  mutate(ISSUE_CONCERN = factor(ISSUE_CONCERN, levels = c("Strongly disagree","Disagree","Somewhat disagree","Somewhat agree","Agree","Strongly agree")),
         ISSUE_CONFIDENT = factor(ISSUE_CONFIDENT, levels = c("Strongly disagree","Disagree","Somewhat disagree","Somewhat agree","Agree","Strongly agree")),
         ISSUE_CUT = factor(ISSUE_CUT, levels = c("Strongly disagree","Disagree","Somewhat disagree","Somewhat agree","Agree","Strongly agree")),
         ISSUE_NORM = factor(ISSUE_NORM, levels = c("Strongly disagree","Disagree","Somewhat disagree","Somewhat agree","Agree","Strongly agree")),
         REG_DIFFICULT = factor(REG_DIFFICULT, levels = c("Strongly disagree","Disagree","Somewhat disagree","Somewhat agree","Agree","Strongly agree")),
         REG_SNORM = factor(REG_SNORM, levels = c("Strongly disagree","Disagree","Somewhat disagree","Somewhat agree","Agree","Strongly agree")),
         REG_REDUCE = factor(REG_REDUCE, levels = c("Strongly disagree","Disagree","Somewhat disagree","Somewhat agree","Agree","Strongly agree")),
         REG_DNORM = factor(REG_DNORM, levels = c("Strongly disagree","Disagree","Somewhat disagree","Somewhat agree","Agree","Strongly agree")),
         REG_AFFECT = factor(REG_AFFECT, levels = c("Strongly disagree","Disagree","Somewhat disagree","Somewhat agree","Agree","Strongly agree")),
         REG_ENOUGH = factor(REG_ENOUGH, levels = c("Strongly disagree","Disagree","Somewhat disagree","Somewhat agree","Agree","Strongly agree")),
         REG_CAUSE = factor(REG_CAUSE, levels = c("Strongly disagree","Disagree","Somewhat disagree","Somewhat agree","Agree","Strongly agree")),
         REG_ENFORCE = factor(REG_ENFORCE, levels = c("Strongly disagree","Disagree","Somewhat disagree","Somewhat agree","Agree","Strongly agree")),
         ACTION_GEAR = factor(ACTION_GEAR, levels = c("Not effective at all","Slightly effective","Moderately effective","Very effective","Extremely effective")),
         ACTION_ZONE = factor(ACTION_ZONE, levels = c("Not effective at all","Slightly effective","Moderately effective","Very effective","Extremely effective")),
         ACTION_ENFORCE = factor(ACTION_ENFORCE, levels = c("Not effective at all","Slightly effective","Moderately effective","Very effective","Extremely effective")),
         ACTION_DEMOLISH = factor(ACTION_DEMOLISH, levels = c("Not effective at all","Slightly effective","Moderately effective","Very effective","Extremely effective")),
         ACTION_VIDEO = factor(ACTION_VIDEO, levels = c("Not effective at all","Slightly effective","Moderately effective","Very effective","Extremely effective")))

psy_ISSUE_CONCERN <- descriptivestats %>%
  drop_na(ISSUE_CONCERN) %>% 
  group_by(ISSUE_CONCERN) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         VARIABLE = "CONCERN") %>%
  rename(SCALE = ISSUE_CONCERN)
psy_ISSUE_CONFIDENT <- descriptivestats %>%
  drop_na(ISSUE_CONFIDENT) %>% 
  group_by(ISSUE_CONFIDENT) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         VARIABLE = "CONFIDENT") %>%
  rename(SCALE = ISSUE_CONFIDENT)
psy_ISSUE_CUT <- descriptivestats %>%
  drop_na(ISSUE_CUT) %>% 
  group_by(ISSUE_CUT) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         VARIABLE = "CUT") %>%
  rename(SCALE = ISSUE_CUT)
psy_ISSUE_NORM <- descriptivestats %>%
  drop_na(ISSUE_NORM) %>% 
  group_by(ISSUE_NORM) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         VARIABLE = "NORM") %>%
  rename(SCALE = ISSUE_NORM)

issues <- rbind(psy_ISSUE_CONCERN, psy_ISSUE_CONFIDENT, psy_ISSUE_CUT, psy_ISSUE_NORM)

plot_issues <- ggplot(issues, aes(x=VARIABLE, y=pct, fill=SCALE)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("Strongly disagree" = "#983E12",
                               "Disagree" = "#D7632B",
                               "Somewhat disagree" = "#FFA67C",
                               "Somewhat agree" = "#7CACFF",
                               "Agree" = "#466EB4",
                               "Strongly agree" = "#244276")) +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        panel.grid  = element_blank())


psy_REG_DIFFICULT <- descriptivestats %>%
  drop_na(REG_DIFFICULT) %>% 
  group_by(REG_DIFFICULT) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         VARIABLE = "DIFFICULT") %>%
  rename(SCALE = REG_DIFFICULT)
psy_REG_SNORM <- descriptivestats %>%
  drop_na(REG_SNORM) %>% 
  group_by(REG_SNORM) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         VARIABLE = "SNORM") %>%
  rename(SCALE = REG_SNORM)
psy_REG_REDUCE <- descriptivestats %>%
  drop_na(REG_REDUCE) %>% 
  group_by(REG_REDUCE) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         VARIABLE = "REDUCE") %>%
  rename(SCALE = REG_REDUCE)
psy_REG_DNORM <- descriptivestats %>%
  drop_na(REG_DNORM) %>% 
  group_by(REG_DNORM) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         VARIABLE = "DNORM") %>%
  rename(SCALE = REG_DNORM)
psy_REG_AFFECT <- descriptivestats %>%
  drop_na(REG_AFFECT) %>% 
  group_by(REG_AFFECT) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         VARIABLE = "AFFECT") %>%
  rename(SCALE = REG_AFFECT)
psy_REG_ENOUGH <- descriptivestats %>%
  drop_na(REG_ENOUGH) %>% 
  group_by(REG_ENOUGH) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         VARIABLE = "ENOUGH") %>%
  rename(SCALE = REG_ENOUGH)
psy_REG_CAUSE <- descriptivestats %>%
  drop_na(REG_CAUSE) %>% 
  group_by(REG_CAUSE) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         VARIABLE = "CAUSE") %>%
  rename(SCALE = REG_CAUSE)
psy_REG_ENFORCE <- descriptivestats %>%
  drop_na(REG_ENFORCE) %>% 
  group_by(REG_ENFORCE) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         VARIABLE = "ENFORCE") %>%
  rename(SCALE = REG_ENFORCE)


regulations <- rbind(psy_REG_AFFECT, psy_REG_DIFFICULT, psy_REG_ENOUGH, psy_REG_CAUSE, psy_REG_SNORM, psy_REG_REDUCE, psy_REG_ENFORCE, psy_REG_DNORM)

plot_regs <- ggplot(regulations, aes(x=VARIABLE, y=pct, fill=SCALE)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("Strongly disagree" = "#983E12",
                               "Disagree" = "#D7632B",
                               "Somewhat disagree" = "#FFA67C",
                               "Somewhat agree" = "#7CACFF",
                               "Agree" = "#466EB4",
                               "Strongly agree" = "#244276")) +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        panel.grid  = element_blank())


psy_ACTION_GEAR <- descriptivestats %>%
  drop_na(ACTION_GEAR) %>% 
  group_by(ACTION_GEAR) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         VARIABLE = "GEAR") %>%
  rename(SCALE = ACTION_GEAR)
psy_ACTION_ZONE <- descriptivestats %>%
  drop_na(ACTION_ZONE) %>% 
  group_by(ACTION_ZONE) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         VARIABLE = "ZONE") %>%
  rename(SCALE = ACTION_ZONE)
psy_ACTION_ENFORCE <- descriptivestats %>%
  drop_na(ACTION_ENFORCE) %>% 
  group_by(ACTION_ENFORCE) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         VARIABLE = "ENFORCE") %>%
  rename(SCALE = ACTION_ENFORCE)
psy_ACTION_DEMOLISH <- descriptivestats %>%
  drop_na(ACTION_DEMOLISH) %>% 
  group_by(ACTION_DEMOLISH) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         VARIABLE = "DEMOLISH") %>%
  rename(SCALE = ACTION_DEMOLISH)
psy_ACTION_VIDEO <- descriptivestats %>%
  drop_na(ACTION_VIDEO) %>% 
  group_by(ACTION_VIDEO) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100,
         VARIABLE = "VIDEO") %>%
  rename(SCALE = ACTION_VIDEO)



actions <- rbind(psy_ACTION_GEAR, psy_ACTION_ZONE, psy_ACTION_ENFORCE, psy_ACTION_DEMOLISH, psy_ACTION_VIDEO)

plot_actions <- ggplot(actions, aes(x=VARIABLE, y=pct, fill=SCALE)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("Not effective at all" = "#C3D7FA",
                               "Slightly effective" = "#9EC0FB",
                               "Moderately effective" = "#6896E5",
                               "Very effective" = "#3667B7",
                               "Extremely effective" = "#244276")) +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        panel.grid  = element_blank())



#### *- Gear ####

descriptivestats <- descriptivestats %>%
  mutate(TACKLE = factor(TACKLE, levels = c("None of them", "One of them", "Two of them", "Three of them", "Four of them", "All of them")),
         TTACKLE = factor(TTACKLE, levels = c("None of them", "One of them", "Two of them", "Three of them", "Four of them", "All of them")))

gear_TACKLE <- descriptivestats %>%
  drop_na(GROUP) %>% 
  group_by(GROUP, TACKLE) %>%
  summarise(n = n())

gear_TTACKLE <- descriptivestats %>%
  drop_na(GROUP) %>% 
  group_by(GROUP, TTACKLE) %>%
  summarise(n = n())

descriptivestats <- descriptivestats %>%
  mutate(TACKLE.Q = ifelse(TACKLE == "None of them", 0, 
                           ifelse(TACKLE == "One of them", 1, 
                                  ifelse(TACKLE == "Two of them", 2, 
                                         ifelse(TACKLE == "Three of them", 3, 
                                                ifelse(TACKLE == "Four of them", 4, 
                                                       ifelse(TACKLE == "All of them" & GROUP == "Control", 4, 
                                                              ifelse(TACKLE == "All of them" & GROUP == "Treatment", 5, NA))))))),
         TTACKLE.Q = ifelse(TTACKLE == "None of them", 0, 
                            ifelse(TTACKLE == "One of them", 1, 
                                   ifelse(TTACKLE == "Two of them", 2, 
                                          ifelse(TTACKLE == "Three of them", 3, 
                                                 ifelse(TTACKLE == "Four of them", 4, 
                                                        ifelse(TTACKLE == "All of them" & GROUP == "Control", 4, 
                                                               ifelse(TTACKLE == "All of them" & GROUP == "Treatment", 5, NA))))))))

sem <- function(x) sd(x)/sqrt(length(x))

mean_TACKLE <- descriptivestats %>%
  drop_na(GROUP) %>% 
  group_by(GROUP) %>%
  summarise(TACKLEmean = mean(TACKLE.Q),
            TACKLEsd = sd(TACKLE.Q),
            TACKLEsem = sem(TACKLE.Q))
mean_TACKLE

mean_TTACKLE <- descriptivestats %>%
  drop_na(GROUP) %>% 
  group_by(GROUP) %>%
  summarise(TTACKLEmean = mean(TTACKLE.Q),
            TTACKLEsd = sd(TTACKLE.Q),
            TTACKLEsem = sem(TTACKLE.Q))
mean_TTACKLE

# Mann-Whitney U test
wilcox.test(TACKLE.Q ~ GROUP, data = descriptivestats)
wilcox.test(TTACKLE.Q ~ GROUP, data = descriptivestats)
#sink()

modeltackle <- lm(TACKLE.Q ~ GROUP + AGE + GENDER + REASON_FOOD + INCOME, data = descriptivestats)
summary(modeltackle)
#par(mfrow = c(2,2))
#plot(modeltackle)

modelttackle <- lm(TTACKLE.Q ~ GROUP + AGE + GENDER + REASON_FOOD + INCOME, data = descriptivestats)
summary(modelttackle)
#par(mfrow = c(2,2))
#plot(modelttackle)



#### *- Pelican observations ####

anglers_pelicans <- read.csv("data/anglers/angler_data_pelicans_aligned.csv")
pelicanestimates <- anglers_pelicans[,-4]
rownames(pelicanestimates) <- anglers_pelicans[,4]
pelicanestimates <- subset(pelicanestimates, select = c(pelican_estimate))

estimatebins <- anglers_pelicans %>%
  group_by(objectiveSECT_cat, subjective_cat) %>%
  summarise(Count = n()) %>%
  mutate(Pct = (Count/74)*100)

estimates <- pelicanestimates %>%
  group_by(pelican_estimate) %>%
  summarise(Count = n()) %>%
  mutate(Pct = (Count/sum(Count))*100)


tblestimates <- table(anglers_pelicans$SECTION,anglers_pelicans$pelican_estimate)
tblestimates
fisher.test(tblestimates)



#### AUDIENCE SEGMENTATION ####

library(tidyverse)
library(lavaan)
library(MVN)
library(FactoMineR)
library(psych)
library(semPlot)
library(psy)
library(cluster)
library(factoextra)
library(dendextend)
library(gridExtra)
library(doBy)
library(matrixStats)
library(pscl)
library(vcd)
library(data.table)
library(FSA)
library(rstatix)


# import survey data
anglers_quant <- read.csv("data/anglers/angler_data_quant.csv")
quantdata <- anglers_quant[,-1]
rownames(quantdata) <- anglers_quant[,1]

anglers_pelicans <- read.csv("data/anglers/angler_data_pelicans_aligned.csv")
pelicanestimates <- anglers_pelicans[,-4]
rownames(pelicanestimates) <- anglers_pelicans[,4]
pelicanestimates <- subset(pelicanestimates, select = c(pelican_estimate))

# Add alignmened subjective/objective pelican estimates to angler survey data
mergeddata <- merge(quantdata, pelicanestimates, by = "row.names", all.x = TRUE)
modeldata <- mergeddata[,-1]
rownames(modeldata) <- mergeddata[,1]


#### *- Clustering ####

clusterdata <- modeldata[c("ISSUE_CONCERN.Q","ISSUE_CONFIDENT.Q","ISSUE_CUT.Q","ISSUE_NORM.Q")] %>%
  na.omit()


# We need to scale the values to z-scores, to avoid the clustering algorithm depending on an arbitrary variable unit
data_scaled <- as.data.frame(scale(clusterdata))

# Get confidence interval of mean
error <- qnorm(0.975)*(sd(data_scaled$ISSUE_CONCERN.Q)/sqrt(length(data_scaled$ISSUE_CONCERN.Q)))
left <- mean(data_scaled$ISSUE_CONCERN.Q)-error
right <- mean(data_scaled$ISSUE_CONCERN.Q)+error
left
right
# CONFIDENCE INTERVAL IS: MEAN = 0 +/- 0.1990042

# Compute hierarchical agglomerative cluster analysis with agnes()
#   Similar to hclust() but we also get the agglomerative coefficient, which measures the amount of clustering structure found 
#   (values closer to 1 suggest strong clustering structure)

#-----first, let's see which hierarchical clustering methods can identify stronger clustering structures-----#

# Methods to assess
test <- c("average", "single", "complete", "ward", "weighted", "gaverage")
names(test) <- c("average", "single", "complete", "ward", "weighted", "gaverage")

# Function to compute the agglomerative coefficient ($ac) (0 = poor clustering, 1 = strong clustering)
ac1 <- function(x) {
  agnes(data_scaled, method = x)$ac
}

map_dbl(test, ac1)
#### Ward's is the best (as expected)


#-----RUN ANALYSIS-----#

issuecluster <- agnes(data_scaled, method = "ward")
pltree(issuecluster, cex = 0.6, hang = -1, main = "Dendrogram of agnes()")


#-----Determine cluster cutoff-----#
# Elbow method
s1 <- fviz_nbclust(data_scaled, FUNcluster = hcut, method = "wss")

# Average silhouette method
s2 <- fviz_nbclust(data_scaled, FUNcluster = hcut, method = "silhouette")

# Gap statistic method
gap_stat <- clusGap(data_scaled, FUNcluster = hcut, K.max = 10, B = 500)
s3 <- fviz_gap_stat(gap_stat)

grid.arrange(s1, s2, s3, nrow = 1)


# Cut tree into X groups
sub_grp2 <- cutree(as.hclust(issuecluster), k = 2)
sub_grp3 <- cutree(as.hclust(issuecluster), k = 3)
sub_grp4 <- cutree(as.hclust(issuecluster), k = 4)
sub_grp5 <- cutree(as.hclust(issuecluster), k = 5)

# Number of members in each cluster
table(sub_grp2)
table(sub_grp3)
table(sub_grp4)
table(sub_grp5)

# Visualize the clustering in a scatter plot
p1 <- fviz_cluster(list(data = data_scaled, cluster = sub_grp2), geom = "point") + ggtitle("k = 2")
p2 <- fviz_cluster(list(data = data_scaled, cluster = sub_grp3), geom = "point") + ggtitle("k = 3")
p3 <- fviz_cluster(list(data = data_scaled, cluster = sub_grp4), geom = "point") + ggtitle("k = 4")
p4 <- fviz_cluster(list(data = data_scaled, cluster = sub_grp5), geom = "point") + ggtitle("k = 5")
grid.arrange(p1, p2, p3, p4, nrow = 2)

# Add the cluster each observation belongs to to the standardized data
scaled_cluster5data <- as.data.frame(data_scaled %>%
                                       mutate(cluster5 = sub_grp5)) %>%
  head(n = 97)

check <- scaled_cluster5data %>%
  group_by(cluster5) %>%
  summarize(Concern = mean(ISSUE_CONCERN.Q),
            Confident = mean(ISSUE_CONFIDENT.Q),
            Cut = mean(ISSUE_CUT.Q),
            Norm = mean(ISSUE_NORM.Q))
check <- melt(setDT(check), id.vars = c("cluster5"), variable.name = "variable")
clustermeans <- ggplot(check, aes(fill=variable, y=value, x=cluster5)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_hline(yintercept = c(-1, -0.1990042, 0.1990042, 1))

# Merge with model dataset
completedata <- merge(modeldata, scaled_cluster5data, by = "row.names", all = TRUE)
completedata <- completedata[!is.na(completedata$cluster5),]
completedata$cluster5 <- as.factor(completedata$cluster5)


#### *- Cluster characteristics ####

completedata$AGEcat <- ifelse(completedata$AGE <= 26, "Generation Z",
                              ifelse(completedata$AGE >= 27 & completedata$AGE <= 42, "Millennial",
                                     ifelse(completedata$AGE >= 43 & completedata$AGE <= 58, "Generation X",
                                            ifelse(completedata$AGE >= 59, "Baby Boomer+", NA))))
completedata$AGEcat <- as.factor(completedata$AGEcat)
completedata$AGEcat <- relevel(completedata$AGEcat, ref = "Baby Boomer+")

completedata$GENDERcat <- ifelse(completedata$MALE == 1, "Male",
                                 ifelse(completedata$MALE == 0, "Female", NA))
completedata$GENDERcat <- as.factor(completedata$GENDERcat)
completedata$GENDERcat <- relevel(completedata$GENDERcat, ref = "Female")

completedata$EMPLOYcat <- ifelse(completedata$FULLTIME == 1, "Full-time",
                                 ifelse(completedata$FULLTIME == 0, "Not Full-time", NA))
completedata$EMPLOYcat <- as.factor(completedata$EMPLOYcat)
completedata$EMPLOYcat <- relevel(completedata$EMPLOYcat, ref = "Not Full-time")

completedata$RESIDENTcat <- ifelse(completedata$FLRESIDENT == 1, "Florida Resident",
                                   ifelse(completedata$FLRESIDENT == 0, "Visitor", NA))
completedata$RESIDENTcat <- as.factor(completedata$RESIDENTcat)
completedata$RESIDENTcat <- relevel(completedata$RESIDENTcat, ref = "Visitor")

completedata$FOODcat <- ifelse(completedata$FOOD == 1, "Fishing for Food",
                               ifelse(completedata$FOOD == 0, "Not Fishing for Food", NA))
completedata$FOODcat <- as.factor(completedata$FOODcat)
completedata$FOODcat <- relevel(completedata$FOODcat, ref = "Not Fishing for Food")

completedata$COMPANYcat <- ifelse(completedata$SOCIAL == 1, "Fishing with Others",
                                  ifelse(completedata$SOCIAL == 0, "Fishing Alone", NA))
completedata$COMPANYcat <- as.factor(completedata$COMPANYcat)
completedata$COMPANYcat <- relevel(completedata$COMPANYcat, ref = "Fishing Alone")

completedata$INCOMEcat <- as.factor(completedata$INCOME3)
completedata$INCOMEcat <- relevel(completedata$INCOMEcat, ref = "$50,001 - $75,000")

completedata$FREQUENCYcat <- as.factor(completedata$FREQUENCY3)
completedata$FREQUENCYcat <- relevel(completedata$FREQUENCYcat, ref = "Rarely")

completedata$SECTIONcat <- as.factor(completedata$SECTION)
completedata$SECTIONcat <- relevel(completedata$SECTIONcat, ref = "C")

completedata$ESTIMATEcat <- as.factor(completedata$pelican_estimate)
completedata$ESTIMATEcat <- relevel(completedata$ESTIMATEcat, ref = "About right")


kruskal.test(ACTION_GEAR.Q ~ cluster5, data = completedata)
kruskal.test(ACTION_ZONE.Q ~ cluster5, data = completedata)
kruskal.test(ACTION_ENFORCE.Q ~ cluster5, data = completedata)
kruskal.test(ACTION_DEMOLISH.Q ~ cluster5, data = completedata)
kruskal.test(ACTION_VIDEO.Q ~ cluster5, data = completedata)
kruskal.test(REG_AFFECT.Q ~ cluster5, data = completedata)
kruskal.test(REG_DIFFICULT.Q ~ cluster5, data = completedata)
kruskal.test(REG_ENOUGH.Q ~ cluster5, data = completedata)
kruskal.test(REG_CAUSE.Q ~ cluster5, data = completedata)
kruskal.test(REG_SNORM.Q ~ cluster5, data = completedata)
kruskal.test(REG_REDUCE.Q ~ cluster5, data = completedata)
kruskal.test(REG_ENFORCE.Q ~ cluster5, data = completedata)
kruskal.test(REG_DNORM.Q ~ cluster5, data = completedata)
kruskal.test(AGE ~ cluster5, data = completedata)

dunnTest(ACTION_GEAR.Q ~ cluster5, data = completedata, method = "bonferroni")
dunnTest(REG_AFFECT.Q ~ cluster5, data = completedata, method = "bonferroni")
dunnTest(REG_DIFFICULT.Q ~ cluster5, data = completedata, method = "bonferroni")
dunnTest(REG_ENOUGH.Q ~ cluster5, data = completedata, method = "bonferroni")
dunnTest(REG_CAUSE.Q ~ cluster5, data = completedata, method = "bonferroni")
dunnTest(REG_SNORM.Q ~ cluster5, data = completedata, method = "bonferroni")
dunnTest(REG_REDUCE.Q ~ cluster5, data = completedata, method = "bonferroni")
dunnTest(REG_DNORM.Q ~ cluster5, data = completedata, method = "bonferroni")

tblCOMPANYcat <- table(completedata$cluster5,completedata$COMPANYcat)
tblCOMPANYcat
fisher.test(tblCOMPANYcat)

tblFREQUENCYcat <- table(completedata$cluster5,completedata$FREQUENCYcat)
tblFREQUENCYcat
fisher.test(tblFREQUENCYcat)

tblINCOMEcat <- table(completedata$cluster5,completedata$INCOMEcat)
tblINCOMEcat
fisher.test(tblINCOMEcat)

tblFOODcat <- table(completedata$cluster5,completedata$FOODcat)
tblFOODcat
fisher.test(tblFOODcat)

tblSECTIONcat <- table(completedata$cluster5,completedata$SECTIONcat)
tblSECTIONcat
fisher.test(tblSECTIONcat)

tblESTIMATEcat <- table(completedata$cluster5,completedata$ESTIMATEcat)
tblESTIMATEcat
fisher.test(tblESTIMATEcat)


sem <- function(x) sd(x)/sqrt(length(x))

means_ACTION_GEAR.Q <- completedata %>%
  drop_na(ACTION_GEAR.Q) %>%
  group_by(cluster5) %>%
  summarise(Mean = mean(ACTION_GEAR.Q),
            SD = sd(ACTION_GEAR.Q),
            SEM = sem(ACTION_GEAR.Q)) %>%
  mutate(Topic = "ACTION_GEAR.Q")

means_ACTION_ZONE.Q <- completedata %>%
  drop_na(ACTION_ZONE.Q) %>%
  group_by(cluster5) %>%
  summarise(Mean = mean(ACTION_ZONE.Q),
            SD = sd(ACTION_ZONE.Q),
            SEM = sem(ACTION_ZONE.Q)) %>%
  mutate(Topic = "ACTION_ZONE.Q")

means_ACTION_DEMOLISH.Q <- completedata %>%
  drop_na(ACTION_DEMOLISH.Q) %>%
  group_by(cluster5) %>%
  summarise(Mean = mean(ACTION_DEMOLISH.Q),
            SD = sd(ACTION_DEMOLISH.Q),
            SEM = sem(ACTION_DEMOLISH.Q)) %>%
  mutate(Topic = "ACTION_DEMOLISH.Q")

means_ACTION_ENFORCE.Q <- completedata %>%
  drop_na(ACTION_ENFORCE.Q) %>%
  group_by(cluster5) %>%
  summarise(Mean = mean(ACTION_ENFORCE.Q),
            SD = sd(ACTION_ENFORCE.Q),
            SEM = sem(ACTION_ENFORCE.Q)) %>%
  mutate(Topic = "ACTION_ENFORCE.Q")

means_ACTION_VIDEO.Q <- completedata %>%
  drop_na(ACTION_VIDEO.Q) %>%
  group_by(cluster5) %>%
  summarise(Mean = mean(ACTION_VIDEO.Q),
            SD = sd(ACTION_VIDEO.Q),
            SEM = sem(ACTION_VIDEO.Q)) %>%
  mutate(Topic = "ACTION_VIDEO.Q")

means_REG_AFFECT.Q <- completedata %>%
  drop_na(REG_AFFECT.Q) %>%
  group_by(cluster5) %>%
  summarise(Mean = mean(REG_AFFECT.Q),
            SD = sd(REG_AFFECT.Q),
            SEM = sem(REG_AFFECT.Q)) %>%
  mutate(Topic = "REG_AFFECT.Q")

means_REG_DIFFICULT.Q <- completedata %>%
  drop_na(REG_DIFFICULT.Q) %>%
  group_by(cluster5) %>%
  summarise(Mean = mean(REG_DIFFICULT.Q),
            SD = sd(REG_DIFFICULT.Q),
            SEM = sem(REG_DIFFICULT.Q)) %>%
  mutate(Topic = "REG_DIFFICULT.Q")

means_REG_ENOUGH.Q <- completedata %>%
  drop_na(REG_ENOUGH.Q) %>%
  group_by(cluster5) %>%
  summarise(Mean = mean(REG_ENOUGH.Q),
            SD = sd(REG_ENOUGH.Q),
            SEM = sem(REG_ENOUGH.Q)) %>%
  mutate(Topic = "REG_ENOUGH.Q")

means_REG_CAUSE.Q <- completedata %>%
  drop_na(REG_CAUSE.Q) %>%
  group_by(cluster5) %>%
  summarise(Mean = mean(REG_CAUSE.Q),
            SD = sd(REG_CAUSE.Q),
            SEM = sem(REG_CAUSE.Q)) %>%
  mutate(Topic = "REG_CAUSE.Q")

means_REG_REDUCE.Q <- completedata %>%
  drop_na(REG_REDUCE.Q) %>%
  group_by(cluster5) %>%
  summarise(Mean = mean(REG_REDUCE.Q),
            SD = sd(REG_REDUCE.Q),
            SEM = sem(REG_REDUCE.Q)) %>%
  mutate(Topic = "REG_REDUCE.Q")

means_REG_ENFORCE.Q <- completedata %>%
  drop_na(REG_ENFORCE.Q) %>%
  group_by(cluster5) %>%
  summarise(Mean = mean(REG_ENFORCE.Q),
            SD = sd(REG_ENFORCE.Q),
            SEM = sem(REG_ENFORCE.Q)) %>%
  mutate(Topic = "REG_ENFORCE.Q")

means_REG_DNORM.Q <- completedata %>%
  drop_na(REG_DNORM.Q) %>%
  group_by(cluster5) %>%
  summarise(Mean = mean(REG_DNORM.Q),
            SD = sd(REG_DNORM.Q),
            SEM = sem(REG_DNORM.Q)) %>%
  mutate(Topic = "REG_DNORM.Q")

means_REG_SNORM.Q <- completedata %>%
  drop_na(REG_SNORM.Q) %>%
  group_by(cluster5) %>%
  summarise(Mean = mean(REG_SNORM.Q),
            SD = sd(REG_SNORM.Q),
            SEM = sem(REG_SNORM.Q)) %>%
  mutate(Topic = "REG_SNORM.Q")


means_characteristics <- rbind(means_ACTION_GEAR.Q,
                               means_ACTION_ZONE.Q,
                               means_ACTION_DEMOLISH.Q,
                               means_ACTION_ENFORCE.Q,
                               means_ACTION_VIDEO.Q,
                               means_REG_AFFECT.Q,
                               means_REG_DIFFICULT.Q,
                               means_REG_ENOUGH.Q,
                               means_REG_CAUSE.Q,
                               means_REG_REDUCE.Q,
                               means_REG_ENFORCE.Q,
                               means_REG_DNORM.Q,
                               means_REG_SNORM.Q)
group.colors <- c("#D7642C", "#00A0E1", "#000000", "#E6A532", "#466EB4")
characteristics <- ggplot(means_characteristics, aes(x=cluster5, y=Mean, color=cluster5)) + 
  geom_pointrange(aes(ymin=Mean-SEM, ymax=Mean+SEM)) +
  scale_x_discrete(limits = c("1", "4", "3", "2", "5")) +
  scale_color_manual(values=group.colors) +
  facet_wrap(vars(Topic), nrow = 3)



#### *- Cluster membership model ####

# Each individual cluster needs a binary indicator that says it belongs to that one
completedata$cluster <- completedata$cluster5
completedata$cluster5 <- NULL
completedata$cluster1 <- ifelse(completedata$cluster=="1",1,0)
completedata$cluster2 <- ifelse(completedata$cluster=="2",1,0)
completedata$cluster3 <- ifelse(completedata$cluster=="3",1,0)
completedata$cluster4 <- ifelse(completedata$cluster=="4",1,0)
completedata$cluster5 <- ifelse(completedata$cluster=="5",1,0)

# Create separate datasets for each cluster group
C5cluster1 <- subset(completedata, select=c(ACTION_GEAR.Q,REG_AFFECT.Q,REG_DIFFICULT.Q,REG_ENOUGH.Q,REG_CAUSE.Q,REG_SNORM.Q,REG_REDUCE.Q,REG_DNORM.Q,COMPANYcat,FOODcat,cluster1))
C5cluster2 <- subset(completedata, select=c(ACTION_GEAR.Q,REG_AFFECT.Q,REG_DIFFICULT.Q,REG_ENOUGH.Q,REG_CAUSE.Q,REG_SNORM.Q,REG_REDUCE.Q,REG_DNORM.Q,COMPANYcat,FOODcat,cluster2))
C5cluster3 <- subset(completedata, select=c(ACTION_GEAR.Q,REG_AFFECT.Q,REG_DIFFICULT.Q,REG_ENOUGH.Q,REG_CAUSE.Q,REG_SNORM.Q,REG_REDUCE.Q,REG_DNORM.Q,COMPANYcat,FOODcat,cluster3))
C5cluster4 <- subset(completedata, select=c(ACTION_GEAR.Q,REG_AFFECT.Q,REG_DIFFICULT.Q,REG_ENOUGH.Q,REG_CAUSE.Q,REG_SNORM.Q,REG_REDUCE.Q,REG_DNORM.Q,COMPANYcat,FOODcat,cluster4))
C5cluster5 <- subset(completedata, select=c(ACTION_GEAR.Q,REG_AFFECT.Q,REG_DIFFICULT.Q,REG_ENOUGH.Q,REG_CAUSE.Q,REG_SNORM.Q,REG_REDUCE.Q,REG_DNORM.Q,COMPANYcat,FOODcat,cluster5))

# Check for number of missing values (if it's few then we can just remove the observations)
sapply(C5cluster1, function(x) sum(is.na(x)))
sapply(C5cluster2, function(x) sum(is.na(x)))
sapply(C5cluster3, function(x) sum(is.na(x)))
sapply(C5cluster4, function(x) sum(is.na(x)))
sapply(C5cluster5, function(x) sum(is.na(x)))
data1 <- na.omit(C5cluster1)
data2 <- na.omit(C5cluster2)
data3 <- na.omit(C5cluster3)
data4 <- na.omit(C5cluster4)
data5 <- na.omit(C5cluster5)

## Run the logistic regressions

# Cluster 1

# Full Model

model1 <- glm(cluster1 ~ ACTION_GEAR.Q+REG_AFFECT.Q+REG_DIFFICULT.Q+REG_ENOUGH.Q+REG_CAUSE.Q+REG_SNORM.Q+REG_REDUCE.Q+REG_DNORM.Q+COMPANYcat+FOODcat, family = binomial(link = "logit"), data = data1)
summary(model1)
pR2(model1)
# AIC = 98.565; McFadden R2 = 0.3035557      

# Optimal Reduced Model

model1 <- glm(cluster1 ~ ACTION_GEAR.Q+REG_ENOUGH.Q+COMPANYcat, family = binomial(link = "logit"), data = data1)
summary(model1)
pR2(model1)
# AIC = 89.469; McFadden R2 = 0.2589438                           


# Cluster 2

# Full Model

model2 <- glm(cluster2 ~ ACTION_GEAR.Q+REG_AFFECT.Q+REG_DIFFICULT.Q+REG_ENOUGH.Q+REG_CAUSE.Q+REG_SNORM.Q+REG_REDUCE.Q+REG_DNORM.Q+COMPANYcat+FOODcat, family = binomial(link = "logit"), data = data2)
summary(model2)
pR2(model2)
# AIC = 93.569; McFadden R2 = 0.2834473         

# Optimal Reduced Model

model2 <- glm(cluster2 ~ REG_DIFFICULT.Q+REG_REDUCE.Q+COMPANYcat, family = binomial(link = "logit"), data = data2)
summary(model2)
pR2(model2)
# AIC = 82.992; McFadden R2 = 0.2491824                                                



# Cluster 3

# Full Model

model3 <- glm(cluster3 ~ ACTION_GEAR.Q+REG_AFFECT.Q+REG_DIFFICULT.Q+REG_ENOUGH.Q+REG_CAUSE.Q+REG_SNORM.Q+REG_REDUCE.Q+REG_DNORM.Q+COMPANYcat+FOODcat, family = binomial(link = "logit"), data = data3)
summary(model3)
pR2(model3)
# AIC = 96.613; McFadden R2 = 0.1274565         

# Optimal Reduced Model

model3 <- glm(cluster3 ~ ACTION_GEAR.Q+REG_REDUCE.Q+COMPANYcat, family = binomial(link = "logit"), data = data3)
summary(model3)
pR2(model3)
# AIC = 87.619; McFadden R2 = 0.06890631                                                



# Cluster 4

# Full Model

model4 <- glm(cluster4 ~ ACTION_GEAR.Q+REG_AFFECT.Q+REG_DIFFICULT.Q+REG_ENOUGH.Q+REG_CAUSE.Q+REG_SNORM.Q+REG_REDUCE.Q+REG_DNORM.Q+COMPANYcat+FOODcat, family = binomial(link = "logit"), data = data4)
summary(model4)
pR2(model4)
# AIC = 49.02; McFadden R2 = 0.4923882         

# Optimal Reduced Model

model4 <- glm(cluster4 ~ ACTION_GEAR.Q+REG_DIFFICULT.Q+REG_CAUSE.Q+COMPANYcat, family = binomial(link = "logit"), data = data4)
summary(model4)
pR2(model4)
# AIC = 39.438; McFadden R2 = 0.4469648                                          


# Cluster 5

# Full Model

model5 <- glm(cluster5 ~ ACTION_GEAR.Q+REG_AFFECT.Q+REG_DIFFICULT.Q+REG_ENOUGH.Q+REG_CAUSE.Q+REG_SNORM.Q+REG_REDUCE.Q+REG_DNORM.Q+COMPANYcat+FOODcat, family = binomial(link = "logit"), data = data5)
summary(model5)
pR2(model5)
# AIC = 62.772; McFadden R2 = 0.2927785         

# Optimal Reduced Model

model5 <- glm(cluster5 ~ REG_DIFFICULT.Q+REG_SNORM.Q+COMPANYcat, family = binomial(link = "logit"), data = data5)
summary(model5)
pR2(model5)
# AIC = 51.774; McFadden R2 = 0.2407203                                                


