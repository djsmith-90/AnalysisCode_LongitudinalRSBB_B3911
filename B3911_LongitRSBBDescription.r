### Script for longitudinal description of ALSPAC's RSBB data (B3911)
### Created 5/4/2022 by Dan Major-Smith
### R version 4.0.4

## Analysis plan for this paper has been pre-registered on the OSF: https://osf.io/w9t2y/


###########################################################################################
#### Clear workspace, install/load packages, and set working directory
rm(list = ls())

setwd("X:\\Studies\\RSBB Team\\Dan\\B3911 - Longitudinal descriptive study")

#install.packages("tidyverse")
library(tidyverse)

## For the sankey plots (also know as 'alluvial' plots, will use the package 'ggalluvial' - see: https://cran.r-project.org/web/packages/ggalluvial/vignettes/ggalluvial.html)
#install.packages("ggalluvial")
library(ggalluvial)



###########################################################################################
#### Read in the raw data, and start processing/analysing the data

data_raw <- read_csv("Longit_RSBB_B3911.csv")

data <- data_raw
head(data)
glimpse(data)


## Want to drop data from one mother if linked to two pregnancies (else data possibly repeated)
table(data$mult_mum_Y, useNA = "ifany")

data <- data[data$mult_mum_Y != "Yes, drop these mult mums", ]

## Also drop data if mother or partner withdrew consent for data to be used
table(data$d810, data$pb150)

data <- data %>%
  mutate(drop = ifelse((d810 == ".a" & !is.na(d810)) | (pb150 == ".c" & !is.na(pb150)), 1, 0)) %>%
  filter(drop == 0) %>%
  select(-drop)


#### Belief in God/a divine power - Responses: Yes vs Not sure vs No

### Mothers

## Pregnancy
table(data$d810, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(d810 = na_if(d810, "-1")) %>%
  mutate(d810 = factor(d810, levels = c("Yes", "Not sure", "No")))

table(data$d810, useNA = "ifany")

## Age 5
table(data$k6240, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(k6240 = na_if(k6240, "No response")) %>%
  mutate(k6240 = na_if(k6240, "Not completed")) %>%
  mutate(k6240 = na_if(k6240, "Triplet / quadruplet")) %>%
  mutate(k6240 = recode(k6240, "No, not at all" = "No")) %>%
  mutate(k6240 = factor(k6240, levels = c("Yes", "Not sure", "No")))

table(data$k6240, useNA = "ifany")

## Age 6
table(data$l7040, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(l7040 = na_if(l7040, "No response")) %>%
  mutate(l7040 = na_if(l7040, "Not completed")) %>%
  mutate(l7040 = na_if(l7040, "Triplet / quadruplet")) %>%
  mutate(l7040 = recode(l7040, "No, not at all" = "No")) %>%
  mutate(l7040 = factor(l7040, levels = c("Yes", "Not sure", "No")))

table(data$l7040, useNA = "ifany")

## Age 9
table(data$p4040, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(p4040 = na_if(p4040, "No response")) %>%
  mutate(p4040 = na_if(p4040, "Not completed")) %>%
  mutate(p4040 = na_if(p4040, "Triplet / quadruplet")) %>%
  mutate(p4040 = factor(p4040, levels = c("Yes", "Not sure", "No")))

table(data$p4040, useNA = "ifany")

## 2019 (Age 28)
table(data$Y3000, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(Y3000 = na_if(Y3000, "Missing")) %>%
  mutate(Y3000 = na_if(Y3000, "Missed whole section C")) %>%
  mutate(Y3000 = na_if(Y3000, "Mother of trip/quad")) %>%
  mutate(Y3000 = na_if(Y3000, "Questionnaire not completed")) %>%
  mutate(Y3000 = na_if(Y3000, "Unresolvable")) %>%
  mutate(Y3000 = factor(Y3000, levels = c("Yes", "Not sure", "No")))

table(data$Y3000, useNA = "ifany")


## Turn this data into a sankey plot - Both for pregnancy to 2019, and just from pregnancy to age 9 (as less missing data)

# First, have to convert the data to the correct format and remove any missing values. Need to have:
#   1) Variable with time-point
#   2) Frequency of said cell/response for each time-point
#   3) A number which uniquely defines said response at each time-point
#   4) The response at each time-point

# To do this, probably easier to first convert the data to summary wide format, then convert to long/lode format. Age age 5 and 6 are very close together, to get a better sense of change over time will drop the age 6 data.
data_temp_age9 <- data %>%
  select(d810, k6240, p4040) %>%
  rename(preg = d810, age5 = k6240, age9 = p4040) %>%
  filter(complete.cases(preg, age5, age9)) %>%
  group_by(preg, age5, age9) %>%
  summarise(freq = n())

summary(data_temp_age9)
head(data_temp_age9)

data_temp_lodes_age9 <- to_lodes_form(data_temp_age9, axes = 1:3, id = "traj")
data_temp_lodes_age9 <- data_temp_lodes_age9 %>%
  rename(time = x, Response = stratum)
head(data_temp_lodes_age9)
summary(data_temp_lodes_age9)

belief_age9 <- ggplot(data_temp_lodes_age9,
                      aes(x = time, stratum = Response, alluvium = traj,
                          y = freq,
                          fill = Response, label = Response)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "none") +
  xlab("Questionnaire time-point") + ylab("Frequency") +
  ggtitle("Belief in God/a divine power - Mothers") + theme(plot.title = element_text(hjust = 0.5))

belief_age9

# Save this plot
pdf("./Results/ReligBelief_Age9_Mothers.pdf", height = 6, width = 10)
plot(belief_age9)
dev.off()

  
## Displaying these results as standard cross-tabs (using only complete cases, so data is identical to that in the sankey plot)

# preg by age 5
data %>%
  select(d810, k6240, p4040) %>%
  rename(preg = d810, age5 = k6240, age9 = p4040) %>%
  filter(complete.cases(preg, age5, age9)) %>%
  group_by(preg, age5) %>%
  summarise(freq = n())

# Age 5 by age 9
data %>%
  select(d810, k6240, p4040) %>%
  rename(preg = d810, age5 = k6240, age9 = p4040) %>%
  filter(complete.cases(preg, age5, age9)) %>%
  group_by(age5, age9) %>%
  summarise(freq = n())

# Proportions at each time-point
tab_age9 <-data %>%
  select(d810, k6240, p4040) %>%
  rename(preg = d810, age5 = k6240, age9 = p4040) %>%
  filter(complete.cases(preg, age5, age9))
summary(tab_age9)


### Now repeat, but including 2019 RSBB data
data_temp_age28 <- data %>%
  select(d810, k6240, p4040, Y3000) %>%
  rename(preg = d810, age5 = k6240, age9 = p4040, age28 = Y3000) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(preg, age5, age9, age28) %>%
  summarise(freq = n())

summary(data_temp_age28)
head(data_temp_age28)

data_temp_lodes_age28 <- to_lodes_form(data_temp_age28, axes = 1:4, id = "traj")
data_temp_lodes_age28 <- data_temp_lodes_age28 %>%
  rename(time = x, Response = stratum)
head(data_temp_lodes_age28)
summary(data_temp_lodes_age28)

belief_age28 <- ggplot(data_temp_lodes_age28,
                      aes(x = time, stratum = Response, alluvium = traj,
                          y = freq,
                          fill = Response, label = Response)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "none") +
  xlab("Questionnaire time-point") + ylab("Frequency") +
  ggtitle("Belief in God/a divine power - Mothers") + theme(plot.title = element_text(hjust = 0.5))

belief_age28

# Save this plot
pdf("./Results/ReligBelief_Age28_Mothers.pdf", height = 6, width = 10)
plot(belief_age28)
dev.off()

## Displaying these results as standard cross-tabs (using only complete cases, so data is identical to that in the sankey plot)

# preg by age 5
data %>%
  select(d810, k6240, p4040, Y3000) %>%
  rename(preg = d810, age5 = k6240, age9 = p4040, age28 = Y3000) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(preg, age5) %>%
  summarise(freq = n())

# Age 5 by age 9
data %>%
  select(d810, k6240, p4040, Y3000) %>%
  rename(preg = d810, age5 = k6240, age9 = p4040, age28 = Y3000) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(age5, age9) %>%
  summarise(freq = n())

# Age 9 by age 28
data %>%
  select(d810, k6240, p4040, Y3000) %>%
  rename(preg = d810, age5 = k6240, age9 = p4040, age28 = Y3000) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(age9, age28) %>%
  summarise(freq = n())

# Proportions at each time-point
tab_age28 <-data %>%
  select(d810, k6240, p4040, Y3000) %>%
  rename(preg = d810, age5 = k6240, age9 = p4040, age28 = Y3000) %>%
  filter(complete.cases(preg, age5, age9, age28))
summary(tab_age28)

## Does appear to be a slight drop in religious belief between pregnancy and age 5, then is stable from age 5 to 9, and then drops again between age 9 and age 28; the 'not sure' group stays approximately the same, while the 'no' group increases. Most of the transitions are between adjacent groups (i.e., from 'yes' to 'not sure', rather than 'yes' to 'no'), although in general large shifts are more likely to be from 'yes' to 'no', rather than from 'no' to 'yes'.

# Sample sizes do decrease quite substantially from age 9 to age 28 - from 6,615 with data at preg, age 5 and age 9, to just 3,682 with data at preg, age 5, age 9 and age 28.
sum(data_temp_age9$freq)
sum(data_temp_age28$freq)


### Now repeat for partners

## Pregnancy
table(data$pb150, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(pb150 = na_if(pb150, "-1")) %>%
  mutate(pb150 = recode(pb150, "N" = "No", "Y" = "Yes")) %>%
  mutate(pb150 = factor(pb150, levels = c("Yes", "Not sure", "No")))

table(data$pb150, useNA = "ifany")

## Age 5
table(data$ph6240, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(ph6240 = na_if(ph6240, "No response")) %>%
  mutate(ph6240 = na_if(ph6240, "Not completed")) %>%
  mutate(ph6240 = na_if(ph6240, "Triplet / quadruplet")) %>%
  mutate(ph6240 = recode(ph6240, "No, not at all" = "No", "Am not sure" = "Not sure")) %>%
  mutate(ph6240 = factor(ph6240, levels = c("Yes", "Not sure", "No")))

table(data$ph6240, useNA = "ifany")

## Age 6
table(data$pj7040, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(pj7040 = na_if(pj7040, "No response")) %>%
  mutate(pj7040 = na_if(pj7040, "Not completed")) %>%
  mutate(pj7040 = na_if(pj7040, "Triplet / quadruplet")) %>%
  mutate(pj7040 = recode(pj7040, "No, not at all" = "No")) %>%
  mutate(pj7040 = factor(pj7040, levels = c("Yes", "Not sure", "No")))

table(data$pj7040, useNA = "ifany")

## Age 9
table(data$pm4040, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(pm4040 = na_if(pm4040, "No response")) %>%
  mutate(pm4040 = na_if(pm4040, "Not completed")) %>%
  mutate(pm4040 = na_if(pm4040, "Triplet / quadruplet")) %>%
  mutate(pm4040 = factor(pm4040, levels = c("Yes", "Not sure", "No")))

table(data$pm4040, useNA = "ifany")

## 2019 (Age 28)
table(data$FC3000, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(FC3000 = na_if(FC3000, "Missing")) %>%
  mutate(FC3000 = na_if(FC3000, "Missed whole section C")) %>%
  mutate(FC3000 = na_if(FC3000, "Mother of trip/quad")) %>%
  mutate(FC3000 = na_if(FC3000, "Questionnaire not completed")) %>%
  mutate(FC3000 = na_if(FC3000, "Unresolvable")) %>%
  mutate(FC3000 = factor(FC3000, levels = c("Yes", "Not sure", "No")))

table(data$FC3000, useNA = "ifany")


## Turn this data into a sankey plot - Both for pregnancy to 2019, and just from pregnancy to age 9 (as less missing data)

# First, have to convert the data to the correct format and remove any missing values. Will first convert the data to summary wide format, then convert to long/lode format. Age age 5 and 6 are very close together, to get a better sense of change over time will drop the age 6 data.
data_temp_age9_p <- data %>%
  select(pb150, ph6240, pm4040) %>%
  rename(preg = pb150, age5 = ph6240, age9 = pm4040) %>%
  filter(complete.cases(preg, age5, age9)) %>%
  group_by(preg, age5, age9) %>%
  summarise(freq = n())

summary(data_temp_age9_p)
head(data_temp_age9_p)

data_temp_lodes_age9_p <- to_lodes_form(data_temp_age9_p, axes = 1:3, id = "traj")
data_temp_lodes_age9_p <- data_temp_lodes_age9_p %>%
  rename(time = x, Response = stratum)
head(data_temp_lodes_age9_p)
summary(data_temp_lodes_age9_p)

belief_age9_p <- ggplot(data_temp_lodes_age9_p,
                      aes(x = time, stratum = Response, alluvium = traj,
                          y = freq,
                          fill = Response, label = Response)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "none") +
  xlab("Questionnaire time-point") + ylab("Frequency") +
  ggtitle("Belief in God/a divine power - Partners") + theme(plot.title = element_text(hjust = 0.5))

belief_age9_p

# Save this plot
pdf("./Results/ReligBelief_Age9_Partners.pdf", height = 6, width = 10)
plot(belief_age9_p)
dev.off()


## Displaying these results as standard cross-tabs (using only complete cases, so data is identical to that in the sankey plot)

# preg by age 5
data %>%
  select(pb150, ph6240, pm4040) %>%
  rename(preg = pb150, age5 = ph6240, age9 = pm4040) %>%
  filter(complete.cases(preg, age5, age9)) %>%
  group_by(preg, age5) %>%
  summarise(freq = n())

# Age 5 by age 9
data %>%
  select(pb150, ph6240, pm4040) %>%
  rename(preg = pb150, age5 = ph6240, age9 = pm4040) %>%
  filter(complete.cases(preg, age5, age9)) %>%
  group_by(age5, age9) %>%
  summarise(freq = n())

# Proportions at each time-point
tab_age9_p <-data %>%
  select(pb150, ph6240, pm4040) %>%
  rename(preg = pb150, age5 = ph6240, age9 = pm4040) %>%
  filter(complete.cases(preg, age5, age9))
summary(tab_age9_p)


### Now repeat, but including 2019 RSBB data
data_temp_age28_p <- data %>%
  select(pb150, ph6240, pm4040, FC3000) %>%
  rename(preg = pb150, age5 = ph6240, age9 = pm4040, age28 = FC3000) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(preg, age5, age9, age28) %>%
  summarise(freq = n())

summary(data_temp_age28_p)
head(data_temp_age28_p)

data_temp_lodes_age28_p <- to_lodes_form(data_temp_age28_p, axes = 1:4, id = "traj")
data_temp_lodes_age28_p <- data_temp_lodes_age28_p %>%
  rename(time = x, Response = stratum)
head(data_temp_lodes_age28_p)
summary(data_temp_lodes_age28_p)

belief_age28_p <- ggplot(data_temp_lodes_age28_p,
                       aes(x = time, stratum = Response, alluvium = traj,
                           y = freq,
                           fill = Response, label = Response)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "none") +
  xlab("Questionnaire time-point") + ylab("Frequency") +
  ggtitle("Belief in God/a divine power - Partners") + theme(plot.title = element_text(hjust = 0.5))

belief_age28_p

# Save this plot
pdf("./Results/ReligBelief_Age28_Partners.pdf", height = 6, width = 10)
plot(belief_age28_p)
dev.off()

## Displaying these results as standard cross-tabs (using only complete cases, so data is identical to that in the sankey plot)

# preg by age 5
data %>%
  select(pb150, ph6240, pm4040, FC3000) %>%
  rename(preg = pb150, age5 = ph6240, age9 = pm4040, age28 = FC3000) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(preg, age5) %>%
  summarise(freq = n())

# Age 5 by age 9
data %>%
  select(pb150, ph6240, pm4040, FC3000) %>%
  rename(preg = pb150, age5 = ph6240, age9 = pm4040, age28 = FC3000) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(age5, age9) %>%
  summarise(freq = n())

# Age 9 by age 28
data %>%
  select(pb150, ph6240, pm4040, FC3000) %>%
  rename(preg = pb150, age5 = ph6240, age9 = pm4040, age28 = FC3000) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(age9, age28) %>%
  summarise(freq = n())

# Proportions at each time-point
tab_age28_p <-data %>%
  select(pb150, ph6240, pm4040, FC3000) %>%
  rename(preg = pb150, age5 = ph6240, age9 = pm4040, age28 = FC3000) %>%
  filter(complete.cases(preg, age5, age9, age28))
summary(tab_age28_p)

## Very similar pattern of results to the mothers: Appears to be a slight drop in religious belief between pregnancy and age 5, then is stable from age 5 to 9, and then drops again between age 9 and age 28; the 'not sure' group stays approximately the same, while the 'no' group increases. Most of the transitions are between adjacent groups (i.e., from 'yes' to 'not sure', rather than 'yes' to 'no'), although in general large shifts are more likely to be from 'yes' to 'no', rather than from 'no' to 'yes'.

# Again, sample sizes do decrease quite substantially from age 9 to age 28 - from 2,666 with data at preg, age 5 and age 9, to just 1,233 with data at preg, age 5, age 9 and age 28.
sum(data_temp_age9_p$freq)
sum(data_temp_age28_p$freq)



#####################################################################################################
#### Feel that God/some divine power has helped at any time - Responses: Yes vs Not sure vs No

### Mothers

## Pregnancy
table(data$d811, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(d811 = na_if(d811, "-1")) %>%
  mutate(d811 = factor(d811, levels = c("Yes", "Not sure", "No")))

table(data$d811, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Very little missing data, and not especially biased towards non-believers, so will leave as is.
table(data$d810[is.na(data$d811)])

## Age 5
table(data$k6241, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(k6241 = na_if(k6241, "No response")) %>%
  mutate(k6241 = na_if(k6241, "Not completed")) %>%
  mutate(k6241 = na_if(k6241, "Triplet / quadruplet")) %>%
  mutate(k6241 = factor(k6241, levels = c("Yes", "Not sure", "No")))

table(data$k6241, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Very little missing data, and not especially biased towards non-believers, so will leave as is.
table(data$k6240[is.na(data$k6241)])

## Age 6
table(data$l7041, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(l7041 = na_if(l7041, "No response")) %>%
  mutate(l7041 = na_if(l7041, "Not completed")) %>%
  mutate(l7041 = na_if(l7041, "Triplet / quadruplet")) %>%
  mutate(l7041 = factor(l7041, levels = c("Yes", "Not sure", "No")))

table(data$l7041, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Very little missing data, and not especially biased towards non-believers, so will leave as is.
table(data$l7040[is.na(data$l7041)])

## Age 9
table(data$p4041, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(p4041 = na_if(p4041, "No response")) %>%
  mutate(p4041 = na_if(p4041, "Not completed")) %>%
  mutate(p4041 = na_if(p4041, "Triplet / quadruplet")) %>%
  mutate(p4041 = factor(p4041, levels = c("Yes", "Not sure", "No")))

table(data$p4041, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Very little missing data, and not especially biased towards non-believers, so will leave as is.
table(data$p4040[is.na(data$p4041)])

## 2019 (Age 28)
table(data$Y3010, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(Y3010 = na_if(Y3010, "Missing")) %>%
  mutate(Y3010 = na_if(Y3010, "Missed whole section C")) %>%
  mutate(Y3010 = na_if(Y3010, "Mother of trip/quad")) %>%
  mutate(Y3010 = na_if(Y3010, "Questionnaire not completed")) %>%
  mutate(Y3010 = na_if(Y3010, "Unresolvable")) %>%
  mutate(Y3010 = factor(Y3010, levels = c("Yes", "Not sure", "No")))

table(data$Y3010, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Very little missing data, and not especially biased towards non-believers, so will leave as is.
table(data$Y3000[is.na(data$Y3010)])


## Turn this data into a sankey plot - Both for pregnancy to 2019, and just from pregnancy to age 9 (as less missing data)

# First, have to convert the data to the correct format and remove any missing values. Will first convert the data to summary wide format, then convert to long/lode format. Age age 5 and 6 are very close together, to get a better sense of change over time will drop the age 6 data.
data_temp_age9 <- data %>%
  select(d811, k6241, p4041) %>%
  rename(preg = d811, age5 = k6241, age9 = p4041) %>%
  filter(complete.cases(preg, age5, age9)) %>%
  group_by(preg, age5, age9) %>%
  summarise(freq = n())

summary(data_temp_age9)
head(data_temp_age9)

data_temp_lodes_age9 <- to_lodes_form(data_temp_age9, axes = 1:3, id = "traj")
data_temp_lodes_age9 <- data_temp_lodes_age9 %>%
  rename(time = x, Response = stratum)
head(data_temp_lodes_age9)
summary(data_temp_lodes_age9)

helped_age9 <- ggplot(data_temp_lodes_age9,
                      aes(x = time, stratum = Response, alluvium = traj,
                          y = freq,
                          fill = Response, label = Response)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "none") +
  xlab("Questionnaire time-point") + ylab("Frequency") +
  ggtitle("Helped by God/divine power - Mothers") + theme(plot.title = element_text(hjust = 0.5))

helped_age9

# Save this plot
pdf("./Results/Helped_Age9_Mothers.pdf", height = 6, width = 10)
plot(helped_age9)
dev.off()


## Displaying these results as standard cross-tabs (using only complete cases, so data is identical to that in the sankey plot)

# preg by age 5
data %>%
  select(d811, k6241, p4041) %>%
  rename(preg = d811, age5 = k6241, age9 = p4041) %>%
  filter(complete.cases(preg, age5, age9)) %>%
  group_by(preg, age5) %>%
  summarise(freq = n())

# Age 5 by age 9
data %>%
  select(d811, k6241, p4041) %>%
  rename(preg = d811, age5 = k6241, age9 = p4041) %>%
  filter(complete.cases(preg, age5, age9)) %>%
  group_by(age5, age9) %>%
  summarise(freq = n())

# Proportions at each time-point
tab_age9 <-data %>%
  select(d811, k6241, p4041) %>%
  rename(preg = d811, age5 = k6241, age9 = p4041) %>%
  filter(complete.cases(preg, age5, age9))
summary(tab_age9)


### Now repeat, but including 2019 RSBB data
data_temp_age28 <- data %>%
  select(d811, k6241, p4041, Y3010) %>%
  rename(preg = d811, age5 = k6241, age9 = p4041, age28 = Y3010) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(preg, age5, age9, age28) %>%
  summarise(freq = n())

summary(data_temp_age28)
head(data_temp_age28)

data_temp_lodes_age28 <- to_lodes_form(data_temp_age28, axes = 1:4, id = "traj")
data_temp_lodes_age28 <- data_temp_lodes_age28 %>%
  rename(time = x, Response = stratum)
head(data_temp_lodes_age28)
summary(data_temp_lodes_age28)

helped_age28 <- ggplot(data_temp_lodes_age28,
                       aes(x = time, stratum = Response, alluvium = traj,
                           y = freq,
                           fill = Response, label = Response)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "none") +
  xlab("Questionnaire time-point") + ylab("Frequency") +
  ggtitle("Helped by God/divine power - Mothers") + theme(plot.title = element_text(hjust = 0.5))

helped_age28

# Save this plot
pdf("./Results/Helped_Age28_Mothers.pdf", height = 6, width = 10)
plot(helped_age28)
dev.off()

## Displaying these results as standard cross-tabs (using only complete cases, so data is identical to that in the sankey plot)

# preg by age 5
data %>%
  select(d811, k6241, p4041, Y3010) %>%
  rename(preg = d811, age5 = k6241, age9 = p4041, age28 = Y3010) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(preg, age5) %>%
  summarise(freq = n())

# Age 5 by age 9
data %>%
  select(d811, k6241, p4041, Y3010) %>%
  rename(preg = d811, age5 = k6241, age9 = p4041, age28 = Y3010) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(age5, age9) %>%
  summarise(freq = n())

# Age 9 by age 28
data %>%
  select(d811, k6241, p4041, Y3010) %>%
  rename(preg = d811, age5 = k6241, age9 = p4041, age28 = Y3010) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(age9, age28) %>%
  summarise(freq = n())

# Proportions at each time-point
tab_age28 <-data %>%
  select(d811, k6241, p4041, Y3010) %>%
  rename(preg = d811, age5 = k6241, age9 = p4041, age28 = Y3010) %>%
  filter(complete.cases(preg, age5, age9, age28))
summary(tab_age28)

## 'Yes' is generally quite stable, but is a shift over time (esp. at age 28) from 'not sure' to 'no'. This is potentially a bit strange, as the question asks if God/divine power has helped 'at any time', so may not really expect shifts from positive to not sure/no (potentially re-evaluating prior experiences, though, perhaps?)

# Sample sizes do decrease quite substantially from age 9 to age 28
sum(data_temp_age9$freq)
sum(data_temp_age28$freq)


### Now repeat for partners

## Pregnancy
table(data$pb151, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(pb151 = na_if(pb151, "-1")) %>%
  mutate(pb151 = recode(pb151, "N" = "No", "Y" = "Yes")) %>%
  mutate(pb151 = factor(pb151, levels = c("Yes", "Not sure", "No")))

table(data$pb151, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Very little missing data, and not especially biased towards non-believers, so will leave as is.
table(data$pb150[is.na(data$pb151)])

## Age 5
table(data$ph6241, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(ph6241 = na_if(ph6241, "No response")) %>%
  mutate(ph6241 = na_if(ph6241, "Not completed")) %>%
  mutate(ph6241 = na_if(ph6241, "Triplet / quadruplet")) %>%
  mutate(ph6241 = factor(ph6241, levels = c("Yes", "Not sure", "No")))

table(data$ph6241, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Very little missing data, and not especially biased towards non-believers, so will leave as is.
table(data$ph6240[is.na(data$ph6241)])

## Age 6
table(data$pj7041, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(pj7041 = na_if(pj7041, "No response")) %>%
  mutate(pj7041 = na_if(pj7041, "Not completed")) %>%
  mutate(pj7041 = na_if(pj7041, "Triplet / quadruplet")) %>%
  mutate(pj7041 = factor(pj7041, levels = c("Yes", "Not sure", "No")))

table(data$pj7041, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Very little missing data, and not especially biased towards non-believers, so will leave as is.
table(data$pj7040[is.na(data$pj7041)])

## Age 9
table(data$pm4041, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(pm4041 = na_if(pm4041, "No response")) %>%
  mutate(pm4041 = na_if(pm4041, "Not completed")) %>%
  mutate(pm4041 = na_if(pm4041, "Triplet / quadruplet")) %>%
  mutate(pm4041 = factor(pm4041, levels = c("Yes", "Not sure", "No")))

table(data$pm4041, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Very little missing data, and not especially biased towards non-believers, so will leave as is.
table(data$pm4040[is.na(data$pm4041)])

## 2019 (Age 28)
table(data$FC3010, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(FC3010 = na_if(FC3010, "Missing")) %>%
  mutate(FC3010 = na_if(FC3010, "Missed whole section C")) %>%
  mutate(FC3010 = na_if(FC3010, "Mother of trip/quad")) %>%
  mutate(FC3010 = na_if(FC3010, "Questionnaire not completed")) %>%
  mutate(FC3010 = na_if(FC3010, "Unresolvable")) %>%
  mutate(FC3010 = factor(FC3010, levels = c("Yes", "Not sure", "No")))

table(data$FC3010, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Very little missing data, and not especially biased towards non-believers, so will leave as is.
table(data$FC3000[is.na(data$FC3010)])



## Turn this data into a sankey plot - Both for pregnancy to 2019, and just from pregnancy to age 9 (as less missing data)

# First, have to convert the data to the correct format and remove any missing values. Will first convert the data to summary wide format, then convert to long/lode format. Age age 5 and 6 are very close together, to get a better sense of change over time will drop the age 6 data.
data_temp_age9_p <- data %>%
  select(pb151, ph6241, pm4041) %>%
  rename(preg = pb151, age5 = ph6241, age9 = pm4041) %>%
  filter(complete.cases(preg, age5, age9)) %>%
  group_by(preg, age5, age9) %>%
  summarise(freq = n())

summary(data_temp_age9_p)
head(data_temp_age9_p)

data_temp_lodes_age9_p <- to_lodes_form(data_temp_age9_p, axes = 1:3, id = "traj")
data_temp_lodes_age9_p <- data_temp_lodes_age9_p %>%
  rename(time = x, Response = stratum)
head(data_temp_lodes_age9_p)
summary(data_temp_lodes_age9_p)

helped_age9_p <- ggplot(data_temp_lodes_age9_p,
                        aes(x = time, stratum = Response, alluvium = traj,
                            y = freq,
                            fill = Response, label = Response)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "none") +
  xlab("Questionnaire time-point") + ylab("Frequency") +
  ggtitle("Helped by God/divine power - Partners") + theme(plot.title = element_text(hjust = 0.5))

helped_age9_p

# Save this plot
pdf("./Results/Helped_Age9_Partners.pdf", height = 6, width = 10)
plot(helped_age9_p)
dev.off()


## Displaying these results as standard cross-tabs (using only complete cases, so data is identical to that in the sankey plot)

# preg by age 5
data %>%
  select(pb151, ph6241, pm4041) %>%
  rename(preg = pb151, age5 = ph6241, age9 = pm4041) %>%
  filter(complete.cases(preg, age5, age9)) %>%
  group_by(preg, age5) %>%
  summarise(freq = n())

# Age 5 by age 9
data %>%
  select(pb151, ph6241, pm4041) %>%
  rename(preg = pb151, age5 = ph6241, age9 = pm4041) %>%
  filter(complete.cases(preg, age5, age9)) %>%
  group_by(age5, age9) %>%
  summarise(freq = n())

# Proportions at each time-point
tab_age9_p <-data %>%
  select(pb151, ph6241, pm4041) %>%
  rename(preg = pb151, age5 = ph6241, age9 = pm4041) %>%
  filter(complete.cases(preg, age5, age9))
summary(tab_age9_p)


### Now repeat, but including 2019 RSBB data
data_temp_age28_p <- data %>%
  select(pb151, ph6241, pm4041, FC3010) %>%
  rename(preg = pb151, age5 = ph6241, age9 = pm4041, age28 = FC3010) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(preg, age5, age9, age28) %>%
  summarise(freq = n())

summary(data_temp_age28_p)
head(data_temp_age28_p)

data_temp_lodes_age28_p <- to_lodes_form(data_temp_age28_p, axes = 1:4, id = "traj")
data_temp_lodes_age28_p <- data_temp_lodes_age28_p %>%
  rename(time = x, Response = stratum)
head(data_temp_lodes_age28_p)
summary(data_temp_lodes_age28_p)

helped_age28_p <- ggplot(data_temp_lodes_age28_p,
                         aes(x = time, stratum = Response, alluvium = traj,
                             y = freq,
                             fill = Response, label = Response)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "none") +
  xlab("Questionnaire time-point") + ylab("Frequency") +
  ggtitle("Helped by God/divine power - Partners") + theme(plot.title = element_text(hjust = 0.5))

helped_age28_p

# Save this plot
pdf("./Results/Helped_Age28_Partners.pdf", height = 6, width = 10)
plot(helped_age28_p)
dev.off()

## Displaying these results as standard cross-tabs (using only complete cases, so data is identical to that in the sankey plot)

# preg by age 5
data %>%
  select(pb151, ph6241, pm4041, FC3010) %>%
  rename(preg = pb151, age5 = ph6241, age9 = pm4041, age28 = FC3010) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(preg, age5) %>%
  summarise(freq = n())

# Age 5 by age 9
data %>%
  select(pb151, ph6241, pm4041, FC3010) %>%
  rename(preg = pb151, age5 = ph6241, age9 = pm4041, age28 = FC3010) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(age5, age9) %>%
  summarise(freq = n())

# Age 9 by age 28
data %>%
  select(pb151, ph6241, pm4041, FC3010) %>%
  rename(preg = pb151, age5 = ph6241, age9 = pm4041, age28 = FC3010) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(age9, age28) %>%
  summarise(freq = n())

# Proportions at each time-point
tab_age28_p <-data %>%
  select(pb151, ph6241, pm4041, FC3010) %>%
  rename(preg = pb151, age5 = ph6241, age9 = pm4041, age28 = FC3010) %>%
  filter(complete.cases(preg, age5, age9, age28))
summary(tab_age28_p)

## Very similar pattern of results to the mothers: Little change in 'yes' responses, but from age 9 to age 28 there is a noticable shift from 'not sure' to 'no'.

# Again, sample sizes do decrease quite substantially from age 9 to age 28
sum(data_temp_age9_p$freq)
sum(data_temp_age28_p$freq)



#####################################################################################################
#### Would appeal to God for help if in trouble - Responses: Yes vs Not sure vs No

### Mothers

## Pregnancy
table(data$d812, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(d812 = na_if(d812, "-1")) %>%
  mutate(d812 = factor(d812, levels = c("Yes", "Not sure", "No")))

table(data$d812, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Very little missing data, and not especially biased towards non-believers, so will leave as is.
table(data$d810[is.na(data$d812)])

## Age 5
table(data$k6242, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(k6242 = na_if(k6242, "No response")) %>%
  mutate(k6242 = na_if(k6242, "Not completed")) %>%
  mutate(k6242 = na_if(k6242, "Triplet / quadruplet")) %>%
  mutate(k6242 = factor(k6242, levels = c("Yes", "Not sure", "No")))

table(data$k6242, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Very little missing data, and not especially biased towards non-believers, so will leave as is.
table(data$k6240[is.na(data$k6242)])

## Age 6
table(data$l7042, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(l7042 = na_if(l7042, "No response")) %>%
  mutate(l7042 = na_if(l7042, "Not completed")) %>%
  mutate(l7042 = na_if(l7042, "Triplet / quadruplet")) %>%
  mutate(l7042 = factor(l7042, levels = c("Yes", "Not sure", "No")))

table(data$l7042, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Very little missing data, and not especially biased towards non-believers, so will leave as is.
table(data$l7040[is.na(data$l7042)])

## Age 9
table(data$p4042, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(p4042 = na_if(p4042, "No response")) %>%
  mutate(p4042 = na_if(p4042, "Not completed")) %>%
  mutate(p4042 = na_if(p4042, "Triplet / quadruplet")) %>%
  mutate(p4042 = factor(p4042, levels = c("Yes", "Not sure", "No")))

table(data$p4042, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Very little missing data, and not especially biased towards non-believers, so will leave as is.
table(data$p4040[is.na(data$p4042)])

## 2019 (Age 28)
table(data$Y3020, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(Y3020 = na_if(Y3020, "Missing")) %>%
  mutate(Y3020 = na_if(Y3020, "Missed whole section C")) %>%
  mutate(Y3020 = na_if(Y3020, "Mother of trip/quad")) %>%
  mutate(Y3020 = na_if(Y3020, "Questionnaire not completed")) %>%
  mutate(Y3020 = na_if(Y3020, "Unresolvable")) %>%
  mutate(Y3020 = factor(Y3020, levels = c("Yes", "Not sure", "No")))

table(data$Y3020, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Very little missing data, and not especially biased towards non-believers, so will leave as is.
table(data$Y3000[is.na(data$Y3020)])


## Turn this data into a sankey plot - Both for pregnancy to 2019, and just from pregnancy to age 9 (as less missing data)

# First, have to convert the data to the correct format and remove any missing values. Will first convert the data to summary wide format, then convert to long/lode format. Age age 5 and 6 are very close together, to get a better sense of change over time will drop the age 6 data.
data_temp_age9 <- data %>%
  select(d812, k6242, p4042) %>%
  rename(preg = d812, age5 = k6242, age9 = p4042) %>%
  filter(complete.cases(preg, age5, age9)) %>%
  group_by(preg, age5, age9) %>%
  summarise(freq = n())

summary(data_temp_age9)
head(data_temp_age9)

data_temp_lodes_age9 <- to_lodes_form(data_temp_age9, axes = 1:3, id = "traj")
data_temp_lodes_age9 <- data_temp_lodes_age9 %>%
  rename(time = x, Response = stratum)
head(data_temp_lodes_age9)
summary(data_temp_lodes_age9)

appealGod_age9 <- ggplot(data_temp_lodes_age9,
                      aes(x = time, stratum = Response, alluvium = traj,
                          y = freq,
                          fill = Response, label = Response)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "none") +
  xlab("Questionnaire time-point") + ylab("Frequency") +
  ggtitle("Appeal to God if in trouble - Mothers") + theme(plot.title = element_text(hjust = 0.5))

appealGod_age9

# Save this plot
pdf("./Results/appealGod_Age9_Mothers.pdf", height = 6, width = 10)
plot(appealGod_age9)
dev.off()


## Displaying these results as standard cross-tabs (using only complete cases, so data is identical to that in the sankey plot)

# preg by age 5
data %>%
  select(d812, k6242, p4042) %>%
  rename(preg = d812, age5 = k6242, age9 = p4042) %>%
  filter(complete.cases(preg, age5, age9)) %>%
  group_by(preg, age5) %>%
  summarise(freq = n())

# Age 5 by age 9
data %>%
  select(d812, k6242, p4042) %>%
  rename(preg = d812, age5 = k6242, age9 = p4042) %>%
  filter(complete.cases(preg, age5, age9)) %>%
  group_by(age5, age9) %>%
  summarise(freq = n())

# Proportions at each time-point
tab_age9 <-data %>%
  select(d812, k6242, p4042) %>%
  rename(preg = d812, age5 = k6242, age9 = p4042) %>%
  filter(complete.cases(preg, age5, age9))
summary(tab_age9)


### Now repeat, but including 2019 RSBB data
data_temp_age28 <- data %>%
  select(d812, k6242, p4042, Y3020) %>%
  rename(preg = d812, age5 = k6242, age9 = p4042, age28 = Y3020) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(preg, age5, age9, age28) %>%
  summarise(freq = n())

summary(data_temp_age28)
head(data_temp_age28)

data_temp_lodes_age28 <- to_lodes_form(data_temp_age28, axes = 1:4, id = "traj")
data_temp_lodes_age28 <- data_temp_lodes_age28 %>%
  rename(time = x, Response = stratum)
head(data_temp_lodes_age28)
summary(data_temp_lodes_age28)

appealGod_age28 <- ggplot(data_temp_lodes_age28,
                       aes(x = time, stratum = Response, alluvium = traj,
                           y = freq,
                           fill = Response, label = Response)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "none") +
  xlab("Questionnaire time-point") + ylab("Frequency") +
  ggtitle("Appeal to God if in trouble - Mothers") + theme(plot.title = element_text(hjust = 0.5))

appealGod_age28

# Save this plot
pdf("./Results/appealGod_Age28_Mothers.pdf", height = 6, width = 10)
plot(appealGod_age28)
dev.off()

## Displaying these results as standard cross-tabs (using only complete cases, so data is identical to that in the sankey plot)

# preg by age 5
data %>%
  select(d812, k6242, p4042, Y3020) %>%
  rename(preg = d812, age5 = k6242, age9 = p4042, age28 = Y3020) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(preg, age5) %>%
  summarise(freq = n())

# Age 5 by age 9
data %>%
  select(d812, k6242, p4042, Y3020) %>%
  rename(preg = d812, age5 = k6242, age9 = p4042, age28 = Y3020) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(age5, age9) %>%
  summarise(freq = n())

# Age 9 by age 28
data %>%
  select(d812, k6242, p4042, Y3020) %>%
  rename(preg = d812, age5 = k6242, age9 = p4042, age28 = Y3020) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(age9, age28) %>%
  summarise(freq = n())

# Proportions at each time-point
tab_age28 <-data %>%
  select(d812, k6242, p4042, Y3020) %>%
  rename(preg = d812, age5 = k6242, age9 = p4042, age28 = Y3020) %>%
  filter(complete.cases(preg, age5, age9, age28))
summary(tab_age28)

## 'Yes' is generally quite stable (but a slight decrease over time), but is a shift over time (esp. at age 28) from 'not sure' to 'no'.

# Sample sizes do decrease quite substantially from age 9 to age 28
sum(data_temp_age9$freq)
sum(data_temp_age28$freq)


### Now repeat for partners

## Pregnancy
table(data$pb152, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(pb152 = na_if(pb152, "-1")) %>%
  mutate(pb152 = recode(pb152, "N" = "No", "Y" = "Yes")) %>%
  mutate(pb152 = factor(pb152, levels = c("Yes", "Not sure", "No")))

table(data$pb152, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Very little missing data, and not especially biased towards non-believers, so will leave as is.
table(data$pb150[is.na(data$pb152)])

## Age 5
table(data$ph6242, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(ph6242 = na_if(ph6242, "No response")) %>%
  mutate(ph6242 = na_if(ph6242, "Not completed")) %>%
  mutate(ph6242 = na_if(ph6242, "Triplet / quadruplet")) %>%
  mutate(ph6242 = factor(ph6242, levels = c("Yes", "Not sure", "No")))

table(data$ph6242, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Very little missing data, and not especially biased towards non-believers, so will leave as is.
table(data$ph6240[is.na(data$ph6242)])

## Age 6
table(data$pj7042, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(pj7042 = na_if(pj7042, "No response")) %>%
  mutate(pj7042 = na_if(pj7042, "Not completed")) %>%
  mutate(pj7042 = na_if(pj7042, "Triplet / quadruplet")) %>%
  mutate(pj7042 = factor(pj7042, levels = c("Yes", "Not sure", "No")))

table(data$pj7042, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Very little missing data, and not especially biased towards non-believers, so will leave as is.
table(data$pj7040[is.na(data$pj7042)])

## Age 9
table(data$pm4042, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(pm4042 = na_if(pm4042, "No response")) %>%
  mutate(pm4042 = na_if(pm4042, "Not completed")) %>%
  mutate(pm4042 = na_if(pm4042, "Triplet / quadruplet")) %>%
  mutate(pm4042 = factor(pm4042, levels = c("Yes", "Not sure", "No")))

table(data$pm4042, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Very little missing data, and not especially biased towards non-believers, so will leave as is.
table(data$pm4040[is.na(data$pm4042)])

## 2019 (Age 28)
table(data$FC3020, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(FC3020 = na_if(FC3020, "Missing")) %>%
  mutate(FC3020 = na_if(FC3020, "Missed whole section C")) %>%
  mutate(FC3020 = na_if(FC3020, "Mother of trip/quad")) %>%
  mutate(FC3020 = na_if(FC3020, "Questionnaire not completed")) %>%
  mutate(FC3020 = na_if(FC3020, "Unresolvable")) %>%
  mutate(FC3020 = factor(FC3020, levels = c("Yes", "Not sure", "No")))

table(data$FC3020, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Very little missing data, and not especially biased towards non-believers, so will leave as is.
table(data$FC3000[is.na(data$FC3020)])


## Turn this data into a sankey plot - Both for pregnancy to 2019, and just from pregnancy to age 9 (as less missing data)

# First, have to convert the data to the correct format and remove any missing values. Will first convert the data to summary wide format, then convert to long/lode format. Age age 5 and 6 are very close together, to get a better sense of change over time will drop the age 6 data.
data_temp_age9_p <- data %>%
  select(pb152, ph6242, pm4042) %>%
  rename(preg = pb152, age5 = ph6242, age9 = pm4042) %>%
  filter(complete.cases(preg, age5, age9)) %>%
  group_by(preg, age5, age9) %>%
  summarise(freq = n())

summary(data_temp_age9_p)
head(data_temp_age9_p)

data_temp_lodes_age9_p <- to_lodes_form(data_temp_age9_p, axes = 1:3, id = "traj")
data_temp_lodes_age9_p <- data_temp_lodes_age9_p %>%
  rename(time = x, Response = stratum)
head(data_temp_lodes_age9_p)
summary(data_temp_lodes_age9_p)

appealGod_age9_p <- ggplot(data_temp_lodes_age9_p,
                        aes(x = time, stratum = Response, alluvium = traj,
                            y = freq,
                            fill = Response, label = Response)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "none") +
  xlab("Questionnaire time-point") + ylab("Frequency") +
  ggtitle("Appeal to God if in trouble - Partners") + theme(plot.title = element_text(hjust = 0.5))

appealGod_age9_p

# Save this plot
pdf("./Results/appealGod_Age9_Partners.pdf", height = 6, width = 10)
plot(appealGod_age9_p)
dev.off()


## Displaying these results as standard cross-tabs (using only complete cases, so data is identical to that in the sankey plot)

# preg by age 5
data %>%
  select(pb152, ph6242, pm4042) %>%
  rename(preg = pb152, age5 = ph6242, age9 = pm4042) %>%
  filter(complete.cases(preg, age5, age9)) %>%
  group_by(preg, age5) %>%
  summarise(freq = n())

# Age 5 by age 9
data %>%
  select(pb152, ph6242, pm4042) %>%
  rename(preg = pb152, age5 = ph6242, age9 = pm4042) %>%
  filter(complete.cases(preg, age5, age9)) %>%
  group_by(age5, age9) %>%
  summarise(freq = n())

# Proportions at each time-point
tab_age9_p <-data %>%
  select(pb152, ph6242, pm4042) %>%
  rename(preg = pb152, age5 = ph6242, age9 = pm4042) %>%
  filter(complete.cases(preg, age5, age9))
summary(tab_age9_p)


### Now repeat, but including 2019 RSBB data
data_temp_age28_p <- data %>%
  select(pb152, ph6242, pm4042, FC3020) %>%
  rename(preg = pb152, age5 = ph6242, age9 = pm4042, age28 = FC3020) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(preg, age5, age9, age28) %>%
  summarise(freq = n())

summary(data_temp_age28_p)
head(data_temp_age28_p)

data_temp_lodes_age28_p <- to_lodes_form(data_temp_age28_p, axes = 1:4, id = "traj")
data_temp_lodes_age28_p <- data_temp_lodes_age28_p %>%
  rename(time = x, Response = stratum)
head(data_temp_lodes_age28_p)
summary(data_temp_lodes_age28_p)

appealGod_age28_p <- ggplot(data_temp_lodes_age28_p,
                         aes(x = time, stratum = Response, alluvium = traj,
                             y = freq,
                             fill = Response, label = Response)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "none") +
  xlab("Questionnaire time-point") + ylab("Frequency") +
  ggtitle("Appeal to God if in trouble - Partners") + theme(plot.title = element_text(hjust = 0.5))

appealGod_age28_p

# Save this plot
pdf("./Results/appealGod_Age28_Partners.pdf", height = 6, width = 10)
plot(appealGod_age28_p)
dev.off()

## Displaying these results as standard cross-tabs (using only complete cases, so data is identical to that in the sankey plot)

# preg by age 5
data %>%
  select(pb152, ph6242, pm4042, FC3020) %>%
  rename(preg = pb152, age5 = ph6242, age9 = pm4042, age28 = FC3020) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(preg, age5) %>%
  summarise(freq = n())

# Age 5 by age 9
data %>%
  select(pb152, ph6242, pm4042, FC3020) %>%
  rename(preg = pb152, age5 = ph6242, age9 = pm4042, age28 = FC3020) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(age5, age9) %>%
  summarise(freq = n())

# Age 9 by age 28
data %>%
  select(pb152, ph6242, pm4042, FC3020) %>%
  rename(preg = pb152, age5 = ph6242, age9 = pm4042, age28 = FC3020) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(age9, age28) %>%
  summarise(freq = n())

# Proportions at each time-point
tab_age28_p <-data %>%
  select(pb152, ph6242, pm4042, FC3020) %>%
  rename(preg = pb152, age5 = ph6242, age9 = pm4042, age28 = FC3020) %>%
  filter(complete.cases(preg, age5, age9, age28))
summary(tab_age28_p)

## Very similar pattern of results to the mothers: Slight decrease in 'yes' responses, but from age 9 to age 28 there is a noticeable shift from 'not sure' to 'no'.

# Again, sample sizes do decrease quite substantially from age 9 to age 28
sum(data_temp_age9_p$freq)
sum(data_temp_age28_p$freq)



#####################################################################################################
#### Would 'pray', even if not in trouble - Responses: Yes vs No (only asked at age 6, age 9 and age 28)

### Mothers

## Age 6
table(data$l7043, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(l7043 = na_if(l7043, "No response")) %>%
  mutate(l7043 = na_if(l7043, "Not completed")) %>%
  mutate(l7043 = na_if(l7043, "Triplet / quadruplet")) %>%
  mutate(l7043 = na_if(l7043, "Don't know")) %>%
  mutate(l7043 = na_if(l7043, "Text response")) %>%
  mutate(l7043 = factor(l7043, levels = c("Yes", "No")))

table(data$l7043, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is quite a bit of missing data, but the majority are 'yes' or 'not sure' to belief in God, so will leave as is.
table(data$l7040[is.na(data$l7043)])

## Age 9
table(data$p4043, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(p4043 = na_if(p4043, "No response")) %>%
  mutate(p4043 = na_if(p4043, "Not completed")) %>%
  mutate(p4043 = na_if(p4043, "Triplet / quadruplet")) %>%
  mutate(p4043 = na_if(p4043, "Text response")) %>%
  mutate(p4043 = factor(p4043, levels = c("Yes", "No")))

table(data$p4043, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is quite a bit of missing data, but the majority are 'yes' or 'not sure' to belief in God, so will leave as is.
table(data$p4040[is.na(data$p4043)])

## 2019 (Age 28)
table(data$Y3030, useNA = "ifany")

# This questionnaire had a 'not sure' response for this question (unlike previous waves) - Will see if sensible to combine with 'no' (if not believe in God), for consistency across time-points. Hmmmm...Are actually quite a few people who believe in God or are not sure who said 'not sure' to this prayer question, while very few non-believers answered 'not sure'. And at the previous time-points for this 'prayer' question it's a relatively even split between 'yes' and 'no'. As not clear how to code these 'not sure' people, will just code them as missing here.
table(data$Y3000[data$Y3030 == "Not sure"])
table(data$l7043[data$Y3030 == "Not sure"])
table(data$p4043[data$Y3030 == "Not sure"])

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(Y3030 = na_if(Y3030, "Missing")) %>%
  mutate(Y3030 = na_if(Y3030, "Missed whole section C")) %>%
  mutate(Y3030 = na_if(Y3030, "Mother of trip/quad")) %>%
  mutate(Y3030 = na_if(Y3030, "Questionnaire not completed")) %>%
  mutate(Y3030 = na_if(Y3030, "Unresolvable")) %>%
  mutate(Y3030 = na_if(Y3030, "Not sure")) %>%
  mutate(Y3030 = factor(Y3030, levels = c("Yes", "No")))

table(data$Y3030, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Very little missing data, and not especially biased towards non-believers, so will leave as is.
table(data$Y3000[is.na(data$Y3030)])


## Turn this data into a sankey plot - As only started from age 6, will just do from age 6 to 2019
data_temp_age28 <- data %>%
  select(l7043, p4043, Y3030) %>%
  rename(age6 = l7043, age9 = p4043, age28 = Y3030) %>%
  filter(complete.cases(age6, age9, age28)) %>%
  group_by(age6, age9, age28) %>%
  summarise(freq = n())

summary(data_temp_age28)
head(data_temp_age28)

data_temp_lodes_age28 <- to_lodes_form(data_temp_age28, axes = 1:3, id = "traj")
data_temp_lodes_age28 <- data_temp_lodes_age28 %>%
  rename(time = x, Response = stratum)
head(data_temp_lodes_age28)
summary(data_temp_lodes_age28)

pray_age28 <- ggplot(data_temp_lodes_age28,
                          aes(x = time, stratum = Response, alluvium = traj,
                              y = freq,
                              fill = Response, label = Response)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "none") +
  xlab("Questionnaire time-point") + ylab("Frequency") +
  ggtitle("Pray to God even if not in trouble - Mothers") + theme(plot.title = element_text(hjust = 0.5))

pray_age28

# Save this plot
pdf("./Results/pray_Age28_Mothers.pdf", height = 6, width = 10)
plot(pray_age28)
dev.off()

## Displaying these results as standard cross-tabs (using only complete cases, so data is identical to that in the sankey plot)

# Age 6 by age 9
data %>%
  select(l7043, p4043, Y3030) %>%
  rename(age6 = l7043, age9 = p4043, age28 = Y3030) %>%
  filter(complete.cases(age6, age9, age28)) %>%
  group_by(age6, age9) %>%
  summarise(freq = n())

# Age 9 by age 28
data %>%
  select(l7043, p4043, Y3030) %>%
  rename(age6 = l7043, age9 = p4043, age28 = Y3030) %>%
  filter(complete.cases(age6, age9, age28)) %>%
  group_by(age9, age28) %>%
  summarise(freq = n())

# Proportions at each time-point
tab_age28 <-data %>%
  select(l7043, p4043, Y3030) %>%
  rename(age6 = l7043, age9 = p4043, age28 = Y3030) %>%
  filter(complete.cases(age6, age9, age28))
summary(tab_age28)

## 'No' increases from age 9 to age 28.
sum(data_temp_age28$freq)


### Now repeat for partners

## Age 6
table(data$pj7043, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(pj7043 = na_if(pj7043, "No response")) %>%
  mutate(pj7043 = na_if(pj7043, "Not completed")) %>%
  mutate(pj7043 = na_if(pj7043, "Triplet / quadruplet")) %>%
  mutate(pj7043 = na_if(pj7043, "Don't know")) %>%
  mutate(pj7043 = factor(pj7043, levels = c("Yes", "No")))

table(data$pj7043, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Very little missing data, and not especially biased towards non-believers, so will leave as is.
table(data$pj7040[is.na(data$pj7043)])

## Age 9
table(data$pm4043, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(pm4043 = na_if(pm4043, "No response")) %>%
  mutate(pm4043 = na_if(pm4043, "Not completed")) %>%
  mutate(pm4043 = na_if(pm4043, "Triplet / quadruplet")) %>%
  mutate(pm4043 = factor(pm4043, levels = c("Yes", "No")))

table(data$pm4043, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Very little missing data, and not especially biased towards non-believers, so will leave as is.
table(data$pm4040[is.na(data$pm4043)])

## 2019 (Age 28)
table(data$FC3030, useNA = "ifany")

# This questionnaire had a 'not sure' response for this question (unlike previous waves) - Will see if sensible to combine with 'no' (if not believe in God), for consistency across time-points. Hmmmm...Are actually quite a few people who believe in God or are not sure who said 'not sure' to this prayer question, while very few non-believers answered 'not sure'. And at the previous time-points for this 'prayer' question it's a relatively even split between 'yes' and 'no'. As not clear how to code these 'not sure' people, will just code them as missing here.
table(data$FC3000[data$FC3030 == "Not sure"])
table(data$pj7043[data$FC3030 == "Not sure"])
table(data$pm4043[data$FC3030 == "Not sure"])

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(FC3030 = na_if(FC3030, "Missing")) %>%
  mutate(FC3030 = na_if(FC3030, "Missed whole section C")) %>%
  mutate(FC3030 = na_if(FC3030, "Questionnaire not completed")) %>%
  mutate(FC3030 = na_if(FC3030, "Unresolvable")) %>%
  mutate(FC3030 = na_if(FC3030, "Not sure")) %>%
  mutate(FC3030 = factor(FC3030, levels = c("Yes", "No")))

table(data$FC3030, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Very little missing data, and not especially biased towards non-believers, so will leave as is.
table(data$FC3000[is.na(data$FC3030)])


## Turn this data into a sankey plot - As only started from age 6, will just do from age 6 to 2019
data_temp_age28_p <- data %>%
  select(pj7043, pm4043, FC3030) %>%
  rename(age6 = pj7043, age9 = pm4043, age28 = FC3030) %>%
  filter(complete.cases(age6, age9, age28)) %>%
  group_by(age6, age9, age28) %>%
  summarise(freq = n())

summary(data_temp_age28_p)
head(data_temp_age28_p)

data_temp_lodes_age28_p <- to_lodes_form(data_temp_age28_p, axes = 1:3, id = "traj")
data_temp_lodes_age28_p <- data_temp_lodes_age28_p %>%
  rename(time = x, Response = stratum)
head(data_temp_lodes_age28_p)
summary(data_temp_lodes_age28_p)

pray_age28_p <- ggplot(data_temp_lodes_age28_p,
                            aes(x = time, stratum = Response, alluvium = traj,
                                y = freq,
                                fill = Response, label = Response)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "none") +
  xlab("Questionnaire time-point") + ylab("Frequency") +
  ggtitle("Pray to God even if not in trouble - Partners") + theme(plot.title = element_text(hjust = 0.5))

pray_age28_p

# Save this plot
pdf("./Results/pray_Age28_Partners.pdf", height = 6, width = 10)
plot(pray_age28_p)
dev.off()

## Displaying these results as standard cross-tabs (using only complete cases, so data is identical to that in the sankey plot)

# Age 6 by age 9
data %>%
  select(pj7043, pm4043, FC3030) %>%
  rename(age6 = pj7043, age9 = pm4043, age28 = FC3030) %>%
  filter(complete.cases(age6, age9, age28)) %>%
  group_by(age6, age9) %>%
  summarise(freq = n())

# Age 9 by age 28
data %>%
  select(pj7043, pm4043, FC3030) %>%
  rename(age6 = pj7043, age9 = pm4043, age28 = FC3030) %>%
  filter(complete.cases(age6, age9, age28)) %>%
  group_by(age9, age28) %>%
  summarise(freq = n())

# Proportions at each time-point
tab_age28_p <-data %>%
  select(pj7043, pm4043, FC3030) %>%
  rename(age6 = pj7043, age9 = pm4043, age28 = FC3030) %>%
  filter(complete.cases(age6, age9, age28))
summary(tab_age28_p)

## Very similar pattern of results to the mothers: Slight decrease in 'yes' responses from age 9 to age 28
sum(data_temp_age28_p$freq)



#####################################################################################################
#### Religious affiliation (grouping Christians) - Responses: Christian vs None vs Other

### Mothers

## Pregnancy
table(data$d813, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(d813 = na_if(d813, "-1")) %>%
  mutate(d813_grpXian = recode(d813, "Buddhist" = "Other", "Hindu" = "Other", "Jewish" = "Other", "Muslim" = "Other",
                       "Rastafarian" = "Other", "Sikh" = "Other")) %>%
  mutate(d813_grpXian = recode(d813_grpXian, "C of E" = "Christian", "Christian SCI" = "Christian", 
                       "Jehovah Witness" = "Christian", "Mormon" = "Christian", "Other Christian" = "Christian",
                       "Roman Catholic" = "Christian")) %>%
  mutate(d813_grpXian = factor(d813_grpXian, levels = c("Christian", "None", "Other")))

table(data$d813_grpXian, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data, but not especially biased towards non-believers, so will leave as is.
table(data$d810[is.na(data$d813_grpXian)])

## Age 5
table(data$k6243, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(k6243 = na_if(k6243, "No response")) %>%
  mutate(k6243 = na_if(k6243, "Not completed")) %>%
  mutate(k6243 = na_if(k6243, "Triplet / quadruplet")) %>%
  mutate(k6243_grpXian = recode(k6243, "Buddhist" = "Other", "Hindu" = "Other", "Jewish" = "Other", "Muslim" = "Other",
                               "Rastafarian" = "Other", "Sikh" = "Other")) %>%
  mutate(k6243_grpXian = recode(k6243_grpXian, "Church of England" = "Christian", "Christian Science" = "Christian", 
                               "Jehovahs Witness" = "Christian", "Mormon" = "Christian", 
                               "Other Christian" = "Christian", "Roman Catholic" = "Christian")) %>%
  mutate(k6243_grpXian = factor(k6243_grpXian, levels = c("Christian", "None", "Other")))

table(data$k6243_grpXian, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data, and is somewhat biased towards non-believers, but given that I haven't altered the results above will leave as is.
table(data$k6240[is.na(data$k6243_grpXian)])

## Age 6
table(data$l7044, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(l7044 = na_if(l7044, "No response")) %>%
  mutate(l7044 = na_if(l7044, "Not completed")) %>%
  mutate(l7044 = na_if(l7044, "Don't know")) %>%
  mutate(l7044 = na_if(l7044, "Triplet / quadruplet")) %>%
  mutate(l7044_grpXian = recode(l7044, "Buddhist" = "Other", "Hindu" = "Other", "Jewish" = "Other", "Muslim" = "Other",
                                "Rastafarian" = "Other", "Sikh" = "Other")) %>%
  mutate(l7044_grpXian = recode(l7044_grpXian, "Church of England" = "Christian", "Christian Science" = "Christian", 
                                "Jehovah's Witness" = "Christian", "Mormon" = "Christian", 
                                "Methodist, Baptist/other Protestant Christian" = "Christian", 
                                "Roman Catholic" = "Christian")) %>%
  mutate(l7044_grpXian = factor(l7044_grpXian, levels = c("Christian", "None", "Other")))

table(data$l7044_grpXian, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data, and is somewhat biased towards non-believers, but given that I haven't altered the results above will leave as is.
table(data$l7040[is.na(data$l7044_grpXian)])

## Age 9
table(data$p4044, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(p4044 = na_if(p4044, "No response")) %>%
  mutate(p4044 = na_if(p4044, "Not completed")) %>%
  mutate(p4044 = na_if(p4044, "Triplet / quadruplet")) %>%
  mutate(p4044_grpXian = recode(p4044, "Buddhist" = "Other", "Hindu" = "Other", "Jewish" = "Other", "Muslim" = "Other",
                                "Rastafarian" = "Other", "Sikh" = "Other")) %>%
  mutate(p4044_grpXian = recode(p4044_grpXian, "Church of England" = "Christian", "Christian Science" = "Christian", 
                                "Jehovah's Witness" = "Christian", "Mormon" = "Christian", 
                                "Methodist, Baptist or other Christian" = "Christian", 
                                "Roman Catholic" = "Christian")) %>%
  mutate(p4044_grpXian = factor(p4044_grpXian, levels = c("Christian", "None", "Other")))

table(data$p4044_grpXian, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data, and is somewhat biased towards non-believers, but given that I haven't altered the results above will leave as is.
table(data$p4040[is.na(data$p4044_grpXian)])

## 2019 (Age 28)
table(data$Y3040, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(Y3040 = na_if(Y3040, "Missing")) %>%
  mutate(Y3040 = na_if(Y3040, "Missed whole section C")) %>%
  mutate(Y3040 = na_if(Y3040, "Mother of trip/quad")) %>%
  mutate(Y3040 = na_if(Y3040, "Questionnaire not completed")) %>%
  mutate(Y3040 = na_if(Y3040, "Unresolvable")) %>%
  mutate(Y3040_grpXian = recode(Y3040, "Buddhist" = "Other", "Jewish" = "Other", "Muslim" = "Other",
                                "Rastafarian" = "Other", "Sikh or Hindu" = "Other",
                                "Other (e.g. New Age, Taoist, Spiritualist)" = "Other")) %>%
  mutate(Y3040_grpXian = recode(Y3040_grpXian, "Church of England" = "Christian", "Baptist/Evangelical" = "Christian", 
                                "Jehovahs Witness" = "Christian", "Mormon" = "Christian", "Methodist" = "Christian",
                                "Other Christian (e.g. Christian Science, Mormon, Presbyterian, Evangelical, Orth" 
                                = "Christian", 
                                "Roman Catholic" = "Christian")) %>%
  mutate(Y3040_grpXian = factor(Y3040_grpXian, levels = c("Christian", "None", "Other")))

table(data$Y3040_grpXian, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is a little missing data, and is somewhat biased towards non-believers, but given that I haven't altered the results above will leave as is.
table(data$Y3000[is.na(data$Y3040_grpXian)])


## Turn this data into a sankey plot - Both for pregnancy to 2019, and just from pregnancy to age 9 (as less missing data)

# First, have to convert the data to the correct format and remove any missing values. Will first convert the data to summary wide format, then convert to long/lode format. Age age 5 and 6 are very close together, to get a better sense of change over time will drop the age 6 data.
data_temp_age9 <- data %>%
  select(d813_grpXian, k6243_grpXian, p4044_grpXian) %>%
  rename(preg = d813_grpXian, age5 = k6243_grpXian, age9 = p4044_grpXian) %>%
  filter(complete.cases(preg, age5, age9)) %>%
  group_by(preg, age5, age9) %>%
  summarise(freq = n())

summary(data_temp_age9)
head(data_temp_age9)

data_temp_lodes_age9 <- to_lodes_form(data_temp_age9, axes = 1:3, id = "traj")
data_temp_lodes_age9 <- data_temp_lodes_age9 %>%
  rename(time = x, Response = stratum)
head(data_temp_lodes_age9)
summary(data_temp_lodes_age9)

affiliation_age9 <- ggplot(data_temp_lodes_age9,
                         aes(x = time, stratum = Response, alluvium = traj,
                             y = freq,
                             fill = Response, label = Response)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "none") +
  xlab("Questionnaire time-point") + ylab("Frequency") +
  ggtitle("Religious affiliation - Mothers") + theme(plot.title = element_text(hjust = 0.5))

affiliation_age9

# Save this plot
pdf("./Results/religAffiliation_Age9_Mothers.pdf", height = 6, width = 10)
plot(affiliation_age9)
dev.off()


## Displaying these results as standard cross-tabs (using only complete cases, so data is identical to that in the sankey plot)

# preg by age 5
data %>%
  select(d813_grpXian, k6243_grpXian, p4044_grpXian) %>%
  rename(preg = d813_grpXian, age5 = k6243_grpXian, age9 = p4044_grpXian) %>%
  filter(complete.cases(preg, age5, age9)) %>%
  group_by(preg, age5) %>%
  summarise(freq = n())

# Age 5 by age 9
data %>%
  select(d813_grpXian, k6243_grpXian, p4044_grpXian) %>%
  rename(preg = d813_grpXian, age5 = k6243_grpXian, age9 = p4044_grpXian) %>%
  filter(complete.cases(preg, age5, age9)) %>%
  group_by(age5, age9) %>%
  summarise(freq = n())

# Proportions at each time-point
tab_age9 <-data %>%
  select(d813_grpXian, k6243_grpXian, p4044_grpXian) %>%
  rename(preg = d813_grpXian, age5 = k6243_grpXian, age9 = p4044_grpXian) %>%
  filter(complete.cases(preg, age5, age9))
summary(tab_age9)


### Now repeat, but including 2019 RSBB data
data_temp_age28 <- data %>%
  select(d813_grpXian, k6243_grpXian, p4044_grpXian, Y3040_grpXian) %>%
  rename(preg = d813_grpXian, age5 = k6243_grpXian, age9 = p4044_grpXian, age28 = Y3040_grpXian) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(preg, age5, age9, age28) %>%
  summarise(freq = n())

summary(data_temp_age28)
head(data_temp_age28)

data_temp_lodes_age28 <- to_lodes_form(data_temp_age28, axes = 1:4, id = "traj")
data_temp_lodes_age28 <- data_temp_lodes_age28 %>%
  rename(time = x, Response = stratum)
head(data_temp_lodes_age28)
summary(data_temp_lodes_age28)

affiliation_age28 <- ggplot(data_temp_lodes_age28,
                          aes(x = time, stratum = Response, alluvium = traj,
                              y = freq,
                              fill = Response, label = Response)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "none") +
  xlab("Questionnaire time-point") + ylab("Frequency") +
  ggtitle("Religious affiliation - Mothers") + theme(plot.title = element_text(hjust = 0.5))

affiliation_age28

# Save this plot
pdf("./Results/religAffiliation_Age28_Mothers.pdf", height = 6, width = 10)
plot(affiliation_age28)
dev.off()

## Displaying these results as standard cross-tabs (using only complete cases, so data is identical to that in the sankey plot)

# preg by age 5
data %>%
  select(d813_grpXian, k6243_grpXian, p4044_grpXian, Y3040_grpXian) %>%
  rename(preg = d813_grpXian, age5 = k6243_grpXian, age9 = p4044_grpXian, age28 = Y3040_grpXian) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(preg, age5) %>%
  summarise(freq = n())

# Age 5 by age 9
data %>%
  select(d813_grpXian, k6243_grpXian, p4044_grpXian, Y3040_grpXian) %>%
  rename(preg = d813_grpXian, age5 = k6243_grpXian, age9 = p4044_grpXian, age28 = Y3040_grpXian) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(age5, age9) %>%
  summarise(freq = n())

# Age 9 by age 28
data %>%
  select(d813_grpXian, k6243_grpXian, p4044_grpXian, Y3040_grpXian) %>%
  rename(preg = d813_grpXian, age5 = k6243_grpXian, age9 = p4044_grpXian, age28 = Y3040_grpXian) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(age9, age28) %>%
  summarise(freq = n())

# Proportions at each time-point
tab_age28 <-data %>%
  select(d813_grpXian, k6243_grpXian, p4044_grpXian, Y3040_grpXian) %>%
  rename(preg = d813_grpXian, age5 = k6243_grpXian, age9 = p4044_grpXian, age28 = Y3040_grpXian) %>%
  filter(complete.cases(preg, age5, age9, age28))
summary(tab_age28)

## Stable from preg to age 9, but then increase in 'none' and 'other' to age 28

# Sample sizes do decrease quite substantially from age 9 to age 28
sum(data_temp_age9$freq)
sum(data_temp_age28$freq)


### Now repeat for partners

## Pregnancy
table(data$pb153, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(pb153 = na_if(pb153, "-1")) %>%
  mutate(pb153_grpXian = recode(pb153, "Buddhist" = "Other", "Hindu" = "Other", "Jewish" = "Other", "Muslim" = "Other",
                               "Rastafarian" = "Other", "Sikh" = "Other")) %>%
  mutate(pb153_grpXian = recode(pb153_grpXian, "C of E" = "Christian", "Christian SCI" = "Christian", 
                               "Jehovah witness" = "Christian", "Mormon" = "Christian", 
                               "Other Christian" = "Christian", "Roman Catholic" = "Christian")) %>%
  mutate(pb153_grpXian = factor(pb153_grpXian, levels = c("Christian", "None", "Other")))

table(data$pb153_grpXian, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data, and is somewhat biased towards non-believers, but given that I haven't altered the results for mothers will leave as is.
table(data$pb150[is.na(data$pb153_grpXian)])

## Age 5
table(data$ph6243, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(ph6243 = na_if(ph6243, "No response")) %>%
  mutate(ph6243 = na_if(ph6243, "Not completed")) %>%
  mutate(ph6243 = na_if(ph6243, "Triplet / quadruplet")) %>%
  mutate(ph6243_grpXian = recode(ph6243, "Buddhist" = "Other", "Hindu" = "Other", "Jewish" = "Other", 
                                 "Muslim" = "Other", "Rastafarian" = "Other", "Sikh" = "Other")) %>%
  mutate(ph6243_grpXian = recode(ph6243_grpXian, "Church of England" = "Christian", "Christian Science" = "Christian", 
                                "Jehovah's Witness" = "Christian", "Mormon" = "Christian", 
                                "Other Christian" = "Christian", "Roman Catholic" = "Christian")) %>%
  mutate(ph6243_grpXian = factor(ph6243_grpXian, levels = c("Christian", "None", "Other")))

table(data$ph6243_grpXian, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data, and is somewhat biased towards non-believers, but given that I haven't altered the results for mothers will leave as is.
table(data$ph6240[is.na(data$ph6243_grpXian)])

## Age 6
table(data$pj7044, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(pj7044 = na_if(pj7044, "No response")) %>%
  mutate(pj7044 = na_if(pj7044, "Not completed")) %>%
  mutate(pj7044 = na_if(pj7044, "Triplet / quadruplet")) %>%
  mutate(pj7044_grpXian = recode(pj7044, "Buddhist" = "Other", "Hindu" = "Other", "Jewish" = "Other", 
                                 "Muslim" = "Other", "Rastafarian" = "Other", "Sikh" = "Other")) %>%
  mutate(pj7044_grpXian = recode(pj7044_grpXian, "Church of England" = "Christian", "Christian Science" = "Christian", 
                                "Jehovah's Witness" = "Christian", "Mormon" = "Christian", 
                                "Methodist/Baptist/Protestant Christian" = "Christian", 
                                "Roman Catholic" = "Christian")) %>%
  mutate(pj7044_grpXian = factor(pj7044_grpXian, levels = c("Christian", "None", "Other")))

table(data$pj7044_grpXian, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data, and is somewhat biased towards non-believers, but given that I haven't altered the results for mothers will leave as is.
table(data$pj7040[is.na(data$pj7044_grpXian)])

## Age 9
table(data$pm4044, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(pm4044 = na_if(pm4044, "No response")) %>%
  mutate(pm4044 = na_if(pm4044, "Not completed")) %>%
  mutate(pm4044 = na_if(pm4044, "Triplet / quadruplet")) %>%
  mutate(pm4044_grpXian = recode(pm4044, "Buddhist" = "Other", "Hindu" = "Other", "Jewish" = "Other", 
                                 "Muslim" = "Other", "Rastafarian" = "Other", "Sikh" = "Other")) %>%
  mutate(pm4044_grpXian = recode(pm4044_grpXian, "Church of England" = "Christian", "Christian Science" = "Christian", 
                                "Jehovah's Witness" = "Christian", "Mormon" = "Christian", 
                                "Methodist, Baptist or other Christian" = "Christian", 
                                "Roman Catholic" = "Christian")) %>%
  mutate(pm4044_grpXian = factor(pm4044_grpXian, levels = c("Christian", "None", "Other")))

table(data$pm4044_grpXian, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data, and is somewhat biased towards non-believers, but given that I haven't altered the results for mothers will leave as is.
table(data$pm4040[is.na(data$pm4044_grpXian)])

## 2019 (Age 28)
table(data$FC3040, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(FC3040 = na_if(FC3040, "Missing")) %>%
  mutate(FC3040 = na_if(FC3040, "Missed whole section C")) %>%
  mutate(FC3040 = na_if(FC3040, "Mother of trip/quad")) %>%
  mutate(FC3040 = na_if(FC3040, "Questionnaire not completed")) %>%
  mutate(FC3040 = na_if(FC3040, "Unresolvable")) %>%
  mutate(FC3040_grpXian = recode(FC3040, "Buddhist" = "Other", "Jewish/Sikh/Hindu/Muslim" = "Other",
                                "Other (e.g. New Age, Taoist, Spiritualist)" = "Other")) %>%
  mutate(FC3040_grpXian = recode(FC3040_grpXian, "Church of England" = "Christian", 
                                 "Baptist/Evangelical" = "Christian", "Jehovahs Witness" = "Christian", 
                                 "Mormon" = "Christian", "Methodist" = "Christian",
                                "Other Christian (e.g. Christian Science, Mormon, Presbyterian, Evangelical, Orth" 
                                = "Christian", 
                                "Roman Catholic" = "Christian")) %>%
  mutate(FC3040_grpXian = factor(FC3040_grpXian, levels = c("Christian", "None", "Other")))

table(data$FC3040_grpXian, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data, and is somewhat biased towards non-believers, but given that I haven't altered the results for mothers will leave as is.
table(data$FC3000[is.na(data$FC3040_grpXian)])


## Turn this data into a sankey plot - Both for pregnancy to 2019, and just from pregnancy to age 9 (as less missing data)

# First, have to convert the data to the correct format and remove any missing values. Will first convert the data to summary wide format, then convert to long/lode format. Age age 5 and 6 are very close together, to get a better sense of change over time will drop the age 6 data.
data_temp_age9_p <- data %>%
  select(pb153_grpXian, ph6243_grpXian, pm4044_grpXian) %>%
  rename(preg = pb153_grpXian, age5 = ph6243_grpXian, age9 = pm4044_grpXian) %>%
  filter(complete.cases(preg, age5, age9)) %>%
  group_by(preg, age5, age9) %>%
  summarise(freq = n())

summary(data_temp_age9_p)
head(data_temp_age9_p)

data_temp_lodes_age9_p <- to_lodes_form(data_temp_age9_p, axes = 1:3, id = "traj")
data_temp_lodes_age9_p <- data_temp_lodes_age9_p %>%
  rename(time = x, Response = stratum)
head(data_temp_lodes_age9_p)
summary(data_temp_lodes_age9_p)

affiliation_age9_p <- ggplot(data_temp_lodes_age9_p,
                           aes(x = time, stratum = Response, alluvium = traj,
                               y = freq,
                               fill = Response, label = Response)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "none") +
  xlab("Questionnaire time-point") + ylab("Frequency") +
  ggtitle("Religious affiliation - Partners") + theme(plot.title = element_text(hjust = 0.5))

affiliation_age9_p

# Save this plot
pdf("./Results/religAffiliation_Age9_Partners.pdf", height = 6, width = 10)
plot(affiliation_age9_p)
dev.off()


## Displaying these results as standard cross-tabs (using only complete cases, so data is identical to that in the sankey plot)

# preg by age 5
data %>%
  select(pb153_grpXian, ph6243_grpXian, pm4044_grpXian) %>%
  rename(preg = pb153_grpXian, age5 = ph6243_grpXian, age9 = pm4044_grpXian) %>%
  filter(complete.cases(preg, age5, age9)) %>%
  group_by(preg, age5) %>%
  summarise(freq = n())

# Age 5 by age 9
data %>%
  select(pb153_grpXian, ph6243_grpXian, pm4044_grpXian) %>%
  rename(preg = pb153_grpXian, age5 = ph6243_grpXian, age9 = pm4044_grpXian) %>%
  filter(complete.cases(preg, age5, age9)) %>%
  group_by(age5, age9) %>%
  summarise(freq = n())

# Proportions at each time-point
tab_age9_p <-data %>%
  select(pb153_grpXian, ph6243_grpXian, pm4044_grpXian) %>%
  rename(preg = pb153_grpXian, age5 = ph6243_grpXian, age9 = pm4044_grpXian) %>%
  filter(complete.cases(preg, age5, age9))
summary(tab_age9_p)


### Now repeat, but including 2019 RSBB data
data_temp_age28_p <- data %>%
  select(pb153_grpXian, ph6243_grpXian, pm4044_grpXian, FC3040_grpXian) %>%
  rename(preg = pb153_grpXian, age5 = ph6243_grpXian, age9 = pm4044_grpXian, age28 = FC3040_grpXian) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(preg, age5, age9, age28) %>%
  summarise(freq = n())

summary(data_temp_age28_p)
head(data_temp_age28_p)

data_temp_lodes_age28_p <- to_lodes_form(data_temp_age28_p, axes = 1:4, id = "traj")
data_temp_lodes_age28_p <- data_temp_lodes_age28_p %>%
  rename(time = x, Response = stratum)
head(data_temp_lodes_age28_p)
summary(data_temp_lodes_age28_p)

affiiation_age28_p <- ggplot(data_temp_lodes_age28_p,
                            aes(x = time, stratum = Response, alluvium = traj,
                                y = freq,
                                fill = Response, label = Response)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "none") +
  xlab("Questionnaire time-point") + ylab("Frequency") +
  ggtitle("Religious affiliation - Partners") + theme(plot.title = element_text(hjust = 0.5))

affiiation_age28_p

# Save this plot
pdf("./Results/religAffiliation_Age28_Partners.pdf", height = 6, width = 10)
plot(affiiation_age28_p)
dev.off()

## Displaying these results as standard cross-tabs (using only complete cases, so data is identical to that in the sankey plot)

# preg by age 5
data %>%
  select(pb153_grpXian, ph6243_grpXian, pm4044_grpXian, FC3040_grpXian) %>%
  rename(preg = pb153_grpXian, age5 = ph6243_grpXian, age9 = pm4044_grpXian, age28 = FC3040_grpXian) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(preg, age5) %>%
  summarise(freq = n())

# Age 5 by age 9
data %>%
  select(pb153_grpXian, ph6243_grpXian, pm4044_grpXian, FC3040_grpXian) %>%
  rename(preg = pb153_grpXian, age5 = ph6243_grpXian, age9 = pm4044_grpXian, age28 = FC3040_grpXian) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(age5, age9) %>%
  summarise(freq = n())

# Age 9 by age 28
data %>%
  select(pb153_grpXian, ph6243_grpXian, pm4044_grpXian, FC3040_grpXian) %>%
  rename(preg = pb153_grpXian, age5 = ph6243_grpXian, age9 = pm4044_grpXian, age28 = FC3040_grpXian) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(age9, age28) %>%
  summarise(freq = n())

# Proportions at each time-point
tab_age28_p <-data %>%
  select(pb153_grpXian, ph6243_grpXian, pm4044_grpXian, FC3040_grpXian) %>%
  rename(preg = pb153_grpXian, age5 = ph6243_grpXian, age9 = pm4044_grpXian, age28 = FC3040_grpXian) %>%
  filter(complete.cases(preg, age5, age9, age28))
summary(tab_age28_p)

## Very similar pattern of results to the mothers: Quite stable from preg to age 9, then increase in 'none' and drop in 'Christian' at age 28

# Again, sample sizes do decrease quite substantially from age 9 to age 28
sum(data_temp_age9_p$freq)
sum(data_temp_age28_p$freq)



#####################################################################################################
#### Religious affiliation (splitting Christians) - Responses: Church of England vs Roman Catholic vs Other Christian vs None vs Other

### Mothers

## Pregnancy
table(data$d813, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(d813 = na_if(d813, "-1")) %>%
  mutate(d813_splitXian = recode(d813, "Buddhist" = "Other", "Hindu" = "Other", "Jewish" = "Other", "Muslim" = "Other",
                               "Rastafarian" = "Other", "Sikh" = "Other")) %>%
  mutate(d813_splitXian = recode(d813_splitXian, "C of E" = "Church of England", 
                               "Christian SCI" = "Other Christian", "Jehovah Witness" = "Other Christian", 
                               "Mormon" = "Other Christian", "Other Christian" = "Other Christian")) %>%
  mutate(d813_splitXian = factor(d813_splitXian, levels = c("Church of England", "Roman Catholic", 
                                                            "Other Christian", "None", "Other")))

table(data$d813_splitXian, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data, but not especially biased towards non-believers, so will leave as is.
table(data$d810[is.na(data$d813_splitXian)])

## Age 5
table(data$k6243, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(k6243 = na_if(k6243, "No response")) %>%
  mutate(k6243 = na_if(k6243, "Not completed")) %>%
  mutate(k6243 = na_if(k6243, "Triplet / quadruplet")) %>%
  mutate(k6243_splitXian = recode(k6243, "Buddhist" = "Other", "Hindu" = "Other", "Jewish" = "Other", 
                                  "Muslim" = "Other", "Rastafarian" = "Other", "Sikh" = "Other")) %>%
  mutate(k6243_splitXian = recode(k6243_splitXian, "Christian Science" = "Other Christian", 
                                "Jehovahs Witness" = "Other Christian", "Mormon" = "Other Christian", 
                                "Other Christian" = "Other Christian")) %>%
  mutate(k6243_splitXian = factor(k6243_splitXian, levels = c("Church of England", "Roman Catholic", 
                                                              "Other Christian", "None", "Other")))

table(data$k6243_splitXian, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data, and is somewhat biased towards non-believers, but given that I haven't altered the results above will leave as is.
table(data$k6240[is.na(data$k6243_splitXian)])

## Age 6
table(data$l7044, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(l7044 = na_if(l7044, "No response")) %>%
  mutate(l7044 = na_if(l7044, "Not completed")) %>%
  mutate(l7044 = na_if(l7044, "Don't know")) %>%
  mutate(l7044 = na_if(l7044, "Triplet / quadruplet")) %>%
  mutate(l7044_splitXian = recode(l7044, "Buddhist" = "Other", "Hindu" = "Other", "Jewish" = "Other", 
                                  "Muslim" = "Other", "Rastafarian" = "Other", "Sikh" = "Other")) %>%
  mutate(l7044_splitXian = recode(l7044_splitXian, "Christian Science" = "Other Christian", 
                                "Jehovah's Witness" = "Other Christian", "Mormon" = "Other Christian", 
                                "Methodist, Baptist/other Protestant Christian" = "Other Christian")) %>%
  mutate(l7044_splitXian = factor(l7044_splitXian, levels = c("Church of England", "Roman Catholic", 
                                                              "Other Christian", "None", "Other")))

table(data$l7044_splitXian, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data, and is somewhat biased towards non-believers, but given that I haven't altered the results above will leave as is.
table(data$l7040[is.na(data$l7044_splitXian)])

## Age 9
table(data$p4044, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(p4044 = na_if(p4044, "No response")) %>%
  mutate(p4044 = na_if(p4044, "Not completed")) %>%
  mutate(p4044 = na_if(p4044, "Triplet / quadruplet")) %>%
  mutate(p4044_splitXian = recode(p4044, "Buddhist" = "Other", "Hindu" = "Other", "Jewish" = "Other", 
                                  "Muslim" = "Other", "Rastafarian" = "Other", "Sikh" = "Other")) %>%
  mutate(p4044_splitXian = recode(p4044_splitXian, "Christian Science" = "OTher Christian", 
                                "Jehovah's Witness" = "Other Christian", "Mormon" = "Other Christian", 
                                "Methodist, Baptist or other Christian" = "Other Christian")) %>%
  mutate(p4044_splitXian = factor(p4044_splitXian, levels = c("Church of England", "Roman Catholic", 
                                                              "Other Christian", "None", "Other")))

table(data$p4044_splitXian, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data, and is somewhat biased towards non-believers, but given that I haven't altered the results above will leave as is.
table(data$p4040[is.na(data$p4044_splitXian)])

## 2019 (Age 28)
table(data$Y3040, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(Y3040 = na_if(Y3040, "Missing")) %>%
  mutate(Y3040 = na_if(Y3040, "Missed whole section C")) %>%
  mutate(Y3040 = na_if(Y3040, "Mother of trip/quad")) %>%
  mutate(Y3040 = na_if(Y3040, "Questionnaire not completed")) %>%
  mutate(Y3040 = na_if(Y3040, "Unresolvable")) %>%
  mutate(Y3040_splitXian = recode(Y3040, "Buddhist" = "Other", "Jewish" = "Other", "Muslim" = "Other",
                                "Rastafarian" = "Other", "Sikh or Hindu" = "Other",
                                "Other (e.g. New Age, Taoist, Spiritualist)" = "Other")) %>%
  mutate(Y3040_splitXian = recode(Y3040_splitXian, "Baptist/Evangelical" = "Other Christian", 
                                "Jehovahs Witness" = "Other Christian", "Mormon" = "Other Christian", 
                                "Methodist" = "Other Christian",
                                "Other Christian (e.g. Christian Science, Mormon, Presbyterian, Evangelical, Orth" 
                                = "Other Christian")) %>%
  mutate(Y3040_splitXian = factor(Y3040_splitXian, levels = c("Church of England", "Roman Catholic", 
                                                              "Other Christian", "None", "Other")))

table(data$Y3040_splitXian, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is a little missing data, and is somewhat biased towards non-believers, but given that I haven't altered the results above will leave as is.
table(data$Y3000[is.na(data$Y3040_splitXian)])


## Turn this data into a sankey plot - Both for pregnancy to 2019, and just from pregnancy to age 9 (as less missing data)

# First, have to convert the data to the correct format and remove any missing values. Will first convert the data to summary wide format, then convert to long/lode format. Age age 5 and 6 are very close together, to get a better sense of change over time will drop the age 6 data.
data_temp_age9 <- data %>%
  select(d813_splitXian, k6243_splitXian, p4044_splitXian) %>%
  rename(preg = d813_splitXian, age5 = k6243_splitXian, age9 = p4044_splitXian) %>%
  filter(complete.cases(preg, age5, age9)) %>%
  group_by(preg, age5, age9) %>%
  summarise(freq = n())

summary(data_temp_age9)
head(data_temp_age9)

data_temp_lodes_age9 <- to_lodes_form(data_temp_age9, axes = 1:3, id = "traj")
data_temp_lodes_age9 <- data_temp_lodes_age9 %>%
  rename(time = x, Response = stratum)
head(data_temp_lodes_age9)
summary(data_temp_lodes_age9)

affiliation_splitXian_age9 <- ggplot(data_temp_lodes_age9,
                           aes(x = time, stratum = Response, alluvium = traj,
                               y = freq,
                               fill = Response, label = Response)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "none") +
  xlab("Questionnaire time-point") + ylab("Frequency") +
  ggtitle("Religious affiliation - Mothers") + theme(plot.title = element_text(hjust = 0.5))

affiliation_splitXian_age9

# Save this plot
pdf("./Results/religAffiliation_splitXian_Age9_Mothers.pdf", height = 6, width = 10)
plot(affiliation_splitXian_age9)
dev.off()


## Displaying these results as standard cross-tabs (using only complete cases, so data is identical to that in the sankey plot)

# preg by age 5
data %>%
  select(d813_splitXian, k6243_splitXian, p4044_splitXian) %>%
  rename(preg = d813_splitXian, age5 = k6243_splitXian, age9 = p4044_splitXian) %>%
  filter(complete.cases(preg, age5, age9)) %>%
  group_by(preg, age5) %>%
  summarise(freq = n())

# Age 5 by age 9
data %>%
  select(d813_splitXian, k6243_splitXian, p4044_splitXian) %>%
  rename(preg = d813_splitXian, age5 = k6243_splitXian, age9 = p4044_splitXian) %>%
  filter(complete.cases(preg, age5, age9)) %>%
  group_by(age5, age9) %>%
  summarise(freq = n())

# Proportions at each time-point
tab_age9 <-data %>%
  select(d813_splitXian, k6243_splitXian, p4044_splitXian) %>%
  rename(preg = d813_splitXian, age5 = k6243_splitXian, age9 = p4044_splitXian) %>%
  filter(complete.cases(preg, age5, age9))
summary(tab_age9)


### Now repeat, but including 2019 RSBB data
data_temp_age28 <- data %>%
  select(d813_splitXian, k6243_splitXian, p4044_splitXian, Y3040_splitXian) %>%
  rename(preg = d813_splitXian, age5 = k6243_splitXian, age9 = p4044_splitXian, age28 = Y3040_splitXian) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(preg, age5, age9, age28) %>%
  summarise(freq = n())

summary(data_temp_age28)
head(data_temp_age28)

data_temp_lodes_age28 <- to_lodes_form(data_temp_age28, axes = 1:4, id = "traj")
data_temp_lodes_age28 <- data_temp_lodes_age28 %>%
  rename(time = x, Response = stratum)
head(data_temp_lodes_age28)
summary(data_temp_lodes_age28)

affiliation_splitXian_age28 <- ggplot(data_temp_lodes_age28,
                            aes(x = time, stratum = Response, alluvium = traj,
                                y = freq,
                                fill = Response, label = Response)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 2) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "none") +
  xlab("Questionnaire time-point") + ylab("Frequency") +
  ggtitle("Religious affiliation - Mothers") + theme(plot.title = element_text(hjust = 0.5))

affiliation_splitXian_age28

# Save this plot
pdf("./Results/religAffiliation_splitXian_Age28_Mothers.pdf", height = 6, width = 10)
plot(affiliation_splitXian_age28)
dev.off()

## Displaying these results as standard cross-tabs (using only complete cases, so data is identical to that in the sankey plot)

# preg by age 5
data %>%
  select(d813_splitXian, k6243_splitXian, p4044_splitXian, Y3040_splitXian) %>%
  rename(preg = d813_splitXian, age5 = k6243_splitXian, age9 = p4044_splitXian, age28 = Y3040_splitXian) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(preg, age5) %>%
  summarise(freq = n())

# Age 5 by age 9
data %>%
  select(d813_splitXian, k6243_splitXian, p4044_splitXian, Y3040_splitXian) %>%
  rename(preg = d813_splitXian, age5 = k6243_splitXian, age9 = p4044_splitXian, age28 = Y3040_splitXian) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(age5, age9) %>%
  summarise(freq = n())

# Age 9 by age 28
data %>%
  select(d813_splitXian, k6243_splitXian, p4044_splitXian, Y3040_splitXian) %>%
  rename(preg = d813_splitXian, age5 = k6243_splitXian, age9 = p4044_splitXian, age28 = Y3040_splitXian) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(age9, age28) %>%
  summarise(freq = n())

# Proportions at each time-point
tab_age28 <-data %>%
  select(d813_splitXian, k6243_splitXian, p4044_splitXian, Y3040_splitXian) %>%
  rename(preg = d813_splitXian, age5 = k6243_splitXian, age9 = p4044_splitXian, age28 = Y3040_splitXian) %>%
  filter(complete.cases(preg, age5, age9, age28))
summary(tab_age28)

## Stable from preg to age 9, but then increase in 'none' and 'other' to age 28

# Sample sizes do decrease quite substantially from age 9 to age 28
sum(data_temp_age9$freq)
sum(data_temp_age28$freq)


### Now repeat for partners

## Pregnancy
table(data$pb153, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(pb153 = na_if(pb153, "-1")) %>%
  mutate(pb153_splitXian = recode(pb153, "Buddhist" = "Other", "Hindu" = "Other", "Jewish" = "Other", 
                                  "Muslim" = "Other",  "Rastafarian" = "Other", "Sikh" = "Other")) %>%
  mutate(pb153_splitXian = recode(pb153_splitXian, "C of E" = "Church of England", 
                                  "Christian SCI" = "Other Christian", "Jehovah witness" = "Other Christian", 
                                  "Mormon" = "Other Christian", "Other Christian" = "Other Christian")) %>%
  mutate(pb153_splitXian = factor(pb153_splitXian, levels = c("Church of England", "Roman Catholic", 
                                                              "Other Christian", "None", "Other")))

table(data$pb153_splitXian, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data, and is somewhat biased towards non-believers, but given that I haven't altered the results for mothers will leave as is.
table(data$pb150[is.na(data$pb153_splitXian)])

## Age 5
table(data$ph6243, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(ph6243 = na_if(ph6243, "No response")) %>%
  mutate(ph6243 = na_if(ph6243, "Not completed")) %>%
  mutate(ph6243 = na_if(ph6243, "Triplet / quadruplet")) %>%
  mutate(ph6243_splitXian = recode(ph6243, "Buddhist" = "Other", "Hindu" = "Other", "Jewish" = "Other", 
                                 "Muslim" = "Other", "Rastafarian" = "Other", "Sikh" = "Other")) %>%
  mutate(ph6243_splitXian = recode(ph6243_splitXian, "Christian Science" = "Other Christian", 
                                 "Jehovah's Witness" = "Other Christian", "Mormon" = "Other Christian", 
                                 "Other Christian" = "Other Christian")) %>%
  mutate(ph6243_splitXian = factor(ph6243_splitXian, levels = c("Church of England", "Roman Catholic", 
                                                                "Other Christian", "None", "Other")))

table(data$ph6243_splitXian, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data, and is somewhat biased towards non-believers, but given that I haven't altered the results for mothers will leave as is.
table(data$ph6240[is.na(data$ph6243_splitXian)])

## Age 6
table(data$pj7044, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(pj7044 = na_if(pj7044, "No response")) %>%
  mutate(pj7044 = na_if(pj7044, "Not completed")) %>%
  mutate(pj7044 = na_if(pj7044, "Triplet / quadruplet")) %>%
  mutate(pj7044_splitXian = recode(pj7044, "Buddhist" = "Other", "Hindu" = "Other", "Jewish" = "Other", 
                                 "Muslim" = "Other", "Rastafarian" = "Other", "Sikh" = "Other")) %>%
  mutate(pj7044_splitXian = recode(pj7044_splitXian, "Christian Science" = "Other Christian", 
                                 "Jehovah's Witness" = "Other Christian", "Mormon" = "Other Christian", 
                                 "Methodist/Baptist/Protestant Christian" = "Other Christian")) %>%
  mutate(pj7044_splitXian = factor(pj7044_splitXian, levels = c("Church of England", "Roman Catholic", 
                                                                "Other Christian", "None", "Other")))

table(data$pj7044_splitXian, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data, and is somewhat biased towards non-believers, but given that I haven't altered the results for mothers will leave as is.
table(data$pj7040[is.na(data$pj7044_splitXian)])

## Age 9
table(data$pm4044, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(pm4044 = na_if(pm4044, "No response")) %>%
  mutate(pm4044 = na_if(pm4044, "Not completed")) %>%
  mutate(pm4044 = na_if(pm4044, "Triplet / quadruplet")) %>%
  mutate(pm4044_splitXian = recode(pm4044, "Buddhist" = "Other", "Hindu" = "Other", "Jewish" = "Other", 
                                 "Muslim" = "Other", "Rastafarian" = "Other", "Sikh" = "Other")) %>%
  mutate(pm4044_splitXian = recode(pm4044_splitXian, "Christian Science" = "Other Christian", 
                                 "Jehovah's Witness" = "Other Christian", "Mormon" = "Other Christian", 
                                 "Methodist, Baptist or other Christian" = "Other Christian")) %>%
  mutate(pm4044_splitXian = factor(pm4044_splitXian, levels = c("Church of England", "Roman Catholic", 
                                                                "Other Christian", "None", "Other")))

table(data$pm4044_splitXian, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data, and is somewhat biased towards non-believers, but given that I haven't altered the results for mothers will leave as is.
table(data$pm4040[is.na(data$pm4044_splitXian)])

## 2019 (Age 28)
table(data$FC3040, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(FC3040 = na_if(FC3040, "Missing")) %>%
  mutate(FC3040 = na_if(FC3040, "Missed whole section C")) %>%
  mutate(FC3040 = na_if(FC3040, "Mother of trip/quad")) %>%
  mutate(FC3040 = na_if(FC3040, "Questionnaire not completed")) %>%
  mutate(FC3040 = na_if(FC3040, "Unresolvable")) %>%
  mutate(FC3040_splitXian = recode(FC3040, "Buddhist" = "Other", "Jewish/Sikh/Hindu/Muslim" = "Other",
                                 "Other (e.g. New Age, Taoist, Spiritualist)" = "Other")) %>%
  mutate(FC3040_splitXian = recode(FC3040_splitXian, "Baptist/Evangelical" = "Other Christian", 
                                   "Jehovahs Witness" = "Other Christian", "Mormon" = "Other Christian", 
                                   "Methodist" = "Other Christian",
                                 "Other Christian (e.g. Christian Science, Mormon, Presbyterian, Evangelical, Orth" 
                                 = "Other Christian")) %>%
  mutate(FC3040_splitXian = factor(FC3040_splitXian, levels = c("Church of England", "Roman Catholic", 
                                                                "Other Christian", "None", "Other")))

table(data$FC3040_splitXian, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data, and is somewhat biased towards non-believers, but given that I haven't altered the results for mothers will leave as is.
table(data$FC3000[is.na(data$FC3040_splitXian)])


## Turn this data into a sankey plot - Both for pregnancy to 2019, and just from pregnancy to age 9 (as less missing data)

# First, have to convert the data to the correct format and remove any missing values. Will first convert the data to summary wide format, then convert to long/lode format. Age age 5 and 6 are very close together, to get a better sense of change over time will drop the age 6 data.
data_temp_age9_p <- data %>%
  select(pb153_splitXian, ph6243_splitXian, pm4044_splitXian) %>%
  rename(preg = pb153_splitXian, age5 = ph6243_splitXian, age9 = pm4044_splitXian) %>%
  filter(complete.cases(preg, age5, age9)) %>%
  group_by(preg, age5, age9) %>%
  summarise(freq = n())

summary(data_temp_age9_p)
head(data_temp_age9_p)

data_temp_lodes_age9_p <- to_lodes_form(data_temp_age9_p, axes = 1:3, id = "traj")
data_temp_lodes_age9_p <- data_temp_lodes_age9_p %>%
  rename(time = x, Response = stratum)
head(data_temp_lodes_age9_p)
summary(data_temp_lodes_age9_p)

affiliation_splitXian_age9_p <- ggplot(data_temp_lodes_age9_p,
                             aes(x = time, stratum = Response, alluvium = traj,
                                 y = freq,
                                 fill = Response, label = Response)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "none") +
  xlab("Questionnaire time-point") + ylab("Frequency") +
  ggtitle("Religious affiliation - Partners") + theme(plot.title = element_text(hjust = 0.5))

affiliation_splitXian_age9_p

# Save this plot
pdf("./Results/religAffiliation_splitXian_Age9_Partners.pdf", height = 6, width = 10)
plot(affiliation_splitXian_age9_p)
dev.off()


## Displaying these results as standard cross-tabs (using only complete cases, so data is identical to that in the sankey plot)

# preg by age 5
data %>%
  select(pb153_splitXian, ph6243_splitXian, pm4044_splitXian) %>%
  rename(preg = pb153_splitXian, age5 = ph6243_splitXian, age9 = pm4044_splitXian) %>%
  filter(complete.cases(preg, age5, age9)) %>%
  group_by(preg, age5) %>%
  summarise(freq = n())

# Age 5 by age 9
data %>%
  select(pb153_splitXian, ph6243_splitXian, pm4044_splitXian) %>%
  rename(preg = pb153_splitXian, age5 = ph6243_splitXian, age9 = pm4044_splitXian) %>%
  filter(complete.cases(preg, age5, age9)) %>%
  group_by(age5, age9) %>%
  summarise(freq = n())

# Proportions at each time-point
tab_age9_p <-data %>%
  select(pb153_splitXian, ph6243_splitXian, pm4044_splitXian) %>%
  rename(preg = pb153_splitXian, age5 = ph6243_splitXian, age9 = pm4044_splitXian) %>%
  filter(complete.cases(preg, age5, age9))
summary(tab_age9_p)


### Now repeat, but including 2019 RSBB data
data_temp_age28_p <- data %>%
  select(pb153_splitXian, ph6243_splitXian, pm4044_splitXian, FC3040_splitXian) %>%
  rename(preg = pb153_splitXian, age5 = ph6243_splitXian, age9 = pm4044_splitXian, age28 = FC3040_splitXian) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(preg, age5, age9, age28) %>%
  summarise(freq = n())

summary(data_temp_age28_p)
head(data_temp_age28_p)

data_temp_lodes_age28_p <- to_lodes_form(data_temp_age28_p, axes = 1:4, id = "traj")
data_temp_lodes_age28_p <- data_temp_lodes_age28_p %>%
  rename(time = x, Response = stratum)
head(data_temp_lodes_age28_p)
summary(data_temp_lodes_age28_p)

affiiation_splitXian_age28_p <- ggplot(data_temp_lodes_age28_p,
                             aes(x = time, stratum = Response, alluvium = traj,
                                 y = freq,
                                 fill = Response, label = Response)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 2) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "none") +
  xlab("Questionnaire time-point") + ylab("Frequency") +
  ggtitle("Religious affiliation - Partners") + theme(plot.title = element_text(hjust = 0.5))

affiiation_splitXian_age28_p

# Save this plot
pdf("./Results/religAffiliation_splitXian_Age28_Partners.pdf", height = 6, width = 10)
plot(affiiation_splitXian_age28_p)
dev.off()

## Displaying these results as standard cross-tabs (using only complete cases, so data is identical to that in the sankey plot)

# preg by age 5
data %>%
  select(pb153_splitXian, ph6243_splitXian, pm4044_splitXian, FC3040_splitXian) %>%
  rename(preg = pb153_splitXian, age5 = ph6243_splitXian, age9 = pm4044_splitXian, age28 = FC3040_splitXian) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(preg, age5) %>%
  summarise(freq = n())

# Age 5 by age 9
data %>%
  select(pb153_splitXian, ph6243_splitXian, pm4044_splitXian, FC3040_splitXian) %>%
  rename(preg = pb153_splitXian, age5 = ph6243_splitXian, age9 = pm4044_splitXian, age28 = FC3040_splitXian) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(age5, age9) %>%
  summarise(freq = n())

# Age 9 by age 28
data %>%
  select(pb153_splitXian, ph6243_splitXian, pm4044_splitXian, FC3040_splitXian) %>%
  rename(preg = pb153_splitXian, age5 = ph6243_splitXian, age9 = pm4044_splitXian, age28 = FC3040_splitXian) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(age9, age28) %>%
  summarise(freq = n())

# Proportions at each time-point
tab_age28_p <-data %>%
  select(pb153_splitXian, ph6243_splitXian, pm4044_splitXian, FC3040_splitXian) %>%
  rename(preg = pb153_splitXian, age5 = ph6243_splitXian, age9 = pm4044_splitXian, age28 = FC3040_splitXian) %>%
  filter(complete.cases(preg, age5, age9, age28))
summary(tab_age28_p)

## Very similar pattern of results to the mothers: Catholics are quite stable, while most change is from C of E to None.

# Again, sample sizes do decrease quite substantially from age 9 to age 28
sum(data_temp_age9_p$freq)
sum(data_temp_age28_p$freq)



#####################################################################################################
#### Length of time had particulat faith - Responses: All life vs > 5 years vs < 5 years

### Mothers

## Pregnancy
table(data$d815, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(d815 = na_if(d815, "-1")) %>%
  mutate(d815 = recode(d815, "<1 YR" = "<5 Years", "1-2 YRS" = "<5 Years", "3-5 YRS" = "<5 Years",
                       ">5 YRS" = ">5 Years", "All my life" = "All life")) %>%
  mutate(d815 = factor(d815, levels = c("All life", ">5 Years", "<5 Years")))

table(data$d815, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data which is biased towards non-believers, but it's not clear how long they have been non-believers, so will leave as is.
table(data$d810[is.na(data$d815)])

## Age 5
table(data$k6246, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(k6246 = na_if(k6246, "No response")) %>%
  mutate(k6246 = na_if(k6246, "Not completed")) %>%
  mutate(k6246 = na_if(k6246, "Triplet / quadruplet")) %>%
  mutate(k6246 = na_if(k6246, "Don't know")) %>%
  mutate(k6246 = na_if(k6246, "Text response")) %>%
  mutate(k6246 = recode(k6246, "< 1 year" = "<5 Years", "1-2 years" = "<5 Years", "3-5 years" = "<5 Years",
                       "> 5 years" = ">5 Years", "All my life" = "All life")) %>%
  mutate(k6246 = factor(k6246, levels = c("All life", ">5 Years", "<5 Years")))

table(data$k6246, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data which is biased towards non-believers, but it's not clear how long they have been non-believers, so will leave as is.
table(data$k6240[is.na(data$k6246)])

## Age 6
table(data$l7047, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(l7047 = na_if(l7047, "No response")) %>%
  mutate(l7047 = na_if(l7047, "Not completed")) %>%
  mutate(l7047 = na_if(l7047, "Don't know")) %>%
  mutate(l7047 = na_if(l7047, "Triplet / quadruplet")) %>%
  mutate(l7047 = na_if(l7047, "Text response")) %>%
  mutate(l7047 = recode(l7047, "< a year" = "<5 Years", "1-2 Years" = "<5 Years", "3-5 Years" = "<5 Years",
                        "> 5 years" = ">5 Years", "All respondents life" = "All life")) %>%
  mutate(l7047 = factor(l7047, levels = c("All life", ">5 Years", "<5 Years")))

table(data$l7047, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data which is biased towards non-believers, but it's not clear how long they have been non-believers, so will leave as is.
table(data$l7040[is.na(data$l7047)])

## Age 9
table(data$p4047, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(p4047 = na_if(p4047, "No response")) %>%
  mutate(p4047 = na_if(p4047, "Not completed")) %>%
  mutate(p4047 = na_if(p4047, "Triplet / quadruplet")) %>%
  mutate(p4047 = recode(p4047, "< 1 year" = "<5 Years", "1-2 years" = "<5 Years", "3-5 years" = "<5 Years",
                        "> 5 years" = ">5 Years", "All her life" = "All life")) %>%
  mutate(p4047 = factor(p4047, levels = c("All life", ">5 Years", "<5 Years")))

table(data$p4047, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data which is biased towards non-believers, but it's not clear how long they have been non-believers, so will leave as is.
table(data$p4040[is.na(data$p4047)])

## 2019 (Age 28)
table(data$Y3050, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(Y3050 = na_if(Y3050, "Missing")) %>%
  mutate(Y3050 = na_if(Y3050, "Missed whole section C")) %>%
  mutate(Y3050 = na_if(Y3050, "Mother of trip/quad")) %>%
  mutate(Y3050 = na_if(Y3050, "Questionnaire not completed")) %>%
  mutate(Y3050 = na_if(Y3050, "Unresolvable")) %>%
  mutate(Y3050 = recode(Y3050, "Less than a year" = "<5 Years", "1-2 years" = "<5 Years", "3-5 years" = "<5 Years",
                        "More than 5 years" = ">5 Years", "All of life" = "All life")) %>%
  mutate(Y3050 = factor(Y3050, levels = c("All life", ">5 Years", "<5 Years")))

table(data$Y3050, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data which is biased towards non-believers, but it's not clear how long they have been non-believers, so will leave as is.
table(data$Y3000[is.na(data$Y3050)])


## Turn this data into a sankey plot - Both for pregnancy to 2019, and just from pregnancy to age 9 (as less missing data)

# First, have to convert the data to the correct format and remove any missing values. Will first convert the data to summary wide format, then convert to long/lode format. Age age 5 and 6 are very close together, to get a better sense of change over time will drop the age 6 data.
data_temp_age9 <- data %>%
  select(d815, k6246, p4047) %>%
  rename(preg = d815, age5 = k6246, age9 = p4047) %>%
  filter(complete.cases(preg, age5, age9)) %>%
  group_by(preg, age5, age9) %>%
  summarise(freq = n())

summary(data_temp_age9)
head(data_temp_age9)

data_temp_lodes_age9 <- to_lodes_form(data_temp_age9, axes = 1:3, id = "traj")
data_temp_lodes_age9 <- data_temp_lodes_age9 %>%
  rename(time = x, Response = stratum)
head(data_temp_lodes_age9)
summary(data_temp_lodes_age9)

lengthFaith_age9 <- ggplot(data_temp_lodes_age9,
                           aes(x = time, stratum = Response, alluvium = traj,
                               y = freq,
                               fill = Response, label = Response)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "none") +
  xlab("Questionnaire time-point") + ylab("Frequency") +
  ggtitle("Length of time had said faith - Mothers") + theme(plot.title = element_text(hjust = 0.5))

lengthFaith_age9

# Save this plot
pdf("./Results/lengthFaith_Age9_Mothers.pdf", height = 6, width = 10)
plot(lengthFaith_age9)
dev.off()


## Displaying these results as standard cross-tabs (using only complete cases, so data is identical to that in the sankey plot)

# preg by age 5
data %>%
  select(d815, k6246, p4047) %>%
  rename(preg = d815, age5 = k6246, age9 = p4047) %>%
  filter(complete.cases(preg, age5, age9)) %>%
  group_by(preg, age5) %>%
  summarise(freq = n())

# Age 5 by age 9
data %>%
  select(d815, k6246, p4047) %>%
  rename(preg = d815, age5 = k6246, age9 = p4047) %>%
  filter(complete.cases(preg, age5, age9)) %>%
  group_by(age5, age9) %>%
  summarise(freq = n())

# Proportions at each time-point
tab_age9 <-data %>%
  select(d815, k6246, p4047) %>%
  rename(preg = d815, age5 = k6246, age9 = p4047) %>%
  filter(complete.cases(preg, age5, age9))
summary(tab_age9)


### Now repeat, but including 2019 RSBB data
data_temp_age28 <- data %>%
  select(d815, k6246, p4047, Y3050) %>%
  rename(preg = d815, age5 = k6246, age9 = p4047, age28 = Y3050) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(preg, age5, age9, age28) %>%
  summarise(freq = n())

summary(data_temp_age28)
head(data_temp_age28)

data_temp_lodes_age28 <- to_lodes_form(data_temp_age28, axes = 1:4, id = "traj")
data_temp_lodes_age28 <- data_temp_lodes_age28 %>%
  rename(time = x, Response = stratum)
head(data_temp_lodes_age28)
summary(data_temp_lodes_age28)

lengthFaith_age28 <- ggplot(data_temp_lodes_age28,
                            aes(x = time, stratum = Response, alluvium = traj,
                                y = freq,
                                fill = Response, label = Response)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "none") +
  xlab("Questionnaire time-point") + ylab("Frequency") +
  ggtitle("Length of time had said faith - Mothers") + theme(plot.title = element_text(hjust = 0.5))

lengthFaith_age28

# Save this plot
pdf("./Results/lengthFaith_Age28_Mothers.pdf", height = 6, width = 10)
plot(lengthFaith_age28)
dev.off()


## Displaying these results as standard cross-tabs (using only complete cases, so data is identical to that in the sankey plot)

# preg by age 5
data %>%
  select(d815, k6246, p4047, Y3050) %>%
  rename(preg = d815, age5 = k6246, age9 = p4047, age28 = Y3050) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(preg, age5) %>%
  summarise(freq = n())

# Age 5 by age 9
data %>%
  select(d815, k6246, p4047, Y3050) %>%
  rename(preg = d815, age5 = k6246, age9 = p4047, age28 = Y3050) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(age5, age9) %>%
  summarise(freq = n())

# Age 9 by age 28
data %>%
  select(d815, k6246, p4047, Y3050) %>%
  rename(preg = d815, age5 = k6246, age9 = p4047, age28 = Y3050) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(age9, age28) %>%
  summarise(freq = n())

# Proportions at each time-point
tab_age28 <-data %>%
  select(d815, k6246, p4047, Y3050) %>%
  rename(preg = d815, age5 = k6246, age9 = p4047, age28 = Y3050) %>%
  filter(complete.cases(preg, age5, age9, age28))
summary(tab_age28)

## Quite a lot of change in this over time, suggests that perhaps people are quite inconsistent in how they answer this question (sometimes they say 'all life', next time say '>5 years'). Quite hard to interpret these findings.

# Sample sizes do decrease quite substantially from age 9 to age 28
sum(data_temp_age9$freq)
sum(data_temp_age28$freq)


### Now repeat for partners

## Pregnancy
table(data$pb154, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(pb154 = na_if(pb154, "-1")) %>%
  mutate(pb154 = recode(pb154, "<1 YR" = "<5 Years", "1-2 YRS" = "<5 Years", "3-5 YRS" = "<5 Years",
                       ">5 YRS" = ">5 Years", "All my life" = "All life")) %>%
  mutate(pb154 = factor(pb154, levels = c("All life", ">5 Years", "<5 Years")))

table(data$pb154, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data which is biased towards non-believers, but it's not clear how long they have been non-believers, so will leave as is.
table(data$pb150[is.na(data$pb154)])

## Age 5
table(data$ph6246, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(ph6246 = na_if(ph6246, "No response")) %>%
  mutate(ph6246 = na_if(ph6246, "Not completed")) %>%
  mutate(ph6246 = na_if(ph6246, "Triplet / quadruplet")) %>%
  mutate(ph6246 = recode(ph6246, "Less than a year" = "<5 Years", "1-2 Years" = "<5 Years", "3-5 Years" = "<5 Years",
                        "More than 5 years" = ">5 Years", "All my life" = "All life")) %>%
  mutate(ph6246 = factor(ph6246, levels = c("All life", ">5 Years", "<5 Years")))

table(data$ph6246, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data which is biased towards non-believers, but it's not clear how long they have been non-believers, so will leave as is..
table(data$ph6240[is.na(data$ph6246)])

## Age 6
table(data$pj7047, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(pj7047 = na_if(pj7047, "No response")) %>%
  mutate(pj7047 = na_if(pj7047, "Not completed")) %>%
  mutate(pj7047 = na_if(pj7047, "Triplet / quadruplet")) %>%
  mutate(pj7047 = recode(pj7047, "Less than a year" = "<5 Years", "1-2 Years" = "<5 Years", "3-5 Years" = "<5 Years",
                         "More than 5 years" = ">5 Years", "All their life" = "All life")) %>%
  mutate(pj7047 = factor(pj7047, levels = c("All life", ">5 Years", "<5 Years")))


table(data$pj7047, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data which is biased towards non-believers, but it's not clear how long they have been non-believers, so will leave as is.
table(data$pj7040[is.na(data$pj7047)])

## Age 9
table(data$pm4047, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(pm4047 = na_if(pm4047, "No response")) %>%
  mutate(pm4047 = na_if(pm4047, "Not completed")) %>%
  mutate(pm4047 = na_if(pm4047, "Triplet / quadruplet")) %>%
  mutate(pm4047 = recode(pm4047, "< 1 year" = "<5 Years", "1-2 years" = "<5 Years", "3-5 years" = "<5 Years",
                         "> 5 years" = ">5 Years", "All his life" = "All life")) %>%
  mutate(pm4047 = factor(pm4047, levels = c("All life", ">5 Years", "<5 Years")))

table(data$pm4047, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data which is biased towards non-believers, but it's not clear how long they have been non-believers, so will leave as is.
table(data$pm4040[is.na(data$pm4047)])

## 2019 (Age 28)
table(data$FC3050, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(FC3050 = na_if(FC3050, "Missing")) %>%
  mutate(FC3050 = na_if(FC3050, "Missed whole section C")) %>%
  mutate(FC3050 = na_if(FC3050, "Mother of trip/quad")) %>%
  mutate(FC3050 = na_if(FC3050, "Questionnaire not completed")) %>%
  mutate(FC3050 = na_if(FC3050, "Unresolvable")) %>%
  mutate(FC3050 = recode(FC3050, "1-2 years/Less than a year" = "<5 Years", "3-5 years" = "<5 Years",
                         "More than 5 years" = ">5 Years", "All of life" = "All life")) %>%
  mutate(FC3050 = factor(FC3050, levels = c("All life", ">5 Years", "<5 Years")))

table(data$FC3050, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data which is biased towards non-believers, but it's not clear how long they have been non-believers, so will leave as is.
table(data$FC3000[is.na(data$FC3050)])


## Turn this data into a sankey plot - Both for pregnancy to 2019, and just from pregnancy to age 9 (as less missing data)

# First, have to convert the data to the correct format and remove any missing values. Will first convert the data to summary wide format, then convert to long/lode format. Age age 5 and 6 are very close together, to get a better sense of change over time will drop the age 6 data.
data_temp_age9_p <- data %>%
  select(pb154, ph6246, pm4047) %>%
  rename(preg = pb154, age5 = ph6246, age9 = pm4047) %>%
  filter(complete.cases(preg, age5, age9)) %>%
  group_by(preg, age5, age9) %>%
  summarise(freq = n())

summary(data_temp_age9_p)
head(data_temp_age9_p)

data_temp_lodes_age9_p <- to_lodes_form(data_temp_age9_p, axes = 1:3, id = "traj")
data_temp_lodes_age9_p <- data_temp_lodes_age9_p %>%
  rename(time = x, Response = stratum)
head(data_temp_lodes_age9_p)
summary(data_temp_lodes_age9_p)

lengthFaith_age9_p <- ggplot(data_temp_lodes_age9_p,
                             aes(x = time, stratum = Response, alluvium = traj,
                                 y = freq,
                                 fill = Response, label = Response)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "none") +
  xlab("Questionnaire time-point") + ylab("Frequency") +
  ggtitle("Length of time had said faith - Partners") + theme(plot.title = element_text(hjust = 0.5))

lengthFaith_age9_p

# Save this plot
pdf("./Results/lengthFaith_Age9_Partners.pdf", height = 6, width = 10)
plot(lengthFaith_age9_p)
dev.off()


## Displaying these results as standard cross-tabs (using only complete cases, so data is identical to that in the sankey plot)

# preg by age 5
data %>%
  select(pb154, ph6246, pm4047) %>%
  rename(preg = pb154, age5 = ph6246, age9 = pm4047) %>%
  filter(complete.cases(preg, age5, age9)) %>%
  group_by(preg, age5) %>%
  summarise(freq = n())

# Age 5 by age 9
data %>%
  select(pb154, ph6246, pm4047) %>%
  rename(preg = pb154, age5 = ph6246, age9 = pm4047) %>%
  filter(complete.cases(preg, age5, age9)) %>%
  group_by(age5, age9) %>%
  summarise(freq = n())

# Proportions at each time-point
tab_age9_p <-data %>%
  select(pb154, ph6246, pm4047) %>%
  rename(preg = pb154, age5 = ph6246, age9 = pm4047) %>%
  filter(complete.cases(preg, age5, age9))
summary(tab_age9_p)


### Now repeat, but including 2019 RSBB data
data_temp_age28_p <- data %>%
  select(pb154, ph6246, pm4047, FC3050) %>%
  rename(preg = pb154, age5 = ph6246, age9 = pm4047, age28 = FC3050) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(preg, age5, age9, age28) %>%
  summarise(freq = n())

summary(data_temp_age28_p)
head(data_temp_age28_p)

data_temp_lodes_age28_p <- to_lodes_form(data_temp_age28_p, axes = 1:4, id = "traj")
data_temp_lodes_age28_p <- data_temp_lodes_age28_p %>%
  rename(time = x, Response = stratum)
head(data_temp_lodes_age28_p)
summary(data_temp_lodes_age28_p)

lengthFaith_age28_p <- ggplot(data_temp_lodes_age28_p,
                             aes(x = time, stratum = Response, alluvium = traj,
                                 y = freq,
                                 fill = Response, label = Response)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "none") +
  xlab("Questionnaire time-point") + ylab("Frequency") +
  ggtitle("Length of time had said faith - Partners") + theme(plot.title = element_text(hjust = 0.5))

lengthFaith_age28_p

# Save this plot
pdf("./Results/lengthFaith_Age28_Partners.pdf", height = 6, width = 10)
plot(lengthFaith_age28_p)
dev.off()

## Displaying these results as standard cross-tabs (using only complete cases, so data is identical to that in the sankey plot)

# preg by age 5
data %>%
  select(pb154, ph6246, pm4047, FC3050) %>%
  rename(preg = pb154, age5 = ph6246, age9 = pm4047, age28 = FC3050) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(preg, age5) %>%
  summarise(freq = n())

# Age 5 by age 9
data %>%
  select(pb154, ph6246, pm4047, FC3050) %>%
  rename(preg = pb154, age5 = ph6246, age9 = pm4047, age28 = FC3050) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(age5, age9) %>%
  summarise(freq = n())

# Age 9 by age 28
data %>%
  select(pb154, ph6246, pm4047, FC3050) %>%
  rename(preg = pb154, age5 = ph6246, age9 = pm4047, age28 = FC3050) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(age9, age28) %>%
  summarise(freq = n())

# Proportions at each time-point
tab_age28_p <-data %>%
  select(pb154, ph6246, pm4047, FC3050) %>%
  rename(preg = pb154, age5 = ph6246, age9 = pm4047, age28 = FC3050) %>%
  filter(complete.cases(preg, age5, age9, age28))
summary(tab_age28_p)

## Very similar pattern of results to the mothers: Quite a bit of change over time (some of which don't make much sense; e.g., from >5 years to all life...)

# Again, sample sizes do decrease quite substantially from age 9 to age 28
sum(data_temp_age9_p$freq)
sum(data_temp_age28_p$freq)



#####################################################################################################
#### Bringing child up this faith - Responses: Yes vs No (only asked at age 6, age 9 and age 28)

### Mothers

## Age 6
table(data$l7048, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(l7048 = na_if(l7048, "No response")) %>%
  mutate(l7048 = na_if(l7048, "Not completed")) %>%
  mutate(l7048 = na_if(l7048, "Triplet / quadruplet")) %>%
  mutate(l7048 = na_if(l7048, "Don't know")) %>%
  mutate(l7048 = na_if(l7048, "Text response")) %>%
  mutate(l7048 = factor(l7048, levels = c("Yes", "No")))

table(data$l7048, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is quite a bit of missing data, especially for non-believers, but as haven't updated any other data, will leave as is.
table(data$l7040[is.na(data$l7048)])

## Age 9
table(data$p4048, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(p4048 = na_if(p4048, "No response")) %>%
  mutate(p4048 = na_if(p4048, "Not completed")) %>%
  mutate(p4048 = na_if(p4048, "Triplet / quadruplet")) %>%
  mutate(p4048 = na_if(p4048, "Text response")) %>%
  mutate(p4048 = factor(p4048, levels = c("Yes", "No")))

table(data$p4048, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is quite a bit of missing data, especially for non-believers, but as haven't updated any other data, will leave as is.
table(data$p4040[is.na(data$p4048)])

## 2019 (Age 28 - This questionnaire asked the question in past tense "Did you bring your children up in your current faith/belief (including none)?")
table(data$Y3070, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(Y3070 = na_if(Y3070, "Missing")) %>%
  mutate(Y3070 = na_if(Y3070, "Missed whole section C")) %>%
  mutate(Y3070 = na_if(Y3070, "Mother of trip/quad")) %>%
  mutate(Y3070 = na_if(Y3070, "Questionnaire not completed")) %>%
  mutate(Y3070 = na_if(Y3070, "Unresolvable")) %>%
  mutate(Y3070 = na_if(Y3070, "Not sure")) %>%
  mutate(Y3070 = factor(Y3070, levels = c("Yes", "No")))

table(data$Y3070, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is quite a bit of missing data, especially for non-believers, but as haven't updated any other data, will leave as is.
table(data$Y3000[is.na(data$Y3070)])


## Turn this data into a sankey plot - As only started from age 6, will just do from age 6 to 2019
data_temp_age28 <- data %>%
  select(l7048, p4048, Y3070) %>%
  rename(age6 = l7048, age9 = p4048, age28 = Y3070) %>%
  filter(complete.cases(age6, age9, age28)) %>%
  group_by(age6, age9, age28) %>%
  summarise(freq = n())

summary(data_temp_age28)
head(data_temp_age28)

data_temp_lodes_age28 <- to_lodes_form(data_temp_age28, axes = 1:3, id = "traj")
data_temp_lodes_age28 <- data_temp_lodes_age28 %>%
  rename(time = x, Response = stratum)
head(data_temp_lodes_age28)
summary(data_temp_lodes_age28)

raiseChild_age28 <- ggplot(data_temp_lodes_age28,
                     aes(x = time, stratum = Response, alluvium = traj,
                         y = freq,
                         fill = Response, label = Response)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "none") +
  xlab("Questionnaire time-point") + ylab("Frequency") +
  ggtitle("Raised child in current faith - Mothers") + theme(plot.title = element_text(hjust = 0.5))

raiseChild_age28

# Save this plot
pdf("./Results/raiseChild_Age28_Mothers.pdf", height = 6, width = 10)
plot(raiseChild_age28)
dev.off()

## Displaying these results as standard cross-tabs (using only complete cases, so data is identical to that in the sankey plot)

# Age 6 by age 9
data %>%
  select(l7048, p4048, Y3070) %>%
  rename(age6 = l7048, age9 = p4048, age28 = Y3070) %>%
  filter(complete.cases(age6, age9, age28)) %>%
  group_by(age6, age9) %>%
  summarise(freq = n())

# Age 9 by age 28
data %>%
  select(l7048, p4048, Y3070) %>%
  rename(age6 = l7048, age9 = p4048, age28 = Y3070) %>%
  filter(complete.cases(age6, age9, age28)) %>%
  group_by(age9, age28) %>%
  summarise(freq = n())

# Proportions at each time-point
tab_age28 <-data %>%
  select(l7048, p4048, Y3070) %>%
  rename(age6 = l7048, age9 = p4048, age28 = Y3070) %>%
  filter(complete.cases(age6, age9, age28))
summary(tab_age28)

## 'No' increases from age 9 to age 28.
sum(data_temp_age28$freq)


### Now repeat for partners

## Age 6
table(data$pj7048, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(pj7048 = na_if(pj7048, "No response")) %>%
  mutate(pj7048 = na_if(pj7048, "Not completed")) %>%
  mutate(pj7048 = na_if(pj7048, "Triplet / quadruplet")) %>%
  mutate(pj7048 = na_if(pj7048, "Don't know")) %>%
  mutate(pj7048 = na_if(pj7048, "Text response")) %>%
  mutate(pj7048 = factor(pj7048, levels = c("Yes", "No")))

table(data$pj7048, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is quite a bit of missing data, especially for non-believers, but as haven't updated any other data, will leave as is.
table(data$pj7040[is.na(data$pj7048)])

## Age 9
table(data$pm4048, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(pm4048 = na_if(pm4048, "No response")) %>%
  mutate(pm4048 = na_if(pm4048, "Not completed")) %>%
  mutate(pm4048 = na_if(pm4048, "Triplet / quadruplet")) %>%
  mutate(pm4048 = factor(pm4048, levels = c("Yes", "No")))

table(data$pm4048, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is quite a bit of missing data, especially for non-believers, but as haven't updated any other data, will leave as is.
table(data$pm4040[is.na(data$pm4048)])

## 2019 (Age 28 - This questionnaire asked the question in past tense "Did you bring your children up in your current faith/belief (including none)?")
table(data$FC3070, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(FC3070 = na_if(FC3070, "Missing")) %>%
  mutate(FC3070 = na_if(FC3070, "Missed whole section C")) %>%
  mutate(FC3070 = na_if(FC3070, "Questionnaire not completed")) %>%
  mutate(FC3070 = na_if(FC3070, "Unresolvable")) %>%
  mutate(FC3070 = na_if(FC3070, "Not sure")) %>%
  mutate(FC3070 = factor(FC3070, levels = c("Yes", "No")))

table(data$FC3070, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is quite a bit of missing data, especially for non-believers, but as haven't updated any other data, will leave as is.
table(data$FC3000[is.na(data$FC3070)])


## Turn this data into a sankey plot - As only started from age 6, will just do from age 6 to 2019
data_temp_age28_p <- data %>%
  select(pj7048, pm4048, FC3070) %>%
  rename(age6 = pj7048, age9 = pm4048, age28 = FC3070) %>%
  filter(complete.cases(age6, age9, age28)) %>%
  group_by(age6, age9, age28) %>%
  summarise(freq = n())

summary(data_temp_age28_p)
head(data_temp_age28_p)

data_temp_lodes_age28_p <- to_lodes_form(data_temp_age28_p, axes = 1:3, id = "traj")
data_temp_lodes_age28_p <- data_temp_lodes_age28_p %>%
  rename(time = x, Response = stratum)
head(data_temp_lodes_age28_p)
summary(data_temp_lodes_age28_p)

raiseChild_age28_p <- ggplot(data_temp_lodes_age28_p,
                       aes(x = time, stratum = Response, alluvium = traj,
                           y = freq,
                           fill = Response, label = Response)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "none") +
  xlab("Questionnaire time-point") + ylab("Frequency") +
  ggtitle("Raised child in current faith - Partners") + theme(plot.title = element_text(hjust = 0.5))

raiseChild_age28_p

# Save this plot
pdf("./Results/raiseChild_Age28_Partners.pdf", height = 6, width = 10)
plot(raiseChild_age28_p)
dev.off()

## Displaying these results as standard cross-tabs (using only complete cases, so data is identical to that in the sankey plot)

# Age 6 by age 9
data %>%
  select(pj7048, pm4048, FC3070) %>%
  rename(age6 = pj7048, age9 = pm4048, age28 = FC3070) %>%
  filter(complete.cases(age6, age9, age28)) %>%
  group_by(age6, age9) %>%
  summarise(freq = n())

# Age 9 by age 28
data %>%
  select(pj7048, pm4048, FC3070) %>%
  rename(age6 = pj7048, age9 = pm4048, age28 = FC3070) %>%
  filter(complete.cases(age6, age9, age28)) %>%
  group_by(age9, age28) %>%
  summarise(freq = n())

# Proportions at each time-point
tab_age28_p <-data %>%
  select(pj7048, pm4048, FC3070) %>%
  rename(age6 = pj7048, age9 = pm4048, age28 = FC3070) %>%
  filter(complete.cases(age6, age9, age28))
summary(tab_age28_p)

## No real change over time in these patterns.
sum(data_temp_age28_p$freq)



#####################################################################################################
#### Frequency attend a place of worship - Responses: Regular attendance vs occasional/non-attendance

## NOTE: At all time-points this question had the following response options ‘Yes, at least once a week’, ‘Yes, at least once a month’, ‘Yes, at least once a year’ and ‘Not at all’. However, at age 5 an ‘Occasional worship’ category was added (although it was not chosen by many), at age 6 an ‘only for special occasions’ response was added (this was chosen by many), and in 2019 an ‘occasionally’ response was added (this was chosen by many). Given these differences it may not be possible to directly compare responses to this question over time using these categories; as such, we will combine these responses into ‘regular attendance’ (attend minimum once a week or once a month) or ‘occasional/non-attendance’ (attend minimum once a year, occasionally, only on special occasions or not at all). We will also describe this variable in some detail to assist researchers when using this data. 

### Mothers

## Pregnancy
table(data$d816, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(d816 = na_if(d816, "-1")) %>%
  mutate(d816_grp = recode(d816, "MIN 1 a MTH" = "Regular", "MIN 1 a WK" = "Regular", 
                           "MIN 1 a YR" = "Occasional/None", "Not at all" = "Occasional/None")) %>%
  mutate(d816_grp = factor(d816_grp, levels = c("Regular", "Occasional/None")))

table(data$d816_grp, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data which is somewhat biased towards non-believers, but as it's not super clear-cut will leave as is.
table(data$d810[is.na(data$d816_grp)])

## Age 5
table(data$k6247, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(k6247 = na_if(k6247, "No response")) %>%
  mutate(k6247 = na_if(k6247, "Not completed")) %>%
  mutate(k6247 = na_if(k6247, "Triplet / quadruplet")) %>%
  mutate(k6247 = na_if(k6247, "Don't know")) %>%
  mutate(k6247 = na_if(k6247, "Text response")) %>%
  mutate(k6247_grp = recode(k6247, "Yes, least once a month" = "Regular", "Yes, least once a week" = "Regular", 
                           "Yes, least once a year" = "Occasional/None", "No, not at all" = "Occasional/None",
                           "Occasional worship" = "Occasional/None")) %>%
  mutate(k6247_grp = factor(k6247_grp, levels = c("Regular", "Occasional/None")))

table(data$k6247_grp, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data which is biased towards non-believers, but as it's not super clear-cut (and didn't edit the pregnancy data) will leave as is.
table(data$k6240[is.na(data$k6247_grp)])

# See how the people who answered 'occasional worship' responded to the 'belief in God/divine power' question, and religious attendance from the pregnancy questionnaire
table(data$k6247, useNA = "ifany")
table(data$k6240[data$k6247 == "Occasional worship"])
table(data$d816[data$k6247 == "Occasional worship"])

## Age 6
table(data$l7049, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(l7049 = na_if(l7049, "No response")) %>%
  mutate(l7049 = na_if(l7049, "Not completed")) %>%
  mutate(l7049 = na_if(l7049, "Don't know")) %>%
  mutate(l7049 = na_if(l7049, "Triplet / quadruplet")) %>%
  mutate(l7049 = na_if(l7049, "Text response")) %>%
  mutate(l7049_grp = recode(l7049, "Yes, at least once a month" = "Regular", "Yes, at least once a week" = "Regular", 
                            "Yes, at least once a year" = "Occasional/None", "No, not at all" = "Occasional/None",
                            "Only for special occasions" = "Occasional/None")) %>%
  mutate(l7049_grp = factor(l7049_grp, levels = c("Regular", "Occasional/None")))

table(data$l7049_grp, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data which is somewhat biased towards non-believers, but as it's not super clear-cut will leave as is.
table(data$l7040[is.na(data$l7049_grp)])

# See how the people who answered 'only for special occasions' responded to the 'belief in God/divine power' question, and religious attendance from the previous questionnaires
table(data$l7049, useNA = "ifany")
table(data$l7040[data$l7049 == "Only for special occasions"])
table(data$d816[data$l7049 == "Only for special occasions"])
table(data$k6247[data$l7049 == "Only for special occasions"])

## Age 9
table(data$p4049, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(p4049 = na_if(p4049, "No response")) %>%
  mutate(p4049 = na_if(p4049, "Not completed")) %>%
  mutate(p4049 = na_if(p4049, "Triplet / quadruplet")) %>%
  mutate(p4049_grp = recode(p4049, "Yes, at least once a month" = "Regular", "Yes, at least once a week" = "Regular", 
                            "Yes, at least once a year" = "Occasional/None", "No, not at all" = "Occasional/None")) %>%
  mutate(p4049_grp = factor(p4049_grp, levels = c("Regular", "Occasional/None")))

table(data$p4049_grp, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data which is somewhat biased towards non-believers, but as it's not super clear-cut will leave as is.
table(data$p4040[is.na(data$p4049_grp)])

## 2019 (Age 28)
table(data$Y3080, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(Y3080 = na_if(Y3080, "Missing")) %>%
  mutate(Y3080 = na_if(Y3080, "Missed whole section C")) %>%
  mutate(Y3080 = na_if(Y3080, "Mother of trip/quad")) %>%
  mutate(Y3080 = na_if(Y3080, "Questionnaire not completed")) %>%
  mutate(Y3080 = na_if(Y3080, "Unresolvable")) %>%
  mutate(Y3080_grp = recode(Y3080, "At least once a month" = "Regular", "At least once a week" = "Regular", 
                            "At least once a year" = "Occasional/None", "Not at all" = "Occasional/None",
                            "Occasionally" = "Occasional/None")) %>%
  mutate(Y3080_grp = factor(Y3080_grp, levels = c("Regular", "Occasional/None")))

table(data$Y3080_grp, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data which is somewhat biased towards non-believers, but as it's not super clear-cut will leave as is.
table(data$Y3000[is.na(data$Y3080_grp)])

# See how the people who answered 'Occasionally' responded to the 'belief in God/divine power' question, and religious attendance from the previous questionnaires
table(data$Y3080, useNA = "ifany")
table(data$Y3000[data$Y3080 == "Occasionally"])
table(data$d816[data$Y3080 == "Occasionally"])
table(data$k6247[data$Y3080 == "Occasionally"])
table(data$l7049[data$Y3080 == "Occasionally"])
table(data$p4049[data$Y3080 == "Occasionally"])


## Turn this data into a sankey plot - Both for pregnancy to 2019, and just from pregnancy to age 9 (as less missing data)

# First, have to convert the data to the correct format and remove any missing values. Will first convert the data to summary wide format, then convert to long/lode format. Age age 5 and 6 are very close together, to get a better sense of change over time will drop the age 6 data.
data_temp_age9 <- data %>%
  select(d816_grp, k6247_grp, p4049_grp) %>%
  rename(preg = d816_grp, age5 = k6247_grp, age9 = p4049_grp) %>%
  filter(complete.cases(preg, age5, age9)) %>%
  group_by(preg, age5, age9) %>%
  summarise(freq = n())

summary(data_temp_age9)
head(data_temp_age9)

data_temp_lodes_age9 <- to_lodes_form(data_temp_age9, axes = 1:3, id = "traj")
data_temp_lodes_age9 <- data_temp_lodes_age9 %>%
  rename(time = x, Response = stratum)
head(data_temp_lodes_age9)
summary(data_temp_lodes_age9)

attend_age9 <- ggplot(data_temp_lodes_age9,
                           aes(x = time, stratum = Response, alluvium = traj,
                               y = freq,
                               fill = Response, label = Response)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "none") +
  xlab("Questionnaire time-point") + ylab("Frequency") +
  ggtitle("Frequency attend place of worship - Mothers") + theme(plot.title = element_text(hjust = 0.5))

attend_age9

# Save this plot
pdf("./Results/attend_Age9_Mothers.pdf", height = 6, width = 10)
plot(attend_age9)
dev.off()


## Displaying these results as standard cross-tabs (using only complete cases, so data is identical to that in the sankey plot)

# preg by age 5
data %>%
  select(d816_grp, k6247_grp, p4049_grp) %>%
  rename(preg = d816_grp, age5 = k6247_grp, age9 = p4049_grp) %>%
  filter(complete.cases(preg, age5, age9)) %>%
  group_by(preg, age5) %>%
  summarise(freq = n())

# Age 5 by age 9
data %>%
  select(d816_grp, k6247_grp, p4049_grp) %>%
  rename(preg = d816_grp, age5 = k6247_grp, age9 = p4049_grp) %>%
  filter(complete.cases(preg, age5, age9)) %>%
  group_by(age5, age9) %>%
  summarise(freq = n())

# Proportions at each time-point
tab_age9 <-data %>%
  select(d816_grp, k6247_grp, p4049_grp) %>%
  rename(preg = d816_grp, age5 = k6247_grp, age9 = p4049_grp) %>%
  filter(complete.cases(preg, age5, age9))
summary(tab_age9)


### Now repeat, but including 2019 RSBB data
data_temp_age28 <- data %>%
  select(d816_grp, k6247_grp, p4049_grp, Y3080_grp) %>%
  rename(preg = d816_grp, age5 = k6247_grp, age9 = p4049_grp, age28 = Y3080_grp) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(preg, age5, age9, age28) %>%
  summarise(freq = n())

summary(data_temp_age28)
head(data_temp_age28)

data_temp_lodes_age28 <- to_lodes_form(data_temp_age28, axes = 1:4, id = "traj")
data_temp_lodes_age28 <- data_temp_lodes_age28 %>%
  rename(time = x, Response = stratum)
head(data_temp_lodes_age28)
summary(data_temp_lodes_age28)

attend_age28 <- ggplot(data_temp_lodes_age28,
                            aes(x = time, stratum = Response, alluvium = traj,
                                y = freq,
                                fill = Response, label = Response)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 2) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "none") +
  xlab("Questionnaire time-point") + ylab("Frequency") +
  ggtitle("Frequency attend place of worship - Mothers") + theme(plot.title = element_text(hjust = 0.5))

attend_age28

# Save this plot
pdf("./Results/attend_Age28_Mothers.pdf", height = 6, width = 10)
plot(attend_age28)
dev.off()


## Displaying these results as standard cross-tabs (using only complete cases, so data is identical to that in the sankey plot)

# preg by age 5
data %>%
  select(d816_grp, k6247_grp, p4049_grp, Y3080_grp) %>%
  rename(preg = d816_grp, age5 = k6247_grp, age9 = p4049_grp, age28 = Y3080_grp) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(preg, age5) %>%
  summarise(freq = n())

# Age 5 by age 9
data %>%
  select(d816_grp, k6247_grp, p4049_grp, Y3080_grp) %>%
  rename(preg = d816_grp, age5 = k6247_grp, age9 = p4049_grp, age28 = Y3080_grp) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(age5, age9) %>%
  summarise(freq = n())

# Age 9 by age 28
data %>%
  select(d816_grp, k6247_grp, p4049_grp, Y3080_grp) %>%
  rename(preg = d816_grp, age5 = k6247_grp, age9 = p4049_grp, age28 = Y3080_grp) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(age9, age28) %>%
  summarise(freq = n())

# Proportions at each time-point
tab_age28 <-data %>%
  select(d816_grp, k6247_grp, p4049_grp, Y3080_grp) %>%
  rename(preg = d816_grp, age5 = k6247_grp, age9 = p4049_grp, age28 = Y3080_grp) %>%
  filter(complete.cases(preg, age5, age9, age28))
summary(tab_age28)

## Interesting patterns. Increase in regular attendance at ages 5 and 9, relative to pregnancy, and then sharp decline to age 28.

# Sample sizes do decrease quite substantially from age 9 to age 28
sum(data_temp_age9$freq)
sum(data_temp_age28$freq)


### Now repeat for partners

## Pregnancy
table(data$pb155, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(pb155 = na_if(pb155, "-1")) %>%
  mutate(pb155_grp = recode(pb155, "MIN 1 PMTH" = "Regular", "MIN 1 PWK" = "Regular", 
                           "MIN 1 PYR" = "Occasional/None", "Not at all" = "Occasional/None")) %>%
  mutate(pb155_grp = factor(pb155_grp, levels = c("Regular", "Occasional/None")))

table(data$pb155_grp, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data which is somewhat biased towards non-believers, but as it's not super clear-cut will leave as is.
table(data$pb150[is.na(data$pb155_grp)])

## Age 5
table(data$ph6247, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(ph6247 = na_if(ph6247, "No response")) %>%
  mutate(ph6247 = na_if(ph6247, "Not completed")) %>%
  mutate(ph6247 = na_if(ph6247, "Triplet / quadruplet")) %>%
  mutate(ph6247_grp = recode(ph6247, "Yes, at least once a month" = "Regular", "Yes at least once a week" = "Regular", 
                            "Yes, at least once a year" = "Occasional/None", "No, not at all" = "Occasional/None",
                            "Occasional worship" = "Occasional/None")) %>%
  mutate(ph6247_grp = factor(ph6247_grp, levels = c("Regular", "Occasional/None")))

table(data$ph6247_grp, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data which is biased towards non-believers, but as it's not super clear-cut (and didn't edit the pregnancy data) will leave as is.
table(data$ph6240[is.na(data$ph6247_grp)])

# See how the people who answered 'occasional worship' responded to the 'belief in God/divine power' question, and religious attendance from the pregnancy questionnaire
table(data$ph6247, useNA = "ifany")
table(data$ph6240[data$ph6247 == "Occasional worship"])
table(data$pb155[data$ph6247 == "Occasional worship"])

## Age 6
table(data$pj7049, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(pj7049 = na_if(pj7049, "No response")) %>%
  mutate(pj7049 = na_if(pj7049, "Not completed")) %>%
  mutate(pj7049 = na_if(pj7049, "Triplet / quadruplet")) %>%
  mutate(pj7049_grp = recode(pj7049, "Yes, at least once a month" = "Regular", 
                             "Yes, at least once a week" = "Regular", 
                            "Yes, at least once a year" = "Occasional/None", "No, not at all" = "Occasional/None",
                            "Only for special occasions" = "Occasional/None")) %>%
  mutate(pj7049_grp = factor(pj7049_grp, levels = c("Regular", "Occasional/None")))


table(data$pj7049_grp, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data which is somewhat biased towards non-believers, but as it's not super clear-cut will leave as is.
table(data$pj7040[is.na(data$pj7049_grp)])

# See how the people who answered 'only for special occasions' responded to the 'belief in God/divine power' question, and religious attendance from the previous questionnaires
table(data$pj7049, useNA = "ifany")
table(data$pj7040[data$pj7049 == "Only for special occasions"])
table(data$pb155[data$pj7049 == "Only for special occasions"])
table(data$ph6247[data$pj7049 == "Only for special occasions"])

## Age 9
table(data$pm4049, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(pm4049 = na_if(pm4049, "No response")) %>%
  mutate(pm4049 = na_if(pm4049, "Not completed")) %>%
  mutate(pm4049 = na_if(pm4049, "Triplet / quadruplet")) %>%
  mutate(pm4049_grp = recode(pm4049, "Yes, at least once a month" = "Regular", 
                             "Yes, at least once a week" = "Regular", 
                            "Yes, at least once a year" = "Occasional/None", "No, not at all" = "Occasional/None")) %>%
  mutate(pm4049_grp = factor(pm4049_grp, levels = c("Regular", "Occasional/None")))

table(data$pm4049_grp, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data which is somewhat biased towards non-believers, but as it's not super clear-cut will leave as is.
table(data$pm4040[is.na(data$pm4049_grp)])

## 2019 (Age 28)
table(data$FC3080, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(FC3080 = na_if(FC3080, "Missing")) %>%
  mutate(FC3080 = na_if(FC3080, "Missed whole section C")) %>%
  mutate(FC3080 = na_if(FC3080, "Mother of trip/quad")) %>%
  mutate(FC3080 = na_if(FC3080, "Questionnaire not completed")) %>%
  mutate(FC3080 = na_if(FC3080, "Unresolvable")) %>%
  mutate(FC3080_grp = recode(FC3080, "At least once a month" = "Regular", "At least once a week" = "Regular", 
                            "At least once a year" = "Occasional/None", "Not at all" = "Occasional/None",
                            "Occasionally" = "Occasional/None")) %>%
  mutate(FC3080_grp = factor(FC3080_grp, levels = c("Regular", "Occasional/None")))

table(data$FC3080_grp, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data which is somewhat biased towards non-believers, but as it's not super clear-cut will leave as is.
table(data$FC3000[is.na(data$FC3080_grp)])

# See how the people who answered 'Occasionally' responded to the 'belief in God/divine power' question, and religious attendance from the previous questionnaires
table(data$FC3080, useNA = "ifany")
table(data$FC3000[data$FC3080 == "Occasionally"])
table(data$pb155[data$FC3080 == "Occasionally"])
table(data$ph6247[data$FC3080 == "Occasionally"])
table(data$pj7049[data$FC3080 == "Occasionally"])
table(data$pm4049[data$FC3080 == "Occasionally"])


## Turn this data into a sankey plot - Both for pregnancy to 2019, and just from pregnancy to age 9 (as less missing data)

# First, have to convert the data to the correct format and remove any missing values. Will first convert the data to summary wide format, then convert to long/lode format. Age age 5 and 6 are very close together, to get a better sense of change over time will drop the age 6 data.
data_temp_age9_p <- data %>%
  select(pb155_grp, ph6247_grp, pm4049_grp) %>%
  rename(preg = pb155_grp, age5 = ph6247_grp, age9 = pm4049_grp) %>%
  filter(complete.cases(preg, age5, age9)) %>%
  group_by(preg, age5, age9) %>%
  summarise(freq = n())

summary(data_temp_age9_p)
head(data_temp_age9_p)

data_temp_lodes_age9_p <- to_lodes_form(data_temp_age9_p, axes = 1:3, id = "traj")
data_temp_lodes_age9_p <- data_temp_lodes_age9_p %>%
  rename(time = x, Response = stratum)
head(data_temp_lodes_age9_p)
summary(data_temp_lodes_age9_p)

attend_age9_p <- ggplot(data_temp_lodes_age9_p,
                             aes(x = time, stratum = Response, alluvium = traj,
                                 y = freq,
                                 fill = Response, label = Response)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "none") +
  xlab("Questionnaire time-point") + ylab("Frequency") +
  ggtitle("Frequency attend place of worship - Partners") + theme(plot.title = element_text(hjust = 0.5))

attend_age9_p

# Save this plot
pdf("./Results/attend_Age9_Partners.pdf", height = 6, width = 10)
plot(attend_age9_p)
dev.off()


## Displaying these results as standard cross-tabs (using only complete cases, so data is identical to that in the sankey plot)

# preg by age 5
data %>%
  select(pb155_grp, ph6247_grp, pm4049_grp) %>%
  rename(preg = pb155_grp, age5 = ph6247_grp, age9 = pm4049_grp) %>%
  filter(complete.cases(preg, age5, age9)) %>%
  group_by(preg, age5) %>%
  summarise(freq = n())

# Age 5 by age 9
data %>%
  select(pb155_grp, ph6247_grp, pm4049_grp) %>%
  rename(preg = pb155_grp, age5 = ph6247_grp, age9 = pm4049_grp) %>%
  filter(complete.cases(preg, age5, age9)) %>%
  group_by(age5, age9) %>%
  summarise(freq = n())

# Proportions at each time-point
tab_age9_p <-data %>%
  select(pb155_grp, ph6247_grp, pm4049_grp) %>%
  rename(preg = pb155_grp, age5 = ph6247_grp, age9 = pm4049_grp) %>%
  filter(complete.cases(preg, age5, age9))
summary(tab_age9_p)


### Now repeat, but including 2019 RSBB data
data_temp_age28_p <- data %>%
  select(pb155_grp, ph6247_grp, pm4049_grp, FC3080_grp) %>%
  rename(preg = pb155_grp, age5 = ph6247_grp, age9 = pm4049_grp, age28 = FC3080_grp) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(preg, age5, age9, age28) %>%
  summarise(freq = n())

summary(data_temp_age28_p)
head(data_temp_age28_p)

data_temp_lodes_age28_p <- to_lodes_form(data_temp_age28_p, axes = 1:4, id = "traj")
data_temp_lodes_age28_p <- data_temp_lodes_age28_p %>%
  rename(time = x, Response = stratum)
head(data_temp_lodes_age28_p)
summary(data_temp_lodes_age28_p)

attend_age28_p <- ggplot(data_temp_lodes_age28_p,
                              aes(x = time, stratum = Response, alluvium = traj,
                                  y = freq,
                                  fill = Response, label = Response)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 2) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "none") +
  xlab("Questionnaire time-point") + ylab("Frequency") +
  ggtitle("Frequency attend place of worship - Partners") + theme(plot.title = element_text(hjust = 0.5))

attend_age28_p

# Save this plot
pdf("./Results/attend_Age28_Partners.pdf", height = 6, width = 10)
plot(attend_age28_p)
dev.off()

## Displaying these results as standard cross-tabs (using only complete cases, so data is identical to that in the sankey plot)

# preg by age 5
data %>%
  select(pb155_grp, ph6247_grp, pm4049_grp, FC3080_grp) %>%
  rename(preg = pb155_grp, age5 = ph6247_grp, age9 = pm4049_grp, age28 = FC3080_grp) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(preg, age5) %>%
  summarise(freq = n())

# Age 5 by age 9
data %>%
  select(pb155_grp, ph6247_grp, pm4049_grp, FC3080_grp) %>%
  rename(preg = pb155_grp, age5 = ph6247_grp, age9 = pm4049_grp, age28 = FC3080_grp) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(age5, age9) %>%
  summarise(freq = n())

# Age 9 by age 28
data %>%
  select(pb155_grp, ph6247_grp, pm4049_grp, FC3080_grp) %>%
  rename(preg = pb155_grp, age5 = ph6247_grp, age9 = pm4049_grp, age28 = FC3080_grp) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(age9, age28) %>%
  summarise(freq = n())

# Proportions at each time-point
tab_age28_p <-data %>%
  select(pb155_grp, ph6247_grp, pm4049_grp, FC3080_grp) %>%
  rename(preg = pb155_grp, age5 = ph6247_grp, age9 = pm4049_grp, age28 = FC3080_grp) %>%
  filter(complete.cases(preg, age5, age9, age28))
summary(tab_age28_p)

## Very similar pattern of results to the mothers: Slight increase between preg and age 5/9, then drop at at 28

# Again, sample sizes do decrease quite substantially from age 9 to age 28
sum(data_temp_age9_p$freq)
sum(data_temp_age28_p$freq)



#####################################################################################################
#### Obtain help/support from leaders of own religious group - Responses: Yes vs No

### Mothers

## Pregnancy
table(data$d817, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(d817 = na_if(d817, "-1")) %>%
  mutate(d817 = factor(d817, levels = c("Yes", "No")))

table(data$d817, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data, but not especially biased towards non-believers, so will leave as is.
table(data$d810[is.na(data$d817)])

## Age 5
table(data$k6248, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(k6248 = na_if(k6248, "No response")) %>%
  mutate(k6248 = na_if(k6248, "Not completed")) %>%
  mutate(k6248 = na_if(k6248, "Triplet / quadruplet")) %>%
  mutate(k6248 = na_if(k6248, "Don't know")) %>%
  mutate(k6248 = na_if(k6248, "Text response")) %>%
  mutate(k6248 = factor(k6248, levels = c("Yes", "No")))

table(data$k6248, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data, but not especially biased towards non-believers, so will leave as is.
table(data$k6240[is.na(data$k6248)])

## Age 6
table(data$l7050, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(l7050 = na_if(l7050, "No response")) %>%
  mutate(l7050 = na_if(l7050, "Not completed")) %>%
  mutate(l7050 = na_if(l7050, "Don't know")) %>%
  mutate(l7050 = na_if(l7050, "Triplet / quadruplet")) %>%
  mutate(l7050 = na_if(l7050, "Text response")) %>%
  mutate(l7050 = factor(l7050, levels = c("Yes", "No")))

table(data$l7050, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data, but not especially biased towards non-believers, so will leave as is.
table(data$l7040[is.na(data$l7050)])

## Age 9
table(data$p4050, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(p4050 = na_if(p4050, "No response")) %>%
  mutate(p4050 = na_if(p4050, "Not completed")) %>%
  mutate(p4050 = na_if(p4050, "Triplet / quadruplet")) %>%
  mutate(p4050 = factor(p4050, levels = c("Yes", "No")))

table(data$p4050, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data, but not especially biased towards non-believers, so will leave as is.
table(data$p4040[is.na(data$p4050)])

## 2019 (Age 28)
table(data$Y3090, useNA = "ifany")

# This questionnaire had a 'not applicable' response for this question (unlike previous waves) - Will see if sensible to combine with 'no' (if not believe in God/no religious affiliation), for consistency across time-points. Hmmmm...Even though most said 'no belief/no affiliation', are actually quite a few people who believe in God or have a religious affiliation who said 'N/A' to this question. Despite this, will still code these 'N/A's as 'no', is if they did obtain help/support then presumably they would have said 'yes'.
table(data$Y3000[data$Y3090 == "Not applicable"])
table(data$Y3040_grpXian[data$Y3090 == "Not applicable"])

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(Y3090 = na_if(Y3090, "Missing")) %>%
  mutate(Y3090 = na_if(Y3090, "Missed whole section C")) %>%
  mutate(Y3090 = na_if(Y3090, "Mother of trip/quad")) %>%
  mutate(Y3090 = na_if(Y3090, "Questionnaire not completed")) %>%
  mutate(Y3090 = na_if(Y3090, "Unresolvable")) %>%
  mutate(Y3090 = recode(Y3090, "Not applicable" = "No")) %>%
  mutate(Y3090 = factor(Y3090, levels = c("Yes", "No")))

table(data$Y3090, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data, but not especially biased towards non-believers, so will leave as is.
table(data$Y3000[is.na(data$Y3090)])


## Turn this data into a sankey plot - Both for pregnancy to 2019, and just from pregnancy to age 9 (as less missing data)

# First, have to convert the data to the correct format and remove any missing values. Will first convert the data to summary wide format, then convert to long/lode format. Age age 5 and 6 are very close together, to get a better sense of change over time will drop the age 6 data.
data_temp_age9 <- data %>%
  select(d817, k6248, p4050) %>%
  rename(preg = d817, age5 = k6248, age9 = p4050) %>%
  filter(complete.cases(preg, age5, age9)) %>%
  group_by(preg, age5, age9) %>%
  summarise(freq = n())

summary(data_temp_age9)
head(data_temp_age9)

data_temp_lodes_age9 <- to_lodes_form(data_temp_age9, axes = 1:3, id = "traj")
data_temp_lodes_age9 <- data_temp_lodes_age9 %>%
  rename(time = x, Response = stratum)
head(data_temp_lodes_age9)
summary(data_temp_lodes_age9)

supportReligLeaders_age9 <- ggplot(data_temp_lodes_age9,
                      aes(x = time, stratum = Response, alluvium = traj,
                          y = freq,
                          fill = Response, label = Response)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "none") +
  xlab("Questionnaire time-point") + ylab("Frequency") +
  ggtitle("Obtains help/support from leaders of own religious group - Mothers") + theme(plot.title = element_text(hjust = 0.5))

supportReligLeaders_age9

# Save this plot
pdf("./Results/supportReligLeaders_Age9_Mothers.pdf", height = 6, width = 10)
plot(supportReligLeaders_age9)
dev.off()


## Displaying these results as standard cross-tabs (using only complete cases, so data is identical to that in the sankey plot)

# preg by age 5
data %>%
  select(d817, k6248, p4050) %>%
  rename(preg = d817, age5 = k6248, age9 = p4050) %>%
  filter(complete.cases(preg, age5, age9)) %>%
  group_by(preg, age5) %>%
  summarise(freq = n())

# Age 5 by age 9
data %>%
  select(d817, k6248, p4050) %>%
  rename(preg = d817, age5 = k6248, age9 = p4050) %>%
  filter(complete.cases(preg, age5, age9)) %>%
  group_by(age5, age9) %>%
  summarise(freq = n())

# Proportions at each time-point
tab_age9 <-data %>%
  select(d817, k6248, p4050) %>%
  rename(preg = d817, age5 = k6248, age9 = p4050) %>%
  filter(complete.cases(preg, age5, age9))
summary(tab_age9)


### Now repeat, but including 2019 RSBB data
data_temp_age28 <- data %>%
  select(d817, k6248, p4050, Y3090) %>%
  rename(preg = d817, age5 = k6248, age9 = p4050, age28 = Y3090) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(preg, age5, age9, age28) %>%
  summarise(freq = n())

summary(data_temp_age28)
head(data_temp_age28)

data_temp_lodes_age28 <- to_lodes_form(data_temp_age28, axes = 1:4, id = "traj")
data_temp_lodes_age28 <- data_temp_lodes_age28 %>%
  rename(time = x, Response = stratum)
head(data_temp_lodes_age28)
summary(data_temp_lodes_age28)

supportReligLeaders_age28 <- ggplot(data_temp_lodes_age28,
                       aes(x = time, stratum = Response, alluvium = traj,
                           y = freq,
                           fill = Response, label = Response)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "none") +
  xlab("Questionnaire time-point") + ylab("Frequency") +
  ggtitle("Obtains help/support from leaders of own religious group - Mothers") + theme(plot.title = element_text(hjust = 0.5))

supportReligLeaders_age28

# Save this plot
pdf("./Results/supportReligLeaders_Age28_Mothers.pdf", height = 6, width = 10)
plot(supportReligLeaders_age28)
dev.off()


## Displaying these results as standard cross-tabs (using only complete cases, so data is identical to that in the sankey plot)

# preg by age 5
data %>%
  select(d817, k6248, p4050, Y3090) %>%
  rename(preg = d817, age5 = k6248, age9 = p4050, age28 = Y3090) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(preg, age5) %>%
  summarise(freq = n())

# Age 5 by age 9
data %>%
  select(d817, k6248, p4050, Y3090) %>%
  rename(preg = d817, age5 = k6248, age9 = p4050, age28 = Y3090) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(age5, age9) %>%
  summarise(freq = n())

# Age 9 by age 28
data %>%
  select(d817, k6248, p4050, Y3090) %>%
  rename(preg = d817, age5 = k6248, age9 = p4050, age28 = Y3090) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(age9, age28) %>%
  summarise(freq = n())

# Proportions at each time-point
tab_age28 <-data %>%
  select(d817, k6248, p4050, Y3090) %>%
  rename(preg = d817, age5 = k6248, age9 = p4050, age28 = Y3090) %>%
  filter(complete.cases(preg, age5, age9, age28))
summary(tab_age28)

## Stable-ish until age 9, then drops at age 28

# Sample sizes do decrease quite substantially from age 9 to age 28
sum(data_temp_age9$freq)
sum(data_temp_age28$freq)


### Now repeat for partners

## Pregnancy
table(data$pb156, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(pb156 = na_if(pb156, "-1")) %>%
  mutate(pb156 = recode(pb156, "Y" = "Yes", "N" = "No")) %>%
  mutate(pb156 = factor(pb156, levels = c("Yes", "No")))

table(data$pb156, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data, but not especially biased towards non-believers, so will leave as is.
table(data$pb150[is.na(data$pb156)])

## Age 5
table(data$ph6248, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(ph6248 = na_if(ph6248, "No response")) %>%
  mutate(ph6248 = na_if(ph6248, "Not completed")) %>%
  mutate(ph6248 = na_if(ph6248, "Triplet / quadruplet")) %>%
  mutate(ph6248 = factor(ph6248, levels = c("Yes", "No")))

table(data$ph6248, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data, but not especially biased towards non-believers, so will leave as is.
table(data$ph6240[is.na(data$ph6248)])

## Age 6
table(data$pj7050, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(pj7050 = na_if(pj7050, "No response")) %>%
  mutate(pj7050 = na_if(pj7050, "Not completed")) %>%
  mutate(pj7050 = na_if(pj7050, "Triplet / quadruplet")) %>%
  mutate(pj7050 = factor(pj7050, levels = c("Yes", "No")))

table(data$pj7050, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data, but not especially biased towards non-believers, so will leave as is.
table(data$pj7040[is.na(data$pj7050)])

## Age 9
table(data$pm4050, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(pm4050 = na_if(pm4050, "No response")) %>%
  mutate(pm4050 = na_if(pm4050, "Not completed")) %>%
  mutate(pm4050 = na_if(pm4050, "Triplet / quadruplet")) %>%
  mutate(pm4050 = factor(pm4050, levels = c("Yes", "No")))

table(data$pm4050, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data, but not especially biased towards non-believers, so will leave as is.
table(data$pm4040[is.na(data$pm4050)])

## 2019 (Age 28)
table(data$FC3090, useNA = "ifany")

# This questionnaire had a 'not applicable' response for this question (unlike previous waves) - Will see if sensible to combine with 'no' (if not believe in God/no religious affiliation), for consistency across time-points. Hmmmm...Even though most said 'no belief/no affiliation', are actually quite a few people who believe in God or have a religious affiliation who said 'N/A' to this question. Despite this, will still code these 'N/A's as 'no', is if they did obtain help/support then presumably they would have said 'yes'.
table(data$FC3000[data$FC3090 == "Not applicable"])
table(data$FC3040_grpXian[data$FC3090 == "Not applicable"])

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(FC3090 = na_if(FC3090, "Missing")) %>%
  mutate(FC3090 = na_if(FC3090, "Missed whole section C")) %>%
  mutate(FC3090 = na_if(FC3090, "Mother of trip/quad")) %>%
  mutate(FC3090 = na_if(FC3090, "Questionnaire not completed")) %>%
  mutate(FC3090 = na_if(FC3090, "Unresolvable")) %>%
  mutate(FC3090 = recode(FC3090, "Not applicable" = "No")) %>%
  mutate(FC3090 = factor(FC3090, levels = c("Yes", "No")))

table(data$FC3090, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data, but not especially biased towards non-believers, so will leave as is.
table(data$FC3000[is.na(data$FC3090)])


## Turn this data into a sankey plot - Both for pregnancy to 2019, and just from pregnancy to age 9 (as less missing data)

# First, have to convert the data to the correct format and remove any missing values. Will first convert the data to summary wide format, then convert to long/lode format. Age age 5 and 6 are very close together, to get a better sense of change over time will drop the age 6 data.
data_temp_age9_p <- data %>%
  select(pb156, ph6248, pm4050) %>%
  rename(preg = pb156, age5 = ph6248, age9 = pm4050) %>%
  filter(complete.cases(preg, age5, age9)) %>%
  group_by(preg, age5, age9) %>%
  summarise(freq = n())

summary(data_temp_age9_p)
head(data_temp_age9_p)

data_temp_lodes_age9_p <- to_lodes_form(data_temp_age9_p, axes = 1:3, id = "traj")
data_temp_lodes_age9_p <- data_temp_lodes_age9_p %>%
  rename(time = x, Response = stratum)
head(data_temp_lodes_age9_p)
summary(data_temp_lodes_age9_p)

supportReligLeaders_age9_p <- ggplot(data_temp_lodes_age9_p,
                        aes(x = time, stratum = Response, alluvium = traj,
                            y = freq,
                            fill = Response, label = Response)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "none") +
  xlab("Questionnaire time-point") + ylab("Frequency") +
  ggtitle("Obtains help/support from leaders of own religious group - Partners") + theme(plot.title = element_text(hjust = 0.5))

supportReligLeaders_age9_p

# Save this plot
pdf("./Results/supportReligLeaders_Age9_Partners.pdf", height = 6, width = 10)
plot(supportReligLeaders_age9_p)
dev.off()


## Displaying these results as standard cross-tabs (using only complete cases, so data is identical to that in the sankey plot)

# preg by age 5
data %>%
  select(pb156, ph6248, pm4050) %>%
  rename(preg = pb156, age5 = ph6248, age9 = pm4050) %>%
  filter(complete.cases(preg, age5, age9)) %>%
  group_by(preg, age5) %>%
  summarise(freq = n())

# Age 5 by age 9
data %>%
  select(pb156, ph6248, pm4050) %>%
  rename(preg = pb156, age5 = ph6248, age9 = pm4050) %>%
  filter(complete.cases(preg, age5, age9)) %>%
  group_by(age5, age9) %>%
  summarise(freq = n())

# Proportions at each time-point
tab_age9_p <-data %>%
  select(pb156, ph6248, pm4050) %>%
  rename(preg = pb156, age5 = ph6248, age9 = pm4050) %>%
  filter(complete.cases(preg, age5, age9))
summary(tab_age9_p)


### Now repeat, but including 2019 RSBB data
data_temp_age28_p <- data %>%
  select(pb156, ph6248, pm4050, FC3090) %>%
  rename(preg = pb156, age5 = ph6248, age9 = pm4050, age28 = FC3090) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(preg, age5, age9, age28) %>%
  summarise(freq = n())

summary(data_temp_age28_p)
head(data_temp_age28_p)

data_temp_lodes_age28_p <- to_lodes_form(data_temp_age28_p, axes = 1:4, id = "traj")
data_temp_lodes_age28_p <- data_temp_lodes_age28_p %>%
  rename(time = x, Response = stratum)
head(data_temp_lodes_age28_p)
summary(data_temp_lodes_age28_p)

supportReligLeaders_age28_p <- ggplot(data_temp_lodes_age28_p,
                         aes(x = time, stratum = Response, alluvium = traj,
                             y = freq,
                             fill = Response, label = Response)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "none") +
  xlab("Questionnaire time-point") + ylab("Frequency") +
  ggtitle("Obtains help/support from leaders of own religious group - Partners") + theme(plot.title = element_text(hjust = 0.5))

supportReligLeaders_age28_p

# Save this plot
pdf("./Results/supportReligLeaders_Age28_Partners.pdf", height = 6, width = 10)
plot(supportReligLeaders_age28_p)
dev.off()

## Displaying these results as standard cross-tabs (using only complete cases, so data is identical to that in the sankey plot)

# preg by age 5
data %>%
  select(pb156, ph6248, pm4050, FC3090) %>%
  rename(preg = pb156, age5 = ph6248, age9 = pm4050, age28 = FC3090) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(preg, age5) %>%
  summarise(freq = n())

# Age 5 by age 9
data %>%
  select(pb156, ph6248, pm4050, FC3090) %>%
  rename(preg = pb156, age5 = ph6248, age9 = pm4050, age28 = FC3090) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(age5, age9) %>%
  summarise(freq = n())

# Age 9 by age 28
data %>%
  select(pb156, ph6248, pm4050, FC3090) %>%
  rename(preg = pb156, age5 = ph6248, age9 = pm4050, age28 = FC3090) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(age9, age28) %>%
  summarise(freq = n())

# Proportions at each time-point
tab_age28_p <-data %>%
  select(pb156, ph6248, pm4050, FC3090) %>%
  rename(preg = pb156, age5 = ph6248, age9 = pm4050, age28 = FC3090) %>%
  filter(complete.cases(preg, age5, age9, age28))
summary(tab_age28_p)

## Very similar pattern of results to the mothers: Stable from preg to age 9, then slight drop to age 28

# Again, sample sizes do decrease quite substantially from age 9 to age 28
sum(data_temp_age9_p$freq)
sum(data_temp_age28_p$freq)



#####################################################################################################
#### Obtain help/support from other members of own religious group - Responses: Yes vs No

### Mothers

## Pregnancy
table(data$d818, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(d818 = na_if(d818, "-1")) %>%
  mutate(d818 = factor(d818, levels = c("Yes", "No")))

table(data$d818, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data, but not especially biased towards non-believers, so will leave as is.
table(data$d810[is.na(data$d818)])

## Age 5
table(data$k6249, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(k6249 = na_if(k6249, "No response")) %>%
  mutate(k6249 = na_if(k6249, "Not completed")) %>%
  mutate(k6249 = na_if(k6249, "Triplet / quadruplet")) %>%
  mutate(k6249 = na_if(k6249, "Don't know")) %>%
  mutate(k6249 = na_if(k6249, "Text response")) %>%
  mutate(k6249 = factor(k6249, levels = c("Yes", "No")))

table(data$k6249, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data, but not especially biased towards non-believers, so will leave as is.
table(data$k6240[is.na(data$k6249)])

## Age 6
table(data$l7051, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(l7051 = na_if(l7051, "No response")) %>%
  mutate(l7051 = na_if(l7051, "Not completed")) %>%
  mutate(l7051 = na_if(l7051, "Don't know")) %>%
  mutate(l7051 = na_if(l7051, "Triplet / quadruplet")) %>%
  mutate(l7051 = na_if(l7051, "Text response")) %>%
  mutate(l7051 = factor(l7051, levels = c("Yes", "No")))

table(data$l7051, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data, but not especially biased towards non-believers, so will leave as is.
table(data$l7040[is.na(data$l7051)])

## Age 9
table(data$p4051, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(p4051 = na_if(p4051, "No response")) %>%
  mutate(p4051 = na_if(p4051, "Not completed")) %>%
  mutate(p4051 = na_if(p4051, "Triplet / quadruplet")) %>%
  mutate(p4051 = factor(p4051, levels = c("Yes", "No")))

table(data$p4051, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data, but not especially biased towards non-believers, so will leave as is.
table(data$p4040[is.na(data$p4051)])

## 2019 (Age 28)
table(data$Y3091, useNA = "ifany")

# This questionnaire had a 'not applicable' response for this question (unlike previous waves) - Will see if sensible to combine with 'no' (if not believe in God/no religious affiliation), for consistency across time-points. Hmmmm...Even though most said 'no belief/no affiliation', are actually quite a few people who believe in God or have a religious affiliation who said 'N/A' to this question. Despite this, will still code these 'N/A's as 'no', is if they did obtain help/support then presumably they would have said 'yes'.
table(data$Y3000[data$Y3091 == "Not applicable"])
table(data$Y3040_grpXian[data$Y3091 == "Not applicable"])

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(Y3091 = na_if(Y3091, "Missing")) %>%
  mutate(Y3091 = na_if(Y3091, "Missed whole section C")) %>%
  mutate(Y3091 = na_if(Y3091, "Mother of trip/quad")) %>%
  mutate(Y3091 = na_if(Y3091, "Questionnaire not completed")) %>%
  mutate(Y3091 = na_if(Y3091, "Unresolvable")) %>%
  mutate(Y3091 = recode(Y3091, "Not applicable" = "No")) %>%
  mutate(Y3091 = factor(Y3091, levels = c("Yes", "No")))

table(data$Y3091, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data, but not especially biased towards non-believers, so will leave as is.
table(data$Y3000[is.na(data$Y3091)])


## Turn this data into a sankey plot - Both for pregnancy to 2019, and just from pregnancy to age 9 (as less missing data)

# First, have to convert the data to the correct format and remove any missing values. Will first convert the data to summary wide format, then convert to long/lode format. Age age 5 and 6 are very close together, to get a better sense of change over time will drop the age 6 data.
data_temp_age9 <- data %>%
  select(d818, k6249, p4051) %>%
  rename(preg = d818, age5 = k6249, age9 = p4051) %>%
  filter(complete.cases(preg, age5, age9)) %>%
  group_by(preg, age5, age9) %>%
  summarise(freq = n())

summary(data_temp_age9)
head(data_temp_age9)

data_temp_lodes_age9 <- to_lodes_form(data_temp_age9, axes = 1:3, id = "traj")
data_temp_lodes_age9 <- data_temp_lodes_age9 %>%
  rename(time = x, Response = stratum)
head(data_temp_lodes_age9)
summary(data_temp_lodes_age9)

supportReligMembers_age9 <- ggplot(data_temp_lodes_age9,
                                   aes(x = time, stratum = Response, alluvium = traj,
                                       y = freq,
                                       fill = Response, label = Response)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "none") +
  xlab("Questionnaire time-point") + ylab("Frequency") +
  ggtitle("Obtains help/support from members of own religious group - Mothers") + theme(plot.title = element_text(hjust = 0.5))

supportReligMembers_age9

# Save this plot
pdf("./Results/supportReligMembers_Age9_Mothers.pdf", height = 6, width = 10)
plot(supportReligMembers_age9)
dev.off()


## Displaying these results as standard cross-tabs (using only complete cases, so data is identical to that in the sankey plot)

# preg by age 5
data %>%
  select(d818, k6249, p4051) %>%
  rename(preg = d818, age5 = k6249, age9 = p4051) %>%
  filter(complete.cases(preg, age5, age9)) %>%
  group_by(preg, age5) %>%
  summarise(freq = n())

# Age 5 by age 9
data %>%
  select(d818, k6249, p4051) %>%
  rename(preg = d818, age5 = k6249, age9 = p4051) %>%
  filter(complete.cases(preg, age5, age9)) %>%
  group_by(age5, age9) %>%
  summarise(freq = n())

# Proportions at each time-point
tab_age9 <-data %>%
  select(d818, k6249, p4051) %>%
  rename(preg = d818, age5 = k6249, age9 = p4051) %>%
  filter(complete.cases(preg, age5, age9))
summary(tab_age9)


### Now repeat, but including 2019 RSBB data
data_temp_age28 <- data %>%
  select(d818, k6249, p4051, Y3091) %>%
  rename(preg = d818, age5 = k6249, age9 = p4051, age28 = Y3091) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(preg, age5, age9, age28) %>%
  summarise(freq = n())

summary(data_temp_age28)
head(data_temp_age28)

data_temp_lodes_age28 <- to_lodes_form(data_temp_age28, axes = 1:4, id = "traj")
data_temp_lodes_age28 <- data_temp_lodes_age28 %>%
  rename(time = x, Response = stratum)
head(data_temp_lodes_age28)
summary(data_temp_lodes_age28)

supportReligMembers_age28 <- ggplot(data_temp_lodes_age28,
                                    aes(x = time, stratum = Response, alluvium = traj,
                                        y = freq,
                                        fill = Response, label = Response)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "none") +
  xlab("Questionnaire time-point") + ylab("Frequency") +
  ggtitle("Obtains help/support from members of own religious group - Mothers") + theme(plot.title = element_text(hjust = 0.5))

supportReligMembers_age28

# Save this plot
pdf("./Results/supportReligMembers_Age28_Mothers.pdf", height = 6, width = 10)
plot(supportReligMembers_age28)
dev.off()


## Displaying these results as standard cross-tabs (using only complete cases, so data is identical to that in the sankey plot)

# preg by age 5
data %>%
  select(d818, k6249, p4051, Y3091) %>%
  rename(preg = d818, age5 = k6249, age9 = p4051, age28 = Y3091) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(preg, age5) %>%
  summarise(freq = n())

# Age 5 by age 9
data %>%
  select(d818, k6249, p4051, Y3091) %>%
  rename(preg = d818, age5 = k6249, age9 = p4051, age28 = Y3091) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(age5, age9) %>%
  summarise(freq = n())

# Age 9 by age 28
data %>%
  select(d818, k6249, p4051, Y3091) %>%
  rename(preg = d818, age5 = k6249, age9 = p4051, age28 = Y3091) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(age9, age28) %>%
  summarise(freq = n())

# Proportions at each time-point
tab_age28 <-data %>%
  select(d818, k6249, p4051, Y3091) %>%
  rename(preg = d818, age5 = k6249, age9 = p4051, age28 = Y3091) %>%
  filter(complete.cases(preg, age5, age9, age28))
summary(tab_age28)

## Stable-ish until age 9, then drops at age 28

# Sample sizes do decrease quite substantially from age 9 to age 28
sum(data_temp_age9$freq)
sum(data_temp_age28$freq)


### Now repeat for partners

## Pregnancy
table(data$pb157, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(pb157 = na_if(pb157, "-1")) %>%
  mutate(pb157 = recode(pb157, "Y" = "Yes", "N" = "No")) %>%
  mutate(pb157 = factor(pb157, levels = c("Yes", "No")))

table(data$pb157, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data, but not especially biased towards non-believers, so will leave as is.
table(data$pb150[is.na(data$pb157)])

## Age 5
table(data$ph6249, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(ph6249 = na_if(ph6249, "No response")) %>%
  mutate(ph6249 = na_if(ph6249, "Not completed")) %>%
  mutate(ph6249 = na_if(ph6249, "Triplet / quadruplet")) %>%
  mutate(ph6249 = factor(ph6249, levels = c("Yes", "No")))

table(data$ph6249, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data, but not especially biased towards non-believers, so will leave as is.
table(data$ph6240[is.na(data$ph6249)])

## Age 6
table(data$pj7051, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(pj7051 = na_if(pj7051, "No response")) %>%
  mutate(pj7051 = na_if(pj7051, "Not completed")) %>%
  mutate(pj7051 = na_if(pj7051, "Triplet / quadruplet")) %>%
  mutate(pj7051 = factor(pj7051, levels = c("Yes", "No")))

table(data$pj7051, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data, but not especially biased towards non-believers, so will leave as is.
table(data$pj7040[is.na(data$pj7051)])

## Age 9
table(data$pm4051, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(pm4051 = na_if(pm4051, "No response")) %>%
  mutate(pm4051 = na_if(pm4051, "Not completed")) %>%
  mutate(pm4051 = na_if(pm4051, "Triplet / quadruplet")) %>%
  mutate(pm4051 = factor(pm4051, levels = c("Yes", "No")))

table(data$pm4051, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data, but not especially biased towards non-believers, so will leave as is.
table(data$pm4040[is.na(data$pm4051)])

## 2019 (Age 28)
table(data$FC3091, useNA = "ifany")

# This questionnaire had a 'not applicable' response for this question (unlike previous waves) - Will see if sensible to combine with 'no' (if not believe in God/no religious affiliation), for consistency across time-points. Hmmmm...Even though most said 'no belief/no affiliation', are actually quite a few people who believe in God or have a religious affiliation who said 'N/A' to this question. Despite this, will still code these 'N/A's as 'no', is if they did obtain help/support then presumably they would have said 'yes'.
table(data$FC3000[data$FC3091 == "Not applicable"])
table(data$FC3040_grpXian[data$FC3091 == "Not applicable"])

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(FC3091 = na_if(FC3091, "Missing")) %>%
  mutate(FC3091 = na_if(FC3091, "Missed whole section C")) %>%
  mutate(FC3091 = na_if(FC3091, "Mother of trip/quad")) %>%
  mutate(FC3091 = na_if(FC3091, "Questionnaire not completed")) %>%
  mutate(FC3091 = na_if(FC3091, "Unresolvable")) %>%
  mutate(FC3091 = recode(FC3091, "Not applicable" = "No")) %>%
  mutate(FC3091 = factor(FC3091, levels = c("Yes", "No")))

table(data$FC3091, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data, but not especially biased towards non-believers, so will leave as is.
table(data$FC3000[is.na(data$FC3091)])


## Turn this data into a sankey plot - Both for pregnancy to 2019, and just from pregnancy to age 9 (as less missing data)

# First, have to convert the data to the correct format and remove any missing values. Will first convert the data to summary wide format, then convert to long/lode format. Age age 5 and 6 are very close together, to get a better sense of change over time will drop the age 6 data.
data_temp_age9_p <- data %>%
  select(pb157, ph6249, pm4051) %>%
  rename(preg = pb157, age5 = ph6249, age9 = pm4051) %>%
  filter(complete.cases(preg, age5, age9)) %>%
  group_by(preg, age5, age9) %>%
  summarise(freq = n())

summary(data_temp_age9_p)
head(data_temp_age9_p)

data_temp_lodes_age9_p <- to_lodes_form(data_temp_age9_p, axes = 1:3, id = "traj")
data_temp_lodes_age9_p <- data_temp_lodes_age9_p %>%
  rename(time = x, Response = stratum)
head(data_temp_lodes_age9_p)
summary(data_temp_lodes_age9_p)

supportReligMembers_age9_p <- ggplot(data_temp_lodes_age9_p,
                                     aes(x = time, stratum = Response, alluvium = traj,
                                         y = freq,
                                         fill = Response, label = Response)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "none") +
  xlab("Questionnaire time-point") + ylab("Frequency") +
  ggtitle("Obtains help/support from members of own religious group - Partners") + theme(plot.title = element_text(hjust = 0.5))

supportReligMembers_age9_p

# Save this plot
pdf("./Results/supportReligMembers_Age9_Partners.pdf", height = 6, width = 10)
plot(supportReligMembers_age9_p)
dev.off()


## Displaying these results as standard cross-tabs (using only complete cases, so data is identical to that in the sankey plot)

# preg by age 5
data %>%
  select(pb157, ph6249, pm4051) %>%
  rename(preg = pb157, age5 = ph6249, age9 = pm4051) %>%
  filter(complete.cases(preg, age5, age9)) %>%
  group_by(preg, age5) %>%
  summarise(freq = n())

# Age 5 by age 9
data %>%
  select(pb157, ph6249, pm4051) %>%
  rename(preg = pb157, age5 = ph6249, age9 = pm4051) %>%
  filter(complete.cases(preg, age5, age9)) %>%
  group_by(age5, age9) %>%
  summarise(freq = n())

# Proportions at each time-point
tab_age9_p <-data %>%
  select(pb157, ph6249, pm4051) %>%
  rename(preg = pb157, age5 = ph6249, age9 = pm4051) %>%
  filter(complete.cases(preg, age5, age9))
summary(tab_age9_p)


### Now repeat, but including 2019 RSBB data
data_temp_age28_p <- data %>%
  select(pb157, ph6249, pm4051, FC3091) %>%
  rename(preg = pb157, age5 = ph6249, age9 = pm4051, age28 = FC3091) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(preg, age5, age9, age28) %>%
  summarise(freq = n())

summary(data_temp_age28_p)
head(data_temp_age28_p)

data_temp_lodes_age28_p <- to_lodes_form(data_temp_age28_p, axes = 1:4, id = "traj")
data_temp_lodes_age28_p <- data_temp_lodes_age28_p %>%
  rename(time = x, Response = stratum)
head(data_temp_lodes_age28_p)
summary(data_temp_lodes_age28_p)

supportReligMembers_age28_p <- ggplot(data_temp_lodes_age28_p,
                                      aes(x = time, stratum = Response, alluvium = traj,
                                          y = freq,
                                          fill = Response, label = Response)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "none") +
  xlab("Questionnaire time-point") + ylab("Frequency") +
  ggtitle("Obtains help/support from members of own religious group - Partners") + theme(plot.title = element_text(hjust = 0.5))

supportReligMembers_age28_p

# Save this plot
pdf("./Results/supportReligMembers_Age28_Partners.pdf", height = 6, width = 10)
plot(supportReligMembers_age28_p)
dev.off()

## Displaying these results as standard cross-tabs (using only complete cases, so data is identical to that in the sankey plot)

# preg by age 5
data %>%
  select(pb157, ph6249, pm4051, FC3091) %>%
  rename(preg = pb157, age5 = ph6249, age9 = pm4051, age28 = FC3091) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(preg, age5) %>%
  summarise(freq = n())

# Age 5 by age 9
data %>%
  select(pb157, ph6249, pm4051, FC3091) %>%
  rename(preg = pb157, age5 = ph6249, age9 = pm4051, age28 = FC3091) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(age5, age9) %>%
  summarise(freq = n())

# Age 9 by age 28
data %>%
  select(pb157, ph6249, pm4051, FC3091) %>%
  rename(preg = pb157, age5 = ph6249, age9 = pm4051, age28 = FC3091) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(age9, age28) %>%
  summarise(freq = n())

# Proportions at each time-point
tab_age28_p <-data %>%
  select(pb157, ph6249, pm4051, FC3091) %>%
  rename(preg = pb157, age5 = ph6249, age9 = pm4051, age28 = FC3091) %>%
  filter(complete.cases(preg, age5, age9, age28))
summary(tab_age28_p)

## Very similar pattern of results to the mothers: Stable from preg to age 9, then slight drop to age 28

# Again, sample sizes do decrease quite substantially from age 9 to age 28
sum(data_temp_age9_p$freq)
sum(data_temp_age28_p$freq)



#####################################################################################################
#### Obtain help/support from other members of other religious groups - Responses: Yes vs No

### Mothers

## Pregnancy
table(data$d819, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(d819 = na_if(d819, "-1")) %>%
  mutate(d819 = factor(d819, levels = c("Yes", "No")))

table(data$d819, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data, but not especially biased towards non-believers, so will leave as is.
table(data$d810[is.na(data$d819)])

## Age 5
table(data$k6250, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(k6250 = na_if(k6250, "No response")) %>%
  mutate(k6250 = na_if(k6250, "Not completed")) %>%
  mutate(k6250 = na_if(k6250, "Triplet / quadruplet")) %>%
  mutate(k6250 = na_if(k6250, "Don't know")) %>%
  mutate(k6250 = na_if(k6250, "Text response")) %>%
  mutate(k6250 = factor(k6250, levels = c("Yes", "No")))

table(data$k6250, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data, but not especially biased towards non-believers, so will leave as is.
table(data$k6240[is.na(data$k6250)])

## Age 6
table(data$l7052, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(l7052 = na_if(l7052, "No response")) %>%
  mutate(l7052 = na_if(l7052, "Not completed")) %>%
  mutate(l7052 = na_if(l7052, "Don't know")) %>%
  mutate(l7052 = na_if(l7052, "Triplet / quadruplet")) %>%
  mutate(l7052 = na_if(l7052, "Text response")) %>%
  mutate(l7052 = factor(l7052, levels = c("Yes", "No")))

table(data$l7052, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data, but not especially biased towards non-believers, so will leave as is.
table(data$l7040[is.na(data$v)])

## Age 9
table(data$p4052, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(p4052 = na_if(p4052, "No response")) %>%
  mutate(p4052 = na_if(p4052, "Not completed")) %>%
  mutate(p4052 = na_if(p4052, "Triplet / quadruplet")) %>%
  mutate(p4052 = factor(p4052, levels = c("Yes", "No")))

table(data$p4052, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data, but not especially biased towards non-believers, so will leave as is.
table(data$p4040[is.na(data$p4052)])

## 2019 (Age 28)
table(data$Y3092, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(Y3092 = na_if(Y3092, "Missing")) %>%
  mutate(Y3092 = na_if(Y3092, "Missed whole section C")) %>%
  mutate(Y3092 = na_if(Y3092, "Mother of trip/quad")) %>%
  mutate(Y3092 = na_if(Y3092, "Questionnaire not completed")) %>%
  mutate(Y3092 = na_if(Y3092, "Unresolvable")) %>%
  mutate(Y3092 = factor(Y3092, levels = c("Yes", "No")))

table(data$Y3092, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data, but not especially biased towards non-believers, so will leave as is.
table(data$Y3000[is.na(data$Y3092)])


## Turn this data into a sankey plot - Both for pregnancy to 2019, and just from pregnancy to age 9 (as less missing data)

# First, have to convert the data to the correct format and remove any missing values. Will first convert the data to summary wide format, then convert to long/lode format. Age age 5 and 6 are very close together, to get a better sense of change over time will drop the age 6 data.
data_temp_age9 <- data %>%
  select(d819, k6250, p4052) %>%
  rename(preg = d819, age5 = k6250, age9 = p4052) %>%
  filter(complete.cases(preg, age5, age9)) %>%
  group_by(preg, age5, age9) %>%
  summarise(freq = n())

summary(data_temp_age9)
head(data_temp_age9)

data_temp_lodes_age9 <- to_lodes_form(data_temp_age9, axes = 1:3, id = "traj")
data_temp_lodes_age9 <- data_temp_lodes_age9 %>%
  rename(time = x, Response = stratum)
head(data_temp_lodes_age9)
summary(data_temp_lodes_age9)

supportOtherReligMembers_age9 <- ggplot(data_temp_lodes_age9,
                                   aes(x = time, stratum = Response, alluvium = traj,
                                       y = freq,
                                       fill = Response, label = Response)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "none") +
  xlab("Questionnaire time-point") + ylab("Frequency") +
  ggtitle("Obtains help/support from members of other religious groups - Mothers") + theme(plot.title = element_text(hjust = 0.5))

supportOtherReligMembers_age9

# Save this plot
pdf("./Results/supportOtherReligMembers_Age9_Mothers.pdf", height = 6, width = 10)
plot(supportOtherReligMembers_age9)
dev.off()


## Displaying these results as standard cross-tabs (using only complete cases, so data is identical to that in the sankey plot)

# preg by age 5
data %>%
  select(d819, k6250, p4052) %>%
  rename(preg = d819, age5 = k6250, age9 = p4052) %>%
  filter(complete.cases(preg, age5, age9)) %>%
  group_by(preg, age5) %>%
  summarise(freq = n())

# Age 5 by age 9
data %>%
  select(d819, k6250, p4052) %>%
  rename(preg = d819, age5 = k6250, age9 = p4052) %>%
  filter(complete.cases(preg, age5, age9)) %>%
  group_by(age5, age9) %>%
  summarise(freq = n())

# Proportions at each time-point
tab_age9 <-data %>%
  select(d819, k6250, p4052) %>%
  rename(preg = d819, age5 = k6250, age9 = p4052) %>%
  filter(complete.cases(preg, age5, age9))
summary(tab_age9)


### Now repeat, but including 2019 RSBB data
data_temp_age28 <- data %>%
  select(d819, k6250, p4052, Y3092) %>%
  rename(preg = d819, age5 = k6250, age9 = p4052, age28 = Y3092) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(preg, age5, age9, age28) %>%
  summarise(freq = n())

summary(data_temp_age28)
head(data_temp_age28)

data_temp_lodes_age28 <- to_lodes_form(data_temp_age28, axes = 1:4, id = "traj")
data_temp_lodes_age28 <- data_temp_lodes_age28 %>%
  rename(time = x, Response = stratum)
head(data_temp_lodes_age28)
summary(data_temp_lodes_age28)

supportOtherReligMembers_age28 <- ggplot(data_temp_lodes_age28,
                                    aes(x = time, stratum = Response, alluvium = traj,
                                        y = freq,
                                        fill = Response, label = Response)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "none") +
  xlab("Questionnaire time-point") + ylab("Frequency") +
  ggtitle("Obtains help/support from members of other religious groups - Mothers") + theme(plot.title = element_text(hjust = 0.5))

supportOtherReligMembers_age28

# Save this plot
pdf("./Results/supportOtherReligMembers_Age28_Mothers.pdf", height = 6, width = 10)
plot(supportOtherReligMembers_age28)
dev.off()


## Displaying these results as standard cross-tabs (using only complete cases, so data is identical to that in the sankey plot)

# preg by age 5
data %>%
  select(d819, k6250, p4052, Y3092) %>%
  rename(preg = d819, age5 = k6250, age9 = p4052, age28 = Y3092) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(preg, age5) %>%
  summarise(freq = n())

# Age 5 by age 9
data %>%
  select(d819, k6250, p4052, Y3092) %>%
  rename(preg = d819, age5 = k6250, age9 = p4052, age28 = Y3092) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(age5, age9) %>%
  summarise(freq = n())

# Age 9 by age 28
data %>%
  select(d819, k6250, p4052, Y3092) %>%
  rename(preg = d819, age5 = k6250, age9 = p4052, age28 = Y3092) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(age9, age28) %>%
  summarise(freq = n())

# Proportions at each time-point
tab_age28 <-data %>%
  select(d819, k6250, p4052, Y3092) %>%
  rename(preg = d819, age5 = k6250, age9 = p4052, age28 = Y3092) %>%
  filter(complete.cases(preg, age5, age9, age28))
summary(tab_age28)

## Stable-ish until age 9, then drops at age 28

# Sample sizes do decrease quite substantially from age 9 to age 28
sum(data_temp_age9$freq)
sum(data_temp_age28$freq)


### Now repeat for partners

## Pregnancy
table(data$pb158, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(pb158 = na_if(pb158, "-1")) %>%
  mutate(pb158 = recode(pb158, "Y" = "Yes", "N" = "No")) %>%
  mutate(pb158 = factor(pb158, levels = c("Yes", "No")))

table(data$pb158, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data, but not especially biased towards non-believers, so will leave as is.
table(data$pb150[is.na(data$pb158)])

## Age 5
table(data$ph6250, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(ph6250 = na_if(ph6250, "No response")) %>%
  mutate(ph6250 = na_if(ph6250, "Not completed")) %>%
  mutate(ph6250 = na_if(ph6250, "Triplet / quadruplet")) %>%
  mutate(ph6250 = factor(ph6250, levels = c("Yes", "No")))

table(data$ph6250, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data, but not especially biased towards non-believers, so will leave as is.
table(data$ph6240[is.na(data$ph6250)])

## Age 6
table(data$pj7052, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(pj7052 = na_if(pj7052, "No response")) %>%
  mutate(pj7052 = na_if(pj7052, "Not completed")) %>%
  mutate(pj7052 = na_if(pj7052, "Triplet / quadruplet")) %>%
  mutate(pj7052 = factor(pj7052, levels = c("Yes", "No")))

table(data$pj7052, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data, but not especially biased towards non-believers, so will leave as is.
table(data$pj7040[is.na(data$pj7052)])

## Age 9
table(data$pm4052, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(pm4052 = na_if(pm4052, "No response")) %>%
  mutate(pm4052 = na_if(pm4052, "Not completed")) %>%
  mutate(pm4052 = na_if(pm4052, "Triplet / quadruplet")) %>%
  mutate(pm4052 = factor(pm4052, levels = c("Yes", "No")))

table(data$pm4052, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data, but not especially biased towards non-believers, so will leave as is.
table(data$pm4040[is.na(data$pm4052)])

## 2019 (Age 28)
table(data$FC3092, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data <- data %>%
  mutate(FC3092 = na_if(FC3092, "Missing")) %>%
  mutate(FC3092 = na_if(FC3092, "Missed whole section C")) %>%
  mutate(FC3092 = na_if(FC3092, "Mother of trip/quad")) %>%
  mutate(FC3092 = na_if(FC3092, "Questionnaire not completed")) %>%
  mutate(FC3092 = na_if(FC3092, "Unresolvable")) %>%
  mutate(FC3092 = recode(FC3092, "Not applicable" = "No")) %>%
  mutate(FC3092 = factor(FC3092, levels = c("Yes", "No")))

table(data$FC3092, useNA = "ifany")

# See if missing and answered 'no' or 'not sure' to belief in a divine power, as could code as 'no' (as question not really relevant to atheists/agnostics). Is some missing data, but not especially biased towards non-believers, so will leave as is.
table(data$FC3000[is.na(data$FC3092)])


## Turn this data into a sankey plot - Both for pregnancy to 2019, and just from pregnancy to age 9 (as less missing data)

# First, have to convert the data to the correct format and remove any missing values. Will first convert the data to summary wide format, then convert to long/lode format. Age age 5 and 6 are very close together, to get a better sense of change over time will drop the age 6 data.
data_temp_age9_p <- data %>%
  select(pb158, ph6250, pm4052) %>%
  rename(preg = pb158, age5 = ph6250, age9 = pm4052) %>%
  filter(complete.cases(preg, age5, age9)) %>%
  group_by(preg, age5, age9) %>%
  summarise(freq = n())

summary(data_temp_age9_p)
head(data_temp_age9_p)

data_temp_lodes_age9_p <- to_lodes_form(data_temp_age9_p, axes = 1:3, id = "traj")
data_temp_lodes_age9_p <- data_temp_lodes_age9_p %>%
  rename(time = x, Response = stratum)
head(data_temp_lodes_age9_p)
summary(data_temp_lodes_age9_p)

supportOtherReligMembers_age9_p <- ggplot(data_temp_lodes_age9_p,
                                     aes(x = time, stratum = Response, alluvium = traj,
                                         y = freq,
                                         fill = Response, label = Response)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "none") +
  xlab("Questionnaire time-point") + ylab("Frequency") +
  ggtitle("Obtains help/support from members of other religious groups - Partners") + theme(plot.title = element_text(hjust = 0.5))

supportOtherReligMembers_age9_p

# Save this plot
pdf("./Results/supportOtherReligMembers_Age9_Partners.pdf", height = 6, width = 10)
plot(supportOtherReligMembers_age9_p)
dev.off()


## Displaying these results as standard cross-tabs (using only complete cases, so data is identical to that in the sankey plot)

# preg by age 5
data %>%
  select(pb158, ph6250, pm4052) %>%
  rename(preg = pb158, age5 = ph6250, age9 = pm4052) %>%
  filter(complete.cases(preg, age5, age9)) %>%
  group_by(preg, age5) %>%
  summarise(freq = n())

# Age 5 by age 9
data %>%
  select(pb158, ph6250, pm4052) %>%
  rename(preg = pb158, age5 = ph6250, age9 = pm4052) %>%
  filter(complete.cases(preg, age5, age9)) %>%
  group_by(age5, age9) %>%
  summarise(freq = n())

# Proportions at each time-point
tab_age9_p <-data %>%
  select(pb158, ph6250, pm4052) %>%
  rename(preg = pb158, age5 = ph6250, age9 = pm4052) %>%
  filter(complete.cases(preg, age5, age9))
summary(tab_age9_p)


### Now repeat, but including 2019 RSBB data
data_temp_age28_p <- data %>%
  select(pb158, ph6250, pm4052, FC3092) %>%
  rename(preg = pb158, age5 = ph6250, age9 = pm4052, age28 = FC3092) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(preg, age5, age9, age28) %>%
  summarise(freq = n())

summary(data_temp_age28_p)
head(data_temp_age28_p)

data_temp_lodes_age28_p <- to_lodes_form(data_temp_age28_p, axes = 1:4, id = "traj")
data_temp_lodes_age28_p <- data_temp_lodes_age28_p %>%
  rename(time = x, Response = stratum)
head(data_temp_lodes_age28_p)
summary(data_temp_lodes_age28_p)

supportOtherReligMembers_age28_p <- ggplot(data_temp_lodes_age28_p,
                                      aes(x = time, stratum = Response, alluvium = traj,
                                          y = freq,
                                          fill = Response, label = Response)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "none") +
  xlab("Questionnaire time-point") + ylab("Frequency") +
  ggtitle("Obtains help/support from members of other religious groups - Partners") + theme(plot.title = element_text(hjust = 0.5))

supportOtherReligMembers_age28_p

# Save this plot
pdf("./Results/supportOtherReligMembers_Age28_Partners.pdf", height = 6, width = 10)
plot(supportOtherReligMembers_age28_p)
dev.off()

## Displaying these results as standard cross-tabs (using only complete cases, so data is identical to that in the sankey plot)

# preg by age 5
data %>%
  select(pb158, ph6250, pm4052, FC3092) %>%
  rename(preg = pb158, age5 = ph6250, age9 = pm4052, age28 = FC3092) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(preg, age5) %>%
  summarise(freq = n())

# Age 5 by age 9
data %>%
  select(pb158, ph6250, pm4052, FC3092) %>%
  rename(preg = pb158, age5 = ph6250, age9 = pm4052, age28 = FC3092) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(age5, age9) %>%
  summarise(freq = n())

# Age 9 by age 28
data %>%
  select(pb158, ph6250, pm4052, FC3092) %>%
  rename(preg = pb158, age5 = ph6250, age9 = pm4052, age28 = FC3092) %>%
  filter(complete.cases(preg, age5, age9, age28)) %>%
  group_by(age9, age28) %>%
  summarise(freq = n())

# Proportions at each time-point
tab_age28_p <-data %>%
  select(pb158, ph6250, pm4052, FC3092) %>%
  rename(preg = pb158, age5 = ph6250, age9 = pm4052, age28 = FC3092) %>%
  filter(complete.cases(preg, age5, age9, age28))
summary(tab_age28_p)

## Slight decrease with age (but very low n...)

# Again, sample sizes do decrease quite substantially from age 9 to age 28
sum(data_temp_age9_p$freq)
sum(data_temp_age28_p$freq)



##################################################################################################
#### Next, to add in Isaac's latent class analysis data and repeat these analyses




##################################################################################################
#### Finally, run some simple analyses illustrating how this longitudinal data can be analysed going forwards

## For this, we will use the mother’s ‘belief in God/a divine power’ at pregnancy and age 9 time-points as an example. Given the multitude of possibilities of exploring patterns of change between these two time-points (nine possibilities in total), we will consider two methods to coding these variables:
## 1) Coding mothers into four groups: i) consistent believers (answered ‘yes’ at both time-points); ii) consistent non-believers (answered ‘no’ or ‘not sure’ at both time-points); iii) new believers (answered ‘no’ or ‘not sure’ in pregnancy, but ‘yes’ at age 9); and iv) new non-believers (answered ‘yes’ in pregnancy, but ‘no’ or ‘not sure’ at age 9).
## 2) Coding mothers into five groups: i) no change (same response at both time-points); ii) slight increase in RSBB (‘not sure’ in pregnancy and ‘yes’ at age 9, or ‘no’ in pregnancy and ‘not sure’ at age 9); iii) major increase in RSBB (from ‘no’ in pregnancy’ to ‘yes’ at age 9); iv) slight decrease in RSBB (‘yes’ in pregnancy to ‘not sure’ at age 9, or ‘not sure’ in pregnancy to ‘no’ at age 9); and v) major decrease in RSBB (from ‘yes’ in pregnancy to ‘no’ at age 9). 

## We will then explore whether the following exposures are associated with these RSBB trajectories (All exposures were measured in pregnancy, other than household income which was measured when the study children were approximately aged 3/4 years old): 
# age at birth (in years; variable mz028b)
# ethnicity (White vs other than White; variable c800)
# socioeconomic position (as proxied by highest maternal education [CSE/None vs vocational vs O-level vs A-level vs Degree; variable c645a], household income [log GDP income per week; variable logavinceq], home ownership status [owned/mortgaged vs rented vs council/housing association vs other; variable a006] and index of multiple deprivation [quintiles; variable dimd2010q5_M]) 
# and whether this was their first pregnancy (yes vs no; variable b032). 


## Cross-tabs of pregnancy by age 9 data for religious belief
table(data$d810, data$p4040)

## Coding as first method
data <- data %>%
  mutate(belief_meth1 = NA) %>%
  mutate(belief_meth1 = ifelse(d810 == "Yes" & p4040 == "Yes", "ConsBel", belief_meth1)) %>%
  mutate(belief_meth1 = ifelse((d810 == "Not sure" | d810 == "No") & 
           (p4040 == "Not sure" | p4040 == "No"), "ConsNonBel", belief_meth1)) %>%
  mutate(belief_meth1 = ifelse(d810 == "Yes" & (p4040 == "Not sure" | p4040 == "No"),  
                               "NewNonBel", belief_meth1)) %>%
  mutate(belief_meth1 = ifelse((d810 == "Not sure" | d810 == "No") & p4040 == "Yes", 
                               "NewBel", belief_meth1)) %>%
  mutate(belief_meth1 = factor(belief_meth1, levels = c("ConsNonBel", "ConsBel", "NewBel", "NewNonBel")))

table(data$belief_meth1)

# Coding as second method
data <- data %>%
  mutate(belief_meth2 = NA) %>%
  mutate(belief_meth2 = ifelse((d810 == "Yes" & p4040 == "Yes") | (d810 == "No" & p4040 == "No") | 
                                 (d810 == "Not sure" & p4040 == "Not sure"), "NoChange", belief_meth2)) %>%
  mutate(belief_meth2 = ifelse((d810 == "Yes" & p4040 == "Not sure") | 
                                 (d810 == "Not sure" & p4040 == "No"), "SmallDec", belief_meth2)) %>%
  mutate(belief_meth2 = ifelse(d810 == "Yes" & p4040 == "No", "BigDec", belief_meth2)) %>%
  mutate(belief_meth2 = ifelse((d810 == "Not sure" & p4040 == "Yes") | (d810 == "No" & p4040 == "Not sure"),
                               "SmallInc", belief_meth2)) %>%
  mutate(belief_meth2 = ifelse(d810 == "No" & p4040 == "Yes", "BigInc", belief_meth2)) %>%
  mutate(belief_meth2 = factor(belief_meth2, levels = c("NoChange", "SmallInc", "BigInc", "SmallDec", "BigDec")))

table(data$belief_meth2)


## Tidy the covariates

# Mother's age at birth (years)
table(data$mz028b, useNA = "ifany")

data <- data %>%
  mutate(mz028b = na_if(mz028b, "Miscarried")) %>%
  mutate(mz028b = na_if(mz028b, "Outcome NK")) %>%
  mutate(mz028b = recode(mz028b, "< 16" = "16", ">43" = "43")) %>%
  mutate(mz028b = as.numeric(mz028b))

table(data$mz028b, useNA = "ifany")
summary(data$mz028b)

# Mother's ethnicity (White vs other than White)
table(data$c800, useNA = "ifany")

data <- data %>%
  mutate(c800 = na_if(c800, "Missing")) %>%
  mutate(c800 = recode(c800, "Bangladeshi" = "Other than White", "Black African" = "Other than White", 
                       "Black Caribbean" = "Other than White", "Chinese" = "Other than White", 
                       "Indian" = "Other than White", "Other" = "Other than White", 
                       "Other black" = "Other than White", "Pakistani" = "Other than White")) %>%
  mutate(c800 = factor(c800, levels = c("White", "Other than White")))

table(data$c800, useNA = "ifany")

# Mother's highest education qualification (CSE/None vs Vocational vs O-level vs A-level vs Degree)
table(data$c645a, useNA = "ifany")

data <- data %>%
  mutate(c645a = na_if(c645a, "Missing")) %>%
  mutate(c645a = recode(c645a, "CSE" = "CSE/None")) %>%
  mutate(c645a = factor(c645a, levels = c("CSE/None", "Vocational", "O level", "A level", "Degree")))

table(data$c645a, useNA = "ifany")

# Household income (log GDP per week)
data <- data %>%
  mutate(logavinceq = as.numeric(logavinceq))

summary(data$logavinceq)

# Home ownership status
table(data$a006, useNA = "ifany")

data <- data %>%
  mutate(a006 = na_if(a006, "Missing")) %>%
  mutate(a006 = na_if(a006, "YE short")) %>%
  mutate(a006 = recode(a006, "Council rented" = "Council/HA", "HA rented" = "Council/HA",
                       "Mortgaged" = "Owned/Mortgaged", "Owned" = "Owned/Mortgaged",
                       "RENT PRIV FURN" = "Rented", "RENT PRIV UNFURN" = "Rented")) %>%
  mutate(a006 = factor(a006, levels = c("Owned/Mortgaged", "Rented", "Council/HA", "Other")))

table(data$a006, useNA = "ifany")

# IMD
table(data$dimd2010q5, useNA = "ifany")

data <- data %>%
  mutate(dimd2010q5 = na_if(dimd2010q5, "Missing")) %>%
  mutate(dimd2010q5 = na_if(dimd2010q5, "Triplets/Quadruplets")) %>%
  mutate(dimd2010q5 = recode(dimd2010q5, "Least deprived" = "Quin. 1/Least deprived", "2" = "Quintile 2",
                             "3" = "Quintile 3", "4" = "Quintile 4", "Most deprived" = "Quin. 5/Most deprived")) %>%
  mutate(dimd2010q5 = factor(dimd2010q5, levels = c("Quin. 1/Least deprived", "Quintile 2", "Quintile 3", 
                                                    "Quintile 4", "Quin. 5/Most deprived")))

table(data$dimd2010q5, useNA = "ifany")

# Whether is first pregnancy or not
table(data$b032, useNA = "ifany")

data <- data %>%
  mutate(b032 = na_if(b032, "Missing")) %>%
  mutate(b032 = na_if(b032, "HaB short")) %>%
  mutate(b032 = na_if(b032, "Inconsistent data")) %>%
  mutate(b032 = as.numeric(b032)) %>%
  mutate(b032 = ifelse(b032 > 0 & !is.na(b032), 1, b032)) %>%
  mutate(b032 = as.factor(b032)) %>%
  mutate(b032 = recode(b032, "0" = "NewMum", "1" = "PriorMum")) %>%
  mutate(b032 = factor(b032, levels = c("PriorMum", "NewMum")))

table(data$b032, useNA = "ifany")


#### Now conduct multinomial regression for each method and for each covariate (all with adjust for age, other than the age-only model). Will calculate p-values using Wald/z-tests (for simple guide to multinomial regression in R, see: https://stats.oarc.ucla.edu/r/dae/multinomial-logistic-regression/)
library(nnet)

## Initialise a dataframe to save results to
results_meth1 <- data.frame(exposure = character(), exp_level = character(), outcome_level = character(),
                            RRR = numeric(), lower_ci = numeric(), upper_ci = numeric(), 
                            p = numeric(), p_exp = numeric(), n = numeric())


### Method 1
table(data$belief_meth1, useNA = "ifany")

## Age at birth

# Descriptives
(meth1_data_age <- data %>%
  filter(complete.cases(belief_meth1, mz028b)) %>%
  group_by(belief_meth1) %>%
  summarise(n = n(), mean = mean(mz028b), sd = sd(mz028b), se = sd/sqrt(n),
            lower_ci = mean - (qt(0.975, df = n - 1) * se), upper_ci = mean + (qt(0.975, df = n - 1) * se)))

# Make plot of results
(meth1_age_plot <- ggplot(meth1_data_age) +
  geom_point(aes(x = belief_meth1, y = mean), size = 6, shape = 18) +
  geom_errorbar(aes(x = belief_meth1, ymin = lower_ci, ymax = upper_ci), width = 0.5, size = 1) +
  ylab("Mean age at birth (years)") + xlab("Change in belief from pregnancy to age 9") +
  ylim(28, 30) + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_discrete(labels = c("ConsNonBel" = "Consistent non-believer \n(n=2,865)", 
                              "ConsBel" = "Consistent believer \n(n=2,906)",
                              "NewBel" = "New believer \n(n=587)", "NewNonBel" = "New non-believer \n(n=855)")) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16)))

# Save this plot
pdf("./Results/meth1_age_plot.pdf", height = 6, width = 10)
plot(meth1_age_plot)
dev.off()

# Model
meth1_age <- multinom(belief_meth1 ~ mz028b, data = data)
summary(meth1_age)
confint(meth1_age)

# P-values
z <- summary(meth1_age)$coefficients/summary(meth1_age)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

# Relative risk ratios and 95% CIs
exp(coef(meth1_age))
exp(confint(meth1_age))

# Compare to null model
meth1_null <- multinom(belief_meth1 ~ 1, data = data, subset = !is.na(mz028b))
anova(meth1_null, meth1_age)

# Add results to results dataframe
results_meth1 <- rbind(results_meth1,
                       setNames(data.frame("AgeAtBirth (years)", "NA", "ConsBel", 
                                           round(exp(coef(meth1_age)["ConsBel", "mz028b"]), 2),
                                           round(exp(confint(meth1_age)["mz028b", "2.5 %", "ConsBel"]), 2), 
                                           round(exp(confint(meth1_age)["mz028b", "97.5 %", "ConsBel"]), 2), 
                                           round(p["ConsBel", "mz028b"], 3), 
                                           round(anova(meth1_null, meth1_age)[2, "Pr(Chi)"], 3),
                                           nrow(meth1_age$fitted.values)), 
                                names(results_meth1)),
                       setNames(data.frame("AgeAtBirth (years)", "NA", "NewBel", 
                                           round(exp(coef(meth1_age)["NewBel", "mz028b"]), 2),
                                           round(exp(confint(meth1_age)["mz028b", "2.5 %", "NewBel"]), 2), 
                                           round(exp(confint(meth1_age)["mz028b", "97.5 %", "NewBel"]), 2), 
                                           round(p["NewBel", "mz028b"], 3),
                                           round(anova(meth1_null, meth1_age)[2, "Pr(Chi)"], 3),
                                           nrow(meth1_age$fitted.values)), 
                                names(results_meth1)),
                       setNames(data.frame("AgeAtBirth (years)", "NA", "NewNonBel", 
                                           round(exp(coef(meth1_age)["NewNonBel", "mz028b"]), 2),
                                           round(exp(confint(meth1_age)["mz028b", "2.5 %", "NewNonBel"]), 2), 
                                           round(exp(confint(meth1_age)["mz028b", "97.5 %", "NewNonBel"]), 2), 
                                           round(p["NewNonBel", "mz028b"], 3),
                                           round(anova(meth1_null, meth1_age)[2, "Pr(Chi)"], 3),
                                           nrow(meth1_age$fitted.values)), 
                                names(results_meth1)))

results_meth1


## Ethnicity

# Descriptives
table(data$belief_meth1, data$c800)
round(prop.table(table(data$belief_meth1, data$c800), 1) * 100, 2)

# And make stacked bar-plot of results
(meth1_data_eth <- data %>%
    filter(complete.cases(belief_meth1, c800)) %>%
    group_by(belief_meth1, c800) %>%
    summarise(n = n()))

(meth1_eth_plot <- ggplot(meth1_data_eth, aes(fill = fct_rev(c800), y = n, x = belief_meth1)) +
    geom_bar(position = "fill", stat = "identity") +
    ylab("Proportion of responses") + xlab("Change in belief from pregnancy to age 9") +
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_x_discrete(labels = c("ConsNonBel" = "Consistent non-believer \n(n=2,812)", 
                                "ConsBel" = "Consistent believer \n(n=2,844)",
                                "NewBel" = "New believer \n(n=574)", "NewNonBel" = "New non-believer \n(n=834)")) +
    labs(fill = "Ethnicity") +
    theme(axis.text = element_text(size = 12), axis.title = element_text(size = 16)))

# Save this plot
pdf("./Results/meth1_ethnicity_plot.pdf", height = 6, width = 10)
plot(meth1_eth_plot)
dev.off()

# Model
meth1_ethnic <- multinom(belief_meth1 ~ c800 + mz028b, data = data)
summary(meth1_ethnic)
confint(meth1_ethnic)

# P-values
z <- summary(meth1_ethnic)$coefficients/summary(meth1_ethnic)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

# Relative risk ratios and 95% CIs
exp(coef(meth1_ethnic))
exp(confint(meth1_ethnic))

# Compare to null age_only model
meth1_age <- multinom(belief_meth1 ~ mz028b, data = data, subset = !is.na(c800))
anova(meth1_age, meth1_ethnic)

# Add results to results dataframe
results_meth1 <- rbind(results_meth1,
                       setNames(data.frame("Ethnicity (ref=White)", "Other than White", "ConsBel", 
                                           round(exp(coef(meth1_ethnic)["ConsBel", "c800Other than White"]), 2),
                                           round(exp(confint(meth1_ethnic)["c800Other than White", "2.5 %", 
                                                                           "ConsBel"]), 2), 
                                           round(exp(confint(meth1_ethnic)["c800Other than White", "97.5 %", 
                                                                           "ConsBel"]), 2), 
                                           round(p["ConsBel", "c800Other than White"], 3), 
                                           round(anova(meth1_age, meth1_ethnic)[2, "Pr(Chi)"], 3),
                                           nrow(meth1_ethnic$fitted.values)), 
                                names(results_meth1)),
                       setNames(data.frame("Ethnicity (ref=White)", "Other than White", "NewBel", 
                                           round(exp(coef(meth1_ethnic)["NewBel", "c800Other than White"]), 2),
                                           round(exp(confint(meth1_ethnic)["c800Other than White", "2.5 %", 
                                                                           "NewBel"]), 2), 
                                           round(exp(confint(meth1_ethnic)["c800Other than White", "97.5 %", 
                                                                           "NewBel"]), 2), 
                                           round(p["NewBel", "c800Other than White"], 3),
                                           round(anova(meth1_age, meth1_ethnic)[2, "Pr(Chi)"], 3),
                                           nrow(meth1_ethnic$fitted.values)), 
                                names(results_meth1)),
                       setNames(data.frame("Ethnicity (ref=White)", "Other than White", "NewNonBel", 
                                           round(exp(coef(meth1_ethnic)["NewNonBel", "c800Other than White"]), 2),
                                           round(exp(confint(meth1_ethnic)["c800Other than White", "2.5 %",
                                                                           "NewNonBel"]), 2), 
                                           round(exp(confint(meth1_ethnic)["c800Other than White", "97.5 %",
                                                                           "NewNonBel"]), 2), 
                                           round(p["NewNonBel", "c800Other than White"], 3),
                                           round(anova(meth1_age, meth1_ethnic)[2, "Pr(Chi)"], 3),
                                           nrow(meth1_ethnic$fitted.values)), 
                                names(results_meth1)))

results_meth1


## Maternal education

# Descriptives
table(data$belief_meth1, data$c645a)
round(prop.table(table(data$belief_meth1, data$c645a), 1) * 100, 2)

# And make stacked bar-plot of results
(meth1_data_edu <- data %>%
    filter(complete.cases(belief_meth1, c645a)) %>%
    group_by(belief_meth1, c645a) %>%
    summarise(n = n()))

(meth1_edu_plot <- ggplot(meth1_data_edu, aes(fill = fct_rev(c645a), y = n, x = belief_meth1)) +
    geom_bar(position = "fill", stat = "identity") +
    ylab("Proportion of responses") + xlab("Change in belief from pregnancy to age 9") +
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_x_discrete(labels = c("ConsNonBel" = "Consistent non-believer \n(n=2,815)", 
                                "ConsBel" = "Consistent believer \n(n=2,854)",
                                "NewBel" = "New believer \n(n=576)", "NewNonBel" = "New non-believer \n(n=838)")) +
    labs(fill = "Educational attainment") +
    theme(axis.text = element_text(size = 12), axis.title = element_text(size = 16)))

# Save this plot
pdf("./Results/meth1_education_plot.pdf", height = 6, width = 10)
plot(meth1_edu_plot)
dev.off()

# Model
meth1_edu <- multinom(belief_meth1 ~ c645a + mz028b, data = data)
summary(meth1_edu)
confint(meth1_edu)

# P-values
z <- summary(meth1_edu)$coefficients/summary(meth1_edu)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

# Relative risk ratios and 95% CIs
exp(coef(meth1_edu))
exp(confint(meth1_edu))

# Compare to null age_only model
meth1_age <- multinom(belief_meth1 ~ mz028b, data = data, subset = !is.na(c645a))
anova(meth1_age, meth1_edu)

# Add results to results dataframe
results_meth1 <- rbind(results_meth1,
                       setNames(data.frame("Education (ref=CSE/None)", "Vocational", "ConsBel", 
                                           round(exp(coef(meth1_edu)["ConsBel", "c645aVocational"]), 2),
                                           round(exp(confint(meth1_edu)["c645aVocational", "2.5 %", 
                                                                           "ConsBel"]), 2), 
                                           round(exp(confint(meth1_edu)["c645aVocational", "97.5 %", 
                                                                           "ConsBel"]), 2), 
                                           round(p["ConsBel", "c645aVocational"], 3), 
                                           round(anova(meth1_age, meth1_edu)[2, "Pr(Chi)"], 3),
                                           nrow(meth1_edu$fitted.values)), 
                                names(results_meth1)),
                       setNames(data.frame("Education (ref=CSE/None)", "O level", "ConsBel", 
                                           round(exp(coef(meth1_edu)["ConsBel", "c645aO level"]), 2),
                                           round(exp(confint(meth1_edu)["c645aO level", "2.5 %", 
                                                                        "ConsBel"]), 2), 
                                           round(exp(confint(meth1_edu)["c645aO level", "97.5 %", 
                                                                        "ConsBel"]), 2), 
                                           round(p["ConsBel", "c645aO level"], 3), 
                                           round(anova(meth1_age, meth1_edu)[2, "Pr(Chi)"], 3),
                                           nrow(meth1_edu$fitted.values)), 
                                names(results_meth1)),
                       setNames(data.frame("Education (ref=CSE/None)", "A level", "ConsBel", 
                                           round(exp(coef(meth1_edu)["ConsBel", "c645aA level"]), 2),
                                           round(exp(confint(meth1_edu)["c645aA level", "2.5 %", 
                                                                        "ConsBel"]), 2), 
                                           round(exp(confint(meth1_edu)["c645aA level", "97.5 %", 
                                                                        "ConsBel"]), 2), 
                                           round(p["ConsBel", "c645aA level"], 3), 
                                           round(anova(meth1_age, meth1_edu)[2, "Pr(Chi)"], 3),
                                           nrow(meth1_edu$fitted.values)), 
                                names(results_meth1)),
                       setNames(data.frame("Education (ref=CSE/None)", "Degree", "ConsBel", 
                                           round(exp(coef(meth1_edu)["ConsBel", "c645aDegree"]), 2),
                                           round(exp(confint(meth1_edu)["c645aDegree", "2.5 %", 
                                                                        "ConsBel"]), 2), 
                                           round(exp(confint(meth1_edu)["c645aDegree", "97.5 %", 
                                                                        "ConsBel"]), 2), 
                                           round(p["ConsBel", "c645aDegree"], 3), 
                                           round(anova(meth1_age, meth1_edu)[2, "Pr(Chi)"], 3),
                                           nrow(meth1_edu$fitted.values)), 
                                names(results_meth1)),
                       setNames(data.frame("Education (ref=CSE/None)", "Vocational", "NewBel", 
                                           round(exp(coef(meth1_edu)["NewBel", "c645aVocational"]), 2),
                                           round(exp(confint(meth1_edu)["c645aVocational", "2.5 %", 
                                                                        "NewBel"]), 2), 
                                           round(exp(confint(meth1_edu)["c645aVocational", "97.5 %", 
                                                                        "NewBel"]), 2), 
                                           round(p["NewBel", "c645aVocational"], 3), 
                                           round(anova(meth1_age, meth1_edu)[2, "Pr(Chi)"], 3),
                                           nrow(meth1_edu$fitted.values)), 
                                names(results_meth1)),
                       setNames(data.frame("Education (ref=CSE/None)", "O level", "NewBel", 
                                           round(exp(coef(meth1_edu)["NewBel", "c645aO level"]), 2),
                                           round(exp(confint(meth1_edu)["c645aO level", "2.5 %", 
                                                                        "NewBel"]), 2), 
                                           round(exp(confint(meth1_edu)["c645aO level", "97.5 %", 
                                                                        "NewBel"]), 2), 
                                           round(p["NewBel", "c645aO level"], 3), 
                                           round(anova(meth1_age, meth1_edu)[2, "Pr(Chi)"], 3),
                                           nrow(meth1_edu$fitted.values)), 
                                names(results_meth1)),
                       setNames(data.frame("Education (ref=CSE/None)", "A level", "NewBel", 
                                           round(exp(coef(meth1_edu)["NewBel", "c645aA level"]), 2),
                                           round(exp(confint(meth1_edu)["c645aA level", "2.5 %", 
                                                                        "NewBel"]), 2), 
                                           round(exp(confint(meth1_edu)["c645aA level", "97.5 %", 
                                                                        "NewBel"]), 2), 
                                           round(p["NewBel", "c645aA level"], 3), 
                                           round(anova(meth1_age, meth1_edu)[2, "Pr(Chi)"], 3),
                                           nrow(meth1_edu$fitted.values)), 
                                names(results_meth1)),
                       setNames(data.frame("Education (ref=CSE/None)", "Degree", "NewBel", 
                                           round(exp(coef(meth1_edu)["NewBel", "c645aDegree"]), 2),
                                           round(exp(confint(meth1_edu)["c645aDegree", "2.5 %", 
                                                                        "NewBel"]), 2), 
                                           round(exp(confint(meth1_edu)["c645aDegree", "97.5 %", 
                                                                        "NewBel"]), 2), 
                                           round(p["NewBel", "c645aDegree"], 3), 
                                           round(anova(meth1_age, meth1_edu)[2, "Pr(Chi)"], 3),
                                           nrow(meth1_edu$fitted.values)), 
                                names(results_meth1)),
                       setNames(data.frame("Education (ref=CSE/None)", "Vocational", "NewNonBel", 
                                           round(exp(coef(meth1_edu)["NewNonBel", "c645aVocational"]), 2),
                                           round(exp(confint(meth1_edu)["c645aVocational", "2.5 %", 
                                                                        "NewNonBel"]), 2), 
                                           round(exp(confint(meth1_edu)["c645aVocational", "97.5 %", 
                                                                        "NewNonBel"]), 2), 
                                           round(p["NewNonBel", "c645aVocational"], 3), 
                                           round(anova(meth1_age, meth1_edu)[2, "Pr(Chi)"], 3),
                                           nrow(meth1_edu$fitted.values)), 
                                names(results_meth1)),
                       setNames(data.frame("Education (ref=CSE/None)", "O level", "NewNonBel", 
                                           round(exp(coef(meth1_edu)["NewNonBel", "c645aO level"]), 2),
                                           round(exp(confint(meth1_edu)["c645aO level", "2.5 %", 
                                                                        "NewNonBel"]), 2), 
                                           round(exp(confint(meth1_edu)["c645aO level", "97.5 %", 
                                                                        "NewNonBel"]), 2), 
                                           round(p["NewNonBel", "c645aO level"], 3), 
                                           round(anova(meth1_age, meth1_edu)[2, "Pr(Chi)"], 3),
                                           nrow(meth1_edu$fitted.values)), 
                                names(results_meth1)),
                       setNames(data.frame("Education (ref=CSE/None)", "A level", "NewNonBel", 
                                           round(exp(coef(meth1_edu)["NewNonBel", "c645aA level"]), 2),
                                           round(exp(confint(meth1_edu)["c645aA level", "2.5 %", 
                                                                        "NewNonBel"]), 2), 
                                           round(exp(confint(meth1_edu)["c645aA level", "97.5 %", 
                                                                        "NewNonBel"]), 2), 
                                           round(p["NewNonBel", "c645aA level"], 3), 
                                           round(anova(meth1_age, meth1_edu)[2, "Pr(Chi)"], 3),
                                           nrow(meth1_edu$fitted.values)), 
                                names(results_meth1)),
                       setNames(data.frame("Education (ref=CSE/None)", "Degree", "NewNonBel", 
                                           round(exp(coef(meth1_edu)["NewNonBel", "c645aDegree"]), 2),
                                           round(exp(confint(meth1_edu)["c645aDegree", "2.5 %", 
                                                                        "NewNonBel"]), 2), 
                                           round(exp(confint(meth1_edu)["c645aDegree", "97.5 %", 
                                                                        "NewNonBel"]), 2), 
                                           round(p["NewNonBel", "c645aDegree"], 3), 
                                           round(anova(meth1_age, meth1_edu)[2, "Pr(Chi)"], 3),
                                           nrow(meth1_edu$fitted.values)), 
                                names(results_meth1))
                       )

results_meth1


## Household income

# Descriptives
(meth1_data_income <- data %>%
  filter(complete.cases(belief_meth1, logavinceq)) %>%
  group_by(belief_meth1) %>%
  summarise(n = n(), mean = mean(logavinceq), sd = sd(logavinceq), se = sd/sqrt(n),
            lower_ci = mean - (qt(0.975, df = n - 1) * se), upper_ci = mean + (qt(0.975, df = n - 1) * se)))

# Make plot of results
(meth1_income_plot <- ggplot(meth1_data_income) +
    geom_point(aes(x = belief_meth1, y = mean), size = 6, shape = 18) +
    geom_errorbar(aes(x = belief_meth1, ymin = lower_ci, ymax = upper_ci), width = 0.5, size = 1) +
    ylab("Mean weekly household income (log GBP)") + xlab("Change in belief from pregnancy to age 9") +
    ylim(5.25, 5.45) + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_x_discrete(labels = c("ConsNonBel" = "Consistent non-believer \n(n=2,652)", 
                                "ConsBel" = "Consistent believer \n(n=2,652)",
                                "NewBel" = "New believer \n(n=536)", "NewNonBel" = "New non-believer \n(n=809)")) +
    theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16)))

# Save this plot
pdf("./Results/meth1_income_plot.pdf", height = 6, width = 10)
plot(meth1_income_plot)
dev.off()

# Model
meth1_income <- multinom(belief_meth1 ~ logavinceq + mz028b, data = data)
summary(meth1_income)
confint(meth1_income)

# P-values
z <- summary(meth1_income)$coefficients/summary(meth1_income)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

# Relative risk ratios and 95% CIs
exp(coef(meth1_income))
exp(confint(meth1_income))

# Compare to null age_only model
meth1_age <- multinom(belief_meth1 ~ mz028b, data = data, subset = !is.na(logavinceq))
anova(meth1_age, meth1_income)

# Add results to results dataframe
results_meth1 <- rbind(results_meth1,
                       setNames(data.frame("Income (log GBP)", "NA", "ConsBel", 
                                           round(exp(coef(meth1_income)["ConsBel", "logavinceq"]), 2),
                                           round(exp(confint(meth1_income)["logavinceq", "2.5 %", "ConsBel"]), 2), 
                                           round(exp(confint(meth1_income)["logavinceq", "97.5 %", "ConsBel"]), 2), 
                                           round(p["ConsBel", "logavinceq"], 3), 
                                           round(anova(meth1_age, meth1_income)[2, "Pr(Chi)"], 3),
                                           nrow(meth1_income$fitted.values)), 
                                names(results_meth1)),
                       setNames(data.frame("Income (log GBP)", "NA", "NewBel", 
                                           round(exp(coef(meth1_income)["NewBel", "logavinceq"]), 2),
                                           round(exp(confint(meth1_income)["logavinceq", "2.5 %", "NewBel"]), 2), 
                                           round(exp(confint(meth1_income)["logavinceq", "97.5 %", "NewBel"]), 2), 
                                           round(p["NewBel", "logavinceq"], 3),
                                           round(anova(meth1_age, meth1_income)[2, "Pr(Chi)"], 3),
                                           nrow(meth1_income$fitted.values)), 
                                names(results_meth1)),
                       setNames(data.frame("Income (log GBP)", "NA", "NewNonBel", 
                                           round(exp(coef(meth1_income)["NewNonBel", "logavinceq"]), 2),
                                           round(exp(confint(meth1_income)["logavinceq", "2.5 %", "NewNonBel"]), 2), 
                                           round(exp(confint(meth1_income)["logavinceq", "97.5 %", "NewNonBel"]), 2), 
                                           round(p["NewNonBel", "logavinceq"], 3),
                                           round(anova(meth1_age, meth1_income)[2, "Pr(Chi)"], 3),
                                           nrow(meth1_income$fitted.values)), 
                                names(results_meth1)))

results_meth1


## Home ownership

# Descriptives
table(data$belief_meth1, data$a006)
round(prop.table(table(data$belief_meth1, data$a006), 1) * 100, 2)

# And make stacked bar-plot of results
(meth1_data_home <- data %>%
    filter(complete.cases(belief_meth1, a006)) %>%
    group_by(belief_meth1, a006) %>%
    summarise(n = n()))

(meth1_home_plot <- ggplot(meth1_data_home, aes(fill = fct_rev(a006), y = n, x = belief_meth1)) +
    geom_bar(position = "fill", stat = "identity") +
    ylab("Proportion of responses") + xlab("Change in belief from pregnancy to age 9") +
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_x_discrete(labels = c("ConsNonBel" = "Consistent non-believer \n(n=2,818)", 
                                "ConsBel" = "Consistent believer \n(n=2,856)",
                                "NewBel" = "New believer \n(n=575)", "NewNonBel" = "New non-believer \n(n=839)")) +
    labs(fill = "Home Ownership Status") +
    theme(axis.text = element_text(size = 12), axis.title = element_text(size = 16)))

# Save this plot
pdf("./Results/meth1_homeOwnership_plot.pdf", height = 6, width = 10)
plot(meth1_home_plot)
dev.off()

# Model
meth1_home <- multinom(belief_meth1 ~ a006 + mz028b, data = data)
summary(meth1_home)
confint(meth1_home)

# P-values
z <- summary(meth1_home)$coefficients/summary(meth1_home)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

# Relative risk ratios and 95% CIs
exp(coef(meth1_home))
exp(confint(meth1_home))

# Compare to null age_only model
meth1_age <- multinom(belief_meth1 ~ mz028b, data = data, subset = !is.na(a006))
anova(meth1_age, meth1_home)

# Add results to results dataframe
results_meth1 <- rbind(results_meth1,
                       setNames(data.frame("Home Ownership (ref=Owned/Mortgaged)", "Rented", "ConsBel", 
                                           round(exp(coef(meth1_home)["ConsBel", "a006Rented"]), 2),
                                           round(exp(confint(meth1_home)["a006Rented", "2.5 %", 
                                                                        "ConsBel"]), 2), 
                                           round(exp(confint(meth1_home)["a006Rented", "97.5 %", 
                                                                        "ConsBel"]), 2), 
                                           round(p["ConsBel", "a006Rented"], 3), 
                                           round(anova(meth1_age, meth1_home)[2, "Pr(Chi)"], 3),
                                           nrow(meth1_home$fitted.values)), 
                                names(results_meth1)),
                       setNames(data.frame("Home Ownership (ref=Owned/Mortgaged)", "Council/HA", "ConsBel", 
                                           round(exp(coef(meth1_home)["ConsBel", "a006Council/HA"]), 2),
                                           round(exp(confint(meth1_home)["a006Council/HA", "2.5 %", 
                                                                        "ConsBel"]), 2), 
                                           round(exp(confint(meth1_home)["a006Council/HA", "97.5 %", 
                                                                        "ConsBel"]), 2), 
                                           round(p["ConsBel", "a006Council/HA"], 3), 
                                           round(anova(meth1_age, meth1_home)[2, "Pr(Chi)"], 3),
                                           nrow(meth1_home$fitted.values)), 
                                names(results_meth1)),
                       setNames(data.frame("Home Ownership (ref=Owned/Mortgaged)", "Other", "ConsBel", 
                                           round(exp(coef(meth1_home)["ConsBel", "a006Other"]), 2),
                                           round(exp(confint(meth1_home)["a006Other", "2.5 %", 
                                                                        "ConsBel"]), 2), 
                                           round(exp(confint(meth1_home)["a006Other", "97.5 %", 
                                                                        "ConsBel"]), 2), 
                                           round(p["ConsBel", "a006Other"], 3), 
                                           round(anova(meth1_age, meth1_home)[2, "Pr(Chi)"], 3),
                                           nrow(meth1_home$fitted.values)), 
                                names(results_meth1)),
                       setNames(data.frame("Home Ownership (ref=Owned/Mortgaged)", "Rented", "NewBel", 
                                           round(exp(coef(meth1_home)["NewBel", "a006Rented"]), 2),
                                           round(exp(confint(meth1_home)["a006Rented", "2.5 %", 
                                                                        "NewBel"]), 2), 
                                           round(exp(confint(meth1_home)["a006Rented", "97.5 %", 
                                                                        "NewBel"]), 2), 
                                           round(p["NewBel", "a006Rented"], 3), 
                                           round(anova(meth1_age, meth1_home)[2, "Pr(Chi)"], 3),
                                           nrow(meth1_home$fitted.values)), 
                                names(results_meth1)),
                       setNames(data.frame("Home Ownership (ref=Owned/Mortgaged)", "Council/HA", "NewBel", 
                                           round(exp(coef(meth1_home)["NewBel", "a006Council/HA"]), 2),
                                           round(exp(confint(meth1_home)["a006Council/HA", "2.5 %", 
                                                                        "NewBel"]), 2), 
                                           round(exp(confint(meth1_home)["a006Council/HA", "97.5 %", 
                                                                        "NewBel"]), 2), 
                                           round(p["NewBel", "a006Council/HA"], 3), 
                                           round(anova(meth1_age, meth1_home)[2, "Pr(Chi)"], 3),
                                           nrow(meth1_home$fitted.values)), 
                                names(results_meth1)),
                       setNames(data.frame("Home Ownership (ref=Owned/Mortgaged)", "Other", "NewBel", 
                                           round(exp(coef(meth1_home)["NewBel", "a006Other"]), 2),
                                           round(exp(confint(meth1_home)["a006Other", "2.5 %", 
                                                                        "NewBel"]), 2), 
                                           round(exp(confint(meth1_home)["a006Other", "97.5 %", 
                                                                        "NewBel"]), 2), 
                                           round(p["NewBel", "a006Other"], 3), 
                                           round(anova(meth1_age, meth1_home)[2, "Pr(Chi)"], 3),
                                           nrow(meth1_home$fitted.values)), 
                                names(results_meth1)),
                       setNames(data.frame("Home Ownership (ref=Owned/Mortgaged)", "Rented", "NewNonBel", 
                                           round(exp(coef(meth1_home)["NewNonBel", "a006Rented"]), 2),
                                           round(exp(confint(meth1_home)["a006Rented", "2.5 %", 
                                                                        "NewNonBel"]), 2), 
                                           round(exp(confint(meth1_home)["a006Rented", "97.5 %", 
                                                                        "NewNonBel"]), 2), 
                                           round(p["NewNonBel", "a006Rented"], 3), 
                                           round(anova(meth1_age, meth1_home)[2, "Pr(Chi)"], 3),
                                           nrow(meth1_home$fitted.values)), 
                                names(results_meth1)),
                       setNames(data.frame("Home Ownership (ref=Owned/Mortgaged)", "Council/HA", "NewNonBel", 
                                           round(exp(coef(meth1_home)["NewNonBel", "a006Council/HA"]), 2),
                                           round(exp(confint(meth1_home)["a006Council/HA", "2.5 %", 
                                                                        "NewNonBel"]), 2), 
                                           round(exp(confint(meth1_home)["a006Council/HA", "97.5 %", 
                                                                        "NewNonBel"]), 2), 
                                           round(p["NewNonBel", "a006Council/HA"], 3), 
                                           round(anova(meth1_age, meth1_home)[2, "Pr(Chi)"], 3),
                                           nrow(meth1_home$fitted.values)), 
                                names(results_meth1)),
                       setNames(data.frame("Home Ownership (ref=Owned/Mortgaged)", "Other", "NewNonBel", 
                                           round(exp(coef(meth1_home)["NewNonBel", "a006Other"]), 2),
                                           round(exp(confint(meth1_home)["a006Other", "2.5 %", 
                                                                        "NewNonBel"]), 2), 
                                           round(exp(confint(meth1_home)["a006Other", "97.5 %", 
                                                                        "NewNonBel"]), 2), 
                                           round(p["NewNonBel", "a006Other"], 3), 
                                           round(anova(meth1_age, meth1_home)[2, "Pr(Chi)"], 3),
                                           nrow(meth1_home$fitted.values)), 
                                names(results_meth1))
                       )

results_meth1


## IMD

# Descriptives
table(data$belief_meth1, data$dimd2010q5)
round(prop.table(table(data$belief_meth1, data$dimd2010q5), 1) * 100, 2)

# And make stacked bar-plot of results
(meth1_data_imd <- data %>%
    filter(complete.cases(belief_meth1, dimd2010q5)) %>%
    group_by(belief_meth1, dimd2010q5) %>%
    summarise(n = n()))

(meth1_imd_plot <- ggplot(meth1_data_imd, aes(fill = fct_rev(dimd2010q5), y = n, x = belief_meth1)) +
    geom_bar(position = "fill", stat = "identity") +
    ylab("Proportion of responses") + xlab("Change in belief from pregnancy to age 9") +
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_x_discrete(labels = c("ConsNonBel" = "Consistent non-believer \n(n=2,645)", 
                                "ConsBel" = "Consistent believer \n(n=2,670)",
                                "NewBel" = "New believer \n(n=545)", "NewNonBel" = "New non-believer \n(n=791)")) +
    labs(fill = "IMD Quintile") +
    theme(axis.text = element_text(size = 12), axis.title = element_text(size = 16)))

# Save this plot
pdf("./Results/meth1_IMD_plot.pdf", height = 6, width = 10)
plot(meth1_imd_plot)
dev.off()

# Model
meth1_imd <- multinom(belief_meth1 ~ dimd2010q5 + mz028b, data = data)
summary(meth1_imd)
confint(meth1_imd)

# P-values
z <- summary(meth1_imd)$coefficients/summary(meth1_imd)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

# Relative risk ratios and 95% CIs
exp(coef(meth1_imd))
exp(confint(meth1_imd))

# Compare to null age_only model
meth1_age <- multinom(belief_meth1 ~ mz028b, data = data, subset = !is.na(dimd2010q5))
anova(meth1_age, meth1_imd)

# Add results to results dataframe
results_meth1 <- rbind(results_meth1,
                       setNames(data.frame("IMD (ref=Quin. 1/Least deprived)", "Quintile 2", "ConsBel", 
                                           round(exp(coef(meth1_imd)["ConsBel", "dimd2010q5Quintile 2"]), 2),
                                           round(exp(confint(meth1_imd)["dimd2010q5Quintile 2", "2.5 %", 
                                                                         "ConsBel"]), 2), 
                                           round(exp(confint(meth1_imd)["dimd2010q5Quintile 2", "97.5 %", 
                                                                         "ConsBel"]), 2), 
                                           round(p["ConsBel", "dimd2010q5Quintile 2"], 3), 
                                           round(anova(meth1_age, meth1_imd)[2, "Pr(Chi)"], 3),
                                           nrow(meth1_imd$fitted.values)), 
                                names(results_meth1)),
                       setNames(data.frame("IMD (ref=Quin. 1/Least deprived)", "Quintile 3", "ConsBel", 
                                           round(exp(coef(meth1_imd)["ConsBel", "dimd2010q5Quintile 3"]), 2),
                                           round(exp(confint(meth1_imd)["dimd2010q5Quintile 3", "2.5 %", 
                                                                         "ConsBel"]), 2), 
                                           round(exp(confint(meth1_imd)["dimd2010q5Quintile 3", "97.5 %", 
                                                                         "ConsBel"]), 2), 
                                           round(p["ConsBel", "dimd2010q5Quintile 3"], 3), 
                                           round(anova(meth1_age, meth1_imd)[2, "Pr(Chi)"], 3),
                                           nrow(meth1_imd$fitted.values)), 
                                names(results_meth1)),
                       setNames(data.frame("IMD (ref=Quin. 1/Least deprived)", "Quintile 4", "ConsBel", 
                                           round(exp(coef(meth1_imd)["ConsBel", "dimd2010q5Quintile 4"]), 2),
                                           round(exp(confint(meth1_imd)["dimd2010q5Quintile 4", "2.5 %", 
                                                                         "ConsBel"]), 2), 
                                           round(exp(confint(meth1_imd)["dimd2010q5Quintile 4", "97.5 %", 
                                                                         "ConsBel"]), 2), 
                                           round(p["ConsBel", "dimd2010q5Quintile 4"], 3), 
                                           round(anova(meth1_age, meth1_imd)[2, "Pr(Chi)"], 3),
                                           nrow(meth1_imd$fitted.values)), 
                                names(results_meth1)),
                       setNames(data.frame("IMD (ref=Quin. 1/Least deprived)", "Quin. 5/Most dep.", "ConsBel", 
                                           round(exp(coef(meth1_imd)["ConsBel", "dimd2010q5Quin. 5/Most deprived"]), 2),
                                           round(exp(confint(meth1_imd)["dimd2010q5Quin. 5/Most deprived", "2.5 %", 
                                                                        "ConsBel"]), 2), 
                                           round(exp(confint(meth1_imd)["dimd2010q5Quin. 5/Most deprived", "97.5 %", 
                                                                        "ConsBel"]), 2), 
                                           round(p["ConsBel", "dimd2010q5Quin. 5/Most deprived"], 3), 
                                           round(anova(meth1_age, meth1_imd)[2, "Pr(Chi)"], 3),
                                           nrow(meth1_imd$fitted.values)), 
                                names(results_meth1)),
                       setNames(data.frame("IMD (ref=Quin. 1/Least deprived)", "Quintile 2", "NewBel", 
                                           round(exp(coef(meth1_imd)["NewBel", "dimd2010q5Quintile 2"]), 2),
                                           round(exp(confint(meth1_imd)["dimd2010q5Quintile 2", "2.5 %", 
                                                                         "NewBel"]), 2), 
                                           round(exp(confint(meth1_imd)["dimd2010q5Quintile 2", "97.5 %", 
                                                                         "NewBel"]), 2), 
                                           round(p["NewBel", "dimd2010q5Quintile 2"], 3), 
                                           round(anova(meth1_age, meth1_imd)[2, "Pr(Chi)"], 3),
                                           nrow(meth1_imd$fitted.values)), 
                                names(results_meth1)),
                       setNames(data.frame("IMD (ref=Quin. 1/Least deprived)", "Quintile 3", "NewBel", 
                                           round(exp(coef(meth1_imd)["NewBel", "dimd2010q5Quintile 3"]), 2),
                                           round(exp(confint(meth1_imd)["dimd2010q5Quintile 3", "2.5 %", 
                                                                         "NewBel"]), 2), 
                                           round(exp(confint(meth1_imd)["dimd2010q5Quintile 3", "97.5 %", 
                                                                         "NewBel"]), 2), 
                                           round(p["NewBel", "dimd2010q5Quintile 3"], 3), 
                                           round(anova(meth1_age, meth1_imd)[2, "Pr(Chi)"], 3),
                                           nrow(meth1_imd$fitted.values)), 
                                names(results_meth1)),
                       setNames(data.frame("IMD (ref=Quin. 1/Least deprived)", "Quintile 4", "NewBel", 
                                           round(exp(coef(meth1_imd)["NewBel", "dimd2010q5Quintile 4"]), 2),
                                           round(exp(confint(meth1_imd)["dimd2010q5Quintile 4", "2.5 %", 
                                                                         "NewBel"]), 2), 
                                           round(exp(confint(meth1_imd)["dimd2010q5Quintile 4", "97.5 %", 
                                                                         "NewBel"]), 2), 
                                           round(p["NewBel", "dimd2010q5Quintile 4"], 3), 
                                           round(anova(meth1_age, meth1_imd)[2, "Pr(Chi)"], 3),
                                           nrow(meth1_imd$fitted.values)), 
                                names(results_meth1)),
                       setNames(data.frame("IMD (ref=Quin. 1/Least deprived)", "Quin. 5/Most dep.", "NewBel", 
                                           round(exp(coef(meth1_imd)["NewBel", "dimd2010q5Quin. 5/Most deprived"]), 2),
                                           round(exp(confint(meth1_imd)["dimd2010q5Quin. 5/Most deprived", "2.5 %", 
                                                                        "NewBel"]), 2), 
                                           round(exp(confint(meth1_imd)["dimd2010q5Quin. 5/Most deprived", "97.5 %", 
                                                                        "NewBel"]), 2), 
                                           round(p["NewBel", "dimd2010q5Quin. 5/Most deprived"], 3), 
                                           round(anova(meth1_age, meth1_imd)[2, "Pr(Chi)"], 3),
                                           nrow(meth1_imd$fitted.values)), 
                                names(results_meth1)),
                       setNames(data.frame("IMD (ref=Quin. 1/Least deprived)", "Quintile 2", "NewNonBel", 
                                           round(exp(coef(meth1_imd)["NewNonBel", "dimd2010q5Quintile 2"]), 2),
                                           round(exp(confint(meth1_imd)["dimd2010q5Quintile 2", "2.5 %", 
                                                                         "NewNonBel"]), 2), 
                                           round(exp(confint(meth1_imd)["dimd2010q5Quintile 2", "97.5 %", 
                                                                         "NewNonBel"]), 2), 
                                           round(p["NewNonBel", "dimd2010q5Quintile 2"], 3), 
                                           round(anova(meth1_age, meth1_imd)[2, "Pr(Chi)"], 3),
                                           nrow(meth1_imd$fitted.values)), 
                                names(results_meth1)),
                       setNames(data.frame("IMD (ref=Quin. 1/Least deprived)", "Quintile 3", "NewNonBel", 
                                           round(exp(coef(meth1_imd)["NewNonBel", "dimd2010q5Quintile 3"]), 2),
                                           round(exp(confint(meth1_imd)["dimd2010q5Quintile 3", "2.5 %", 
                                                                         "NewNonBel"]), 2), 
                                           round(exp(confint(meth1_imd)["dimd2010q5Quintile 3", "97.5 %", 
                                                                         "NewNonBel"]), 2), 
                                           round(p["NewNonBel", "dimd2010q5Quintile 3"], 3), 
                                           round(anova(meth1_age, meth1_imd)[2, "Pr(Chi)"], 3),
                                           nrow(meth1_imd$fitted.values)), 
                                names(results_meth1)),
                       setNames(data.frame("IMD (ref=Quin. 1/Least deprived)", "Quintile 4", "NewNonBel", 
                                           round(exp(coef(meth1_imd)["NewNonBel", "dimd2010q5Quintile 4"]), 2),
                                           round(exp(confint(meth1_imd)["dimd2010q5Quintile 4", "2.5 %", 
                                                                         "NewNonBel"]), 2), 
                                           round(exp(confint(meth1_imd)["dimd2010q5Quintile 4", "97.5 %", 
                                                                         "NewNonBel"]), 2), 
                                           round(p["NewNonBel", "dimd2010q5Quintile 4"], 3), 
                                           round(anova(meth1_age, meth1_imd)[2, "Pr(Chi)"], 3),
                                           nrow(meth1_imd$fitted.values)), 
                                names(results_meth1)),
                       setNames(data.frame("IMD (ref=Quin. 1/Least deprived)", "Quin. 5/Most dep.", "NewNonBel", 
                                           round(exp(coef(meth1_imd)["NewNonBel", "dimd2010q5Quin. 5/Most deprived"]), 2),
                                           round(exp(confint(meth1_imd)["dimd2010q5Quin. 5/Most deprived", "2.5 %", 
                                                                        "NewNonBel"]), 2), 
                                           round(exp(confint(meth1_imd)["dimd2010q5Quin. 5/Most deprived", "97.5 %", 
                                                                        "NewNonBel"]), 2), 
                                           round(p["NewNonBel", "dimd2010q5Quin. 5/Most deprived"], 3), 
                                           round(anova(meth1_age, meth1_imd)[2, "Pr(Chi)"], 3),
                                           nrow(meth1_imd$fitted.values)), 
                                names(results_meth1))
)

results_meth1


## First-time mum

# Descriptives
table(data$belief_meth1, data$b032)
round(prop.table(table(data$belief_meth1, data$b032), 1) * 100, 2)

# And make stacked bar-plot of results
(meth1_data_firstMum <- data %>%
    filter(complete.cases(belief_meth1, b032)) %>%
    group_by(belief_meth1, b032) %>%
    summarise(n = n()))

(meth1_firstMum_plot <- ggplot(meth1_data_firstMum, aes(fill = fct_rev(b032), y = n, x = belief_meth1)) +
    geom_bar(position = "fill", stat = "identity") +
    ylab("Proportion of responses") + xlab("Change in belief from pregnancy to age 9") +
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_x_discrete(labels = c("ConsNonBel" = "Consistent non-believer \n(n=2,795)", 
                                "ConsBel" = "Consistent believer \n(n=2,851)",
                                "NewBel" = "New believer \n(n=575)", "NewNonBel" = "New non-believer \n(n=832)")) +
    labs(fill = "First-time Mother") +
    theme(axis.text = element_text(size = 12), axis.title = element_text(size = 16)))

# Save this plot
pdf("./Results/meth1_firstMum_plot.pdf", height = 6, width = 10)
plot(meth1_firstMum_plot)
dev.off()

# Model
meth1_firstMum <- multinom(belief_meth1 ~ b032 + mz028b, data = data)
summary(meth1_firstMum)
confint(meth1_firstMum)

# P-values
z <- summary(meth1_firstMum)$coefficients/summary(meth1_firstMum)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

# Relative risk ratios and 95% CIs
exp(coef(meth1_firstMum))
exp(confint(meth1_firstMum))

# Compare to null age_only model
meth1_age <- multinom(belief_meth1 ~ mz028b, data = data, subset = !is.na(b032))
anova(meth1_age, meth1_firstMum)

# Add results to results dataframe
results_meth1 <- rbind(results_meth1,
                       setNames(data.frame("First-time Mother (ref=No)", "New Mother", "ConsBel", 
                                           round(exp(coef(meth1_firstMum)["ConsBel", "b032NewMum"]), 2),
                                           round(exp(confint(meth1_firstMum)["b032NewMum", "2.5 %", 
                                                                           "ConsBel"]), 2), 
                                           round(exp(confint(meth1_firstMum)["b032NewMum", "97.5 %", 
                                                                           "ConsBel"]), 2), 
                                           round(p["ConsBel", "b032NewMum"], 3), 
                                           round(anova(meth1_age, meth1_firstMum)[2, "Pr(Chi)"], 3),
                                           nrow(meth1_firstMum$fitted.values)), 
                                names(results_meth1)),
                       setNames(data.frame("First-time Mother (ref=No)", "New Mother", "NewBel", 
                                           round(exp(coef(meth1_firstMum)["NewBel", "b032NewMum"]), 2),
                                           round(exp(confint(meth1_firstMum)["b032NewMum", "2.5 %", 
                                                                           "NewBel"]), 2), 
                                           round(exp(confint(meth1_firstMum)["b032NewMum", "97.5 %", 
                                                                           "NewBel"]), 2), 
                                           round(p["NewBel", "b032NewMum"], 3),
                                           round(anova(meth1_age, meth1_firstMum)[2, "Pr(Chi)"], 3),
                                           nrow(meth1_firstMum$fitted.values)), 
                                names(results_meth1)),
                       setNames(data.frame("First-time Mother (ref=No)", "New Mother", "NewNonBel", 
                                           round(exp(coef(meth1_firstMum)["NewNonBel", "b032NewMum"]), 2),
                                           round(exp(confint(meth1_firstMum)["b032NewMum", "2.5 %",
                                                                           "NewNonBel"]), 2), 
                                           round(exp(confint(meth1_firstMum)["b032NewMum", "97.5 %",
                                                                           "NewNonBel"]), 2), 
                                           round(p["NewNonBel", "b032NewMum"], 3),
                                           round(anova(meth1_age, meth1_firstMum)[2, "Pr(Chi)"], 3),
                                           nrow(meth1_firstMum$fitted.values)), 
                                names(results_meth1)))

results_meth1


## Save this results table
write_csv(results_meth1, "./Results/results_meth1.csv")



### Method 2
table(data$belief_meth2, useNA = "ifany")

## Initialise a dataframe to save results to
results_meth2 <- data.frame(exposure = character(), exp_level = character(), outcome_level = character(),
                            RRR = numeric(), lower_ci = numeric(), upper_ci = numeric(), 
                            p = numeric(), p_exp = numeric(), n = numeric())

## Age at birth

# Descriptives
(meth2_data_age <- data %>%
    filter(complete.cases(belief_meth2, mz028b)) %>%
    group_by(belief_meth2) %>%
    summarise(n = n(), mean = mean(mz028b), sd = sd(mz028b), se = sd/sqrt(n),
              lower_ci = mean - (qt(0.975, df = n - 1) * se), upper_ci = mean + (qt(0.975, df = n - 1) * se)))

# Make plot of results
(meth2_age_plot <- ggplot(meth2_data_age) +
    geom_point(aes(x = belief_meth2, y = mean), size = 6, shape = 18) +
    geom_errorbar(aes(x = belief_meth2, ymin = lower_ci, ymax = upper_ci), width = 0.5, size = 1) +
    ylab("Mean age at birth (years)") + xlab("Change in belief from pregnancy to age 9") +
    ylim(26, 30) + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_x_discrete(labels = c("NoChange" = "No change \n(n=5,088)", "SmallInc" = "Small increase \n(n=748)",
                                "BigInc" = "Large increase \n(n=57)", "SmallDec" = "Small decrease \n(n=1,212)",
                                "BigDec" = "Large decrease \n(n=108)")) +
    theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16)))

# Save this plot
pdf("./Results/meth2_age_plot.pdf", height = 6, width = 10)
plot(meth2_age_plot)
dev.off()

# Model
meth2_age <- multinom(belief_meth2 ~ mz028b, data = data)
summary(meth2_age)
confint(meth2_age)

# P-values
z <- summary(meth2_age)$coefficients/summary(meth2_age)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

# Relative risk ratios and 95% CIs
exp(coef(meth2_age))
exp(confint(meth2_age))

# Compare to null model
meth2_null <- multinom(belief_meth2 ~ 1, data = data, subset = !is.na(mz028b))
anova(meth2_null, meth2_age)

# Add results to results dataframe
results_meth2 <- rbind(results_meth2,
                       setNames(data.frame("AgeAtBirth (years)", "NA", "SmallInc", 
                                           round(exp(coef(meth2_age)["SmallInc", "mz028b"]), 2),
                                           round(exp(confint(meth2_age)["mz028b", "2.5 %", "SmallInc"]), 2), 
                                           round(exp(confint(meth2_age)["mz028b", "97.5 %", "SmallInc"]), 2), 
                                           round(p["SmallInc", "mz028b"], 3), 
                                           round(anova(meth2_null, meth2_age)[2, "Pr(Chi)"], 3),
                                           nrow(meth2_age$fitted.values)), 
                                names(results_meth2)),
                       setNames(data.frame("AgeAtBirth (years)", "NA", "BigInc", 
                                           round(exp(coef(meth2_age)["BigInc", "mz028b"]), 2),
                                           round(exp(confint(meth2_age)["mz028b", "2.5 %", "BigInc"]), 2), 
                                           round(exp(confint(meth2_age)["mz028b", "97.5 %", "BigInc"]), 2), 
                                           round(p["BigInc", "mz028b"], 3),
                                           round(anova(meth2_null, meth2_age)[2, "Pr(Chi)"], 3),
                                           nrow(meth2_age$fitted.values)), 
                                names(results_meth2)),
                       setNames(data.frame("AgeAtBirth (years)", "NA", "SmallDec", 
                                           round(exp(coef(meth2_age)["SmallDec", "mz028b"]), 2),
                                           round(exp(confint(meth2_age)["mz028b", "2.5 %", "SmallDec"]), 2), 
                                           round(exp(confint(meth2_age)["mz028b", "97.5 %", "SmallDec"]), 2), 
                                           round(p["SmallDec", "mz028b"], 3),
                                           round(anova(meth2_null, meth2_age)[2, "Pr(Chi)"], 3),
                                           nrow(meth2_age$fitted.values)), 
                                names(results_meth2)),
                       setNames(data.frame("AgeAtBirth (years)", "NA", "BigDec", 
                                           round(exp(coef(meth2_age)["BigDec", "mz028b"]), 2),
                                           round(exp(confint(meth2_age)["mz028b", "2.5 %", "BigDec"]), 2), 
                                           round(exp(confint(meth2_age)["mz028b", "97.5 %", "BigDec"]), 2), 
                                           round(p["BigDec", "mz028b"], 3),
                                           round(anova(meth2_null, meth2_age)[2, "Pr(Chi)"], 3),
                                           nrow(meth2_age$fitted.values)), 
                                names(results_meth2))
                       )

results_meth2


## Ethnicity

# Descriptives
table(data$belief_meth2, data$c800)
round(prop.table(table(data$belief_meth2, data$c800), 1) * 100, 2)

# And make stacked bar-plot of results
(meth2_data_eth <- data %>%
    filter(complete.cases(belief_meth2, c800)) %>%
    group_by(belief_meth2, c800) %>%
    summarise(n = n()))

(meth2_eth_plot <- ggplot(meth2_data_eth, aes(fill = fct_rev(c800), y = n, x = belief_meth2)) +
    geom_bar(position = "fill", stat = "identity") +
    ylab("Proportion of responses") + xlab("Change in belief from pregnancy to age 9") +
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_x_discrete(labels = c("NoChange" = "No change \n(n=4,986)", "SmallInc" = "Small increase \n(n=729)",
                                "BigInc" = "Large increase \n(n=56)", "SmallDec" = "Small decrease \n(n=1,190)",
                                "BigDec" = "Large decrease \n(n=103)")) +
    labs(fill = "Ethnicity") +
    theme(axis.text = element_text(size = 12), axis.title = element_text(size = 16)))

# Save this plot
pdf("./Results/meth2_ethnicity_plot.pdf", height = 6, width = 10)
plot(meth2_eth_plot)
dev.off()

# Model
meth2_ethnic <- multinom(belief_meth2 ~ c800 + mz028b, data = data)
summary(meth2_ethnic)
confint(meth2_ethnic)

# P-values
z <- summary(meth2_ethnic)$coefficients/summary(meth2_ethnic)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

# Relative risk ratios and 95% CIs
exp(coef(meth2_ethnic))
exp(confint(meth2_ethnic))

# Compare to null age_only model
meth2_age <- multinom(belief_meth2 ~ mz028b, data = data, subset = !is.na(c800))
anova(meth2_age, meth2_ethnic)

# Add results to results dataframe
results_meth2 <- rbind(results_meth2,
                       setNames(data.frame("Ethnicity (ref=White)", "Other than White", "SmallInc", 
                                           round(exp(coef(meth2_ethnic)["SmallInc", "c800Other than White"]), 2),
                                           round(exp(confint(meth2_ethnic)["c800Other than White", "2.5 %", 
                                                                           "SmallInc"]), 2), 
                                           round(exp(confint(meth2_ethnic)["c800Other than White", "97.5 %", 
                                                                           "SmallInc"]), 2), 
                                           round(p["SmallInc", "c800Other than White"], 3), 
                                           round(anova(meth2_age, meth2_ethnic)[2, "Pr(Chi)"], 3),
                                           nrow(meth2_ethnic$fitted.values)), 
                                names(results_meth2)),
                       setNames(data.frame("Ethnicity (ref=White)", "Other than White", "BigInc", 
                                           round(exp(coef(meth2_ethnic)["BigInc", "c800Other than White"]), 2),
                                           round(exp(confint(meth2_ethnic)["c800Other than White", "2.5 %", 
                                                                           "BigInc"]), 2), 
                                           round(exp(confint(meth2_ethnic)["c800Other than White", "97.5 %", 
                                                                           "BigInc"]), 2), 
                                           round(p["BigInc", "c800Other than White"], 3),
                                           round(anova(meth2_age, meth2_ethnic)[2, "Pr(Chi)"], 3),
                                           nrow(meth2_ethnic$fitted.values)), 
                                names(results_meth2)),
                       setNames(data.frame("Ethnicity (ref=White)", "Other than White", "SmallDec", 
                                           round(exp(coef(meth2_ethnic)["SmallDec", "c800Other than White"]), 2),
                                           round(exp(confint(meth2_ethnic)["c800Other than White", "2.5 %",
                                                                           "SmallDec"]), 2), 
                                           round(exp(confint(meth2_ethnic)["c800Other than White", "97.5 %",
                                                                           "SmallDec"]), 2), 
                                           round(p["SmallDec", "c800Other than White"], 3),
                                           round(anova(meth2_age, meth2_ethnic)[2, "Pr(Chi)"], 3),
                                           nrow(meth2_ethnic$fitted.values)), 
                                names(results_meth2)),
                       setNames(data.frame("Ethnicity (ref=White)", "Other than White", "BigDec", 
                                           round(exp(coef(meth2_ethnic)["BigDec", "c800Other than White"]), 2),
                                           round(exp(confint(meth2_ethnic)["c800Other than White", "2.5 %",
                                                                           "BigDec"]), 2), 
                                           round(exp(confint(meth2_ethnic)["c800Other than White", "97.5 %",
                                                                           "BigDec"]), 2), 
                                           round(p["BigDec", "c800Other than White"], 3),
                                           round(anova(meth2_age, meth2_ethnic)[2, "Pr(Chi)"], 3),
                                           nrow(meth2_ethnic$fitted.values)), 
                                names(results_meth2))
                       )

results_meth2


## Maternal education

# Descriptives
table(data$belief_meth2, data$c645a)
round(prop.table(table(data$belief_meth2, data$c645a), 1) * 100, 2)

# And make stacked bar-plot of results
(meth2_data_edu <- data %>%
    filter(complete.cases(belief_meth2, c645a)) %>%
    group_by(belief_meth2, c645a) %>%
    summarise(n = n()))

(meth2_edu_plot <- ggplot(meth2_data_edu, aes(fill = fct_rev(c645a), y = n, x = belief_meth2)) +
    geom_bar(position = "fill", stat = "identity") +
    ylab("Proportion of responses") + xlab("Change in belief from pregnancy to age 9") +
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_x_discrete(labels = c("NoChange" = "No change \n(n=5,000)", "SmallInc" = "Small increase \n(n=734)",
                                "BigInc" = "Large increase \n(n=55)", "SmallDec" = "Small decrease \n(n=1,190)",
                                "BigDec" = "Large decrease \n(n=104)")) +
    labs(fill = "Educational attainment") +
    theme(axis.text = element_text(size = 12), axis.title = element_text(size = 16)))

# Save this plot
pdf("./Results/meth2_education_plot.pdf", height = 6, width = 10)
plot(meth2_edu_plot)
dev.off()

# Model
meth2_edu <- multinom(belief_meth2 ~ c645a + mz028b, data = data)
summary(meth2_edu)
confint(meth2_edu)

# P-values
z <- summary(meth2_edu)$coefficients/summary(meth2_edu)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

# Relative risk ratios and 95% CIs
exp(coef(meth2_edu))
exp(confint(meth2_edu))

# Compare to null age_only model
meth2_age <- multinom(belief_meth2 ~ mz028b, data = data, subset = !is.na(c645a))
anova(meth2_age, meth2_edu)

# Add results to results dataframe
results_meth2 <- rbind(results_meth2,
                       setNames(data.frame("Education (ref=CSE/None)", "Vocational", "SmallInc", 
                                           round(exp(coef(meth2_edu)["SmallInc", "c645aVocational"]), 2),
                                           round(exp(confint(meth2_edu)["c645aVocational", "2.5 %", 
                                                                        "SmallInc"]), 2), 
                                           round(exp(confint(meth2_edu)["c645aVocational", "97.5 %", 
                                                                        "SmallInc"]), 2), 
                                           round(p["SmallInc", "c645aVocational"], 3), 
                                           round(anova(meth2_age, meth2_edu)[2, "Pr(Chi)"], 3),
                                           nrow(meth2_edu$fitted.values)), 
                                names(results_meth2)),
                       setNames(data.frame("Education (ref=CSE/None)", "O level", "SmallInc", 
                                           round(exp(coef(meth2_edu)["SmallInc", "c645aO level"]), 2),
                                           round(exp(confint(meth2_edu)["c645aO level", "2.5 %", 
                                                                        "SmallInc"]), 2), 
                                           round(exp(confint(meth2_edu)["c645aO level", "97.5 %", 
                                                                        "SmallInc"]), 2), 
                                           round(p["SmallInc", "c645aO level"], 3), 
                                           round(anova(meth2_age, meth2_edu)[2, "Pr(Chi)"], 3),
                                           nrow(meth2_edu$fitted.values)), 
                                names(results_meth2)),
                       setNames(data.frame("Education (ref=CSE/None)", "A level", "SmallInc", 
                                           round(exp(coef(meth2_edu)["SmallInc", "c645aA level"]), 2),
                                           round(exp(confint(meth2_edu)["c645aA level", "2.5 %", 
                                                                        "SmallInc"]), 2), 
                                           round(exp(confint(meth2_edu)["c645aA level", "97.5 %", 
                                                                        "SmallInc"]), 2), 
                                           round(p["SmallInc", "c645aA level"], 3), 
                                           round(anova(meth2_age, meth2_edu)[2, "Pr(Chi)"], 3),
                                           nrow(meth2_edu$fitted.values)), 
                                names(results_meth2)),
                       setNames(data.frame("Education (ref=CSE/None)", "Degree", "SmallInc", 
                                           round(exp(coef(meth2_edu)["SmallInc", "c645aDegree"]), 2),
                                           round(exp(confint(meth2_edu)["c645aDegree", "2.5 %", 
                                                                        "SmallInc"]), 2), 
                                           round(exp(confint(meth2_edu)["c645aDegree", "97.5 %", 
                                                                        "SmallInc"]), 2), 
                                           round(p["SmallInc", "c645aDegree"], 3), 
                                           round(anova(meth2_age, meth2_edu)[2, "Pr(Chi)"], 3),
                                           nrow(meth2_edu$fitted.values)), 
                                names(results_meth2)),
                       setNames(data.frame("Education (ref=CSE/None)", "Vocational", "BigInc", 
                                           round(exp(coef(meth2_edu)["BigInc", "c645aVocational"]), 2),
                                           round(exp(confint(meth2_edu)["c645aVocational", "2.5 %", 
                                                                        "BigInc"]), 2), 
                                           round(exp(confint(meth2_edu)["c645aVocational", "97.5 %", 
                                                                        "BigInc"]), 2), 
                                           round(p["BigInc", "c645aVocational"], 3), 
                                           round(anova(meth2_age, meth2_edu)[2, "Pr(Chi)"], 3),
                                           nrow(meth2_edu$fitted.values)), 
                                names(results_meth2)),
                       setNames(data.frame("Education (ref=CSE/None)", "O level", "BigInc", 
                                           round(exp(coef(meth2_edu)["BigInc", "c645aO level"]), 2),
                                           round(exp(confint(meth2_edu)["c645aO level", "2.5 %", 
                                                                        "BigInc"]), 2), 
                                           round(exp(confint(meth2_edu)["c645aO level", "97.5 %", 
                                                                        "BigInc"]), 2), 
                                           round(p["BigInc", "c645aO level"], 3), 
                                           round(anova(meth2_age, meth2_edu)[2, "Pr(Chi)"], 3),
                                           nrow(meth2_edu$fitted.values)), 
                                names(results_meth2)),
                       setNames(data.frame("Education (ref=CSE/None)", "A level", "BigInc", 
                                           round(exp(coef(meth2_edu)["BigInc", "c645aA level"]), 2),
                                           round(exp(confint(meth2_edu)["c645aA level", "2.5 %", 
                                                                        "BigInc"]), 2), 
                                           round(exp(confint(meth2_edu)["c645aA level", "97.5 %", 
                                                                        "BigInc"]), 2), 
                                           round(p["BigInc", "c645aA level"], 3), 
                                           round(anova(meth2_age, meth2_edu)[2, "Pr(Chi)"], 3),
                                           nrow(meth2_edu$fitted.values)), 
                                names(results_meth2)),
                       setNames(data.frame("Education (ref=CSE/None)", "Degree", "BigInc", 
                                           round(exp(coef(meth2_edu)["BigInc", "c645aDegree"]), 2),
                                           round(exp(confint(meth2_edu)["c645aDegree", "2.5 %", 
                                                                        "BigInc"]), 2), 
                                           round(exp(confint(meth2_edu)["c645aDegree", "97.5 %", 
                                                                        "BigInc"]), 2), 
                                           round(p["BigInc", "c645aDegree"], 3), 
                                           round(anova(meth2_age, meth2_edu)[2, "Pr(Chi)"], 3),
                                           nrow(meth2_edu$fitted.values)), 
                                names(results_meth2)),
                       setNames(data.frame("Education (ref=CSE/None)", "Vocational", "SmallDec", 
                                           round(exp(coef(meth2_edu)["SmallDec", "c645aVocational"]), 2),
                                           round(exp(confint(meth2_edu)["c645aVocational", "2.5 %", 
                                                                        "SmallDec"]), 2), 
                                           round(exp(confint(meth2_edu)["c645aVocational", "97.5 %", 
                                                                        "SmallDec"]), 2), 
                                           round(p["SmallDec", "c645aVocational"], 3), 
                                           round(anova(meth2_age, meth2_edu)[2, "Pr(Chi)"], 3),
                                           nrow(meth2_edu$fitted.values)), 
                                names(results_meth2)),
                       setNames(data.frame("Education (ref=CSE/None)", "O level", "SmallDec", 
                                           round(exp(coef(meth2_edu)["SmallDec", "c645aO level"]), 2),
                                           round(exp(confint(meth2_edu)["c645aO level", "2.5 %", 
                                                                        "SmallDec"]), 2), 
                                           round(exp(confint(meth2_edu)["c645aO level", "97.5 %", 
                                                                        "SmallDec"]), 2), 
                                           round(p["SmallDec", "c645aO level"], 3), 
                                           round(anova(meth2_age, meth2_edu)[2, "Pr(Chi)"], 3),
                                           nrow(meth2_edu$fitted.values)), 
                                names(results_meth2)),
                       setNames(data.frame("Education (ref=CSE/None)", "A level", "SmallDec", 
                                           round(exp(coef(meth2_edu)["SmallDec", "c645aA level"]), 2),
                                           round(exp(confint(meth2_edu)["c645aA level", "2.5 %", 
                                                                        "SmallDec"]), 2), 
                                           round(exp(confint(meth2_edu)["c645aA level", "97.5 %", 
                                                                        "SmallDec"]), 2), 
                                           round(p["SmallDec", "c645aA level"], 3), 
                                           round(anova(meth2_age, meth2_edu)[2, "Pr(Chi)"], 3),
                                           nrow(meth2_edu$fitted.values)), 
                                names(results_meth2)),
                       setNames(data.frame("Education (ref=CSE/None)", "Degree", "SmallDec", 
                                           round(exp(coef(meth2_edu)["SmallDec", "c645aDegree"]), 2),
                                           round(exp(confint(meth2_edu)["c645aDegree", "2.5 %", 
                                                                        "SmallDec"]), 2), 
                                           round(exp(confint(meth2_edu)["c645aDegree", "97.5 %", 
                                                                        "SmallDec"]), 2), 
                                           round(p["SmallDec", "c645aDegree"], 3), 
                                           round(anova(meth2_age, meth2_edu)[2, "Pr(Chi)"], 3),
                                           nrow(meth2_edu$fitted.values)), 
                                names(results_meth2)),
                       setNames(data.frame("Education (ref=CSE/None)", "Vocational", "BigDec", 
                                           round(exp(coef(meth2_edu)["BigDec", "c645aVocational"]), 2),
                                           round(exp(confint(meth2_edu)["c645aVocational", "2.5 %", 
                                                                        "BigDec"]), 2), 
                                           round(exp(confint(meth2_edu)["c645aVocational", "97.5 %", 
                                                                        "BigDec"]), 2), 
                                           round(p["BigDec", "c645aVocational"], 3), 
                                           round(anova(meth2_age, meth2_edu)[2, "Pr(Chi)"], 3),
                                           nrow(meth2_edu$fitted.values)), 
                                names(results_meth2)),
                       setNames(data.frame("Education (ref=CSE/None)", "O level", "BigDec", 
                                           round(exp(coef(meth2_edu)["BigDec", "c645aO level"]), 2),
                                           round(exp(confint(meth2_edu)["c645aO level", "2.5 %", 
                                                                        "BigDec"]), 2), 
                                           round(exp(confint(meth2_edu)["c645aO level", "97.5 %", 
                                                                        "BigDec"]), 2), 
                                           round(p["BigDec", "c645aO level"], 3), 
                                           round(anova(meth2_age, meth2_edu)[2, "Pr(Chi)"], 3),
                                           nrow(meth2_edu$fitted.values)), 
                                names(results_meth2)),
                       setNames(data.frame("Education (ref=CSE/None)", "A level", "BigDec", 
                                           round(exp(coef(meth2_edu)["BigDec", "c645aA level"]), 2),
                                           round(exp(confint(meth2_edu)["c645aA level", "2.5 %", 
                                                                        "BigDec"]), 2), 
                                           round(exp(confint(meth2_edu)["c645aA level", "97.5 %", 
                                                                        "BigDec"]), 2), 
                                           round(p["BigDec", "c645aA level"], 3), 
                                           round(anova(meth2_age, meth2_edu)[2, "Pr(Chi)"], 3),
                                           nrow(meth2_edu$fitted.values)), 
                                names(results_meth2)),
                       setNames(data.frame("Education (ref=CSE/None)", "Degree", "BigDec", 
                                           round(exp(coef(meth2_edu)["BigDec", "c645aDegree"]), 2),
                                           round(exp(confint(meth2_edu)["c645aDegree", "2.5 %", 
                                                                        "BigDec"]), 2), 
                                           round(exp(confint(meth2_edu)["c645aDegree", "97.5 %", 
                                                                        "BigDec"]), 2), 
                                           round(p["BigDec", "c645aDegree"], 3), 
                                           round(anova(meth2_age, meth2_edu)[2, "Pr(Chi)"], 3),
                                           nrow(meth2_edu$fitted.values)), 
                                names(results_meth2))
                       )

results_meth2


## Household income

# Descriptives
(meth2_data_income <- data %>%
    filter(complete.cases(belief_meth2, logavinceq)) %>%
    group_by(belief_meth2) %>%
    summarise(n = n(), mean = mean(logavinceq), sd = sd(logavinceq), se = sd/sqrt(n),
              lower_ci = mean - (qt(0.975, df = n - 1) * se), upper_ci = mean + (qt(0.975, df = n - 1) * se)))

# Make plot of results
(meth2_income_plot <- ggplot(meth2_data_income) +
    geom_point(aes(x = belief_meth2, y = mean), size = 6, shape = 18) +
    geom_errorbar(aes(x = belief_meth2, ymin = lower_ci, ymax = upper_ci), width = 0.5, size = 1) +
    ylab("Mean weekly household income (log GBP)") + xlab("Change in belief from pregnancy to age 9") +
    ylim(5, 5.5) + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_x_discrete(labels = c("NoChange" = "No change \n(n=4,681)", "SmallInc" = "Small increase \n(n=684)",
                                "BigInc" = "Large increase \n(n=52)", "SmallDec" = "Small decrease \n(n=1,131)",
                                "BigDec" = "Large decrease \n(n=101)")) +
    theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16)))

# Save this plot
pdf("./Results/meth2_income_plot.pdf", height = 6, width = 10)
plot(meth2_income_plot)
dev.off()

# Model
meth2_income <- multinom(belief_meth2 ~ logavinceq + mz028b, data = data)
summary(meth2_income)
confint(meth2_income)

# P-values
z <- summary(meth2_income)$coefficients/summary(meth2_income)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

# Relative risk ratios and 95% CIs
exp(coef(meth2_income))
exp(confint(meth2_income))

# Compare to null age_only model
meth2_age <- multinom(belief_meth2 ~ mz028b, data = data, subset = !is.na(logavinceq))
anova(meth2_age, meth2_income)

# Add results to results dataframe
results_meth2 <- rbind(results_meth2,
                       setNames(data.frame("Income (log GBP)", "NA", "SmallInc", 
                                           round(exp(coef(meth2_income)["SmallInc", "logavinceq"]), 2),
                                           round(exp(confint(meth2_income)["logavinceq", "2.5 %", "SmallInc"]), 2), 
                                           round(exp(confint(meth2_income)["logavinceq", "97.5 %", "SmallInc"]), 2), 
                                           round(p["SmallInc", "logavinceq"], 3), 
                                           round(anova(meth2_age, meth2_income)[2, "Pr(Chi)"], 3),
                                           nrow(meth2_income$fitted.values)), 
                                names(results_meth2)),
                       setNames(data.frame("Income (log GBP)", "NA", "BigInc", 
                                           round(exp(coef(meth2_income)["BigInc", "logavinceq"]), 2),
                                           round(exp(confint(meth2_income)["logavinceq", "2.5 %", "BigInc"]), 2), 
                                           round(exp(confint(meth2_income)["logavinceq", "97.5 %", "BigInc"]), 2), 
                                           round(p["BigInc", "logavinceq"], 3),
                                           round(anova(meth2_age, meth2_income)[2, "Pr(Chi)"], 3),
                                           nrow(meth2_income$fitted.values)), 
                                names(results_meth2)),
                       setNames(data.frame("Income (log GBP)", "NA", "SmallDec", 
                                           round(exp(coef(meth2_income)["SmallDec", "logavinceq"]), 2),
                                           round(exp(confint(meth2_income)["logavinceq", "2.5 %", "SmallDec"]), 2), 
                                           round(exp(confint(meth2_income)["logavinceq", "97.5 %", "SmallDec"]), 2), 
                                           round(p["SmallDec", "logavinceq"], 3),
                                           round(anova(meth2_age, meth2_income)[2, "Pr(Chi)"], 3),
                                           nrow(meth2_income$fitted.values)), 
                                names(results_meth2)),
                       setNames(data.frame("Income (log GBP)", "NA", "BigDec", 
                                           round(exp(coef(meth2_income)["BigDec", "logavinceq"]), 2),
                                           round(exp(confint(meth2_income)["logavinceq", "2.5 %", "BigDec"]), 2), 
                                           round(exp(confint(meth2_income)["logavinceq", "97.5 %", "BigDec"]), 2), 
                                           round(p["BigDec", "logavinceq"], 3),
                                           round(anova(meth2_age, meth2_income)[2, "Pr(Chi)"], 3),
                                           nrow(meth2_income$fitted.values)), 
                                names(results_meth2))
                       )

results_meth2


## Home ownership

# Descriptives
table(data$belief_meth2, data$a006)
round(prop.table(table(data$belief_meth2, data$a006), 1) * 100, 2)

# And make stacked bar-plot of results
(meth2_data_home <- data %>%
    filter(complete.cases(belief_meth2, a006)) %>%
    group_by(belief_meth2, a006) %>%
    summarise(n = n()))

(meth2_home_plot <- ggplot(meth2_data_home, aes(fill = fct_rev(a006), y = n, x = belief_meth2)) +
    geom_bar(position = "fill", stat = "identity") +
    ylab("Proportion of responses") + xlab("Change in belief from pregnancy to age 9") +
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_x_discrete(labels = c("NoChange" = "No change \n(n=5,004)", "SmallInc" = "Small increase \n(n=738)",
                                "BigInc" = "Large increase \n(n=55)", "SmallDec" = "Small decrease \n(n=1,185)",
                                "BigDec" = "Large decrease \n(n=106)")) +
    labs(fill = "Home Ownership Status") +
    theme(axis.text = element_text(size = 12), axis.title = element_text(size = 16)))

# Save this plot
pdf("./Results/meth2_homeOwnership_plot.pdf", height = 6, width = 10)
plot(meth2_home_plot)
dev.off()

# Model
meth2_home <- multinom(belief_meth2 ~ a006 + mz028b, data = data)
summary(meth2_home)
confint(meth2_home)

# P-values
z <- summary(meth2_home)$coefficients/summary(meth2_home)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

# Relative risk ratios and 95% CIs
exp(coef(meth2_home))
exp(confint(meth2_home))

# Compare to null age_only model
meth2_age <- multinom(belief_meth2 ~ mz028b, data = data, subset = !is.na(a006))
anova(meth2_age, meth2_home)

# Add results to results dataframe
results_meth2 <- rbind(results_meth2,
                       setNames(data.frame("Home Ownership (ref=Owned/Mortgaged)", "Rented", "SmallInc", 
                                           round(exp(coef(meth2_home)["SmallInc", "a006Rented"]), 2),
                                           round(exp(confint(meth2_home)["a006Rented", "2.5 %", 
                                                                         "SmallInc"]), 2), 
                                           round(exp(confint(meth2_home)["a006Rented", "97.5 %", 
                                                                         "SmallInc"]), 2), 
                                           round(p["SmallInc", "a006Rented"], 3), 
                                           round(anova(meth2_age, meth2_home)[2, "Pr(Chi)"], 3),
                                           nrow(meth2_home$fitted.values)), 
                                names(results_meth2)),
                       setNames(data.frame("Home Ownership (ref=Owned/Mortgaged)", "Council/HA", "SmallInc", 
                                           round(exp(coef(meth2_home)["SmallInc", "a006Council/HA"]), 2),
                                           round(exp(confint(meth2_home)["a006Council/HA", "2.5 %", 
                                                                         "SmallInc"]), 2), 
                                           round(exp(confint(meth2_home)["a006Council/HA", "97.5 %", 
                                                                         "SmallInc"]), 2), 
                                           round(p["SmallInc", "a006Council/HA"], 3), 
                                           round(anova(meth2_age, meth2_home)[2, "Pr(Chi)"], 3),
                                           nrow(meth2_home$fitted.values)), 
                                names(results_meth2)),
                       setNames(data.frame("Home Ownership (ref=Owned/Mortgaged)", "Other", "SmallInc", 
                                           round(exp(coef(meth2_home)["SmallInc", "a006Other"]), 2),
                                           round(exp(confint(meth2_home)["a006Other", "2.5 %", 
                                                                         "SmallInc"]), 2), 
                                           round(exp(confint(meth2_home)["a006Other", "97.5 %", 
                                                                         "SmallInc"]), 2), 
                                           round(p["SmallInc", "a006Other"], 3), 
                                           round(anova(meth2_age, meth2_home)[2, "Pr(Chi)"], 3),
                                           nrow(meth2_home$fitted.values)), 
                                names(results_meth2)),
                       setNames(data.frame("Home Ownership (ref=Owned/Mortgaged)", "Rented", "BigInc", 
                                           round(exp(coef(meth2_home)["BigInc", "a006Rented"]), 2),
                                           round(exp(confint(meth2_home)["a006Rented", "2.5 %", 
                                                                         "BigInc"]), 2), 
                                           round(exp(confint(meth2_home)["a006Rented", "97.5 %", 
                                                                         "BigInc"]), 2), 
                                           round(p["BigInc", "a006Rented"], 3), 
                                           round(anova(meth2_age, meth2_home)[2, "Pr(Chi)"], 3),
                                           nrow(meth2_home$fitted.values)), 
                                names(results_meth2)),
                       setNames(data.frame("Home Ownership (ref=Owned/Mortgaged)", "Council/HA", "BigInc", 
                                           round(exp(coef(meth2_home)["BigInc", "a006Council/HA"]), 2),
                                           round(exp(confint(meth2_home)["a006Council/HA", "2.5 %", 
                                                                         "BigInc"]), 2), 
                                           round(exp(confint(meth2_home)["a006Council/HA", "97.5 %", 
                                                                         "BigInc"]), 2), 
                                           round(p["BigInc", "a006Council/HA"], 3), 
                                           round(anova(meth2_age, meth2_home)[2, "Pr(Chi)"], 3),
                                           nrow(meth2_home$fitted.values)), 
                                names(results_meth2)),
                       setNames(data.frame("Home Ownership (ref=Owned/Mortgaged)", "Other", "BigInc", 
                                           round(exp(coef(meth2_home)["BigInc", "a006Other"]), 2),
                                           round(exp(confint(meth2_home)["a006Other", "2.5 %", 
                                                                         "BigInc"]), 2), 
                                           round(exp(confint(meth2_home)["a006Other", "97.5 %", 
                                                                         "BigInc"]), 2), 
                                           round(p["BigInc", "a006Other"], 3), 
                                           round(anova(meth2_age, meth2_home)[2, "Pr(Chi)"], 3),
                                           nrow(meth2_home$fitted.values)), 
                                names(results_meth2)),
                       setNames(data.frame("Home Ownership (ref=Owned/Mortgaged)", "Rented", "SmallDec", 
                                           round(exp(coef(meth2_home)["SmallDec", "a006Rented"]), 2),
                                           round(exp(confint(meth2_home)["a006Rented", "2.5 %", 
                                                                         "SmallDec"]), 2), 
                                           round(exp(confint(meth2_home)["a006Rented", "97.5 %", 
                                                                         "SmallDec"]), 2), 
                                           round(p["SmallDec", "a006Rented"], 3), 
                                           round(anova(meth2_age, meth2_home)[2, "Pr(Chi)"], 3),
                                           nrow(meth2_home$fitted.values)), 
                                names(results_meth2)),
                       setNames(data.frame("Home Ownership (ref=Owned/Mortgaged)", "Council/HA", "SmallDec", 
                                           round(exp(coef(meth2_home)["SmallDec", "a006Council/HA"]), 2),
                                           round(exp(confint(meth2_home)["a006Council/HA", "2.5 %", 
                                                                         "SmallDec"]), 2), 
                                           round(exp(confint(meth2_home)["a006Council/HA", "97.5 %", 
                                                                         "SmallDec"]), 2), 
                                           round(p["SmallDec", "a006Council/HA"], 3), 
                                           round(anova(meth2_age, meth2_home)[2, "Pr(Chi)"], 3),
                                           nrow(meth2_home$fitted.values)), 
                                names(results_meth2)),
                       setNames(data.frame("Home Ownership (ref=Owned/Mortgaged)", "Other", "SmallDec", 
                                           round(exp(coef(meth2_home)["SmallDec", "a006Other"]), 2),
                                           round(exp(confint(meth2_home)["a006Other", "2.5 %", 
                                                                         "SmallDec"]), 2), 
                                           round(exp(confint(meth2_home)["a006Other", "97.5 %", 
                                                                         "SmallDec"]), 2), 
                                           round(p["SmallDec", "a006Other"], 3), 
                                           round(anova(meth2_age, meth2_home)[2, "Pr(Chi)"], 3),
                                           nrow(meth2_home$fitted.values)), 
                                names(results_meth2)),
                       setNames(data.frame("Home Ownership (ref=Owned/Mortgaged)", "Rented", "BigDec", 
                                           round(exp(coef(meth2_home)["BigDec", "a006Rented"]), 2),
                                           round(exp(confint(meth2_home)["a006Rented", "2.5 %", 
                                                                         "BigDec"]), 2), 
                                           round(exp(confint(meth2_home)["a006Rented", "97.5 %", 
                                                                         "BigDec"]), 2), 
                                           round(p["BigDec", "a006Rented"], 3), 
                                           round(anova(meth2_age, meth2_home)[2, "Pr(Chi)"], 3),
                                           nrow(meth2_home$fitted.values)), 
                                names(results_meth2)),
                       setNames(data.frame("Home Ownership (ref=Owned/Mortgaged)", "Council/HA", "BigDec", 
                                           round(exp(coef(meth2_home)["BigDec", "a006Council/HA"]), 2),
                                           round(exp(confint(meth2_home)["a006Council/HA", "2.5 %", 
                                                                         "BigDec"]), 2), 
                                           round(exp(confint(meth2_home)["a006Council/HA", "97.5 %", 
                                                                         "BigDec"]), 2), 
                                           round(p["BigDec", "a006Council/HA"], 3), 
                                           round(anova(meth2_age, meth2_home)[2, "Pr(Chi)"], 3),
                                           nrow(meth2_home$fitted.values)), 
                                names(results_meth2)),
                       setNames(data.frame("Home Ownership (ref=Owned/Mortgaged)", "Other", "BigDec", 
                                           round(exp(coef(meth2_home)["BigDec", "a006Other"]), 2),
                                           round(exp(confint(meth2_home)["a006Other", "2.5 %", 
                                                                         "BigDec"]), 2), 
                                           round(exp(confint(meth2_home)["a006Other", "97.5 %", 
                                                                         "BigDec"]), 2), 
                                           round(p["BigDec", "a006Other"], 3), 
                                           round(anova(meth2_age, meth2_home)[2, "Pr(Chi)"], 3),
                                           nrow(meth2_home$fitted.values)), 
                                names(results_meth2))
                       )

results_meth2


## IMD

# Descriptives
table(data$belief_meth2, data$dimd2010q5)
round(prop.table(table(data$belief_meth2, data$dimd2010q5), 1) * 100, 2)

# And make stacked bar-plot of results
(meth2_data_imd <- data %>%
    filter(complete.cases(belief_meth2, dimd2010q5)) %>%
    group_by(belief_meth2, dimd2010q5) %>%
    summarise(n = n()))

(meth2_imd_plot <- ggplot(meth2_data_imd, aes(fill = fct_rev(dimd2010q5), y = n, x = belief_meth2)) +
    geom_bar(position = "fill", stat = "identity") +
    ylab("Proportion of responses") + xlab("Change in belief from pregnancy to age 9") +
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_x_discrete(labels = c("NoChange" = "No change \n(n=4,674)", "SmallInc" = "Small increase \n(n=698)",
                                "BigInc" = "Large increase \n(n=53)", "SmallDec" = "Small decrease \n(n=1,126)",
                                "BigDec" = "Large decrease \n(n=100)")) +
    labs(fill = "IMD Quintile") +
    theme(axis.text = element_text(size = 12), axis.title = element_text(size = 16)))

# Save this plot
pdf("./Results/meth2_IMD_plot.pdf", height = 6, width = 10)
plot(meth2_imd_plot)
dev.off()

# Model
meth2_imd <- multinom(belief_meth2 ~ dimd2010q5 + mz028b, data = data)
summary(meth2_imd)
confint(meth2_imd)

# P-values
z <- summary(meth2_imd)$coefficients/summary(meth2_imd)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

# Relative risk ratios and 95% CIs
exp(coef(meth2_imd))
exp(confint(meth2_imd))

# Compare to null age_only model
meth2_age <- multinom(belief_meth2 ~ mz028b, data = data, subset = !is.na(dimd2010q5))
anova(meth2_age, meth2_imd)

# Add results to results dataframe
results_meth2 <- rbind(results_meth2,
                       setNames(data.frame("IMD (ref=Quin. 1/Least deprived)", "Quintile 2", "SmallInc", 
                                           round(exp(coef(meth2_imd)["SmallInc", "dimd2010q5Quintile 2"]), 2),
                                           round(exp(confint(meth2_imd)["dimd2010q5Quintile 2", "2.5 %", 
                                                                        "SmallInc"]), 2), 
                                           round(exp(confint(meth2_imd)["dimd2010q5Quintile 2", "97.5 %", 
                                                                        "SmallInc"]), 2), 
                                           round(p["SmallInc", "dimd2010q5Quintile 2"], 3), 
                                           round(anova(meth2_age, meth2_imd)[2, "Pr(Chi)"], 3),
                                           nrow(meth2_imd$fitted.values)), 
                                names(results_meth2)),
                       setNames(data.frame("IMD (ref=Quin. 1/Least deprived)", "Quintile 3", "SmallInc", 
                                           round(exp(coef(meth2_imd)["SmallInc", "dimd2010q5Quintile 3"]), 2),
                                           round(exp(confint(meth2_imd)["dimd2010q5Quintile 3", "2.5 %", 
                                                                        "SmallInc"]), 2), 
                                           round(exp(confint(meth2_imd)["dimd2010q5Quintile 3", "97.5 %", 
                                                                        "SmallInc"]), 2), 
                                           round(p["SmallInc", "dimd2010q5Quintile 3"], 3), 
                                           round(anova(meth2_age, meth2_imd)[2, "Pr(Chi)"], 3),
                                           nrow(meth2_imd$fitted.values)), 
                                names(results_meth2)),
                       setNames(data.frame("IMD (ref=Quin. 1/Least deprived)", "Quintile 4", "SmallInc", 
                                           round(exp(coef(meth2_imd)["SmallInc", "dimd2010q5Quintile 4"]), 2),
                                           round(exp(confint(meth2_imd)["dimd2010q5Quintile 4", "2.5 %", 
                                                                        "SmallInc"]), 2), 
                                           round(exp(confint(meth2_imd)["dimd2010q5Quintile 4", "97.5 %", 
                                                                        "SmallInc"]), 2), 
                                           round(p["SmallInc", "dimd2010q5Quintile 4"], 3), 
                                           round(anova(meth2_age, meth2_imd)[2, "Pr(Chi)"], 3),
                                           nrow(meth2_imd$fitted.values)), 
                                names(results_meth2)),
                       setNames(data.frame("IMD (ref=Quin. 1/Least deprived)", "Quin. 5/Most dep.", "SmallInc", 
                                           round(exp(coef(meth2_imd)["SmallInc", "dimd2010q5Quin. 5/Most deprived"]), 2),
                                           round(exp(confint(meth2_imd)["dimd2010q5Quin. 5/Most deprived", "2.5 %", 
                                                                        "SmallInc"]), 2), 
                                           round(exp(confint(meth2_imd)["dimd2010q5Quin. 5/Most deprived", "97.5 %", 
                                                                        "SmallInc"]), 2), 
                                           round(p["SmallInc", "dimd2010q5Quin. 5/Most deprived"], 3), 
                                           round(anova(meth2_age, meth2_imd)[2, "Pr(Chi)"], 3),
                                           nrow(meth2_imd$fitted.values)), 
                                names(results_meth2)),
                       setNames(data.frame("IMD (ref=Quin. 1/Least deprived)", "Quintile 2", "BigInc", 
                                           round(exp(coef(meth2_imd)["BigInc", "dimd2010q5Quintile 2"]), 2),
                                           round(exp(confint(meth2_imd)["dimd2010q5Quintile 2", "2.5 %", 
                                                                        "BigInc"]), 2), 
                                           round(exp(confint(meth2_imd)["dimd2010q5Quintile 2", "97.5 %", 
                                                                        "BigInc"]), 2), 
                                           round(p["BigInc", "dimd2010q5Quintile 2"], 3), 
                                           round(anova(meth2_age, meth2_imd)[2, "Pr(Chi)"], 3),
                                           nrow(meth2_imd$fitted.values)), 
                                names(results_meth2)),
                       setNames(data.frame("IMD (ref=Quin. 1/Least deprived)", "Quintile 3", "BigInc", 
                                           round(exp(coef(meth2_imd)["BigInc", "dimd2010q5Quintile 3"]), 2),
                                           round(exp(confint(meth2_imd)["dimd2010q5Quintile 3", "2.5 %", 
                                                                        "BigInc"]), 2), 
                                           round(exp(confint(meth2_imd)["dimd2010q5Quintile 3", "97.5 %", 
                                                                        "BigInc"]), 2), 
                                           round(p["BigInc", "dimd2010q5Quintile 3"], 3), 
                                           round(anova(meth2_age, meth2_imd)[2, "Pr(Chi)"], 3),
                                           nrow(meth2_imd$fitted.values)), 
                                names(results_meth2)),
                       setNames(data.frame("IMD (ref=Quin. 1/Least deprived)", "Quintile 4", "BigInc", 
                                           round(exp(coef(meth2_imd)["BigInc", "dimd2010q5Quintile 4"]), 2),
                                           round(exp(confint(meth2_imd)["dimd2010q5Quintile 4", "2.5 %", 
                                                                        "BigInc"]), 2), 
                                           round(exp(confint(meth2_imd)["dimd2010q5Quintile 4", "97.5 %", 
                                                                        "BigInc"]), 2), 
                                           round(p["BigInc", "dimd2010q5Quintile 4"], 3), 
                                           round(anova(meth2_age, meth2_imd)[2, "Pr(Chi)"], 3),
                                           nrow(meth2_imd$fitted.values)), 
                                names(results_meth2)),
                       setNames(data.frame("IMD (ref=Quin. 1/Least deprived)", "Quin. 5/Most dep.", "BigInc", 
                                           round(exp(coef(meth2_imd)["BigInc", "dimd2010q5Quin. 5/Most deprived"]), 2),
                                           round(exp(confint(meth2_imd)["dimd2010q5Quin. 5/Most deprived", "2.5 %", 
                                                                        "BigInc"]), 2), 
                                           round(exp(confint(meth2_imd)["dimd2010q5Quin. 5/Most deprived", "97.5 %", 
                                                                        "BigInc"]), 2), 
                                           round(p["BigInc", "dimd2010q5Quin. 5/Most deprived"], 3), 
                                           round(anova(meth2_age, meth2_imd)[2, "Pr(Chi)"], 3),
                                           nrow(meth2_imd$fitted.values)), 
                                names(results_meth2)),
                       setNames(data.frame("IMD (ref=Quin. 1/Least deprived)", "Quintile 2", "SmallDec", 
                                           round(exp(coef(meth2_imd)["SmallDec", "dimd2010q5Quintile 2"]), 2),
                                           round(exp(confint(meth2_imd)["dimd2010q5Quintile 2", "2.5 %", 
                                                                        "SmallDec"]), 2), 
                                           round(exp(confint(meth2_imd)["dimd2010q5Quintile 2", "97.5 %", 
                                                                        "SmallDec"]), 2), 
                                           round(p["SmallDec", "dimd2010q5Quintile 2"], 3), 
                                           round(anova(meth2_age, meth2_imd)[2, "Pr(Chi)"], 3),
                                           nrow(meth2_imd$fitted.values)), 
                                names(results_meth2)),
                       setNames(data.frame("IMD (ref=Quin. 1/Least deprived)", "Quintile 3", "SmallDec", 
                                           round(exp(coef(meth2_imd)["SmallDec", "dimd2010q5Quintile 3"]), 2),
                                           round(exp(confint(meth2_imd)["dimd2010q5Quintile 3", "2.5 %", 
                                                                        "SmallDec"]), 2), 
                                           round(exp(confint(meth2_imd)["dimd2010q5Quintile 3", "97.5 %", 
                                                                        "SmallDec"]), 2), 
                                           round(p["SmallDec", "dimd2010q5Quintile 3"], 3), 
                                           round(anova(meth2_age, meth2_imd)[2, "Pr(Chi)"], 3),
                                           nrow(meth2_imd$fitted.values)), 
                                names(results_meth2)),
                       setNames(data.frame("IMD (ref=Quin. 1/Least deprived)", "Quintile 4", "SmallDec", 
                                           round(exp(coef(meth2_imd)["SmallDec", "dimd2010q5Quintile 4"]), 2),
                                           round(exp(confint(meth2_imd)["dimd2010q5Quintile 4", "2.5 %", 
                                                                        "SmallDec"]), 2), 
                                           round(exp(confint(meth2_imd)["dimd2010q5Quintile 4", "97.5 %", 
                                                                        "SmallDec"]), 2), 
                                           round(p["SmallDec", "dimd2010q5Quintile 4"], 3), 
                                           round(anova(meth2_age, meth2_imd)[2, "Pr(Chi)"], 3),
                                           nrow(meth2_imd$fitted.values)), 
                                names(results_meth2)),
                       setNames(data.frame("IMD (ref=Quin. 1/Least deprived)", "Quin. 5/Most dep.", "SmallDec", 
                                           round(exp(coef(meth2_imd)["SmallDec", "dimd2010q5Quin. 5/Most deprived"]), 2),
                                           round(exp(confint(meth2_imd)["dimd2010q5Quin. 5/Most deprived", "2.5 %", 
                                                                        "SmallDec"]), 2), 
                                           round(exp(confint(meth2_imd)["dimd2010q5Quin. 5/Most deprived", "97.5 %", 
                                                                        "SmallDec"]), 2), 
                                           round(p["SmallDec", "dimd2010q5Quin. 5/Most deprived"], 3), 
                                           round(anova(meth2_age, meth2_imd)[2, "Pr(Chi)"], 3),
                                           nrow(meth2_imd$fitted.values)), 
                                names(results_meth2)),
                       setNames(data.frame("IMD (ref=Quin. 1/Least deprived)", "Quintile 2", "BigDec", 
                                           round(exp(coef(meth2_imd)["BigDec", "dimd2010q5Quintile 2"]), 2),
                                           round(exp(confint(meth2_imd)["dimd2010q5Quintile 2", "2.5 %", 
                                                                        "BigDec"]), 2), 
                                           round(exp(confint(meth2_imd)["dimd2010q5Quintile 2", "97.5 %", 
                                                                        "BigDec"]), 2), 
                                           round(p["BigDec", "dimd2010q5Quintile 2"], 3), 
                                           round(anova(meth2_age, meth2_imd)[2, "Pr(Chi)"], 3),
                                           nrow(meth2_imd$fitted.values)), 
                                names(results_meth2)),
                       setNames(data.frame("IMD (ref=Quin. 1/Least deprived)", "Quintile 3", "BigDec", 
                                           round(exp(coef(meth2_imd)["BigDec", "dimd2010q5Quintile 3"]), 2),
                                           round(exp(confint(meth2_imd)["dimd2010q5Quintile 3", "2.5 %", 
                                                                        "BigDec"]), 2), 
                                           round(exp(confint(meth2_imd)["dimd2010q5Quintile 3", "97.5 %", 
                                                                        "BigDec"]), 2), 
                                           round(p["BigDec", "dimd2010q5Quintile 3"], 3), 
                                           round(anova(meth2_age, meth2_imd)[2, "Pr(Chi)"], 3),
                                           nrow(meth2_imd$fitted.values)), 
                                names(results_meth2)),
                       setNames(data.frame("IMD (ref=Quin. 1/Least deprived)", "Quintile 4", "BigDec", 
                                           round(exp(coef(meth2_imd)["BigDec", "dimd2010q5Quintile 4"]), 2),
                                           round(exp(confint(meth2_imd)["dimd2010q5Quintile 4", "2.5 %", 
                                                                        "BigDec"]), 2), 
                                           round(exp(confint(meth2_imd)["dimd2010q5Quintile 4", "97.5 %", 
                                                                        "BigDec"]), 2), 
                                           round(p["BigDec", "dimd2010q5Quintile 4"], 3), 
                                           round(anova(meth2_age, meth2_imd)[2, "Pr(Chi)"], 3),
                                           nrow(meth2_imd$fitted.values)), 
                                names(results_meth2)),
                       setNames(data.frame("IMD (ref=Quin. 1/Least deprived)", "Quin. 5/Most dep.", "BigDec", 
                                           round(exp(coef(meth2_imd)["BigDec", "dimd2010q5Quin. 5/Most deprived"]), 2),
                                           round(exp(confint(meth2_imd)["dimd2010q5Quin. 5/Most deprived", "2.5 %", 
                                                                        "BigDec"]), 2), 
                                           round(exp(confint(meth2_imd)["dimd2010q5Quin. 5/Most deprived", "97.5 %", 
                                                                        "BigDec"]), 2), 
                                           round(p["BigDec", "dimd2010q5Quin. 5/Most deprived"], 3), 
                                           round(anova(meth2_age, meth2_imd)[2, "Pr(Chi)"], 3),
                                           nrow(meth2_imd$fitted.values)), 
                                names(results_meth2))
                       )

results_meth2


## First-time mum

# Descriptives
table(data$belief_meth2, data$b032)
round(prop.table(table(data$belief_meth2, data$b032), 1) * 100, 2)

# And make stacked bar-plot of results
(meth2_data_firstMum <- data %>%
    filter(complete.cases(belief_meth2, b032)) %>%
    group_by(belief_meth2, b032) %>%
    summarise(n = n()))

(meth2_firstMum_plot <- ggplot(meth2_data_firstMum, aes(fill = fct_rev(b032), y = n, x = belief_meth2)) +
    geom_bar(position = "fill", stat = "identity") +
    ylab("Proportion of responses") + xlab("Change in belief from pregnancy to age 9") +
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_x_discrete(labels = c("NoChange" = "No change \n(n=4,984)", "SmallInc" = "Small increase \n(n=735)",
                                "BigInc" = "Large increase \n(n=55)", "SmallDec" = "Small decrease \n(n=1,172)",
                                "BigDec" = "Large decrease \n(n=107)")) +
    labs(fill = "First-time Mother") +
    theme(axis.text = element_text(size = 12), axis.title = element_text(size = 16)))

# Save this plot
pdf("./Results/meth2_firstMum_plot.pdf", height = 6, width = 10)
plot(meth2_firstMum_plot)
dev.off()

# Model
meth2_firstMum <- multinom(belief_meth2 ~ b032 + mz028b, data = data)
summary(meth2_firstMum)
confint(meth2_firstMum)

# P-values
z <- summary(meth2_firstMum)$coefficients/summary(meth2_firstMum)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

# Relative risk ratios and 95% CIs
exp(coef(meth2_firstMum))
exp(confint(meth2_firstMum))

# Compare to null age_only model
meth2_age <- multinom(belief_meth2 ~ mz028b, data = data, subset = !is.na(b032))
anova(meth2_age, meth2_firstMum)

# Add results to results dataframe
results_meth2 <- rbind(results_meth2,
                       setNames(data.frame("First-time Mother (ref=No)", "New Mother", "SmallInc", 
                                           round(exp(coef(meth2_firstMum)["SmallInc", "b032NewMum"]), 2),
                                           round(exp(confint(meth2_firstMum)["b032NewMum", "2.5 %", 
                                                                             "SmallInc"]), 2), 
                                           round(exp(confint(meth2_firstMum)["b032NewMum", "97.5 %", 
                                                                             "SmallInc"]), 2), 
                                           round(p["SmallInc", "b032NewMum"], 3), 
                                           round(anova(meth2_age, meth2_firstMum)[2, "Pr(Chi)"], 3),
                                           nrow(meth2_firstMum$fitted.values)), 
                                names(results_meth2)),
                       setNames(data.frame("First-time Mother (ref=No)", "New Mother", "BigInc", 
                                           round(exp(coef(meth2_firstMum)["BigInc", "b032NewMum"]), 2),
                                           round(exp(confint(meth2_firstMum)["b032NewMum", "2.5 %", 
                                                                             "BigInc"]), 2), 
                                           round(exp(confint(meth2_firstMum)["b032NewMum", "97.5 %", 
                                                                             "BigInc"]), 2), 
                                           round(p["BigInc", "b032NewMum"], 3),
                                           round(anova(meth2_age, meth2_firstMum)[2, "Pr(Chi)"], 3),
                                           nrow(meth2_firstMum$fitted.values)), 
                                names(results_meth2)),
                       setNames(data.frame("First-time Mother (ref=No)", "New Mother", "SmallDec", 
                                           round(exp(coef(meth2_firstMum)["SmallDec", "b032NewMum"]), 2),
                                           round(exp(confint(meth2_firstMum)["b032NewMum", "2.5 %",
                                                                             "SmallDec"]), 2), 
                                           round(exp(confint(meth2_firstMum)["b032NewMum", "97.5 %",
                                                                             "SmallDec"]), 2), 
                                           round(p["SmallDec", "b032NewMum"], 3),
                                           round(anova(meth2_age, meth2_firstMum)[2, "Pr(Chi)"], 3),
                                           nrow(meth2_firstMum$fitted.values)), 
                                names(results_meth2)),
                       setNames(data.frame("First-time Mother (ref=No)", "New Mother", "BigDec", 
                                           round(exp(coef(meth2_firstMum)["BigDec", "b032NewMum"]), 2),
                                           round(exp(confint(meth2_firstMum)["b032NewMum", "2.5 %",
                                                                             "BigDec"]), 2), 
                                           round(exp(confint(meth2_firstMum)["b032NewMum", "97.5 %",
                                                                             "BigDec"]), 2), 
                                           round(p["BigDec", "b032NewMum"], 3),
                                           round(anova(meth2_age, meth2_firstMum)[2, "Pr(Chi)"], 3),
                                           nrow(meth2_firstMum$fitted.values)), 
                                names(results_meth2))
                       )

results_meth2


## Save this results table
write_csv(results_meth2, "./Results/results_meth2.csv")

