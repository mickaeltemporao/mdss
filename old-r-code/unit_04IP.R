# Session 3 - Introduction à la visualisation de données

## Chargement de la librarie nécessaire
library(tidyverse)

## Chargement des données de l'ANES
data_url <- "https://github.com/mickaeltemporao/CMT4A-CMSS-TEMPLATE/raw/main/data/ts_2016.rds"
raw_data <- readRDS(url(data_url,"rb"))


## Préparation des données
raw_data %>%
  select(
    V161267,   # age
    V161342,   # gender
    V163003,   # region
    V161361x,  # income
    V161270,   # education
    V161031,   # vote intention
    V161155,   # party identification
    V161126,   # ideology
    V161241,   # religion importance
    V161310x,  # race
  ) %>%
  rename(
    "age"         = "V161267",
    "gender"      = "V161342",
    "region"      = "V163003",
    "income"      = "V161361x",
    "education"   = "V161270",
    "voting_int"  = "V161031",
    "party_id"    = "V161155",
    "ideology"    = "V161126",
    "religion"    = "V161241",
    "race"        = "V161310x",
  ) %>%
  filter(
    age >= 18,
    between(gender, 1, 3),
    between(income, 1, 16),
    education > 0,
    between(voting_int, 1, 4),
    between(party_id, 1, 3),
    between(ideology, 1, 7),
    between(religion, 1, 2),
    between(race, 1, 6),
  ) %>%
  mutate(
    gender     = factor(gender,  labels = c("Male", "Female", "Other")),
    region     = factor(region,  labels = c("Northeast", "Midwest", "South", "West")),
    education  = recode(education,
                        "1:8 = 'Some HS';
                         9 = 'HS Grad';
                         10:12 = 'Some College'


    race       = factor(region,  labels = c("Northeast", "Midwest", "South", "West")),
    voting_int = factor(voting_int, labels = c("H. Clinton", "D. Trump", "G. Johnson", "J. Stein")),
    party_id   = factor(party_id, labels = c("Democrat", "Republican", "Independent")),
    religion   = factor(religion, labels = c("Important", "Not important")),
  ) %>%
                    educat <-
                      recode(as.numeric(V161270),
                        9 = 'HS Grad';
                        10:12 = 'Some College';
                        13 = 'BA';
                        14 = 'Master';
                        15 = 'MD,DDS, etc.';
                        16 = 'PhD';
                        else=NA")

anes2016$HLEducation<-recode(anes2016$V161270, recodes = "15:95=NA;-9=NA;10=NA;1:8='LessHSDipGED'; 9='HSDipGED'; 11:12='Assoc'; 13='Bachlrs'; 14='Mastrs'", as.factor = T)


                    agecat <-
                      recode(as.numeric(V161267),
                                   "17:29 = '18-29';
                                    30:44 = '30-44';
                                    45:65 = '45-65';
                                    64:100 = '65+';
                                    else=NA", as.factor=TRUE)
                    race <- recode(as.numeric(V161310x),
                                   "1 = 'White';
                                   2 = 'Black';
                                   5 = 'Hisp';
                                   3:4 = 'Other/Multiple races';
                                   6='Other/Multiple races'")

## Visualization avancée
ggplot(clean_data, aes(x = ideology, group=voting_int)) +
  geom_bar() +
  facet_wrap(~voting_int) +
  scale_x_discrete(
    limits=c("Extrm. Lib", "", "", "Moderate", "", "", "Extrm. Con.")
  ) +
  labs(
    x = "Ideological Self placement",
    y = "Proportion"
  ) +
  theme_light()

## Introduction à ggplot

## Visualiser la distribution d'une variable

## Visualiser des relations entre variables
