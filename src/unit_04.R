# Session 4 - Introduction à la préparation de données
## dplyr et tidyverse

# Chargement des libraries requises
library(tidyverse)

# Chargement des données de l'ANES 2016
data_url <- "https://github.com/mickaeltemporao/CMT4A-CMSS-TEMPLATE/raw/main/data/ts_2016.rds"
raw_data <- readRDS(url(data_url,"rb"))

# Préparation des données

# On commence par sélectionner nos variables
tmp_data <- raw_data %>%
  select(
    V161267,   # age
    V161342,   # gender
    V163003,   # region
    V161031,   # vote intention
    V161155,   # party identification
    V161126,   # ideology
    V162243,   # equal opportunity
    V162113,   # ft black lives matter
  )

# Ensuite on rennome nos variables
tmp_data <- tmp_data %>%
  rename(
    "age"        = "V161267",
    "gender"     = "V161342",
    "region"     = "V163003",
    "voting_int" = "V161031",
    "party_id"   = "V161155",
    "ideology"   = "V161126",
    "equality"   = "V162243",
    "therm_blm"  = "V162113",
  )

# Ensuite le codage peut commencer.
# Mais avant nous devons savoir dans quel état sont nos données...
# ... afin de pouvoir identifier et régler les problèmes éventuels.

# Observer les premières observation
head(tmp_data)

# Résummé des donnés
summary(tmp_data)

# Regardons du coté de l'âge
tmp_data %>% select(age) %>% table()
tmp_data %>% select(age) %>% summary()

# Que dis le codebook?
tmp_data <- tmp_data %>%
  filter(age >= 18)

# Qu'en est-il du genre?
tmp_data %>% select(gender) %>% table()

tmp_data <- tmp_data %>%
  filter(between(gender, 1, 3))

# OU

tmp_data <- tmp_data %>%
  filter(
    gender >= 1,
    gender <= 3
  )

# On regarde ce que nous avons fait
tmp_data %>% select(gender) %>% table()

# L'ordre est IMPORTANT
summary(tmp_data)

# L'ordre est IMPORTANT !!
tmp_data <- tmp_data %>%
  mutate(
    gender = factor(gender,  labels = c("Male", "Female", "Other"))
  )

summary(tmp_data)
tmp_data %>% select(gender) %>% table()


# Regardons les intentions de vote
tmp_data %>% select(voting_int) %>% table()

# On va garder 4 partis
tmp_data <- tmp_data %>%
  filter(between(voting_int, 1, 4)) %>%
  mutate(
    voting_int = factor(voting_int, labels = c("H. Clinton", "D. Trump", "G. Johnson", "J. Stein")),
  )

tmp_data %>% select(voting_int) %>% table()

# Quid de mes autres variables?

tmp_data %>% select(party_id) %>% table()
tmp_data %>% select(ideology) %>% table()
tmp_data %>% select(equality) %>% table()
tmp_data %>% select(therm_blm) %>% table()
tmp_data %>% select(therm_blm) %>% summary()

clean_data <- tmp_data %>%
  filter(
    between(party_id, 1, 3),
    between(ideology, 1, 7),
    between(equality, 1, 5),
    between(therm_blm, 1, 100),
  ) %>%
  mutate(
    region     = factor(region,  labels = c("Northeast", "Midwest", "South", "West")),
    party_id   = factor(party_id, labels = c("Democrat", "Republican", "Independent")),
    equality  = factor(equality, labels = c("1. Strongly Agree", "2. Agree somewhat", "3. Neither agree or disagree", "4. Disagree somewhat", "5. Disagree strongly"), order=TRUE),
  )

# Souvenez-vous, la semaine passé nous avions tout codé en une fois!
# Rien ne nous empêche de visualiser une variable non codé pour nous aider
# mails lorsqu'on intégregra un graphique dans un travail on utilisera
# la version bien codée/préparée.
