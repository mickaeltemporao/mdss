# Session 3 - Introduction à la visualisation de données

# Chargement des libraries requises
library(tidyverse)

# Chargement des données de l'ANES 2016
data_url <- "https://github.com/mickaeltemporao/CMT4A-CMSS-TEMPLATE/raw/main/data/ts_2016.rds"
raw_data <- readRDS(url(data_url,"rb"))


# Préparation des données
clean_data <- raw_data %>%
  select(
    V161267,   # age
    V161342,   # gender
    V163003,   # region
    V161031,   # vote intention
    V161155,   # party identification
    V161126,   # ideology
    V162209,   # tolerance
  ) %>%
  rename(
    "age"         = "V161267",
    "gender"      = "V161342",
    "region"      = "V163003",
    "voting_int"  = "V161031",
    "party_id"    = "V161155",
    "ideology"    = "V161126",
    "tolerance"   = "V162209",
  ) %>%
  filter(
    age >= 18,
    between(gender, 1, 3),
    between(voting_int, 1, 4),
    between(party_id, 1, 3),
    between(ideology, 1, 7),
    between(tolerance, 1, 5),
  ) %>%
  mutate(
    gender     = factor(gender,  labels = c("Male", "Female", "Other")),
    region     = factor(region,  labels = c("Northeast", "Midwest", "South", "West")),
    voting_int = factor(voting_int, labels = c("H. Clinton", "D. Trump", "G. Johnson", "J. Stein")),
    party_id   = factor(party_id, labels = c("Democrat", "Republican", "Independent")),
    tolerance  = factor(tolerance, labels = c("1. Strongly Agree", "2. Agree somewhat", "3. Neither agree or disagree", "4. Disagree somewhat", "5. Disagree strongly"), order=TRUE),
  )


# Introduction à ggplot
ggplot(clean_data, aes(x=gender)) +
  geom_bar()

## Visualiser la distribution d'une variable

### Les variables continues
#### Hstogramme
ggplot(clean_data, aes(x=age)) +
  geom_histogram()

ggplot(clean_data, aes(x=age)) +
  geom_histogram(binwidth=1)

ggplot(clean_data, aes(x=age)) +
  geom_histogram() +
  geom_vline(
    aes(xintercept=mean(age)),
    color="red",
    linetype="dashed"
  )

### Les variables discrès
#### Graphique à barres
ggplot(clean_data, aes(x=gender)) +
  geom_bar()

#### Essayez de visualiser d'autres variables discretes
"-- créez vos graphique ici! --"


### Les niveaux de mesure
#### Variable nominales
ggplot(clean_data, aes(x=region)) +
  geom_bar()

ggplot(clean_data, aes(x=voting_int)) +
  geom_bar()

#### Variable ordinales
ggplot(clean_data, aes(x=tolerance)) +
  geom_bar()

#### Variable interval/ratio
ggplot(clean_data, aes(x=ideology)) +
  geom_bar()

ggplot(clean_data, aes(x=ideology)) +
  geom_bar() +
  scale_x_discrete(
    limits=c("Extrm. Lib", "", "", "Moderate", "", "", "Extrm. Con.")
  )


## Visualiser des relations entre variables
ggplot(clean_data, aes(x=tolerance, fill=party_id)) +
  geom_bar(position="dodge")


