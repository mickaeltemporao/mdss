# Script R: Session 2

## Dans un script R, tout ce qui suit un hashtag (#) est un commmentaire.
## Les commentaires sont ignorés par la console.
## Utilisez les commentaires pour prendre des notes et documenter votre code!


# Les bases
## Opérateurs arithmétiques
5 + 5
   10        / 2
         2^3
2-2


## Opérateur d'assignation "<-" et types de données
year <- 2020
democratic <- 0.513
conservative <- 0.469
nom_parti_1 <- "Démocrate"
nom_parti_2 <- "Conservateur"


## Opérateurs logiques
conservative > democratic
conservative < democratic
conservative != democratic
conservative == democratic


## Vecteurs
share <- c(0.513, 0.469)
party <- c("Democratic", "Conservative")
leader <- c("Biden", "Trump")
seats_2020 <- c(306, 232)
seats_2016 <- c(227, 304)


## Les fonctions "mot()"
class(year)
length(party)
sum(share)


### Les data.frames!
df <- data.frame(share, party, leader, seats_2016, seats_2020)
head(df)


# Le tidyverse
library(tidyverse)

## Sélectionner des variables
select(df, share)
select(df, party)

clean_data <- select(df, share, party)

## Renomer une variable
rename(df, "pourcentage"  = "share")

## Filtrer les observations
filter(df, party == "Democratic")

## Transformer les variables
mutate(df, pct = share * 100)


## Qu'est-ce que ça donne avec des vrais données?

data_url <- "https://github.com/mickaeltemporao/CMT4A-CMSS-TEMPLATE/raw/main/data/ts_2016.rds"
raw_data <- readRDS(url(data_url,"rb"))


## Enchainer les opérations "%>%"
clean_data <- raw_data %>%
  select(
    V161031,   # vote intention
    V161126,   # ideology
  ) %>%
  rename(
    "voting_int"  = "V161031",
    "ideology"    = "V161126",
  ) %>%
  filter(
    between(voting_int, 1, 4),
    between(ideology, 1, 7),
  ) %>%
  mutate(
    voting_int = factor(voting_int, labels = c("H. Clinton", "D. Trump", "G. Johnson", "J. Stein")),
  )

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
