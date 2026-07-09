library(tidyverse)
library(lme4)
library(lmerTest)
library(patchwork)
library(car)
library(emmeans)
data <- read.csv("/Users/semmeijer/Downloads/Ecology&Conservation/Flycatcher_Hybridization/Data/database_preferences.csv") |>
  mutate(patch_h = as.numeric(patch_h),
         patch_b = as.numeric(patch_b),
         tail = as.numeric(tail),
         tarsus = as.numeric(tarsus),
         wing = as.numeric(wing),
         mass = as.numeric(mass),
         beak = as.numeric(beak),
         patch_size = patch_h*patch_b) |>
  filter(species == "PF" | species == "CF")

bad_birds <- data |>
  group_by(ring_nb) |>
  summarise(n_sexes = n_distinct(sex)) |>
  filter(n_sexes > 1) |>
  pull(ring_nb)
bad_nests <- data |>
  filter(ring_nb %in% bad_birds) |>
  pull(yearAreaBox) |>
  unique()
bad_nests

data_clean <- data |>
  filter(!yearAreaBox %in% bad_nests) |>
  filter(nestbox != "NANA") |>
  group_by(yearAreaBox) |>
  mutate(hybridnest = ifelse(n_distinct(species) > 1, 1, 0)) |>
  mutate(n_birds=n())|>
  ungroup() |> 
  mutate(
    tarsus = ifelse(tarsus <=2 | tarsus >= 50, NA, tarsus),
    tail   = ifelse(tail >= 100, NA, tail),
    beak   = ifelse(beak >= 22, NA, beak),
    mass   = ifelse(mass <= 4 | mass >= 80, NA, mass),
    wing   = ifelse(wing <= 8, NA, wing),
    sum_of_white_on_primaries = ifelse(sum_of_white_on_primaries >= 200, NA, sum_of_white_on_primaries),
    patch_size = ifelse(patch_size >= 300, NA, patch_size),
    adj.wing_patch = sum_of_white_on_primaries/mass,
    adj.patch_size = patch_size/mass) 

filtered_data <- data_clean |>
  filter(n_birds==2)

female_data <- filtered_data |>
  filter(sex=="female") |>
  rename_with(~ paste0(.x, "_f"), -c(yearAreaBox, year, nestbox, fledge_nb, hybridnest, n_birds))
male_data <- filtered_data |>
  filter(sex=='male') |>
  rename_with(~ paste0(.x, "_m"), -c(yearAreaBox, year, nestbox, fledge_nb, hybridnest, n_birds))

combined_data <- female_data |> left_join(male_data, by=c("yearAreaBox","year","nestbox","fledge_nb","hybridnest","n_birds")) |> filter(!is.na(ring_nb_m))
view(combined_data)

#what are the proportions that hybridize per species in comparison to non hybrid pairs
combined_data |>
  group_by(species_f, hybridnest) |>
  summarise(n = n()) |>
  group_by(species_f) |>
  mutate(prop = n/sum(n))
view(combined_data)

combined_data |> #check which ring_nb_f have more than 1 hybridnest 
  group_by(ring_nb_f) |>
  summarise(n_hybridnests = sum(hybridnest)) |>
  filter(n_hybridnests > 1)

str(combined_data)

