library(tidyverse)
library(lme4)
library(lmerTest)
library(patchwork)
library(car)
library(emmeans)
#testing females that have both hybrid and non-hybrid entries
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
view(data_clean)

filtered_data <- data_clean |>
  filter(n_birds == 2) |> #filter for all female pied flycatchers and their mate
  group_by(yearAreaBox) |>
  filter(any(sex=="female" & species =="PF"))
view(filtered_data)

female_data <- filtered_data |>
  filter(sex=="female") |>
  rename_with(~ paste0(.x, "_f"), -c(yearAreaBox, year, nestbox, fledge_nb, hybridnest, n_birds))
male_data <- filtered_data |>
  filter(sex=='male') |>
  rename_with(~ paste0(.x, "_m"), -c(yearAreaBox, year, nestbox, fledge_nb, hybridnest, n_birds))

#now i need to have the female ringnb, nestbox, year, and then the male metrics and species
combined_data <- female_data |> left_join(male_data, by=c("yearAreaBox","year","nestbox","fledge_nb","hybridnest","n_birds"))
view(combined_data)


female_switch <- female_data %>%
  group_by(ring_nb_f) %>%
  summarise(
    n_years = n_distinct(year),
    hybrid_values = n_distinct(hybridnest, na.rm = TRUE),
    has_both = hybrid_values > 1
  ) %>%
  filter(has_both)

#pull ring number from these females and add a column

combined_data$switch <- combined_data$ring_nb_f %in% female_switch$ring_nb_f
subdata <- combined_data |>
  filter(switch == TRUE) 
view(subdata)

#make boxplots for each ring_nb and their male patch size
ggplot(subdata, aes(x=factor(hybridnest),y=sum_of_white_on_primaries_m)) +
  geom_point() + facet_wrap(~ring_nb_f) 
