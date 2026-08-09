#phenotypes over the years
library(tidyverse)
library(lme4)
library(lmerTest)
library(patchwork)
library(car)
library(emmeans)
#look at all hybrid nests
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

#check combined_data for female birds that switch species
species_switch <- data |>
  group_by(ring_nb) |>
  summarize(n_species = n_distinct(species)) |>
  filter(n_species > 1) |>
  pull(ring_nb)

#check for birds that switch sex
bad_birds <- data |>
  group_by(ring_nb) |>
  summarise(n_sexes = n_distinct(sex)) |>
  filter(n_sexes > 1) |>
  pull(ring_nb)
bad_nests <- data |>
  filter(ring_nb %in% bad_birds | ring_nb %in% species_switch) |>
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
    patch_size = ifelse(patch_size >= 300, NA, patch_size)) 

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

#plot the male phenotypes in a violin plot over the years
ggplot(combined_data, aes(x=as.factor(year), y=tarsus_m, fill=species_m)) +
  geom_violin() +
  geom_boxplot(width=0.1, fill="white") +
  theme_classic() +
  labs(x="Year", y="Beak length (mm)", fill="Species") +
  scale_fill_manual(values=c("orange", "blue")) +
  theme(legend.position = "top")

ggplot(combined_data, aes(x=as.factor(year), y=patch_size_m, fill=species_m)) +
  geom_violin() +
  geom_boxplot(width=0.1, fill="white") +
  theme_classic() +
  labs(x="Year", y="Forehead patch size (mm^2)", fill="Species") +
  scale_fill_manual(values=c("orange", "blue")) +
  theme(legend.position = "top")

ggplot(combined_data, aes(x=as.factor(year), y=sum_of_white_on_primaries_m, fill=species_m)) +
  geom_violin() +
  geom_boxplot(width=0.1, fill="white") +
  theme_classic() +
  labs(x="Year", y="Cumulative Wingpatch (mm)", fill="Species") +
  scale_fill_manual(values=c("orange", "blue")) +
  theme(legend.position = "top")

ggplot(combined_data, aes(x=as.factor(year), y=wing_m, fill=species_m)) +
  geom_violin() +
  geom_boxplot(width=0.1, fill="white") +
  theme_classic() +
  labs(x="Year", y="Wing length (mm)", fill="Species") +
  scale_fill_manual(values=c("orange", "blue")) +
  theme(legend.position = "top")

#plot the median for each year and species
medians <- combined_data |>
  group_by(year, species_m) |>
  summarise(median_tarsus = median(tarsus_m, na.rm=TRUE),
            median_patch_size = median(patch_size_m, na.rm=TRUE),
            median_sum_of_white_on_primaries = median(sum_of_white_on_primaries_m, na.rm=TRUE),
            median_wing = median(wing_m, na.rm=TRUE))
ggplot(medians, aes(x=as.factor(year), y=median_tarsus, color=species_m, group=species_m)) +
  geom_line() +
  geom_point() +
  theme_classic() +
  labs(x="Year", y="Median tarsus length (mm)", color="Species") +
  scale_color_manual(values=c("orange", "blue")) +
  theme(legend.position = "top")

ggplot(medians, aes(x=as.factor(year), y=median_patch_size, color=species_m, group=species_m)) +
  geom_line() +
  geom_point() +
  theme_classic() +
  labs(x="Year", y="Median forehead patch size (mm^2)", color="Species") +
  scale_color_manual(values=c("orange", "blue")) +
  theme(legend.position = "top")

ggplot(medians, aes(x=as.factor(year), y=median_sum_of_white_on_primaries, color=species_m, group=species_m)) +
  geom_line() +
  geom_point() +
  theme_classic() +
  labs(x="Year", y="Median cumulative wingpatch (mm)", color="Species") +
  scale_color_manual(values=c("orange", "blue")) +
  theme(legend.position = "top")

ggplot(medians, aes(x=as.factor(year), y=median_wing, color=species_m, group=species_m)) +
  geom_line() +
  geom_point() +
  theme_classic() +
  labs(x="Year", y="Median wing length (mm)", color="Species") +
  scale_color_manual(values=c("orange", "blue")) +
  theme(legend.position = "top")
