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

avg_data <- combined_data |> select(ring_nb_f,year, species_f, age_category_f, hybridnest, ring_nb_m, species_m, tarsus_m, tail_m, wing_m, beak_m, patch_size_m, sum_of_white_on_primaries_m, age_category_m, mass_m) |>
  group_by(ring_nb_f) |> 
  mutate(n_hybridized = sum(hybridnest)) |>
  filter(hybridnest == 0) |>
  summarize(species_f = first(species_f),
            avg_tarsus_m = mean(tarsus_m, na.rm=TRUE),
            avg_tail_m = mean(tail_m, na.rm=TRUE),
            avg_wing_m = mean(wing_m, na.rm=TRUE),
            avg_beak_m = mean(beak_m, na.rm=TRUE),
            avg_patch_size_m = mean(patch_size_m, na.rm=TRUE),
            avg_wing_patch_m = mean(sum_of_white_on_primaries_m, na.rm=TRUE),
            avg_mass_m = mean(mass_m, na.rm=TRUE),
            n_hybridized = first(n_hybridized)) |>
  mutate(hybridized = ifelse(n_hybridized > 0, 1, 0)) 
view(avg_data)  
str(avg_data)

m1pf <- glm(hybridized ~ avg_patch_size_m, data=subset(avg_data,species_f=="PF"), family=binomial)
summary(m1pf)
m2pf <- glm(hybridized ~ avg_wing_patch_m, data=subset(avg_data,species_f=="PF"), family=binomial)
summary(m2pf)
m3pf <- glm(hybridized ~ avg_tarsus_m, data=subset(avg_data,species_f=="PF"), family=binomial)
summary(m3pf)
m4pf <- glm(hybridized ~ avg_wing_m, data=subset(avg_data,species_f=="PF"), family=binomial)
summary(m4pf)

m1cf <- glm(hybridized ~ avg_patch_size_m, data=subset(avg_data,species_f=="CF"), family=binomial)
summary(m1cf)
m2cf <- glm(hybridized ~ avg_wing_patch_m, data=subset(avg_data,species_f=="CF"), family=binomial)
summary(m2cf)
m3cf <- glm(hybridized ~ avg_tarsus_m, data=subset(avg_data,species_f=="CF"), family=binomial)
summary(m3cf)
m4cf <- glm(hybridized ~ avg_wing_m, data=subset(avg_data,species_f=="CF"), family=binomial)
summary(m4cf)


ggplot(subset(avg_data,species_f=="CF"), aes(x=factor(hybridized), y=avg_patch_size_m)) + geom_boxplot() + theme_minimal()

m5pf <- glm(n_hybridized ~ avg_patch_size_m, data=subset(avg_data,species_f=="PF"), family=poisson)
summary(m5pf)
m6pf <- glm(n_hybridized ~ avg_wing_patch_m, data=subset(avg_data,species_f=="PF"), family=poisson)
summary(m6pf)
m7pf <- glm(n_hybridized ~ avg_tarsus_m, data=subset(avg_data,species_f=="PF"), family=poisson)
summary(m7pf)
m8pf <- glm(n_hybridized ~ avg_wing_m, data=subset(avg_data,species_f=="PF"), family=poisson)
summary(m8pf)

m5cf <- glm(n_hybridized ~ avg_patch_size_m, data=subset(avg_data,species_f=="CF"), family=poisson)
summary(m5cf)
m6cf <- glm(n_hybridized ~ avg_wing_patch_m, data=subset(avg_data,species_f=="CF"), family=poisson)
summary(m6cf)
m7cf <- glm(n_hybridized ~ avg_tarsus_m, data=subset(avg_data,species_f=="CF"), family=poisson)
summary(m7cf)
m8cf <- glm(n_hybridized ~ avg_wing_m, data=subset(avg_data,species_f=="CF"), family=poisson)
summary(m8cf)

