#repeatability with new dataset
library(tidyverse)
library(lme4)
library(lmerTest)
library(patchwork)
library(car)
library(emmeans)
library(ggeffects)
data <- read.csv("/Users/semmeijer/Downloads/Ecology&Conservation/Flycatcher_Hybridization/Data/database_preferences_updated_08.26.csv") |>
  mutate(patch_h = as.numeric(patch_h),
         patch_b = as.numeric(patch_b),
         tail = as.numeric(tail),
         tarsus = as.numeric(tarsus),
         wing = as.numeric(wing),
         mass = as.numeric(mass),
         beak = as.numeric(beak),
         patch_size = patch_h*patch_b) |>
  filter(species == "PF" | species == "CF") |>
  select(-X,-measurer, -recruitnb_new, -total_mass_d12_new, -mean_mass_d12_new, -fledge_nb_new, -predation, -predation_date, -nb_eggs, -experiment) |> #filter for nestbox with AVI in the nestbox code
  filter(!grepl("AVI", nestbox)) |>
  filter(!early==1) |>
  select(-early)

species_switch <- data |>
  group_by(ring_nb) |>
  summarize(n_species = n_distinct(species)) |>
  filter(n_species > 1) |>
  pull(ring_nb)

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
    patch_size = ifelse(patch_size >= 300, NA, patch_size),
    adj.wing_patch = sum_of_white_on_primaries/mass,
    adj.patch_size = patch_size/mass) 

filtered_data <- data_clean |>
  filter(n_birds==2)

female_data <- filtered_data |>
  filter(sex=="female") |>
  rename_with(~ paste0(.x, "_f"), -c(yearAreaBox, year, nestbox, laying_date, day_real_hatch, hq, habitat_quality, hybridnest, n_birds))
male_data <- filtered_data |>
  filter(sex=='male') |>
  rename_with(~ paste0(.x, "_m"), -c(yearAreaBox, year, nestbox, laying_date, day_real_hatch, hq, habitat_quality, hybridnest, n_birds))

combined_data <- female_data |> left_join(male_data, by=c("yearAreaBox","year","nestbox","hybridnest","n_birds", "laying_date", "day_real_hatch","hq","habitat_quality")) |> filter(!is.na(ring_nb_m))
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

repeat_data <- combined_data |>
  select(yearAreaBox, year, nestbox, ring_nb_f, species_f, hybridnest) |>
  group_by(ring_nb_f) |>
  arrange(year, .by_group = TRUE) |>
  mutate(
    previous_hybrid = lag(cumsum(hybridnest), default = 0),
    previous_hybrid_binary = ifelse(previous_hybrid > 0, 1, 0)
  ) |>
  ungroup()

view(repeat_data)

m1 <- glm(hybridnest ~ previous_hybrid + species_f + factor(year), data = repeat_data, family = binomial)
summary(m1) #should i include random effect for individual? i dont think that works

m2 <- glmer(hybridnest ~ previous_hybrid * species_f + (1|year), data = repeat_data, family = binomial)
summary(m2)

m3 <- glmer(hybridnest ~ previous_hybrid + species_f + (1|year), data = repeat_data, family = binomial)
summary(m3)

m4 <- glmer(hybridnest ~ previous_hybrid_binary + species_f + (1|year), data = repeat_data, family = binomial)
summary(m4)

#calculate probabilities for each category from m3
newdata <- expand.grid(
  previous_hybrid = c(0, 1,2),
  species_f = c("CF", "PF")
)

newdata$predicted_prob <- predict(
  m3,
  newdata = newdata,
  type = "response",
  re.form = NA
)

newdata

pred <- ggpredict(
  m3,
  terms = c("previous_hybrid", "species_f")
)

plot(pred)

#do the same for m4
newdata1 <- expand.grid(
  previous_hybrid_binary = c(0, 1),
  species_f = c("CF", "PF")
)

newdata1$predicted_prob <- predict(
  m4,
  newdata = newdata1,
  type = "response",
  re.form = NA
)

newdata1

pred1 <- ggpredict(
  m4,
  terms = c("previous_hybrid_binary", "species_f")
)

plot(pred1)

###### now do the same but remove all birds with only 1 entry #######
repeat_data1 <- repeat_data |>
  group_by(ring_nb_f) |>
  mutate(n_entries = n()) |>
  ungroup() |>
  filter(n_entries > 1 )
view(repeat_data1)

m1f <- glm(hybridnest ~ previous_hybrid + species_f + factor(year), data = repeat_data1, family = binomial)
summary(m1f)

m2f <- glmer(hybridnest ~ previous_hybrid * species_f + (1|year), data = repeat_data1, family = binomial)
summary(m2f)

m3f <- glmer(hybridnest ~ previous_hybrid + species_f + (1|year), data = repeat_data1, family = binomial)
summary(m3f)

m4f <- glmer(hybridnest ~ previous_hybrid_binary + species_f + (1|year), data = repeat_data1, family = binomial)
summary(m4f)

m5f <- glmer(hybridnest ~ previous_hybrid_binary * species_f + (1|year), data = repeat_data1, family = binomial)
summary(m5f)

m6f <- glmer(hybridnest ~ previous_hybrid * species_f + (1|year) + (1|ring_nb_f), data = repeat_data1, family = binomial)
summary(m6f)

m7f <- glmer(hybridnest ~ previous_hybrid_binary * species_f + (1|year) + (1|ring_nb_f), data = repeat_data1, family = binomial)
summary(m7f)


#calculate probabilities for each category from m3f
newdata <- expand.grid(
  previous_hybrid = c(0, 1,2),
  species_f = c("CF", "PF")
)

newdata$predicted_prob <- predict(
  m3f,
  newdata = newdata,
  type = "response",
  re.form = NA
)

newdata

predf <- ggpredict(
  m3f,
  terms = c("previous_hybrid", "species_f")
)

plot(predf)

#do the same for m4
newdata1 <- expand.grid(
  previous_hybrid_binary = c(0, 1),
  species_f = c("CF", "PF")
)

newdata1$predicted_prob <- predict(
  m4f,
  newdata = newdata1,
  type = "response",
  re.form = NA
)

newdata1

pred1f <- ggpredict(
  m4f,
  terms = c("previous_hybrid_binary", "species_f")
)

plot(pred1f)

#count the number of unique ring_nb per species
repeat_data1 |>
  group_by(species_f) |>
  summarise(n_unique_females = n_distinct(ring_nb_f))


#plotting the raw data 

raw <- repeat_data1 |>
  group_by(species_f, previous_hybrid_binary) |>
  summarise(
    prop = mean(hybridnest),
    n = n(),
    .groups = "drop"
  )

ggplot(raw,
       aes(previous_hybrid_binary, prop,
           colour = species_f,
           group = species_f)) +
  geom_line() 

#plot raw data but not on count instead of proportions
raw_count <- repeat_data1 |>
  group_by(species_f, previous_hybrid_binary) |>
  summarise(
    n_hybrid = sum(hybridnest),
    n_total = n(),
    .groups = "drop"
  )
ggplot(raw_count,
       aes(previous_hybrid_binary, n_hybrid,
           colour = species_f,
           group = species_f)) +
  geom_line()

#now add a plot with the actual datapoints
ggplot(repeat_data1, aes(x = previous_hybrid_binary, y = hybridnest, color = species_f)) +
  geom_jitter(width = 0.1, alpha = 0.5) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 18) +
  labs(x = "Previous Hybrid Nest (0 = No, 1 = Yes)", y = "Hybrid Nest (0 = No, 1 = Yes)", color = "Species") +
  theme_minimal()
