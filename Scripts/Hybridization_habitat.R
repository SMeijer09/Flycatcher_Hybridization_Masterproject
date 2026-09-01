#testing the effect of habitat quality
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

#modelling the effect of habitat quality
model_data <- combined_data |> select(yearAreaBox,year,nestbox, hybridnest,ring_nb_f,species_f,habitat_quality)
view(model_data)

m1 <- glmer(hybridnest ~ habitat_quality + (1|year), data=model_data, family=binomial)
summary(m1)
m2 <- glmer(hybridnest ~ habitat_quality*species_f + (1|year), data=model_data, family=binomial)
summary(m2)

#plot the hybridnest, habitat quality per species
ggplot(model_data, aes(x=habitat_quality, y=hybridnest, color=species_f)) +
  geom_jitter(width=0.1, height=0.1, alpha=0.5) +
  geom_smooth(method="glm", method.args=list(family="binomial"), se=TRUE) +
  labs(x="Habitat Quality", y="Hybrid Nest (0/1)", color="Female Species") +
  theme_minimal()

pred_m2 <- ggpredict(
  m2,
  terms = c("habitat_quality", "species_f")
)

plot(pred_m2) +
  theme_classic() +
  labs(
    x = "Habitat quality",
    y = "Predicted probability of mixed nest",
    color = "Species",
    title = ""
  )

plot_data <- model_data |> #remove NA
  filter(!is.na(habitat_quality)) |> #order habitat quality from bad to average, good, very good
  mutate(habitat_quality = factor(habitat_quality, levels=c("bad", "average", "good", "verygood")))
#now make a similar plot with the raw data with the numbers of hybrid to normal nests in a bar chart
ggplot(plot_data, aes(x=habitat_quality, fill=as.factor(hybridnest))) +
  geom_bar(position="fill") +
  labs(x="Habitat Quality", y="Proportion of Mixed Nests", fill="Mixed Nest") +
  theme_minimal() + facet_wrap(~species_f)
