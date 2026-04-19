library(tidyverse)
library(lme4)
library(lmerTest)
library(patchwork)
library(car)
library(emmeans)
#First models

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

hist(combined_data$mass_m,breaks=100)
hist(combined_data$patch_size_m,breaks=100)

#count the number of females that have a hybrid nest and more than 1 entry in a year
combined_data |> 
  group_by(sex_f, ring_nb_f, hybridnest) |>
  summarise(n = n()) |>
  filter(n>1) |> 
  print(n=100) 

female_switch <- female_data %>%
  group_by(ring_nb_f) %>%
  summarise(
    n_years = n_distinct(year),
    hybrid_values = n_distinct(hybridnest, na.rm = TRUE),
    has_both = hybrid_values > 1
  ) %>%
  filter(has_both)

print(female_switch,n=38)

female_only_hybrid <- female_data %>%
  filter(!is.na(hybridnest)) %>%
  group_by(ring_nb_f) %>%
  summarise(
    n_obs = n(),
    n_years = n_distinct(year),
    all_hybrid = all(hybridnest == 1),
    any_nonhybrid = any(hybridnest == 0)
  ) %>%
  filter(all_hybrid)

print(female_only_hybrid,n=155)
 

m1 <- glmer(hybridnest ~ patch_size_m + mass_m + (1|year), data=combined_data, family=binomial)
summary(m1)

m2 <- glmer(hybridnest ~ adj.patch_size_m + mass_m + (1|year), data=combined_data, family=binomial)
summary(m2)

m3 <- glmer(hybridnest ~ adj.patch_size_m + (1|year), data=combined_data, family=binomial)
summary(m3)

m4 <- glmer(hybridnest ~ mass_m + (1|year), data=combined_data, family=binomial)
summary(m4)

m5 <- glmer(hybridnest ~ patch_size_m + (1|year), data=combined_data, family=binomial)
summary(m5)
AIC(m3,m5)

ggplot(combined_data, aes(x=factor(hybridnest),y=adj.patch_size_m)) +
  geom_boxplot() +
  geom_jitter(width=0.2, alpha=0.1,color="red")

ggplot(combined_data, aes(x=factor(hybridnest),y=mass_m)) +
  geom_boxplot() +
  geom_jitter(width=0.2, alpha=0.1,color="red")

ggplot(combined_data, aes(x=factor(hybridnest),y=adj.wing_patch_m)) +
  geom_boxplot() +
  geom_jitter(width=0.2, alpha=0.1,color="red")

#centering and scaling the measurements for all pied flycatcher males and collared flycatcher males seperately
allmale_data <- data_clean |>
  filter(sex=="male") |>
  filter(!is.na(mass),!is.na(patch_size),!is.na(sum_of_white_on_primaries),!is.na(adj.wing_patch),!is.na(adj.patch_size)) |>
  group_by(year,species) |>
  mutate(z_mass = as.numeric(scale(mass)),
         z_patch_size = as.numeric(scale(patch_size)),
         z_wing_patch = as.numeric(scale(sum_of_white_on_primaries)),
         z_adj.wing_patch = as.numeric(scale(adj.wing_patch)),
         z_adj.patch_size = as.numeric(scale(adj.patch_size))) |>
  ungroup() |>
  select(yearAreaBox,ring_nb,z_mass,z_patch_size,z_wing_patch,z_adj.wing_patch,z_adj.patch_size)
view(allmale_data) 

combined_data <- combined_data |> left_join(allmale_data, by=c("yearAreaBox","ring_nb_m"="ring_nb")) |>
  rename(z_mass_m = z_mass, z_patch_size_m = z_patch_size, z_wing_patch_m = z_wing_patch, z_adj.wing_patch_m = z_adj.wing_patch, z_adj.patch_size_m = z_adj.patch_size) 
view(combined_data)

m6 <- glmer(hybridnest ~ z_adj.patch_size_m + z_mass_m + (1|ring_nb_f), data=combined_data, family=binomial)
summary(m6)

m7 <- glmer(hybridnest ~ z_patch_size_m + z_mass_m + (1|year) + (1|ring_nb_f), data=combined_data, family=binomial)
summary(m7)

m8 <- glmer(hybridnest ~ z_wing_patch_m + z_patch_size_m + z_mass_m + (1|ring_nb_f), data=combined_data, family=binomial)
summary(m8)

m9 <- glmer(hybridnest ~ z_adj.wing_patch_m + z_mass_m + z_adj.patch_size_m + (1|ring_nb_f), data=combined_data, family=binomial)
summary(m9)

ggplot(combined_data, aes(x=factor(hybridnest),y=z_patch_size_m)) +
  geom_boxplot() +
  geom_jitter(width=0.2, alpha=0.15,color="red")

ggplot(combined_data, aes(x=factor(hybridnest),y=z_mass_m)) +
  geom_boxplot() +
  geom_jitter(width=0.2, alpha=0.15,color="red")

ggplot(combined_data, aes(x=factor(hybridnest),y=z_wing_patch_m)) +
  geom_boxplot() +
  geom_jitter(width=0.2, alpha=0.15,color="red")

ggplot(combined_data, aes(x=factor(hybridnest),y=z_adj.wing_patch_m)) +
  geom_boxplot() +
  geom_jitter(width=0.2, alpha=0.15,color="red")

ggplot(combined_data, aes(x=factor(hybridnest),y=z_adj.patch_size_m)) +
  geom_boxplot() +
  geom_jitter(width=0.2, alpha=0.15,color="red")

#same plot but color the age_category_m
ggplot(combined_data, aes(x=factor(hybridnest),y=z_patch_size_m,color=factor(age_category_m))) +
  geom_boxplot() +
  geom_jitter(width=0.2, alpha=0.3)

m10 <- glmer(hybridnest ~ z_adj.patch_size_m + z_mass_m + age_category_m + z_wing_patch_m + (1|ring_nb_f), data=combined_data, family=binomial)
summary(m10)
vif(m10)

data_cleany <- data_clean |>
  filter(tarsus<40,
         mass >7,
         sex=="male") 
plot(data_cleany$mass,data_cleany$tarsus)

table(combined_data$age_category_m, combined_data$hybridnest, useNA="ifany")

ggplot(data_cleany, aes(x=mass,y=tarsus)) +
  geom_point(alpha=0.1) +
  facet_wrap(~species)

ggplot(data_cleany, aes(x=mass,y=wing)) +
  geom_point(alpha=0.1) 

view(combined_data)


