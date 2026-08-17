#average phenotype with updated dataset
library(tidyverse)
library(lme4)
library(lmerTest)
library(patchwork)
library(car)
library(emmeans)
#look at all hybrid nests
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

combined_data <- female_data |> left_join(male_data, by=c("yearAreaBox","year","nestbox","hybridnest","n_birds", "laying_date", "day_real_hatch","hq","habitat_quality")) |> filter(!is.na(ring_nb_m)) |>
  group_by(ring_nb_f) |>
  mutate(n_years = n_distinct(year),
         n_hybrid_years = sum(hybridnest),
         prop_hybrid_years = n_hybrid_years/n_years) 
view(combined_data)



avg_data <- combined_data |> select(ring_nb_f,year, species_f, age_category_corrected_f, hybridnest, ring_nb_m, species_m, tarsus_m, tail_m, wing_m, beak_m, patch_size_m, sum_of_white_on_primaries_m, age_category_corrected_m, mass_m,prop_hybrid_years) |>
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
            n_hybridized = first(n_hybridized),
            prop_hybrid_years = first(prop_hybrid_years),
            stdev_patch_m = sd(patch_size_m, na.rm=TRUE),
            n_males = n()) |>
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

ggplot(subset(avg_data,species_f=="CF"), aes(x=factor(hybridized), y=avg_patch_size_m)) + geom_violin() + theme_minimal()

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

ggplot(subset(avg_data,species_f=="CF"), aes(y=avg_patch_size_m, x=factor(n_hybridized))) + geom_boxplot() + theme_minimal()


##### now do the same but remove all females with only 1 entry ######
avg_data1 <- avg_data |>
  filter(n_males>1)
view(avg_data1)

m1pf_f <- glm(hybridized ~ avg_patch_size_m, data=subset(avg_data1,species_f=="PF"), family=binomial)
summary(m1pf_f)
m2pf_f <- glm(hybridized ~ avg_wing_patch_m, data=subset(avg_data1,species_f=="PF"), family=binomial)
summary(m2pf_f)
m3pf_f <- glm(hybridized ~ avg_tarsus_m, data=subset(avg_data1,species_f=="PF"), family=binomial)
summary(m3pf_f) #
m4pf_f <- glm(hybridized ~ avg_wing_m, data=subset(avg_data1,species_f=="PF"), family=binomial)
summary(m4pf_f)

m1cf_f <- glm(hybridized ~ avg_patch_size_m, data=subset(avg_data1,species_f=="CF"), family=binomial)
summary(m1cf_f)
m2cf_f <- glm(hybridized ~ avg_wing_patch_m, data=subset(avg_data1,species_f=="CF"), family=binomial)
summary(m2cf_f)
m3cf_f <- glm(hybridized ~ avg_tarsus_m, data=subset(avg_data1,species_f=="CF"), family=binomial)
summary(m3cf_f)
m4cf_f <- glm(hybridized ~ avg_wing_m, data=subset(avg_data1,species_f=="CF"), family=binomial)
summary(m4cf_f)

ggplot(subset(avg_data1,species_f=="PF"),aes(x=factor(hybridized),y=avg_tarsus_m)) + geom_violin()

m5pf_f <- glm(n_hybridized ~ avg_patch_size_m, data=subset(avg_data1,species_f=="PF"), family=poisson)
summary(m5pf_f)
m6pf_f <- glm(n_hybridized ~ avg_wing_patch_m, data=subset(avg_data1,species_f=="PF"), family=poisson)
summary(m6pf_f)
m7pf_f <- glm(n_hybridized ~ avg_tarsus_m, data=subset(avg_data1,species_f=="PF"), family=poisson)
summary(m7pf_f)
m8pf_f <- glm(n_hybridized ~ avg_wing_m, data=subset(avg_data1,species_f=="PF"), family=poisson)
summary(m8pf_f)

m5cf_f <- glm(n_hybridized ~ avg_patch_size_m, data=subset(avg_data1,species_f=="CF"), family=poisson)
summary(m5cf_f)
m6cf_f <- glm(n_hybridized ~ avg_wing_patch_m, data=subset(avg_data1,species_f=="CF"), family=poisson)
summary(m6cf_f)
m7cf_f <- glm(n_hybridized ~ avg_tarsus_m, data=subset(avg_data1,species_f=="CF"), family=poisson)
summary(m7cf_f)
m8cf_f <- glm(n_hybridized ~ avg_wing_m, data=subset(avg_data1,species_f=="CF"), family=poisson)
summary(m8cf_f)

ggplot(subset(avg_data1,species_f=="PF"),aes(x=factor(n_hybridized),y=avg_tarsus_m)) + geom_violin()

#extract ring numbers for species PF and hybridized is 1
a <- avg_data1 |>
  filter(species_f=="PF" & hybridized==1) |>
  select(ring_nb_f)

data_pf_1 <- combined_data |>
  filter(ring_nb_f %in% a$ring_nb_f) |>
  select(yearAreaBox,year,ring_nb_f,species_m,tarsus_m)
view(data_pf_1)


#plot per female the each male_tarsus length and color for species
ggplot(data_pf_1, aes(y=tarsus_m, x=year, color=species_m)) + geom_point() + theme_minimal() + #add line for the ring_nb_f
  geom_line(aes(group=ring_nb_f)) 

#average heterospecific for each bird 
data_pf_1 |>
  group_by(ring_nb_f, species_m) |>
  summarize(avg_tarsus_m = mean(tarsus_m, na.rm=TRUE)) |>
  print(n=Inf) |>
  ggplot(aes(y=avg_tarsus_m, x=species_m)) + geom_boxplot() + theme_minimal() + facet_wrap(~ring_nb_f)


#modelling with proportion of hybrid years as response variable
m1pf_prop <- glm(prop_hybrid_years ~ avg_patch_size_m, data=subset(avg_data1,species_f=="PF"), family=poisson)
summary(m1pf_prop)
m2pf_prop <- glm(prop_hybrid_years ~ avg_wing_patch_m, data=subset(avg_data1,species_f=="PF"), family=poisson)
summary(m2pf_prop)
m3pf_prop <- glm(prop_hybrid_years ~ avg_tarsus_m, data=subset(avg_data1,species_f=="PF"), family=poisson)
summary(m3pf_prop)
m4pf_prop <- glm(prop_hybrid_years ~ avg_wing_m, data=subset(avg_data1,species_f=="PF"), family=poisson)
summary(m4pf_prop)

m1cf_prop <- glm(prop_hybrid_years ~ avg_patch_size_m, data=subset(avg_data1,species_f=="CF"), family=poisson)
summary(m1cf_prop)
m2cf_prop <- glm(prop_hybrid_years ~ avg_wing_patch_m, data=subset(avg_data1,species_f=="CF"), family=poisson)
summary(m2cf_prop)
m3cf_prop <- glm(prop_hybrid_years ~ avg_tarsus_m, data=subset(avg_data1,species_f=="CF"), family=poisson)
summary(m3cf_prop)
m4cf_prop <- glm(prop_hybrid_years ~ avg_wing_m, data=subset(avg_data1,species_f=="CF"), family=poisson)
summary(m4cf_prop)
