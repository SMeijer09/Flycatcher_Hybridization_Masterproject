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


##### now do the same but remove all females with only 1 entry, if hybridized is 0 #####
avg_data1 <- avg_data |>
  filter(!(n_hybridized == 0 & n_males == 1))
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

#plot average tarsus distribution
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

#i want to split up the average male phenotypes into before and after hybridization event, make a column in which it mentions if the male is before, after for the combined_data. order the years to ensure that the before and after is right. also add never if the individual never hybridized
combined_data <- combined_data |>
  arrange(ring_nb_f, year) |>
  group_by(ring_nb_f) |>
  mutate(
    first_hybrid_year = ifelse(
      any(hybridnest == 1),
      min(year[hybridnest == 1]),
      NA
    ),
    hybrid_status = case_when(
      is.na(first_hybrid_year) ~ "never",
      year < first_hybrid_year ~ "before",
      year == first_hybrid_year ~ "hybrid",
      year > first_hybrid_year ~ "after"
    )
  ) |>
  ungroup() |>
  select(-first_hybrid_year)

view(combined_data)

#now make an average_data table again with all the average male phenotypic traits, but split the averages in before and after hybridization
average_data_split <- combined_data |>
  group_by(ring_nb_f) |>
  mutate(
    hybridized = ifelse(any(hybridnest == 1), 1, 0)
  ) |>
  ungroup() |>
  group_by(ring_nb_f, species_f, hybrid_status) |>
  summarize(
    avg_tarsus_m = mean(tarsus_m, na.rm = TRUE),
    avg_tail_m = mean(tail_m, na.rm = TRUE),
    avg_wing_m = mean(wing_m, na.rm = TRUE),
    avg_beak_m = mean(beak_m, na.rm = TRUE),
    avg_patch_size_m = mean(patch_size_m, na.rm = TRUE),
    avg_wing_patch_m = mean(sum_of_white_on_primaries_m, na.rm = TRUE),
    avg_mass_m = mean(mass_m, na.rm = TRUE),
    n_years = n(),
    n_hybridized = sum(hybridnest),
    hybridized = first(hybridized)
  ) |>
  ungroup() 
view(average_data_split)

#now redo the previous models
m1s_cf <- glm(hybridized ~ avg_patch_size_m, data=subset(average_data_split,species_f=="CF" & (hybrid_status=="before" | hybrid_status=="never")), family=binomial)
summary(m1s_cf)
m2s_cf <- glm(hybridized ~ avg_wing_patch_m, data=subset(average_data_split,species_f=="CF" & (hybrid_status=="before" | hybrid_status=="never")), family=binomial)
summary(m2s_cf)
m3s_cf <- glm(hybridized ~ avg_tarsus_m, data=subset(average_data_split,species_f=="CF" & (hybrid_status=="before" | hybrid_status=="never")), family=binomial)
summary(m3s_cf)
m4s_cf <- glm(hybridized ~ avg_wing_m, data=subset(average_data_split,species_f=="CF" & (hybrid_status=="before" | hybrid_status=="never")), family=binomial)
summary(m4s_cf)

m1s_pf <- glm(hybridized ~ avg_patch_size_m, data=subset(average_data_split,species_f=="PF" & (hybrid_status=="before" | hybrid_status=="never")), family=binomial)
summary(m1s_pf)
m2s_pf <- glm(hybridized ~ avg_wing_patch_m, data=subset(average_data_split,species_f=="PF" & (hybrid_status=="before" | hybrid_status=="never")), family=binomial)
summary(m2s_pf)
m3s_pf <- glm(hybridized ~ avg_tarsus_m, data=subset(average_data_split,species_f=="PF" & (hybrid_status=="before" | hybrid_status=="never")), family=binomial)
summary(m3s_pf)
m4s_pf <- glm(hybridized ~ avg_wing_m, data=subset(average_data_split,species_f=="PF" & (hybrid_status=="before" | hybrid_status=="never")), family=binomial)

#now lets do it with again only females that have more than 1 entry
average_data_split_filtered <- average_data_split |>
  group_by(ring_nb_f) |>
  filter(!(n_hybridized == 0 & n_years == 1)) |>
  ungroup()
m1sf_cf <- glm(hybridized ~ avg_patch_size_m, data=subset(average_data_split_filtered,species_f=="CF" & (hybrid_status=="before" | hybrid_status=="never")), family=binomial)
summary(m1sf_cf)
m2sf_cf <- glm(hybridized ~ avg_wing_patch_m, data=subset(average_data_split_filtered,species_f=="CF" & (hybrid_status=="before" | hybrid_status=="never")), family=binomial)
summary(m2sf_cf)
m3sf_cf <- glm(hybridized ~ avg_tarsus_m, data=subset(average_data_split_filtered,species_f=="CF" & (hybrid_status=="before" | hybrid_status=="never")), family=binomial)
summary(m3sf_cf)
m4sf_cf <- glm(hybridized ~ avg_wing_m, data=subset(average_data_split_filtered,species_f=="CF" & (hybrid_status=="before" | hybrid_status=="never")), family=binomial)
summary(m4sf_cf)

m1sf_pf <- glm(hybridized ~ avg_patch_size_m, data=subset(average_data_split_filtered,species_f=="PF" & (hybrid_status=="before" | hybrid_status=="never")), family=binomial)
summary(m1sf_pf)
m2sf_pf <- glm(hybridized ~ avg_wing_patch_m, data=subset(average_data_split_filtered,species_f=="PF" & (hybrid_status=="before" | hybrid_status=="never")), family=binomial)
summary(m2sf_pf)
m3sf_pf <- glm(hybridized ~ avg_tarsus_m, data=subset(average_data_split_filtered,species_f=="PF" & (hybrid_status=="before" | hybrid_status=="never")), family=binomial)
summary(m3sf_pf)
m4sf_pf <- glm(hybridized ~ avg_wing_m, data=subset(average_data_split_filtered,species_f=="PF" & (hybrid_status=="before" | hybrid_status=="never")), family=binomial)
summary(m4sf_pf)

#now from combined_data we make new columns for wing patch and patch size where we account for the effect of age on these traits

#make boxplots for each male trait per species per age category
ggplot(combined_data, aes(x=factor(age_category_corrected_m), y=patch_size_m, color=species_m)) + geom_boxplot() + theme_minimal()

ggplot(combined_data, aes(x=factor(age_category_corrected_m), y=sum_of_white_on_primaries_m, color=species_m)) + geom_boxplot() + theme_minimal()

ggplot(combined_data, aes(x=factor(age_category_corrected_m), y=wing_m, color=species_m)) + geom_boxplot() + theme_minimal()

ggplot(combined_data, aes(x=factor(age_category_corrected_m), y=tarsus_m, color=species_m)) + geom_boxplot() + theme_minimal()


#test if patch size is significantly different between age categories
m1 <- lm(wing_m ~ age_category_corrected_m, data=subset(combined_data,species_m=="PF"))
summary(m1)

#now from combined_data we make new columns for all phenotype traits and account for the age effect before taking the averages
combined_data <- combined_data |>
  group_by(species_m) |>
  mutate(
    patch_size_m_age_adj = resid(
      lm(patch_size_m ~ age_category_corrected_m, na.action = na.exclude)
    ),
    wing_patch_m_age_adj = resid(
      lm(sum_of_white_on_primaries_m ~ age_category_corrected_m, na.action = na.exclude)
    ),
    wing_m_age_adj = resid(
      lm(wing_m ~ age_category_corrected_m, na.action = na.exclude)
    ),
    tarsus_m_age_adj = resid(
      lm(tarsus_m ~ age_category_corrected_m, na.action = na.exclude)
    ),
    beak_m_age_adj = resid(
      lm(beak_m ~ age_category_corrected_m, na.action = na.exclude)
    ),
    mass_m_age_adj = resid(
      lm(mass_m ~ age_category_corrected_m, na.action = na.exclude))) |>
  ungroup()
view(combined_data)

avg_data_adj <- combined_data |> select(ring_nb_f,year, species_f, age_category_corrected_f, hybridnest, ring_nb_m, species_m, tarsus_m_age_adj, wing_m_age_adj, beak_m_age_adj, patch_size_m_age_adj, wing_patch_m_age_adj, age_category_corrected_m, mass_m_age_adj,prop_hybrid_years) |>
  group_by(ring_nb_f) |> 
  mutate(n_hybridized = sum(hybridnest)) |>
  filter(hybridnest == 0) |>
  summarize(species_f = first(species_f),
            avg_tarsus_m = mean(tarsus_m_age_adj, na.rm=TRUE),
            avg_wing_m = mean(wing_m_age_adj, na.rm=TRUE),
            avg_beak_m = mean(beak_m_age_adj, na.rm=TRUE),
            avg_patch_size_m = mean(patch_size_m_age_adj, na.rm=TRUE),
            avg_wing_patch_m = mean(wing_patch_m_age_adj, na.rm=TRUE),
            avg_mass_m = mean(mass_m_age_adj, na.rm=TRUE),
            n_hybridized = first(n_hybridized),
            prop_hybrid_years = first(prop_hybrid_years),
            n_males = n()) |>
  mutate(hybridized = ifelse(n_hybridized > 0, 1, 0)) 

m1pf_adj <- glm(hybridized ~ avg_patch_size_m, data=subset(avg_data_adj,species_f=="PF"), family=binomial)  
summary(m1pf_adj)
m2pf_adj <- glm(hybridized ~ avg_wing_patch_m, data=subset(avg_data_adj,species_f=="PF"), family=binomial)
summary(m2pf_adj)
m3pf_adj <- glm(hybridized ~ avg_tarsus_m, data=subset(avg_data_adj,species_f=="PF"), family=binomial)
summary(m3pf_adj)
m4pf_adj <- glm(hybridized ~ avg_wing_m, data=subset(avg_data_adj,species_f=="PF"), family=binomial)
summary(m4pf_adj)
m5pf_adj <- glm(hybridized ~ avg_beak_m, data=subset(avg_data_adj,species_f=="PF"), family=binomial)
summary(m5pf_adj)
m6pf_adj <- glm(hybridized ~ avg_mass_m, data=subset(avg_data_adj,species_f=="PF"), family=binomial)
summary(m6pf_adj)

m1cf_adj <- glm(hybridized ~ avg_patch_size_m, data=subset(avg_data_adj,species_f=="CF"), family=binomial)
summary(m1cf_adj)
m2cf_adj <- glm(hybridized ~ avg_wing_patch_m, data=subset(avg_data_adj,species_f=="CF"), family=binomial)
summary(m2cf_adj)
m3cf_adj <- glm(hybridized ~ avg_tarsus_m, data=subset(avg_data_adj,species_f=="CF"), family=binomial)
summary(m3cf_adj)
m4cf_adj <- glm(hybridized ~ avg_wing_m, data=subset(avg_data_adj,species_f=="CF"), family=binomial)
summary(m4cf_adj)
m5cf_adj <- glm(hybridized ~ avg_beak_m, data=subset(avg_data_adj,species_f=="CF"), family=binomial)
summary(m5cf_adj)
m6cf_adj <- glm(hybridized ~ avg_mass_m, data=subset(avg_data_adj,species_f=="CF"), family=binomial)
summary(m6cf_adj)

avg_data1_adj <- avg_data_adj |>
  filter(!(n_hybridized == 0 & n_males == 1))

m1pf_adj_f <- glm(hybridized ~ avg_patch_size_m, data=subset(avg_data1_adj,species_f=="PF"), family=binomial)
summary(m1pf_adj_f)
m2pf_adj_f <- glm(hybridized ~ avg_wing_patch_m, data=subset(avg_data1_adj,species_f=="PF"), family=binomial)
summary(m2pf_adj_f)
m3pf_adj_f <- glm(hybridized ~ avg_tarsus_m, data=subset(avg_data1_adj,species_f=="PF"), family=binomial)
summary(m3pf_adj_f)
m4pf_adj_f <- glm(hybridized ~ avg_wing_m, data=subset(avg_data1_adj,species_f=="PF"), family=binomial)
summary(m4pf_adj_f)
m5pf_adj_f <- glm(hybridized ~ avg_beak_m, data=subset(avg_data1_adj,species_f=="PF"), family=binomial)
summary(m5pf_adj_f)
m6pf_adj_f <- glm(hybridized ~ avg_mass_m, data=subset(avg_data1_adj,species_f=="PF"), family=binomial)
summary(m6pf_adj_f)

m1cf_adj_f <- glm(hybridized ~ avg_patch_size_m, data=subset(avg_data1_adj,species_f=="CF"), family=binomial)
summary(m1cf_adj_f)
m2cf_adj_f <- glm(hybridized ~ avg_wing_patch_m, data=subset(avg_data1_adj,species_f=="CF"), family=binomial)
summary(m2cf_adj_f)
m3cf_adj_f <- glm(hybridized ~ avg_tarsus_m, data=subset(avg_data1_adj,species_f=="CF"), family=binomial)
summary(m3cf_adj_f)
m4cf_adj_f <- glm(hybridized ~ avg_wing_m, data=subset(avg_data1_adj,species_f=="CF"), family=binomial)
summary(m4cf_adj_f)
m5cf_adj_f <- glm(hybridized ~ avg_beak_m, data=subset(avg_data1_adj,species_f=="CF"), family=binomial)
summary(m5cf_adj_f)
m6cf_adj_f <- glm(hybridized ~ avg_mass_m, data=subset(avg_data1_adj,species_f=="CF"), family=binomial)
summary(m6cf_adj_f)
