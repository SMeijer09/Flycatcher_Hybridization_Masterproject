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
  filter(n_birds == 2) #filter for all female pied flycatchers and their mate
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
view(female_switch)
#pull ring number from these females and add a column
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

combined_data$switch <- combined_data$ring_nb_f %in% female_switch$ring_nb_f
subdata <- combined_data |>
  filter(switch == TRUE) 
view(subdata)

subdata <- subdata[order(subdata$ring_nb_f, subdata$year), ]             

#plot the number of hybridnests per species_f per year, without hybridnest=0
ggplot(combined_data,aes(x=year, fill=factor(hybridnest))) +
  geom_bar(position = "dodge") +
  facet_wrap(~species_f) +
  labs(fill = "Hybrid Nest") +
  theme_minimal()

ggplot(subset(subdata,species_f=="CF"), aes(
  x = year,
  y = z_mass_m,
  color = factor(hybridnest),
  group = interaction(ring_nb_f, hybridnest)
)) +
  geom_point() +
  geom_line() + facet_wrap(~ring_nb_f) 



