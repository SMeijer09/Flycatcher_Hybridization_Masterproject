#some simple demographics
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

#plot the number of pure nests over time and include hybrids, seperate species
ggplot(combined_data, aes(x = year)) +
  
  # Pure nests
  geom_bar(
    data = combined_data |> filter(hybridnest == 0),
    aes(fill = species_f),
    position = "dodge"
  ) +
  
  # Hybrid nests
  geom_bar(
    data = combined_data |> filter(hybridnest == 1),
    aes(fill = "Mixed Nest"),
    position = "dodge",
    color = "black",
    alpha = 0.5
  ) +
  
  scale_fill_manual(
    name = "Nest type / Species",
    values = c(
      "PF" = "#1f77b4",
      "CF" = "#ff7f0e",
      "Mixed Nest" = "grey"
    ),
    labels = c(
      "PF" = "Pied Flycatcher",
      "CF" = "Collared Flycatcher",
      "Mixed Nest" = "Mixed Nest"
    )
  ) +
  
  labs(
    x = "Year",
    y = "Count of Nests"
  ) +
  
  scale_x_continuous(
    breaks = seq(min(combined_data$year),
                 max(combined_data$year), by = 1)
  ) +
  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  theme(legend.position = "bottom") 

combined_data |>
  filter(!is.na(age_category_corrected_f)) |>
  ggplot(aes(x = factor(age_category_corrected_f), fill = species_f)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(
    name = "Species",
    values = c(
      "PF" = "#1f77b4",
      "CF" = "#ff7f0e"
    ),
    labels = c(
      "PF" = "Pied Flycatcher",
      "CF" = "Collared Flycatcher"
    )
  ) +
  labs(
    x = "Age category",
    y = "Count"
  ) +
  theme_minimal()

combined_data |>
  filter(!is.na(age_category_corrected_m)) |>
  ggplot(aes(x = factor(age_category_corrected_m), fill = species_m)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(
    name = "Species",
    values = c(
      "PF" = "#1f77b4",
      "CF" = "#ff7f0e"
    ),
    labels = c(
      "PF" = "Pied Flycatcher",
      "CF" = "Collared Flycatcher"
    )
  ) +
  labs(
    x = "Age category",
    y = "Count"
  ) +
  theme_minimal()


age_data <- bind_rows(
  combined_data |>
    filter(!is.na(age_category_corrected_f)) |>
    transmute(
      age = age_category_corrected_f,
      species = species_f,
      sex = "Female"
    ),
  
  combined_data |>
    filter(!is.na(age_category_corrected_m)) |>
    transmute(
      age = age_category_corrected_m,
      species = species_m,
      sex = "Male"
    )
)

ggplot(age_data, aes(x = factor(age), fill = species)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(
    name = "Species",
    values = c(
      "PF" = "#1f77b4",
      "CF" = "#ff7f0e"
    ),
    labels = c(
      "PF" = "Pied Flycatcher",
      "CF" = "Collared Flycatcher"
    )
  ) +
  labs(
    x = "Age category",
    y = "Number of Observations"
  ) +
  theme_minimal()

#rename age categories to 1 juvenile 2 adult
age_data <- age_data |>
  mutate(age = case_when(
    age == 1 ~ "Juvenile",
    age == 2 ~ "Adult",
    TRUE ~ as.character(age)
  ))

ggplot(age_data, aes(x = species, fill = age)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(
    name = "Age category",
    values = c(
      "Juvenile" = "#66c2a5",
      "Adult" = "#fc8d62"
    )
  ) +
  scale_x_discrete(
    labels = c(
      "PF" = "Pied Flycatcher",
      "CF" = "Collared Flycatcher"
    )
  ) +
  labs(
    x = "Species",
    y = "Number of Observations"
  ) +
  theme_minimal() +
  scale_y_continuous(breaks=seq(0, 7000, by=1000)) 


#plot habitat quality on the x-axis and the number of nests on the y-axis, with different colors for pure nests per species. also remove NAs. Good but stack the pure and mixed pair nests
#and order from bad, average to good verygood
ggplot(combined_data |> filter(!is.na(habitat_quality)), aes(x = factor(habitat_quality, levels = c("bad", "average", "good", "verygood")), fill = species_f)) +
  geom_bar(data = combined_data |> filter(!is.na(habitat_quality) & hybridnest == 0), position = "dodge") +
  geom_bar(data = combined_data |> filter(!is.na(habitat_quality) & hybridnest == 1), aes(fill = "Mixed Nest"), position = "dodge", color = "black", alpha = 0.5) +
  scale_fill_manual(
    name = "Nest type / Species",
    values = c(
      "PF" = "#1f77b4",
      "CF" = "#ff7f0e",
      "Mixed Nest" = "grey"
    ),
    labels = c(
      "PF" = "Pied Flycatcher",
      "CF" = "Collared Flycatcher",
      "Mixed Nest" = "Mixed Nest"
    )
  ) +
  labs(
    x = "Habitat Quality",
    y = "Number of Nests"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

#make a plot of the mixed pairs, and with the species of the female over the years
hybrid_counts <- combined_data |>
  filter(hybridnest == 1) |>
  count(year, species_f)

ggplot(hybrid_counts, aes(x = year, y = n, color = species_f, group = species_f)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_color_manual(
    name = "Female species",
    values = c(
      "PF" = "#1f77b4",
      "CF" = "#ff7f0e"
    ),
    labels = c(
      "PF" = "Pied Flycatcher",
      "CF" = "Collared Flycatcher"
    )
  ) +
  scale_x_continuous(
    breaks = seq(min(hybrid_counts$year),
                 max(hybrid_counts$year), by = 1) 
  ) +
  scale_y_continuous(
    breaks = seq(0, max(hybrid_counts$n), by = 1)
  ) +
  labs(
    x = "Year",
    y = "Number of Mixed Nests"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) 


#test if years with less collared nests have more pied nests
nest_data <- combined_data %>%
  mutate(
    nest_type = case_when(
      hybridnest == 0 & species_f == "CF" ~ "CF pure",
      hybridnest == 0 & species_f == "PF" ~ "PF pure",
      hybridnest == 1 ~ "Mixed"
    )
  )

year_species <- nest_data %>%
  filter(hybridnest == 0) %>%
  count(year, species_f) %>%
  tidyr::pivot_wider(
    names_from = species_f,
    values_from = n,
    values_fill = 0
  )
year_species

cor.test(
  year_species$CF,
  year_species$PF,
  method = "spearman"
)

year_species_change <- year_species %>%
  arrange(year) %>%
  mutate(
    change_CF = CF - lag(CF),
    change_PF = PF - lag(PF)
  )

cor.test(
  year_species_change$change_CF,
  year_species_change$change_PF,
  method = "spearman",
  use = "complete.obs"
)


#now testing if the species balance has an effect on the amount of mixed nests.

year_species <- nest_data %>%
  filter(hybridnest == 0) %>%
  count(year, species_f) %>%
  tidyr::pivot_wider(
    names_from = species_f,
    values_from = n,
    values_fill = 0
  ) %>%
  mutate(
    total_pure = CF + PF,
    prop_CF = CF / total_pure,
    prop_PF = PF / total_pure
  )
year_species <- year_species %>%
  mutate(
    species_balance = pmin(prop_CF, prop_PF)
  )
year_hybrid <- nest_data %>%
  group_by(year) %>%
  summarise(
    mixed = sum(hybridnest == 1),
    total = n(),
    .groups = "drop"
  )
year_analysis <- year_hybrid %>%
  left_join(
    year_species %>%
      select(year, CF, PF, species_balance),
    by = "year"
  )
year_analysis 


m_balance <- glm(
  cbind(mixed, total - mixed) ~ species_balance,
  family = binomial,
  data = year_analysis
)

summary(m_balance)

m_species <- glm(
  cbind(mixed, total - mixed) ~ CF + PF,
  family = binomial,
  data = year_analysis
)

summary(m_species)


#null distribution for to see if a species is more likely to hybridize
n_hybrid <- sum(combined_data$hybridnest == 1)
set.seed(123)

n_perm <- 10000

observed_diff <- with(
  combined_data,
  mean(hybridnest[species_f == "PF"]) -
    mean(hybridnest[species_f == "CF"])
)

null_dist <- replicate(n_perm, {
  
  shuffled_hybrid <- sample(combined_data$hybridnest)
  
  mean(shuffled_hybrid[combined_data$species_f == "PF"]) -
    mean(shuffled_hybrid[combined_data$species_f == "CF"])
})

p_value <- mean(abs(null_dist) >= abs(observed_diff))

observed_diff
p_value
