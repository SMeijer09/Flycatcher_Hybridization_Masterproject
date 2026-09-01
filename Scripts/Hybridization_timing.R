#effect of timing (laying date)
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

#plot laying date and hatching date
hist(combined_data$laying_date, breaks = 20)

#compare the mean to the median date
combined_data |> 
  group_by(year) |>
  summarise(mean_laying_date = mean(laying_date, na.rm = TRUE),
            median_laying_date = median(laying_date, na.rm = TRUE)) |>
  ggplot(aes(x=year)) +
  geom_point(aes(y=mean_laying_date), color="blue") +
  geom_point(aes(y=median_laying_date), color="red") +
  geom_line(aes(y=mean_laying_date), color="blue") +
  geom_line(aes(y=median_laying_date), color="red") +
  theme_classic() +
  labs(x="Year", y="Laying date") +
  scale_y_continuous(breaks = seq(0, 200, by = 1)) +
  scale_x_continuous(breaks = seq(2000, 2023, by = 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

model_data <- combined_data |> select(yearAreaBox, year, nestbox, ring_nb_f, species_f, hybridnest, 
    laying_date) |> group_by(year) |>
  mutate(laying_date_relative = laying_date - mean(laying_date, na.rm = TRUE),
         laying_date_z = (laying_date - mean(laying_date, na.rm = TRUE)) /
           sd(laying_date, na.rm = TRUE)) |>
  ungroup()
view(model_data)

#plot and compare the relative and z laying dates distributions
ggplot(model_data, aes(x=laying_date_relative)) +
  geom_histogram(bins=20, fill="blue", alpha=0.5) +
  theme_classic() +
  labs(x="Relative laying date", y="Count") +
  scale_x_continuous(breaks = seq(-50, 50, by = 5)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10))
ggplot(model_data, aes(x=laying_date_z)) +
  geom_histogram(bins=20, fill="red", alpha=0.5) +
  theme_classic() +
  labs(x="Z laying date", y="Count") +
  scale_x_continuous(breaks = seq(-3, 3, by = 0.5)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10))

#now model the effect of laying date on hybridnest
m1 <- glmer(hybridnest ~ laying_date_relative + (1|year), data=model_data, family=binomial)
summary(m1)
m2 <- glmer(hybridnest ~ laying_date_relative * species_f + (1|year), data=model_data, family=binomial)
summary(m2)
m3 <- glmer(hybridnest ~ laying_date_z + (1|year), data=model_data, family=binomial)
summary(m3)
m4 <- glmer(hybridnest ~ laying_date_z * species_f + (1|year), data=model_data, family=binomial)
summary(m4)

#plot the effect of laying date on hybridnest per species
ggplot(model_data, aes(x=laying_date_relative, y=hybridnest, color=species_f)) +
  geom_point(alpha=0.5) +
  geom_smooth(method="glm", method.args=list(family="binomial"), se=TRUE) +
  theme_classic() +
  labs(x="Relative laying date", y="Probability of hybrid nest") +
  scale_x_continuous(breaks = seq(-50, 50, by = 5)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1))

#ok this is for the laying date adjusted to the two species together, but they have different timing so maybe it should be separated instead!
#lets give that a try

model_data1 <- combined_data |> select(yearAreaBox, year, nestbox, ring_nb_f, species_f, hybridnest, 
    laying_date) |> group_by(year, species_f) |>
  mutate(laying_date_relative = laying_date - mean(laying_date, na.rm = TRUE),
         laying_date_z = (laying_date - mean(laying_date, na.rm = TRUE)) /
           sd(laying_date, na.rm = TRUE)) |>
  ungroup()

m1_species <- glmer(hybridnest ~ laying_date_relative + (1|year), data=model_data1, family=binomial)
summary(m1_species)
m2_species <- glmer(hybridnest ~ laying_date_relative * species_f + (1|year), data=model_data1, family=binomial)
summary(m2_species)
m3_species <- glmer(hybridnest ~ laying_date_z + (1|year), data=model_data1, family=binomial)
summary(m3_species)
m4_species <- glmer(hybridnest ~ laying_date_z * species_f + (1|year), data=model_data1, family=binomial)
summary(m4_species)

#show a figure with the laying date distribution per year per species
ggplot(model_data1, aes(x=laying_date_relative, fill=species_f)) +
  geom_histogram(position="identity", alpha=0.5, bins=20) +
  theme_classic() +
  labs(x="Relative laying date", y="Count") +
  scale_x_continuous(breaks = seq(-50, 50, by = 5)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  facet_wrap(~year)

#again plot the laying date with hybridnest per species, but now with the relative laying date per species
ggplot(model_data1, aes(x=laying_date_relative, y=hybridnest, color=species_f)) +
  geom_point(alpha=0.5) +
  geom_smooth(method="glm", method.args=list(family="binomial"), se=TRUE) +
  theme_classic() +
  labs(x="Relative laying date per species", y="Probability of hybrid nest") +
  scale_x_continuous(breaks = seq(-50, 50, by = 5)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1))

#now with z laying date
ggplot(model_data1, aes(x=laying_date_z, y=hybridnest, color=species_f)) +
  geom_jitter(alpha=0.5,height = 0.05,width=0) +
  geom_smooth(method="glm", method.args=list(family="binomial"), se=TRUE) +
  theme_classic() +
  labs(x="Z laying date per species", y="Probability of hybrid nest") +
  scale_x_continuous(breaks = seq(-3, 7, by = 1)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1))

#plot the distribution of the laying_date_z per species overall
ggplot(model_data1, aes(x=laying_date_z, fill=species_f)) +
  geom_histogram(position="identity", alpha=0.5, bins=20) +
  theme_classic() +
  labs(x="Z laying date", y="Count") +
  scale_x_continuous(breaks = seq(-3, 7, by = 1)) +
  scale_y_continuous(breaks = seq(0, 1500, by = 100))

#now plot a histogram of the actual laying date for each species and color the hybrid nests, but put them in 1 graph, where species and species b are on top of each other, but with different colors for the hybrid nests
ggplot(model_data1, aes(x = laying_date)) +
  
  # CF: all nests
  geom_histogram(
    data = subset(model_data1, species_f == "CF"),
    aes(y = after_stat(count), fill = "CF – non-hybrid"),
    binwidth = 2,
    alpha = 0.5
  ) +
  
  # PF: all nests
  geom_histogram(
    data = subset(model_data1, species_f == "PF"),
    aes(y = -after_stat(count), fill = "PF – non-hybrid"),
    binwidth = 2,
    alpha = 0.5
  ) +
  
  # CF: hybrid nests
  geom_histogram(
    data = subset(model_data1, species_f == "CF" & hybridnest == 1),
    aes(y = after_stat(count), fill = "CF – hybrid"),
    binwidth = 2,
    alpha = 0.8
  ) +
  
  # PF: hybrid nests
  geom_histogram(
    data = subset(model_data1, species_f == "PF" & hybridnest == 1),
    aes(y = -after_stat(count), fill = "PF – hybrid"),
    binwidth = 2,
    alpha = 0.8
  ) +
  
  geom_hline(yintercept = 0) +
  
  scale_fill_manual(
    name = "Nest type",
    values = c(
      "CF – non-hybrid" = "grey70",
      "CF – hybrid" = "blue",
      "PF – non-hybrid" = "grey40",
      "PF – hybrid" = "red"
    )
  ) +
  
  theme_classic() +
  labs(
    x = "Laying date",
    y = "Number of nests"
  ) +
  scale_y_continuous(
    breaks = seq(-200, 700, by = 100),
    labels = abs(seq(-200, 700, by = 100))
  ) 

#plot the mean and median laying date per year per species in 1 plot with the standard deviation
ggplot(model_data1, aes(x=year, y=laying_date, color=species_f)) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar", width = 0.2) +
  theme_classic() +
  labs(x="Year", y="Laying date") +
  scale_y_continuous(breaks = seq(0, 200, by = 5)) +
  scale_x_continuous(breaks = seq(2000, 2023, by = 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_smooth(aes(group=species_f), method="lm", se=FALSE) 

#test if the laying date has changed over the years for the species
m1 <- lmer(laying_date ~ year * species_f + (1|ring_nb_f), data=model_data1)
summary(m1)
par(mfrow=c(2,2))
plot(m1)

#make a plot based on the model and include confidence interval
ggplot(model_data1, aes(x=year, y=laying_date, color=species_f)) +
  geom_point(alpha=0.5) +
  geom_smooth(method="lm", se=TRUE) +
  theme_classic() +
  labs(x="Year", y="Laying date") +
  scale_y_continuous(breaks = seq(0, 200, by = 5)) +
  scale_x_continuous(breaks = seq(2000, 2023, by = 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
