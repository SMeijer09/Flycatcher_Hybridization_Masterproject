#testing hybridization frequency to site specific relative abundance
library(tidyverse)
library(lme4)
library(lmerTest)
library(patchwork)
library(car)
library(emmeans)
library(ggeffects)
library(rptR)
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

#now removing all females with more than 1 nest in a year
combined_data <- combined_data |>
  group_by(year, ring_nb_f) |>
  mutate(n_entries = n()) |>
  filter(n_entries == 1) |>
  ungroup() |>
  select(-n_entries)
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

#make a Area column in combined data
combined_data <- combined_data |>
  mutate(Area = substr(nestbox, 1, 3))
view(combined_data)

############### #########
# Summarise hybridization frequency and local species composition
plot_year_data <- combined_data |>
  filter(
    !is.na(species_m),
    !is.na(hybridnest),
    !is.na(Area),
    !is.na(year),
    Area != "DUN"
  ) |>
  group_by(Area, year) |>
  summarise(
    n_total = n(),
    n_hybrid = sum(hybridnest == 1),
    n_CF_males = sum(species_m == "CF"),
    .groups = "drop"
  ) |>
  mutate(
    prop_CF_males = n_CF_males / n_total,
    hybridnest_freq = n_hybrid / n_total,
    # Express the effect per 10 percentage-point increase
    prop_CF_males_10pp = prop_CF_males * 10
  ) #|>
filter(n_total >= 0,
       prop_CF_males < 1,
       prop_CF_males > 0,
       hybridnest_freq > 0)

View(plot_year_data)

m_social <- glmer(
  cbind(n_hybrid, n_total - n_hybrid) ~
    prop_CF_males + I(prop_CF_males^2) +
    (1 | Area) +
    (1 | year),
  data = plot_year_data,
  family = binomial
)

m_social1 <- glmer(
  cbind(n_hybrid, n_total - n_hybrid) ~ I(prop_CF_males^2) +
    (1 | Area) +
    (1 | year),
  data = plot_year_data,
  family = binomial
)

m_social2 <- glmer(
  cbind(n_hybrid, n_total - n_hybrid) ~ prop_CF_males +
    (1 | Area) +
    (1 | year),
  data = plot_year_data,
  family = binomial
)

summary(m_social)
summary(m_social1)
summary(m_social2)
AIC(m_social, m_social1, m_social2)
pred_social <- ggpredict(m_social, terms = "prop_CF_males [all]")

p1 <- ggplot(plot_year_data,
             aes(x = prop_CF_males, y = hybridnest_freq)) +
  geom_point(aes(size = n_total), alpha = 0.6) +
  geom_line(
    data = as.data.frame(pred_social),
    aes(x = x, y = predicted),
    inherit.aes = FALSE,
    linewidth = 1
  ) +
  geom_ribbon(
    data = as.data.frame(pred_social),
    aes(x = x, ymin = conf.low, ymax = conf.high),
    inherit.aes = FALSE,
    alpha = 0.2
  ) +
  labs(
    x = "Proportion of collared flycatcher males",
    y = "Observed hybrid nest frequency",
    size = "Breeding occurrences",
    title = ""
  ) +
  scale_x_continuous(
    labels = scales::label_percent()
  ) +
  theme_classic()

############## Lets try again but now only look at habitat quality and not site ############
hq_data <- combined_data |>
  filter(
    !is.na(species_m),
    !is.na(hybridnest),
    !is.na(habitat_quality),
    !is.na(year)
  ) |>
  group_by(habitat_quality, year) |>
  summarise(
    n_total = n(),
    n_hybrid = sum(hybridnest == 1),
    n_CF_males = sum(species_m == "CF"),
    .groups = "drop"
  ) |>
  mutate(
    prop_CF_males = n_CF_males / n_total,
    hybridnest_freq = n_hybrid / n_total) |>
  filter(n_total >= 1,
         hybridnest_freq > 0,
         prop_CF_males < 1)
view(hq_data)

#plot 
ggplot(hq_data, aes(x = prop_CF_males, y = hybridnest_freq)) +
  geom_point(aes(size = n_total), alpha = 0.6) +
  labs(
    x = "Proportion of collared flycatcher males",
    y = "Observed hybrid nest frequency",
    size = "Breeding attempts",
    title = ""
  ) +
  scale_x_continuous(
    labels = scales::label_percent()
  ) +
  theme_classic()

hq_model <- glmer(
  cbind(n_hybrid, n_total - n_hybrid) ~
    prop_CF_males +
    (1 | habitat_quality) +
    (1 | year),
  data = hq_data,
  family = binomial
)
summary(hq_model)

########### only sites now no years
plot_data <- combined_data |>
  filter(
    !is.na(species_m),
    !is.na(hybridnest),
    !is.na(Area),
    !is.na(year),
    Area != "DUN"
  ) |>
  group_by(Area) |>
  summarise(
    n_total = n(),
    n_hybrid = sum(hybridnest == 1),
    n_CF_males = sum(species_m == "CF"),
    .groups = "drop"
  ) |>
  mutate(
    prop_CF_males = n_CF_males / n_total,
    hybridnest_freq = n_hybrid / n_total
  ) |>
  filter(n_total >= 0,
         prop_CF_males < 1,
         prop_CF_males > 0,
         hybridnest_freq > 0)

View(plot_data)

plot_model <- glm(
  cbind(n_hybrid, n_total - n_hybrid) ~
    prop_CF_males + I(prop_CF_males^2),
  data = plot_data,
  family = binomial
)
summary(plot_model)


pred_plot <- ggpredict(plot_model, terms = "prop_CF_males [all]")


p2 <- ggplot(plot_data,
             aes(x = prop_CF_males, y = hybridnest_freq)) +
  geom_point(aes(size = n_total), alpha = 0.6) +
  labs(
    x = "Proportion of collared flycatcher males",
    y = "Observed hybrid nest frequency",
    size = "Breeding occurrences",
    title = ""
  ) +
  scale_x_continuous(
    labels = scales::label_percent()
  ) +
  theme_classic() + #add slope plus CI
  geom_line(
    data = as.data.frame(pred_plot),
    aes(x = x, y = predicted),
    inherit.aes = FALSE,
    linewidth = 1
  ) +
  geom_ribbon(
    data = as.data.frame(pred_plot),
    aes(x = x, ymin = conf.low, ymax = conf.high),
    inherit.aes = FALSE,
    alpha = 0.2
  )

#plot together
p2 + p1 + plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(size = 16))

ggsave(filename = "prop_plot.png",scale = 1, width = 14, height = 6, units = "in", dpi = 300)


#model plot by year and plot again but now without filtering for hybridnest_freq > 0
plot_year_data2 <- combined_data |>
  filter(
    !is.na(species_m),
    !is.na(hybridnest),
    !is.na(Area),
    !is.na(year),
    Area != "DUN"
  ) |>
  group_by(Area, year) |>
  summarise(
    n_total = n(),
    n_hybrid = sum(hybridnest == 1),
    n_CF_males = sum(species_m == "CF"),
    .groups = "drop"
  ) |>
  mutate(
    prop_CF_males = n_CF_males / n_total,
    hybridnest_freq = n_hybrid / n_total,
  ) |>
  filter(n_total >= 0,
         prop_CF_males < 1,
         prop_CF_males > 0)

model_year <- glmer(
  cbind(n_hybrid, n_total - n_hybrid) ~
    prop_CF_males + I(prop_CF_males^2) +
    (1 | Area) +
    (1 | year),
  data = plot_year_data2,
  family = binomial
)
summary(model_year)

pred_year <- ggpredict(model_year, terms = "prop_CF_males [all]")

p3 <- ggplot(plot_year_data2,
             aes(x = prop_CF_males, y = hybridnest_freq)) +
  geom_point(aes(size = n_total), alpha = 0.6) +
  labs(
    x = "Proportion of collared flycatcher males",
    y = "Observed hybrid nest frequency",
    size = "Breeding occurrences",
    title = ""
  ) +
  scale_x_continuous(
    labels = scales::label_percent()
  ) +
  theme_classic() + #add slope plus CI
  geom_line(
    data = as.data.frame(pred_year),
    aes(x = x, y = predicted),
    inherit.aes = FALSE,
    linewidth = 1
  ) +
  geom_ribbon(
    data = as.data.frame(pred_year),
    aes(x = x, ymin = conf.low, ymax = conf.high),
    inherit.aes = FALSE,
    alpha = 0.2
  )

#now for plot combined
plot_data1 <- combined_data |>
  filter(
    !is.na(species_m),
    !is.na(hybridnest),
    !is.na(Area),
    !is.na(year),
    Area != "DUN"
  ) |>
  group_by(Area) |>
  summarise(
    n_total = n(),
    n_hybrid = sum(hybridnest == 1),
    n_CF_males = sum(species_m == "CF"),
    .groups = "drop"
  ) |>
  mutate(
    prop_CF_males = n_CF_males / n_total,
    hybridnest_freq = n_hybrid / n_total,
  ) |>
  filter(n_total >= 0,
         prop_CF_males < 1,
         prop_CF_males > 0)

model_combined <- glmer(
  cbind(n_hybrid, n_total - n_hybrid) ~
    prop_CF_males + I(prop_CF_males^2) +
    (1 | Area),
  data = plot_data1,
  family = binomial
)
summary(model_combined)

pred_combined <- ggpredict(model_combined, terms = "prop_CF_males [all]")

p4 <- ggplot(plot_data1,
             aes(x = prop_CF_males, y = hybridnest_freq)) +
  geom_point(aes(size = n_total), alpha = 0.6) +
  labs(
    x = "Proportion of collared flycatcher males",
    y = "Observed hybrid nest frequency",
    size = "Breeding occurrences",
    title = ""
  ) +
  scale_x_continuous(
    labels = scales::label_percent()
  ) +
  theme_classic() + #add slope plus CI
  geom_line(
    data = as.data.frame(pred_combined),
    aes(x = x, y = predicted),
    inherit.aes = FALSE,
    linewidth = 1
  ) +
  geom_ribbon(
    data = as.data.frame(pred_combined),
    aes(x = x, ymin = conf.low, ymax = conf.high),
    inherit.aes = FALSE,
    alpha = 0.2
  )

#togetheer
p4 + p3 + plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(size = 16))
#ggsave(filename = "prop_plot_year_combined.png",scale = 1, width = 14, height = 6, units = "in", dpi = 300)

#one last time including 0 proportions
plot_year_data3 <- combined_data |>
  filter(
    !is.na(species_m),
    !is.na(hybridnest),
    !is.na(Area),
    !is.na(year),
    Area != "DUN"
  ) |>
  group_by(Area, year) |>
  summarise(
    n_total = n(),
    n_hybrid = sum(hybridnest == 1),
    n_CF_males = sum(species_m == "CF"),
    .groups = "drop"
  ) |>
  mutate(
    prop_CF_males = n_CF_males / n_total,
    hybridnest_freq = n_hybrid / n_total,
  ) |>
  filter(n_total >= 0)

model_year3 <- glmer(
  cbind(n_hybrid, n_total - n_hybrid) ~
    prop_CF_males + I(prop_CF_males^2) +
    (1 | Area) +
    (1 | year),
  data = plot_year_data3,
  family = binomial
)
summary(model_year3)

pred_year3 <- ggpredict(model_year3, terms = "prop_CF_males [all]")

p6 <- ggplot(plot_year_data3,
             aes(x = prop_CF_males, y = hybridnest_freq)) +
  geom_point(aes(size = n_total), alpha = 0.6) +
  labs(
    x = "Proportion of collared flycatcher males",
    y = "Observed hybrid nest frequency",
    size = "Breeding occurrences",
    title = ""
  ) +
  scale_x_continuous(
    labels = scales::label_percent()
  ) +
  theme_classic() + #add slope plus CI
  geom_line(
    data = as.data.frame(pred_year3),
    aes(x = x, y = predicted),
    inherit.aes = FALSE,
    linewidth = 1
  ) +
  geom_ribbon(
    data = as.data.frame(pred_year3),
    aes(x = x, ymin = conf.low, ymax = conf.high),
    inherit.aes = FALSE,
    alpha = 0.2
  )

#confidence interval of the model_year3

#one last time for the area

data_area <- combined_data |>
  filter(
    !is.na(species_m),
    !is.na(hybridnest),
    !is.na(Area),
    !is.na(year),
    Area != "DUN"
  ) |>
  group_by(Area) |>
  summarise(
    n_total = n(),
    n_hybrid = sum(hybridnest == 1),
    n_CF_males = sum(species_m == "CF"),
    .groups = "drop"
  ) |>
  mutate(
    prop_CF_males = n_CF_males / n_total,
    hybridnest_freq = n_hybrid / n_total,
  ) |>
  filter(n_total >= 0)

area_model <- glmer(
  cbind(n_hybrid, n_total - n_hybrid) ~
    prop_CF_males + I(prop_CF_males^2) +
    (1 | Area),
  data = data_area,
  family = binomial
)
summary(area_model)
pred_area <- ggpredict(area_model, terms = "prop_CF_males [all]")
p5 <- ggplot(data_area,
             aes(x = prop_CF_males, y = hybridnest_freq)) +
  geom_point(aes(size = n_total), alpha = 0.6) +
  labs(
    x = "Proportion of collared flycatcher males",
    y = "Observed hybrid nest frequency",
    size = "Breeding occurrences",
    title = ""
  ) +
  scale_x_continuous(
    labels = scales::label_percent()
  ) +
  theme_classic() + #add slope plus CI
  geom_line(
    data = as.data.frame(pred_area),
    aes(x = x, y = predicted),
    inherit.aes = FALSE,
    linewidth = 1
  ) +
  geom_ribbon(
    data = as.data.frame(pred_area),
    aes(x = x, ymin = conf.low, ymax = conf.high),
    inherit.aes = FALSE,
    alpha = 0.2
  )

p5 + p6 + plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(size = 16))
#ggsave(filename = "prop_plot_year_combined_including0.png",scale = 1, width = 14, height = 6, units = "in", dpi = 300)
p6
