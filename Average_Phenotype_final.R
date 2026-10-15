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

#now removing all females with more than 1 nest in a year
combined_data <- combined_data |>
  group_by(year, ring_nb_f) |>
  mutate(n_entries = n()) |>
  filter(n_entries == 1) |>
  ungroup() |>
  select(-n_entries)
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

#now from combined_data we make new columns for wing patch and patch size where we account for the effect of age on these traits
#make boxplots for each male trait per species per age category
ggplot(combined_data, aes(x=factor(age_category_corrected_m), y=patch_size_m, color=species_m)) + geom_boxplot() + theme_minimal()

ggplot(combined_data, aes(x=factor(age_category_corrected_m), y=sum_of_white_on_primaries_m, color=species_m)) + geom_boxplot() + theme_minimal()

ggplot(combined_data, aes(x=factor(age_category_corrected_m), y=wing_m, color=species_m)) + geom_boxplot() + theme_minimal()

ggplot(combined_data, aes(x=factor(age_category_corrected_m), y=tarsus_m, color=species_m)) + geom_boxplot() + theme_minimal()


#test if patch size is significantly different between age categories
m1 <- lm(patch_size_m ~ age_category_corrected_m, data=subset(combined_data,species_m=="PF"))
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

#Make some boxplots to see that there is no sginficant differences in phenotypes
ggplot(avg_data1_adj, aes(x=factor(hybridized), y=avg_patch_size_m, color=species_f)) + geom_boxplot() + theme_minimal()

ggplot(avg_data1_adj, aes(x=factor(hybridized), y=avg_wing_patch_m, color=species_f)) + geom_boxplot() + theme_minimal()

ggplot(avg_data1_adj, aes(x=factor(hybridized), y=avg_tarsus_m, color=species_f)) + geom_boxplot() + theme_minimal()

ggplot(avg_data1_adj, aes(x=factor(hybridized), y=avg_wing_m, color=species_f)) + geom_boxplot() + theme_minimal()

#lets do a violin 

ggplot(avg_data1_adj, aes(x=factor(hybridized), y=avg_patch_size_m, color=species_f)) + geom_violin() + theme_minimal()

#lets do a boxplot with datapoints
ggplot(
  avg_data1_adj,
  aes(
    x = interaction(hybridized, species_f),
    y = avg_patch_size_m,
    color = species_f
  )
) +
  geom_boxplot(width = 0.6) +
  geom_jitter(
    width = 0.12,
    alpha = 0.5
  ) +
  theme_minimal() +
  labs(x = "Hybridization")

p1 <- ggplot(
  avg_data1_adj,
  aes(
    x = interaction(hybridized, species_f),
    y = avg_patch_size_m,
    color = species_f
  )
) +
  geom_boxplot(
    width = 0.55,
    outlier.shape = NA,
    alpha = 0.15
  ) +
  geom_jitter(
    width = 0.10,
    alpha = 0.45,
    size = 1.8
  ) +
  scale_x_discrete(
    labels = c(
      "0.PF" = "Non-Hybridized",
      "0.CF" = "Non-Hybridized",
      "1.PF" = "Hybridized",
      "1.CF" = "Hybridized"
    )
  ) +
  scale_y_continuous(
    breaks = seq(-40, 40, by = 10)
  ) +
  scale_color_manual(
    values = c(
      "PF" = "#1f77b4",
      "CF" = "#ff7f0e"
    ),
    labels = c(
      "PF" = "Pied",
      "CF" = "Collared"
    )
  ) +
  labs(
    x = NULL,
    y = "Average Residual Forehead Patch Size (mm²)",
    color = "Female species"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 11),
  )


p2 <- ggplot(
  avg_data1_adj,
  aes(
    x = interaction(hybridized, species_f),
    y = avg_wing_patch_m,
    color = species_f
  )
) +
  geom_boxplot(
    width = 0.55,
    outlier.shape = NA,
    alpha = 0.15
  ) +
  geom_jitter(
    width = 0.10,
    alpha = 0.45,
    size = 1.8
  ) +
  scale_x_discrete(
    labels = c(
      "0.PF" = "Non-Hybridized",
      "0.CF" = "Non-Hybridized",
      "1.PF" = "Hybridized",
      "1.CF" = "Hybridized"
    )
  ) +
  scale_y_continuous(
    breaks = seq(-40, 40, by = 10)
  ) +
  scale_color_manual(
    values = c(
      "PF" = "#1f77b4",
      "CF" = "#ff7f0e"
    ),
    labels = c(
      "PF" = "Pied",
      "CF" = "Collared"
    )
  ) +
  labs(
    x = NULL,
    y = "Average Residual Wing Patch Size (mm)",
    color = "Female species"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 11),
  )

#now make the same plot but for tarsus, but then non age adjusted
p3<- ggplot(
  avg_data1,
  aes(
    x = interaction(hybridized, species_f),
    y = avg_tarsus_m,
    color = species_f
  )
) +
  geom_boxplot(
    width = 0.55,
    outlier.shape = NA,
    alpha = 0.15
  ) +
  geom_jitter(
    width = 0.10,
    alpha = 0.45,
    size = 1.8
  ) +
  scale_x_discrete(
    labels = c(
      "0.PF" = "Non-Hybridized",
      "0.CF" = "Non-Hybridized",
      "1.PF" = "Hybridized",
      "1.CF" = "Hybridized"
    )
  ) +
  scale_color_manual(
    values = c(
      "PF" = "#1f77b4",
      "CF" = "#ff7f0e"
    ),
    labels = c(
      "PF" = "Pied",
      "CF" = "Collared"
    )
  ) +
  labs(
    x = NULL,
    y = "Average Tarsus Length (mm)",
    color = "Female species"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 11),
  )

p4 <- ggplot(
  avg_data1,
  aes(
    x = interaction(hybridized, species_f),
    y = avg_wing_m,
    color = species_f
  )
) +
  geom_boxplot(
    width = 0.55,
    outlier.shape = NA,
    alpha = 0.15
  ) +
  geom_jitter(
    width = 0.10,
    alpha = 0.45,
    size = 1.8
  ) +
  scale_x_discrete(
    labels = c(
      "0.PF" = "Non-Hybridized",
      "0.CF" = "Non-Hybridized",
      "1.PF" = "Hybridized",
      "1.CF" = "Hybridized"
    )
  ) +
  scale_color_manual(
    values = c(
      "PF" = "#1f77b4",
      "CF" = "#ff7f0e"
    ),
    labels = c(
      "PF" = "Pied",
      "CF" = "Collared"
    )
  ) +
  labs(
    x = NULL,
    y = "Average Wing Length (mm)",
    color = "Female species"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 11),
  )

#quick test what happens when i do lm instead
mtest <- lm(avg_wing_m ~ hybridized, data= subset(avg_data1, species_f=="PF"))
summary(mtest)

#plot p1 to p4 together
library(patchwork)
(p1 | p2) / (p3 | p4) + plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(size = 14, face = "bold"))

(p1 | p2) / (p3 | p4) +
  plot_annotation(tag_levels = "A") &
  theme(
    plot.tag = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 10)
  )

#also make the plots for beak length, and wing length
p5 <- ggplot(avg_data1,aes(
  x = interaction(hybridized, species_f),
  y = avg_beak_m,
  color = species_f
)
) +
  geom_boxplot(
    width = 0.55,
    outlier.shape = NA,
    alpha = 0.15
  ) +
  geom_jitter(
    width = 0.10,
    alpha = 0.45,
    size = 1.8
  ) +
  scale_x_discrete(
    labels = c(
      "0.PF" = "Non-Hybridized",
      "0.CF" = "Non-Hybridized",
      "1.PF" = "Hybridized",
      "1.CF" = "Hybridized"
    )
  ) +
  scale_color_manual(
    values = c(
      "PF" = "#1f77b4",
      "CF" = "#ff7f0e"
    ),
    labels = c(
      "PF" = "Pied",
      "CF" = "Collared"
    )
  ) +
  labs(
    x = NULL,
    y = "Average Beak Length (mm)",
    color = "Female species"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 11))

#plot together
(p1 | p2) / (p3 | p4) / (p5) + plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(size = 14, face = "bold"))

#ggsave("male_phenotype_boxplots.png", width = 12, height = 13, dpi = 300)

