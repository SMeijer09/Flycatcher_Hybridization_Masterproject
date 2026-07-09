library(tidyverse)
library(lme4)
library(lmerTest)
library(patchwork)
library(car)
library(emmeans)
#look at all hybrid nests
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

filtered_data <- data_clean |>
  filter(n_birds==2)

female_data <- filtered_data |>
  filter(sex=="female") |>
  rename_with(~ paste0(.x, "_f"), -c(yearAreaBox, year, nestbox, fledge_nb, hybridnest, n_birds))
male_data <- filtered_data |>
  filter(sex=='male') |>
  rename_with(~ paste0(.x, "_m"), -c(yearAreaBox, year, nestbox, fledge_nb, hybridnest, n_birds))

combined_data <- female_data |> left_join(male_data, by=c("yearAreaBox","year","nestbox","fledge_nb","hybridnest","n_birds"))
view(combined_data)

#count the number of entries per species and hybridnest
combined_data |>
  group_by(hybridnest,species_f) |>
  summarise(n = n())

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

ggplot(subset(combined_data,!is.na(age_category_m)), aes(x=factor(hybridnest),y=z_mass_m)) +
  geom_boxplot() +
  facet_wrap(~species_f)

#plot the age category of the hybrid birds per species
ggplot(subset(combined_data,!is.na(age_category_m)), aes(x=factor(hybridnest), fill=factor(age_category_m))) +
  geom_bar(position="dodge") +
  facet_wrap(~species_m)

#what are the proportions that hybridize per species in comparison to non hybrid pairs
combined_data |>
  group_by(species_f, hybridnest) |>
  summarise(n = n()) |>
  group_by(species_f) |>
  mutate(prop = n/sum(n))

#plot the amount of hybridnest = 1 over the years and in the background plot a line for the amount of hybrid nest = 0 over the years
p1 <- ggplot(subset(combined_data,hybridnest==1), aes(x=year, fill=factor(hybridnest))) +
  geom_bar(position="dodge") + geom_line(data=subset(combined_data,hybridnest==0), aes(x=year), stat="count", color="red") 

#now plot the proportion of hybrid nests over the years
p2 <- combined_data |>
  group_by(year, hybridnest) |>
  summarise(n = n()) |>
  group_by(year) |>
  mutate(prop = n/sum(n)) |>
  filter(hybridnest==1) |>
  ggplot(aes(x=year, y=prop)) +
  geom_line() +
  geom_point() +
  ylim(0,0.2) +
  labs(y="Proportion of hybrid nests", x="Year") +
  theme_minimal()

p1 + p2 

#overlay p1 and p2


plotdat <- combined_data |>
  group_by(year) |>
  summarise(
    hybrid_nests = sum(hybridnest == 1),
    total_nests = n(),
    prop_hybrid = hybrid_nests / total_nests
  )

scale_factor <- 200   # experiment with this

ggplot(plotdat, aes(year)) +
  geom_col(aes(y = hybrid_nests), fill = "steelblue") +
  geom_line(aes(y = prop_hybrid * scale_factor),
            colour = "red", linewidth = 1.2) +
  geom_point(aes(y = prop_hybrid * scale_factor),
             colour = "red") +
  scale_y_continuous(
    name = "Number of hybrid nests",
    sec.axis = sec_axis(~ . / scale_factor,
                        name = "Proportion of hybrid nests")
  ) + theme_minimal()


#test if the proportions significantly differ between years
tab <- table(combined_data$year, combined_data$hybridnest)
tab
chisq.test(tab)

glm_model <- glm(hybridnest ~ year, data = combined_data, family = binomial)
summary(glm_model)
#no significant increase or decrease in proportions over the years

glm_model2 <- glm(hybridnest ~ factor(year), data = combined_data, family = binomial)
summary(glm_model2)

overall_prop <- mean(combined_data$hybridnest)

overall_prop

year_tests <- combined_data |>
  group_by(year) |>
  summarise(
    hybrids = sum(hybridnest),
    total = n(),
    prop = hybrids / total,
    p_value = prop.test(
      hybrids,
      total,
      p = overall_prop
    )$p.value
  ) |>
  mutate(
    p_adj = p.adjust(p_value, method = "BH")
  )
print(year_tests,n=Inf)

?prop.test()
