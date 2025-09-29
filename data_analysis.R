# Cindy Gao
# LDP Final Project
# Study question: How do penguin traits vary across the islands
# of the Palmer Archipelago?
# Traits being studied: bill length (mm), flipper length (mm),
# and body mass (g)


# setup; loading packages and reading in data -----------------------------
library(palmerpenguins)
library(tidyverse)
library(emmeans)
data(penguins)


# exploratory data analysis -----------------------------------------------
# taking a glimpse at the mean trait values for each island
penguins |>
  group_by(island) |>
  summarise(
    count = n(),
    mean_bill_length = mean(bill_length_mm, na.rm = T),
    mean_flipper_length = mean(flipper_length_mm, na.rm = T),
    mean_body_mass = mean(body_mass_g, na.rm = T)
  )

# examining the distribution of bill length 
# for each island
penguins |>
  ggplot(aes(x = bill_length_mm)) +
  geom_histogram() +
  facet_wrap(~island)

# examining the distribution of flipper length
# for each island
penguins |>
  ggplot(aes(x = flipper_length_mm)) +
  geom_histogram() +
  facet_wrap(~island)

# examining the distribution of body mass
# for each island
penguins |>
  ggplot(aes(x = body_mass_g)) +
  geom_histogram() +
  facet_wrap(~island)

# data preparation --------------------------------------------------------

# getting rid of NAs and bill depth measurements
# i am not examining bill depth for this study
penguins_final <- penguins |>
  drop_na() |>
  select(-bill_depth_mm)

# assessing for differences in mean trait value across islands ------------

# my first question is: Is there a significant difference in mean trait value
# across the islands?

# using ANOVA to test for differences in mean flipper length across islands
flipper_length_anova <- aov(flipper_length_mm ~ island, data = penguins_final)
summary(flipper_length_anova)

# using ANOVA to test for differences in mean bill length across islands
bill_length_anova <- aov(bill_length_mm ~ island, data = penguins_final)
summary(bill_length_anova)

# using ANOVA to test for differences in mean body mass across islands
body_mass_anova <- aov(body_mass_g ~ island, data = penguins_final)
summary(body_mass_anova)

# from the summaries, it seems that all traits exhibit significant differences
# in mean trait value across the islands
# i'll identify important pairwise comparisons using a Tukey Test

flipper_length_tukey <- emmeans(flipper_length_anova, 'island')
pairs(flipper_length_tukey)
# for flipper length, the significant pairwise comparisons are biscoe and dream island
# as well as biscoe and torgersen island

bill_length_tukey <- emmeans(bill_length_anova, 'island')
pairs(bill_length_tukey)
# for bill length, the significant pairwise comparisons are biscoe and torgersen,
# as well as dream and torgersen

body_mass_tukey <- emmeans(body_mass_anova, 'island')
pairs(body_mass_tukey)
# for body mass, the significant pairwise comparisons are biscoe and dream,
# as well as biscoe and torgersen

