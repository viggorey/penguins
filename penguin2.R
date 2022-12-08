### Packages used   --------------------

library(palmerpenguins)
library(ggplot2)
suppressPackageStartupMessages(library(janitor))
suppressPackageStartupMessages(library(dplyr))
library(ragg)
library(svglite)

# rm(list = ls()) # to clean environment

### Set working directory  --------------

setwd('/Users/viggorey/Desktop/PenguinProject') 


### Examine data ------------------------

head(penguins_raw)
names(penguins_raw) 


### Load the data -----------------------

write.csv(penguins_raw, "data_raw/penguins_raw.csv")


### Cleaning data  -----------------------

# Examine column names
names(penguins_raw)

# Clean names in columns to r-friendly, empty rows removed, remove columns with comment and delta in them
cleaning <- function(data_raw){data_raw %>%
    clean_names() %>% 
    remove_empty(c("rows", "cols")) %>% 
    select(-starts_with("delta")) %>%
    select(-comments)}

penguins_clean <- cleaning(penguins_raw)

names(penguins_clean)


### Load clean data  ---------------------

dir.create("data_clean")

write.csv(penguins_clean, "data_clean/penguins_clean.csv")


### Data analysis  -----------------------
## Examining if culmen length explains flipper length in female and male Adelie penguins acording to collected data using linear regression obtaining a p-value 

# First create one dataset separating sex, species, flipper and culmen length datapoints. Also removing NAs.
# Do this first by creating function and then applying it on cleaned dataset
penguin_male_data_function <- function(data_clean){
  data_clean %>%
    filter(!is.na(sex)) %>%
    filter(!is.na(species)) %>%
    filter(!is.na(flipper_length_mm)) %>%
    filter(!is.na(culmen_length_mm)) %>%
    select(sex, species, flipper_length_mm, culmen_length_mm)
}

penguins1 <- penguin_male_data_function(penguins_clean)

# Second create two datasets, one for female data and one for male data, separating out Adelie penguin data as well
penguins1_FEMALE <- penguins1 %>% filter(sex == 'FEMALE') %>% filter(species == 'Adelie Penguin (Pygoscelis adeliae)')
penguins1_MALE <- penguins1 %>% filter(sex == 'MALE') %>% filter(species == 'Adelie Penguin (Pygoscelis adeliae)')

# Third do linear regression analysis on both datasets
penguinF_mod <- lm(culmen_length_mm ~ flipper_length_mm, penguins1_FEMALE)
summary(penguinF_mod)

penguinM_mod <- lm(culmen_length_mm ~ flipper_length_mm, penguins1_MALE)
summary(penguinM_mod)

# Correlations significant in males but not females...
# 8.6% of variation in flipper length is explained by culmen length in male adelaide penguins

### Create figure  ---------------------
## Plot two (male and female) scatterplots with linear model (lm) overlaid in each figure showing the relationship

femaleplot <- ggplot(penguins1_FEMALE, aes(x = culmen_length_mm, y = flipper_length_mm)) +
  geom_point() +
  geom_smooth(method = lm) +
  xlab('Culmen Length (mm)') +
  ylab('Flipper Length (mm)') +
  ggtitle('Flipper and culmen length relationship for female Adelaide penguins')

maleplot <- ggplot(penguins1_MALE, aes(x = culmen_length_mm, y = flipper_length_mm)) +
  geom_point() +
  geom_smooth(method = lm) +
  xlab('Culmen Length (mm)') +
  ylab('Flipper Length (mm)') +
  ggtitle('Flipper and culmen length relationship for male Adelaide penguins')

# Put both plots together into one figure in a grid
totalplot <- plot_grid(femaleplot, maleplot, labels = LETTERS, ncol = 1)
totalplot


### Save figure  ---------------------

dir.create('figures')

# Create function to save plot into directory whilst modifying location, size, and scale
save_flipper_plot_svg <- function(totalplot, 
                                  filename, size, scaling){
  size_inches = size/2.54
  svglite(filename, width   = size_inches, 
          height  = size_inches, 
          scaling = scaling)
  totalplot
  print(totalplot)
  dev.off()
}

# Then make function work on given graph such:
save_flipper_plot_svg(totalplot, 
                      "figures/penguins_vector.svg", 
                      size = 15, scaling = 0.8)

# Figure should now be saved in directory


### Fin  -----------------------------









