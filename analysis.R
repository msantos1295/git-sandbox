#' First, I am going to go ahead and install the necessary package
#' and load the dataset.
# This is the palmerpenguins package that has the dataset 'penguins'
# ggplot2 is for the graphing; pretty confident it's already installed
install.packages("palmerpenguins")
library(palmerpenguins)
library(ggplot2)
#' Let's take a look at what exactly is in the dataset here
head(penguins)
summary(penguins)
#' Time to create a plot of it
# simple scatter plot showing bill length vs body math separated by male and female penguins
ggplot(penguins, aes(x=bill_length_mm, y=body_mass_g, color=sex)) +
  geom_point() +
  labs(x="Bill Length", y="Body Mass")