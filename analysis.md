Palmer Penguins: A Practice Analysis
================
msant
2026-10-16

A small analysis for practicing the git workflow. Each section is short
and independent, so it’s easy to tweak one piece per commit.

Setup: install any required packages that aren’t already installed
(packages you already have are skipped, so this is safe to re-run):

``` r
pkgs <- c("palmerpenguins", "ggplot2", "dplyr", "knitr")
missing <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(missing) > 0) install.packages(missing)
```

``` r
library(palmerpenguins)
library(ggplot2)
library(dplyr)
```

## 1. The data

Measurements for 344 penguins of three species on three islands in
Antarctica’s Palmer Archipelago.

``` r
glimpse(penguins)
```

    ## Rows: 344
    ## Columns: 8
    ## $ species           <fct> Adelie, Adelie, Adelie, Adelie, Adelie, Adelie, Adel…
    ## $ island            <fct> Torgersen, Torgersen, Torgersen, Torgersen, Torgerse…
    ## $ bill_length_mm    <dbl> 39.1, 39.5, 40.3, NA, 36.7, 39.3, 38.9, 39.2, 34.1, …
    ## $ bill_depth_mm     <dbl> 18.7, 17.4, 18.0, NA, 19.3, 20.6, 17.8, 19.6, 18.1, …
    ## $ flipper_length_mm <int> 181, 186, 195, NA, 193, 190, 181, 195, 193, 190, 186…
    ## $ body_mass_g       <int> 3750, 3800, 3250, NA, 3450, 3650, 3625, 4675, 3475, …
    ## $ sex               <fct> male, female, female, NA, female, male, female, male…
    ## $ year              <int> 2007, 2007, 2007, 2007, 2007, 2007, 2007, 2007, 2007…

## 2. Cleaning

Drop penguins with any missing values.

``` r
penguins_clean <- penguins |>
  na.omit()
 
nrow(penguins) - nrow(penguins_clean)  # rows dropped
```

    ## [1] 11

## 3. Summary table

Average measurements by species.

``` r
species_summary <- penguins_clean |>
  group_by(species) |>
  summarise(
    n = n(),
    mean_mass_g = mean(body_mass_g),
    mean_bill_mm = mean(bill_length_mm),
    mean_flipper_mm = mean(flipper_length_mm)
  )
 
knitr::kable(species_summary, digits = 1)
```

| species   |   n | mean_mass_g | mean_bill_mm | mean_flipper_mm |
|:----------|----:|------------:|-------------:|----------------:|
| Adelie    | 146 |      3706.2 |         38.8 |           190.1 |
| Chinstrap |  68 |      3733.1 |         48.8 |           195.8 |
| Gentoo    | 119 |      5092.4 |         47.6 |           217.2 |

## 4. Plot

Body mass rises with flipper length, and the three species sit in fairly
distinct clusters.

``` r
# Dark-mode palette: colorblind-safe hues stepped for a dark background
dark_bg    <- "#1a1a19"
text_main  <- "#f0efec"
text_muted <- "#c3c2b7"
grid_col   <- "#33332f"
species_colors <- c(Adelie = "#3987e5", Chinstrap = "#d95926", Gentoo = "#199e70")
species_shapes <- c(Adelie = 21, Chinstrap = 24, Gentoo = 22)  # circle, triangle, square

# Label each cluster directly, placed by hand in open space next to it
# (Adelie and Chinstrap overlap, so their labels sit off to the sides)
species_labels <- data.frame(
  species = c("Adelie", "Chinstrap", "Gentoo"),
  flipper_length_mm = c(176, 207, 215),
  body_mass_g = c(4300, 3150, 6450)
)

ggplot(penguins_clean, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(
    aes(fill = species, shape = species),
    color = dark_bg, stroke = 0.6, size = 3, alpha = 0.85
  ) +
  geom_text(
    data = species_labels,
    aes(label = species),
    color = text_main, fontface = "bold", size = 4.2
  ) +
  scale_fill_manual(values = species_colors) +
  scale_shape_manual(values = species_shapes) +
  scale_y_continuous(labels = scales::label_comma(suffix = " g")) +
  scale_x_continuous(labels = scales::label_number(suffix = " mm")) +
  labs(
    title = "Bigger flippers, heavier penguins",
    subtitle = "Body mass vs. flipper length for 333 penguins in the Palmer Archipelago",
    x = "Flipper length",
    y = "Body mass",
    fill = "Species",
    shape = "Species",
    caption = "Data: palmerpenguins (Gorman, Williams & Fraser, 2014)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background   = element_rect(fill = dark_bg, color = NA),
    panel.background  = element_rect(fill = dark_bg, color = NA),
    panel.grid.major  = element_line(color = grid_col, linewidth = 0.4),
    panel.grid.minor  = element_blank(),
    text              = element_text(color = text_main),
    axis.text         = element_text(color = text_muted),
    axis.title        = element_text(color = text_muted),
    axis.title.x      = element_text(margin = margin(t = 10)),
    axis.title.y      = element_text(margin = margin(r = 10)),
    plot.title        = element_text(face = "bold", size = 18),
    plot.subtitle     = element_text(color = text_muted, margin = margin(b = 15)),
    plot.caption      = element_text(color = text_muted, size = 9, hjust = 0),
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    legend.position   = "top",
    legend.justification = "left",
    legend.text       = element_text(color = text_main),
    legend.title      = element_text(color = text_muted),
    legend.key        = element_rect(fill = dark_bg, color = NA),
    plot.margin       = margin(20, 20, 15, 20)
  ) +
  guides(fill = guide_legend(override.aes = list(size = 4, alpha = 1)))
```

![](analysis_files/figure-gfm/r%20plot-1.png)<!-- -->

## 5. A simple regression

How much heavier is a penguin for each extra millimeter of flipper,
holding species fixed?

``` r
model <- lm(body_mass_g ~ flipper_length_mm + species, data = penguins_clean)
summary(model)
```

    ## 
    ## Call:
    ## lm(formula = body_mass_g ~ flipper_length_mm + species, data = penguins_clean)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -898.8 -252.0  -24.8  229.8 1191.6 
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       -4013.18     586.25  -6.846 3.74e-11 ***
    ## flipper_length_mm    40.61       3.08  13.186  < 2e-16 ***
    ## speciesChinstrap   -205.38      57.57  -3.568 0.000414 ***
    ## speciesGentoo       284.52      95.43   2.981 0.003083 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 373.3 on 329 degrees of freedom
    ## Multiple R-squared:  0.787,  Adjusted R-squared:  0.7851 
    ## F-statistic: 405.3 on 3 and 329 DF,  p-value: < 2.2e-16

## 6. Notes

Ideas to add later, one commit at a time:

- Split the plot by sex with `facet_wrap(~ sex)`
- Add a second summary table by island
- Add `sex` to the regression
