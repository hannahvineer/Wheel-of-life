# Wheel of Life

A Shiny app for building and scoring a personal Wheel of Life.

1. **Set up the wheel**: use the standard 12 life areas, or enter your own (one per line, minimum 3).
2. **Rate each area from 0 to 10**: set how satisfied you are now (*current*) and how satisfied you want to be (*desired*).
3. **Read the wheel**: current scores are solid colour. Desired scores are translucent with a dashed outline. Download the wheel as a PNG, or the scores as a CSV.

Default areas: Physical Health, Mental & Emotional Wellbeing, Career, Finances,
Romance & Partner, Family, Friends & Social Life, Personal Growth & Learning,
Fun & Recreation, Home & Environment, Spirituality & Purpose, Community & Contribution.

## Running

```r
install.packages(c("shiny", "bslib", "ggplot2"))
shiny::runApp()
```

The plotting code is in `R/wheel.R`. You can also use it outside Shiny:

```r
source("R/wheel.R")
scores <- data.frame(domain = default_domains, current = 5, desired = 8)
plot_wheel(scores)
```
