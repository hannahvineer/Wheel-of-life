# Life Audit tool

A Shiny app for building and scoring a personal Life Audit or Career Audit wheel.

1. **Set up the wheel**: pick a preset (whole life or career & work), or enter your own areas (one per line, minimum 3).
2. **Rate each area from 0 to 10** in the table: how satisfied you are now and how satisfied you want to be.
3. **Read the wheel**: current scores are solid colour. Desired scores are translucent with a dashed outline. Download the wheel as a PNG, or the scores as a CSV.

Preset area lists:

- **Whole life (12):** Physical Health, Mental & Emotional Wellbeing, Career,
  Finances, Romance & Partner, Family, Friends & Social Life, Personal Growth &
  Learning, Fun & Recreation, Home & Environment, Spirituality & Purpose,
  Community & Contribution.
- **Career & work (8):** Pay & Benefits, Career Progression, Skills &
  Development, Meaning & Purpose, Challenge & Stimulation, Recognition &
  Feedback, Working Relationships, Work-Life Balance & Flexibility.

## Running

```r
install.packages(c("shiny", "bslib", "ggplot2"))
shiny::runApp()
```

The plotting code is in `R/wheel.R`. You can also use it outside Shiny:

```r
source("R/wheel.R")
scores <- data.frame(domain = preset_domains$career, current = 5, desired = 8)
plot_wheel(scores)
```
