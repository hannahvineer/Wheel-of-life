# Plotting helpers for the Wheel of Life app. Files in R/ are sourced
# automatically by shiny::runApp(), and can also be sourced on their own to
# produce a wheel from a plain R script.

library(ggplot2)

`%||%` <- function(x, y) if (is.null(x)) y else x

preset_domains <- list(
  life = c(
    "Physical Health",
    "Mental & Emotional Wellbeing",
    "Career",
    "Finances",
    "Romance & Partner",
    "Family",
    "Friends & Social Life",
    "Personal Growth & Learning",
    "Fun & Recreation",
    "Home & Environment",
    "Spirituality & Purpose",
    "Community & Contribution"
  ),
  career = c(
    "Pay & Benefits",
    "Career Progression",
    "Skills & Development",
    "Meaning & Purpose",
    "Challenge & Stimulation",
    "Recognition & Feedback",
    "Working Relationships",
    "Work-Life Balance & Flexibility"
  )
)

preset_titles <- c(life = "Life Audit", career = "Career Audit")

# One distinct colour per domain.
domain_colours <- function(n) {
  grDevices::hcl.colors(n, palette = "Dark 3")
}

# data: data.frame with columns domain, current, desired (0-10).
plot_wheel <- function(data, title = "Life Audit", wrap_width = 14) {
  n <- nrow(data)
  data$domain <- factor(data$domain, levels = data$domain)
  labels <- vapply(levels(data$domain),
                   function(x) paste(strwrap(x, wrap_width), collapse = "\n"),
                   character(1))

  long <- rbind(
    data.frame(domain = data$domain, score = data$current, type = "Current"),
    data.frame(domain = data$domain, score = data$desired, type = "Desired")
  )
  long$type <- factor(long$type, levels = c("Current", "Desired"))

  ggplot(long, aes(x = domain, y = score, fill = domain)) +
    # Opaque bar for current satisfaction.
    geom_col(data = long[long$type == "Current", ],
             aes(alpha = type), width = 1, colour = "white", linewidth = 0.4) +
    # Translucent bar with a dashed outline for desired satisfaction, drawn on
    # top so it stays visible even when desired < current.
    geom_col(data = long[long$type == "Desired", ],
             aes(alpha = type), width = 1, colour = "grey20",
             linetype = "dashed", linewidth = 0.5) +
    geom_vline(xintercept = seq(0.5, n + 0.5, by = 1),
               colour = "grey70", linewidth = 0.3) +
    annotate("label", x = 0.5, y = seq(2, 10, by = 2), label = seq(2, 10, by = 2),
             size = 3, colour = "grey40", label.size = 0, fill = "white", alpha = 0.8) +
    scale_y_continuous(limits = c(0, 10), breaks = 0:10, expand = c(0, 0)) +
    scale_x_discrete(labels = labels) +
    scale_fill_manual(values = domain_colours(n), guide = "none") +
    scale_alpha_manual(
      name = NULL,
      values = c(Current = 1, Desired = 0.3),
      labels = c(Current = "Current satisfaction", Desired = "Desired satisfaction"),
      drop = FALSE
    ) +
    guides(alpha = guide_legend(override.aes = list(fill = "grey30", colour = NA))) +
    coord_polar(clip = "off") +
    labs(title = title, x = NULL, y = NULL) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = "grey85"),
      panel.grid.minor = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_text(face = "bold", size = 11),
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, face = "bold", size = 18)
    )
}
