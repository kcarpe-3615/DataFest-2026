library(ggplot2)
library(grid)

# ---- Data: pulled from your earlier queries ----
# If anything changes upstream, just rerun the queries and update these numbers
counts <- list(
  total_in_records         = 947685,
  with_encounters          = 359760,
  ever_approached          = 61453,
  approached_and_answered  = 60850,
  approached_only_declined = 603,
  positive_any_domain      = 6056
)

# Per-domain breakdown
domains <- data.frame(
  domain     = c("Housing Stability", "Transportation Needs", "Financial Resource Strain"),
  n_positive = c(1177, 3058, 4444),
  n_negative = c(35678, 58297, 57715)
)
domains$total       <- domains$n_positive + domains$n_negative
domains$pct_positive <- round(domains$n_positive / domains$total * 100, 1)

# Derived exclusions
excl_no_encounter <- counts$total_in_records - counts$with_encounters
excl_never_asked  <- counts$with_encounters - counts$ever_approached

# ---- Color palette (matches your slide deck) ----
BERRY  <- "#6D2E46"
ROSE   <- "#A26769"
SLATE  <- "#3D5A6C"
CREAM  <- "white"
BORDER <- "#E0D8CF"

# ---- Helper to format numbers with commas ----
fmt <- function(n) format(n, big.mark = ",")

# ---- Build the diagram ----
# Strategy: use a blank ggplot canvas with annotation_custom() for boxes and arrows.
# Coordinates run 0-100 horizontally, 0-100 vertically (works like a regular layout grid).

p <- ggplot() +
  xlim(0, 100) + ylim(0, 100) +
  theme_void() +
  theme(plot.background = element_rect(fill = CREAM, color = NA),
        plot.margin = margin(20, 20, 20, 20))

# Title and subtitle
p <- p +
  annotate("text", x = 50, y = 97, label = "Cohort flow: from all SVH patients to analysis subgroups",
           size = 7, fontface = "bold", color = BERRY, family = "serif") +
  annotate("text", x = 50, y = 93, label = "Stormont Vail Health, 2022-2025. Counts are unique patients.",
           size = 3.5, fontface = "italic", color = TEXT)

# Main funnel boxes (centered column on left side of canvas)
# Each box: rectangle + label text + count text
draw_main_box <- function(plot, y_center, label, count, fill_color = "white",
                          text_color = BERRY, label_color = TEXT, border_color = BERRY) {
  plot +
    annotate("rect", xmin = 25, xmax = 55, ymin = y_center - 4, ymax = y_center + 4,
             fill = fill_color, color = border_color, linewidth = 0.6) +
    annotate("text", x = 40, y = y_center + 1.5, label = label,
             size = 3.5, color = label_color) +
    annotate("text", x = 40, y = y_center - 2, label = fmt(count),
             size = 6, fontface = "bold", color = text_color, family = "serif")
}

p <- draw_main_box(p, 87, "All patients in SVH records", counts$total_in_records)
p <- draw_main_box(p, 75, "Had >=1 encounter in 2022-2025", counts$with_encounters)
p <- draw_main_box(p, 63, "Approached for SDoH screening", counts$ever_approached)
p <- draw_main_box(p, 51, "Answered >=1 SDoH question (study population)",
                   counts$approached_and_answered,
                   fill_color = BERRY, text_color = "white", label_color = "#F5EDE6",
                   border_color = BERRY)

# Exclusion boxes (right of main funnel, dashed)
draw_exclusion <- function(plot, y_center, label, count) {
  plot +
    annotate("rect", xmin = 65, xmax = 92, ymin = y_center - 3, ymax = y_center + 3,
             fill = "#F5EDE6", color = ROSE, linewidth = 0.4, linetype = "dashed") +
    annotate("text", x = 78.5, y = y_center + 1, label = label,
             size = 2.8, color = TEXT) +
    annotate("text", x = 78.5, y = y_center - 1.5, label = fmt(count),
             size = 4, fontface = "bold", color = ROSE) +
    # Arrow from main funnel to exclusion box
    annotate("segment", x = 55, xend = 64, y = y_center, yend = y_center,
             color = ROSE, linewidth = 0.4, linetype = "dashed",
             arrow = arrow(length = unit(0.15, "cm"), type = "closed"))
}

p <- draw_exclusion(p, 87, "Excluded: no encounter in window", excl_no_encounter)
p <- draw_exclusion(p, 75, "Excluded: never approached for SDoH", excl_never_asked)
p <- draw_exclusion(p, 63, "Excluded: only declined / unable", counts$approached_only_declined)

# Vertical arrows between main boxes
arrow_seg <- function(plot, y_top, y_bot) {
  plot +
    annotate("segment", x = 40, xend = 40, y = y_top, yend = y_bot,
             color = TEXT, linewidth = 0.5,
             arrow = arrow(length = unit(0.2, "cm"), type = "closed"))
}

p <- arrow_seg(p, 83, 79.5)
p <- arrow_seg(p, 71, 67.5)
p <- arrow_seg(p, 59, 55.5)

# Combined positive cohort box
# Combined positive cohort box (expanded for breathing room)
p <- p +
  annotate("rect", xmin = 22, xmax = 58, ymin = 36, ymax = 46,
           fill = "white", color = ROSE, linewidth = 0.6) +
  annotate("text", x = 40, y = 43, label = "Positive on >=1 of 3 focus domains (combined cohort)",
           size = 3.1, color = TEXT) +
  annotate("text", x = 40, y = 39.5, label = fmt(counts$positive_any_domain),
           size = 5.5, fontface = "bold", color = ROSE, family = "serif")
# Arrow from study population to combined cohort
p <- arrow_seg(p, 47, 46.5)

# Three domain boxes at the bottom
draw_domain_box <- function(plot, x_center, domain_row) {
  d <- domain_row
  plot +
    annotate("rect", xmin = x_center - 14, xmax = x_center + 14, ymin = 6, ymax = 30,
             fill = "white", color = SLATE, linewidth = 0.6) +
    annotate("text", x = x_center, y = 27, label = d$domain,
             size = 4, fontface = "bold", color = SLATE) +
    # Vertical drop from combined cohort
    annotate("segment", x = 40, xend = 40, y = 36, yend = 34,
             color = TEXT, linewidth = 0.5) +
    # Positive
    annotate("text", x = x_center - 7, y = 20, label = "Positive", size = 2.8, color = TEXT) +
    annotate("text", x = x_center - 7, y = 16, label = fmt(d$n_positive),
             size = 5, fontface = "bold", color = BERRY, family = "serif") +
    # Negative
    annotate("text", x = x_center + 7, y = 20, label = "Negative", size = 2.8, color = TEXT) +
    annotate("text", x = x_center + 7, y = 16, label = fmt(d$n_negative),
             size = 5, fontface = "bold", color = SLATE, family = "serif") +
    # Pct positive footnote
    annotate("text", x = x_center, y = 10, label = paste0(d$pct_positive, "% positive rate"),
             size = 2.8, fontface = "italic", color = TEXT)
}

p <- draw_domain_box(p, 18, domains[1, ])
p <- draw_domain_box(p, 50, domains[2, ])
p <- draw_domain_box(p, 82, domains[3, ])

# Branching arrows from combined cohort to three domain boxes
p <- p +
  # Vertical drop from combined cohort
  annotate("segment", x = 40, xend = 40, y = 38, yend = 34,
           color = TEXT, linewidth = 0.5) +
  # Horizontal spreader
  annotate("segment", x = 18, xend = 82, y = 34, yend = 34,
           color = TEXT, linewidth = 0.5) +
  # Three vertical drops to each domain box
  annotate("segment", x = 18, xend = 18, y = 34, yend = 30.5,
           color = TEXT, linewidth = 0.5,
           arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
  annotate("segment", x = 50, xend = 50, y = 34, yend = 30.5,
           color = TEXT, linewidth = 0.5,
           arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
  annotate("segment", x = 82, xend = 82, y = 34, yend = 30.5,
           color = TEXT, linewidth = 0.5,
           arrow = arrow(length = unit(0.2, "cm"), type = "closed"))

# Footer notes
p <- p +
  annotate("text", x = 50, y = 2.5,
           label = "Each domain's positive + negative does not equal 60,850 because not every patient was asked every domain question.",
           size = 2.6, color = TEXT) +
  annotate("text", x = 50, y = 0.5,
           label = "Combined cohort (6,056) counts a patient once even if positive on multiple domains.",
           size = 2.6, color = TEXT)

# Save to disk
ggsave("cohort_diagram.png", plot = p, width = 13, height = 9, dpi = 200, bg = CREAM)

# Display it
print(p)