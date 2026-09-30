# =============================================================================
# Group Dynamics Manuscript Figure 1 
# Natalia Karadimitriou September 2026
# =============================================================================

library(ggplot2)
library(patchwork)

font_family <- "Times New Roman"
pt <- function(x) x * (25.4 / 72.27)
base_pt <- 12

col_known <- "#009E73"
col_size  <- "#D55E00"
col_curve <- "grey15"
col_note  <- "grey35"

gsl <- function(d) 4.709 * d - 0.037 * d^2

lab <- function(x, y, text, colour, hjust = 0.5, fontface = "plain") {
  data.frame(x = x, y = y, text = text, colour = colour, hjust = hjust,
             fontface = fontface, stringsAsFactors = FALSE)
}

lev_known <- "GSL manipulated"
lev_size  <- "Anemone size manipulated"

shared_theme <- theme_classic(base_size = base_pt, base_family = font_family) +
  theme(
    text             = element_text(family = font_family, size = base_pt),
    plot.title       = element_text(size = base_pt, face = "bold", hjust = 0,
                                    margin = margin(b = 8)),
    legend.position  = "bottom",
    legend.title     = element_blank(),
    legend.margin    = margin(t = -4),
    legend.key.width = unit(1.1, "lines"),
    axis.line        = element_line(linewidth = 0.4, colour = "black"),
    axis.ticks       = element_line(linewidth = 0.4, colour = "black"),
    plot.margin      = margin(6, 10, 6, 6)
  )

x_min <- 0; x_max <- 68
y_min <- 0; y_max <- 195

# Panel A: large, wild anemones 

d0a <- 50; shift_d <- 10; shift_g <- 30
centre_a <- data.frame(d0 = d0a, g0 = gsl(d0a))

# Drawn as a single solid line throughout -- conceptual figure
# illustrating the idea being tested,  Buston (2003) reports a mean
# +/- SD anemone diameter (50 +/- 8 cm, n = 71), not a min/max, so there's no
# dependable value to place an "extrapolated" cutoff at.

curve_a <- data.frame(d = seq(4, x_max, length.out = 500))
curve_a$gsl <- gsl(curve_a$d)

arms_a <- data.frame(
  d0 = centre_a$d0, g0 = centre_a$g0,
  d   = c(centre_a$d0, centre_a$d0, centre_a$d0 - shift_d, centre_a$d0 + shift_d),
  gsl = c(centre_a$g0 + shift_g, centre_a$g0 - shift_g, centre_a$g0, centre_a$g0),
  arm = c("vertical", "vertical", "horizontal", "horizontal")
)
arms_a$type <- factor(ifelse(arms_a$arm == "vertical", lev_known, lev_size),
                      levels = c(lev_known, lev_size))
drops_a <- subset(arms_a, arm == "horizontal")
drops_a$curve_y <- gsl(drops_a$d)

annots_a <- rbind(
  lab(2, 178, "High saturation", col_note, 0, "italic"),
  lab(66, 12,  "Low saturation",  col_note, 1, "italic"),
  lab(d0a + 4, centre_a$g0 + shift_g + 6, "Eviction", col_known, 0, "bold"),
  lab(d0a + 4, centre_a$g0 - shift_g - 4, "Recruitment", col_known, 0, "bold"),
  lab(d0a - shift_d, centre_a$g0 + 16, "?", col_size, 0.5, "bold"),
  lab(d0a + shift_d, centre_a$g0 - 18, "?", col_size, 0.5, "bold")
)

text_layer <- function(annots, col) {
  d <- annots[annots$colour == col, ]
  geom_text(data = d, aes(x, y, label = text, hjust = hjust, fontface = fontface),
            colour = col, family = font_family, size = pt(base_pt),
            lineheight = 0.95, show.legend = FALSE)
}

pa <- ggplot() +
  geom_line(data = curve_a, aes(d, gsl), colour = col_curve, linewidth = 0.8) +
  geom_segment(data = drops_a, aes(x = d, xend = d, y = gsl, yend = curve_y),
               colour = col_size, linetype = "dotted", linewidth = 0.5) +
  geom_segment(data = arms_a, aes(x = d0, y = g0, xend = d, yend = gsl, colour = type),
               linewidth = 0.6, arrow = arrow(length = unit(2.2, "mm"), type = "closed", angle = 20)) +
  geom_point(data = centre_a, aes(d0, g0), shape = 21, size = 3, fill = "white",
             colour = col_curve, stroke = 0.8) +
  text_layer(annots_a, col_note) + text_layer(annots_a, col_known) + text_layer(annots_a, col_size) +
  scale_discrete_identity(aesthetics = "fontface") +
  scale_colour_manual(values = setNames(c(col_known, col_size), c(lev_known, lev_size)), name = NULL) +
  scale_x_continuous(limits = c(x_min, x_max), breaks = seq(0, 60, 10), expand = c(0, 0)) +
  scale_y_continuous(limits = c(y_min, y_max), breaks = seq(0, 180, 30), expand = c(0, 0)) +
  labs(title = "A: large, wild anemones (Buston 2003)",
       x = "Mean anemone diameter (cm)", y = "Group standard length, GSL (mm)") +
  shared_theme

# Panel B: small anemones, this study 

d0b <- 25
#curve_max_b <- 55
exp_lo <- 2 * sqrt(52 / pi); exp_hi <- 2 * sqrt(1740 / pi)
centre_b <- data.frame(d0 = d0b, g0 = gsl(d0b))

curve_b <- curve_a
curve_b$gsl <- gsl(curve_b$d)

arms_b <- data.frame(
  d0 = centre_b$d0, g0 = centre_b$g0,
  d   = c(centre_b$d0, centre_b$d0, centre_b$d0 - shift_d, centre_b$d0 + shift_d),
  gsl = c(centre_b$g0 + shift_g, centre_b$g0 - shift_g, centre_b$g0, centre_b$g0),
  arm = c("vertical", "vertical", "horizontal", "horizontal")
)
arms_b$type <- factor(ifelse(arms_b$arm == "vertical", lev_known, lev_size),
                      levels = c(lev_known, lev_size))
drops_b <- subset(arms_b, arm == "horizontal")
drops_b$curve_y <- gsl(drops_b$d)

band_b <- data.frame(xmin = exp_lo, xmax = exp_hi, ymin = 1, ymax = 7)

annots_b <- rbind(
  lab(2, 178, "High saturation", col_note, 0, "italic"),
  lab(66, 26,  "Low saturation",  col_note, 1, "italic"),
  lab(d0b + 3, centre_b$g0 + shift_g + 6, "Eviction", col_known, 0, "bold"),
  lab(d0b + 3, centre_b$g0 - shift_g - 4, "Recruitment", col_known, 0, "bold"),
  lab(d0b - shift_d - 2, centre_b$g0 + 14, "Eviction", col_size, 1, "bold"),
  lab(d0b + shift_d + 2, centre_b$g0 - 14, "Recruitment", col_size, 0, "bold"),
  lab((exp_lo + exp_hi) / 2, 14,
      sprintf("anemone sizes used here (%.0f-%.0f cm)", exp_lo, exp_hi), col_note, 0.5, "italic")
)

pb <- ggplot() +
  geom_rect(data = band_b, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = col_size, alpha = 0.30) +
  geom_line(data = curve_b, aes(d, gsl), colour = col_curve, linewidth = 0.8) +
  geom_segment(data = drops_b, aes(x = d, xend = d, y = gsl, yend = curve_y),
               colour = col_size, linetype = "dotted", linewidth = 0.5) +
  geom_segment(data = arms_b, aes(x = d0, y = g0, xend = d, yend = gsl, colour = type),
               linewidth = 0.6, arrow = arrow(length = unit(2.2, "mm"), type = "closed", angle = 20)) +
  geom_point(data = centre_b, aes(d0, g0), shape = 21, size = 3, fill = "white",
             colour = col_curve, stroke = 0.8) +
  text_layer(annots_b, col_note) + text_layer(annots_b, col_known) + text_layer(annots_b, col_size) +
  scale_discrete_identity(aesthetics = "fontface") +
  scale_colour_manual(values = setNames(c(col_known, col_size), c(lev_known, lev_size)), name = NULL) +
  scale_x_continuous(limits = c(x_min, x_max), breaks = seq(0, 60, 10), expand = c(0, 0)) +
  scale_y_continuous(limits = c(y_min, y_max), breaks = seq(0, 180, 30), expand = c(0, 0)) +
  labs(title = "B: small anemones (this study)",
       x = "Mean anemone diameter (cm)", y = "Group standard length, GSL (mm)") +
  shared_theme
pb
# Combine: one shared y-axis title, one shared x-axis title, one legend 

combined <- (pa + pb) +
  plot_layout(ncol = 2, widths = c(1, 1),
              axis_titles = "collect", guides = "collect") &
  theme(legend.position = "bottom",
        legend.text = element_text(family = font_family, size = base_pt))



ggsave("../Figures/Figure1_combined.png", combined, width = 260, height = 135, units = "mm", dpi = 1200, bg="white")
ggsave("../Figures/Figure1_combined.svg", combined, width = 260, height = 135, units = "mm", dpi = 1200, bg="white")
