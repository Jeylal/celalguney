library(tidyverse)
library(readxl)
library(ggrepel)

# ============================================================
# Data import
# ============================================================
# Sheet "siot" = symmetric input-output table at basic prices
# Rows 6-56 (in the raw tibble) = 51 industry sectors
# Cols 3-53  = inter-industry Z matrix
# Col  67    = total household final consumption (HFCE)
# Row  61    = total output by sector (x)

siot_raw <- read_excel("IO_table_ofs_2017.xlsx", sheet = "siot", col_names = TRUE)

n <- 51   # number of sectors

sector_codes <- siot_raw[[1]][6:56]   # NACE codes
sector_names <- siot_raw[[2]][6:56]   # full sector names

# Inter-industry flow matrix Z (51 × 51)
Z <- siot_raw[6:56, 3:53] |>
  mutate(across(everything(), as.numeric)) |>
  as.matrix()
rownames(Z) <- sector_codes
colnames(Z) <- sector_codes

# Total output vector x
x <- siot_raw[61, 3:53] |> unlist() |> as.numeric()

# Household final consumption by sector (HFCE weights)
hfce <- siot_raw[6:56, 67] |> unlist() |> as.numeric()


# ============================================================
# Leontief quantity model
# ============================================================
# Technical coefficient matrix A = Z * x_hat^{-1}
A <- Z %*% diag(1 / x, ncol = n)

# Leontief inverse L = (I - A)^{-1}
L <- solve(diag(n) - A)
rownames(L) <- sector_codes
colnames(L) <- sector_codes

# Forward and backward linkages
forward_linkages  <- rowSums(L)
backward_linkages <- colSums(L)

linkages <- tibble(
  sector      = sector_codes,
  sector_name = sector_names,
  forward     = forward_linkages,
  backward    = backward_linkages
)


# ============================================================
# Leontief price model
# ============================================================
# Price model: p = (I - A')^{-1} v = t(L) v
# Column j of t(L) = price changes in all sectors from a 100%
# shock originating in sector j.

L_price <- t(L)
rownames(L_price) <- sector_codes
colnames(L_price) <- sector_codes


# ============================================================
# Synthetic CPI impact
# ============================================================
# Weights = HFCE shares; CPI_j = w' %*% L_price[, j]

w          <- hfce / sum(hfce)
cpi_impact <- as.numeric(t(w) %*% L_price)

cpi_results <- tibble(
  sector      = sector_codes,
  sector_name = sector_names,
  cpi_impact  = cpi_impact,
  hfce_share  = w
) |>
  arrange(desc(cpi_impact))

cpi_results


# ============================================================
# Chart 1: CPI impact by sector (bar chart)
# ============================================================
cpi_plot <- cpi_results |>
  filter(cpi_impact > 0) |>
  ggplot(aes(x = cpi_impact,
             y = reorder(sector_name, cpi_impact))) +
  geom_col() +
  labs(
    title    = "CPI impact of a 100% price increase by sector",
    subtitle = "Switzerland 2017 — weighted by HFCE shares",
    x        = "Synthetic CPI change",
    y        = NULL
  ) +
  theme_minimal(base_size = 11)

cpi_plot

ggsave("IO_cpi_impact_ofs2017.png", cpi_plot,
       width = 10, height = 8, dpi = 300)


# ============================================================
# Chart 2: Normalised forward/backward linkage quadrant
# ============================================================
linkages_plot <- linkages |>
  mutate(
    bl_norm = backward / mean(backward),
    fl_norm = forward  / mean(forward)
  ) |>
  left_join(select(cpi_results, sector, cpi_impact), by = "sector") |>
  slice_max(bl_norm + fl_norm, n = 15) |>
  ggplot(aes(x = bl_norm, y = fl_norm,
             size = cpi_impact, label = sector_name)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey60") +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey60") +
  geom_point(alpha = 0.6) +
  geom_text_repel(size = 2.8, max.overlaps = 20) +
  annotate("text", x = 1.55, y = 1.55, label = "Key sectors",
           colour = "grey40", size = 3.2, fontface = "italic") +
  annotate("text", x = 0.5,  y = 1.55, label = "Base sectors",
           colour = "grey40", size = 3.2, fontface = "italic") +
  annotate("text", x = 1.55, y = 0.5,  label = "Final demand sectors",
           colour = "grey40", size = 3.2, fontface = "italic") +
  annotate("text", x = 0.5,  y = 0.5,  label = "Independent sectors",
           colour = "grey40", size = 3.2, fontface = "italic") +
  scale_size_continuous(range = c(1, 8), name = "CPI impact") +
  labs(
    title    = "Forward and backward linkages by sector",
    subtitle = "Switzerland 2017 — normalised by cross-sectoral mean, point size = CPI impact",
    x        = "Backward linkage (normalised)",
    y        = "Forward linkage (normalised)"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

linkages_plot

ggsave("IO_linkages_ofs2017.png", linkages_plot,
       width = 10, height = 7, dpi = 300)
