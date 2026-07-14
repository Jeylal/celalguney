rm(list = ls())
library(tidyverse)
library(readr)

CHE2022ttl <- read_csv("CHE2022ttl.csv") |> 
  column_to_rownames(var = "...1")

dim(CHE2022ttl)

n = 50 # number of sectors

Z = CHE2022ttl[1:n, 1:n] |> # inter-industry transaction matrix (n x n only)
  as.matrix()

X = CHE2022ttl["OUTPUT", 1:n] |> # output vector
  as.vector() |>
  as.numeric()

X_safe = ifelse(X == 0, 1, X) # to avoid dividing by zero

A = sweep(Z, 2, X_safe, "/") # technical coefficient matrix: a_ij = z_ij / x_j

hfce <- as.numeric(CHE2022ttl[1:n, "HFCE"])

L = solve(diag(n) - t(A)) # price Leontief matrix: (I - A')^-1

# ---------------------------------------------------------------------------
# Exogeneity structure (following Weber et al. 2024)
#
# Always-exogenous sectors: commodity extraction and finance/real estate,
# whose prices are demand- or rent-determined rather than cost-determined.
# Mapped from Weber et al.'s asterisked BEA sectors to OECD ISIC Rev.4:
#
#   B05  Mining of coal and lignite          ↔  212* Mining excl. oil & gas
#   B06  Crude petroleum & natural gas       ↔  211* Oil & gas extraction
#   B07  Mining of metal ores               ↔  212* Mining excl. oil & gas
#   B08  Other mining and quarrying          ↔  212* Mining excl. oil & gas
#   K    Financial and insurance activities  ↔  521CI*, 523*, 524*, 525*
#   L    Real estate activities              ↔  HS* Housing + ORE* Other real estate
#
# Borderline cases resolved: M, O, Q, R → endogenous
# ---------------------------------------------------------------------------

sector_codes <- colnames(CHE2022ttl)[1:n]

always_exo_codes <- c("B05", "B06", "B07", "B08", "K", "L")
always_exo_idx   <- which(sector_codes %in% always_exo_codes)

# ---------------------------------------------------------------------------
# Consumption shares and forward linkages
# ---------------------------------------------------------------------------

c_share      <- hfce / sum(hfce)
fwd_linkages <- rowSums(solve(diag(n) - A))   # row sums of (I - A)^{-1}

# ---------------------------------------------------------------------------
# Simulation: unit price shock (s = 1) to each sector
#
# For sector x:
#   exo set  = always_exo_idx ∪ {x}
#   endo set = complement
#   ΔP_E     = (I - A'_EE)^{-1}  A'_XE  e_x
#   IP_dir   = c_x
#   IP_ind   = c_E' ΔP_E
#   IP_tot   = IP_dir + IP_ind
#
# A unit shock (s = 1) gives the structural CPI multiplier per 100% price
# increase. Rankings are invariant to shock size, which can be applied later
# by multiplying IP_tot by the actual sector-level price change.
# ---------------------------------------------------------------------------

A_prime <- t(A)

results <- map_dfr(seq_len(n), function(x) {

  exo_idx  <- sort(union(always_exo_idx, x))
  endo_idx <- setdiff(seq_len(n), exo_idx)
  e        <- length(endo_idx)

  A_EE  <- A_prime[endo_idx, endo_idx]
  A_XE  <- A_prime[endo_idx, exo_idx, drop = FALSE]  # e × |exo|
  x_pos <- which(exo_idx == x)                        # column of x in A_XE

  delta_PE <- as.numeric(solve(diag(e) - A_EE) %*% A_XE[, x_pos])

  ip_dir <- as.numeric(c_share[x])
  ip_ind <- as.numeric(c_share[endo_idx] %*% delta_PE)

  tibble(
    index    = as.integer(x),
    code     = sector_codes[x],
    cons_shr = ip_dir,
    fwd_link = as.numeric(fwd_linkages[x]),
    IP_dir   = ip_dir,
    IP_ind   = ip_ind,
    IP_tot   = ip_dir + ip_ind
  )
})

results <- results |> arrange(desc(IP_tot))

# ---------------------------------------------------------------------------
# Figure 2 (adapted from Weber et al. 2024)
# X: forward linkages  |  Y: indirect CPI multiplier (proxy for price volatility
# × forward linkages; the original uses sector price volatility σ which is not
# available for Switzerland here)  |  Size: consumption share
# ---------------------------------------------------------------------------

sector_plot_labels <- c(
  A01="Crops & livestock", A02="Forestry", A03="Fishing",
  B05="Coal mining", B06="Oil & gas extr.", B07="Metal ore mining",
  B08="Other mining", B09="Mining support",
  C10T12="Food & beverages", C13T15="Textiles", C16="Wood",
  C17_18="Paper & printing", C19="Petroleum products",
  C20="Chemicals", C21="Pharmaceuticals", C22="Rubber & plastics",
  C23="Non-metallic minerals", C24A="Iron & steel",
  C24B="Non-ferrous metals", C25="Fabricated metals",
  C26="Electronics", C27="Electrical equip.", C28="Machinery",
  C29="Motor vehicles", C301="Ships", C302T309="Other transport equip.",
  C31T33="Furniture & misc. mfg",
  D="Electricity & gas", E="Water & waste", F="Construction",
  G="Wholesale & retail trade", H49="Land transport",
  H50="Water transport", H51="Air transport",
  H52="Warehousing", H53="Postal services",
  I="Accommodation & food svcs", J58T60="Publishing & broadcasting",
  J61="Telecom", J62_63="IT services",
  K="Finance & insurance", L="Real estate",
  M="Professional services", N="Admin & support",
  O="Public admin", P="Education", Q="Health & social work",
  R="Arts & recreation", S="Other services", T="Households"
)

top_n_sig <- 8
sig_codes <- results |> slice_head(n = top_n_sig) |> pull(code)

plot_df <- results |>
  mutate(
    label     = sector_plot_labels[code],
    sig       = code %in% sig_codes,
    sig_label = if_else(sig, label, NA_character_)
  )

mean_fwd  <- mean(plot_df$fwd_link)
mean_iind <- mean(plot_df$IP_ind)

library(ggrepel)

fig2 <- ggplot(plot_df, aes(x = fwd_link, y = IP_ind)) +
  geom_vline(xintercept = mean_fwd,  linetype = "dashed", colour = "grey60", linewidth = 0.4) +
  geom_hline(yintercept = mean_iind, linetype = "dashed", colour = "grey60", linewidth = 0.4) +
  geom_point(aes(size = cons_shr, colour = sig), alpha = 0.75) +
  geom_text_repel(
    aes(label = sig_label),
    size = 3, fontface = "bold",
    box.padding = 0.5, min.segment.length = 0.2,
    na.rm = TRUE
  ) +
  scale_colour_manual(
    values = c("TRUE" = "#5C2D91", "FALSE" = "#6BAED6"),
    labels = c("TRUE" = "Systemically significant", "FALSE" = "Other"),
    name   = NULL
  ) +
  scale_size_area(
    max_size = 14,
    labels   = scales::percent_format(accuracy = 1),
    name     = "Share of personal\nconsumption"
  ) +
  annotate("text", x = mean_fwd + 0.15, y = max(plot_df$IP_ind) * 0.97,
           label = "Average forward linkage", hjust = 0, size = 2.8, colour = "grey45") +
  annotate("text", x = max(plot_df$fwd_link) * 0.97, y = mean_iind + 0.002,
           label = "Average indirect impact", hjust = 1, size = 2.8, colour = "grey45") +
  labs(
    x = "Forward linkages  —  row sums of (I\u2212A)\u207b\u00b9",
    y = "Indirect CPI inflation impact  (unit shock)",
    title = "Systemically significant prices  \u2014  Switzerland 2022",
    subtitle = paste0(
      "Bubble size = household consumption share  \u2022  ",
      "Purple = top ", top_n_sig, " sectors by total inflation impact\n",
      "Y-axis: indirect CPI multiplier (structural proxy; Weber et al. 2024 use sector price volatility)"
    ),
    caption = "Source: OECD IO Table CHE 2022 (total flows). Method: Weber et al. (2024)."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position  = "right",
    plot.subtitle    = element_text(size = 8, colour = "grey35"),
    plot.caption     = element_text(size = 8, colour = "grey50"),
    panel.grid.minor = element_blank()
  )
fig2
ggsave("figure2_switzerland.png", fig2, width = 9, height = 6, dpi = 150)

# ---------------------------------------------------------------------------
# Figure 1 (adapted from Weber et al. 2024)
# Stacked horizontal bar chart: direct (yellow) + indirect (purple) CPI impact
# Top N sectors ranked by IP_tot, unit price shock (s = 1)
# ---------------------------------------------------------------------------

library(tidyr)

top_n <- 15

bar_df <- results |>
  slice_head(n = top_n) |>
  mutate(label = sector_plot_labels[code]) |>
  select(label, IP_dir, IP_ind, IP_tot) |>
  mutate(label = factor(label, levels = rev(label))) |>
  pivot_longer(c(IP_dir, IP_ind), names_to = "component", values_to = "value") |>
  mutate(component = factor(component,
    levels = c("IP_dir", "IP_ind"),
    labels = c("Direct effect", "Indirect effect")
  ))

fig1 <- ggplot(bar_df, aes(x = value, y = label, fill = component)) +
  geom_col(width = 0.7) +
  scale_fill_manual(
    values = c("Direct effect" = "#F5C518", "Indirect effect" = "#5C2D91"),
    name = NULL
  ) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = "CPI inflation impact (unit price shock)",
    y = NULL,
    title = "Inflation impact by sector \u2014 Switzerland 2022",
    subtitle = paste0(
      "Top ", top_n, " sectors ranked by total CPI impact of a unit (100%) price shock\n",
      "Direct = sector\u2019s own consumption share; Indirect = supply-chain propagation"
    ),
    caption = "Source: OECD IO Table CHE 2022 (total flows). Method: Weber et al. (2024)."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position    = "bottom",
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.subtitle      = element_text(size = 8.5, colour = "grey35"),
    plot.caption       = element_text(size = 8,   colour = "grey50"),
    axis.text.y        = element_text(size = 9)
  )

fig1
ggsave("figure1_switzerland.png", fig1, width = 8, height = 7, dpi = 150)









