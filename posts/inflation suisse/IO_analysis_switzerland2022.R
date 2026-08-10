library(tidyverse)
library(ggrepel)
rm(list = ls())


CHE2022ttl <- read_csv("CHE2022ttl.csv") |> 
  column_to_rownames(var = "...1")

# extract inter-industry transaction flow matrix
Z = CHE2022ttl[1:50, 1:50] |> 
  as.matrix()

# extract total output of each sector
x = CHE2022ttl[1:50, "TOTAL"] |> 
  as.vector()

x_hat_inverse <- diag(1/x, ncol = 50) # x^hat^-1

A = Z%*%x_hat_inverse

A[, 4] <- 0

# computation check: Ax + f should sum to x, with f the sum of final demand external to Z 

f = CHE2022ttl[, 51:58] |> 
  as.matrix() |> 
  apply(1, sum) |> 
  as.matrix()

f = f[1:50]

x_check <- A%*%x + f # does not really sum to x...


# Compute leontief matrix (I-A)^-1

L = solve(diag(50)- A)
L[, 4] = 0
L[,50] = 0

sectors = rownames(CHE2022ttl)

rownames(L) <- sectors[1:50]

# forward linkages: rowsums of the L

forward_linkages = apply(L,1, sum) |> as.matrix()
backward_linkages = apply(L,2, sum) |> as.matrix()

linkages <- cbind(forward_linkages, backward_linkages) |> 
  as_tibble() |> 
  rename(
    "forward" = V1,
    "backward" = V2
  ) |> 
  mutate(
    sector = sectors[1:50]
  )


linkages_plot <- linkages |>
  mutate(
    bl_norm = backward / mean(backward),
    fl_norm = forward  / mean(forward)
  ) |>
  left_join(sector_names, by = "sector") |>
  left_join(select(cpi_results, sector, cpi_impact), by = "sector") |>
  slice_max(bl_norm + fl_norm, n = 15) |>
  ggplot(aes(x = bl_norm, y = fl_norm, size = cpi_impact, label = sector_name)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey60") +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey60") +
  geom_point(alpha = 0.6) +
  geom_text_repel(size = 2.8, max.overlaps = 20) +
  annotate("text", x = 1.6,  y = 1.6,  label = "Key sectors",          colour = "grey40", size = 3.2, fontface = "italic") +
  annotate("text", x = 0.45, y = 1.6,  label = "Base sectors",         colour = "grey40", size = 3.2, fontface = "italic") +
  annotate("text", x = 1.6,  y = 0.45, label = "Final demand sectors", colour = "grey40", size = 3.2, fontface = "italic") +
  annotate("text", x = 0.45, y = 0.45, label = "Independent sectors",  colour = "grey40", size = 3.2, fontface = "italic") +
  scale_size_continuous(range = c(1, 8), name = "CPI impact") +
  labs(
    title    = "Forward and backward linkages by sector",
    subtitle = "Normalised by cross-sectoral mean — point size = synthetic CPI impact",
    x        = "Backward linkage (normalised)",
    y        = "Forward linkage (normalised)"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

linkages_plot

ggsave("IO_linkages_switzerland2022.png", linkages_plot,
       width = 10, height = 7, dpi = 300)


# ============================================================
# Leontief Price Model
# ============================================================
# Price model: p = (I - A')^{-1} v
# (I - A')^{-1} = t(L), since t((I-A)^{-1}) = (I - A')^{-1}
#
# Simulation: a 100% price increase in sector j propagates
# through A' and raises prices in all upstream-using sectors.
# The effect on all sectors from a shock in j = column j of t(L).

L_price <- t(L)
rownames(L_price) <- sectors[1:50]
colnames(L_price) <- sectors[1:50]

# Tidy: price_change = % price rise in affected_sector
# caused by a 100% cost shock originating in shocked_sector
price_shocks <- L_price |>
  as_tibble(rownames = "affected_sector") |>
  pivot_longer(
    -affected_sector,
    names_to  = "shocked_sector",
    values_to = "price_change"
  )

# Economy-wide impact per shocked sector
# (sum of price changes across all sectors)
sector_impact <- price_shocks |>
  group_by(shocked_sector) |>
  summarise(total_price_impact = sum(price_change)) |>
  arrange(desc(total_price_impact))

sector_impact |>
  ggplot(aes(x = total_price_impact,
             y = reorder(shocked_sector, total_price_impact))) +
  geom_col() +
  labs(
    title = "Economy-wide price impact of a 100% price increase by sector",
    x = "Sum of price changes across all sectors",
    y = NULL
  ) +
  theme_minimal(base_size = 11)

# Helper: top sectors most affected by a given shock
top_affected <- function(shocked, n = 10) {
  price_shocks |>
    filter(shocked_sector == shocked) |>
    arrange(desc(price_change)) |>
    slice_head(n = n)
}


# ============================================================
# Synthetic Consumer Price Index (CPI) impact
# ============================================================
# Weights = household final consumption expenditure (HFCE) shares
# CPI impact of shock in sector j = w' %*% L_price[, j]
# i.e. the consumption-weighted average of all price changes
# triggered by a 100% cost increase in sector j.

hfce <- CHE2022ttl[1:50, "HFCE"] |> as.numeric()
w    <- hfce / sum(hfce)   # consumption share weights

cpi_impact <- as.numeric(t(w) %*% L_price)

# OECD ICIO sector name lookup
sector_names <- tribble(
  ~sector,          ~sector_name,
  "TTL_A01",        "Crop & animal production",
  "TTL_A02",        "Forestry & logging",
  "TTL_A03",        "Fishing & aquaculture",
  "TTL_B05",        "Coal mining",
  "TTL_B06",        "Crude petroleum & gas",
  "TTL_B07",        "Metal ore mining",
  "TTL_B08",        "Other mining & quarrying",
  "TTL_B09",        "Mining support services",
  "TTL_C10T12",     "Food, beverages & tobacco",
  "TTL_C13T15",     "Textiles & apparel",
  "TTL_C16",        "Wood products",
  "TTL_C17_18",     "Paper & printing",
  "TTL_C19",        "Refined petroleum",
  "TTL_C20",        "Chemicals",
  "TTL_C21",        "Pharmaceuticals",
  "TTL_C22",        "Rubber & plastics",
  "TTL_C23",        "Non-metallic minerals",
  "TTL_C24A",       "Iron & steel",
  "TTL_C24B",       "Other basic metals",
  "TTL_C25",        "Fabricated metal products",
  "TTL_C26",        "Computers & electronics",
  "TTL_C27",        "Electrical equipment",
  "TTL_C28",        "Machinery & equipment",
  "TTL_C29",        "Motor vehicles",
  "TTL_C301",       "Ships & boats",
  "TTL_C302T309",   "Other transport equipment",
  "TTL_C31T33",     "Furniture & other manufacturing",
  "TTL_D",          "Electricity & gas supply",
  "TTL_E",          "Water & waste management",
  "TTL_F",          "Construction",
  "TTL_G",          "Wholesale & retail trade",
  "TTL_H49",        "Land transport",
  "TTL_H50",        "Water transport",
  "TTL_H51",        "Air transport",
  "TTL_H52",        "Warehousing & transport support",
  "TTL_H53",        "Postal & courier services",
  "TTL_I",          "Accommodation & food services",
  "TTL_J58T60",     "Publishing & broadcasting",
  "TTL_J61",        "Telecommunications",
  "TTL_J62_63",     "IT & information services",
  "TTL_K",          "Financial & insurance services",
  "TTL_L",          "Real estate",
  "TTL_M",          "Professional & scientific services",
  "TTL_N",          "Administrative & support services",
  "TTL_O",          "Public administration & defence",
  "TTL_P",          "Education",
  "TTL_Q",          "Health & social work",
  "TTL_R",          "Arts & entertainment",
  "TTL_S",          "Other service activities",
  "TTL_T",          "Household employer activities"
)

cpi_results <- tibble(
  sector     = sectors[1:50],
  cpi_impact = cpi_impact,
  hfce_share = w
) |>
  left_join(sector_names, by = "sector") |>
  arrange(desc(cpi_impact))

cpi_results

cpi_results |>
  filter(cpi_impact > 0) |>
  ggplot(aes(x = cpi_impact,
             y = reorder(sector_name, cpi_impact))) +
  geom_col() +
  labs(
    title    = "CPI impact of a 100% price increase by sector",
    subtitle = "Weighted by household final consumption shares (HFCE)",
    x        = "Synthetic CPI change (percentage points)",
    y        = NULL
  ) +
  theme_minimal(base_size = 11)






