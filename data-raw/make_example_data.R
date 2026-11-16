# =============================================================================
# make_example_data.R
# Simulates the two example datasets shipped with the app.
# The data are SIMULATED (not field measurements) so that every method in the
# app and the teaching module has a clear, known pattern to find.
# Run from the repository root:  Rscript data-raw/make_example_data.R
# =============================================================================
set.seed(2026)

# ---- 1. forest_plots.csv : 120 plots, 30 per forest type --------------------
types <- c("Primary", "Logged", "Agroforest", "Plantation")
n_per <- 30
n     <- n_per * length(types)
forest_type <- rep(types, each = n_per)
ti <- match(forest_type, types)

elevation_m      <- round(runif(n, 60, 900))
slope_deg        <- round(pmin(rgamma(n, shape = 3, rate = 0.25), 40), 1)
canopy_cover_pct <- round(pmin(pmax(rnorm(n, c(90, 78, 64, 55)[ti], 6), 25), 99), 1)
soil_pH          <- round(rnorm(n, c(4.6, 4.8, 5.3, 5.1)[ti], 0.35), 2)
soil_C_pct       <- round(pmax(rnorm(n, c(4.2, 3.6, 2.9, 2.2)[ti], 0.6), 0.5), 2)
soil_N_pct       <- round(pmax(0.02 + 0.075 * soil_C_pct + rnorm(n, 0, 0.03), 0.03), 3)
litter_depth_cm  <- round(pmax(rnorm(n, 0.5 + 0.045 * canopy_cover_pct, 0.7), 0.2), 1)

basal_area_m2ha  <- round(pmax(rnorm(n, c(38, 27, 19, 22)[ti], 4.5), 5), 1)
tree_density_ha  <- round(pmax(rnorm(n, c(520, 610, 430, 680)[ti], 70), 150))

# Above-ground biomass: right-skewed (log-normal) -> good transformation example
agb_Mg_ha <- round(exp(3.45 + 0.05 * basal_area_m2ha + rnorm(n, 0, 0.38)), 1)

# Tree species richness: Poisson counts driven by type, canopy and elevation
log_mu_rich <- 1.55 + c(0.75, 0.50, 0.10, -0.55)[ti] +
  0.008 * (canopy_cover_pct - 70) - 0.0010 * (elevation_m - 450)
species_richness <- rpois(n, exp(log_mu_rich))

# Seedling counts: overdispersed (negative binomial), more under open canopy gaps
seedling_count <- rnbinom(n, mu = exp(4.6 - 0.028 * canopy_cover_pct), size = 1.6)

# Understory cover: hump-shaped (quadratic) response to canopy cover
understory_cover_pct <- round(pmin(pmax(
  62 - 0.045 * (canopy_cover_pct - 68)^2 + rnorm(n, 0, 6), 2), 95), 1)

# Hornbill presence/absence: logistic response to basal area (large trees)
p_hornbill <- plogis(-5.2 + 0.19 * basal_area_m2ha)
hornbill_present <- rbinom(n, 1, p_hornbill)

# Fire history: associated with forest type -> chi-square example
fire_history <- ifelse(runif(n) < c(0.07, 0.30, 0.50, 0.63)[ti], "Burned", "Unburned")

# Paired measurements on the same plot: soil moisture in dry vs wet season
soil_moisture_dry_pct <- round(pmax(rnorm(n, 14 + 0.16 * canopy_cover_pct, 3), 5), 1)
soil_moisture_wet_pct <- round(soil_moisture_dry_pct + rnorm(n, 6.5, 3.2), 1)

forest_plots <- data.frame(
  plot_id = sprintf("P%03d", seq_len(n)),
  forest_type, elevation_m, slope_deg, canopy_cover_pct,
  soil_pH, soil_C_pct, soil_N_pct, litter_depth_cm,
  basal_area_m2ha, tree_density_ha, agb_Mg_ha,
  species_richness, seedling_count, understory_cover_pct,
  hornbill_present, fire_history,
  soil_moisture_dry_pct, soil_moisture_wet_pct
)
forest_plots <- forest_plots[sample(n), ]           # shuffle plot order

# ---- 2. tree_community.csv : 40 plots x 14 tree taxa (stem counts) ----------
n_c    <- 10
c_type <- rep(types, each = n_c)
ci     <- match(c_type, types)
# expected stems per plot for each taxon in each forest type
mu <- rbind(
  #                      Primary Logged Agrof. Plant.
  Shorea_leprosula     = c(14,   6,   1,   0.1),
  Shorea_parvifolia    = c(10,   4,   0.5, 0.1),
  Dipterocarpus_sp     = c( 9,   3,   0.3, 0.1),
  Hopea_mengarawan     = c( 6,   2,   0.2, 0.1),
  Koompassia_excelsa   = c( 4,   2,   0.5, 0.1),
  Dyera_costulata      = c( 3,   3,   1,   0.2),
  Syzygium_sp          = c( 6,   7,   3,   1),
  Macaranga_gigantea   = c( 0.5, 12,  4,   2),
  Mallotus_paniculatus = c( 0.5,  9,  3,   2),
  Trema_orientalis     = c( 0.1,  5,  2,   3),
  Durio_zibethinus     = c( 0.5,  1,  8,   0.3),
  Parkia_speciosa      = c( 0.5,  1,  6,   0.3),
  Coffea_canephora     = c( 0,    0.2, 15, 1),
  Hevea_brasiliensis   = c( 0,    0.3, 5,  28)
)
comm <- sapply(rownames(mu), function(sp) rnbinom(length(ci), mu = mu[sp, ci], size = 2.5))
tree_community <- data.frame(
  plot_id     = sprintf("C%02d", seq_along(ci)),
  forest_type = c_type,
  elevation_m = round(runif(length(ci), 60, 900)),
  comm, check.names = FALSE
)
write.csv(tree_community, "data/tree_community.csv", row.names = FALSE)

# ---- 3. Grouping structure for mixed models (random factors) ----------------
# Plots are grouped in 12 sites (10 plots each). Plots from the same site share
# local conditions, so they are NOT independent: `site` is a random factor.
# Canopy height depends on basal area, plus a site effect, plus plot noise.
# (A separate seed keeps every variable above unchanged.)
set.seed(2027)
site_levels <- sprintf("S%02d", 1:12)
forest_plots$site <- sample(rep(site_levels, each = n / 12))
site_effect <- setNames(rnorm(12, 0, 3), site_levels)
forest_plots$canopy_height_m <- round(
  12 + 0.45 * forest_plots$basal_area_m2ha + site_effect[forest_plots$site] +
    rnorm(n, 0, 2), 1)
forest_plots <- forest_plots[, c("plot_id", "site", setdiff(names(forest_plots), c("plot_id", "site")))]
write.csv(forest_plots, "data/forest_plots.csv", row.names = FALSE)

cat("Wrote data/forest_plots.csv and data/tree_community.csv\n")
