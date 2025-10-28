## code to prepare `DATASET` dataset goes here


pkgload::load_all()

# f_var_dic_default()

# Simulate a microdata dataset with varaibles that correspond to the variable dictionary

n <- 1000
n_hh <-n
hh_size <- sample(1:6, n_hh, replace = TRUE, prob = c(0.2, 0.3, 0.25, 0.15, 0.07, 0.03))

# Gender typology
group_1 <- sample(c("Male", "Female"), n_hh, replace = TRUE, prob = c(0.62, 0.38))
group_2 <- sample(c("Urban", "Rural"), n_hh, replace = TRUE, prob = c(0.3, 0.7))
group_3 <- sample(c("Agriculture", "Services", "Manufacturing"), n_hh, replace = TRUE, prob = c(0.5, 0.2, 0.3))

set.seed(123)

# Market inceomd --------------------------------------------------------
ym <- rlnorm(n, meanlog = 3, sdlog = .5) * hh_size / (runif(n, 0.8, 1.2) * median (hh_size))
ym[sample(1:n, n * 0.05)] <- 0
ym[sample(1:n, n * 0.01)] <- max(ym) * 5


## Simlate benefits --------------------------------------------------------
sim_ben_share_value <- function(n, base_value, share_ben = 0.3, min_share = 0.1, max_share = 0.5,  if_zero = rep(FALSE, n)) {
  ben_values <- rep(0, n)
  ben_idx <- sample(1:n, n * share_ben)
  ben_values[ben_idx] <- base_value[ben_idx] * runif(length(ben_idx), min_share, max_share)
  ben_values[if_zero] <- 0
  return(ben_values)
}

# Pension benefits:
ben_pen <- gen_var_from_var(ym, corr = 0.25, probs = c(.7, .6, .5, .5), share_eligile = 0.9, min_share = 0.2, max_share = 0.6)
ben_unemp <- gen_var_from_var(ym, corr = -0.015, probs = c(.8, .5, .1, .075, .025), share_eligile = 0.4, min_share = 0.2, max_share = 0.5)


# Market income plys pension benefits ---------------------------------------------
yp = ym + ben_pen

check_vars_by_deciles(ym, ben_pen, ben_unemp, yp)


# Direct taxes ----------------------------------------------------------------

pay_t1 <- rep(0, n) # Most pay direct taxes
pay_t1_cond <- ben_unemp == 0 & ym > quantile(ym, 0.15)
pay_t1[pay_t1_cond] <- ym[pay_t1_cond] * 0.15

pay_t2 <- rep(0, n) # Employees
pay_t2_cond <- ben_unemp == 0 & ben_pen == 0 & ym > quantile(ym, 0.45)
pay_t2[pay_t2_cond] <- ym[pay_t2_cond] * 0.10

pay_t3 <- rep(0, n) # Unemployed
pay_t3_cond <- ben_unemp == 0 & ben_pen == 0 & pay_t2_cond == 0 & pay_t1_cond == 0
pay_t3[pay_t3_cond] <- ym[pay_t3_cond] * 0.10

dtx_prog1 <- pay_t1
dtx_prog2 <- pay_t2
dtx_prog3 <- pay_t3

dtx_total <- dtx_prog1 + dtx_prog2 + dtx_prog3

# Net market income ---------------------------------------------------------

yn <- ym - dtx_prog1 - dtx_prog2 - dtx_prog3
yn[yn < 0] <- 0

check_vars_by_deciles(ym, ben_pen, ben_unemp, yp, dtx_prog1, dtx_prog2, dtx_prog3, yn)

# Direct transfers ---------------------------------------------------------

dtr_prog1 <- gen_var_from_var(
  yn,
  corr = -0.05,
  probs = c(0.9, 0.8, 0.25, 0.1, 0, 0, 0, 0),
  share_eligile = 0.75,
  min_share = 0.2,
  max_share = 0.5
)

dtr_prog2 <-
  gen_var_from_var(
    yn,
    corr = -0.05,
    probs = c(0.9, 0.2, 0.1, 0, 0, 0,  0, 0, 0, 0),
    share_eligile = 0.50,
    min_share = 0.6,
    max_share = 0.8
  )

# Direct subsidies ----------------------------------------------------------

# Gross income  ---------------------------------------------------------
yg <- yp  +  dtr_prog1 + dtr_prog2 + ben_unemp

check_vars_by_deciles(
  ym,
  ben_pen,
  ben_unemp,
  yp,
  dtx_prog1,
  dtx_prog2,
  dtx_prog3,
  yn,
  dtr_prog1,
  dtr_prog2,
  yg
)

# Disposable income ---------------------------------------------------------
yd <- yg - dtx_prog1 - dtx_prog2 - dtx_prog3

# Indirect subsidies and taxes ---------------------------------------------
sub_energy <- gen_var_from_var(
  hh_size,
  corr = 10.1,
  probs = c(0.7, 0.6, 0.8, 0.9),
  share_eligile = 0.7,
  min_share = .2,
  max_share = .3
)

sub_food <- gen_var_from_var(
  hh_size,
  corr = 10.1,
  probs = c(0.5, 0.4, 0, 0, 0),
  share_eligile = 0.7,
  min_share = .5,
  max_share = .75
)
sub_total <- sub_energy + sub_food

check_vars_by_deciles(ym, ben_pen, ben_unemp, yp, dtx_prog1, dtx_prog2, dtx_prog3, yn,
                       dtr_prog1, dtr_prog2, yg, yd, sub_energy, sub_food)

# Consumption ----------------------------------------------------------
itx_vat <- sim_ben_share_value(n, yd, 0.70, 0.07, 0.12)
itx_excise <- sim_ben_share_value(n, yd, 0.70, 0.07, 0.12)
itx_total <- itx_vat + itx_excise

# Consumable income ---------------------------------------------------------
yc <- yd - itx_vat - itx_excise + sub_energy + sub_food

check_vars_by_deciles(ym, ben_pen, ben_unemp, yp, dtx_prog1, dtx_prog2, dtx_prog3, yn,
                       dtr_prog1, dtr_prog2, yg, yd, sub_energy, sub_food,
                       itx_vat, itx_excise, yc)

# In kind ---------------------------------------------------------------
ink_helth <- sim_ben_share_value(n, hh_size, 0.90, 0.5, 0.75) * mean(yg) / 10
ink_education <- sim_ben_share_value(n, hh_size, 0.80, 0.5, 0.8, hh_size > 2) * mean(yg) / 15
ink_total <- ink_helth + ink_education

# Final income ------------------------------------------------------------
yf <- yc + ink_helth + ink_education

check_vars_by_deciles(ym, ben_pen, ben_unemp, yp, dtx_prog1, dtx_prog2, dtx_prog3, yn,
                       dtr_prog1, dtr_prog2, yg, yd, sub_energy, sub_food,
                       itx_vat, itx_excise, yc,
                       ink_helth, ink_education, yf)

# Micro dta
dta_hh <-
  tibble(
    hhid = 1:n_hh,
    hhwt = runif(n_hh, 0.5, 2),
    hhsize = hh_size,
    group_1 = group_1,
    group_2 = group_2,
    group_3 = group_3,
    ym = ym,
    yn = yn,
    yp = yp,
    yg = yg,
    yd = yd,
    yc = yc,
    yf = yf,
    ben_pen = ben_pen,
    ben_unemp = ben_unemp,
    ben_total = ben_pen + ben_unemp,
    dtx_prog1 = dtx_prog1,
    dtx_prog2 = dtx_prog2,
    dtx_prog3 = dtx_prog3,
    dtx_total = dtx_total,
    dtr_prog1 = dtr_prog1,
    dtr_prog2 = dtr_prog2,
    dtr_total = dtr_prog1 + dtr_prog2,
    sub_energy = sub_energy,
    sub_food = sub_food,
    sub_total = sub_total,
    itx_vat = itx_vat,
    itx_excise = itx_excise,
    itx_total = itx_total,
    ink_helth = ink_helth,
    ink_education = ink_education,
    ink_total = ink_total
  )

# Using data in the package --------------------------------------------
usethis::use_data(dta_hh, overwrite = TRUE)

# Generating simulation results data  --------------------------------------------

dta_hh_sim1 <- dta_hh |> 
  mutate(
    across(
      where(is.numeric) & any_of(get_inc_nm(suffix = NULL)$var),
      ~ .x * runif(n(), 0.9, 1.1)
    )
  )

dta_sim <-
  list(
    list(
      policy_sim_raw = dta_hh,
      policy_name = "Baseline"
    ),
    list(
      policy_sim_raw = dta_hh_sim1,
      policy_name = "Sim 1"
    )
  )


usethis::use_data(dta_sim, overwrite = TRUE)


dta_sim |> agg_sims_by_deciles(

  dec_by = get_inc_nm(suffix = NULL)$var[1],
  dec_vars = get_var_nm(suffix = NULL)$var,
  wt_var = get_wt_nm()
)


# Checking by deciles data ---------------------------------------------

dta_hh_agg <- 
  dta_hh |>
  calc_deciles(
    dec_var = get_inc_nm(suffix = NULL)$var,
    wt_var = get_wt_nm(),
    n_dec = 10
  ) |>
  calc_agg_by(
    vars = get_var_nm(suffix = NULL)$var,
    by_var = "ym_decile",
    wt_var = get_wt_nm()
  ) 



# # Varaibles missing from the aggregated data
# setdiff(get_var_nm(suffix = NULL)$var, names(dta_hh))
# setdiff(names(dta_hh), get_var_nm(suffix = NULL)$var)

# using data in the package


# Sim direct taxes --------------------------------------------------------

# # Add 2 types of direct taxes proportional to the market income

# # Direct tax on employees income: 10% flat rate
# dtx_inc <- market_income * 0.1

# # Direct tax on employers income: apply to 50% of hh only, 5% flat rate
# dtx_con <- rep(0, n)
# dtx_con_idx <- sample(1:n, n * 0.5)
# dtx_con[dtx_con_idx] <- market_income[dtx_con_idx] * 0.05


  
# ggplot() + 
#   geom_density(aes(x = market_income, colour = "Market income")) + 
#   geom_density(aes(x = pension_benefits, colour = "Pension")) +
#   scale_x_log10()

  
# ggplot() + geom_density(aes(x = microsim_dta$ym)) + scale_x_log10()


# Final data set
# miro_dta <-
dta_hh |>
  pivot_longer(
    cols = -c(hhid, hhwt, hhsize, group_1, group_2, group_3),
    names_to = "var",
    values_to = "value"
  ) |>
  ggplot() +
  geom_density(aes(x = value, colour = var)) +
  scale_x_log10()


