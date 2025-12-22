pkgload::load_all()


dta_agg_0 <-
  dta_hh |>
  f_agg_by_decile_one(
    var_decile = "ym___decile",
    var_agg = c("ym", "dtx_total", "dtx_prog1", "dtx_prog2", "dtx_prog3"),
    var_group = "group_1",
    wt_var = get_wt_nm()
  )

dta_agg_0 |>
  count(group_var, group_val)


dta_agg_1 <-
  dta_hh |>
  f_agg_by_decile(
    var_decile = c("ym___decile"),
    var_agg = c("ym", "dtx_total", "dtx_prog1", "dtx_prog2", "dtx_prog3"),
    var_group = c("all", "group_2"),
    wt_var = get_wt_nm()
  )

dta_agg_1 |>
  count(group_var, group_val)

dta_agg_1 |>
  f_plot_gg(
    x_var = "decile",
    y_var = "level",
    color_var = "var",
    facet_var = "group_val",
    type = "line"
  )

dta_agg_1 |>
  f_plot_gg(
    x_var = "decile",
    y_var = "level",
    color_var = "var",
    facet_var = "group_val",
    type = "bar",
    bar_position = "stack"
  )


dta_agg |>
  f_plot_gg(x_var = "decile", y_var = "level", color_var = "var", type = "bar")
