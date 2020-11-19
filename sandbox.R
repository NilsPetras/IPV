# some example data
rm(list = ls())
devtools::load_all()
load("../Manuskript/analysis/data/Moshagen_2020_ag.rda")


# # x <- get_names(dat_ag)
# y <- ipv_est(dat_ag, "agreeableness")
# nested_chart(y$est)
# item_chart(y$est$global)
# nested_chart(y$est, relative_scaling = 1)
# facet_chart(y$est$global)

y_partial <- ipv_est(dat_ag[ ,1:64], "big_5_ag", include_lav = TRUE)
nested_chart(y_partial$est)
item_chart(y_partial$est$global)
item_chart(y_partial$est$tests$bfas)
