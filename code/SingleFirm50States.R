setwd("/Users/nathan/Downloads/SFA")
pacman::p_load(data.table)
expenditures = fread("code/simmed_data/State_Consumption_Expenditures.csv")
Walmart_revenue_mil = 611000
Total_PCE_mil = 17511000

eps_val_everywhere = 6
marginal_cost_everywhere = 0.4




CJ.dt = function(X,Y) {
  stopifnot(is.data.table(X),is.data.table(Y))
  k = NULL
  X = X[, c(k = 1, .SD)]
  setkey(X, k)
  Y = Y[, c(k = 1, .SD)]
  setkey(Y, NULL)
  X[Y, allow.cartesian = TRUE][, k := NULL][]
}


expenditures[, Walmart_revenue := `Consumption Expenditures (Millions)` * Walmart_revenue_mil / Total_PCE_mil]
expenditures[, Walmart_cost := marginal_cost_everywhere][, Walmart_elasticity := 6][, Walmart_price := Walmart_cost * Walmart_elasticity / (Walmart_elasticity - 1)]
expenditures[, Walmart_quantity := Walmart_revenue / Walmart_price]



expenditures[, Walmart_implied_kappa := Walmart_quantity / (Walmart_price ^ - Walmart_elasticity)]

# # Double checking sigma / (sigma - 1) is the correct profit maximizing solution
# expenditures[, Walmart_profits := (Walmart_price - Walmart_cost) * Walmart_quantity]
# 
# expenditures[, Walmart_alt_price_1 := Walmart_price - 0.001]
# expenditures[, Walmart_alt_price_2 := Walmart_price + 0.001]
# expenditures[, Walmart_alt_quantity_1 := Walmart_implied_kappa * Walmart_alt_price_1 ^ -Walmart_elasticity]
# expenditures[, Walmart_alt_quantity_2 := Walmart_implied_kappa * Walmart_alt_price_2 ^ -Walmart_elasticity]
# 
# expenditures[, Walmart_alt_profits_1 := (Walmart_alt_price_1 - Walmart_cost) * Walmart_alt_quantity_1]
# expenditures[, Walmart_alt_profits_2 := (Walmart_alt_price_2 - Walmart_cost) * Walmart_alt_quantity_2]
# 
# expenditures[state == "California"]




expenditures[, firm_id := "Walmart"]

firm_characteristics = CJ.dt(expenditures, data.table(year = c(2010:2011)))[, .(state, firm_id, year, Walmart_implied_kappa)]


firm_characteristics[, `:=`(
  cost = marginal_cost_everywhere,
  epsilon = eps_val_everywhere
)]

firm_characteristics[firm_id == "Walmart", kappa := Walmart_implied_kappa]


tax = unique(firm_characteristics[, .(state, year)])

tax[, `:=`(
  sales_weight = 1,
  tax_rate = 0.04,
  payroll_weight = 0,
  property_weight = 0
)]

tax[state == "California" & year == 2011, tax_rate := 0.08]






overall = merge(
  firm_characteristics, tax, by = c("state", "year")
)

setkey(overall, firm_id, year, state)



fwrite(overall, "code/simmed_data/SingleFirm50States_params_only.csv")
