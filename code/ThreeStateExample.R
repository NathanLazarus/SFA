setwd("/Users/nathan/Downloads/SFA")
pacman::p_load(data.table, foreach, iterators)

eps_val_everywhere = 6 # markup of 6 / (6 - 1) = 20%
materials_cost = 100 # 1 # 5/6
wage_val = 5/12 # 1
rental_rate_val = 5/12 # 1
alpha_val = 0.5 # 0.3
A_val = 1
iceberg_trade_cost_everywhere = 0.01
non_iceberg_trade_cost_everywhere = 0
Lbar_val = 100 # 27.848558398 # 1
rho_val = 3.8 # midpoint of Kroft et al. construction estimates, markdown of 1 / (3.8 + 1) = 21%
# rho_val = Inf

CJ.dt = function(X,Y) {
  stopifnot(is.data.table(X),is.data.table(Y))
  k = NULL
  X = X[, c(k = 1, .SD)]
  setkey(X, k)
  Y = Y[, c(k = 1, .SD)]
  setkey(Y, NULL)
  X[Y, allow.cartesian = TRUE][, k := NULL][]
}

three_state_data =
  rbind(
    data.table(state = 1:5, year = 0),
    data.table(state = 1, year = 1),
    data.table(state = 1:2, year = 2),
    data.table(state = 1:3, year = 3),
    data.table(state = 1:5, year = 4),
    data.table(state = 1:10, year = 5) # ,
    # data.table(state = 1:50, year = 6)
  )

three_state_data[, `:=`(
  firm_id = 1,
  cost = materials_cost,
  wage = wage_val,
  rental_rate = rental_rate_val,
  epsilon = eps_val_everywhere,
  kappa = 1,
  alpha = alpha_val,
  A = A_val,
  Lbar = Lbar_val,
  rho = rho_val,
  sales_weight = 1,
  payroll_weight = 0,
  property_weight = 0,
  FA_tax_rate = 0,
  separate_acct_tax_rate = 0,
  sales_tax_rate = 0,
  payroll_tax_rate = 0
)]

tax_rate = eps_val_everywhere * (1 - 1 / 1.04) # 0.05940594 (to get that the sales tax prediction is 1.04)
three_state_data[state == 1 & year == 0, sales_tax_rate := tax_rate * 1 / eps_val_everywhere] # 1 - 1/1.06] # 0.05660377
# three_state_data[state == 1 & year > 0, FA_tax_rate := tax_rate] # 1 - 1/1.06] # 0.05660377
three_state_data[state == 1 & year > 0, separate_acct_tax_rate := tax_rate]

trade_costs =
  foreach(firm_year = iter(unique(three_state_data[, .(firm_id, year)]), by = "row"), .combine = rbind) %do% {
    all_states_for_firm_year = three_state_data[firm_id == firm_year$firm & year == firm_year$year, state]
    state_pairs = CJ(all_states_for_firm_year, all_states_for_firm_year)
    setnames(state_pairs, c("producing_state", "consuming_state"))
    state_pairs[, `:=`(firm_id = firm_year$firm_id, year = firm_year$year)]
  }

trade_costs[producing_state != consuming_state, `:=`(
  iceberg_trade_cost = iceberg_trade_cost_everywhere,
  non_iceberg_trade_cost = non_iceberg_trade_cost_everywhere)
  ]
trade_costs[producing_state == consuming_state, `:=`(
  iceberg_trade_cost = 0,
  non_iceberg_trade_cost = 0)
  ]


# three_state_data = three_state_data[year %in% c(1, 2, 3)]
three_state_data = three_state_data[year %in% c(1)]

fwrite(three_state_data, "code/simmed_data/ThreeStates_params_only.csv")

fwrite(trade_costs, "code/simmed_data/ThreeStates_trade_costs.csv")

