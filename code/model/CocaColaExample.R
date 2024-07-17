setwd("/Users/nathan/Downloads/SFA")
pacman::p_load(data.table, foreach, iterators)

data_name = "CocaColaExample"

eps_val_everywhere = 3.64432 # markup of 1 / (3.64432 - 1) = 38%
materials_cost = 100 # make it not at all attractive to use materials in production
alpha_val = 0.3
wage_val = (1 - alpha_val) * (eps_val_everywhere - 1) / (eps_val_everywhere)
rental_rate_val = alpha_val * (eps_val_everywhere - 1) / (eps_val_everywhere)
A_val = 1
iceberg_trade_cost_everywhere = 0
non_iceberg_trade_cost_everywhere = 0
Lbar_val = 27.848558398 # 1
rho_val = Inf
FA_tax_rate_everywhere = 0.25

FA_tax_increase_in_raising_state = 0.1

CJ.dt = function(X,Y) {
  stopifnot(is.data.table(X),is.data.table(Y))
  k = NULL
  X = X[, c(k = 1, .SD)]
  setkey(X, k)
  Y = Y[, c(k = 1, .SD)]
  setkey(Y, NULL)
  X[Y, allow.cartesian = TRUE][, k := NULL][]
}

params_by_state_year =
  rbind(
    data.table(state = 1:5, year = 0),
    data.table(state = 1, year = 1),
    data.table(state = 1:2, year = 2),
    data.table(state = 1:3, year = 3),
    data.table(state = 1:5, year = 4),
    data.table(state = 1:10, year = 5),
    data.table(state = 1:50, year = 6)
  )

params_by_state_year[, `:=`(
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
  FA_tax_rate = FA_tax_rate_everywhere,
  separate_acct_tax_rate = 0,
  sales_tax_rate = 0,
  payroll_tax_rate = 0
)]


params_by_state_year[state == 1 & year == 0, sales_tax_rate := FA_tax_increase_in_raising_state * 1 / eps_val_everywhere]
params_by_state_year[state == 1 & year > 0, FA_tax_rate := FA_tax_rate_everywhere + FA_tax_increase_in_raising_state]


trade_costs =
  foreach(firm_year = iter(unique(params_by_state_year[, .(firm_id, year)]), by = "row"), .combine = rbind) %do% {
    all_states_for_firm_year = params_by_state_year[firm_id == firm_year$firm & year == firm_year$year, state]
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

fwrite(params_by_state_year, paste0("code/simmed_data/", data_name, "_params_only.csv"))

fwrite(trade_costs, paste0("code/simmed_data/", data_name, "_trade_costs.csv"))

