
# Suppose state 1 lowers it's tax apportionment rate, everyone uses 100% sales apportionment

firm_1_data = data.table(state = 1, firm_id = 1, year = c(0, 1), price_p_exog = c(2, 2), quantity_p_exog_q_exog = c(1, 1))
firm_2_data = data.table(state = c(1, 1, 2, 2), firm_id = 2, year = c(0, 1, 0, 1), price_p_exog = c(2, 1.9, 2, 2.1), quantity_p_exog_q_exog = c(1, 1.1, 1, 0.9))
firm_3_data = data.table(state = c(2, 2, 3, 3), firm_id = 3, year = c(0, 1, 0, 1), price_p_exog = c(2, 2, 2, 2), quantity_p_exog_q_exog = c(1, 1, 1, 1))
firm_4_data = data.table(state = c(1, 1, 2, 2, 3, 3), firm_id = 4, year = c(0, 1, 0, 1, 0, 1), price_p_exog = c(2, 1.94, 2, 2.03, 2, 2.03), quantity_p_exog_q_exog = c(1, 1, 1, 1, 1, 1))


tax = data.table(
  state = c(1, 2, 3, 1, 2, 3),
  sales_weight = c(1, 1, 1, 1, 1, 1),
  payroll_weight = c(0, 0, 0, 0, 0, 0),
  property_weight = c(0, 0, 0, 0, 0, 0),
  # tax_rate = c(0.1, 0.2, 0.3, 0.1, 0.2, 0.3),
  tax_rate = c(0.1, 0.1, 0.1, 0.05, 0.1, 0.1),
  year = c(0, 0, 0, 1, 1, 1)
)

firm_state_costs_and_elasticities = data.table(
  firm_id = c(1, 2, 2, 3, 3, 4, 4, 4),
  state = c(1, 1, 2, 2, 3, 1, 2, 3),
  epsilon = 6, # c(4, 5, 6, 4, 5.5, 5, 6, 4),
  kappa = 1,
  cost = 1
  )


firm_characteristics = rbind(
  firm_1_data, firm_2_data, firm_3_data, firm_4_data
  )


overall = merge(
  merge(firm_characteristics, tax, by = c("state", "year")),
  firm_state_costs_and_elasticities,
  by = c("state", "firm_id")
)

setkey(overall, firm_id, year, state)



fwrite(overall, "code/simmed_data/simmed_data_params_only.csv")
