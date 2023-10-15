setwd("/Users/nathan/Downloads/SFA")
pacman::p_load(data.table)

eps_val_everywhere = 6
materials_cost = 5/6
wage_val = 1
rental_rate_val = 1
alpha_val = 0.3


three_state_data =
  rbind(
    data.table(state = 1:5, year = 0),
    data.table(state = 1, year = 1),
    data.table(state = 1:2, year = 2),
    data.table(state = 1:3, year = 3),
    data.table(state = 1:5, year = 4),
    data.table(state = 1:10, year = 5),
    data.table(state = 1:50, year = 6)
  )

three_state_data[, `:=`(
  firm_id = 1,
  cost = materials_cost,
  wage = wage_val,
  rental_rate = rental_rate_val,
  epsilon = eps_val_everywhere,
  kappa = 1,
  alpha = alpha_val,
  sales_weight = 1,
  payroll_weight = 0,
  property_weight = 0,
  SFA_tax_rate = 0,
  separate_acct_tax_rate = 0,
  sales_tax_rate = 0,
  payroll_tax_rate = 0
)]

tax_rate = eps_val_everywhere * (1 - 1 / 1.04) # 0.05940594 (to get that the sales tax prediction is 1.04)
three_state_data[state == 1 & year == 0, sales_tax_rate := tax_rate * 1 / eps_val_everywhere] # 1 - 1/1.06] # 0.05660377
three_state_data[state == 1 & year > 0, SFA_tax_rate := tax_rate] # 1 - 1/1.06] # 0.05660377


# three_state_data = three_state_data[year %in% c(1, 2, 3)]

fwrite(three_state_data, "code/simmed_data/ThreeStates_params_only.csv")
