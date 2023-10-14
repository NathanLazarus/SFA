setwd("/Users/nathan/Downloads/SFA")
pacman::p_load(data.table)

eps_val_everywhere = 6
marginal_cost_everywhere = 5/6


three_state_data =
  data.table(
    firm_id = 1,
    state = 1:3,
    year = 2000,
    cost = marginal_cost_everywhere,
    epsilon = eps_val_everywhere,
    kappa = 1,
    sales_weight = 1,
    payroll_weight = 0,
    property_weight = 0,
    SFA_tax_rate = 0,
    separate_acct_tax_rate = 0,
    sales_tax_rate = 0,
    payroll_tax_rate = 0
  )

three_state_data[state == 1, SFA_tax_rate := 0.04] # 1 - 1/1.06] # 0.05660377


fwrite(three_state_data, "code/simmed_data/ThreeStates_params_only.csv")
