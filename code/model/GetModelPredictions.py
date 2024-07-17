from casadi import *
import numpy as np
import pandas as pd
import sys


data_name = "VaryingRates2d" # "CocaColaExample"
production_function = "Cobb-Douglas"
trade = False # False # Allow for the turning off of trade to solve the model with 50 states without running into dimensionality issues

# param_vals = pd.read_csv("code/simmed_data/simmed_data_params_only.csv", sep=",")
param_vals = pd.read_csv("code/simmed_data/" + data_name + "_params_only.csv", sep=",")
trade_cost_df = pd.read_csv("code/simmed_data/" + data_name + "_trade_costs.csv", sep=",")
file_out_with_solutions = "code/simmed_data/" + data_name + "_with_optimal_prices.csv"


# TODO: Fajgelbaum et al. not defined with kappas, need to derive
# TODO: add multiple products per firm
# TODO: have we thought about how labor is jointly determined with capital,
# and increasing the payroll weight relative to the property weight won't do anything if it's Leontief?
# Good to go beyond Cobb-Douglas
# And similarly for sales if trade costs are infinite: trade costs.

# Note: large iceberg trade cost doesn't seem to change location or quantity of production, just the location of sales
# Note: non-linearity: if you have states with tax rates of 0, 0.2, 0.2, and then 0, 0.2, 0.6, the price decline is larger in the 0 state than the 0.2 state
# I think this is driven by the non-linearity in keep rates (50% tax should lead to doubling of prices in McLure)

def D(p, params):
    taxes, factor_weights, production_params, cost_params = params
    ε, κ, α, A = production_params
    D_values = κ * p ** -ε
    # D_values = 2 - p
    return D_values


def inverse_D(q, params):
    taxes, factor_weights, production_params, cost_params = params
    ε, κ, α, A = production_params        
    prices = (q / κ) ** (-1 / ε)
    return prices

def Fajgelbaum_et_al_system(q, params):
    taxes, factor_weights, production_params, cost_params = params
    FA_tax_rate, separate_acct_tax_rate, sales_tax_rate, payroll_tax_rate = taxes
    ε, κ, α, A = production_params
    c, w, ρ, Lbar, r, iceberg_trade_cost_mat, non_iceberg_trade_cost_mat = cost_params

    t_dot_q = sum1(FA_tax_rate * q)
    ttilde = (FA_tax_rate - t_dot_q / sum1(q)) / (1 - t_dot_q / sum1(q))
    return q ** (- 1 / ε) - (ε / (ε - ttilde)) * (ε / (ε - 1)) * c

def all_jurisdictions_FA_tax_rate_on_profits_from_state_s(p, q_consumed, l, k, m, params):
    taxes, factor_weights, production_params, cost_params = params
    FA_tax_rate, separate_acct_tax_rate, sales_tax_rate, payroll_tax_rate = taxes
    c, w, ρ, Lbar, r, iceberg_trade_cost_mat, non_iceberg_trade_cost_mat = cost_params

    sales_weight, payroll_weight, property_weight = factor_weights
    sales_revenue = p * q_consumed
    wages = get_wages(l, params)
    rental_rate_bill = r * k
    taxes_on_total_profits_in_s_by_jurisdiction = \
        sales_weight * sales_revenue / sum1(sales_revenue) * FA_tax_rate \
        + payroll_weight * wages / sum1(wages) * l \
        + property_weight * rental_rate_bill / sum1(rental_rate_bill) * k
    return sum1(taxes_on_total_profits_in_s_by_jurisdiction)

def after_tax_profits(p, q_consumed, trade_matrix, l, k, m, params, trade):
    taxes, factor_weights, production_params, cost_params = params
    FA_tax_rate, separate_acct_tax_rate, sales_tax_rate, payroll_tax_rate = taxes
    c, w, ρ, Lbar, r, iceberg_trade_cost_mat, non_iceberg_trade_cost_mat = cost_params

    revenue = p * q_consumed
    production_cost = factor_costs(l, k, m, params)
    production_state_profits = get_production_state_profits(p, trade_matrix, l, k, m, params, trade)
    if trade:
        non_iceberg_trade_costs = sum1(sum2(non_iceberg_trade_cost_mat * trade_matrix))
    else:
        non_iceberg_trade_costs = 0

    return sum1((1 - all_jurisdictions_FA_tax_rate_on_profits_from_state_s(p, q_consumed, l, k, m, params)) * (revenue - production_cost) - separate_acct_tax_rate * production_state_profits - sales_tax_rate * revenue - payroll_tax_rate * sum1(get_wages(l, params))) - non_iceberg_trade_costs

def production(l, k, m, params, production_function="linear"):
    taxes, factor_weights, production_params, cost_params = params
    
    if production_function == "linear":
        return l + k + m
    elif production_function == "Cobb-Douglas":
        ε, κ, α, A = production_params        
        return A * k ** α * l ** (1 - α) + m
    else:
        raise ValueError("Invalid production function specified")
        
def naive_all_jurisdictions_FA_tax_rate_on_profits_from_state_s(p, q_consumed, l, k, m, params):
    taxes, factor_weights, production_params, cost_params = params
    FA_tax_rate, separate_acct_tax_rate, sales_tax_rate, payroll_tax_rate = taxes
    sales_weight, payroll_weight, property_weight = factor_weights
    n_jurisdictions = p.shape[0]
    naive_sales_payroll_property_share = 1 / n_jurisdictions
    taxes_on_total_profits_by_jurisdiction = naive_sales_payroll_property_share * FA_tax_rate
    return sum1(taxes_on_total_profits_by_jurisdiction)

def multiply_every_row_by_same_vector(square_SX, vector):
    return (square_SX.T * vector).T

def get_production_state_profits(p, trade_matrix, l, k, m, params, trade):
    taxes, factor_weights, production_params, cost_params = params
    c, w, ρ, Lbar, r, iceberg_trade_cost_mat, non_iceberg_trade_cost_mat = cost_params

    if trade:
        state_production_revenues = multiply_every_row_by_same_vector((1 - iceberg_trade_cost_mat) * trade_matrix, p)
    else:
        # This is an incorrect way of doing separate accounting, but if the separate accounting tax rate is 0 it will avoid returning an error
        # I think this is the same as what Republicans called the DBCFT: a destination-based cash flow tax, firms pay taxes on their sales 
        # in the US minus their costs in the US, regardless of where the output is sold
        state_consumption_revenues = D(p, params) * p
        state_production_revenues = state_consumption_revenues
    state_costs = factor_costs(l, k, m, params)
    production_state_profits = sum2(state_production_revenues) - state_costs
    return production_state_profits

def get_wages(l, params):
    taxes, factor_weights, production_params, cost_params = params
    c, w, ρ, Lbar, r, iceberg_trade_cost_mat, non_iceberg_trade_cost_mat = cost_params

    n_jurisdictions = l.shape[0]
    monopsony_w = (l / Lbar) ** (1 / ρ)
    exogneous_w = w
    wage = SX.zeros(n_jurisdictions)
    for i, rho_val in enumerate(ρ):
        if rho_val == np.inf:
            wage[i] = exogneous_w[i]
        else:
            wage[i] = monopsony_w[i]
    return wage

def factor_costs(l, k, m, params):
    taxes, factor_weights, production_params, cost_params = params
    c, w, ρ, Lbar, r, iceberg_trade_cost_mat, non_iceberg_trade_cost_mat = cost_params

    # Monopsony: l(w) = Lbar w ** ρ
    # As ρ goes to infinity, we get perfect competition.
    # Therefore, input ρ = inf for the perfect competition case (constant w equal to some exogenous w)
    # l(w) = Lbar w ** ρ implies a wage of ρ / (ρ + 1) of marginal revenue product, so 
    # a markdown of 1 / (ρ + 1)
    wage = get_wages(l, params)

    cost = wage * l + r * k + c * m
    return cost

def after_tax_profits_naive(p, q_consumed, trade_matrix, l, k, m, params, trade):
    taxes, factor_weights, production_params, cost_params = params
    FA_tax_rate, separate_acct_tax_rate, sales_tax_rate, payroll_tax_rate = taxes
    c, w, ρ, Lbar, r, iceberg_trade_cost_mat, non_iceberg_trade_cost_mat = cost_params

    revenue = p * q_consumed
    production_cost = factor_costs(l, k, m, params)
    production_state_profits = get_production_state_profits(p, trade_matrix, l, k, m, params, trade)
    if trade:
        non_iceberg_trade_costs = sum1(sum2(non_iceberg_trade_cost_mat * trade_matrix))
    else:
        non_iceberg_trade_costs = 0

    return sum1((1 - naive_all_jurisdictions_FA_tax_rate_on_profits_from_state_s(p, q_consumed, l, k, m, params)) * (revenue - production_cost) - separate_acct_tax_rate * production_state_profits - sales_tax_rate * revenue - payroll_tax_rate * sum1(get_wages(l, params))) - non_iceberg_trade_costs


def was_solution_found(solution, solver_stats, maximum_obj_value):
    ee_error = sum1(fabs(solution["g"]) ** 4) ** (1/4)
    # print(f"{ee_error = }")
    solution_found = True
    obj = solution["f"]
    print("Maximum Absolute Error = " + str(ee_error) + ", Objective Value = " + str(obj))
    solver_succeeded = solver_stats["return_status"] == "Solve_Succeeded" and solver_stats["success"] == True
    if ee_error > 1e-4 or not solver_succeeded: # or (ee_error == 0 and solution["g"].shape[0] > 0):
        solution_found = False
        # sys.exit("No Solution Found, Maximum Absolute Error = " + str(ee_error))
        if ee_error > 1e-4:
            print("No Solution Found, Maximum Absolute Error = " + str(ee_error) + " greater than 1e-4, Objective Value = " + str(obj))
        if ee_error == 0 and solution["g"].shape[0] > 0:
            print("No Solution Found, Maximum Absolute Error exactly 0")
            print(solution)
        if not solver_succeeded:
            print(solver_stats)
    elif obj > maximum_obj_value:
        solution_found = False
        # sys.exit("No Solution Found, Objective Value = " + str(obj))
        print("Inexact Solution Found, Objective Value = " + str(obj) + ", greater than maximum allowed value of " + str(maximum_obj_value))
    return solution_found

def SX_to_numpy(ary):
    return np.array(DM(ary)).squeeze()

def get_profit_maxmizing_prices(profit_func, params, x0_vals, trade):

    n_jurisdictions = x0_vals.shape[0]

    l = SX.sym("l", n_jurisdictions)
    k = SX.sym("k", n_jurisdictions)
    m = SX.sym("m", n_jurisdictions)
    p = SX.sym("m", n_jurisdictions)


    if trade:
        trade_matrix = SX.sym("trade_matrix", n_jurisdictions, n_jurisdictions)
        # Row 0 of trade_matrix is the quantity produced in jurisdiction 1. Entry 0,0 is the quantity it consumes,
        # while entry 0,5 is the quantity produced in 1 and sold to 5.
        # Therefore the sum of column 0 (sum1) is the quantity consumed in jurisdiction 1.
    
        q_consumed = sum1((1 - iceberg_trade_cost_mat) * trade_matrix).reshape((n_jurisdictions, 1))
        # p = inverse_D(q_consumed, params) Not doing this because then everything gets highly non-linear
        q_produced = sum2(trade_matrix)

        x = vertcat(trade_matrix.reshape((n_jurisdictions ** 2, 1)), l, k, m, p)
        production_constraint = q_produced - production(l, k, m, params, production_function=production_function)

    else:

        q_consumed = SX.sym("q_consumed", n_jurisdictions)
        q_produced = []
        trade_matrix = []

        x = vertcat(q_consumed, l, k, m, p)
        production_constraint = sum1(q_consumed - production(l, k, m, params, production_function=production_function))


    demand_constraint = q_consumed - D(p, params)

    constraint = vertcat(production_constraint, demand_constraint)

    obj = -profit_func(p, q_consumed, trade_matrix, l, k, m, params, trade)

    x0 = DM(np.full((x.shape[0], x.shape[1]), x0_vals[0]))

    if trade:
        x0[:n_jurisdictions ** 2] = 1 / (n_jurisdictions ** 2)
    else:
        x0[:n_jurisdictions] = 1 # q_consumed
        x0[n_jurisdictions:2 * n_jurisdictions] = 1 # l
        x0[2 * n_jurisdictions:3 * n_jurisdictions] = 1 # k
        x0[3 * n_jurisdictions:4 * n_jurisdictions] = 0 # m
        x0[4 * n_jurisdictions:5 * n_jurisdictions] = 1 #p
        x0[4 * n_jurisdictions] = 1.02 # p(state 1)

        

    nlp = {
        "x": x,
        "f": obj,
        "g": constraint
    }

    print_opt_diagnostics = False
    if print_opt_diagnostics:
        ipopt_print_level = 3
    else:
        ipopt_print_level = 0

    solver = nlpsol("solver", "ipopt", nlp, {
                    "ipopt.print_level": ipopt_print_level, "ipopt.tol": 1e-10, 'print_time': 0})
    solution = solver(
        x0=x0,
        lbx=0,
        lbg=-1e-10,
        ubg=1e-10,
    )

    sol = np.array(solution["x"]).squeeze()

    
    if trade:
        n_production_vals = n_jurisdictions ** 2
        trade_matrix_sol = sol[:n_production_vals].reshape((n_jurisdictions, n_jurisdictions), order="F")
        q_consumed_sol = np.sum((1 - iceberg_trade_cost_mat) * trade_matrix_sol, axis=0).squeeze()
        q_produced_sol = np.sum(trade_matrix_sol, axis=1)
        l_sol, k_sol, m_sol, p_sol = np.split(sol[n_production_vals:], 4)
        x_sol = [trade_matrix_sol, l_sol, k_sol, m_sol, p_sol]
    else:
        q_consumed_sol, l_sol, k_sol, m_sol, p_sol = np.split(sol, 5)
        q_produced_sol = production(l_sol, k_sol, m_sol, params, production_function=production_function)
        trade_matrix_sol = []
        x_sol = [q_consumed_sol, l_sol, k_sol, m_sol, p_sol]
    

    # print(f"{q_produced_sol = }")
    # print(solution)
    print(f"{trade_matrix_sol = }")
    # print(f"{l_sol = }")
    # print(f"{k_sol = }")
    # print(f"{m_sol = }")
    print(f"{p_sol = }")
    print(f"Quantity demanded: {D(p_sol, params)}")
    print(f"Quantity consumed: {q_consumed_sol}")
    print(f"Quantity produced: {q_produced_sol}")
    production_cost_sol = SX_to_numpy(factor_costs(l_sol, k_sol, m_sol, params))
    # print(f"{production_cost_sol = }")
    # print(f"Production cost per unit {production_cost_sol / q_produced_sol}")
    profits_sol = SX_to_numpy(after_tax_profits(p_sol, q_consumed_sol, trade_matrix_sol, l_sol, k_sol, m_sol, params, trade))
    production_profits_sol = SX_to_numpy(get_production_state_profits(p_sol, trade_matrix_sol, l_sol, k_sol, m_sol, params, trade))
    # print(f"{profits_sol = }")
    # print(f"{production_profits_sol = }")
    f_of_x_sol = [D(p_sol, params), q_consumed_sol, q_produced_sol, production_cost_sol, profits_sol, production_profits_sol]

    solution_found = was_solution_found(solution, solver.stats(), 0)
    if solution_found:
        return [x_sol, f_of_x_sol]
    else:
        # return [np.full_like(x_sol, np.nan), np.full_like(f_of_x_sol, np.nan)]
        return [[np.full_like(x_sol_element, np.nan) for x_sol_element in x_sol], [np.full_like(f_of_x_sol_element, np.nan) for f_of_x_sol_element in f_of_x_sol]]


def get_p_Fajgelbaum_et_al(params, q_x0_vals):

    n_jurisdictions = q_x0_vals.shape[0]
    q = SX.sym("q", n_jurisdictions)

    x = q
    obj = 1

    x0 = DM(q_x0_vals)

    nlp = {
        "x": x,
        "f": obj,
        "g": Fajgelbaum_et_al_system(q, params)
    }

    print_opt_diagnostics = False
    if print_opt_diagnostics:
        ipopt_print_level = 3
    else:
        ipopt_print_level = 0

    solver = nlpsol("solver", "ipopt", nlp, {
                    "ipopt.print_level": ipopt_print_level, "ipopt.tol": 1e-10, 'print_time': 0})
    solution = solver(
        x0=x0,
        lbg=-1e-10,
        ubg=1e-10
    )
    

    q = np.array(solution["x"]).squeeze()
    
    # print(q)
    p_Fajgelbaum_et_al = inverse_D(q, params)
    # print("p_Fajgelbaum_et_al")
    # print(p_Fajgelbaum_et_al)
          
    solution_found = was_solution_found(solution, solver.stats(), 2)
    if solution_found:
        return p_Fajgelbaum_et_al
    else:
        return np.full_like(p_Fajgelbaum_et_al, np.nan)

# jurisdiction_type_1 = pd.DataFrame([{"t": 0, "ε": 6, "κ": 10, "c": 1}])
# jurisdiction_type_2 = pd.DataFrame([{"t": 0.2, "ε": 6, "κ": 10, "c": 1}])

# n_jurisdictions_by_type = np.array([1, 3])


unique_firm_years = param_vals[["firm_id", "year"]].drop_duplicates()

# Initialize columns with the optimal prices and Fajgelbaum et al prices to NaN
param_vals[["optimal_price", "optimal_quantity_consumed", "optimal_quantity_produced",
                  "optimal_state_revenue", "optimal_state_cost", "optimal_profits", "optimal_state_profits",
                  "price_Fajgelbaum_et_al", "quantity_Fajgelbaum_et_al", "revenue_Fajgelbaum_et_al"]] = np.nan
param_vals.set_index(["firm_id", "year", "state"], inplace=True)
trade_cost_df.set_index(["firm_id", "year", "producing_state", "consuming_state"], inplace=True)


for index, row in unique_firm_years.iterrows():
    this_firm_id = row["firm_id"]
    this_year = row["year"]
    param_vals_this_firm_year = param_vals.loc[(this_firm_id, this_year, slice(None)), :]
    trade_costs_this_firm_year = trade_cost_df.loc[(this_firm_id, this_year, slice(None), slice(None)), :]
    print("firm is " + str(this_firm_id) + " year is " + str(this_year))
    print("states")
    print(param_vals_this_firm_year.index)
    print(param_vals_this_firm_year[["FA_tax_rate", "separate_acct_tax_rate", "sales_tax_rate", "payroll_tax_rate"]])

    n_jurisdictions = len(param_vals_this_firm_year)
    # Make the trade costs data frame into a matrix (take advantage of the fact that they're sorted by producing state first
    # (C order is the default for reshape; it puts the entries for the first producing state in the first row of the resulting matrix))
    iceberg_trade_cost_mat = trade_costs_this_firm_year.sort_index()[["iceberg_trade_cost"]].to_numpy().reshape((n_jurisdictions, n_jurisdictions), order="C")
    non_iceberg_trade_cost_mat = trade_costs_this_firm_year.sort_index()[["non_iceberg_trade_cost"]].to_numpy().reshape((n_jurisdictions, n_jurisdictions), order="C")
    
    # FA_tax_rate = param_vals_this_firm_year["FA_tax_rate"].to_numpy()
    # separate_acct_tax_rate = param_vals_this_firm_year["separate_acct_tax_rate"].to_numpy()
    # sales_tax_rate = param_vals_this_firm_year["sales_tax_rate"].to_numpy()
    # payroll_tax_rate = param_vals_this_firm_year["payroll_tax_rate"].to_numpy()
    # taxes = [FA_tax_rate, separate_acct_tax_rate, sales_tax_rate, payroll_tax_rate]
    param_names = ["epsilon", "kappa", "alpha", "A", "cost", "wage", "rho", "Lbar", "rental_rate", "sales_weight", "payroll_weight", "property_weight", "FA_tax_rate", "separate_acct_tax_rate", "sales_tax_rate", "payroll_tax_rate"]
    ε, κ, α, A, c, w, ρ, Lbar, r, sales_weight, payroll_weight, property_weight, FA_tax_rate, separate_acct_tax_rate, sales_tax_rate, payroll_tax_rate = param_vals_this_firm_year[param_names].to_numpy().T
    taxes = [FA_tax_rate, separate_acct_tax_rate, sales_tax_rate, payroll_tax_rate]
    factor_weights = [sales_weight, payroll_weight, property_weight]
    production_params = [ε, κ, α, A]
    cost_params = [c, w, ρ, Lbar, r, iceberg_trade_cost_mat, non_iceberg_trade_cost_mat]
    params_firm_year = [taxes, factor_weights, production_params, cost_params]


    n_jurisdictions = len(param_vals_this_firm_year)
    initial_price_guess_everywhere = 1
    x_sol, f_of_x_sol = get_profit_maxmizing_prices(after_tax_profits, params_firm_year, np.full(n_jurisdictions, initial_price_guess_everywhere), trade)
    trade_matrix_sol, l_sol, k_sol, m_sol, p_sol = x_sol
    D_sol, q_consumed_sol, q_produced_sol, production_cost_sol, profits_sol, prod_profits_sol = f_of_x_sol
    optimal_price = p_sol
    optimal_quantity_consumed = q_consumed_sol
    optimal_quantity_produced = q_produced_sol
    optimal_state_revenue = q_consumed_sol * optimal_price
    optimal_state_cost = production_cost_sol
    optimal_profits = profits_sol
    optimal_state_profits = prod_profits_sol
    # p, q_consumed, trade_matrix, l, k, m, params
    # π = after_tax_profits(optimal_price.squeeze(), optimal_quantity_consumed, optimal_state_revenue, params_firm_year)
    # π = after_tax_profits(optimal_price.squeeze(), params_firm_year) # this is a scalar, total worldwide profits
    # optimal_price_naive = get_profit_maxmizing_prices(after_tax_profits_naive, params_firm_year, np.full(n_jurisdictions, initial_price_guess_everywhere))
    # price_Fajgelbaum_et_al = get_p_Fajgelbaum_et_al(params_firm_year, np.ones((n_jurisdictions, 1)))
    # quantity_Fajgelbaum_et_al = D(price_Fajgelbaum_et_al, params_firm_year)
    # revenue_Fajgelbaum_et_al = quantity_Fajgelbaum_et_al * price_Fajgelbaum_et_al
    # π_Fajgelbaum_et_al = after_tax_profits(price_Fajgelbaum_et_al.squeeze(), params_firm_year)  # this is a scalar, total worldwide profits
    param_vals_this_firm_year = param_vals_this_firm_year.assign(
        optimal_price=optimal_price,
        optimal_quantity_consumed=optimal_quantity_consumed,
        optimal_quantity_produced=optimal_quantity_produced,
        optimal_state_revenue=optimal_state_revenue,
        optimal_state_cost=optimal_state_cost,
        optimal_profits=optimal_profits,
        optimal_state_profits=optimal_state_profits,
        # price_Fajgelbaum_et_al=price_Fajgelbaum_et_al,
        # quantity_Fajgelbaum_et_al=quantity_Fajgelbaum_et_al,
        # revenue_Fajgelbaum_et_al=revenue_Fajgelbaum_et_al,
    )

    param_vals.update(param_vals_this_firm_year[[
        "optimal_price",
        "optimal_quantity_consumed",
        "optimal_quantity_produced",
        "optimal_state_revenue",
        "optimal_state_cost",
        "optimal_profits",
        "optimal_state_profits",
        # "price_Fajgelbaum_et_al",
        # "quantity_Fajgelbaum_et_al",
        # "revenue_Fajgelbaum_et_al",
        ]])


param_vals.to_csv(file_out_with_solutions, index=True)

