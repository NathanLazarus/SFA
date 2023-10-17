from casadi import *
import numpy as np
import pandas as pd
import sys


data_name = "ThreeStates"
# simmed_data_vals = pd.read_csv("code/simmed_data/simmed_data_params_only.csv", sep=",")
simmed_data_vals = pd.read_csv("code/simmed_data/" + data_name + "_params_only.csv", sep=",")


# TODO: Fajgelbaum et al. not defined with kappas, need to derive
# TODO: add multiple products per firm
# TODO: add in labor, use payroll weight (also, have we thought about how labor is jointly determined with capital,
# and increasing the payroll weight relative to the property weight won't do anything if it's Leontief?
# And similarly for sales if trade costs are infinite—would be good to be explicity about trade costs.)
# TODO: get separate tax to work with cross-hauling, be explicit about production

def D(p, params):
    taxes, factor_weights, production_params, cost_params = params
    ε, κ, α, A = production_params        
    D_values = κ * p ** -ε
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
    c, w, ρ, Lbar, r = cost_params

    t_dot_q = sum1(FA_tax_rate * q)
    ttilde = (FA_tax_rate - t_dot_q / sum1(q)) / (1 - t_dot_q / sum1(q))
    return q ** (- 1 / ε) - (ε / (ε - ttilde)) * (ε / (ε - 1)) * c

def all_jurisdictions_FA_tax_rate_on_profits_from_state_s(p, q_consumed, l, k, m, params):
    taxes, factor_weights, production_params, cost_params = params
    FA_tax_rate, separate_acct_tax_rate, sales_tax_rate, payroll_tax_rate = taxes
    c, w, ρ, Lbar, r = cost_params

    sales_weight, payroll_weight, property_weight = factor_weights
    sales_revenue = p * q_consumed
    wages = get_wages(l, params)
    rental_rate_bill = r * k
    taxes_on_total_profits_in_s_by_jurisdiction = \
        sales_weight * sales_revenue / sum1(sales_revenue) * FA_tax_rate \
        + payroll_weight * wages / sum1(wages) * l \
        + property_weight * rental_rate_bill / sum1(rental_rate_bill) * k
    return sum1(taxes_on_total_profits_in_s_by_jurisdiction)

def after_tax_profits(p, q_consumed, trade_matrix, l, k, m, params):
    taxes, factor_weights, production_params, cost_params = params
    FA_tax_rate, separate_acct_tax_rate, sales_tax_rate, payroll_tax_rate = taxes

    revenue = p * q_consumed
    production_cost = factor_costs(l, k, m, params)
    production_state_profits = get_production_state_profits(p, trade_matrix, l, k, m, params)
    return sum1((1 - all_jurisdictions_FA_tax_rate_on_profits_from_state_s(p, q_consumed, l, k, m, params)) * (revenue - production_cost) - separate_acct_tax_rate * production_state_profits - sales_tax_rate * revenue - payroll_tax_rate * sum1(get_wages(l, params))) # - sum1(sum2(non_iceberg_trade_costs * trade_matrix))

def production(l, k, m, params, production_function="linear"):
    taxes, factor_weights, production_params, cost_params = params
    
    if production_function == "linear":
        return l + k + m
    elif production_function == "Cobb-Douglas":
        ε, κ, α, A = production_params        
        return A * l ** α * k ** (1 - α) + m
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

def get_production_state_profits(p, trade_matrix, l, k, m, params):
    state_production_revenues = multiply_every_row_by_same_vector(trade_matrix, p)
    state_costs = factor_costs(l, k, m, params)
    production_state_profits = sum2(state_production_revenues) - state_costs
    return production_state_profits

def get_wages(l, params):
    taxes, factor_weights, production_params, cost_params = params
    c, w, ρ, Lbar, r = cost_params

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
    c, w, ρ, Lbar, r = cost_params

    # Monopsony: l(w) = Lbar w ** ρ
    # As ρ goes to infinity, we get perfect competition.
    # Therefore, input ρ = inf for the perfect competition case (constant w equal to some exogenous w)
    # l(w) = Lbar w ** ρ implies a wage of ρ / (ρ + 1) of marginal revenue product, so 
    # a markdown of 1 / (ρ + 1)
    wage = get_wages(l, params)

    cost = wage * l + r * k + c * m
    return cost

def after_tax_profits_naive(p, q_consumed, trade_matrix, l, k, m, params):
    taxes, factor_weights, production_params, cost_params = params
    FA_tax_rate, separate_acct_tax_rate, sales_tax_rate, payroll_tax_rate = taxes

    revenue = p * q_consumed
    production_cost = factor_costs(l, k, m, params)
    return sum1((1 - naive_all_jurisdictions_FA_tax_rate_on_profits_from_state_s(p, q_consumed, l, k, m, params) - separate_acct_tax_rate) * (revenue - production_cost) - sales_tax_rate * revenue) # - payroll_tax_rate * sum1(p * L(p, params))


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

def get_profit_maxmizing_prices(profit_func, params, x0_vals):

    n_jurisdictions = x0_vals.shape[0]
    # p = SX.sym("p", n_jurisdictions)
    trade_matrix = SX.sym("trade_matrix", n_jurisdictions, n_jurisdictions)
    # Row 0 of trade_matrix is the quantity produced in jurisdiction 1. Entry 0,0 is the quantity it consumes,
    # while entry 0,5 is the quantity produced in 1 and sold to 5.
    # Therefore the sum of column 0 (sum1) is the quantity consumed in jurisdiction 1.
    l = SX.sym("l", n_jurisdictions)
    k = SX.sym("k", n_jurisdictions)
    m = SX.sym("m", n_jurisdictions)
    p = SX.sym("m", n_jurisdictions)

    x = vertcat(trade_matrix.reshape((n_jurisdictions ** 2, 1)), l, k, m, p)
    x_shapes = [trade_matrix.reshape((n_jurisdictions ** 2, 1)).shape, l.shape, k.shape, m.shape, p.shape]

    q_consumed = sum1(trade_matrix).reshape((n_jurisdictions, 1))
    # p = inverse_D(q_consumed, params)
    q_produced = sum2(trade_matrix)

    print(trade_matrix)
    print(q_consumed)
    print(q_produced)
    print(x[:n_jurisdictions ** 2])

    production_constraint = q_produced - production(l, k, m, params, production_function="Cobb-Douglas")
    demand_constraint = q_consumed - D(p, params)

    constraint = vertcat(production_constraint, demand_constraint)

    obj = -profit_func(p, q_consumed, trade_matrix, l, k, m, params)

    x0 = DM(np.full((x.shape[0], x.shape[1]), x0_vals[0]))
    # x0 = DM(np.array(1, 0, 0, 1, 1))

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
    trade_matrix_sol = sol[:n_jurisdictions ** 2].reshape((n_jurisdictions, n_jurisdictions), order = "F")
    l_sol = sol[n_jurisdictions ** 2:n_jurisdictions ** 2 + n_jurisdictions]
    k_sol = sol[n_jurisdictions ** 2 + n_jurisdictions:n_jurisdictions ** 2 + 2 * n_jurisdictions]
    m_sol = sol[n_jurisdictions ** 2 + 2 * n_jurisdictions:n_jurisdictions ** 2 + 3 * n_jurisdictions]
    p_sol = sol[n_jurisdictions ** 2 + 3 * n_jurisdictions:]

    q_consumed_sol = np.sum(trade_matrix_sol, axis=0).squeeze()
    q_produced_sol = np.sum(trade_matrix_sol, axis=1)
    print(sum1(trade_matrix_sol).reshape((n_jurisdictions, 1)))
    print(f"{q_produced_sol = }")
    print(solution)
    print(f"{trade_matrix_sol = }")
    print(f"{l_sol = }")
    print(f"{k_sol = }")
    print(f"{m_sol = }")
    print(f"{p_sol = }")
    print(f"Quantity demanded {D(p_sol, params) = }")
    print(f"Quantity consumed {q_consumed_sol = }")
    print(f"Quantity produced {q_produced_sol = }")
    production_cost = factor_costs(l_sol, k_sol, m_sol, params)
    print(f"{production_cost = }")
    print(f"Production cost per unit {production_cost / q_produced_sol}")
    profits = after_tax_profits(p_sol, q_consumed_sol, trade_matrix_sol, l_sol, k_sol, m_sol, params)
    prod_profits = get_production_state_profits(p_sol, trade_matrix_sol, l_sol, k_sol, m_sol, params)
    print(f"{profits = }")
    print(f"{prod_profits = }")
    if n_jurisdictions == 2:
        print([q_produced_sol[1] - q_consumed_sol[1], q_consumed_sol[1]])
        my_way_trade_matrix = np.array([[q_produced_sol[0], 0], [q_produced_sol[1] - q_consumed_sol[1], q_consumed_sol[1]]])
        profits_my_way = after_tax_profits(p_sol, q_consumed_sol, my_way_trade_matrix, l_sol, k_sol, m_sol, params)
        print(f"{profits_my_way = }")
        prod_profits_my_way = get_production_state_profits(p_sol, my_way_trade_matrix, l_sol, k_sol, m_sol, params)
        print(f"{prod_profits_my_way = }")


    solution_found = was_solution_found(solution, solver.stats(), 0)
    if solution_found:
        return sol
    else:
        return np.full_like(sol, np.nan)


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


unique_firm_years = simmed_data_vals[["firm_id", "year"]].drop_duplicates()

# Initialize columns with the optimal prices and Fajgelbaum et al prices to NaN
simmed_data_vals[["optimal_price", "optimal_quantity", "optimal_revenue",
                  "price_Fajgelbaum_et_al", "quantity_Fajgelbaum_et_al", "revenue_Fajgelbaum_et_al"]] = np.nan
simmed_data_vals.set_index(["firm_id", "state", "year"], inplace=True)


for index, row in unique_firm_years.iterrows():
    this_firm_id = row["firm_id"]
    this_year = row["year"]
    simmed_data_vals_this_firm_year = simmed_data_vals.loc[(this_firm_id, slice(None), this_year), :]
    print("firm is " + str(this_firm_id) + " year is " + str(this_year))
    print("states")
    print(simmed_data_vals_this_firm_year.index)
    # FA_tax_rate = simmed_data_vals_this_firm_year["FA_tax_rate"].to_numpy()
    # separate_acct_tax_rate = simmed_data_vals_this_firm_year["separate_acct_tax_rate"].to_numpy()
    # sales_tax_rate = simmed_data_vals_this_firm_year["sales_tax_rate"].to_numpy()
    # payroll_tax_rate = simmed_data_vals_this_firm_year["payroll_tax_rate"].to_numpy()
    # taxes = [FA_tax_rate, separate_acct_tax_rate, sales_tax_rate, payroll_tax_rate]
    param_names = ["epsilon", "kappa", "alpha", "A", "cost", "wage", "rho", "Lbar", "rental_rate", "sales_weight", "payroll_weight", "property_weight", "FA_tax_rate", "separate_acct_tax_rate", "sales_tax_rate", "payroll_tax_rate"]
    ε, κ, α, A, c, w, ρ, Lbar, r, sales_weight, payroll_weight, property_weight, FA_tax_rate, separate_acct_tax_rate, sales_tax_rate, payroll_tax_rate = simmed_data_vals_this_firm_year[param_names].to_numpy().T
    taxes = [FA_tax_rate, separate_acct_tax_rate, sales_tax_rate, payroll_tax_rate]
    factor_weights = [sales_weight, payroll_weight, property_weight]
    production_params = [ε, κ, α, A]
    cost_params = [c, w, ρ, Lbar, r]
    params_firm_year = [taxes, factor_weights, production_params, cost_params]


    n_jurisdictions = len(simmed_data_vals_this_firm_year)
    initial_price_guess_everywhere = 1
    optimal_price = get_profit_maxmizing_prices(after_tax_profits, params_firm_year, np.full(n_jurisdictions, initial_price_guess_everywhere))
    # optimal_quantity = D(optimal_price, params_firm_year)
    # optimal_revenue = optimal_quantity * optimal_price
    # π = after_tax_profits(optimal_price.squeeze(), params_firm_year) # this is a scalar, total worldwide profits
    # optimal_price_naive = get_profit_maxmizing_prices(after_tax_profits_naive, params_firm_year, np.full(n_jurisdictions, initial_price_guess_everywhere))
    # price_Fajgelbaum_et_al = get_p_Fajgelbaum_et_al(params_firm_year, np.ones((n_jurisdictions, 1)))
    # quantity_Fajgelbaum_et_al = D(price_Fajgelbaum_et_al, params_firm_year)
    # revenue_Fajgelbaum_et_al = quantity_Fajgelbaum_et_al * price_Fajgelbaum_et_al
    # π_Fajgelbaum_et_al = after_tax_profits(price_Fajgelbaum_et_al.squeeze(), params_firm_year)  # this is a scalar, total worldwide profits
    # simmed_data_vals_this_firm_year = simmed_data_vals_this_firm_year.assign(
    #     optimal_price=optimal_price,
    #     optimal_quantity=optimal_quantity,
    #     optimal_revenue=optimal_revenue,
    #     price_Fajgelbaum_et_al=price_Fajgelbaum_et_al,
    #     quantity_Fajgelbaum_et_al=quantity_Fajgelbaum_et_al,
    #     revenue_Fajgelbaum_et_al=revenue_Fajgelbaum_et_al,
    # )

    # simmed_data_vals.update(simmed_data_vals_this_firm_year[[
    #     "optimal_price",
    #     "optimal_quantity",
    #     "optimal_revenue",
    #     "price_Fajgelbaum_et_al",
    #     "quantity_Fajgelbaum_et_al",
    #     "revenue_Fajgelbaum_et_al",
    #     ]])


simmed_data_vals.to_csv("code/simmed_data/" + data_name + "_with_optimal_prices.csv", index=True)
