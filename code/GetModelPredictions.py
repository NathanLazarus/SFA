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
    taxes, ε, κ, α, c, w, r, factor_weights = params
    D_values = κ * p ** -ε
    return D_values


def inverse_D(q, params):
    taxes, ε, κ, α, c, w, r, factor_weights = params
    prices = (q / κ) ** (-1 / ε)
    return prices

def Fajgelbaum_et_al_system(q, params):
    taxes, ε, κ, α, c, w, r, factor_weights = params
    SFA_tax_rate, separate_acct_tax_rate, sales_tax_rate, payroll_tax_rate = taxes
    t_dot_q = sum1(SFA_tax_rate * q)
    ttilde = (SFA_tax_rate - t_dot_q / sum1(q)) / (1 - t_dot_q / sum1(q))
    return q ** (- 1 / ε) - (ε / (ε - ttilde)) * (ε / (ε - 1)) * c

def all_jurisdictions_SFA_tax_rate_on_profits_from_state_s(p, q_consumed, params):
    taxes, ε, κ, α, c, w, r, factor_weights = params
    SFA_tax_rate, separate_acct_tax_rate, sales_tax_rate, payroll_tax_rate = taxes
    sales_weight = factor_weights
    sales_revenue = p * q_consumed
    total_sales_revenue = sum1(sales_revenue)
    taxes_on_total_profits_in_s_by_jurisdiction = sales_weight * sales_revenue / total_sales_revenue * SFA_tax_rate
    return sum1(taxes_on_total_profits_in_s_by_jurisdiction)

def after_tax_profits(p, q_consumed, l, k, m, params):
    taxes, ε, κ, α, c, w, r, factor_weights = params
    SFA_tax_rate, separate_acct_tax_rate, sales_tax_rate, payroll_tax_rate = taxes

    revenue = p * q_consumed
    production_cost = c * m + w * l + r * k
    return sum1((1 - all_jurisdictions_SFA_tax_rate_on_profits_from_state_s(p, q_consumed, params) - separate_acct_tax_rate) * (revenue - production_cost) - sales_tax_rate * revenue) # - payroll_tax_rate * sum1(p * L(p, params))

def production(l, k, m, params):
    taxes, ε, κ, α, c, w, r, factor_weights = params
    return l + k + m # l ** α * k ** (1 - α) + m

def naive_all_jurisdictions_SFA_tax_rate_on_profits_from_state_s(p, q_consumed, params):
    taxes, ε, κ, α, c, w, r, factor_weights = params
    SFA_tax_rate, separate_acct_tax_rate, sales_tax_rate, payroll_tax_rate = taxes
    sales_weight = factor_weights
    n_jurisdictions = ε.shape[0]
    naive_sales_share = 1 / n_jurisdictions
    taxes_on_total_profits_by_jurisdiction = sales_weight * naive_sales_share * SFA_tax_rate
    return sum1(taxes_on_total_profits_by_jurisdiction)

def after_tax_profits_naive(p, q_consumed, l, k, m, params):
    taxes, ε, κ, α, c, w, r, factor_weights = params
    SFA_tax_rate, separate_acct_tax_rate, sales_tax_rate, payroll_tax_rate = taxes

    revenue = p * q_consumed
    production_cost = c * m + w * l + r * k
    return sum1((1 - naive_all_jurisdictions_SFA_tax_rate_on_profits_from_state_s(p, q_consumed, params) - separate_acct_tax_rate) * (revenue - production_cost) - sales_tax_rate * revenue) # - payroll_tax_rate * sum1(p * L(p, params))


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
    l = SX.sym("l", n_jurisdictions)
    k = SX.sym("k", n_jurisdictions)
    m = SX.sym("m", n_jurisdictions)
    p = SX.sym("m", n_jurisdictions)

    x = vertcat(trade_matrix.reshape((n_jurisdictions ** 2, 1)), l, k, m, p)
    print(x)

    q_consumed = sum1(trade_matrix).reshape((n_jurisdictions, 1))
    # p = inverse_D(q_consumed, params)
    q_produced = sum2(trade_matrix)

    production_constraint = q_produced - production(l, k, m, params)
    demand_constraint = q_consumed - D(p, params)

    constraint = vertcat(production_constraint, demand_constraint)

    obj = -profit_func(p, q_consumed, l, k, m, params)

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

    price_sol = np.array(solution["x"]).squeeze()
    print(solution)
    print(price_sol[n_jurisdictions ** 2 + 3 * n_jurisdictions:])
    print(D(price_sol[n_jurisdictions ** 2 + 3 * n_jurisdictions:], params))

    solution_found = was_solution_found(solution, solver.stats(), 0)
    if solution_found:
        return price_sol
    else:
        return np.full_like(price_sol, np.nan)


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
    SFA_tax_rate = simmed_data_vals_this_firm_year["SFA_tax_rate"].to_numpy()
    separate_acct_tax_rate = simmed_data_vals_this_firm_year["separate_acct_tax_rate"].to_numpy()
    sales_tax_rate = simmed_data_vals_this_firm_year["sales_tax_rate"].to_numpy()
    payroll_tax_rate = simmed_data_vals_this_firm_year["payroll_tax_rate"].to_numpy()
    taxes = [SFA_tax_rate, separate_acct_tax_rate, sales_tax_rate, payroll_tax_rate]
    ε = simmed_data_vals_this_firm_year["epsilon"].to_numpy()
    κ = simmed_data_vals_this_firm_year["kappa"].to_numpy()
    α = simmed_data_vals_this_firm_year["alpha"].to_numpy()
    c = simmed_data_vals_this_firm_year["cost"].to_numpy()
    w = simmed_data_vals_this_firm_year["wage"].to_numpy()
    r = simmed_data_vals_this_firm_year["rental_rate"].to_numpy()
    sales_weight = simmed_data_vals_this_firm_year["sales_weight"].to_numpy()
    params_firm_year = [taxes, ε, κ, α, c, w, r, sales_weight]
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
