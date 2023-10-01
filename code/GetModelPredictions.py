from casadi import *
import numpy as np
import pandas as pd
import sys


simmed_data_vals = pd.read_csv("code/simmed_data/simmed_data_params_only.csv", sep=",")

# TODO: double check that this gives the same thing as Mathematica NB
# TODO: implement Fajgelbaum et al. formula
# TODO: add in labor, use payroll weight (also, have we thought about how labor is jointly determined with capital,
# and increasing the payroll weight relative to the property weight won't do anything if it's Leontief?
# And similarly for sales if trade costs are infinite—would be good to be explicity about trade costs.)

def D(p, params):
    t, ε, κ, c, factor_weights = params
    D_values = κ * p ** -ε
    return D_values


def τ(p, params):
    t, ε, κ, c, factor_weights = params
    sales_weight = factor_weights
    sales_revenue = p * D(p, params)
    total_sales_revenue = sum1(sales_revenue)
    taxes_on_total_profits_by_jurisdiction = sales_weight * sales_revenue / total_sales_revenue * t
    return sum1(taxes_on_total_profits_by_jurisdiction)

def before_tax_profits(p, params):
    t, ε, κ, c, factor_weights = params
    return (p - c) * D(p, params)

def after_tax_profits(p, params):
    return sum1((1-τ(p, params))*before_tax_profits(p, params))


def max_profits_casadi(params, x0_vals):

    n_jurisdictions = x0_vals.shape[0]
    p = SX.sym("p", n_jurisdictions)

    x = p
    obj = -after_tax_profits(p, params)

    x0 = DM(x0_vals)

    nlp = {
        "x": x,
        "f": obj,
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
    )

    return np.array(solution["x"])

jurisdiction_type_1 = pd.DataFrame([{"t": 0, "ε": 6, "κ": 10, "c": 1}])
jurisdiction_type_2 = pd.DataFrame([{"t": 0.2, "ε": 6, "κ": 10, "c": 1}])

n_jurisdictions_by_type = np.array([1, 3])


unique_firm_years = simmed_data_vals[["firm_id", "year"]].drop_duplicates()

simmed_data_vals["optimal_price"] = np.nan
simmed_data_vals["quantity"] = np.nan
simmed_data_vals["revenue"] = np.nan
simmed_data_vals.set_index(["firm_id", "state", "year"], inplace=True)
print(simmed_data_vals)


for index, row in unique_firm_years.iterrows():
    this_firm_id = row["firm_id"]
    this_year = row["year"]
    simmed_data_vals_this_firm_year = simmed_data_vals.loc[(this_firm_id, slice(None), this_year), :]
    t = simmed_data_vals_this_firm_year["tax_rate"].to_numpy()
    ε = simmed_data_vals_this_firm_year["epsilon"].to_numpy()
    κ = simmed_data_vals_this_firm_year["kappa"].to_numpy()
    c = simmed_data_vals_this_firm_year["cost"].to_numpy()
    sales_weight = simmed_data_vals_this_firm_year["sales_weight"].to_numpy()
    params_firm_year = [t, ε, κ, c, sales_weight]
    n_jurisdictions = t.shape[0]
    optimal_price = max_profits_casadi(params_firm_year, np.ones((n_jurisdictions, 1))).squeeze()
    quantity = D(optimal_price, params_firm_year)
    revenue = quantity * optimal_price
    π = after_tax_profits(optimal_price.squeeze(), params_firm_year) # this is a scalar, total worldwide profits
    simmed_data_vals_this_firm_year = simmed_data_vals_this_firm_year.assign(optimal_price=optimal_price)
    simmed_data_vals_this_firm_year = simmed_data_vals_this_firm_year.assign(quantity=quantity)
    simmed_data_vals_this_firm_year = simmed_data_vals_this_firm_year.assign(revenue=revenue)

    simmed_data_vals.update(simmed_data_vals_this_firm_year[["optimal_price", "quantity", "revenue"]])


simmed_data_vals.to_csv("code/simmed_data/simmed_data_vals_with_optimal_prices.csv", index=False)
