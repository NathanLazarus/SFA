overall_with_optimal_values = fread("code/simmed_data/simmed_data_optimal_qs.csv")



overall_with_optimal_values_long = melt(overall_with_optimal_values)
# TODO: finish this, actually do the reshape
# Three times the length, with the type of price/quantity in each row (p_exog, Fajgelbaum, optimal)


# Now let quantities be a function of elasticities of demand to the firm that are fixed at the firm-state level


overall_with_optimal_values[, quantity_p_exog := price_p_exog ^ elasticity]



# Now take p from the Fajgelbaum formula:

overall_with_optimal_values[, price_Fajgelbaum := quantity_Fajgelbaum ^ elasticity]

# And from our solutions

overall_with_optimal_values[, price_optimized := quantity_optimized ^ elasticity]

# overall_with_optimal_values[, revenue_exog_p := price_exog_p * quantity_p_exog]
# overall_with_optimal_values[, revenue_Fajgelbaum := price_Fajgelbaum * quantity_Fajgelbaum]
# overall_with_optimal_values[, revenue_optimized := price_optimized * quantity_optimized]


overall_with_optimal_values_long[, revenue := price * quantity]
overall_with_optimal_values_long[, profits := revenue - cost * quantity]
overall_with_optimal_values_long[, national_profits := sum(profits), .(firm_id, year)]
