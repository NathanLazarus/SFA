overall_with_optimal_values = fread("code/simmed_data/simmed_data_vals_with_optimal_prices.csv")

# Slide ideas: find McLure references. Note that passthrough of firm VAT changes is important, well-studied
# If it's genuinely a sales tax
# Where McLure goes wrong: if you pass along a sales tax, you're changing the markup.
# One clear case: consider the case of a 100% SFA tax. "McLure": increase prices by 100%. Actual firms: indifferent

overall_with_optimal_values[, perceived_sales_tax_rate := 1 + tax_rate * 1 / epsilon]
overall_with_optimal_values[, perceived_sales_tax_price := epsilon / (epsilon - 1) * cost * perceived_sales_tax_rate]
overall_with_optimal_values[, perceived_sales_tax_quantity := kappa * perceived_sales_tax_price ^ -epsilon]
overall_with_optimal_values[, perceived_sales_tax_revenue := perceived_sales_tax_price * perceived_sales_tax_quantity]
overall_with_optimal_values[, perceived_sales_tax_state_profits := perceived_sales_tax_quantity * (perceived_sales_tax_price - cost)]
overall_with_optimal_values[, perceived_sales_tax_sales_share := perceived_sales_tax_revenue / sum(perceived_sales_tax_revenue), .(year)]
overall_with_optimal_values[, perceived_sales_tax_state_tax_bill := perceived_sales_tax_sales_share * tax_rate * sum(perceived_sales_tax_state_profits), .(year)]


overall_with_optimal_values[, sales_tax_producer_price := epsilon / (epsilon - 1) * cost]
overall_with_optimal_values[, sales_tax_consumer_price := sales_tax_producer_price * (1 + tax_rate * 1 / epsilon)]
overall_with_optimal_values[, sales_tax_quantity := kappa * sales_tax_consumer_price ^ -epsilon]
overall_with_optimal_values[, sales_tax_profits := sales_tax_quantity * (sales_tax_producer_price - cost)]

overall_with_optimal_values[, naive_price := epsilon / (epsilon - 1) * cost]
# overall_with_optimal_values[state == "California" & year == 2011, naive_price := 1.006]
overall_with_optimal_values[, naive_quantity := kappa * naive_price ^ -epsilon]
overall_with_optimal_values[, naive_revenue := naive_price * naive_quantity]
overall_with_optimal_values[, naive_state_profits := naive_quantity * (naive_price - cost)]
overall_with_optimal_values[, naive_sales_share := naive_revenue / sum(naive_revenue), .(year)]
overall_with_optimal_values[, naive_state_tax_bill := naive_sales_share * tax_rate * sum(naive_state_profits), .(year)]


overall_with_optimal_values[, optimal_state_profits := optimal_quantity * (optimal_price - cost)]
overall_with_optimal_values[, optimal_sales_share := optimal_revenue / sum(optimal_revenue), .(year)]
overall_with_optimal_values[, optimal_state_tax_bill := optimal_sales_share * tax_rate * sum(optimal_state_profits), .(year)]
overall_with_optimal_values[
  ,
  .(
    total_post_tax_naive_profits = sum(naive_state_profits) - sum(naive_state_tax_bill),
    total_post_tax_optimal_profits = sum(optimal_state_profits) - sum(optimal_state_tax_bill),
    total_sales_tax_profits = sum(perceived_sales_tax_state_profits) - sum(perceived_sales_tax_state_tax_bill)
  ),
  .(year)
]

# 101226.0


# Naive McLure: always passing along the corporate tax
# Sophisticated McLure: not passing it along at baseline, but passing changes along
# 
# # Double checking sigma / (sigma - 1) * (1 + tax_rate) is the correct profit maximizing solution with a sales tax
# 
# overall_with_optimal_values[, sales_tax_alt_producer_price_1 := sales_tax_producer_price - 0.001]
# overall_with_optimal_values[, sales_tax_alt_producer_price_2 := sales_tax_producer_price + 0.001]
# overall_with_optimal_values[, sales_tax_alt_consumer_price_1 := sales_tax_alt_producer_price_1 * (1 + tax_rate)]
# overall_with_optimal_values[, sales_tax_alt_consumer_price_2 := sales_tax_alt_producer_price_2 * (1 + tax_rate)]
# overall_with_optimal_values[, sales_tax_alt_quantity_1 := kappa * sales_tax_alt_consumer_price_1 ^ -epsilon]
# overall_with_optimal_values[, sales_tax_alt_quantity_2 := kappa * sales_tax_alt_consumer_price_2 ^ -epsilon]
# overall_with_optimal_values[, sales_tax_alt_profits_1 := sales_tax_alt_quantity_1 * (sales_tax_alt_producer_price_1 - cost)]
# overall_with_optimal_values[, sales_tax_alt_profits_2 := sales_tax_alt_quantity_2 * (sales_tax_alt_producer_price_2 - cost)]
# 
# overall_with_optimal_values[state == "California"]



# overall_with_optimal_values_long = melt(overall_with_optimal_values)
# TODO: finish this, actually do the reshape
# Three times the length, with the type of price/quantity in each row (p_naive, p_McLure, Fajgelbaum, optimal)

# 
# # Now let quantities be a function of elasticities of demand to the firm that are fixed at the firm-state level
# 
# 
# overall_with_optimal_values[, quantity_p_exog := price_p_exog ^ elasticity]
# 
# 
# 
# # Now take p from the Fajgelbaum formula:
# 
# overall_with_optimal_values[, price_Fajgelbaum := quantity_Fajgelbaum ^ elasticity]
# 
# # And from our solutions
# 
# overall_with_optimal_values[, price_optimized := quantity_optimized ^ elasticity]
# 
# # overall_with_optimal_values[, revenue_exog_p := price_exog_p * quantity_p_exog]
# # overall_with_optimal_values[, revenue_Fajgelbaum := price_Fajgelbaum * quantity_Fajgelbaum]
# # overall_with_optimal_values[, revenue_optimized := price_optimized * quantity_optimized]

# 
# overall_with_optimal_values_long[, revenue := price * quantity]
# overall_with_optimal_values_long[, profits := revenue - cost * quantity]
# overall_with_optimal_values_long[, national_profits := sum(profits), .(firm_id, year)]


# overall_with_optimal_values[, post_tax_profits := ]
