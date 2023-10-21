pacman::p_load(data.table, ggplot2, cowplot, scales)

plus1_formatter <- function(x) {x +1}


data_name = "VaryingRates2d"
plot_file_out = paste0(data_name, "_Price_Distortions.pdf")

pricing_data = fread(paste0("code/simmed_data/", data_name, "_with_optimal_prices.csv"))

pricing_data = pricing_data[year <= 100 & year > 80]

pricing_data[, N_states := .N, year]
pricing_data[, year_descr := paste0(N_states, "\nCountries")]
pricing_data[year == 1, year_descr := "1\nCountry"]
pricing_data[, state_descr := paste0("Country ", state)]

pricing_data[, price_to_plot := optimal_price - 1]

myColors = c("#990026", hue_pal()(2)[2], "#5C4033") # #00BFC4

names(myColors) = c(paste("Country", 1:2), "Separate Accounting")
colScale = scale_color_manual(name = "state_descr", values = myColors)

pricing_data[, total_firm_revenue := sum(optimal_state_revenue), .(firm_id, year)]
pricing_data[, total_firm_cost := sum(optimal_state_cost), .(firm_id, year)]
pricing_data[, total_firm_sales_quantity := sum(optimal_quantity_consumed), .(firm_id, year)]
pricing_data[, firm_resulting_sales_weighted_markup := (total_firm_revenue - total_firm_cost) / total_firm_cost]

# For this you'd want the actual trade matrix: production cost of goods that were sold in s relative to price
pricing_data[, state_markup := (optimal_price - (optimal_state_cost / optimal_quantity_produced)) / (optimal_state_cost / optimal_quantity_produced)]
pricing_data[, firm_resulting_sales_weighted_markup2 := sum(state_markup * optimal_quantity_consumed / sum(optimal_quantity_consumed)), .(firm_id, year)]

pricing_data[, .(firm_id, state, year, optimal_price, optimal_quantity_consumed, firm_resulting_sales_weighted_markup, firm_resulting_sales_weighted_markup2)]

pricing_data[, FA_tax_rate_in_1 := FA_tax_rate[state == 1], .(year)]

pricing_data = rbind(pricing_data, pricing_data[state == 1][, state_descr := "Separate Accounting"][, price_to_plot := 0])
pricing_data[, separate_accounting := 1]


ggplot(pricing_data, aes(y = price_to_plot, x = FA_tax_rate_in_1)) +
  geom_segment(aes(x = 0.25, y = 0.935 - 1, xend = 0.25, yend = 1.1 - 1), linetype = "dashed", color = "#999999") +
  annotate("text", x = 0.25, y = max(pricing_data$price_to_plot) * 0.97, label = "Country 2 tax rate", color = "#999999", vjust = -0.5) +
  geom_line(aes(color = state_descr)) +
  scale_y_continuous(labels = plus1_formatter, limits = c(0.935 - 1, 1.12 - 1), expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0.01)) + 
  ggtitle("Price") +
  theme_cowplot() +
  xlab("Tax Rate in Country 1") +
  theme(
    plot.title = element_text(size = 22, hjust = 0),
    axis.title.y = element_blank(),
    # axis.title.x = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.grid.major.y = element_line(linewidth = .1, color = "#999999"),
    axis.text = element_text(size = 18)
  ) +
  guides(fill = guide_legend(nrow = 2)) +
  colScale

ggsave(plot_file_out, width = 14.5 * 3 / 4, height = 8 * 3 / 4, unit = "in")

