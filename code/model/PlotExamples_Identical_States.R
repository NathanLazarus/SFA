pacman::p_load(data.table, ggplot2, cowplot, scales)

plus1_formatter <- function(x) {x +1}


data_name = "CocaColaExample"
plot_file_out = paste0(data_name, "_Price_Distortions.pdf")

pricing_data = fread(paste0("code/simmed_data/", data_name, "_with_optimal_prices.csv"))

pricing_data[, N_states := .N, year]
pricing_data[, year_descr := paste0(N_states, "\nCountries")]
pricing_data[year == 0, year_descr := "Equivalent\nSales Tax\n(2.7%)"]
pricing_data[year == 1, year_descr := "1\nCountry"]
pricing_data[, state_descr := fifelse(state == 1, "Country with 10% Higher Taxes", paste0("Country ", state))]

pricing_data[, price_to_plot := optimal_price - 1]

n_states = 5

myColors = c(0, gray.colors(n_states - 1, 0.2, 0.6))
myColors[1] = "#990026"
names(myColors) = c("Country with 10% Higher Taxes", paste("Country", 2:5))
colScale = scale_fill_manual(name = "state_descr", values = myColors)

year_descr_order = c("1\nCountry", "2\nCountries", "3\nCountries", "5\nCountries", "10\nCountries", "50\nCountries", "Equivalent\nSales Tax\n(2.7%)")

pricing_data[, year_descr_to_plot := factor(year_descr, levels = year_descr_order)]

pricing_data[, total_firm_revenue := sum(optimal_state_revenue), .(firm_id, year)]
pricing_data[, total_firm_cost := sum(optimal_state_cost), .(firm_id, year)]
pricing_data[, total_firm_sales_quantity := sum(optimal_quantity_consumed), .(firm_id, year)]
pricing_data[, firm_resulting_sales_weighted_markup := (total_firm_revenue - total_firm_cost) / total_firm_cost]

# For this you'd want the actual trade matrix: production cost of goods that were sold in s relative to price
pricing_data[, state_markup := (optimal_price - (optimal_state_cost / optimal_quantity_produced)) / (optimal_state_cost / optimal_quantity_produced)]
pricing_data[, firm_resulting_sales_weighted_markup2 := sum(state_markup * optimal_quantity_consumed / sum(optimal_quantity_consumed)), .(firm_id, year)]

pricing_data[, .(firm_id, state, year, optimal_price, optimal_quantity_consumed, firm_resulting_sales_weighted_markup, firm_resulting_sales_weighted_markup2)]



ggplot(pricing_data[state <= 5], aes(fill=state_descr, y=price_to_plot, x=year_descr_to_plot)) +
  geom_bar(position="dodge", stat="identity") +
  geom_segment(aes(x = 0.55, xend = 1.45, y = 0, yend = 0), color = myColors[1]) +
  # # Or to make every bar the same width:
  # geom_bar(position=position_dodge2(width = 0.9, preserve = "single"), stat="identity") +
  # geom_segment(aes(x = 0.91, xend = 1.09, y = 0, yend = 0), color = myColors[1]) +
  scale_y_continuous(labels=plus1_formatter) +
  ggtitle("Price") +
  theme_cowplot() +
  # xlab("Number of States") +
  theme(
    plot.title = element_text(size=22, hjust = 0),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    legend.position="bottom",
    legend.title = element_blank(),
    panel.grid.major.y = element_line(linewidth = .1, color="#999999"),
    axis.text=element_text(size=18)
  ) +
  guides(fill = guide_legend(nrow = 2)) +
  colScale

ggsave(plot_file_out, width = 14.5 * 3 / 4, height = 8 * 3 / 4, unit = "in")

