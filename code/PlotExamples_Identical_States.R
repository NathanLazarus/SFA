pacman::p_load(data.table, ggplot2, cowplot, scales)

plus1_formatter <- function(x) {x +1}

pricing_data = fread("code/simmed_data/ThreeStates_with_optimal_prices.csv")

pricing_data[, N_states := .N, year]
pricing_data[, year_descr := paste0(N_states, " states")]
pricing_data[year == 0, year_descr := "Sales Tax"]
pricing_data[year == 1, year_descr := "1 state"]
pricing_data[, state_descr := fifelse(state == 1, "State with Positive Taxes", paste0("State ", state))]

pricing_data[, price_to_plot := optimal_price - 1]

n_states = 5

myColors = c(0, gray.colors(n_states - 1, 0.2, 0.6))
myColors[1] = "#990026"
names(myColors) = c("State with Positive Taxes", paste("State", 2:5))
colScale = scale_fill_manual(name = "state_descr", values = myColors)

year_descr_order = c("1 state", "2 states", "3 states", "5 states", "10 states", "50 states", "Sales Tax")

pricing_data[, year_descr_to_plot := factor(year_descr, levels = year_descr_order)]

ggplot(pricing_data[state <= 5], aes(fill=state_descr, y=price_to_plot, x=year_descr_to_plot)) + 
  geom_bar(position="dodge", stat="identity") +
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

ggsave("Price_Distortions.pdf")

