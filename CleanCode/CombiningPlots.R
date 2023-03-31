#Option 1
detach("package:sjPlot", unload=TRUE)
plot <- plot_grid(histogram_search_strategy_family_across_game_2, learning_curve_innovation_distance,  nrow=1, labels=c('a', 'b'))
save_plot("Results/Search_strategies_Evolution_combined.pdf", plot, base_height=6.5, base_width=10, ncol=1, nrow=1)


#Option 2
plot2 <- ggdraw(learning_curve_round_payoff) + # + theme_half_open(12)) +
          draw_plot(plot_payoff_no_stat_comparison, .12, .55, .40, .35) +
         draw_plot_label(
           c("a", "b"),
           #c(0, 2),
           #c(33, 31),
           c(0.03, 0.14),
           c(1, 0.95),
           size = 12
         )
save_plot("Results/LearningCurve_RoundPayoff_combined.pdf", plot2, base_height=4.5, base_width=7 ,ncol=1, nrow=1)


#Option 3
first_row = plot_grid(learning_curve_round_payoff_by_shareDecision,learning_curve_observed_round_payoff_by_shareDecision, labels = c('a', 'b'))
second_row = plot_grid(plot_additional_info_aross_time,  labels = c('c'), nrow = 1)
gg_all = plot_grid(first_row, second_row, labels=c('', ''), ncol=1)
ggsave(filename = "Results/SharingBenefits.pdf", plot = gg_all, height =10, width = 12, units = "in")

#Option 4

# plot <- ggdraw() +
#   draw_plot(learning_curve_round_payoff_by_shareDecision, x = 0, y = .5, width = .5, height = .5) +
#   draw_plot(learning_curve_observed_round_payoff_by_shareDecision, x = .5, y = .5, width = .5, height = .5) +
#   draw_plot(plot_additional_info_aross_time, x = 0, y = 0, width = 0.685, height = 0.5) +
#   draw_plot_label(label = c("a", "b", "c"), size = 12,
#                   x = c(0, 0.5, 0), y = c(1, 1, 0.5))
# ggsave(filename = "Results/SharingBenefits.pdf", plot = plot, height =10, width = 12, units = "in")


plot <- ggdraw() +
  draw_plot(learning_curve_round_payoff_by_shareDecision, x = 0, y = 0.66, width = 0.97, height = .33) +
  draw_plot(learning_curve_observed_round_payoff_by_shareDecision, x = 0, y = .33, width = 0.95, height = .33) +
  draw_plot(plot_additional_info_aross_time, x = 0, y = 0, width = 0.97, height = 0.33) +
  draw_plot_label(label = c("a", "b", "c"), size = 12,
                  x = c(0, 0, 0), y = c(1, .66, .33))
ggsave(filename = "Results/SharingBenefits_all.pdf", plot = plot, height =10, width = 7, units = "in")



plot <- ggdraw() +
  draw_plot(learning_curve_round_payoff_by_shareDecision, x = 0, y = 0.66, width = 1, height = .33) +
  draw_plot(learning_curve_observed_round_payoff_by_shareDecision_subset, x = 0.1, y = .35, width = 0.8, height = .3) +
  draw_plot(plot_additional_info_aross_time_subset, x = 0.1, y = 0.05, width = 0.825, height = 0.3) +
  draw_plot_label(label = c("a", "b", "c"), size = 12,
                  x = c(0, 0, 0), y = c(1, .66, .35))
ggsave(filename = "Results/SharingBenefits_subsets_combined.pdf", plot = plot, height =10, width = 7, units = "in")

