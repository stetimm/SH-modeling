#plot_distributions
#load distributions
source("distributions.R")
require("ggplot2")

#set range
x = seq(0, 1, by=0.01)
plot.data = data.frame(x)

#define values of interest
plot.data$G_H = G_H(x)
plot.data$G_L = G_L(x)
plot.data$F_H = F_H(x)
plot.data$F_L = F_L(x)
plot.data$g_H = g_H(x)
plot.data$g_L = g_L(x)
plot.data$f_H = f_H(x)
plot.data$f_L = f_L(x)
plot.data$test_H = plot.data$G_H - plot.data$F_H
plot.data$test_L = plot.data$G_L - plot.data$F_L
plot.data$test_F = plot.data$F_H - plot.data$F_L
plot.data$test_G = plot.data$G_H - plot.data$G_L
plot.data$mlrp_G = plot.data$g_H/plot.data$g_L
plot.data$mlrp_F = plot.data$f_H/plot.data$f_L

plot_distributions = ggplot(plot.data, aes(x, y=value, color = variable)) +
  geom_line(aes(y=F_L, col = "SH and low type")) +
  geom_line(aes(y=F_H, col = "SH and high type")) +
  geom_line(aes(y=G_L, col = "L and low type")) +
  geom_line(aes(y=G_H, col = "L and high type")) +
  labs(title = "Cumulative Distributions", x = "Difficulty in (0,1)", y = "Probability of failing")
plot_distributions + coord_fixed(ratio = 0.6)

#plot densities
x = seq(0, 1, by=0.01)

plot_densities = ggplot(plot.data, aes(x, y=value, color = variable)) +
  geom_line(aes(y=f_L, col = "SH and low type")) +
  geom_line(aes(y=f_H, col = "SH and high type")) +
  geom_line(aes(y=g_L, col = "L and low type")) +
  geom_line(aes(y=g_H, col = "L and high type")) +
  labs(title = "Probability densities", x = "Score in (0,1)", y = "Density")
plot_densities + coord_fixed(ratio = 0.3)

#plot mlrp ratio

x = seq(0, 1, by=0.01)
plot_mlrp = ggplot(plot.data, aes(x, y=value, color = variable)) +
  geom_line(aes(y=mlrp_F, col = "Ratio for F")) +
  #geom_line(aes(y=f_L, col = "f_L")) +
  #geom_line(aes(y=f_H, col = "f_H")) +
  #geom_line(aes(y=mlrp_G, col = "Ratio for G")) +
  labs(title = "Ratios of density functions", x = "Difficulty in (0,1)", y = "Ratio")
plot_mlrp
