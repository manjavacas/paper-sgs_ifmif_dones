library(ggplot2)
library(ggbreak)
library(dplyr)
library(tidyr)

df <- read.csv("./data/scenarios_A.csv")

df$Scenario <- factor(df$Scenario)

df <- df %>%
  mutate(N2 = ifelse(Scenario == "(A9)", N2 / 6.7809 * 4.5 * 0.8 - 0.95, N2)) %>%
  mutate(N2 = ifelse(Scenario == "(A8)", N2 / 6.7809 * 4.5 * 0.55 - 0.95, N2)) %>%
  mutate(N2 = ifelse(Scenario == "(A7)", N2 / 6.7809 * 4.5 * 0.30 - 0.95, N2)) %>%
  mutate(O2 = ifelse(Scenario == "(A7)", 4.05, O2))

df <- df %>% mutate(O2 = 10^O2, N2 = 10^N2, H2O = 10^(H2O * 10))

df_long <- df %>%
  pivot_longer(cols = c(O2, N2, H2O, VR), names_to = "Variable", values_to = "Value")

df_long$Variable <- ifelse(df_long$Variable == "H2O", "H2O (x1E5)", df_long$Variable)

ggplot(df_long, aes(x = Scenario, y = Value, fill = Variable)) +
  geom_col(data = df_long %>% filter(Variable != "VR"), aes(y = Value), position = "dodge") +
  geom_col(data = df_long %>% filter(Variable == "VR"), aes(y = Value), color = "black", alpha = 0.3, fill = "gray") +
  scale_y_continuous(
    name = expression(V[R] ~ ~ (m^3)),
    trans = "log10",
    breaks = c(1, 10, 100, 1000, 5000),
    sec.axis = sec_axis(
      ~ log10(.),
      name = "Impurities (% vol)",
      breaks = seq(0, 3.5, 0.5),
    ),
  ) +
  scale_y_break(c(10^3.7, 10^3.87), space = 0.3) +
  geom_text(label = "28.8", x = 3, y = 4.68, color = "black") +
  geom_text(label = "6.8", x = 3.3, y = 3.97, color = "black") +
  geom_text(label = "14.4", x = 2, y = 4.2, color = "black") +
  geom_text(label = "9.6", x = 1, y = 4.07, color = "black") +
  coord_flip() +
  theme_linedraw(base_size = 18) +
  scale_x_discrete(limits = rev(levels(df$Scenario))) +
  scale_fill_manual(values = c("H2O (x1E5)" = "#5fb3cf", "O2" = "#20a420", "N2" = "lightcoral")) +
  theme(
    legend.title = element_blank(),
    legend.position = "top",
    panel.border = element_blank(),
    panel.grid.major = element_line(color = "gray", linewidth = 0.5),
    panel.grid.minor = element_line(color = "gray", linewidth = 0.25),
    axis.line = element_line(colour = "black")
  )

ggsave("./plots/a_scenarios.png")