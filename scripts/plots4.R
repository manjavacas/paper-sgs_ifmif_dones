##########################################################################
##                                                                      ##
##  Definition and optimization of a MELCOR model of the                ##
##  IFMIF-DONES Argon Purification Subsystem                            ##
##                                                                      ##
##  Authors: A. Manjavacas, M. A. Vazquez                               ##
##                                                                      ##
##########################################################################

library(ggbreak)
library(tidyverse)

df <- read.csv("./data/scenarios_B.csv")

df$Scenario <- factor(df$Scenario)
# df <- df %>% mutate(O2 = 10^O2, N2 = 10^N2, H2O = 10^(H2O * 10))
df <- df %>% mutate(VR = log10(VR), VI = log10(VI), H2O = H2O * 10)

df_long <- df %>%
  pivot_longer(cols = c(O2, N2, H2O, VR, VI), names_to = "Variable", values_to = "Value")

df_long$Variable <- ifelse(df_long$Variable == "H2O", "H2O (x10)", df_long$Variable)

ggplot(df_long, aes(x = Scenario, y = Value, fill = Variable)) +
  geom_col(data = df_long %>% filter(!Variable %in% c("VR", "VI")), aes(y = Value), position = "dodge", width=0.8) +
  geom_col(data = df_long %>% filter(Variable %in% c("VR", "VI")), aes(y = Value), alpha=0.4, color = "black", position="dodge") +
  geom_text(label = "27.1", x = 1, y = 26.95, color = "black", size=5.5) +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(df$Scenario))) +
  scale_fill_manual(values = c("H2O (x10)" = "#5fb3cf", "O2" = "#20a420", "N2" = "lightcoral", "VR" = "gray", "VI" = "white")) +
  scale_y_continuous(
    name = expression(paste(V[R], ", ", V[I] ~ ~ (m^3))),
    breaks = c(1, 2, 3, 4, 5, 6),
    labels = function(x) format(c(10, 100, 1e3, 1e4, 1e5, 1e6), scientific=TRUE),
    sec.axis = sec_axis(
      ~.,
      name = "Impurities (% vol)",
      breaks = c(0, 2, 4, 6, 8)
    )
  ) +
  expand_limits(y=27.5) +
  scale_y_break(c(7, 26.5), space = 0.3) +
  theme_linedraw(base_size = 18) + 
  theme(
    legend.title = element_blank(),
    legend.position = "top"
  )
ggsave("./plots/b_scenarios.png")
