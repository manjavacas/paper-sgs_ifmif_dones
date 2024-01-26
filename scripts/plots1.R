##########################################################################
##                                                                      ##
##  Definition and optimization of a MELCOR model of the                ##
##  IFMIF-DONES Argon Purification Subsystem                            ##
##                                                                      ##
##  Authors: A. Manjavacas, M. A. Vazquez                               ##
##                                                                      ##
##########################################################################

library(tidyverse)
library(grid)

###################### FUNCTIONS ######################

plot_cases <- function(df, filename) {
  # N2 + O2 barplot
  p1 <- ggplot(df, aes(x = df$Scenario)) +
    geom_bar(aes(y = df$N2, fill = "N2"),
      stat = "identity", position = "dodge"
    ) +
    geom_bar(aes(y = df$O2, fill = "O2"),
      stat = "identity", position = "dodge"
    ) +
    geom_text(aes(y = df$N2, label = df$N2),
      vjust = ifelse(df$Scenario == "(B1)", -1.5, -0.4),
      position = position_dodge(width = 0.9)
    ) +
    geom_text(aes(y = df$O2, label = df$O2),
      vjust = -0.5,
      position = position_dodge(width = 0.9)
    ) +
    labs(
      x = "", fill = "",
      y = expression(paste("N"[2], ", ", "O"[2], " ", "(% vol.)"))
    ) +
    scale_fill_manual(values = c("N2" = "orange", "O2" = "red")) +
    theme_linedraw()

  # H2O barplot
  p2 <- ggplot(df, aes(x = df$Scenario, y = df$H2O)) +
    geom_bar(stat = "identity", fill = "navy", position = "dodge") +
    geom_text(aes(label = df$H2O),
      vjust = -0.5,
      position = position_dodge(width = 0.9)
    ) +
    labs(x = "", y = expression(paste("H"[2], "O", " ", "(ppm)"))) +
    theme_linedraw()

  # VR barplot
  p3 <- ggplot(df, aes(x = df$Scenario, y = df$VR)) +
    geom_bar(stat = "identity", fill = "#137539", position = "dodge") +
    geom_text(aes(label = df$VR),
      vjust = -0.5,
      position = position_dodge(width = 0.9)
    ) +
    labs(x = "", y = expression(paste("V"[R], " ", "(m"^3, ")"))) +
    theme_linedraw()

  p <- ggpubr::ggarrange(p1, p2, p3,
    ncol = 3, nrow = 1,
    legend = "top"
  )

  ggsave(filename, plot = p, width = 12, height = 4.8, dpi = 400, bg = "white")
}

plot_impurities <- function(df, filename) {
  df1 <- df %>% filter(grepl("^\\(C[1-4]\\)", df$Scenario))
  df2 <- df %>% filter(grepl("^\\(C[5-8]\\)", df$Scenario))
  df3 <- df %>% filter(grepl("^\\(C(9|1[0-2])\\)", df$Scenario))

  df3$Scenario <- factor(df3$Scenario,
    levels = c("(C9)", "(C10)", "(C11)", "(C12)")
  )

  p1 <- ggplot(df1, aes(x = df1$Scenario, y = 100 - df1$Ar)) +
    geom_bar(stat = "identity", fill = "navy") +
    labs(title = "1 day, 1 VPSA", x = "", y = "") +
    geom_text(aes(label = round(100 - df1$Ar, 2)),
      vjust = 1.5,
      position = position_dodge(width = 0.9),
      color = "white"
    ) +
    theme_linedraw()
  p2 <- ggplot(df2, aes(x = df2$Scenario, y = 100 - df2$Ar)) +
    geom_bar(stat = "identity", fill = "navy") +
    labs(title = "1 day, 2 VPSA", x = "", y = "") +
    geom_text(aes(label = round(100 - df2$Ar, 2)),
      vjust = 1.5,
      position = position_dodge(width = 0.9),
      color = "white"
    ) +
    theme_linedraw()
  p3 <- ggplot(df3, aes(x = df3$Scenario, y = 100 - df3$Ar)) +
    geom_bar(stat = "identity", fill = "navy") +
    labs(title = "1 day, 1 VPSA (saturation)", x = "", y = "") +
    geom_text(aes(label = round(100 - df3$Ar, 2)),
      vjust = 1.5,
      position = position_dodge(width = 0.9),
      color = "white"
    ) +
    theme_linedraw()

  p <- ggpubr::ggarrange(p1, p2, p3,
    ncol = 3, nrow = 1
  )
  p <- ggpubr::annotate_figure(
    p,
    left = textGrob("Total impurities (% vol.)",
      rot = 90,
      vjust = 1, gp = gpar(cex = 0.8)
    ),
  )

  ggsave(filename, plot = p, width = 10, height = 3, dpi = 400, bg = "white")
}

plot_pressures <- function(df, filename) {
  df$med <- 0.5 * (df$MinP + df$MaxP)
  p <- ggplot(
    df,
    aes(
      x = Scenario, ymin = MinP, lower = MinP,
      middle = med, upper = MaxP, ymax = MaxP
    )
  ) +
    geom_boxplot(stat = "identity", fill = "lightgray") +
    labs(x = "", y = "Pressure (Pa)") +
    theme_linedraw()

  ggsave(filename, plot = p, width = 10, height = 6, dpi = 300, bg = "white")
}

# Load data
scenarios <- read_csv("./data/scenarios.csv")

###################### A SCENARIOS ######################

cases_a1_to_a3 <- scenarios %>%
  filter(grepl("^\\(A[1-3]\\)", Scenario))
cases_a4_to_a6 <- scenarios %>%
  filter(grepl("^\\(A[4-6]\\)", Scenario))
cases_a7_to_a9 <- scenarios %>%
  filter(grepl("^\\(A[7-9]\\)", Scenario))

plot_cases(cases_a1_to_a3, "./plots/cases_a1_to_a3.png")
plot_cases(cases_a4_to_a6, "./plots/cases_a4_to_a6.png")
plot_cases(cases_a7_to_a9, "./plots/cases_a7_to_a9.png")

###################### B SCENARIOS ######################

cases_b1_to_b3 <- scenarios %>%
  filter(grepl("^\\(B[1-3]\\)", Scenario))

plot_cases(cases_b1_to_b3, "./plots/cases_b1_to_b3.png")

###################### C SCENARIOS ######################

cases_c1_to_c12 <- scenarios %>%
  filter(grepl("^\\(C([1-9]|1[0-2])\\)", Scenario))

plot_impurities(cases_c1_to_c12, "./plots/cases_c1_to_c12.png")

####################### PRESSURES #######################

# Load data
pressures <- read_csv("./data/min_max_pressures.csv")

a_pressures <- pressures %>%
  filter(grepl("^\\(A[1-9]\\)", Scenario))
b_pressures <- pressures %>%
  filter(grepl("^\\(B[1-3]\\)", Scenario))
c_pressures <- pressures %>%
  filter(grepl("^\\(C([1-9]|1[0-2])\\)", Scenario)) %>%
  mutate(Scenario = factor(Scenario, levels = paste0("(C", 1:12, ")")))

plot_pressures(a_pressures, "./plots/a_pressures.png")
plot_pressures(b_pressures, "./plots/b_pressures.png")
plot_pressures(c_pressures, "./plots/c_pressures.png")
