##########################################################################
##                                                                      ##
##  Definition and optimization of a MELCOR model of the                ##
##  IFMIF-DONES Argon Purification Subsystem                            ##
##                                                                      ##
##  Authors: A. Manjavacas, M. A. Vazquez                               ##
##                                                                      ##
##########################################################################

library(tidyverse)

df <- read.csv("./data/scenarios_D.csv")

df$Scenario <- factor(df$Scenario)

new_order <- c(
    "(C1)", "(D1)", "(D5)", "(D9)", "(D13)",
    "(C2)", "(D2)", "(D6)", "(D10)", "(D14)",
    "(C3)", "(D3)", "(D7)", "(D11)", "(D15)",
    "(C4)", "(D4)", "(D8)", "(D12)", "(D16)"
)

df$Scenario <- factor(df$Scenario, levels = new_order)

df$Ar <- (df$Ar - 60) * 60 * 2.5

df_long <- df %>%
    select(c(Scenario, Ar, VR, VI)) %>%
    pivot_longer(cols = c(VR, Ar), names_to = "Variable", values_to = "Value")

ggplot(df_long, aes(x = Scenario, y = Value, fill = Variable)) +
    geom_hline(yintercept = 2400, linetype = "dashed", color = "black") +
    geom_hline(yintercept = 4800, linetype = "dashed", color = "black") +
    geom_hline(yintercept = 6000, linetype = "dashed", color = "black") +
    geom_hline(yintercept = 7200, linetype = "dashed", color = "black") +
    # geom_vline(xintercept = 16.5, alpha = 0.5, color = "red") +
    # geom_vline(xintercept = 12.5, alpha = 0.5, color = "red") +
    # geom_vline(xintercept = 8.5, alpha = 0.5, color = "red") +
    # geom_vline(xintercept = 4.5, alpha = 0.5, color = "red") +
    geom_bar(stat = "identity", position = "dodge", alpha = 0.8, width = 0.7, color = "black") +
    geom_text(label = expression(paste(V[I], "  = 2400")), angle = 270, x = 19.5, y = 2500, color = "black", size = 4) +
    geom_text(label = expression(paste(V[I], "  = 4800")), angle = 270, x = 19.5, y = 4900, color = "black", size = 4) +
    geom_text(label = expression(paste(V[I], "  = 6000")), angle = 270, x = 19.5, y = 6100, color = "black", size = 4) +
    geom_text(label = expression(paste(V[I], "  = 7200")), angle = 270, x = 19.5, y = 7300, color = "black", size = 4) +
    scale_y_continuous(
        name = expression(paste(C[f]^Ar, " (% vol)")),
        breaks = seq(0, 6000, 750),
        labels = seq(60, 100, 5),
        sec.axis = sec_axis(
            ~.,
            name = expression(V[R] ~ ~ (m^3)),
            breaks = c(0, 1200, 2400, 3600, 4800, 6000, 7200)
        )
    ) +
    scale_x_discrete(limits = rev(levels(df$Scenario))) +
    coord_flip() +
    theme_linedraw(base_size = 18) +
    scale_fill_manual(values = c("VR" = "gray", "Ar" = "steelblue")) +
    theme(
        legend.title = element_blank(),
        legend.position = "top"
    )

ggsave("./plots/d_scenarios.png")
