##########################################################################
##                                                                      ##
##  Definition and optimization of a MELCOR model of the                ##
##  IFMIF-DONES Argon Purification Subsystem                            ##
##                                                                      ##
##  Authors: A. Manjavacas                                              ##
##                                                                      ##
##  Copyright (c) Antonio Manjavacas, 2023                              ##
##  Contact: manjavacas@ugr.es                                          ##
##                                                                      ##
##########################################################################

library(tidyverse)

plot_pressure <- function(df) {
    ggplot(df, aes(x = `t (s)`, y = `P (Pa)`)) +
        geom_line() +
        labs(
            x = "Time (s)",
            y = "Pressure (Pa)"
        ) +
        theme_linedraw()
}

reduce_rows <- function(df, percentage = 25) {
    df %>%
        sample_frac(percentage / 100) %>%
        arrange(`t (s)`)
}


# Load data

A1 <- read_csv("./data/pressures/A1.csv") %>% reduce_rows()
A4 <- read_csv("./data/pressures/A4.csv") %>% reduce_rows()
A7 <- read_csv("./data/pressures/A7.csv") %>% reduce_rows()

B1 <- read_csv("./data/pressures/B1.csv") %>% reduce_rows()
B2 <- read_csv("./data/pressures/B2.csv") %>% reduce_rows()
B3 <- read_csv("./data/pressures/B3.csv") %>% reduce_rows()

C4 <- read_csv("./data/pressures/C4.csv") %>% reduce_rows()
C8 <- read_csv("./data/pressures/C8.csv") %>% reduce_rows()
C12 <- read_csv("./data/pressures/C12.csv") %>% reduce_rows()

D4 <- read_csv("./data/pressures/D4.csv") %>% reduce_rows()
D8 <- read_csv("./data/pressures/D8.csv") %>% reduce_rows()
D12 <- read_csv("./data/pressures/D12.csv") %>% reduce_rows()
D16 <- read_csv("./data/pressures/D16.csv") %>% reduce_rows()

A_scenarios <- list(A1 = A1, A4 = A4, A7 = A7)
B_scenarios <- list(B1 = B1, B2 = B2, B3 = B3)
C_scenarios <- list(C4 = C4, C8 = C8, C12 = C12)
D_scenarios <- list(D4 = D4, D8 = D8, D12 = D12, D16 = D16)


plot_datasets_facet <- function(dataset_list) {
    for (i in seq_along(dataset_list)) {
        dataset_list[[i]]$dataset <- names(dataset_list)[i]
    }

    plot <- ggplot(do.call(rbind, dataset_list), aes(x = `t (s)`, y = `Pnorm (Pa)`)) +
        geom_line() +
        facet_grid(. ~ dataset) +
        labs(x = "Time (s)", y = "Pressure variation (Pa)") +
        theme_linedraw() +
        theme(strip.text = element_text(size = 14))

    plot
}

plot_datasets_combined <- function(dataset_list, filename, leg_order) {
    dataset_list <- lapply(seq_along(dataset_list), function(i) {
        dataset <- dataset_list[[i]]
        dataset$dataset <- factor(names(dataset_list)[i], levels = leg_order)
        return(dataset)
    })

    p <- ggplot(do.call(rbind, dataset_list), aes(x = `t (s)`, y = `Pnorm (Pa)`, color = dataset)) +
        geom_line() +
        labs(x = "Time (s)", y = "Pressure variation (Pa)") +
        theme_linedraw() +
        theme(
            legend.position = "right",
            legend.title = element_blank(),
            legend.text = element_text(size = 14),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 14)
        )

    ggsave(filename, plot = p, width = 10, height = 6, dpi = 300, bg = "white")
}

plot_datasets_combined(A_scenarios, "plots/pressures_A.png", c("A1", "A4", "A7"))
plot_datasets_combined(B_scenarios, "plots/pressures_B.png", c("B1", "B2", "B3"))
plot_datasets_combined(C_scenarios, "plots/pressures_C.png", c("C4", "C8", "C12"))
plot_datasets_combined(D_scenarios, "plots/pressures_D.png", c("D4", "D8", "D12", "D16"))
