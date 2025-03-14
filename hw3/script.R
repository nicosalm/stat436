# HOW TO RUN:
# Terminal: Rscript script.R
# R Console: source("script.R")
# RStudio: Click "Source" or press Ctrl+Shift+S

if (!require(tidyverse)) install.packages("tidyverse")
if (!require(patchwork)) install.packages("patchwork")
if (!require(scales)) install.packages("scales")

library(tidyverse)
library(patchwork)
library(scales)

theme_set(
    theme_minimal() +
        theme(
            plot.title = element_text(size = 16, face = "bold", margin = margin(b = 15)),
            plot.subtitle = element_text(size = 12, margin = margin(b = 20)),
            plot.caption = element_text(size = 9, color = "gray30", margin = margin(t = 20)),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_line(color = "gray90"),
            panel.grid.major.y = element_line(color = "gray90"),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 10),
            legend.position = "bottom",
            legend.title = element_text(size = 12),
            legend.text = element_text(size = 10),
            strip.text = element_text(size = 12, face = "bold", color = "white"),
            plot.margin = margin(20, 20, 20, 20),
            panel.background = element_rect(fill = "white", color = NA),
            strip.background = element_rect(fill = "gray30", color = NA)
        )
)

renewable_data <- read_csv("data/modern-renewable-energy-consumption.csv", show_col_types = FALSE)

countries_of_interest <- c("World", "United States", "China", "Germany", "Denmark")

hydro_col <- "Hydro generation - TWh"
wind_col <- "Wind generation - TWh"
solar_col <- "Solar generation - TWh"
other_col <- "Other renewables (including geothermal and biomass) electricity generation - TWh"

renewable_long <- renewable_data %>%
    filter(Entity %in% countries_of_interest) %>%
    select(Entity, Code, Year, all_of(c(hydro_col, wind_col, solar_col, other_col))) %>%
    pivot_longer(
        cols = all_of(c(hydro_col, wind_col, solar_col, other_col)),
        names_to = "source",
        values_to = "twh"
    ) %>%
    mutate(
        source = case_when(
            source == hydro_col ~ "Hydropower",
            source == wind_col ~ "Wind",
            source == solar_col ~ "Solar",
            source == other_col ~ "Other Renewables",
            TRUE ~ source
        ),
        twh = replace_na(twh, 0)
    )

recent_data <- renewable_long %>%
    filter(Year >= 2000, Year <= 2023)

source_colors <- c(
    "Hydropower" = "#053061",
    "Wind" = "#2166ac",
    "Solar" = "#f7e362",
    "Other Renewables" = "#4daf4a"
)

# PART 1: UPWARDS - Growth in renewable sources
p1 <- recent_data %>%
    filter(Entity %in% c("China", "United States", "Germany", "Denmark")) %>%
    group_by(Entity, Year) %>%
    summarize(total_twh = sum(twh, na.rm = TRUE), .groups = "drop") %>%
    ggplot(aes(x = Year, y = total_twh)) +
    geom_area(aes(fill = Entity), alpha = 0.8) +
    geom_line(aes(color = Entity), linewidth = 1) +
    facet_wrap(~Entity, scales = "free_y", ncol = 2) +
    scale_fill_manual(values = c(
        "China" = "#1a9850",
        "Denmark" = "#66bd63",
        "Germany" = "#a6d96a",
        "United States" = "#d9ef8b"
    )) +
    scale_color_manual(values = c(
        "China" = "#004529",
        "Denmark" = "#238b45",
        "Germany" = "#41ab5d",
        "United States" = "#74c476"
    )) +
    scale_y_continuous(
        labels = function(x) paste0(round(x/1000, 1), " k"),
        expand = c(0, 0)
    ) +
    guides(fill = "none", color = "none") +
    labs(
        title = "UPWARDS: The Rise of Renewable Energy (2000-2023)",
        subtitle = "Total renewable electricity generation has grown substantially in all countries",
        x = "Year",
        y = "Total Renewable Generation (TWh)"
    ) +
    theme(
        strip.background = element_rect(fill = "#4DAF4A", color = NA),
        strip.text = element_text(color = "white", face = "bold"),
        panel.background = element_rect(fill = "#f7fcf5", color = NA)
    )

# PART 2: DOWNWARDS - Mirrored chart
hydro_vs_other <- renewable_long %>%
    filter(Entity %in% c("China", "United States", "Germany", "World"),
        Year >= 2000, Year <= 2023) %>%
    mutate(category = if_else(source == "Hydropower", "Hydro", "Non-Hydro")) %>%
    group_by(Entity, Year, category) %>%
    summarize(twh = sum(twh, na.rm = TRUE), .groups = "drop") %>%
    group_by(Entity, Year) %>%
    mutate(
        total = sum(twh),
        percentage = twh / total * 100,
        mirror_value = if_else(category == "Hydro", -percentage, percentage)
    )

p2 <- hydro_vs_other %>%
    ggplot(aes(x = Year, y = mirror_value, fill = category)) +
    geom_area(alpha = 0.9) +
    facet_wrap(~Entity, ncol = 4) +
    scale_fill_manual(values = c("Hydro" = "#2166ac", "Non-Hydro" = "#b2df8a")) +
    scale_y_continuous(
        labels = function(x) paste0(abs(round(x)), "%"),
        breaks = seq(-100, 100, by = 25)
    ) +
    geom_hline(yintercept = 0, linetype = "solid", color = "white", size = 1) +
    coord_cartesian(ylim = c(-100, 100)) +
    labs(
        title = "DOWNWARDS vs UPWARDS: Mirrored View of Hydro vs Other Renewables",
        subtitle = "Hydropower (below) is declining in share while other renewables (above) gain importance",
        x = "Year",
        y = "Share of Total Renewables (%)",
        fill = "Energy Category"
    ) +
    theme(
        strip.background = element_rect(fill = "#2166ac", color = NA),
        strip.text = element_text(color = "white", face = "bold"),
        panel.background = element_rect(fill = "#f1f9ff", color = NA),
        legend.position = "bottom"
    )

# PART 3: Flow of energy sources
yearly_source_mix <- renewable_long %>%
    filter(Entity == "World", Year >= 2000, Year <= 2023) %>%
    group_by(Year, source) %>%
    summarize(twh = sum(twh, na.rm = TRUE), .groups = "drop") %>%
    group_by(Year) %>%
    mutate(
        total = sum(twh),
        percentage = twh / total * 100
    ) %>%
    ungroup() %>%
    mutate(source = factor(source, levels = c("Hydropower", "Other Renewables", "Wind", "Solar")))

p3 <- yearly_source_mix %>%
    ggplot(aes(x = Year, y = percentage, fill = source)) +
    geom_area(alpha = 0.9, position = "stack") +
    scale_fill_manual(values = source_colors) +
    scale_x_continuous(breaks = seq(2000, 2020, by = 5)) +
    scale_y_continuous(
        labels = function(x) paste0(round(x), "%"),
        expand = c(0, 0)
    ) +
    labs(
        title = "Flow of Renewable Energy Sources (2000-2023)",
        subtitle = "From dark blue (hydropower) to bright yellow (solar), mirroring the transition from night to day",
        x = "Year",
        y = "Share of Renewables (%)"
    ) +
    theme(
        panel.background = element_rect(fill = "#fcfbf8", color = NA),
        legend.position = "bottom"
    )

final_plot <- (p1 / p2 / p3) +
    plot_layout(heights = c(2, 2, 1.5), guides = "collect") &
    theme(
        legend.position = "bottom",
        plot.background = element_rect(fill = "white", color = NA)
    ) &
    plot_annotation(
        title = "Upwards & Downwards in Green Energy",
        subtitle = "The renewable energy transition features both upward growth and downward shifts in technology importance",
        caption = "#30DayChartChallenge | Day 21: Upwards/Downwards | Data: Energy Institute - Statistical Review of World Energy via Our World in Data",
        theme = theme(
            plot.title = element_text(size = 20, hjust = 0.5, margin = margin(b = 10)),
            plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(b = 25)),
            plot.caption = element_text(size = 10, hjust = 1, margin = margin(t = 15)),
            plot.background = element_rect(fill = "white", color = NA)
        )
    )

final_plot
ggsave("upwards_downwards_green_energy.png", final_plot, width = 12, height = 14, dpi = 300)
