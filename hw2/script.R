# Global Temperature Anomalies Explorer
# STAT436 Shiny Portfolio Project

library(shiny)
library(tidyverse)
library(plotly)
library(DT)
library(shinythemes)

# first, we must define the user interface
ui <- fluidPage(
    theme = shinythemes::shinytheme("flatly"),

    titlePanel("Global Temperature Anomalies Explorer"),

    sidebarLayout(
        sidebarPanel(
            width = 3,

            HTML("<p><strong>Explore how global temperatures have changed over time.</strong></p>
                <p>This app visualizes temperature anomalies from the
                1901-2000 baseline using NOAA climate data.</p>"),

            sliderInput("yearRange",
                "Select Time Period:",
                min = 1850,
                max = 2023,
                value = c(1950, 2023),
                step = 1,
                sep = ""),

            radioButtons("vizType",
                "Choose Visualization:",
                choices = c("Line Chart" = "line",
                    "Bar Chart" = "bar",
                    "Smoothed Trend" = "smooth"),
                selected = "line"),

            checkboxGroupInput("refPeriods",
                "Show Reference Periods:",
                choices = c("Pre-Industrial (1850-1900)" = "preindustrial",
                    "Mid-Century (1950-1980)" = "midcentury",
                    "Recent Decades (1990-2020)" = "recent"),
                selected = "preindustrial"),

            checkboxInput("showTrend", "Show Linear Trend Line", TRUE),

            HTML("<hr><small>Data: NOAA National Centers for Environmental Information,
                <a href='https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/global/time-series'
                target='_blank'>Climate at a Glance</a>.</small>")
        ),

        mainPanel(
            width = 9,

            tabsetPanel(
                tabPanel("Temperature Trends",
                    br(),
                    plotlyOutput("tempPlot", height = "500px"),
                    br(),
                    htmlOutput("trendSummary")),

                tabPanel("Warming Since Pre-Industrial",
                    br(),
                    plotlyOutput("diffPlot", height = "500px"),
                    br(),
                    htmlOutput("diffSummary")),

                tabPanel("Data Table",
                    br(),
                    DTOutput("dataTable"),
                    br(),
                    downloadButton("downloadData", "Download Filtered Data"))
            )
        )
    )
)

# Set up the server
server <- function(input, output, session) {

    all_data <- reactive({
        read.csv("data.csv", comment.char = "#", header = TRUE)
    })

    filtered_data <- reactive({
        all_data() %>%
            filter(Year >= input$yearRange[1], Year <= input$yearRange[2])
    })

    period_averages <- reactive({
        data <- all_data()

        list(
            preindustrial = data %>%
                filter(Year >= 1850, Year <= 1900) %>%
                summarise(avg = mean(Anomaly)) %>%
                pull(avg),

            midcentury = data %>%
                filter(Year >= 1950, Year <= 1980) %>%
                summarise(avg = mean(Anomaly)) %>%
                pull(avg),

            recent = data %>%
                filter(Year >= 1990, Year <= 2020) %>%
                summarise(avg = mean(Anomaly)) %>%
                pull(avg)
        )
    })

    warming_data <- reactive({
        baseline <- period_averages()$preindustrial

        filtered_data() %>%
            mutate(Warming = Anomaly - baseline)
    })

    trend <- reactive({
        lm(Anomaly ~ Year, data = filtered_data())
    })

    output$tempPlot <- renderPlotly({
        data <- filtered_data()
        periods <- period_averages()

        # Calculate plot boundaries
        year_range <- max(data$Year) - min(data$Year)
        x_label_pos <- min(data$Year) + (year_range * 0.05)  # Add a little left margin

        p <- ggplot(data, aes(x = Year, y = Anomaly)) +
            geom_hline(yintercept = 0, linetype = "dashed", color = "darkgray") +
            labs(
                title = "Global Temperature Anomalies",
                subtitle = "Temperature deviation from 1901-2000 average",
                x = "Year",
                y = "Temperature Anomaly (°C)",
                caption = "Data source: NOAA Climate at a Glance"
            )

        # Introduce reference period lines
        if ("preindustrial" %in% input$refPeriods) {
            p <- p + geom_hline(yintercept = periods$preindustrial,
                linetype = "dotted", color = "blue", linewidth = 0.7) +
                annotate("text", x = x_label_pos,
                    y = periods$preindustrial + 0.05,
                    label = "Pre-Industrial Average",
                    hjust = 0, vjust = 0, color = "blue", size = 3.2)
        }

        if ("midcentury" %in% input$refPeriods) {
            p <- p + geom_hline(yintercept = periods$midcentury,
                linetype = "dotted", color = "purple", linewidth = 0.7) +
                annotate("text", x = x_label_pos,
                    y = periods$midcentury + 0.05,
                    label = "Mid-Century Average",
                    hjust = 0, vjust = 0, color = "purple", size = 3.2)
        }

        if ("recent" %in% input$refPeriods) {
            p <- p + geom_hline(yintercept = periods$recent,
                linetype = "dotted", color = "red", linewidth = 0.7) +
                annotate("text", x = x_label_pos,
                    y = periods$recent + 0.05,
                    label = "Recent Decades Average",
                    hjust = 0, vjust = 0, color = "red", size = 3.2)
        }

        if (input$vizType == "line") {
            p <- p + geom_line(color = "#1f77b4", linewidth = 0.8) +
                geom_point(color = "#1f77b4", size = 2, alpha = 0.7)
        } else if (input$vizType == "bar") {
            p <- p + geom_col(aes(fill = Anomaly), width = 0.7, alpha = 0.8) +
                scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                    midpoint = 0, guide = "none")
        } else if (input$vizType == "smooth") {
            p <- p + geom_point(color = "#1f77b4", size = 2, alpha = 0.5) +
                geom_smooth(color = "#ff7f0e", linewidth = 1.2, span = 0.2, se = FALSE)
        }

        if (input$showTrend) {
            model <- trend()
            trend_line <- data.frame(
                Year = seq(min(data$Year), max(data$Year), length.out = 100)
            )
            trend_line$Anomaly <- predict(model, newdata = trend_line)

            p <- p + geom_line(data = trend_line, aes(x = Year, y = Anomaly),
                color = "red", linewidth = 1, linetype = "solid")
        }

        p <- p + theme_minimal() +
            theme(
                plot.title = element_text(size = 16, face = "bold"),
                plot.subtitle = element_text(size = 12),
                axis.title = element_text(size = 12),
                axis.text = element_text(size = 10),
                legend.position = "bottom"
            )

        # Convert to plotly with improved layout
        ggplotly(p) %>%
            layout(
                hovermode = "x unified",
                hoverlabel = list(namelength = -1),
                margin = list(l = 60, r = 40, b = 60, t = 80)
            )
    })

    output$diffPlot <- renderPlotly({
        data <- warming_data()
        year_range <- max(data$Year) - min(data$Year)
        x_label_pos <- min(data$Year) + (year_range * 0.05)

        p <- ggplot(data, aes(x = Year, y = Warming)) +
            geom_hline(yintercept = 0, linetype = "dashed", color = "darkgray") +
            geom_col(aes(fill = Warming), width = 0.7, alpha = 0.8) +
            scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                midpoint = 0, guide = "none") +
            labs(
                title = "Warming Since Pre-Industrial Period",
                subtitle = "Temperature change relative to 1850-1900 average",
                x = "Year",
                y = "Temperature Increase (°C)",
                caption = "Data source: NOAA Climate at a Glance"
            ) +
            theme_minimal() +
            theme(
                plot.title = element_text(size = 16, face = "bold"),
                plot.subtitle = element_text(size = 12),
                axis.title = element_text(size = 12),
                axis.text = element_text(size = 10)
            )

        # Thresholds
        p <- p + geom_hline(yintercept = 1.5, linetype = "dotted", color = "orange", linewidth = 0.7) +
            annotate("text", x = x_label_pos, y = 1.5 + 0.1,
                label = "1.5°C threshold", hjust = 0, vjust = 0, color = "orange", size = 3.2)

        p <- p + geom_hline(yintercept = 2.0, linetype = "dotted", color = "red", linewidth = 0.7) +
            annotate("text", x = x_label_pos, y = 2.0 + 0.1,
                label = "2.0°C threshold", hjust = 0, vjust = 0, color = "red", size = 3.2)

        ggplotly(p) %>%
            layout(
                hovermode = "x unified",
                hoverlabel = list(namelength = -1),
                margin = list(l = 60, r = 40, b = 60, t = 80) # Increase margins
            )
    })

    output$trendSummary <- renderUI({
        model <- trend()
        rate_per_decade <- coef(model)[2] * 10
        total_change <- rate_per_decade * (input$yearRange[2] - input$yearRange[1]) / 10

        data <- filtered_data()
        midpoint <- mean(c(input$yearRange[1], input$yearRange[2]))

        first_half <- data %>% filter(Year < midpoint)
        second_half <- data %>% filter(Year >= midpoint)

        if (nrow(first_half) > 5 && nrow(second_half) > 5) {
            model1 <- lm(Anomaly ~ Year, data = first_half)
            model2 <- lm(Anomaly ~ Year, data = second_half)

            rate1 <- coef(model1)[2] * 10
            rate2 <- coef(model2)[2] * 10
            acceleration <- rate2 - rate1

            HTML(paste0(
                "<div class='panel panel-info'>",
                "<div class='panel-heading'><h4>Analysis</h4></div>",
                "<div class='panel-body'>",
                "<p><strong>Trend Analysis:</strong> For the selected period (",
                input$yearRange[1], "-", input$yearRange[2], "), temperatures are ",
                ifelse(rate_per_decade > 0, "increasing", "decreasing"), " at a rate of <b>",
                round(abs(rate_per_decade), 3), "°C per decade</b>.</p>",

                "<p>The overall change during this period is approximately <b>",
                round(total_change, 2), "°C</b>.</p>",

                "<p><strong>Warming Acceleration:</strong> The rate of warming in the second half of this period ",
                ifelse(acceleration > 0, "increased", "decreased"), " by <b>",
                round(abs(acceleration), 3), "°C per decade</b> compared to the first half.</p>",

                "</div></div>"
            ))
        } else {
            HTML(paste0(
                "<div class='panel panel-info'>",
                "<div class='panel-heading'><h4>Analysis</h4></div>",
                "<div class='panel-body'>",
                "<p><strong>Trend Analysis:</strong> For the selected period (",
                input$yearRange[1], "-", input$yearRange[2], "), temperatures are ",
                ifelse(rate_per_decade > 0, "increasing", "decreasing"), " at a rate of <b>",
                round(abs(rate_per_decade), 3), "°C per decade</b>.</p>",

                "<p>The overall change during this period is approximately <b>",
                round(total_change, 2), "°C</b>.</p>",

                "<p>(Select a longer time period to see warming acceleration analysis)</p>",

                "</div></div>"
            ))
        }
    })

    output$diffSummary <- renderUI({
        data <- warming_data()

        crosses_1p5 <- data %>% filter(Warming >= 1.5)
        year_1p5 <- if(nrow(crosses_1p5) > 0) min(crosses_1p5$Year) else "Not yet"

        crosses_2p0 <- data %>% filter(Warming >= 2.0)
        year_2p0 <- if(nrow(crosses_2p0) > 0) min(crosses_2p0$Year) else "Not yet"

        latest_warming <- data %>%
            filter(Year == max(Year)) %>%
            pull(Warming)

        HTML(paste0(
            "<div class='panel panel-info'>",
            "<div class='panel-heading'><h4>Climate Thresholds</h4></div>",
            "<div class='panel-body'>",

            "<p>Warming since pre-industrial times reached <b>",
            round(latest_warming, 2), "°C</b> by ", max(data$Year), ".</p>",

            "<p>The 1.5°C threshold was first exceeded in <b>", year_1p5, "</b>.</p>",

            "<p>The 2.0°C threshold was first exceeded in <b>", year_2p0, "</b>.</p>",

            "<p>The Paris Climate Agreement aims to limit warming to well below 2.0°C above pre-industrial levels, with an aspirational goal of 1.5°C.</p>",

            "</div></div>"
        ))
    })

    output$dataTable <- renderDT({
        datatable(filtered_data(),
            options = list(pageLength = 15,
                dom = 'ftip',
                searching = TRUE,
                ordering = TRUE),
            rownames = FALSE)
    })

    output$downloadData <- downloadHandler(
        filename = function() {
            paste0("climate_data_", input$yearRange[1], "-", input$yearRange[2], ".csv")
        },
        content = function(file) {
            write.csv(filtered_data(), file, row.names = FALSE)
        }
    )
}

# Run the App
shinyApp(ui = ui, server = server)
