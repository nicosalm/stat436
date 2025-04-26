# D&D Spell Explorer
# i've made an interactive visualization that applies dimensionality reduction to explore D&D spells

library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(Rtsne)
library(umap)
library(ggplot2)
library(plotly)

prepare_spell_data <- function() {
    spells_raw <- read_excel("DnD 5e Spells.xlsx")

    names(spells_raw) <- trimws(names(spells_raw))

    # data cleaning
    spells <- spells_raw %>%
        mutate(
            Name = trimws(Name),
            Level = as.numeric(Level),
            School = as.factor(School),
            # we are going to create binary indicators for components
            HasVerbal = ifelse(!is.na(Verbal) & Verbal == "Y", 1, 0),
            HasSomatic = ifelse(!is.na(Somatic) & Somatic == "Y", 1, 0),
            HasMaterial = ifelse(!is.na(Material) & Material == "Y", 1, 0),

            # the next section tries to take what in the PHB is categorical and assign real-world values in seconds to them
            CastingTimeNumeric = case_when(
                grepl("1 Action", `Casting Time`) ~ 1,
                grepl("Bonus", `Casting Time`) ~ 0.5,
                grepl("Reaction", `Casting Time`) ~ 0.3,
                grepl("1 Minute", `Casting Time`) ~ 10,
                grepl("10 Minute", `Casting Time`) ~ 100,
                grepl("1 Hour", `Casting Time`) ~ 600,
                TRUE ~ 1
            ),
            DurationNumeric = case_when(
                grepl("Instantaneous", Duration) ~ 0,
                grepl("1 Round", Duration) ~ 1,
                grepl("1 Minute", Duration) ~ 10,
                grepl("10 Minute", Duration) ~ 100,
                grepl("1 Hour", Duration) ~ 600,
                grepl("8 Hour", Duration) ~ 4800,
                grepl("24 Hour", Duration) ~ 24 * 60,
                grepl("Until", Duration) ~ 100,         #special case for "Until dispelled"
                grepl("Concentration", Duration) ~ 100, # special case for concentration spells
                TRUE ~ 10  # Default
            ),
            RangeNumeric = case_when(
                grepl("Self", Range) ~ 0,
                grepl("Touch", Range) ~ 5,
                grepl("feet", Range) | grepl("ft", Range) ~ as.numeric(gsub("\\D", "", Range)),
                grepl("mile", Range) ~ 5280,
                TRUE ~ 30  # Default
            ),
            # we'll set flags for common damage types
            HasDamage = ifelse(grepl("Damage", `Damage/Effect`) |
                grepl("damage", Details), 1, 0),
            IsHealing = ifelse(grepl("Heal", `Damage/Effect`) |
                grepl("heal", Details), 1, 0),
            IsControl = ifelse(grepl("Control", `Damage/Effect`) |
                grepl("charm", Details) |
                grepl("hold", Details) |
                grepl("stun", Details), 1, 0),
            # also we want to identify whether the spell is from the PHB or later sourcebooks for comparison in our visualization
            IsCore = ifelse(Source == "Players Handbook", 1, 0)
        )

    # now, let's work on the viz
    # first we create a data frame for dimensionality reduction
    spells_numeric <- spells %>%
        select(Level, HasVerbal, HasSomatic, HasMaterial,
            CastingTimeNumeric, DurationNumeric, RangeNumeric,
            HasDamage, IsHealing, IsControl, IsCore)

    # then, one-hot encode the School factor for dimensionality reduction
    school_dummies <- model.matrix(~ 0 + School, data = spells) %>% as.data.frame()
    spells_features <- bind_cols(spells_numeric, school_dummies)
    spells_features_norm <- scale(spells_features)  # normalize

    # finally, return both the original data and the features for dimensionality reduction
    list(
        spells = spells,
        features = spells_features_norm
    )
}

# UI (woo frontend)
ui <- fluidPage(
    tags$head(
        tags$style(HTML("
            body {
            background-color: #f9f7f1;
            color: #333;
            font-family: 'Roboto', sans-serif;
            font-size: 15px;
            line-height: 1.5;
            }
            .controls {
            background-color: #f0e6d2;
            padding: 18px;
            border-radius: 5px;
            box-shadow: 0 2px 5px rgba(0,0,0,0.1);
            margin-right: 15px;
            }
            .control-label {
            font-weight: 500;
            font-size: 16px;
            margin-bottom: 8px;
            display: block;
            }
            .main-panel {
            background-color: #fff;
            padding: 25px;
            border-radius: 5px;
            box-shadow: 0 0 10px rgba(0,0,0,0.1);
            }
            h2 {
            color: #7b341e;
            font-family: 'Cinzel', serif;
            font-size: 24px;
            margin-bottom: 20px;
            }
            h3 {
            color: #7b341e;
            font-family: 'Cinzel', serif;
            font-size: 22px;
            margin-bottom: 18px;
            margin-top: 0;
            }
            .tooltip-inner {
            max-width: 300px;
            padding: 10px;
            }
            .btn-warning {
            background-color: #e67e22;
            border-color: #d35400;
            font-size: 16px;
            padding: 8px 16px;
            margin-top: 10px;
            }
            .btn-warning:hover {
            background-color: #d35400;
            }
            .dimension-label {
            font-style: italic;
            color: #666;
            }
            #spell-details-container {
            margin-top: 30px;
            background-color: #f0e6d2;
            padding: 25px;
            border-radius: 5px;
            box-shadow: 0 2px 5px rgba(0,0,0,0.1);
            }
            #spell_details p {
            margin-bottom: 12px;
            line-height: 1.6;
            }
            #spell_details strong {
            font-weight: 500;
            }
            .spell-name {
            font-size: 28px;
            margin-top: 0;
            margin-bottom: 15px;
            font-family: 'Cinzel', serif;
            color: #7b341e;
            }
            .spell-school-level {
            font-size: 18px;
            margin-bottom: 20px;
            }
            .spell-description {
            margin-top: 20px;
            line-height: 1.8;
            }
            .form-group {
            margin-bottom: 20px;
            }
            .checkbox, .radio {
            margin-top: 2px;
            margin-bottom: 2px;
            }
            "))
    ),
    # this is terrible and should never actually be done on a real site (see: https://nick.winans.io/blog/optimizing-blog-fonts/)
    tags$head(
        tags$link(href = "https://fonts.googleapis.com/css2?family=Cinzel:wght@400;600&family=Roboto:wght@300;400;500&display=swap", rel = "stylesheet")
    ),

    titlePanel(
        div(
            style = "margin-bottom: 20px;",
            h1("D&D Spell Explorer",
                style = "color: #7b341e; font-family: 'Cinzel', serif; font-weight: 600;"),
            p("An interactive dimensionality reduction visualization of 519 D&D spells",
                style = "color: #666; font-style: italic; margin-top: -10px;")
        )
    ),

    # our sidebar
    sidebarLayout(
        sidebarPanel(
            class = "controls",
            width = 4,

            h4("Dimensionality Reduction Technique:", class = "control-label"),
            selectInput("dr_technique", NULL,
                c("t-SNE" = "tsne",
                    "UMAP" = "umap",
                    "PCA" = "pca")),

            conditionalPanel(
                condition = "input.dr_technique == 'tsne'",
                h4("Perplexity:", class = "control-label"),
                sliderInput("perplexity", NULL,
                    min = 5, max = 50, value = 30)
            ),

            conditionalPanel(
                condition = "input.dr_technique == 'umap'",
                h4("Number of Neighbors:", class = "control-label"),
                sliderInput("n_neighbors", NULL,
                    min = 5, max = 50, value = 15)
            ),

            h4("Filter by School:", class = "control-label"),
            checkboxGroupInput("school_filter", NULL,
                c("Abjuration", "Conjuration", "Divination",
                    "Enchantment", "Evocation", "Illusion",
                    "Necromancy", "Transmutation"),
                selected = c("Abjuration", "Conjuration", "Divination",
                    "Enchantment", "Evocation", "Illusion",
                    "Necromancy", "Transmutation")),

            h4("Filter by Spell Level:", class = "control-label"),
            sliderInput("level_filter", NULL,
                min = 0, max = 9, value = c(0, 9), step = 1),

            h4("Filter by Components:", class = "control-label"),
            checkboxGroupInput("component_filter", NULL,
                c("Verbal" = "verbal",
                    "Somatic" = "somatic",
                    "Material" = "material"),
                selected = c("verbal", "somatic", "material")),

            h4("Filter by Source:", class = "control-label"),
            checkboxGroupInput("source_filter", NULL,
                c("Players Handbook" = "Players Handbook",
                    "Other Books" = "other"),
                selected = c("Players Handbook", "other")),

            checkboxInput("show_clusters", "Show Automated Clusters", FALSE),
            actionButton("reset", "Reset All Filters", class = "btn-warning")
        ),

        # main panel (which displays the outputs)
        mainPanel(
            class = "main-panel",

            plotlyOutput("spell_plot", height = "600px"),
            div(
                id = "spell-details-container",

                h3("Spell Details", style = "font-size: 24px; margin-bottom: 20px;"),

                # Selected spell details
                htmlOutput("spell_details")
            )
        )
    )
)

server <- function(input, output, session) {
    data <- reactive({
        prepare_spell_data()
    })

    filtered_data <- reactive({
        req(data())

        spells <- data()$spells
        filtered <- spells %>%
            filter(School %in% input$school_filter) %>%
            filter(Level >= input$level_filter[1] & Level <= input$level_filter[2])

        if ("verbal" %in% input$component_filter) {
            filtered <- filtered %>% filter(HasVerbal == 1)
        }
        if ("somatic" %in% input$component_filter) {
            filtered <- filtered %>% filter(HasSomatic == 1)
        }
        if ("material" %in% input$component_filter) {
            filtered <- filtered %>% filter(HasMaterial == 1)
        }

        if (length(input$source_filter) > 0) {
            if (length(input$source_filter) == 1) {
                if ("Players Handbook" %in% input$source_filter) {
                    filtered <- filtered %>% filter(Source == "Players Handbook")
                } else {
                    filtered <- filtered %>% filter(Source != "Players Handbook")
                }
            }
        }

        filtered
    })

    # here we compute the dimensionality reduction
    # we are using a seed (123) for reproducability
    dr_coordinates <- reactive({
        req(filtered_data())
        spells <- filtered_data()
        indices <- match(spells$Name, data()$spells$Name)
        features <- data()$features[indices, ]
        if (input$dr_technique == "tsne") {
            # t-SNE
            set.seed(123)
            tsne_result <- Rtsne(features, perplexity = input$perplexity,
                check_duplicates = FALSE, pca = TRUE,
                theta = 0.5, dims = 2)
            coords <- tsne_result$Y
            colnames(coords) <- c("x", "y")
        } else if (input$dr_technique == "umap") {
            # UMAP
            set.seed(123)
            umap_result <- umap(features, n_neighbors = input$n_neighbors,
                min_dist = 0.1, n_components = 2)
            coords <- umap_result$layout
            colnames(coords) <- c("x", "y")
        } else if (input$dr_technique == "pca") {
            # PCA
            pca_result <- prcomp(features, center = TRUE, scale. = TRUE)
            coords <- pca_result$x[, 1:2]
            colnames(coords) <- c("x", "y")
        }

        result <- cbind(spells, as.data.frame(coords))

        if (input$show_clusters) {
            set.seed(123)
            k <- min(8, nrow(result))
            clusters <- kmeans(coords, centers = k)
            result$Cluster <- as.factor(clusters$cluster)
        } else {
            result$Cluster <- as.factor(1)
        }

        return(result)
    })

    # main plot gen
    output$spell_plot <- renderPlotly({
        req(dr_coordinates())
        plot_data <- dr_coordinates()

        school_colors <- c(
            "Abjuration" = "#FFEB3B",       # yellow
            "Conjuration" = "#4CAF50",      # green
            "Divination" = "#9C27B0",       # purple
            "Enchantment" = "#FF9800",      # orange
            "Evocation" = "#F44336",        # red
            "Illusion" = "#2196F3",         # blue
            "Necromancy" = "#607D8B",       # grayish
            "Transmutation" = "#009688"     # teal
        )

        variance_explained <- NULL
        if (input$dr_technique == "pca") {
            features <- data()$features[match(plot_data$Name, data()$spells$Name), ]
            pca_result <- prcomp(features, center = TRUE, scale. = TRUE)
            variance <- pca_result$sdev^2
            prop_variance <- variance / sum(variance)
            variance_explained <- prop_variance[1:2] * 100
        }

        p <- ggplot(plot_data, aes(x = x, y = y,
            color = School,
            size = Level + 1,  # we are adding 1 to make cantrips visible (cantrips are level 0 spells in D&D 5e)
            text = paste0(
                "<b>", Name, "</b><br>",
                "Level: ", Level, "<br>",
                "School: ", School, "<br>",
                "Casting Time: ", `Casting Time`, "<br>",
                "Duration: ", Duration, "<br>",
                "Range: ", Range, "<br>",
                "Source: ", Source
            ))) +
            geom_point(alpha = 0.7) +
            scale_color_manual(values = school_colors) +
            scale_size_continuous(range = c(3, 10)) +
            labs(
                title = paste0("D&D Spells visualized using ",
                    switch(input$dr_technique,
                        "tsne" = "t-SNE",
                        "umap" = "UMAP",
                        "pca" = "PCA")),
                subtitle = paste0(nrow(plot_data), " spells shown"),
                x = if(input$dr_technique == "pca") paste0("PC1 (", round(variance_explained[1], 1), "% variance explained)") else "Dimension 1",
                y = if(input$dr_technique == "pca") paste0("PC2 (", round(variance_explained[2], 1), "% variance explained)") else "Dimension 2",
                color = "School",
                size = "Level"
            ) +
            theme_minimal() +
            theme(
                plot.title = element_text(size = 16, face = "bold"),
                plot.subtitle = element_text(size = 12),
                legend.position = "right"
            )

        if (input$show_clusters) {
            p <- p +
                stat_ellipse(aes(group = Cluster),
                    type = "norm",
                    level = 0.8,
                    linetype = 2,
                    color = "black",
                    size = 0.5)
        }

        plt <- ggplotly(p, tooltip = "text") %>%
            plotly::layout(
                hoverlabel = list(
                    bgcolor = "white",
                    bordercolor = "darkgray",
                    font = list(family = "sans-serif", size = 12)
                ),
                legend = list(
                    orientation = "v",
                    y = 0.8
                )
            )

        plt <- plotly::event_register(plt, "plotly_click")
        plt
    })

    output$spell_details <- renderUI({
        event_data <- event_data("plotly_click")

        if (is.null(event_data)) {
            return(HTML("<p>Click on a spell to see its details.</p>"))
        }

        point_index <- event_data$pointNumber + 1
        selected_spell <- dr_coordinates()[point_index, ]

        HTML(paste0(
            "<div style='background-color: #f0e6d2; padding: 15px; border-radius: 5px; margin-top: 20px;'>",
            "<h3 style='color: #7b341e;'>", selected_spell$Name, "</h3>",
            "<p><strong>Level ", selected_spell$Level, " ", selected_spell$School, "</strong></p>",
            "<p><strong>Casting Time:</strong> ", selected_spell$`Casting Time`, "</p>",
            "<p><strong>Duration:</strong> ", selected_spell$Duration, "</p>",
            "<p><strong>Range:</strong> ", selected_spell$Range, "</p>",
            "<p><strong>Components:</strong> ",
            ifelse(selected_spell$HasVerbal == 1, "V ", ""),
            ifelse(selected_spell$HasSomatic == 1, "S ", ""),
            ifelse(selected_spell$HasMaterial == 1, "M ", ""),
            "</p>",
            "<p><strong>Source:</strong> ", selected_spell$Source, "</p>",
            "<p><strong>Description:</strong><br>", gsub("\n", "<br>", selected_spell$Details), "</p>",
            "</div>"
        ))
    })

    observeEvent(input$reset, {
        updateCheckboxGroupInput(session, "school_filter",
            selected = c("Abjuration", "Conjuration", "Divination",
                "Enchantment", "Evocation", "Illusion",
                "Necromancy", "Transmutation"))

        updateSliderInput(session, "level_filter", value = c(0, 9))
        updateCheckboxGroupInput(session, "component_filter", selected = c("verbal", "somatic", "material"))
        updateCheckboxGroupInput(session, "source_filter", selected = c("Players Handbook", "other"))
        updateCheckboxInput(session, "show_clusters", value = FALSE)
    })
}

shinyApp(ui = ui, server = server)
