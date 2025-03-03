# Data Visualization and Interactive Graphics Notesheet

## Midterm Topics Review

### Key Areas to Focus On
1. **Faceting and compound figures**: Understanding how to create and arrange multiple plots
2. **Data tidying**: Converting data between formats to support visualization
3. **Data transformation**: Creating derived variables and reshaping for specific plot types
4. **Reactive graphs in Shiny**: Understanding dependencies between inputs and outputs
5. **Interactive visualization design**: Principles for building user-focused interactive plots

*Note: We are skipping spatial data visualization (Q5) and autocorrelation in time series (Q6b) for this exam.*

## ggplot2 Fundamentals

### Components of a Graph
1. **Data**: The dataset being visualized
2. **Geometry**: The type of plot (points, lines, bars, etc.)
3. **Aesthetic mapping**: How data maps to visual elements (position, color, size, etc.)

### Creating a Plot with ggplot2
```r
# Basic structure
ggplot(data, aes(x = var1, y = var2)) + 
  geom_point()

# Adding layers
ggplot(data, aes(x = var1, y = var2)) + 
  geom_point() +
  geom_line() +
  labs(title = "Title", x = "X-axis", y = "Y-axis")
```

### Aesthetic Mappings
- Position (`x`, `y`)
- Color (`color`, `fill`)
- Shape (`shape`)
- Size (`size`)
- Line type (`linetype`)
- Transparency (`alpha`)

### Common Geometries
- `geom_point()`: Scatterplots
- `geom_line()`: Line plots
- `geom_bar()`: Bar charts
- `geom_histogram()`: Histograms
- `geom_boxplot()`: Box plots
- `geom_text()`: Text labels
- `geom_tile()`: Heatmaps

### Scales and Transformations
```r
# Log transformations
ggplot(data, aes(x, y)) + 
  geom_point() +
  scale_x_log10() +
  scale_y_log10()

# Manual color scales
ggplot(data, aes(x, y, color = group)) + 
  geom_point() +
  scale_color_manual(values = c("red", "blue", "green"))
```

## Multi-Panel Figures

### Small Multiples (Faceting)
```r
# Facet by one variable
ggplot(data, aes(x, y)) + 
  geom_point() +
  facet_wrap(~ variable)

# Facet by two variables
ggplot(data, aes(x, y)) + 
  geom_point() +
  facet_grid(var1 ~ var2)
```

### Best Practices for Multi-Panel Figures
1. Use a consistent scale across panels (default behavior)
2. Order panels logically 
3. Ensure proper alignment between panels
4. Use a consistent visual language (colors, symbols, etc.)
5. Label panels clearly (a, b, c, etc. for compound figures)

## Tidy Data Principles

### What Makes Data Tidy
1. Each variable has its own column
2. Each observation has its own row
3. Each value has its own cell

### dplyr Functions for Data Manipulation
```r
# Filter rows
filter(data, condition)
# Example: filter(mtcars, mpg > 20)

# Select columns
select(data, col1, col2, col3)
# Example: select(mtcars, mpg, cyl, hp)

# Create or transform columns
mutate(data, new_col = expression)
# Example: mutate(mtcars, kpl = mpg * 0.425)

# Group and summarize
group_by(data, grouping_var) %>%
  summarize(
    mean_val = mean(numeric_var),
    count = n()
  )
# Example: 
# mtcars %>% 
#   group_by(cyl) %>% 
#   summarize(avg_mpg = mean(mpg), count = n())

# Count occurrences
count(data, var1, var2)
# Example: count(mtcars, cyl, am)

# Arrange data
arrange(data, col1, desc(col2))
# Example: arrange(mtcars, cyl, desc(mpg))

# Join datasets
left_join(data1, data2, by = "key_column")
# Example: left_join(df1, df2, by = "id")
```

### Common Reshaping Operations
```r
# Pivot longer (wide to long)
pivot_longer(
  data,
  cols = columns_to_pivot,
  names_to = "name_column",
  values_to = "value_column"
)
# Example:
# table4a %>% 
#   pivot_longer(c(`1999`, `2000`), 
#                names_to = "year", 
#                values_to = "cases")

# Pivot wider (long to wide)
pivot_wider(
  data,
  names_from = column_with_names,
  values_from = column_with_values
)
# Example:
# table2 %>%
#   pivot_wider(names_from = type, values_from = count)

# Separate a column into multiple columns
separate(
  data,
  col = column_to_separate,
  into = c("new_col1", "new_col2"),
  sep = "separator"
)
# Example:
# table3 %>% separate(rate, into = c("cases", "population"), sep = "/")

# Unite multiple columns into one
unite(
  data,
  col = "new_column",
  c("col1", "col2"),
  sep = "separator"
)
# Example:
# table5 %>% unite("year", century, year, sep = "")
```

### Data Transformation for Visualization (Exam Practice)
```r
# Q3-type examples: Transforming data for specific visualizations

# Example 1: Creating a ratio column for boxplot (like Attack/Defense ratio)
pokemon %>%
  mutate(ad_ratio = Attack / Defense) %>%
  ggplot(aes(x = ad_ratio, y = reorder(type_1, ad_ratio, median))) +
  geom_boxplot() +
  labs(x = "AD Ratio", y = "Type 1")

# Example 2: Calculating averages by group for line plots
pokemon %>%
  group_by(type_1, Generation) %>%
  summarize(ad_ratio = mean(Attack / Defense), .groups = "drop") %>%
  ggplot(aes(x = Generation, y = ad_ratio)) +
  geom_point() +
  geom_line(aes(group = type_1)) +
  facet_wrap(~ type_1)

# Example 3: Reshaping for histogram facets
pokemon %>%
  select(Name, type_1, Attack, Defense) %>%
  pivot_longer(c("Attack", "Defense"), 
               names_to = "Statistic", 
               values_to = "value") %>%
  ggplot() +
  geom_histogram(aes(value)) +
  facet_wrap(~Statistic)
```

### Rate and Proportion Calculations (Exam Practice)
```r
# Computing rates like cases per 10,000 population
table2 %>%
  pivot_wider(names_from = type, values_from = count) %>%
  mutate(rate = 10000 * cases / population)

# Computing gold medal count by country
olympics %>%
  group_by(Country) %>%
  summarize(total = sum(Gold))

# Comparing total values between time periods
gapminder %>%
  filter(year %in% c(1970, 2010)) %>%
  group_by(year) %>%
  summarize(total_pop = sum(population))
```

## Shiny Reactivity

### Basic Structure
```r
library(shiny)

ui <- fluidPage(
  # Input widgets
  # Output placeholders
)

server <- function(input, output, session) {
  # Reactive expressions
  # Render outputs
}

shinyApp(ui, server)
```

### Input-Output Relationship
- **Inputs**: User-controlled widgets (`input$x`)
- **Outputs**: Display elements (`output$y`)
- **Reactivity**: Automatic updates when inputs change

### Understanding the Reactive Graph
- **Reactive graph**: Shows dependencies between inputs, reactive expressions, and outputs
- **Reactive producers**: Inputs and reactive expressions that provide values
- **Reactive consumers**: Reactive expressions and outputs that use values
- **Reactive dependencies**: Path that reactivity flows through

Visual representation of reactive graphs:
```
# Simple reactive graph
input$x ---------> output$y
                  
# With reactive expression
input$x ---------> reactive_expr() ---------> output$y

# Multiple dependencies
input$x ----\
            \---> reactive_expr() ---------> output$y
input$y ----/

# Complex graph with multiple expressions
input$a ----\
            \---> reactive_expr1() ----\
input$b ----/                          \
                                        \---> output$z
input$c ----\                          /
            \---> reactive_expr2() ----/
input$d ----/
```

### Drawing Reactive Graphs (Exam Practice)
When drawing reactive graphs:
1. Use circles/ovals for inputs
2. Use rectangles for reactive expressions
3. Use squares for outputs
4. Use arrows to show dependencies

Example: For the server function below, draw the reactive graph:
```r
server <- function(input, output, session) {
  x <- reactive(input$a + input$b)
  y <- reactive(x() * input$c)
  output$d <- renderText(y())
  output$e <- renderPlot(x() + y())
}
```

Reactive graph:
```
input$a ----\
            \---> x() ----\
input$b ----/              \
                            \---> output$d
input$c -----------------> y() -/
                            \
                             \---> output$e
                              /
                    x() -----/
```

### Reactive Expressions
```r
# Define reactive expression
data_filtered <- reactive({
  filter(dataset, variable == input$selection)
})

# Use reactive expression
output$plot <- renderPlot({
  ggplot(data_filtered(), aes(x, y)) + geom_point()
})
```

### Benefits of Reactive Expressions
1. **Efficiency**: Computation happens only when needed
2. **Modularity**: Simplifies the reactive graph
3. **Maintainability**: Easier to understand and debug
4. **Reduces duplication**: Compute once, use many times

### Reactive Flow Control
```r
# Only run when button is clicked
data_filtered <- eventReactive(input$go_button, {
  filter(dataset, variable == input$selection)
})

# Run timer-based updates
autoInvalidate <- reactiveTimer(5000)  # 5 seconds
data_latest <- reactive({
  autoInvalidate()
  get_latest_data()
})

# Create an observer (no return value)
observeEvent(input$save_button, {
  saveData(data_filtered())
})
```

### Observers vs. Reactive Expressions
- **Reactive expressions**: Return values that can be used by other reactive consumers
- **Observers**: Perform actions with side effects but don't return values
- Both respond to changes in their reactive dependencies

### Using reactiveVal for State
```r
# Initialize a reactive value
counter <- reactiveVal(0)

# Read the value
counter()

# Update the value
counter(counter() + 1)

# Example: Keeping track of clicked points
selected_points <- reactiveVal(NULL)
observeEvent(input$plot_click, {
  new_point <- nearPoints(data, input$plot_click)
  # Add to existing points
  selected_points(rbind(selected_points(), new_point))
})
```

## Interactive Visualization Design (Q7 Practice)

### Designing for User Interactivity
```r
# Example solution for a question like Q7 - interactive app design
ui <- fluidPage(
  fluidRow(
    column(3,
      # Inputs that affect data/visualization
      numericInput("lambda1", "Parameter 1", 3),
      numericInput("lambda2", "Parameter 2", 5),
      numericInput("n", "Sample size", 1000),
      actionButton("simulate", "Run Simulation")
    ),
    column(9, plotOutput("hist"))
  )
)

server <- function(input, output, session) {
  # Reactive data that depends on inputs
  data <- eventReactive(input$simulate, {
    # Generate data based on inputs
    data.frame(
      x = rpois(input$n, input$lambda1),
      y = rpois(input$n, input$lambda2)
    )
  })
  
  # Output visualization
  output$hist <- renderPlot({
    req(data())
    ggplot(data(), aes(x)) +
      geom_histogram(binwidth = 1)
  })
}
```

### UI Layout Sketching
For exam questions asking you to sketch an interactive visualization:

1. Define user inputs:
   - What parameters need to be adjustable?
   - What type of control (slider, dropdown, buttons) is appropriate?
   - How should they be arranged?

2. Define outputs:
   - What visualizations will show the data?
   - How should multiple views be coordinated?
   - What supplementary information should be displayed?

3. Consider user flow:
   - How will users interact with the application?
   - What feedback will show that interaction occurred?
   - How to handle initial state before user interaction?

Example sketch for a time series exploration app:
```
+-------------------------------------+
| [Time Range]  [Variable▼]  [Update] |
+---------------+---------------------+
|               |                     |
| [Line Chart   |  [Statistics        |
|  showing      |   showing summary   |
|  trends]      |   of selected data] |
|               |                     |
+---------------+---------------------+
| [Histogram showing distribution]    |
+-------------------------------------+
```

## Crosstalk for Interactive HTML Widgets

### Creating Linked Visualizations
```r
library(crosstalk)

# Create shared data object
shared_data <- SharedData$new(data, key = ~id_column)

# Create linked visualizations
plot_widget <- plot_ly(shared_data, x = ~x, y = ~y)
table_widget <- datatable(shared_data)
map_widget <- leaflet(shared_data) %>% addMarkers()

# Display widgets together
bscols(plot_widget, table_widget, map_widget)
```

### Linking Multiple Datasets
```r
# Same group connects different datasets
data1_shared <- SharedData$new(data1, key = ~id, group = "my_group")
data2_shared <- SharedData$new(data2, key = ~id, group = "my_group")
```

## Spatial Data Visualization

*Note: This section (corresponding to Q5) will not be covered on the exam but is included for reference.*

### Working with sf Objects
```r
library(sf)
library(tmap)

# Read spatial data
spatial_data <- read_sf("path/to/shapefile.shp")

# Basic plot
plot(spatial_data)

# Using ggplot2 with sf objects
ggplot(spatial_data) +
  geom_sf(aes(fill = variable)) +
  scale_fill_viridis_c()
```

### tmap for Thematic Maps
```r
# Quick map
qtm(spatial_data)

# More detailed map
tm_shape(spatial_data) +
  tm_polygons("variable", palette = "Blues") +
  tm_layout(title = "Map Title")

# Points on a map
tm_shape(spatial_data) +
  tm_dots(col = "variable", size = 0.1) +
  tm_layout(legend.outside = TRUE)
```

## Time Series Visualization

### Basic Time Series Plots
```r
# Line plot for time series
ggplot(time_data, aes(x = date, y = value)) +
  geom_line()

# Multiple time series
ggplot(time_data, aes(x = date, y = value, color = group)) +
  geom_line()

# Area plot for cumulative values
ggplot(time_data, aes(x = date, y = value, fill = group)) +
  geom_area()
```

*Note: Time series autocorrelation analysis (Q6b) will not be covered on the exam.*

## Data Visualization Best Practices

### General Principles
1. Choose the right chart type for your data
2. Maintain consistent scales
3. Use color deliberately and sparingly 
4. Label axes clearly
5. Consider your audience
6. Tell a coherent story with your visualization
7. Avoid chart junk (unnecessary visual elements)

### Common Chart Types and Use Cases
- **Bar charts**: Comparing categorical data
- **Line charts**: Showing trends over time
- **Scatterplots**: Relationships between two variables
- **Histograms**: Distribution of a continuous variable
- **Box plots**: Distribution and outliers
- **Heatmaps**: Patterns in dense data
- **Maps**: Geospatial data
- **Density plots**: Smooth distribution of values
- **Ridge plots**: Multiple density plots for comparison

### Transformations
- **Log**: For skewed data or multiplicative relationships
- **Square root**: For count data
- **Logistic**: For proportions (0-1 range)

### Checking for Problems
1. Overplotting (too many points overlapping)
2. Inappropriate scales hiding patterns
3. Misleading axes (e.g., not starting at zero)
4. Visual clutter distracting from the message
5. Poor color choices (not colorblind-friendly)
6. Inappropriate chart types for the data
7. Missing context for interpretation
