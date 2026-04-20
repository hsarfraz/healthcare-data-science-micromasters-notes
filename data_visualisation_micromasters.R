# Load libraries

library(ggplot2)
library(ggforce)   # For advanced scatter plots
library(ggridges)  # For river/ridgeline plots
library(ggmap)     # For maps
library(dplyr)     # For data manipulation

# Simulated healthcare dataset
set.seed(123)
df <- data.frame(
  Age = sample(40:80, 100, replace = TRUE),
  Cholesterol = rnorm(100, mean = 200, sd = 30),
  BloodPressure = rnorm(100, mean = 120, sd = 15),
  Outcome = sample(0:1, 100, replace = TRUE)
)

# Convert Outcome to factor for visualisation
df$Outcome <- factor(df$Outcome, labels = c("Healthy", "At Risk"))

ggplot(df, aes(x = Cholesterol, y = BloodPressure, color = Outcome)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  labs(title = "Scatter Plot -Cholesterol vs Blood Pressure",
       x = "Cholesterol Level",
       y = "Blood Pressure",
       color = "Health Outcome") +
  theme_minimal()

#--------------------------------------
# Ridgeline Plot: Cholesterol Distribution by Health Outcome
#--------------------------------------
ggplot(df, aes(x = Cholesterol, y = Outcome, fill = Outcome)) +
  geom_density_ridges(alpha = 0.8) +
  labs(title = "Joyplot: Cholesterol Distribution by Health Outcome",
       x = "Cholesterol Level",
       y = "Health Outcome") +
  theme_minimal()


######################
# R SHINY
######################

library(shiny)
library(ggplot2)
library(plotly)

# ✅ Define data OUTSIDE server
set.seed(123)

df <- data.frame(
  Age = sample(40:80, 100, replace = TRUE),
  Cholesterol = rnorm(100, mean = 200, sd = 30),
  BloodPressure = rnorm(100, mean = 120, sd = 15),
  Outcome = sample(0:1, 100, replace = TRUE)
)

df$Outcome <- factor(df$Outcome, labels = c("Healthy", "At Risk"))

ui <- fluidPage(
  titlePanel("Healthcare Data Dashboard"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("ageRange", "Select Age Range:", min = 40, max = 80, value = c(40, 80))
    ),
    mainPanel(
      plotlyOutput("scatterPlot"),
      plotOutput("barChart")
    )
  )
)

# ✅ Only ONE server function
server <- function(input, output) {
  
  filtered_data <- reactive({
    df[df$Age >= input$ageRange[1] & df$Age <= input$ageRange[2], ]
  })
  
  output$scatterPlot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = Cholesterol, y = BloodPressure, color = Outcome)) +
      geom_point(size = 3, alpha = 0.7) +
      labs(
        title = "Cholesterol vs Blood Pressure",
        x = "Cholesterol Level",
        y = "Blood Pressure",
        color = "Health Outcome"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$barChart <- renderPlot({
    ggplot(filtered_data(), aes(x = Outcome, fill = Outcome)) +
      geom_bar() +
      labs(
        title = "Number of Patients by Health Outcome",
        x = "Health Outcome",
        y = "Count"
      ) +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)