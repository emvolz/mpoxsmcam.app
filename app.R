library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(mpoxsmcam.app)
library(shinyWidgets)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Mpox SMCAM Explorer"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Forecasting", tabName = "forecasting", icon = icon("chart-line")),
      menuItem("Parameter Sensitivity", tabName = "sensitivity", icon = icon("sliders-h"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Forecasting tab
      tabItem(tabName = "forecasting",
        fluidRow(
          box(width = 3, title = "Forecast Settings", status = "primary", solidHeader = TRUE,
            selectInput("country", "Country:", 
                       choices = c("Netherlands" = "NL", "Spain" = "ES", "Ireland" = "IE"),
                       selected = "NL"),
            
            sliderInput("horizon", "Forecast Horizon (days):", 
                       min = 30, max = 3650, value = 365, step = 1),
            
            sliderInput("nsim", "Number of Simulations:", 
                       min = 50, max = 5000, value = 1000, step = 50),
            
            hr(),
            h4("Parameter Modifications"),
            uiOutput("parameter_inputs"),
            
            br(),
            actionButton("run_forecast", "Run Forecast", class = "btn-primary")
          ),
          
          box(width = 9, title = "Forecast Results", status = "info", solidHeader = TRUE,
            tabsetPanel(
              tabPanel("Time Series", plotOutput("forecast_plot", height = "400px")),
              tabPanel("Max Cases Distribution", plotOutput("max_cases_plot", height = "400px")),
              tabPanel("Cumulative Cases Distribution", plotOutput("cumulative_cases_plot", height = "400px"))
            )
          )
        )
      ),
      
      # Parameter Sensitivity tab (placeholder)
      tabItem(tabName = "sensitivity",
        h2("Parameter Sensitivity Analysis"),
        p("This tab will contain parameter sensitivity analysis tools.")
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Get elastic parameters from psumdf
  elastic_params <- reactive({
    mpoxsmcam.app::psumdf[mpoxsmcam.app::psumdf$Elasticity == TRUE, ]
  })
  
  # Get default parameter values for selected country
  default_params <- reactive({
    country_fits <- mpoxsmcam.app::fits[[mpoxsmcam.app::BEST]][[input$country]]
    coef(country_fits)
  })
  
  # Generate parameter input controls
  output$parameter_inputs <- renderUI({
    params_df <- elastic_params()
    defaults <- default_params()
    
    # Create input controls for each elastic parameter
    param_inputs <- lapply(1:nrow(params_df), function(i) {
      param_name <- params_df$parameter[i]
      param_alias <- params_df$alias[i]
      is_estimated <- params_df$Estimated[i]
      default_val <- defaults[param_name]
      
      # Create label with mathematical expression
      # Convert LaTeX to Unicode/HTML where possible
      label_text <- param_alias
      label_text <- gsub("\\$", "", label_text)  # Remove $ delimiters
      label_text <- gsub("\\\\gamma", "γ", label_text)
      label_text <- gsub("\\\\alpha", "α", label_text)
      label_text <- gsub("\\\\omega", "ω", label_text)
      label_text <- gsub("\\\\mu", "μ", label_text)
      label_text <- gsub("\\\\sigma", "σ", label_text)
      label_text <- gsub("\\\\rho", "ρ", label_text)
      label_text <- gsub("\\\\iota", "ι", label_text)
      label_text <- gsub("\\\\mathrm\\{([^}]+)\\}", "\\1", label_text)
      label_text <- gsub("_\\{([^}]+)\\}", "₍\\1₎", label_text)
      label_text <- gsub("_([a-z0-9])", "₍\\1₎", label_text)
      
      if (is_estimated) {
        label_text <- HTML(paste0("<b>", label_text, "</b>"))
      }
      
      numericInput(
        inputId = paste0("param_", param_name),
        label = label_text,
        value = default_val,
        min = 0,
        step = default_val * 0.01
      )
    })
    
    do.call(tagList, param_inputs)
  })
  
  # Reactive values for storing results
  forecast_results <- reactiveVal(NULL)
  
  # Run forecast when button is clicked
  observeEvent(input$run_forecast, {
    req(input$country)
    
    # Collect modified parameters
    params_df <- elastic_params()
    newparmlist <- list()
    
    for (i in 1:nrow(params_df)) {
      param_name <- params_df$parameter[i]
      input_name <- paste0("param_", param_name)
      if (!is.null(input[[input_name]])) {
        newparmlist[[param_name]] <- input[[input_name]]
      }
    }
    
    # Show progress
    withProgress(message = 'Running forecast...', value = 0, {
      incProgress(0.3, detail = "Setting up simulation")
      
      # Run fopl function
      results <- mpoxsmcam.app::fopl(
        cntry = input$country,
        newstatelist = list(),
        newparmlist = newparmlist,
        horizon = input$horizon,
        nsim = input$nsim
      )
      
      incProgress(0.7, detail = "Generating plots")
      forecast_results(results)
    })
  })
  
  # Render forecast plot
  output$forecast_plot <- renderPlot({
    results <- forecast_results()
    if (!is.null(results)) {
      results$plot
    }
  })
  
  # Render max cases plot
  output$max_cases_plot <- renderPlot({
    results <- forecast_results()
    if (!is.null(results)) {
      results$plotmax
    }
  })
  
  # Render cumulative cases plot
  output$cumulative_cases_plot <- renderPlot({
    results <- forecast_results()
    if (!is.null(results)) {
      results$plotsum
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)