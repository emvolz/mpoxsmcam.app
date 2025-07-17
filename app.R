library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(mpoxsmcam.app)
library(shinyWidgets)

# Define UI
ui <- dashboardPage(
	dashboardHeader(title = "Mpox SMCAM Explorer")
	, dashboardSidebar(
		sidebarMenu(
			menuItem("Forecasting", tabName = "forecasting", icon = icon("chart-line"))
			, menuItem("Parameter Sensitivity", tabName = "sensitivity", icon = icon("sliders-h"))
		)
	)
	, dashboardBody(
		tabItems(
			# Forecasting tab
			tabItem(tabName = "forecasting"
				, fluidRow(
					box(width = 3, title = "Forecast Settings", status = "primary", solidHeader = TRUE
						, selectInput("country", "Country:"
							, choices = c("Netherlands" = "NL", "Spain" = "ES", "Ireland" = "IE")
							, selected = "NL")
						, sliderInput("horizon", "Forecast Horizon (days):"
							, min = 30, max = 3650, value = 365, step = 1)
						, sliderInput("nsim", "Number of Simulations:"
							, min = 50, max = 5000, value = 1000, step = 50)
						, hr()
						, h4("Parameter Modifications")
						, uiOutput("parameter_inputs")
						, br()
						, actionButton("run_forecast", "Run Forecast", class = "btn-primary")
					)
					, box(width = 9, title = "Forecast Results", status = "info", solidHeader = TRUE
						, tabsetPanel(
							tabPanel("Time Series", plotOutput("forecast_plot", height = "400px"))
							, tabPanel("Max Cases Distribution", plotOutput("max_cases_plot", height = "400px"))
							, tabPanel("Cumulative Cases Distribution", plotOutput("cumulative_cases_plot", height = "400px"))
						)
					)
				)
			)
			# Parameter Sensitivity tab
			, tabItem(tabName = "sensitivity"
				, fluidRow(
					box(width = 3, title = "Sensitivity Settings", status = "primary", solidHeader = TRUE
						, sliderInput("dx", "Proportional Change:"
							, min = 0.01, max = 0.5, value = 0.10, step = 0.01)
						, p("This controls the proportional change in parameters for sensitivity analysis.")
					)
					, box(width = 9, title = "Parameter Sensitivity", status = "info", solidHeader = TRUE
						, plotOutput("sensitivity_plot", height = "600px")
					)
				)
			)
		)
	)
)

# Define server logic
server <- function(input, output, session) {
	
	# Get elastic parameters from psumdf (excluding rdrift)
	elastic_params <- reactive({
		params <- mpoxsmcam.app::psumdf[mpoxsmcam.app::psumdf$Elasticity == TRUE, ]
		params[params$parameter != "rdrift", ]
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
				inputId = paste0("param_", param_name)
				, label = label_text
				, value = default_val
				, min = 0
				, step = default_val * 0.01
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
				cntry = input$country
				, newstatelist = list()
				, newparmlist = newparmlist
				, horizon = input$horizon
				, nsim = input$nsim
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
	
	# Generate sensitivity data
	sensitivity_data <- reactive({
		req(input$dx)
		
		# Get dlikelihoodtable results
		dllik_table <- mpoxsmcam.app::dlikelihoodtable(input$dx)
		
		# Filter for elastic parameters only (excluding rdrift)
		elastic_params_df <- mpoxsmcam.app::psumdf[mpoxsmcam.app::psumdf$Elasticity == TRUE, ]
		elastic_params_df <- elastic_params_df[elastic_params_df$parameter != "rdrift", ]
		
		# Filter the table for elastic parameters
		dllik_filtered <- dllik_table[dllik_table$parameter %in% elastic_params_df$parameter, ]
		
		# Create a lookup for metadata from psumdf
		psumdf_lookup <- mpoxsmcam.app::psumdf
		rownames(psumdf_lookup) <- psumdf_lookup$parameter
		
		# Add metadata safely
		dllik_filtered$alias <- psumdf_lookup[dllik_filtered$parameter, "alias"]
		dllik_filtered$estimated <- psumdf_lookup[dllik_filtered$parameter, "Estimated"]
		
		# Convert LaTeX aliases to Unicode/HTML
		dllik_filtered$alias_formatted <- dllik_filtered$alias
		dllik_filtered$alias_formatted <- gsub("\\$", "", dllik_filtered$alias_formatted)
		dllik_filtered$alias_formatted <- gsub("\\\\gamma", "γ", dllik_filtered$alias_formatted)
		dllik_filtered$alias_formatted <- gsub("\\\\alpha", "α", dllik_filtered$alias_formatted)
		dllik_filtered$alias_formatted <- gsub("\\\\omega", "ω", dllik_filtered$alias_formatted)
		dllik_filtered$alias_formatted <- gsub("\\\\mu", "μ", dllik_filtered$alias_formatted)
		dllik_filtered$alias_formatted <- gsub("\\\\sigma", "σ", dllik_filtered$alias_formatted)
		dllik_filtered$alias_formatted <- gsub("\\\\rho", "ρ", dllik_filtered$alias_formatted)
		dllik_filtered$alias_formatted <- gsub("\\\\iota", "ι", dllik_filtered$alias_formatted)
		dllik_filtered$alias_formatted <- gsub("\\\\mathrm\\{([^}]+)\\}", "\\1", dllik_filtered$alias_formatted)
		dllik_filtered$alias_formatted <- gsub("_\\{([^}]+)\\}", "₍\\1₎", dllik_filtered$alias_formatted)
		dllik_filtered$alias_formatted <- gsub("_([a-z0-9])", "₍\\1₎", dllik_filtered$alias_formatted)
		
		# Calculate maximum dloglikelihood for each parameter for ordering
		max_dllik <- aggregate(dloglikelihood ~ parameter, data = dllik_filtered, FUN = max)
		dllik_filtered$max_dllik <- max_dllik$dloglikelihood[match(dllik_filtered$parameter, max_dllik$parameter)]
		
		# Order by maximum dloglikelihood
		dllik_filtered <- dllik_filtered[order(dllik_filtered$max_dllik, decreasing = TRUE), ]
		
		# Create ordered factor for consistent plotting
		param_order <- unique(dllik_filtered$alias_formatted[order(dllik_filtered$max_dllik, decreasing = TRUE)])
		dllik_filtered$alias_formatted <- factor(dllik_filtered$alias_formatted, levels = param_order)
		
		dllik_filtered
	})
	
	# Render sensitivity plot
	output$sensitivity_plot <- renderPlot({
		data <- sensitivity_data()
		req(nrow(data) > 0)
		
		# Create the plot
		p <- ggplot(data, aes(x = alias_formatted, y = dloglikelihood, fill = direction)) +
			geom_col(position = position_dodge(width = 0.8), width = 0.7) +
			coord_flip() +
			scale_fill_manual(values = c("downwards" = "#2166ac", "upwards" = "#762a83"),
				labels = c("Decrease", "Increase")) +
			labs(
				title = paste0("Parameter Sensitivity (", input$dx * 100, "% change)")
				, x = "Parameter"
				, y = "Change in Log-Likelihood"
				, fill = "Direction"
			) +
			theme_minimal() +
			theme(
				axis.text.y = element_text(size = 11)
				, plot.title = element_text(size = 14, face = "bold")
				, legend.position = "bottom"
			)
		
		# Make estimated parameters bold
		estimated_params <- data$alias_formatted[data$estimated]
		if (length(estimated_params) > 0) {
			p <- p + theme(
				axis.text.y = element_text(
					face = ifelse(levels(data$alias_formatted) %in% estimated_params, "bold", "plain")
				)
			)
		}
		
		p
	})
}

# Run the application
shinyApp(ui = ui, server = server)