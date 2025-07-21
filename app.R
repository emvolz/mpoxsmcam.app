library(shiny)
library(shinydashboard)
library(shinyBS)
library(mpoxsmcam.app)

# Get package version dynamically
app_version <- as.character(packageVersion("mpoxsmcam.app"))

# Parameter descriptions from the report
param_descriptions <- list(
	"gammai" = "Infectious period: Average duration of infectiousness (4 days)",
	"alpha" = "Risk heterogeneity shape: Controls variation in individual risk behavior. Lower values = more heterogeneous population",
	"waningnat" = "Natural immunity waning rate: Rate at which immunity from natural infection wanes",
	"waningvac" = "Vaccine immunity waning rate: Rate at which immunity from vaccination wanes", 
	"turnover" = "Population turnover rate: Rate of entry/exit of individuals from the population (1/40 years)",
	"omega" = "Individual risk dynamics rate: Rate at which individuals change their risk behavior over time",
	"N" = "MSM population size: Total size of the men-who-have-sex-with-men population",
	"r" = "Initial transmission rate: Baseline transmission rate parameter",
	"iota0" = "Import rate intercept: Baseline rate of case importation from outside the region",
	"iota1" = "Import rate memory coefficient: How strongly recent importation history affects current imports",
	"iota2" = "Import rate decay: Rate at which importation memory decays over time", 
	"iota3" = "Import rate travel association: Proportion of imports associated with local transmission",
	"sampf" = "Sampling fraction: Proportion of infections that are detected and reported (33%)",
	"ve1" = "Vaccine efficacy 1 dose: Protective efficacy of single vaccine dose (36%)",
	"ve" = "Vaccine efficacy 2 dose: Protective efficacy of two vaccine doses (66%)",
	"initv" = "Proportion vaccinated and immune <2022: Fraction of population with pre-existing immunity from smallpox vaccination"
)

# Create font URLs
font_regular <- system.file('ImperialSansDisplay-Regular.ttf', package = 'mpoxsmcam.app')
font_extralight <- system.file('ImperialSansDisplay-Extralight.ttf', package = 'mpoxsmcam.app')
font_medium <- system.file('ImperialSansDisplay-Medium.ttf', package = 'mpoxsmcam.app')
font_bold <- system.file('ImperialSansDisplay-Bold.ttf', package = 'mpoxsmcam.app')

# Define UI
ui <- dashboardPage(
	dashboardHeader(title = paste0("mpox SMCAM Explorer v", app_version))
	, dashboardSidebar(
		sidebarMenu(
			menuItem("Forecasting", tabName = "forecasting", icon = icon("chart-line"))
			, menuItem("Parameter Sensitivity", tabName = "sensitivity", icon = icon("sliders-h"))
			, menuItem("About", tabName = "about", icon = icon("info-circle"))
		)
	)
	, dashboardBody(
		# Custom CSS for Imperial College Mood 1 styling
		tags$head(
			tags$style(HTML(paste0("
				/* Imperial Sans Display Font Face Declarations */
				@font-face {
					font-family: 'Imperial Sans Display';
					src: url('", font_regular, "') format('truetype');
					font-weight: normal;
					font-style: normal;
				}
				
				@font-face {
					font-family: 'Imperial Sans Display';
					src: url('", font_extralight, "') format('truetype');
					font-weight: 200;
					font-style: normal;
				}
				
				@font-face {
					font-family: 'Imperial Sans Display';
					src: url('", font_medium, "') format('truetype');
					font-weight: 500;
					font-style: normal;
				}
				
				@font-face {
					font-family: 'Imperial Sans Display';
					src: url('", font_bold, "') format('truetype');
					font-weight: bold;
					font-style: normal;
				}
				
				/* Header styling - Dark blue with pink text */
				.main-header .navbar {
					background-color: #003E74 !important;
				}
				
				.main-header .logo {
					background-color: #003E74 !important;
					color: #D881B5 !important;
				}
				
				.main-header .logo:hover {
					background-color: #02137E !important;
				}
				
				/* Sidebar styling - Dark blue with pink text */
				.main-sidebar {
					background-color: #003E74 !important;
				}
				
				.sidebar-menu > li > a {
					color: #D881B5 !important;
				}
				
				.sidebar-menu > li.active > a {
					background-color: #02137E !important;
					color: #D881B5 !important;
				}
				
				.sidebar-menu > li:hover > a {
					background-color: #02137E !important;
					color: #D881B5 !important;
				}
				
				/* Box headers only - preserve box body backgrounds */
				.box.box-primary > .box-header {
					background-color: #003E74 !important;
					color: #D881B5 !important;
				}
				
				.box.box-info > .box-header {
					background-color: #003E74 !important;
					color: #D881B5 !important;
				}
				
				/* Button styling - keep original hover behavior */
				.btn-primary {
					background-color: #ED2D3C !important;
					border-color: #ED2D3C !important;
					color: white !important;
				}
				
				.btn-primary:hover, .btn-primary:focus, .btn-primary:active {
					background-color: #02137E !important;
					border-color: #02137E !important;
					color: white !important;
				}
				
				/* Preserve default content area for plots */
				.content-wrapper, .right-side {
					background-color: #f4f4f4 !important;
				}
				
				/* Tab styling for forecasting/sensitivity tabs */
				.nav-tabs > li.active > a {
					background-color: #003E74 !important;
					color: #D881B5 !important;
				}
				
				.nav-tabs > li > a:hover {
					background-color: #02137E !important;
					color: #D881B5 !important;
				}
				
				/* About tab - restore to default styling */
				#about .box {
					background-color: white !important;
					color: #333 !important;
				}
				
				#about h3, #about h4, #about p {
					color: #333 !important;
				}
				
				#about em {
					color: #666 !important;
				}
				
				/* Imperial Sans Display Font Applications */
				body, .content-wrapper {
					font-family: 'Imperial Sans Display', sans-serif !important;
					font-weight: 200 !important; /* Extralight for body text */
				}
				
				h1, h2, h3, h4, h5, h6, .box-title, .main-header .logo {
					font-family: 'Imperial Sans Display', sans-serif !important;
					font-weight: bold !important; /* Bold for headings */
				}
				
				.btn, .form-control, .sidebar-menu > li > a {
					font-family: 'Imperial Sans Display', sans-serif !important;
					font-weight: 500 !important; /* Medium for UI elements */
				}
				
				p, .form-group label, .checkbox label {
					font-family: 'Imperial Sans Display', sans-serif !important;
					font-weight: normal !important; /* Regular for labels and paragraphs */
				}
			")))
		)
		, tabItems(
			# Forecasting tab
			tabItem(tabName = "forecasting"
				, fluidRow(
					box(width = 3, title = "Settings", status = "primary", solidHeader = TRUE
						, actionButton("run_forecast", "Run simulations", class = "btn-primary", style = "width: 100%; margin-bottom: 15px;")
						, selectInput("country", "Country:"
							, choices = c("Netherlands" = "NL", "Spain" = "ES", "Ireland" = "IE")
							, selected = "NL")
						, sliderInput("horizon", "Forecast horizon (days):"
							, min = 30, max = 3650, value = 3*365, step = 1)
						, sliderInput("nsim", "Number of simulations:"
							, min = 50, max = 1500, value = 200, step = 50)
						, sliderInput("ntrajshow", "Individual trajectories to display:"
							, min = 0, max = 20, value = 0, step = 1)
						, hr()
						, selectInput("scenario", "Scenario:"
							, choices = setNames(1:length(mpoxsmcam.app::scenarios), sapply(mpoxsmcam.app::scenarios, function(x) x$name))
							, selected = 1)
						, p("See About tab for scenario details", style = "font-size: 0.9em; color: #666;")
						, hr()
						, h4("Parameters")
						, p("Hover mouse for tooltip")
						, p("Refresh app to restore defaults")
						, uiOutput("validation_errors")
						, uiOutput("parameter_inputs")
					)
					, box(width = 9, title = "Forecasts", status = "info", solidHeader = TRUE
						, tabsetPanel(
							tabPanel("Time Series"
								 , hr()
								 , h6("Cases over time, fitted and forecast")
								 , hr()
								 , plotOutput("forecast_plot", height = "400px"))
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
						, hr()
						, h4("Parameter inclusion")
						, p("Hover mouse for tooltip")
						, uiOutput("parameter_checkboxes")
					)
					, box(width = 9, title = "Parameter Sensitivity", status = "info", solidHeader = TRUE
						, p("Parameters in bold were fitted to surveillance data. Others parameters are based on literature or manual calibration")
						, hr() 
						, plotOutput("sensitivity_plot", height = "1450px")
					)
				)
			)
			# About tab
			, tabItem(tabName = "about"
				, fluidRow(
					box(width = 12, title = paste0("About the mpox SMCAM Model v", app_version), status = "primary", solidHeader = TRUE
						, h3("Model Objectives")
						, p("The mpox Stochastic Moment Closure Approximation Model (SMCAM) aims to quantify and compare the contribution of different behavioral and immunological factors to the maintenance of mpox transmission in Europe. This analysis focuses on population-level risk dynamics, individual-level risk dynamics, waning immunity, and population turnover as plausible factors which either individually or collectively increase the force of infection and lead to sustained transmission in European MSM populations.")
						
						, h3("Model Approach") 
						, p("We developed a stochastic SEIR model incorporating heterogeneous transmission risk and moment closure approximations. The key feature is that infectiousness is correlated with susceptibility via extreme heterogeneity in risk behavior (e.g. number of sexual partners). The model uses a data-driven approach for estimating risk heterogeneity and focuses on longitudinal changes in risk behavior both at the individual and population level, as well as changes in population immunity.")
						
						, h3("Applications")
						, p("This tool allows users to explore forecasting scenarios and parameter sensitivity for mpox transmission in the Netherlands, Spain, and Ireland. Users can modify epidemiological parameters to understand their impact on transmission dynamics and generate probabilistic forecasts under different assumptions about risk behavior, immunity waning, and importation patterns.")
						
						, h3("Scenarios")
						, p("The following predefined scenarios are available for forecasting:")
						, tags$ul(
							tags$li(strong("Default:"), " Uses fitted parameter values without modification"),
							tags$li(strong("Higher transmissibility:"), " Increases transmission rate (r) by 10% to explore scenarios with enhanced viral transmission"),
							tags$li(strong("More importation:"), " Increases importation rate (ι₀) by 3-fold to model higher case importation from other regions"),
							tags$li(strong("Increase in subclinical infections:"), " Models a scenario with more subclinical disease by increasing the infectious period (γᵢ) by 20%, decreasing the detection rate (ρ) by 20%, and decreasing the transmission rate (r) by 15%")
						)
						
						, hr()
						, h4("Contact Information")
						, p(strong("Erik Volz"))
						, p("MRC Centre for Global Infectious Disease Analysis")
						, p("Imperial College London")
						, p(a("e.volz@imperial.ac.uk", href = "mailto:e.volz@imperial.ac.uk"))
						
						, hr()
						, p(em(paste0("This application implements the model described in: \"Model-based investigation of factors sustaining mpox transmission in Europe in 2025\" by Erik Volz, Imperial College London.")))
						, p(em(paste0("App version: ", app_version)))
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
	
	# Store original default values for reset functionality (country-specific)
	original_defaults <- reactive({
		mpoxsmcam.app::fits[[mpoxsmcam.app::BEST]][[input$country]] |> coef()
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
		
		# Apply scenario modifications to defaults for initial values
		if (!is.null(input$scenario)) {
			scenario_idx <- as.numeric(input$scenario)
			if (!is.null(scenario_idx) && scenario_idx > 0 && scenario_idx <= length(mpoxsmcam.app::scenarios)) {
				scenario <- mpoxsmcam.app::scenarios[[scenario_idx]]
				if (length(scenario$parameter) > 0) {
					for (i in 1:length(scenario$parameter)) {
						param_name <- scenario$parameter[i]
						factor_val <- scenario$factor[i]
						if (param_name %in% names(defaults)) {
							defaults[param_name] <- defaults[param_name] * factor_val
						}
					}
				}
			}
		}
		
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
			
			div(
				numericInput(
					inputId = paste0("param_", param_name)
					, label = label_text
					, value = signif(default_val, 3)
					, min = 0
					, step = signif(default_val * 0.01, 3)
				)
				, if (!is.null(param_descriptions[[param_name]])) {
					bsTooltip(
						id = paste0("param_", param_name)
						, title = param_descriptions[[param_name]]
						, placement = "right"
						, trigger = "hover"
					)
				}
			)
		})
		
		do.call(tagList, param_inputs)
	})
	
	# Generate parameter inclusion checkboxes for sensitivity analysis
	output$parameter_checkboxes <- renderUI({
		params_df <- elastic_params()
		
		# Create checkboxes for each elastic parameter
		checkbox_inputs <- lapply(1:nrow(params_df), function(i) {
			param_name <- params_df$parameter[i]
			param_alias <- params_df$alias[i]
			
			# Convert LaTeX to Unicode/HTML for display
			label_text <- param_alias
			label_text <- gsub("\\$", "", label_text)
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
			
			div(
				checkboxInput(
					inputId = paste0("include_", param_name)
					, label = label_text
					, value = TRUE  # Default all checked
				)
				, if (!is.null(param_descriptions[[param_name]])) {
					bsTooltip(
						id = paste0("include_", param_name)
						, title = param_descriptions[[param_name]]
						, placement = "right"
						, trigger = "hover"
					)
				}
			)
		})
		
		do.call(tagList, checkbox_inputs)
	})
	
	# Reactive values for storing results
	forecast_results <- reactiveVal(NULL)
	
	# Parameter validation reactive
	parameter_validation <- reactive({
		params_df <- elastic_params()
		defaults <- original_defaults()
		errors <- c()
		
		for (i in 1:nrow(params_df)) {
			param_name <- params_df$parameter[i]
			input_name <- paste0("param_", param_name)
			if (!is.null(input[[input_name]])) {
				current_val <- input[[input_name]]
				
				# Check if value is valid (numeric and positive)
				if (is.na(current_val) || !is.numeric(current_val) || current_val <= 0) {
					errors <- c(errors, paste0("Parameter '", param_name, "' must be a positive number"))
				} else if (!is.null(defaults[param_name])) {
					default_val <- defaults[param_name]
					
					# Check range validation if we have valid numeric values
					if (is.numeric(default_val) && !is.na(default_val)) {
						if (current_val < default_val / 4 || current_val > default_val * 4) {
							errors <- c(errors, paste0("Parameter '", param_name, "' must be between ", 
								signif(default_val / 4, 4), " and ", signif(default_val * 4, 4)))
						}
					}
				}
			}
		}
		
		return(errors)
	})
	
	# Display validation errors
	output$validation_errors <- renderUI({
		errors <- parameter_validation()
		if (length(errors) > 0) {
			div(
				style = "margin-top: 10px; padding: 10px; background-color: #f8d7da; border: 1px solid #f5c6cb; border-radius: 5px; color: #721c24;",
				h5("Parameter Validation Errors:", style = "margin-top: 0;"),
				lapply(errors, function(error) p(error, style = "margin-bottom: 5px;"))
			)
		}
	})
	
	# Function to run forecast simulation
	run_forecast_simulation <- function() {
		req(input$country)
		
		# Check for validation errors first
		errors <- parameter_validation()
		if (length(errors) > 0) {
			# Don't run forecast if there are validation errors (they show in UI)
			return()
		}
		
		# Collect parameters ONLY from UI text fields (validation already done)
		params_df <- elastic_params()
		newparmlist <- list()
		
		for (i in 1:nrow(params_df)) {
			param_name <- params_df$parameter[i]
			input_name <- paste0("param_", param_name)
			if (!is.null(input[[input_name]])) {
				newparmlist[[param_name]] <- input[[input_name]]
			}
		}
		
		# Only proceed if we have parameter values
		if (length(newparmlist) == 0) {
			return()
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
				, ntrajshow = input$ntrajshow
			)
			
			incProgress(0.7, detail = "Generating plots")
			forecast_results(results)
		})
	}
	
	# Auto-run forecast on app startup
	observe({
		# Wait for all required inputs to be available
		req(input$country, input$horizon, input$nsim, input$ntrajshow, input$scenario)
		
		# Only run once when app starts (when forecast_results is NULL)
		if (is.null(forecast_results())) {
			run_forecast_simulation()
		}
	})
	
	
	# Run forecast when button is clicked
	observeEvent(input$run_forecast, {
		run_forecast_simulation()
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
		
		# Get selected parameters from checkboxes
		selected_params <- c()
		for (param in elastic_params_df$parameter) {
			checkbox_id <- paste0("include_", param)
			if (!is.null(input[[checkbox_id]]) && input[[checkbox_id]]) {
				selected_params <- c(selected_params, param)
			}
		}
		
		# If no parameters selected, return empty data frame
		if (length(selected_params) == 0) {
			return(data.frame())
		}
		
		# Filter the table for selected elastic parameters
		dllik_filtered <- dllik_table[dllik_table$parameter %in% selected_params, ]
		
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
		
		# Order by maximum dloglikelihood (ascending so largest effects appear at top after coord_flip)
		dllik_filtered <- dllik_filtered[order(dllik_filtered$max_dllik, decreasing = FALSE), ]
		
		# Create ordered factor for consistent plotting
		param_order <- unique(dllik_filtered$alias_formatted[order(dllik_filtered$max_dllik, decreasing = FALSE)])
		dllik_filtered$alias_formatted <- factor(dllik_filtered$alias_formatted, levels = param_order)
		
		dllik_filtered
	})
	
	# Render sensitivity plot
	output$sensitivity_plot <- renderPlot({
		data <- sensitivity_data()
		
		# Handle case where no parameters are selected
		if (nrow(data) == 0) {
			# Create an empty plot with message
			ggplot() + 
				annotate("text", x = 0.5, y = 0.5, label = "No parameters selected.\nPlease check at least one parameter.", 
						size = 6, hjust = 0.5, vjust = 0.5) +
				theme_void() +
				xlim(0, 1) + ylim(0, 1)
		} else {
		
		# Create the plot
		p <- ggplot(data, aes(x = alias_formatted, y = dloglikelihood, fill = direction)) +
			geom_col(position = position_dodge(width = 0.8), width = 0.7) +
			coord_flip() +
			scale_fill_manual(values = c("downwards" = "#2166ac", "upwards" = "#762a83"),
				labels = c("Decrease", "Increase")) +
			labs(
				title = paste0("Likelihood elasticity (", input$dx * 100, "% change)")
				, x = "Parameter"
				, y = "Abs. value change in log-likelihood"
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
		}
	})
}

# Run the application
shinyApp(ui = ui, server = server)
