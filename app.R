# Print current working directory at the start of the app for debugging path issues
print(paste("Current Working Directory when app starts:", getwd()))

#JEFF RONYL R. PAUSAL
#BSCS2
#CS 226 - DATA ANALYTICS - STAT USING R

library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(DT)
library(lubridate)
library(forecast)
library(zoo) # as.yearqtr
library(stringr) # str_replace_all and str_trim
library(shinycssloaders) # loading indicators
library(sf) # handling shapefiles
library(leaflet) # interactive maps


# Helper function to truncate strings
truncate_text <- function(text, len) {
  if (is.null(text) || is.na(text)) return("")
  if (nchar(text) > len) {
    return(paste0(substr(text, 1, len - 3), "..."))
  } else {
    return(text)
  }
}


# --- Data Loading and Preprocessing Function ---
load_and_preprocess_data <- function() {
  df_crops_raw <- read.csv("CROPS_R.csv", check.names = FALSE, stringsAsFactors = FALSE)
  df_crops <- df_crops_raw %>%
    rename(Item = `Ecosystem/Croptype`) %>%
    mutate(Geolocation = str_trim(str_replace_all(Geolocation, "^\\.\\.", "")),
           Item = str_trim(Item))
  cols_to_pivot_crops <- setdiff(names(df_crops), c("Item", "Geolocation"))
  df_crops_long <- df_crops %>%
    mutate(across(all_of(cols_to_pivot_crops), as.character)) %>%
    pivot_longer(cols = all_of(cols_to_pivot_crops),
                 names_to = "PeriodFull",
                 values_to = "Value_chr") %>%
    mutate(Value = suppressWarnings(as.numeric(Value_chr))) %>%
    filter(!is.na(Value), Value_chr != "",
           !grepl("Semester", PeriodFull, ignore.case = TRUE)) %>%
    mutate(
      Year = as.integer(str_extract(PeriodFull, "^\\d{4}")),
      PeriodName = str_trim(str_replace(PeriodFull, "^\\d{4}\\s*", ""))
    ) %>%
    separate(PeriodName, into = c("Period_Type_Raw", "Period_Num_Raw"), sep = " ", fill = "right", extra = "merge") %>%
    mutate(
      Period_Type = case_when(
        grepl("Annual", Period_Type_Raw, ignore.case = TRUE) ~ "Annual",
        grepl("Quarter", Period_Type_Raw, ignore.case = TRUE) ~ "Quarter",
        TRUE ~ NA_character_
      ),
      Period_Num = case_when(
        Period_Type == "Quarter" ~ as.integer(Period_Num_Raw),
        TRUE ~ NA_integer_
      ),
      Category = "Crops"
    ) %>%
    filter(!is.na(Period_Type)) %>%
    select(Category, Item, Geolocation, Year, Period_Type, Period_Num, Value)
  
  df_livestock_raw <- read.csv("LIVESTOCKPOULTRYFISHERIES_R.csv", check.names = FALSE, stringsAsFactors = FALSE)
  df_livestock <- df_livestock_raw %>%
    rename(Item = Species) %>%
    mutate(
      Geolocation = str_trim(str_replace_all(Geolocation, "^\\.\\.", "")),
      Item = str_trim(str_replace_all(Item, "^\\.\\.", "")),
      Geolocation = case_when(
        Geolocation == "CAR" ~ "CORDILLERA ADMINISTRATIVE REGION (CAR)",
        Geolocation == "REGION X -(NORTHERN MINDANAO)" ~ "REGION X (NORTHERN MINDANAO)", # Standardized
        Geolocation == "REGION X (NOTHERN MINDANAO)" ~ "REGION X (NORTHERN MINDANAO)", # Standardized
        TRUE ~ Geolocation
      )
    )
  cols_to_pivot_livestock <- setdiff(names(df_livestock), c("Item", "Geolocation"))
  df_livestock_long <- df_livestock %>%
    mutate(across(all_of(cols_to_pivot_livestock), as.character)) %>%
    pivot_longer(cols = all_of(cols_to_pivot_livestock),
                 names_to = "PeriodFull",
                 values_to = "Value_chr") %>%
    mutate(Value = suppressWarnings(as.numeric(Value_chr))) %>%
    filter(!is.na(Value), Value_chr != "") %>%
    mutate(
      Year = as.integer(str_extract(PeriodFull, "^\\d{4}")),
      PeriodName = str_trim(str_replace(PeriodFull, "^\\d{4}\\s*", ""))
    ) %>%
    separate(PeriodName, into = c("Period_Type_Raw", "Period_Num_Raw"), sep = " ", fill = "right", extra = "merge") %>%
    mutate(
      Period_Type = case_when(
        grepl("Annual", Period_Type_Raw, ignore.case = TRUE) ~ "Annual",
        grepl("Quarter", Period_Type_Raw, ignore.case = TRUE) ~ "Quarter",
        TRUE ~ NA_character_
      ),
      Period_Num = case_when(
        Period_Type == "Quarter" ~ as.integer(Period_Num_Raw),
        TRUE ~ NA_integer_
      ),
      Category = "Livestock & Fisheries"
    ) %>%
    filter(!is.na(Period_Type)) %>%
    select(Category, Item, Geolocation, Year, Period_Type, Period_Num, Value)
  
  combined_data <- bind_rows(df_crops_long, df_livestock_long) %>%
    filter(!is.na(Item), Item != "", !is.na(Geolocation), Geolocation != "") %>%
    mutate(
      Geolocation = toupper(Geolocation),
      Item = toupper(Item),
      Date = case_when(
        Period_Type == "Quarter" & Period_Num == 1 ~ make_date(Year, 1, 1),
        Period_Type == "Quarter" & Period_Num == 2 ~ make_date(Year, 4, 1),
        Period_Type == "Quarter" & Period_Num == 3 ~ make_date(Year, 7, 1),
        Period_Type == "Quarter" & Period_Num == 4 ~ make_date(Year, 10, 1),
        Period_Type == "Annual" ~ make_date(Year, 1, 1),
        TRUE ~ NA_Date_
      )
    ) %>%
    filter(!is.na(Date)) %>%
    arrange(Date)
  return(combined_data)
}

full_data <- load_and_preprocess_data()
period_levels <- c("Quarter", "Annual")
full_data$Period_Type <- factor(full_data$Period_Type, levels = period_levels, ordered = TRUE)

# --- Load Shapefile ---
shapefile_path <- "shapefiles/Regions.shp"

ph_regions_sf <- tryCatch({
  sf::st_read(shapefile_path, quiet = TRUE) %>%
    sf::st_transform(crs = 4326)
}, error = function(e) {
  message(paste("Error loading shapefile from path:", shapefile_path))
  message("Please ensure the file and its companions (.dbf, .prj, .shx, etc.) are in that location relative to the app's working directory.")
  message(paste("Current R working directory reported as:", getwd()))
  message(paste("Detailed error from st_read:", e$message))
  return(NULL)
})


REGION_NAME_COLUMN_IN_SHAPEFILE <- "REGION" 

if (!is.null(ph_regions_sf)) {
  if (!(REGION_NAME_COLUMN_IN_SHAPEFILE %in% names(ph_regions_sf)) && REGION_NAME_COLUMN_IN_SHAPEFILE != "YOUR_ACTUAL_REGION_NAME_COLUMN") {
    warning(paste("The specified region name column '", REGION_NAME_COLUMN_IN_SHAPEFILE, "' was not found in the shapefile.",
                  "Please verify the column name. Available columns:", paste(names(ph_regions_sf), collapse=", ")))
  } else if (REGION_NAME_COLUMN_IN_SHAPEFILE == "YOUR_ACTUAL_REGION_NAME_COLUMN") {
    warning(paste("Placeholder 'YOUR_ACTUAL_REGION_NAME_COLUMN' is still being used.",
                  "Please replace it with the actual column name from your Regions.shp file.",
                  "Available columns:", paste(names(ph_regions_sf), collapse=", ")))
  }
  
  ph_regions_sf <- ph_regions_sf %>%
    mutate(
      shapefile_region_name_raw = toupper(!!sym(REGION_NAME_COLUMN_IN_SHAPEFILE)),
      shapefile_region_name_join = toupper(!!sym(REGION_NAME_COLUMN_IN_SHAPEFILE)),
      shapefile_region_name_join = case_when(
        shapefile_region_name_join == "CORDILLERA ADMINISTRATIVE REGION (CAR)" ~ "CORDILLERA ADMINISTRATIVE REGION (CAR)",
        shapefile_region_name_join == "AUTONOMOUS REGION OF MUSLIM MINDANAO (ARMM)" ~ "BANGSAMORO AUTONOMOUS REGION IN MUSLIM MINDANAO (BARMM)",
        shapefile_region_name_join == "BICOL REGION (REGION V)" ~ "REGION V (BICOL REGION)",
        shapefile_region_name_join == "CAGAYAN VALLEY (REGION II)" ~ "REGION II (CAGAYAN VALLEY)",
        shapefile_region_name_join == "CALABARZON (REGION IV-A)" ~ "REGION IV-A (CALABARZON)",
        shapefile_region_name_join == "CARAGA (REGION XIII)" ~ "REGION XIII (CARAGA)",
        shapefile_region_name_join == "CENTRAL LUZON (REGION III)" ~ "REGION III (CENTRAL LUZON)",
        shapefile_region_name_join == "CENTRAL VISAYAS (REGION VII)" ~ "REGION VII (CENTRAL VISAYAS)",
        shapefile_region_name_join == "DAVAO REGION (REGION XI)" ~ "REGION XI (DAVAO REGION)",
        shapefile_region_name_join == "EASTERN VISAYAS (REGION VIII)" ~ "REGION VIII (EASTERN VISAYAS)",
        shapefile_region_name_join == "ILOCOS REGION (REGION I)" ~ "REGION I (ILOCOS REGION)",
        shapefile_region_name_join == "METROPOLITAN MANILA" ~ "NATIONAL CAPITAL REGION (NCR)",
        shapefile_region_name_join == "MIMAROPA (REGION IV-B)" ~ "MIMAROPA REGION",
        shapefile_region_name_join == "NORTHERN MINDANAO (REGION X)" ~ "REGION X (NORTHERN MINDANAO)",
        shapefile_region_name_join == "SOCCSKSARGEN (REGION XII)" ~ "REGION XII (SOCCSKSARGEN)",
        shapefile_region_name_join == "WESTERN VISAYAS (REGION VI)" ~ "REGION VI (WESTERN VISAYAS)",
        shapefile_region_name_join == "ZAMBOANGA PENINSULA (REGION IX)" ~ "REGION IX (ZAMBOANGA PENINSULA)",
        TRUE ~ shapefile_region_name_join 
      )
    )
  
  print(paste("--- Shapefile Region Names (Raw from shapefile, Uppercased, using column:", REGION_NAME_COLUMN_IN_SHAPEFILE, ") ---"))
  if (REGION_NAME_COLUMN_IN_SHAPEFILE %in% names(ph_regions_sf)) {
    print(sort(unique(toupper(ph_regions_sf[[REGION_NAME_COLUMN_IN_SHAPEFILE]]))))
  } else if (REGION_NAME_COLUMN_IN_SHAPEFILE != "YOUR_ACTUAL_REGION_NAME_COLUMN") {
    print(paste("Warning: Specified column '", REGION_NAME_COLUMN_IN_SHAPEFILE, "' not found for debug. Avail:", paste(names(ph_regions_sf), collapse=", ")))
  } else {
    print(paste("Warning: Placeholder used for REGION_NAME_COLUMN_IN_SHAPEFILE. Avail:", paste(names(ph_regions_sf), collapse=", ")))
  }
  print("--- Shapefile Region Names (Standardized for Joining - shapefile_region_name_join) ---")
  if ("shapefile_region_name_join" %in% names(ph_regions_sf)) {
    print(sort(unique(ph_regions_sf$shapefile_region_name_join)))
  } else {
    print("Warning: 'shapefile_region_name_join' column not created.")
  }
  print("--- Production Data Geolocation Names (Uppercase, for Joining) ---")
  data_geo_names_debug <- sort(unique(full_data$Geolocation[full_data$Geolocation != "PHILIPPINES"]))
  print(data_geo_names_debug)
  if ("shapefile_region_name_join" %in% names(ph_regions_sf)) {
    shapefile_names_debug_join_key <- sort(unique(ph_regions_sf$shapefile_region_name_join))
    print("--- Names in Shapefile (Join Key) NOT in Production Data (Geolocation) ---")
    print(setdiff(shapefile_names_debug_join_key, data_geo_names_debug))
    print("--- Names in Production Data (Geolocation) NOT in Shapefile (Join Key) ---")
    print(setdiff(data_geo_names_debug, shapefile_names_debug_join_key))
  } else {
    print("Skipping setdiff checks for join key.")
  }
}

ui <- dashboardPage(
  dashboardHeader(title = "PH Agri Production Dashboard", titleWidth = 350),
  dashboardSidebar(
    width = 350,
    selectInput("category_filter", "Select Category:",
                choices = c("All", unique(full_data$Category)), selected = "All"),
    selectInput("item_filter", "Select Item (Overview/Explorer):", choices = NULL),
    selectInput("geo_filter", "Select Geolocation (Overview/Explorer):", choices = NULL),
    selectInput("period_type_filter_main", "Select Period Type (Overview/Explorer):",
                choices = c("Quarter", "Annual"), selected = "Annual"),
    sliderInput("year_range_filter", "Select Year Range (Overview/Explorer):",
                min = min(full_data$Year, na.rm = TRUE),
                max = max(full_data$Year, na.rm = TRUE),
                value = c(min(full_data$Year, na.rm = TRUE), max(full_data$Year, na.rm = TRUE)),
                step = 1, sep = "")
  ),
  dashboardBody(
    tags$head(tags$style(HTML(".main-sidebar { font-size: 14px; }"))),
    tabsetPanel(
      id = "main_tabs",
      tabPanel("Overview & Trends",
               fluidRow(
                 valueBoxOutput("total_production_box", width = 4),
                 valueBoxOutput("avg_production_box", width = 4),
                 valueBoxOutput("num_items_box", width = 4)
               ),
               fluidRow(
                 box(title = "Production Trend Over Time", status = "primary", solidHeader = TRUE, width = 12,
                     plotlyOutput("trend_plot") %>% withSpinner(color="#0dc5c1"))
               ),
               fluidRow(
                 box(title = "Production Comparison", status = "info", solidHeader = TRUE, width = 12,
                     plotlyOutput("comparison_plot") %>% withSpinner(color="#0dc5c1"))
               )
      ),
      tabPanel("Regional Breakdown",
               fluidRow(
                 box(title = "Regional Production (Selected Year)", status = "success", solidHeader = TRUE, width = 12,
                     selectInput("regional_item_filter", "Select Item:", choices = NULL),
                     selectInput("regional_year_filter", "Select Year:", choices = sort(unique(full_data$Year)), selected = max(full_data$Year)),
                     selectInput("regional_period_type_filter", "Select Period Type:", choices = c("Quarter", "Annual"), selected = "Annual"),
                     plotlyOutput("regional_plot") %>% withSpinner(color="#0dc5c1"))
               )
      ),
      tabPanel("Spatio-Temporal Analysis",
               fluidRow(
                 column(width = 3,
                        box(title = "Spatio-Temporal Controls", status = "info", solidHeader = TRUE, width = NULL,
                            selectInput("st_item_filter", "Select Item for Map/Chart:", choices = NULL),
                            selectInput("st_period_type_filter", "Select Period Type:",
                                        choices = c("Quarter", "Annual"), selected = "Annual"),
                            uiOutput("st_time_slider_ui"),
                            textOutput("selected_time_output_st")
                        )
                 ),
                 column(width = 9,
                        box(title = "Spatio-Temporal Production Map", status = "primary", solidHeader = TRUE, width = 12,
                            if (is.null(ph_regions_sf) || (exists("REGION_NAME_COLUMN_IN_SHAPEFILE") && REGION_NAME_COLUMN_IN_SHAPEFILE == "YOUR_ACTUAL_REGION_NAME_COLUMN")) {
                              HTML("<p style='color:red; font-weight:bold;'>Shapefile for map visualization could not be loaded or configured correctly. Please check file paths and ensure the correct region name column from your shapefile is specified in the R script (replace 'YOUR_ACTUAL_REGION_NAME_COLUMN').</p>")
                            } else {
                              leafletOutput("spatio_temporal_map", height = "500px") %>% withSpinner(color="#0dc5c1")
                            }
                        ),
                        box(title = "Regional Production Bar Chart (Selected Time)", status = "success", solidHeader = TRUE, width = 12,
                            plotlyOutput("spatio_temporal_barchart") %>% withSpinner(color="#0dc5c1"))
                 )
               )
      ),
      tabPanel("Data Explorer", ############# UI FOR DATA EXPLORER TAB ############
               fluidRow(
                 box(title = "Filtered Data Table", status = "warning", solidHeader = TRUE, width = 12,
                     DTOutput("data_table") %>% withSpinner(color="#0dc5c1"))
               )
      ),
      tabPanel("Time Series Analysis & Forecasting",
               sidebarLayout(
                 sidebarPanel(
                   width = 3,
                   h4("Forecasting Controls"),
                   helpText("Select a specific Item, Geolocation, and Period Type for analysis. Then, hit the Run Analysis & Forecast button."),
                   selectInput("ts_item_filter", "Select Item for TS:", choices = NULL),
                   selectInput("ts_geo_filter", "Select Geolocation for TS:", choices = NULL),
                   selectInput("ts_period_type_filter", "Select Period Type for TS:",
                               choices = c("Quarter", "Annual"), selected = "Annual"),
                   numericInput("forecast_horizon", "Forecast Horizon (Periods):", value = 8, min = 1, max = 20),
                   actionButton("run_ts_analysis", "Run Analysis & Forecast", icon = icon("cogs")),
                   hr(),
                   h5("Model Information:"),
                   verbatimTextOutput("ts_model_info")
                 ),
                 mainPanel(
                   width = 9,
                   tabsetPanel(
                     tabPanel("Decomposition",
                              plotlyOutput("ts_decomposition_plot", height = "600px") %>% withSpinner(color="#0dc5c1")
                     ),
                     tabPanel("Forecast Plot",
                              plotlyOutput("ts_forecast_plot", height = "500px") %>% withSpinner(color="#0dc5c1")
                     ),
                     tabPanel("Forecast Data Table",
                              DTOutput("ts_forecast_table") %>% withSpinner(color="#0dc5c1")
                     )
                   )
                 )
               )
      ),
      tabPanel("Information",
               icon = icon("info-circle"),
               div(class = "info-tab-content",
                   h3("About This Dashboard"),
                   p("This dashboard provides an interactive platform to explore and analyze the volume of production for staple crops, livestock, and fisheries in the Philippines. It aims to generate relevant insights using statistical methods and visualizations as well as predictions through Time-Series Analysis; all which are important in helping smart agriculture in the future by analyzing past data."),
                   h3("Data Sources"),
                   # MODIFIED LINE BELOW
                   p("The data came from Philippine Statistics Authority's OpenStat. Here is the link: ", 
                     tags$a(href="https://openstat.psa.gov.ph/Database/Agriculture-Forestry-Fisheries", 
                            "https://openstat.psa.gov.ph/Database/Agriculture-Forestry-Fisheries", 
                            target="_blank")), # target="_blank" opens in new tab
                   p("Furthermore, the application utilizes two main datasets that were both manually engineered in Excel and cleaned in R:"),
                   tags$ul(
                     tags$li(tags$strong("CROPS_R.csv:"), "Contains production data for various crops (Palay, Corn, Cassava, Camote, Banana Saba) across different regions and time periods (Quarterly, Annual) from 2010 to 2024."),
                     tags$li(tags$strong("LIVESTOCKPOULTRYFISHERIES_R.csv:"), "Contains production data for livestock, poultry, and fisheries (Milkfish, Tilapia, Hog, Chicken, Chicken Eggs) across different regions and time periods (Quarterly, Annual) from 2010 to 2024.")
                   ),
                   h3("Key Features"),
                   tags$h4("1. Overview & Trends Tab:"),
                   tags$ul(
                     tags$li(tags$strong("Summary Statistics:"), "Value boxes display total production, average production, and the number of items selected based on current filters."),
                     tags$li(tags$strong("Production Trend:"), "An interactive line chart visualizes the production volume over time for the selected item(s), geolocation, period type, and year range. A LOESS smoothing line is added for quarterly data if sufficient data points are available."),
                     tags$li(tags$strong("Production Comparison:"), "A bar chart that dynamically changes to compare:"),
                     tags$ul(
                       tags$li("Items within a selected specific region for the latest year in the selected range."),
                       tags$li("Regions for a selected specific item (when 'PHILIPPINES' is chosen as geolocation) for the latest year in the selected range.")
                     )
                   ),
                   tags$h4("2. Regional Breakdown Tab:"),
                   tags$ul(
                     tags$li("Displays a bar chart showing the production volume of a selected specific item across different regions for a user-selected year and period type.")
                   ),
                   tags$h4("3. Spatio-Temporal Analysis Tab:"),
                   tags$ul(
                     tags$li(tags$strong("Animated Map:"), "Displays a choropleth map of the Philippines, colored by production volume for the selected item and period type. A time slider allows animation across years (for Annual data) or year-quarters (for Quarterly data). Hovering over regions shows detailed information. Satellite and street map base layers are available."),
                     tags$li(tags$strong("Regional Bar Chart (Time-Linked):"), "Complements the map by showing the production volume of the selected item across different regions for the time point selected on the slider.")
                   ),
                   tags$h4("4. Data Explorer Tab:"),
                   tags$ul(
                     tags$li("Provides a searchable and sortable data table containing the raw data filtered according to the user's selections in the sidebar.")
                   ),
                   tags$h4("5. Time Series Analysis & Forecasting Tab:"),
                   tags$ul(
                     tags$li("Dedicated controls allow selection of a specific item, geolocation (excluding national aggregate), and period type (Quarterly or Annual) for in-depth time series analysis."),
                     tags$li(tags$strong("Decomposition Plot:"), "Visualizes the time series decomposed into its observed, trend, seasonal, and remainder components. It uses STL (Seasonal and Trend decomposition using Loess) for periodic data or classical decomposition as a fallback."),
                     tags$li(tags$strong("Forecast Plot:"), "Displays the historical data along with forecasted values and 80%/95% prediction intervals. The forecast model (ARIMA or ETS) is chosen automatically based on data characteristics."),
                     tags$li(tags$strong("Forecast Data Table:"), "Presents the numerical point forecasts and prediction intervals for the specified forecast horizon."),
                     tags$li(tags$strong("Model Information:"), "Shows details of the fitted time series model (e.g., ARIMA or ETS parameters).")
                   ),
                   h3("Statistical Methods"),
                   tags$ul(
                     tags$li(tags$strong("Descriptive Statistics:"), "Calculation of sums, averages, and counts displayed in value boxes and used for visualizations."),
                     tags$li(tags$strong("Data Aggregation:"), "Dynamic grouping and summarization of data based on user filters for trend and comparison plots."),
                     tags$li(tags$strong("Geospatial Visualization:"), "Choropleth maps using Leaflet to display regional production data, with animation for spatio-temporal analysis."),
                     tags$li(tags$strong("Time Series Decomposition:"), "Utilizes STL (Seasonal and Trend decomposition using Loess) or classical decomposition methods to identify underlying patterns in the data."),
                     tags$li(tags$strong("Inferential Statistics (Forecasting):"), "Employs automated ARIMA (Autoregressive Integrated Moving Average) or ETS (Error, Trend, Seasonality / Exponential Smoothing) models from the `forecast` package to predict future production volumes. Prediction intervals provide a measure of uncertainty for these forecasts.")
                   ),
                   div(class = "creator-info",
                       h3("About the Creator"),
                       tags$img(src = "job_applicant_picture.jpg", alt = "Jeff Ronyl R. Pausal",
                                style = "border-radius: 50%; width: 150px; height: 150px; object-fit: cover; margin-bottom: 15px; border: 3px solid #dee2e6; display: block; margin-left: auto; margin-right: auto;"),
                       tags$p(tags$strong("Jeff Ronyl R. Pausal")),
                       tags$p("Computer Science Student"),
                       tags$p("Major in Data Science"),
                       tags$p("University of Southeastern Philippines")
                   )
               )
      )
    )
  )
)

# SERVER
server <- function(input, output, session) {
  
  observe({
    current_cat <- input$category_filter
    if (current_cat == "All") {
      available_items <- sort(unique(full_data$Item))
    } else {
      available_items <- sort(unique(full_data$Item[full_data$Category == current_cat]))
    }
    updateSelectInput(session, "item_filter", choices = c("All", available_items), selected = "All")
    
    all_items_for_dedicated_tabs <- sort(unique(full_data$Item)) 
    updateSelectInput(session, "regional_item_filter", choices = all_items_for_dedicated_tabs,
                      selected = if(length(all_items_for_dedicated_tabs) > 0) all_items_for_dedicated_tabs[1] else NULL)
    updateSelectInput(session, "st_item_filter", choices = all_items_for_dedicated_tabs,
                      selected = if(length(all_items_for_dedicated_tabs) > 0) all_items_for_dedicated_tabs[1] else NULL)
    updateSelectInput(session, "ts_item_filter", choices = all_items_for_dedicated_tabs,
                      selected = if(length(all_items_for_dedicated_tabs) > 0) all_items_for_dedicated_tabs[1] else NULL)
  })
  
  observe({
    available_geos_main <- sort(unique(full_data$Geolocation))
    available_geos_ts <- sort(unique(full_data$Geolocation[full_data$Geolocation != "PHILIPPINES"]))
    updateSelectInput(session, "geo_filter", choices = c("PHILIPPINES", available_geos_main[available_geos_main != "PHILIPPINES"]), selected = "PHILIPPINES")
    updateSelectInput(session, "ts_geo_filter", choices = available_geos_ts, selected = if(length(available_geos_ts) > 0) available_geos_ts[1] else NULL)
  })
  
  filtered_data <- reactive({
    data <- full_data
    if (input$category_filter != "All") data <- data %>% filter(Category == input$category_filter)
    if (input$item_filter != "All") data <- data %>% filter(Item == input$item_filter)
    if (input$geo_filter != "PHILIPPINES") data <- data %>% filter(Geolocation == input$geo_filter)
    data <- data %>% filter(Period_Type == input$period_type_filter_main)
    data <- data %>% filter(Year >= input$year_range_filter[1] & Year <= input$year_range_filter[2])
    return(data)
  })
  
  output$total_production_box <- renderValueBox({
    data <- filtered_data()
    total_prod <- if(nrow(data) > 0 && input$geo_filter == "PHILIPPINES" && input$item_filter != "All") {
      full_data %>%
        filter(Item == input$item_filter, Geolocation == "PHILIPPINES", Period_Type == input$period_type_filter_main,
               Year >= input$year_range_filter[1] & Year <= input$year_range_filter[2]) %>%
        summarise(Value = sum(Value, na.rm=TRUE)) %>% pull(Value)
    } else if (nrow(data) > 0) {
      sum(data$Value, na.rm = TRUE)
    } else {
      0
    }
    item_name <- if(input$item_filter == "All") "Selected Category" else input$item_filter
    valueBox(
      format(round(total_prod, 0), big.mark = ","),
      paste("Total Production:", item_name, "(", input$period_type_filter_main, ") in", input$geo_filter),
      icon = icon("seedling"), color = "green"
    )
  })
  
  output$avg_production_box <- renderValueBox({
    data_for_avg <- filtered_data()
    if (input$geo_filter == "PHILIPPINES" && input$item_filter != "All") {
      data_for_avg <- full_data %>%
        filter(Item == input$item_filter, Geolocation == "PHILIPPINES",
               Period_Type == input$period_type_filter_main,
               Year >= input$year_range_filter[1] & Year <= input$year_range_filter[2])
    }
    if (nrow(data_for_avg) > 0 && "Value" %in% names(data_for_avg)) {
      if(input$period_type_filter_main == "Annual") {
        avg_prod <- mean(data_for_avg$Value, na.rm = TRUE)
        subtitle_text <- paste("Avg. Annual Production:", input$item_filter, "in", input$geo_filter)
      } else {
        avg_prod_per_period <- data_for_avg %>%
          group_by(Year, Period_Num) %>%
          summarise(TotalVal = sum(Value, na.rm = TRUE), .groups = 'drop') %>%
          ungroup() %>%
          summarise(AvgVal = mean(TotalVal, na.rm = TRUE)) %>%
          pull(AvgVal)
        avg_prod <- avg_prod_per_period
        subtitle_text <- paste("Avg. Quarterly Production:", input$item_filter, "in", input$geo_filter)
      }
    } else {
      avg_prod <- 0
      subtitle_text <- "Avg. Production (N/A)"
    }
    valueBox(
      format(round(avg_prod, 0), big.mark = ","),
      subtitle_text,
      icon = icon("balance-scale"), color = "blue"
    )
  })
  
  output$num_items_box <- renderValueBox({
    data <- filtered_data()
    num <- if(input$item_filter == "All" && input$category_filter != "All") length(unique(data$Item[data$Category == input$category_filter]))
    else if(input$item_filter == "All" && input$category_filter == "All") length(unique(data$Item))
    else 1
    valueBox(
      num,
      "Number of Unique Items Displayed",
      icon = icon("list"), color = "purple"
    )
  })
  
  output$trend_plot <- renderPlotly({
    data_to_plot <- filtered_data()
    if (nrow(data_to_plot) == 0) return(NULL)
    if(input$geo_filter == "PHILIPPINES"){
      if(input$item_filter == "All" && input$category_filter == "All") {
        data_agg <- full_data %>%
          filter(Geolocation == "PHILIPPINES",
                 Period_Type == input$period_type_filter_main,
                 Year >= input$year_range_filter[1] & Year <= input$year_range_filter[2]) %>%
          group_by(Date) %>% summarise(Value = sum(Value, na.rm = TRUE))
        title_plot <- "Overall Production Trend in PHILIPPINES (All Items)"
      } else if (input$item_filter == "All" && input$category_filter != "All") {
        data_agg <- full_data %>%
          filter(Geolocation == "PHILIPPINES", Category == input$category_filter,
                 Period_Type == input$period_type_filter_main,
                 Year >= input$year_range_filter[1] & Year <= input$year_range_filter[2]) %>%
          group_by(Date) %>% summarise(Value = sum(Value, na.rm = TRUE))
        title_plot <- paste("Production Trend for", input$category_filter, "in PHILIPPINES")
      } else {
        data_agg <- full_data %>%
          filter(Geolocation == "PHILIPPINES", Item == input$item_filter,
                 Period_Type == input$period_type_filter_main,
                 Year >= input$year_range_filter[1] & Year <= input$year_range_filter[2])
        title_plot <- paste("Production Trend for", input$item_filter, "in PHILIPPINES")
      }
    } else {
      data_agg <- data_to_plot %>%
        group_by(Date) %>% summarise(Value = sum(Value, na.rm = TRUE))
      title_plot <- paste("Production Trend for", ifelse(input$item_filter=="All", "Selected Category", input$item_filter), "in", input$geo_filter)
    }
    p <- ggplot(data_agg, aes(x = Date, y = Value)) +
      geom_line(color = "steelblue") + geom_point(color = "steelblue") +
      labs(title = title_plot, x = "Year", y = "Production Volume") +
      theme_minimal()
    if (input$period_type_filter_main == "Quarter" && nrow(data_agg) > 4) {
      p <- p + geom_smooth(method = "loess", se = FALSE, color = "orange", span=0.3)
    }
    ggplotly(p)
  })
  
  output$comparison_plot <- renderPlotly({
    data_to_plot_base <- filtered_data()
    if (nrow(data_to_plot_base) == 0 && !(input$item_filter != "All" && input$geo_filter == "PHILIPPINES")) {
      return(plotly_empty(type = "scatter", mode = "text") %>% layout(title = "No data for selected main filters for comparison."))
    }
    latest_year_in_range <- input$year_range_filter[2]
    if (input$item_filter == "All" && input$geo_filter != "PHILIPPINES") {
      plot_data <- full_data %>%
        filter(Geolocation == input$geo_filter,
               Category == (if(input$category_filter == "All") Category else input$category_filter),
               Year == latest_year_in_range,
               Period_Type == input$period_type_filter_main) %>%
        group_by(Item) %>%
        summarise(TotalValue = sum(Value, na.rm = TRUE), .groups = 'drop') %>%
        filter(TotalValue > 0) %>%
        arrange(desc(TotalValue)) %>%
        head(15)
      if(nrow(plot_data) == 0) return(plotly_empty(type = "scatter", mode = "text") %>% layout(title = paste("No item data for", input$geo_filter, "in", latest_year_in_range)))
      p_title <- paste("Top Item Production in", input$geo_filter, "-", latest_year_in_range, "(", input$period_type_filter_main, ")")
      p_x_label <- "Item"
    } else if (input$item_filter != "All" && input$geo_filter == "PHILIPPINES") {
      plot_data <- full_data %>%
        filter(Item == input$item_filter,
               Geolocation != "PHILIPPINES",
               Year == latest_year_in_range,
               Period_Type == input$period_type_filter_main) %>%
        group_by(Geolocation) %>%
        summarise(TotalValue = sum(Value, na.rm = TRUE), .groups = 'drop') %>%
        filter(TotalValue > 0) %>%
        arrange(desc(TotalValue))
      if(nrow(plot_data) == 0) return(plotly_empty(type = "scatter", mode = "text") %>% layout(title = paste("No regional data for", input$item_filter, "in", latest_year_in_range)))
      p_title <- paste("Regional Production for", input$item_filter, "-", latest_year_in_range, "(", input$period_type_filter_main, ")")
      p_x_label <- "Geolocation"
    } else {
      return(plotly_empty(type = "scatter", mode = "text") %>%
               layout(title = "Select (All Items + Specific Region) OR (Specific Item + PHILIPPINES) for comparison."))
    }
    p <- ggplot(plot_data, aes(x = reorder(!!sym(p_x_label), -TotalValue), y = TotalValue, fill = !!sym(p_x_label))) +
      geom_bar(stat = "identity") +
      labs(title = p_title, x = p_x_label, y = "Total Production Volume") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size=8), legend.position = "none")
    ggplotly(p)
  })
  
  output$regional_plot <- renderPlotly({
    req(input$regional_item_filter, input$regional_year_filter, input$regional_period_type_filter)
    
    regional_data_orig <- full_data %>%
      filter(Item == input$regional_item_filter,
             Year == as.integer(input$regional_year_filter),
             Period_Type == input$regional_period_type_filter,
             Geolocation != "PHILIPPINES") %>%
      group_by(Geolocation) %>%
      summarise(TotalValue = sum(Value, na.rm = TRUE), .groups = 'drop') %>%
      filter(TotalValue > 0) %>%
      arrange(desc(TotalValue))
    
    if (nrow(regional_data_orig) == 0) {
      return(plotly_empty(type = "scatter", mode = "text") %>%
               layout(title = paste("No data for", input$regional_item_filter, "in", input$regional_year_filter, "(", input$regional_period_type_filter, ")")))
    }
    p_regional_orig <- ggplot(regional_data_orig, aes(x = reorder(Geolocation, -TotalValue), y = TotalValue, fill = Geolocation)) +
      geom_bar(stat = "identity") +
      labs(title = paste("Regional Production of", input$regional_item_filter, "in", input$regional_year_filter, "(", input$regional_period_type_filter, ")"),
           x = "Region", y = "Production Volume") +
      theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1, size=8), legend.position = "none")
    ggplotly(p_regional_orig)
  })
  
  output$st_time_slider_ui <- renderUI({
    req(input$st_item_filter, input$st_period_type_filter)
    relevant_data_st <- full_data %>%
      filter(Item == input$st_item_filter, Period_Type == input$st_period_type_filter) %>%
      arrange(Date)
    if (nrow(relevant_data_st) == 0) {
      return(p("No data available for the selected item and period type to create a time slider."))
    }
    if (input$st_period_type_filter == "Annual") {
      min_year_st <- min(relevant_data_st$Year, na.rm = TRUE)
      max_year_st <- max(relevant_data_st$Year, na.rm = TRUE)
      if (is.infinite(min_year_st) || is.infinite(max_year_st) || min_year_st > max_year_st) {
        return(p("Insufficient data to determine year range for slider."))
      }
      sliderInput("st_year_slider", "Select Year:",
                  min = min_year_st, max = max_year_st, value = min_year_st, step = 1, sep = "",
                  animate = animationOptions(interval = 2000, loop = FALSE))
    } else {
      relevant_data_st_unique_periods <- relevant_data_st %>%
        distinct(Year, Period_Num, Date, .keep_all = TRUE) %>%
        arrange(Date)
      if (nrow(relevant_data_st_unique_periods) == 0) {
        return(p("Insufficient quarterly data to create a time slider."))
      }
      sliderInput("st_year_qtr_slider_index", "Select Year-Quarter (Chronological):",
                  min = 1, max = nrow(relevant_data_st_unique_periods), value = 1, step = 1,
                  animate = animationOptions(interval = 1500, loop = FALSE))
    }
  })
  
  output$selected_time_output_st <- renderText({
    req(input$st_item_filter, input$st_period_type_filter == "Quarter", input$st_year_qtr_slider_index)
    relevant_data_st_unique_periods <- full_data %>%
      filter(Item == input$st_item_filter, Period_Type == "Quarter") %>%
      arrange(Date) %>%
      distinct(Year, Period_Num, Date, .keep_all = TRUE)
    if (nrow(relevant_data_st_unique_periods) > 0 && 
        input$st_year_qtr_slider_index >= 1 &&
        input$st_year_qtr_slider_index <= nrow(relevant_data_st_unique_periods)) {
      selected_row <- relevant_data_st_unique_periods[as.integer(input$st_year_qtr_slider_index), ]
      return(paste("Currently displaying:", selected_row$Year, "Q", selected_row$Period_Num))
    } else {
      return("Select a time point.")
    }
  })
  
  current_st_time_selection <- reactive({
    req(input$st_item_filter, input$st_period_type_filter) 
    if (input$st_period_type_filter == "Annual") {
      req(input$st_year_slider) 
      return(list(year = input$st_year_slider, quarter = NULL))
    } else { 
      req(input$st_year_qtr_slider_index) 
      relevant_data_st_unique_periods <- full_data %>%
        filter(Item == input$st_item_filter, Period_Type == "Quarter") %>%
        arrange(Date) %>% 
        distinct(Year, Period_Num, Date, .keep_all = TRUE) 
      slider_idx <- as.integer(input$st_year_qtr_slider_index)
      if (nrow(relevant_data_st_unique_periods) > 0 && slider_idx >= 1 && slider_idx <= nrow(relevant_data_st_unique_periods)) {
        selected_row <- relevant_data_st_unique_periods[slider_idx, ]
        return(list(year = selected_row$Year, quarter = selected_row$Period_Num))
      } else {
        if(nrow(relevant_data_st_unique_periods) > 0){
          first_period <- relevant_data_st_unique_periods[1, ]
          return(list(year = first_period$Year, quarter = first_period$Period_Num))
        } else {
          return(list(year=NA, quarter=NA))
        }
      }
    }
  })
  
  output$spatio_temporal_map <- renderLeaflet({
    req(ph_regions_sf, input$st_item_filter, input$st_period_type_filter, 
        REGION_NAME_COLUMN_IN_SHAPEFILE != "YOUR_ACTUAL_REGION_NAME_COLUMN", 
        REGION_NAME_COLUMN_IN_SHAPEFILE %in% names(ph_regions_sf) 
    )
    time_selection <- current_st_time_selection()
    req(time_selection, !is.na(time_selection$year))
    selected_year <- time_selection$year
    selected_quarter <- time_selection$quarter
    map_data_filtered <- full_data %>%
      filter(Item == input$st_item_filter,
             Year == selected_year,
             Period_Type == input$st_period_type_filter,
             Geolocation != "PHILIPPINES")
    if (!is.null(selected_quarter)) {
      map_data_filtered <- map_data_filtered %>% filter(Period_Num == selected_quarter)
    }
    map_data_summarized <- map_data_filtered %>%
      group_by(Geolocation) %>%
      summarise(TotalValue = sum(Value, na.rm = TRUE), .groups = 'drop')
    regions_with_data_st <- ph_regions_sf %>%
      left_join(map_data_summarized, by = c("shapefile_region_name_join" = "Geolocation"))
    if (sum(!is.na(regions_with_data_st$TotalValue)) == 0 && input$st_item_filter != "All") { 
      title_message <- paste("No production data for", input$st_item_filter, "in", selected_year, 
                             if(!is.null(selected_quarter)) paste0(" Q", selected_quarter) else "")
      return(
        leaflet(data = ph_regions_sf) %>%
          addProviderTiles(providers$Esri.WorldImagery, group = "Satellite Imagery") %>%
          addProviderTiles(providers$CartoDB.Positron, group = "Street Map (Light)") %>%
          addPolygons(fillColor = "grey", weight = 1, opacity = 1, color = "white", fillOpacity = 0.5, 
                      label = ~htmltools::htmlEscape(shapefile_region_name_raw)) %>%
          addLayersControl(baseGroups = c("Satellite Imagery", "Street Map (Light)"), options = layersControlOptions(collapsed = FALSE)) %>%
          setView(lng = 121.7740, lat = 12.8797, zoom = 5.5) %>%
          addControl(title_message, position = "topright")
      )
    }
    global_production_values <- full_data %>%
      filter(Item == input$st_item_filter, 
             Period_Type == input$st_period_type_filter,
             Geolocation != "PHILIPPINES") %>%
      pull(Value)
    valid_min_values <- global_production_values[!is.na(global_production_values) & global_production_values >= 0]
    min_val_global <- if(length(valid_min_values) > 0) min(valid_min_values, na.rm = TRUE) else 0
    valid_max_values <- global_production_values[!is.na(global_production_values)]
    max_val_global <- if(length(valid_max_values) > 0) max(valid_max_values, na.rm = TRUE) else 1
    if (is.infinite(min_val_global) || is.na(min_val_global)) min_val_global <- 0
    if (is.infinite(max_val_global) || is.na(max_val_global) || max_val_global <= min_val_global) {
      max_val_global <- min_val_global + ifelse(min_val_global == 0, 1, abs(min_val_global * 0.1) + 1) 
    }
    pal_st <- colorNumeric(palette = "YlOrRd", domain = c(min_val_global, max_val_global), na.color = "#E0E0E0")
    map_labels_st <- sprintf(
      "<strong>Region:</strong> %s<br/><strong>Item:</strong> %s<br/><strong>Production:</strong> %s",
      regions_with_data_st$shapefile_region_name_raw, 
      input$st_item_filter,
      ifelse(is.na(regions_with_data_st$TotalValue), "No data", format(round(regions_with_data_st$TotalValue,0), big.mark = ","))
    ) %>% lapply(htmltools::HTML)
    map_title_text <- paste(input$st_item_filter, "- Production", selected_year, if(!is.null(selected_quarter)) paste0(" Q", selected_quarter) else "")
    leaflet(data = regions_with_data_st) %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite Imagery") %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Street Map (Light)") %>%
      addPolygons(
        fillColor = ~pal_st(TotalValue),
        weight = 1.5, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.7,
        highlightOptions = highlightOptions(weight = 3, color = "#444", dashArray = "", fillOpacity = 0.9, bringToFront = TRUE),
        label = map_labels_st,
        labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px", "font-size" = "12px"), textsize = "12px", direction = "auto")
      ) %>%
      addLegend(pal = pal_st, 
                values =  if(sum(!is.na(regions_with_data_st$TotalValue)) > 0) regions_with_data_st$TotalValue else c(min_val_global, max_val_global), 
                opacity = 0.7, 
                title = paste("Prod. of", truncate_text(input$st_item_filter, 10)), position = "bottomright", na.label = "No Data") %>%
      addLayersControl(baseGroups = c("Satellite Imagery", "Street Map (Light)"), options = layersControlOptions(collapsed = FALSE)) %>%
      setView(lng = 121.7740, lat = 12.8797, zoom = 5.5) %>%
      addControl(tags$div(HTML(paste0("<h4>", map_title_text, "</h4>"))), position = "topright")
  })
  
  output$spatio_temporal_barchart <- renderPlotly({
    req(input$st_item_filter, input$st_period_type_filter)
    time_selection <- current_st_time_selection()
    req(time_selection, !is.na(time_selection$year))
    selected_year <- time_selection$year
    selected_quarter <- time_selection$quarter
    plot_title_suffix <- paste(selected_year, if(!is.null(selected_quarter)) paste0(" Q", selected_quarter) else "")
    st_regional_data_filtered <- full_data %>%
      filter(Item == input$st_item_filter,
             Year == selected_year,
             Period_Type == input$st_period_type_filter,
             Geolocation != "PHILIPPINES")
    if(!is.null(selected_quarter)){
      st_regional_data_filtered <- st_regional_data_filtered %>% filter(Period_Num == selected_quarter)
    }
    st_regional_data_summarized <- st_regional_data_filtered %>%
      group_by(Geolocation) %>%
      summarise(TotalValue = sum(Value, na.rm = TRUE), .groups = 'drop') %>%
      filter(TotalValue > 0) %>% 
      arrange(desc(TotalValue))
    if (nrow(st_regional_data_summarized) == 0) {
      return(plotly_empty(type = "scatter", mode = "text") %>% layout(title = paste("No production data for", input$st_item_filter, "in", plot_title_suffix)))
    }
    p_st_regional <- ggplot(st_regional_data_summarized, aes(x = reorder(Geolocation, -TotalValue), y = TotalValue, fill = Geolocation)) +
      geom_bar(stat = "identity") +
      labs(title = paste("Regional Production of", input$st_item_filter, "-", plot_title_suffix),
           x = "Region", y = "Production Volume") +
      theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1, size=8), legend.position = "none")
    ggplotly(p_st_regional)
  })
  
  # ########## THIS IS THE MISSING PART - DATA EXPLORER TABLE #############
  output$data_table <- renderDT({
    data_for_dt <- filtered_data() # Uses the main sidebar filters
    
    # Ensure 'Date' column exists before trying to remove it, if that's intended
    if ("Date" %in% names(data_for_dt)) {
      data_for_dt_display <- data_for_dt %>% select(-Date)
    } else {
      data_for_dt_display <- data_for_dt
    }
    
    # Defensive check for number of columns if using hardcoded order index
    num_cols <- ncol(data_for_dt_display)
    dt_options <- list(pageLength = 10, scrollX = TRUE)
    if (num_cols >= 3) { # Ensure column index 2 (3rd column) is valid for ordering
      dt_options$order = list(list(2, 'desc')) 
    }
    
    datatable(data_for_dt_display, 
              options = dt_options, 
              rownames = FALSE,
              filter = 'top' # Adds individual column filters at the top of the table
    ) 
  })
  # #######################################################################
  
  ts_analysis_results <- eventReactive(input$run_ts_analysis, {
    shiny::validate(
      need(input$ts_item_filter != "", "Please select an Item for Time Series Analysis."),
      need(input$ts_geo_filter != "", "Please select a Geolocation for Time Series Analysis."),
      need(input$ts_period_type_filter != "", "Please select a Period Type for Time Series Analysis.")
    )
    ts_df_filtered <- full_data %>%
      filter(Item == input$ts_item_filter,
             Geolocation == input$ts_geo_filter,
             Period_Type == input$ts_period_type_filter) %>%
      arrange(Date) %>%
      select(Date, Value, Year, Period_Num)
    min_obs_quarterly <- 8 
    min_obs_annual <- 5 
    ts_df_filtered_non_na <- ts_df_filtered %>% filter(!is.na(Value))
    if (input$ts_period_type_filter == "Quarter" && nrow(ts_df_filtered_non_na) < min_obs_quarterly) {
      shiny::validate(paste("Not enough non-NA quarterly data (need", min_obs_quarterly, "). Has", nrow(ts_df_filtered_non_na)))
      return(NULL)
    }
    if (input$ts_period_type_filter == "Annual" && nrow(ts_df_filtered_non_na) < min_obs_annual) {
      shiny::validate(paste("Not enough non-NA annual data (need", min_obs_annual, "). Has", nrow(ts_df_filtered_non_na)))
      return(NULL)
    }
    ts_freq <- ifelse(input$ts_period_type_filter == "Quarter", 4, 1)
    start_year <- min(ts_df_filtered_non_na$Year, na.rm = TRUE) 
    start_period_num <- if (ts_freq == 4) {
      min_period_num <- min(ts_df_filtered_non_na$Period_Num[ts_df_filtered_non_na$Year == start_year], na.rm = TRUE)
      if (is.infinite(min_period_num)) 1 else min_period_num 
    } else {1}
    if(is.infinite(start_year) || is.na(start_year) || (ts_freq == 4 && (is.infinite(start_period_num) || is.na(start_period_num)) ) ){
      shiny::validate("Could not determine a valid start date for the time series.")
      return(NULL)
    }
    ts_values_imputed <- NULL 
    if (ts_freq == 4) {
      if(nrow(ts_df_filtered_non_na) == 0) {shiny::validate("No non-NA quarterly data."); return(NULL)}
      all_dates_in_range <- seq(min(ts_df_filtered_non_na$Date, na.rm=T), max(ts_df_filtered_non_na$Date, na.rm=T), by = "quarter")
      complete_df <- data.frame(Date = all_dates_in_range) %>% left_join(ts_df_filtered_non_na, by = "Date") 
      ts_val_temp <- complete_df$Value
      ts_val_temp <- zoo::na.locf(ts_val_temp, na.rm = FALSE) 
      ts_val_temp <- zoo::na.locf(ts_val_temp, fromLast = TRUE, na.rm = FALSE) 
      if(sum(!is.na(ts_val_temp)) > 1) {ts_val_temp <- na.approx(ts_val_temp, na.rm = FALSE)}
      ts_values_imputed <- ts_val_temp
    } else { 
      if(nrow(ts_df_filtered_non_na) == 0) {shiny::validate("No non-NA annual data."); return(NULL)}
      min_yr <- min(ts_df_filtered_non_na$Year); max_yr <- max(ts_df_filtered_non_na$Year)
      all_years_df <- data.frame(Year = seq(min_yr, max_yr, by=1))
      annual_data_prepared <- all_years_df %>% left_join(ts_df_filtered_non_na %>% select(Year, Value), by="Year") 
      ts_val_temp <- annual_data_prepared$Value
      ts_val_temp <- zoo::na.locf(ts_val_temp, na.rm = FALSE)
      ts_val_temp <- zoo::na.locf(ts_val_temp, fromLast = TRUE, na.rm = FALSE)
      if(sum(!is.na(ts_val_temp)) > 1) {ts_val_temp <- na.approx(ts_val_temp, na.rm = FALSE)}
      ts_values_imputed <- ts_val_temp
    }
    if(all(is.na(ts_values_imputed))) {
      shiny::validate(paste("Insufficient data: all values are NA after imputation for", input$ts_item_filter, "in", input$ts_geo_filter))
      return(NULL)
    }
    min_points_needed_q <- max(min_obs_quarterly, 2 * ts_freq)
    min_points_needed_a <- min_obs_annual
    if (ts_freq == 4 && sum(!is.na(ts_values_imputed)) < min_points_needed_q) {
      shiny::validate(paste("Not enough data points post-imputation for quarterly series (need", min_points_needed_q,"). Found:", sum(!is.na(ts_values_imputed))))
      return(NULL)
    }
    if (ts_freq == 1 && sum(!is.na(ts_values_imputed)) < min_points_needed_a) {
      shiny::validate(paste("Not enough data points post-imputation for annual series (need", min_points_needed_a,"). Found:", sum(!is.na(ts_values_imputed))))
      return(NULL)
    }
    ts_object_actual <- ts(ts_values_imputed, start = c(start_year, start_period_num), frequency = ts_freq)
    fit <- tryCatch({
      if (length(ts_object_actual) < 2 * ts_freq || length(ts_object_actual) < 10) { 
        ets(ts_object_actual)
      } else {
        auto.arima(ts_object_actual, stepwise = TRUE, approximation = FALSE, trace = FALSE,
                   allowdrift = TRUE, allowmean = TRUE, D = ifelse(ts_freq > 1, 1, 0))
      }
    }, error = function(e) {
      shiny::validate(paste("Error fitting ARIMA:", e$message, ". Trying ETS."))
      tryCatch({ ets(ts_object_actual) }, error = function(e_ets) {
        shiny::validate(paste("ETS fallback failed:", e_ets$message))
        return(NULL)
      })
    })
    if(is.null(fit)) return(NULL)
    fc <- forecast(fit, h = input$forecast_horizon)
    return(list(ts_object_for_model = ts_object_actual, 
                original_data_for_plot = ts_df_filtered_non_na, 
                forecast_obj = fc, 
                model_fit = fit))
  })
  
  output$ts_model_info <- renderPrint({
    results <- ts_analysis_results()
    if(is.null(results$model_fit)) return("No model fitted. Select filters & run analysis.")
    print(results$model_fit)
  })
  
  output$ts_decomposition_plot <- renderPlotly({
    results <- ts_analysis_results()
    req(results$ts_object_for_model) 
    ts_obj <- results$ts_object_for_model
    original_plot_data <- results$original_data_for_plot 
    time_idx_ts_obj <- time(ts_obj)
    dates_ts_obj <- if(frequency(ts_obj) == 4) as.Date(as.yearqtr(time_idx_ts_obj)) else make_date(floor(time_idx_ts_obj),1,1)
    decomp_df_list <- list() 
    if (frequency(ts_obj) < 2 || length(ts_obj) < 2 * frequency(ts_obj)) { 
      if (frequency(ts_obj) > 1 && length(ts_obj) >= 2 * frequency(ts_obj)) { 
        decomp_classical <- decompose(ts_obj, type = "multiplicative") 
        decomp_df_list$Observed <- data.frame(Date=dates_ts_obj, Value = as.numeric(decomp_classical$x), Component="Observed (Imputed)")
        if(!is.null(decomp_classical$trend)) decomp_df_list$Trend <- data.frame(Date=dates_ts_obj, Value = as.numeric(decomp_classical$trend), Component="Trend")
        if(!is.null(decomp_classical$seasonal)) decomp_df_list$Seasonal <- data.frame(Date=dates_ts_obj, Value = as.numeric(decomp_classical$seasonal), Component="Seasonal")
        if(!is.null(decomp_classical$random)) decomp_df_list$Random <- data.frame(Date=dates_ts_obj, Value = as.numeric(decomp_classical$random), Component="Random")
        p_title <- paste("Classical Decomposition:", input$ts_item_filter)
      } else { 
        plot_df <- original_plot_data %>% select(Date, Value) %>% rename(Observed_Original=Value)
        p_decomp_gg <- ggplot(plot_df, aes(x=Date, y=Observed_Original)) + geom_line(color="blue") +
          geom_point(color="blue", size=1) +
          ggtitle(paste(input$ts_item_filter, "(Decomposition needs more data/periodicity)")) + theme_minimal()
        return(ggplotly(p_decomp_gg))
      }
    } else { 
      decomp_stl <- stl(ts_obj, s.window = "periodic", robust = TRUE)
      decomp_df_list$Observed <- data.frame(Date=dates_ts_obj, Value = as.numeric(ts_obj), Component="Observed (Imputed)")
      decomp_df_list$Trend <- data.frame(Date=dates_ts_obj, Value = as.numeric(decomp_stl$time.series[,"trend"]), Component="Trend")
      decomp_df_list$Seasonal <- data.frame(Date=dates_ts_obj, Value = as.numeric(decomp_stl$time.series[,"seasonal"]), Component="Seasonal")
      decomp_df_list$Remainder <- data.frame(Date=dates_ts_obj, Value = as.numeric(decomp_stl$time.series[,"remainder"]), Component="Remainder")
      p_title <- paste("STL Decomposition:", input$ts_item_filter)
    }
    if (!is.null(decomp_df_list$Observed) && nrow(original_plot_data) > 0) {
      original_points_df <- original_plot_data %>% select(Date, Value) %>% mutate(Component = "Observed (Imputed)", PointType = "Original")
    } else {
      original_points_df <- NULL
    }
    decomp_df_final <- bind_rows(decomp_df_list) %>% mutate(Component = factor(Component, levels = unique(Component))) 
    p_decomp_gg <- ggplot(decomp_df_final, aes(x = Date, y = Value)) +
      geom_line(color = "darkcyan") +
      facet_wrap(~Component, scales = "free_y", ncol = 1) +
      labs(title = p_title, x = "Time", y="Value") + theme_bw() +
      theme(strip.text = element_text(face = "bold"))
    if (!is.null(original_points_df)) {
      p_decomp_gg <- p_decomp_gg + geom_point(data = original_points_df, aes(x=Date, y=Value), color="red", size=1.5, alpha=0.7)
    }
    ggplotly(p_decomp_gg, height = 700) 
  })
  
  output$ts_forecast_plot <- renderPlotly({
    results <- ts_analysis_results()
    req(results$forecast_obj, results$original_data_for_plot)
    fc <- results$forecast_obj
    historical_plot_data <- results$original_data_for_plot 
    last_hist_date <- max(historical_plot_data$Date, na.rm = TRUE)
    fc_horizon <- length(fc$mean)
    if (input$ts_period_type_filter == "Quarter") {
      forecast_dates <- seq(from = last_hist_date %m+% months(3), by = "quarter", length.out = fc_horizon)
    } else { 
      forecast_dates <- seq(from = last_hist_date %m+% years(1), by = "year", length.out = fc_horizon)
    }
    forecast_plot_df <- data.frame(
      Date = forecast_dates, Value = as.numeric(fc$mean),
      Lo.80 = as.numeric(fc$lower[,1]), Hi.80 = as.numeric(fc$upper[,1]),
      Lo.95 = as.numeric(fc$lower[,2]), Hi.95 = as.numeric(fc$upper[,2])
    )
    p_fc_manual <- ggplot() +
      geom_line(data = historical_plot_data, aes(x = Date, y = Value, colour = "Historical Data")) +
      geom_point(data = historical_plot_data, aes(x = Date, y = Value, colour = "Historical Data"), size=1.5) + 
      geom_line(data = forecast_plot_df, aes(x = Date, y = Value, colour = "Forecast")) +
      geom_ribbon(data = forecast_plot_df, aes(x = Date, ymin = Lo.95, ymax = Hi.95, fill = "95% PI"), alpha = 0.2) +
      geom_ribbon(data = forecast_plot_df, aes(x = Date, ymin = Lo.80, ymax = Hi.80, fill = "80% PI"), alpha = 0.3) +
      scale_colour_manual(name = "Data Series", values = c("Historical Data" = "navyblue", "Forecast" = "orangered")) +
      scale_fill_manual(name = "Prediction Interval", values = c("95% PI" = "grey70", "80% PI" = "grey50")) +
      labs(title = paste("Forecast:", input$ts_item_filter, "in", input$ts_geo_filter),
           subtitle = paste("Model:", fc$method),
           x = "Date", y = "Production Volume") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5, face="bold"), plot.subtitle = element_text(hjust = 0.5))
    ggplotly(p_fc_manual)
  })
  
  output$ts_forecast_table <- renderDT({
    results <- ts_analysis_results()
    if (is.null(results$forecast_obj) || is.null(results$original_data_for_plot) ) {
      return(datatable(data.frame(Message="No forecast data. Run analysis.")))
    }
    fc <- results$forecast_obj
    last_date_historical <- max(results$original_data_for_plot$Date, na.rm=TRUE)
    if (input$ts_period_type_filter == "Quarter") {
      forecast_dates <- seq(from = last_date_historical %m+% months(3), by = "quarter", length.out = input$forecast_horizon)
      period_labels <- format(as.yearqtr(forecast_dates), format = "%Y Q%q")
    } else { 
      forecast_dates <- seq(from = last_date_historical %m+% years(1), by = "year", length.out = input$forecast_horizon)
      period_labels <- as.character(year(forecast_dates))
    }
    forecast_df_table <- data.frame(
      Period = period_labels, Forecasted_Value = as.numeric(fc$mean),
      `Lower_80_PI` = as.numeric(fc$lower[,1]), `Upper_80_PI` = as.numeric(fc$upper[,1]),
      `Lower_95_PI` = as.numeric(fc$lower[,2]), `Upper_95_PI` = as.numeric(fc$upper[,2])
    )
    datatable(forecast_df_table, 
              options = list(pageLength = input$forecast_horizon, scrollX = TRUE, dom = 't'), 
              caption = htmltools::tags$caption(
                style = 'caption-side: top; text-align: center; font-weight: bold;',
                paste('Forecasted Production for', input$ts_item_filter, 'in', input$ts_geo_filter)
              ),
              rownames=FALSE,
              colnames = c('Period', 'Point Forecast', 'Low 80%', 'High 80%', 'Low 95%', 'High 95%')
    ) %>% formatRound(columns = 2:6, digits = 2)
  })
}

shinyApp(ui = ui, server = server)