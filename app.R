# Load required packages
required_packages <- c(
  "shiny", "shinydashboard", "ggplot2", "dplyr", 
  "leaflet", "plotly", "gtable", "grid", "rsconnect"
)

# Check and install missing packages
needed <- required_packages[!sapply(required_packages, require, character.only = TRUE)]
if (length(needed) > 0) {
  install.packages(needed, repos = "https://cloud.r-project.org")
}

# Load required libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(leaflet)
library(plotly)
library(gtable)
library(grid)
library(rsconnect)

# Set shinyapps.io account details
rsconnect::setAccountInfo(
  name = 'mianyuzhou',
  token = 'B9F0F3E23B2EF8D9368933DBBADD8475',
  secret = 'Yfl+vazT1Vz/awEoV4NTJq+OUEcdS31uCAkO1c9s'
)

# URLs for datasets hosted on GitHub
urls <- list(
  country1 = "https://github.com/Minaaaa-z/shiny-app-data/raw/refs/heads/main/1.%20country_inflow_outflow.rds",
  country2 = "https://github.com/Minaaaa-z/shiny-app-data/raw/refs/heads/main/2.%20country_inflow_outflow_ind.rds",
  country3 = "https://github.com/Minaaaa-z/shiny-app-data/raw/refs/heads/main/3.%20country_inflow_outflow_skill.rds",
  pairwise4 = "https://github.com/Minaaaa-z/shiny-app-data/raw/refs/heads/main/4.%20pairwise_flows.rds",
  pairwise5 = "https://github.com/Minaaaa-z/shiny-app-data/raw/refs/heads/main/5.%20pairwise_flows_ind.rds",
  pairwise6 = "https://github.com/Minaaaa-z/shiny-app-data/raw/refs/heads/main/6.%20pairwise_flows_skill.rds"
)

# Helper function to load data from GitHub URLs
load_data <- function(url) {
  temp_file <- tempfile(fileext = ".rds")
  download.file(url, temp_file, mode = "wb")  # 'wb' for binary files
  readRDS(temp_file)
}

# Load datasets dynamically from GitHub
country1 <- load_data(urls$country1)
country2 <- load_data(urls$country2)
country3 <- load_data(urls$country3)
pairwise4 <- load_data(urls$pairwise4)
pairwise5 <- load_data(urls$pairwise5)
pairwise6 <- load_data(urls$pairwise6)

# Optimize app loading
options(shiny.sanitize.errors = FALSE) # Disable error sanitization for debugging
options(shiny.maxRequestSize = 30 * 1024^2) # Increase max request size for large datasets
options(rsconnect.max.bundle.size = 3145728000 * 2) 
# Define a minimal UI and Server to test deployment
ui <- dashboardPage(
  dashboardHeader(title = "Test App"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
      box(title = "Welcome", width = 12, status = "primary", "App loaded successfully!")
    )
  )
)

server <- function(input, output, session) {
  message("Server started")
}

# Test the app locally
# Uncomment this to test locally before deploying
# shinyApp(ui = ui, server = server)

# Deploy app to shinyapps.io
rsconnect::deployApp(
  appDir = ".",  # Use the current working directory
  appFiles = c(
    "app.R"  # Only app.R is necessary since data is dynamically loaded
  ),
  appName = "ass2",
  forceUpdate = TRUE
)

# View logs to debug deployment
rsconnect::showLogs(appName = "ass2")






# Country map location
country_coords <- data.frame(
  country_region = c("Algeria", "Argentina", "Australia", "Austria", "Bahrain", "Bangladesh",
                     "Belgium", "Brazil", "Canada", "Chile", "China", "Colombia", "Costa Rica",
                     "Cyprus", "Croatia", "Denmark", "Ecuador", "Egypt", "Estonia", "Finland",
                     "France", "Germany", "Ghana", "Greece", "Iceland", "India", "Indonesia",
                     "Iraq", "Ireland", "Israel", "Italy", "Japan", "Jordan", "Kenya", "Latvia",
                     "Lebanon", "Luxembourg", "Malaysia", "Malta", "Mexico", "Morocco", 
                     "Netherlands", "New Zealand", "Nigeria", "Norway", "Pakistan", "Panama", 
                     "Peru", "Philippines", "Poland", "Portugal", "Puerto Rico", "Qatar", "Romania", 
                     "Saudi Arabia", "Singapore", "Slovenia", "South Africa", "Spain", "Sri Lanka",
                     "Sweden", "Switzerland", "Thailand", "Tunisia", "Turkey", "Ukraine", 
                     "United Arab Emirates", "United Kingdom", "United States", "Uruguay", "Venezuela"),
  latitude = c(28.0339, -34.6037, -25.2744, 47.5162, 26.0667, 23.6850, 50.5039, -14.2350,
               56.1304, -35.6751, 35.8617, 4.5709, 9.7489, 35.1264, 45.1, 56.2639, -1.8312,
               26.8206, 58.5953, 61.9241, 46.6034, 51.1657, 7.9465, 39.0742, 64.9631, 20.5937,
               -0.7893, 33.2232, 53.1424, 31.0461, 41.8719, 36.2048, 30.5852, -1.2921, 56.8796,
               33.8547, 49.8153, 4.2105, 35.9375, 23.6345, 31.7917, 52.1326, -40.9006, 9.0820,
               60.4720, 30.3753, 8.5379, -9.19, 12.8797, 51.9194, 39.3999, 18.2208, 25.3548,
               45.9432, 23.8859, 1.3521, 46.1512, -30.5595, 40.4637, 7.8731, 60.1282, 46.8182,
               15.8700, 33.8869, 38.9637, 48.3794, 23.4241, 55.3781, 37.0902, -32.5228, 6.4238),
  longitude = c(1.6596, -58.3816, 133.7751, 14.5501, 50.5577, 90.3563, 4.4699, -51.9253,
                -106.3468, -71.5430, 104.1954, -74.2973, -83.7534, 33.4299, 15.2, 9.5018,
                -78.1834, 30.8025, 25.0136, 25.7482, 2.2137, 10.4515, -1.0232, 21.8243,
                -19.0208, 78.9629, 113.9213, 43.6793, -7.6921, 34.8516, 12.5674, 138.2529,
                36.2384, 36.8219, 24.6032, 35.8623, 6.1296, 101.9758, 14.3754, -102.5528,
                -7.0926, 5.2913, 174.8859, 8.6753, 8.4689, 69.3451, -80.7821, -75.0152,
                121.7740, 19.1451, -8.2245, -66.5901, 51.1839, 24.9668, 45.0792, 46.1996,
                18.4241, -3.7492, 80.7718, 18.6435, 8.2275, 100.9925, 9.5375, 35.2433,
                31.1656, 50.4501, 53.8478, -3.4360, -95.7129, -55.7658, -66.5897)
)
# Merge country1 with country_coords based on country name
country1 <- merge(country1, country_coords, by = "country_region", all.x = TRUE)

# UI Layout using shinydashboard
dashboardHeader <- dashboardHeader(title = "Global Talent Migration Dashboard")

dashboardSidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Introduction", tabName = "introduction", icon = icon("info-circle")),
    menuItem("Migration Map & Trends", tabName = "map_trends", icon = icon("globe")),
    menuItem("Insight & Analysis", tabName = "insight_analysis", icon = icon("chart-bar")),
    menuItem("Summary", tabName = "summary", icon = icon("clipboard-list")),
    menuItem("Reference and Data Source", tabName = "Reference", icon = icon("book"))
  )
)



dashboardBody <- dashboardBody(
  tabItems(
    tabItem(tabName = "introduction",
            fluidRow(
              box(title = "Introduction", width = 12, status = "primary", solidHeader = TRUE,
                  p(style = "font-size: 18px;","Welcome to the Global Talent Migration Dashboard. Tis project will explore how talent migrates globally across countries, industries, and skill levels. And identify which countries and sectors are experiencing brain drain [loss of skilled talent] or brain gain [influx of skilled talent], and provide insights into the skills that are currently in high demand, focusing on skill level, industry needs, and flow trends."))
            )
    ),
    tabItem(tabName = "map_trends",
            fluidRow(
              box(title = "Migration Map", width = 6, status = "primary", solidHeader = TRUE,
                  p(style = "font-size: 18px;","This map shows the inflow and outflow of talent for selected countries. You can see the ratio of migration, where red indicates higher inflow than outflow and green indicates lower inflow. Use the filters to explore the data by year and country."),
                  leafletOutput("migrationMap")),
              box(title = "Time Series Trends", width = 6, status = "primary", solidHeader = TRUE,
                  p(style = "font-size: 18px;","This section shows the overall migration trend over time. The time series chart reflects how the inflow/outflow ratio has evolved across the years."),
                  p(style = "font-size: 18px;","Generally, the inflow/outflow ratio is keeping increasing."),
                  plotOutput("timeSeries"))
            )
    ),
    tabItem(tabName = "insight_analysis",
            fluidRow(
              box(title = "Filters", width = 12, status = "primary", solidHeader = TRUE,
                  p(style = "font-size: 18px;","You can select specific country, skill level, industry, and year here."),
                  selectInput("country", "Select Country", choices = c("All", unique(country1$country_region)), selected = "All", multiple = FALSE),
                  selectInput("skill_level", "Skill Level", choices = c("All", unique(country3$skill_level)), selected = "All", multiple = FALSE),
                  selectInput("industry", "Industry", choices = c("All", unique(country2$l1_industry_name)), selected = "All", multiple = FALSE),
                  selectInput("year", "Select Year", choices = c("All", unique(country1$year)), selected = "All", multiple = FALSE)
              )
            ),
            fluidRow(
              box(title = "Skill Level Analysis", width = 6, status = "primary", solidHeader = TRUE,
                  p(style = "font-size: 18px;","This chart provides insights into the migration trends categorized by skill level. You can see the average inflow/outflow ratio for different skill levels across the selected filters."),
                  p(style = "font-size: 18px;", "Generally, low-skill migration is dominant in developed countries."),
                  plotOutput("skillLevelChart")),
              box(title = "Summary of Insights", width = 6, status = "primary", solidHeader = TRUE,
                  textOutput("insightSummary"),
                  style = "font-size: 18px")
            ),
            fluidRow(
              box(title = "Industry Demand Analysis", width = 6, status = "primary", solidHeader = TRUE,
                  p(style = "font-size: 18px;","This chart shows the top 10 industries by average inflow/outflow ratio when select 'all' of industry filter. It helps identify which industries are experiencing significant talent migration."),
                  p(style = "font-size: 18px;"," Industries like healthcare and retail show higher demand for talent."),
                  plotOutput("industryDemandChart")),
              box(title = "Country Comparison Heatmap", width = 6, status = "primary", solidHeader = TRUE,
                  p(style = "font-size: 18px;","This heatmap shows a comparison of migration flows between different countries. If a specific country is selected, it shows the source-destination migration ratios for that country."),
                  p(style = "font-size: 18px;","Canada, Luxembourg, Australia and Switzerland ranking high in inflow/outflow ratios"),
                 p(style = "font-size: 18px;","Canada people prefers talent migrate to Switzerland, United State, Germany, New Zetland;
                    Australians prefers talent migrate to Switzerland;
                    Luxembourg people prefer talent migrate to Germany;
                    Switzerland people prefer talent migrate to United Arab Emirates.
                    "),
                  plotOutput("overviewHeatmap"))
            )
    ),
    
    tabItem(
      tabName = "summary",
      fluidPage(
        div(style = "text-align: center;", titlePanel("Summary")),
        mainPanel(
          width = 12,
          p(style = "font-size: 18px;", "
         The dashboard integrates multiple datasets, including country inflow/outflow statistics, 
         industry-specific migration data, and skill-level migration patterns. 
         This comprehensive approach provides a holistic view of global talent migration."
          ),
          p(style = "font-size: 18px; margin-bottom: 20px;", "
         Migration Trends Over Time: The time series analysis reveals fluctuations in migration patterns, 
         generally increasing, even during COVID-19."
          ),
          p(style = "font-size: 18px; margin-bottom: 20px;", "
         Skill Level Migration: Analysis indicates varying inflow/outflow ratios across different skill levels. 
         Low-skill migration is dominant in developed countries, as seen in the Skill Level chart; 
         industries like healthcare and retail show higher demand for talent, reflecting global economic priorities."
          ),
          p(style = "font-size: 18px;", "
         Global Aging Trend: Developed countries are experiencing aging populations due to declining fertility rates 
         and increasing life expectancy. This demographic shift has led to an increased demand for healthcare services, 
         including elderly care and specialized treatments."
          ),
          p(style = "font-size: 18px; margin-bottom: 20px;", "
         Industry Demand: The industry demand chart highlights sectors with higher talent inflow, reflecting global 
         industry trends and economic shifts. Industries like healthcare and retail show higher demand for talent."
          ),
          p(style = "font-size: 18px; margin-bottom: 20px;", "
         Country Comparisons: The heatmap highlights migration hubs, with countries like Canada, Luxembourg, 
         Australia, and Switzerland ranking high in inflow/outflow ratios."
          ),
          p(style = "font-size: 18px; margin-bottom: 20px;", "Canada people prefers talent migrate to Switzerland, United State, Germany, New Zetland;
              Australians prefers talent migrate to Switzerland;
              Luxembourg people prefer talent migrate to Germany;
              Switzerland people prefer talent migrate to United Arab Emirates.
")
        )
      )
    ),
    
    
    tabItem(
      tabName = "Reference",
      fluidRow(
        div(style = "text-align: center;", titlePanel("Reference and Data Source")),
        mainPanel(
          width = 12,
          br(),
          h3("Reference"),
          p(
            style = "font-size: 18px;", 
            "1. George W. Bush Presidential Center: ", 
            br(),
            a(href = "https://www.bushcenter.org/publications/how-gender-and-technology-impact-migration", 
              "How Gender and Technology Impact Migration"),
            br(),
            "2. CBRE: ", 
            br(),
            a(href = "https://www.cbre.com/insights/viewpoints/global-talent-migration-the-new-mobility-paradigm", 
              "Global Talent Migration: The ‘New Mobility’ Paradigm: "),
            br(),
            "3. R-studio github: ", 
            br(),
            a(href = "https://rstudio.github.io/shinydashboard/appearance.html", 
              "Shiny Dashboard Introduction"),
            br(),
            "4. Work Visa Lawyers: ", 
            br(),
            a(href = "https://www.workvisalawyers.com.au/news/all/top-5-most-popular-sectors-for-the-global-talent-visa-program.html",
              "Top 5 Most Popular Sectors For The Global Talent Visa Program"),
            br(),
            "5. Population Europe: ",
            br(),
            a(href = "https://population-europe.eu/research/policy-insights/race-global-talent",
              "The Race for Global Talent"),
          ),
          br(),
          h3("Datasets"),
          p(
            style = "font-size: 18px;", 
            "WorldBank Group :", 
            br(),
            a(href = "https://datacatalog.worldbank.org/search/dataset/0038044/Talent-Migration---LinkedIn-Data-", 
              "[2024 upload]Excel: Consolidated Country, Industry, Skill Migration Data", target = "_blank")
          )
        )
      )
    )
  )
)


ui <- dashboardPage(
  dashboardHeader,
  dashboardSidebar,
  dashboardBody
)

# Server Logic
server <- function(input, output) {
  
  filtered_map_data <- reactive({
    data <- country1
    
    if (!("All" %in% input$country)) {
      data <- data %>% filter(country_region %in% input$country)
    }
    
    if (!("All" %in% input$year)) {
      data <- data %>% filter(year %in% input$year)
    } else {
      data <- data %>%
        group_by(country_region, latitude, longitude) %>%
        summarise(inflow_outflow_ratio = mean(inflow_outflow_ratio, na.rm = TRUE)) %>%
        ungroup()
    }
    
    return(data)
  })
  
  filtered_skill_data <- reactive({
    data <- country3  
    
    if (!("All" %in% input$country)) {
      data <- data %>% filter(country_region %in% input$country)
    }
    
    if (!("All" %in% input$skill_level)) {
      data <- data %>% filter(skill_level %in% input$skill_level)
    }
    
    if (!("All" %in% input$year)) {
      data <- data %>% filter(year %in% input$year)
    }
    
    if ("All" %in% input$country || "All" %in% input$skill_level || "All" %in% input$year) {
      data <- data %>%
        group_by(skill_level) %>%
        summarise(inflow_outflow_ratio = mean(inflow_outflow_ratio, na.rm = TRUE))
    }
    
    return(data)
  })
  
  output$migrationMap <- renderLeaflet({
    data <- filtered_map_data()
    
    if (is.null(data) || nrow(data) == 0) {
      showNotification("No data available for the selected filters", type = "error")
      return(NULL)
    }
    
    leaflet(data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addCircles(
        lng = ~longitude,
        lat = ~latitude,
        weight = 1,
        color = ~ifelse(inflow_outflow_ratio > 1, "red", "green"),
        radius = 90000,
        label = ~paste(country_region, ": ", round(inflow_outflow_ratio, 2))
      ) %>%
      addLegend(
        "bottomright",
        colors = c("red", "green"),
        labels = c("High Ratio (>1)", "Low Ratio (<1)"),
        title = if ("All" %in% input$year) {
          "Overall Average Inflow/Outflow Ratio"
        } else {
          paste("Inflow/Outflow Ratio for", input$year)
        }
      )
  })
  
  output$insightSummary <- renderText({
    data <- country1
    
    if (!("All" %in% input$country)) {
      data <- data %>% filter(country_region %in% input$country)
    }
    
    if (!("All" %in% input$year)) {
      data <- data %>% filter(year %in% input$year)
    }
    
    if (!("All" %in% input$industry)) {
      industry_data <- country2 %>% filter(l1_industry_name %in% input$industry)
      data <- data %>% filter(country_region %in% industry_data$country_region & year %in% industry_data$year)
    }
    
    if (!("All" %in% input$skill_level)) {
      skill_data <- country3 %>% filter(skill_level %in% input$skill_level)
      data <- data %>% filter(country_region %in% skill_data$country_region & year %in% skill_data$year)
    }
    
    if (nrow(data) == 0) {
      return("No data available for the selected filters.")
    }
    
    avg_ratio <- round(mean(data$inflow_outflow_ratio, na.rm = TRUE), 2)
    
    filters <- list()
    if (!("All" %in% input$country)) {
      filters <- append(filters, paste0("country '", paste(input$country, collapse = ", "), "'"))
    }
    if (!("All" %in% input$industry)) {
      filters <- append(filters, paste0("industry '", paste(input$industry, collapse = ", "), "'"))
    }
    if (!("All" %in% input$skill_level)) {
      filters <- append(filters, paste0("skill level '", paste(input$skill_level, collapse = ", "), "'"))
    }
    
    if (length(filters) == 0 && "All" %in% input$year) {
      return(paste0("The overall average inflow/outflow ratio is ", avg_ratio, " from 2019-2022."))
    } else if (length(filters) == 0) {
      return(paste0("For year ", input$year, ", the overall average inflow/outflow ratio is ", avg_ratio, "."))
    } else if (length(filters) == 1 && "All" %in% input$year) {
      return(paste0("The inflow/outflow ratio of ", filters[[1]], " is ", avg_ratio, " from 2019-2022."))
    } else if (length(filters) == 1) {
      return(paste0("For year ", input$year, ", the inflow/outflow ratio of ", filters[[1]], " is ", avg_ratio, "."))
    } else if (length(filters) == 2 && "All" %in% input$year) {
      return(paste0("The inflow/outflow ratio of ", paste(filters, collapse = " and "), " is ", avg_ratio, " from 2019-2022."))
    } else if (length(filters) == 2) {
      return(paste0("For year ", input$year, ", the inflow/outflow ratio of ", paste(filters, collapse = " and "), " is ", avg_ratio, "."))
    } else {
      return("The overall inflow/outflow ratio summary is not defined for the selected filters.")
    }
  })
  
  output$skillLevelChart <- renderPlot({
    skill_data <- filtered_skill_data()
    
    if (is.null(skill_data) || nrow(skill_data) == 0) {
      showNotification("No data available for the selected filters", type = "error")
      return(NULL)
    }
    
    skill_data$skill_level <- factor(skill_data$skill_level, levels = c("HIGH", "MEDIUM", "LOW"))
    
    ggplot(skill_data, aes(x = skill_level, y = inflow_outflow_ratio, fill = skill_level)) +
      geom_bar(stat = "identity") +
      labs(
        title = if ("All" %in% input$country && "All" %in% input$year)
          "Skill Level Migration Analysis (Overview)"
        else if ("All" %in% input$country)
          paste("Skill Level Migration Analysis for All Countries in", input$year)
        else if ("All" %in% input$year)
          paste("Skill Level Migration Analysis for", input$country, "(All Years)")
        else
          paste("Skill Level Migration Analysis for", input$country, "in", input$year),
        x = "Skill Level",
        y = "Average Inflow/Outflow Ratio"
      ) +
      theme_minimal() +
      theme(
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold"),
        legend.position = "none"
      )
  })
  
  
  output$industryDemandChart <- renderPlot({
    industry_filtered <- country2 %>%
      filter((input$country == "All" | country_region %in% input$country),
             (input$industry == "All" | l1_industry_name %in% input$industry),
             (input$year == "All" | year %in% input$year))
    
    if (nrow(industry_filtered) == 0) {
      showNotification("No data available for the selected filters", type = "error")
      return(NULL)
    }
    
    aggregated_data <- industry_filtered %>%
      group_by(l1_industry_name) %>%
      summarize(
        Avg_Ratio = mean(inflow_outflow_ratio, na.rm = TRUE)
      )
    
    ggplot(aggregated_data, aes(x = reorder(l1_industry_name, Avg_Ratio), y = Avg_Ratio, fill = Avg_Ratio)) +
      geom_bar(stat = "identity") +
      labs(
        title = if ("All" %in% input$country && "All" %in% input$industry && "All" %in% input$year)
          "Top 10 Industries by Average Inflow/Outflow Ratio"
        else
          "Industry Demand for Talent",
        x = "Industry",
        y = "Average Inflow/Outflow Ratio"
      ) +
      theme_minimal() +
      theme(
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold"),
        legend.position = "none"
      ) +
      scale_fill_gradient(low = "lightblue", high = "blue") +
      coord_flip()
  })
  
  
  output$overviewHeatmap <- renderPlot({
    if ("All" %in% input$country) {
      # Show overall average for all countries
      table_data <- country1 %>%
        filter((input$year == "All" | year %in% input$year)) %>%  # Apply year filter
        group_by(country_region) %>%
        summarize(
          Avg_Ratio = round(mean(inflow_outflow_ratio, na.rm = TRUE), 2)
        ) %>%
        arrange(desc(Avg_Ratio)) %>%
        slice_head(n = 10)  # Top 10 countries
      
      ggplot(table_data, aes(x = 1, y = reorder(country_region, -Avg_Ratio), fill = Avg_Ratio)) +
        geom_tile(color = "white", width = 0.5) +  # Adjust width for aesthetics
        geom_text(aes(label = Avg_Ratio), color = "black", size = 5) +
        scale_fill_gradient(low = "red", high = "green", name = "Ratio") +
        labs(
          title = if (input$year == "All") "Top 10 Countries by Average Inflow/Outflow Ratio" 
          else paste("Top 10 Countries by Inflow/Outflow Ratio in", input$year),
          x = "",
          y = "Country"
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_text(size = 12),
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          legend.position = "none"  # Remove legend
        )
    } else {
      # Show heatmap for specific country
      table_data <- pairwise4 %>%
        filter(source_country_region %in% input$country, 
               (input$year == "All" | year %in% input$year)) %>%  # Apply year filter
        group_by(source_country_region, dest_country_region) %>%
        summarize(
          Avg_Ratio = round(mean(s2d_d2s_ratio, na.rm = TRUE), 2)
        ) %>%
        arrange(desc(Avg_Ratio))
      
      # Check if there is data to plot
      if (nrow(table_data) == 0) {
        showNotification("No data available for the selected filters", type = "error")
        return(NULL)
      }
      
      ggplot(table_data, aes(x = source_country_region, y = reorder(dest_country_region, -Avg_Ratio), fill = Avg_Ratio)) +
        geom_tile(color = "white", width = 0.5) +  # Adjust width for aesthetics
        geom_text(aes(label = Avg_Ratio), color = "black", size = 5) +
        scale_fill_gradient(low = "red", high = "green", name = "Ratio") +
        labs(
          title = if (input$year == "All") 
            paste("Source-Destination Ratios for", paste(input$country, collapse = ", "), "(All Years)") 
          else 
            paste("Source-Destination Ratios for", paste(input$country, collapse = ", "), "in", input$year),
          x = "Source Country",
          y = "Destination Country"
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(hjust = 1),
          axis.text.y = element_text(size = 12),
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          legend.position = "none"  # Remove legend
        )
    }
  })
  
  output$timeSeries <- renderPlot({
    overall_data <- country1 %>%
      group_by(year) %>%
      summarise(avg_ratio = mean(inflow_outflow_ratio, na.rm = TRUE))
    
    ggplot(overall_data, aes(x = year, y = avg_ratio)) +
      geom_line(size = 1.5, color = "blue") +
      geom_point(size = 3, color = "blue") +
      labs(
        title = "Overall Average Time Series of Migration Trends",
        x = "Year",
        y = "Average Inflow/Outflow Ratio"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)
      )
  })
}

# Run the Shiny App
shinyApp(ui = ui, server = server)
