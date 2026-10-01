# ============================================================
# E-COMMERCE SALES DASHBOARD
# R SHINY
# ============================================================

library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(readr)
library(scales)


# ============================================================
# 1. READ ZIP FILE
# ============================================================

zip_file <- "C://Users//Administrator//Downloads//iphone_sales_dataset.zip"

if (!file.exists(zip_file)) {
  stop("ZIP file not found. Check the file path.")
}

# Temporary extraction folder
extract_folder <- file.path(
  tempdir(),
  "ecommerce_sales"
)

dir.create(
  extract_folder,
  recursive = TRUE,
  showWarnings = FALSE
)

# Extract ZIP
unzip(
  zip_file,
  exdir = extract_folder,
  overwrite = TRUE
)

# Find CSV
csv_file <- list.files(
  extract_folder,
  pattern = "\\.csv$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

if (length(csv_file) == 0) {
  stop("No CSV file found inside the ZIP.")
}

# Read CSV
sales_data <- read_csv(
  csv_file[1],
  show_col_types = FALSE
)


# ============================================================
# 2. CLEAN DATA
# ============================================================

names(sales_data) <- trimws(names(sales_data))

sales_data$Sale_Date <- as.Date(
  sales_data$Sale_Date
)

sales_data <- sales_data %>%
  mutate(
    Revenue = Price * Quantity
  )


# ============================================================
# 3. USER INTERFACE
# ============================================================

ui <- dashboardPage(
  
  # ----------------------------------------------------------
  # HEADER
  # ----------------------------------------------------------
  
  dashboardHeader(
    title = "E-Commerce Sales"
  ),
  
  
  # ----------------------------------------------------------
  # SIDEBAR
  # ----------------------------------------------------------
  
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem(
        "Dashboard",
        tabName = "dashboard",
        icon = icon("dashboard")
      ),
      
      menuItem(
        "Sales Analysis",
        tabName = "analysis",
        icon = icon("bar-chart")
      ),
      
      menuItem(
        "Sales Data",
        tabName = "data",
        icon = icon("table")
      ),
      
      menuItem(
        "About",
        tabName = "about",
        icon = icon("info-circle")
      )
      
    ),
    
    br(),
    
    h4(
      "FILTERS",
      style = "color:white;text-align:center;"
    ),
    
    
    # COUNTRY FILTER
    selectInput(
      "country",
      "Country:",
      choices = sort(unique(sales_data$Country)),
      selected = unique(sales_data$Country),
      multiple = TRUE
    ),
    
    
    # MODEL FILTER
    selectInput(
      "model",
      "iPhone Model:",
      choices = sort(unique(sales_data$iPhone_Model)),
      selected = unique(sales_data$iPhone_Model),
      multiple = TRUE
    ),
    
    
    # STORAGE FILTER
    selectInput(
      "storage",
      "Storage:",
      choices = sort(unique(sales_data$Storage)),
      selected = unique(sales_data$Storage),
      multiple = TRUE
    ),
    
    
    # PAYMENT FILTER
    selectInput(
      "payment",
      "Payment Method:",
      choices = sort(unique(sales_data$Payment_Method)),
      selected = unique(sales_data$Payment_Method),
      multiple = TRUE
    ),
    
    
    # DATE FILTER
    dateRangeInput(
      "date",
      "Sales Date:",
      start = min(
        sales_data$Sale_Date,
        na.rm = TRUE
      ),
      end = max(
        sales_data$Sale_Date,
        na.rm = TRUE
      ),
      format = "dd-mm-yyyy"
    ),
    
    
    br(),
    
    
    # RESET BUTTON
    actionButton(
      "reset",
      "Reset Filters",
      icon = icon("refresh"),
      class = "btn-warning",
      width = "100%"
    ),
    
    
    br(),
    br(),
    
    
    # DOWNLOAD
    downloadButton(
      "downloadData",
      "Download CSV",
      class = "btn-success",
      width = "100%"
    )
    
  ),
  
  
  # ==========================================================
  # DASHBOARD BODY
  # ==========================================================
  
  dashboardBody(
    
    # --------------------------------------------------------
    # CUSTOM CSS
    # --------------------------------------------------------
    
    tags$head(
      
      tags$style(HTML("

        .content-wrapper {
          background-color: #f4f6f9;
        }

        .main-header .logo {
          background-color: #111827 !important;
          font-weight: bold;
        }

        .main-header .navbar {
          background-color: #1f2937 !important;
        }

        .main-sidebar {
          background-color: #111827 !important;
        }

        .sidebar-menu > li.active > a {
          background-color: #2563eb !important;
        }

        .box {
          border-radius: 12px;
          box-shadow: 0px 3px 12px rgba(0,0,0,0.10);
        }

        .small-box {
          border-radius: 12px;
          box-shadow: 0px 4px 15px rgba(0,0,0,0.12);
        }

        .dashboard-title {
          font-size: 30px;
          font-weight: bold;
          color: #111827;
        }

      "))
      
    ),
    
    
    tabItems(
      
      # ======================================================
      # DASHBOARD TAB
      # ======================================================
      
      tabItem(
        
        tabName = "dashboard",
        
        h1(
          "E-Commerce Sales Dashboard",
          class = "dashboard-title"
        ),
        
        p(
          "Interactive Sales Performance Dashboard"
        ),
        
        
        # KPI CARDS
        fluidRow(
          
          valueBoxOutput(
            "totalSales",
            width = 3
          ),
          
          valueBoxOutput(
            "totalOrders",
            width = 3
          ),
          
          valueBoxOutput(
            "totalQuantity",
            width = 3
          ),
          
          valueBoxOutput(
            "averageOrder",
            width = 3
          )
          
        ),
        
        
        # SALES TREND
        fluidRow(
          
          box(
            title = "Sales Trend",
            status = "primary",
            solidHeader = TRUE,
            width = 8,
            
            plotlyOutput(
              "salesTrend",
              height = "350px"
            )
          ),
          
          box(
            title = "Payment Methods",
            status = "info",
            solidHeader = TRUE,
            width = 4,
            
            plotlyOutput(
              "paymentChart",
              height = "350px"
            )
          )
          
        ),
        
        
        # MODEL + COUNTRY
        fluidRow(
          
          box(
            title = "Sales by iPhone Model",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            
            plotlyOutput(
              "modelChart",
              height = "400px"
            )
          ),
          
          box(
            title = "Sales by Country",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            
            plotlyOutput(
              "countryChart",
              height = "400px"
            )
          )
          
        ),
        
        
        # STORAGE + COLOR
        fluidRow(
          
          box(
            title = "Storage Analysis",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            
            plotlyOutput(
              "storageChart",
              height = "350px"
            )
          ),
          
          box(
            title = "Color Analysis",
            status = "danger",
            solidHeader = TRUE,
            width = 6,
            
            plotlyOutput(
              "colorChart",
              height = "350px"
            )
          )
          
        )
        
      ),
      
      
      # ======================================================
      # ANALYSIS TAB
      # ======================================================
      
      tabItem(
        
        tabName = "analysis",
        
        h1(
          "Detailed Sales Analysis",
          class = "dashboard-title"
        ),
        
        fluidRow(
          
          box(
            title = "Revenue by Country",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            plotlyOutput(
              "countryRevenue",
              height = "450px"
            )
          )
          
        ),
        
        fluidRow(
          
          box(
            title = "Revenue by Model and Storage",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            
            plotlyOutput(
              "modelStorage",
              height = "500px"
            )
          )
          
        )
        
      ),
      
      
      # ======================================================
      # DATA TAB
      # ======================================================
      
      tabItem(
        
        tabName = "data",
        
        h1(
          "Sales Transaction Data",
          class = "dashboard-title"
        ),
        
        box(
          
          title = "Filtered Sales Records",
          
          status = "primary",
          
          solidHeader = TRUE,
          
          width = 12,
          
          DTOutput(
            "salesTable"
          )
          
        )
        
      ),
      
      
      # ======================================================
      # ABOUT TAB
      # ======================================================
      
      tabItem(
        
        tabName = "about",
        
        h1(
          "About Project",
          class = "dashboard-title"
        ),
        
        box(
          
          title = "E-Commerce Sales Dashboard",
          
          status = "primary",
          
          solidHeader = TRUE,
          
          width = 12,
          
          HTML("

            <h2>E-Commerce Sales Dashboard</h2>

            <p>
            This project is an interactive E-Commerce Sales
            Dashboard developed using R and Shiny.
            </p>

            <h3>Project Objectives</h3>

            <ul>

              <li>Analyze total revenue</li>

              <li>Analyze total orders</li>

              <li>Analyze units sold</li>

              <li>Analyze iPhone models</li>

              <li>Analyze country-wise sales</li>

              <li>Analyze payment methods</li>

              <li>Analyze storage preferences</li>

              <li>Analyze color preferences</li>

              <li>Analyze sales trends</li>

              <li>Download filtered data</li>

            </ul>

            <h3>Technologies Used</h3>

            <ul>

              <li>R</li>

              <li>RStudio</li>

              <li>Shiny</li>

              <li>Shinydashboard</li>

              <li>Plotly</li>

              <li>DT</li>

              <li>dplyr</li>

              <li>ggplot2</li>

            </ul>

          ")
          
        )
        
      )
      
    )
    
  )
  
)


# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {
  
  
  # ----------------------------------------------------------
  # FILTERED DATA
  # ----------------------------------------------------------
  
  filtered_data <- reactive({
    
    req(input$country)
    req(input$model)
    req(input$storage)
    req(input$payment)
    req(input$date)
    
    sales_data %>%
      
      filter(
        
        Country %in% input$country,
        
        iPhone_Model %in% input$model,
        
        Storage %in% input$storage,
        
        Payment_Method %in% input$payment,
        
        Sale_Date >= input$date[1],
        
        Sale_Date <= input$date[2]
        
      )
    
  })
  
  
  # ----------------------------------------------------------
  # RESET FILTERS
  # ----------------------------------------------------------
  
  observeEvent(input$reset, {
    
    updateSelectInput(
      session,
      "country",
      selected = unique(sales_data$Country)
    )
    
    updateSelectInput(
      session,
      "model",
      selected = unique(sales_data$iPhone_Model)
    )
    
    updateSelectInput(
      session,
      "storage",
      selected = unique(sales_data$Storage)
    )
    
    updateSelectInput(
      session,
      "payment",
      selected = unique(sales_data$Payment_Method)
    )
    
    updateDateRangeInput(
      session,
      "date",
      start = min(
        sales_data$Sale_Date,
        na.rm = TRUE
      ),
      end = max(
        sales_data$Sale_Date,
        na.rm = TRUE
      )
    )
    
  })
  
  
  # ==========================================================
  # KPI 1
  # ==========================================================
  
  output$totalSales <- renderValueBox({
    
    data <- filtered_data()
    
    total <- sum(
      data$Revenue,
      na.rm = TRUE
    )
    
    valueBox(
      
      value = dollar(
        total,
        prefix = "$"
      ),
      
      subtitle = "Total Revenue",
      
      icon = icon(
        "money"
      ),
      
      color = "green"
      
    )
    
  })
  
  
  # ==========================================================
  # KPI 2
  # ==========================================================
  
  output$totalOrders <- renderValueBox({
    
    data <- filtered_data()
    
    orders <- n_distinct(
      data$Order_ID
    )
    
    valueBox(
      
      value = comma(orders),
      
      subtitle = "Total Orders",
      
      icon = icon(
        "shopping-cart"
      ),
      
      color = "blue"
      
    )
    
  })
  
  
  # ==========================================================
  # KPI 3
  # ==========================================================
  
  output$totalQuantity <- renderValueBox({
    
    data <- filtered_data()
    
    quantity <- sum(
      data$Quantity,
      na.rm = TRUE
    )
    
    valueBox(
      
      value = comma(quantity),
      
      subtitle = "Units Sold",
      
      icon = icon(
        "cube"
      ),
      
      color = "orange"
      
    )
    
  })
  
  
  # ==========================================================
  # KPI 4
  # ==========================================================
  
  output$averageOrder <- renderValueBox({
    
    data <- filtered_data()
    
    orders <- n_distinct(
      data$Order_ID
    )
    
    revenue <- sum(
      data$Revenue,
      na.rm = TRUE
    )
    
    avg <- ifelse(
      orders > 0,
      revenue / orders,
      0
    )
    
    valueBox(
      
      value = dollar(
        avg,
        prefix = "$"
      ),
      
      subtitle = "Average Order Value",
      
      icon = icon(
        "line-chart"
      ),
      
      color = "purple"
      
    )
    
  })
  
  
  # ==========================================================
  # SALES TREND
  # ==========================================================
  
  output$salesTrend <- renderPlotly({
    
    data <- filtered_data() %>%
      
      group_by(Sale_Date) %>%
      
      summarise(
        Revenue = sum(
          Revenue,
          na.rm = TRUE
        ),
        .groups = "drop"
      )
    
    p <- ggplot(
      data,
      aes(
        x = Sale_Date,
        y = Revenue,
        text = paste(
          "Date:",
          Sale_Date,
          "<br>Revenue:",
          dollar(Revenue)
        )
      )
    ) +
      
      geom_line(
        linewidth = 1.2
      ) +
      
      geom_point(
        size = 3
      ) +
      
      labs(
        x = "Date",
        y = "Revenue"
      ) +
      
      theme_minimal()
    
    ggplotly(
      p,
      tooltip = "text"
    )
    
  })
  
  
  # ==========================================================
  # PAYMENT CHART
  # ==========================================================
  
  output$paymentChart <- renderPlotly({
    
    data <- filtered_data() %>%
      
      group_by(Payment_Method) %>%
      
      summarise(
        Revenue = sum(
          Revenue,
          na.rm = TRUE
        ),
        .groups = "drop"
      )
    
    p <- ggplot(
      data,
      aes(
        x = "",
        y = Revenue,
        fill = Payment_Method,
        text = paste(
          Payment_Method,
          "<br>Revenue:",
          dollar(Revenue)
        )
      )
    ) +
      
      geom_col(
        width = 1
      ) +
      
      coord_polar(
        theta = "y"
      ) +
      
      theme_void()
    
    ggplotly(
      p,
      tooltip = "text"
    )
    
  })
  
  
  # ==========================================================
  # MODEL CHART
  # ==========================================================
  
  output$modelChart <- renderPlotly({
    
    data <- filtered_data() %>%
      
      group_by(iPhone_Model) %>%
      
      summarise(
        Revenue = sum(
          Revenue,
          na.rm = TRUE
        ),
        .groups = "drop"
      )
    
    p <- ggplot(
      data,
      aes(
        x = reorder(
          iPhone_Model,
          Revenue
        ),
        y = Revenue,
        text = paste(
          "Model:",
          iPhone_Model,
          "<br>Revenue:",
          dollar(Revenue)
        )
      )
    ) +
      
      geom_col() +
      
      coord_flip() +
      
      labs(
        x = "iPhone Model",
        y = "Revenue"
      ) +
      
      theme_minimal()
    
    ggplotly(
      p,
      tooltip = "text"
    )
    
  })
  
  
  # ==========================================================
  # COUNTRY CHART
  # ==========================================================
  
  output$countryChart <- renderPlotly({
    
    data <- filtered_data() %>%
      
      group_by(Country) %>%
      
      summarise(
        Revenue = sum(
          Revenue,
          na.rm = TRUE
        ),
        .groups = "drop"
      )
    
    p <- ggplot(
      data,
      aes(
        x = reorder(
          Country,
          Revenue
        ),
        y = Revenue,
        text = paste(
          "Country:",
          Country,
          "<br>Revenue:",
          dollar(Revenue)
        )
      )
    ) +
      
      geom_col() +
      
      coord_flip() +
      
      labs(
        x = "Country",
        y = "Revenue"
      ) +
      
      theme_minimal()
    
    ggplotly(
      p,
      tooltip = "text"
    )
    
  })
  
  
  # ==========================================================
  # STORAGE CHART
  # ==========================================================
  
  output$storageChart <- renderPlotly({
    
    data <- filtered_data() %>%
      
      group_by(Storage) %>%
      
      summarise(
        Quantity = sum(
          Quantity,
          na.rm = TRUE
        ),
        .groups = "drop"
      )
    
    p <- ggplot(
      data,
      aes(
        x = Storage,
        y = Quantity,
        text = paste(
          "Storage:",
          Storage,
          "<br>Units:",
          comma(Quantity)
        )
      )
    ) +
      
      geom_col() +
      
      labs(
        x = "Storage",
        y = "Units Sold"
      ) +
      
      theme_minimal()
    
    ggplotly(
      p,
      tooltip = "text"
    )
    
  })
  
  
  # ==========================================================
  # COLOR CHART
  # ==========================================================
  
  output$colorChart <- renderPlotly({
    
    data <- filtered_data() %>%
      
      group_by(Color) %>%
      
      summarise(
        Quantity = sum(
          Quantity,
          na.rm = TRUE
        ),
        .groups = "drop"
      )
    
    p <- ggplot(
      data,
      aes(
        x = reorder(
          Color,
          Quantity
        ),
        y = Quantity,
        text = paste(
          "Color:",
          Color,
          "<br>Units:",
          comma(Quantity)
        )
      )
    ) +
      
      geom_col() +
      
      coord_flip() +
      
      labs(
        x = "Color",
        y = "Units Sold"
      ) +
      
      theme_minimal()
    
    ggplotly(
      p,
      tooltip = "text"
    )
    
  })
  
  
  # ==========================================================
  # COUNTRY REVENUE
  # ==========================================================
  
  output$countryRevenue <- renderPlotly({
    
    data <- filtered_data() %>%
      
      group_by(Country) %>%
      
      summarise(
        Revenue = sum(
          Revenue,
          na.rm = TRUE
        ),
        .groups = "drop"
      )
    
    p <- ggplot(
      data,
      aes(
        x = reorder(
          Country,
          Revenue
        ),
        y = Revenue,
        text = paste(
          "Country:",
          Country,
          "<br>Revenue:",
          dollar(Revenue)
        )
      )
    ) +
      
      geom_col() +
      
      coord_flip() +
      
      labs(
        x = "Country",
        y = "Revenue"
      ) +
      
      theme_minimal()
    
    ggplotly(
      p,
      tooltip = "text"
    )
    
  })
  
  
  # ==========================================================
  # MODEL + STORAGE
  # ==========================================================
  
  output$modelStorage <- renderPlotly({
    
    data <- filtered_data() %>%
      
      group_by(
        iPhone_Model,
        Storage
      ) %>%
      
      summarise(
        Revenue = sum(
          Revenue,
          na.rm = TRUE
        ),
        .groups = "drop"
      )
    
    p <- ggplot(
      data,
      aes(
        x = iPhone_Model,
        y = Revenue,
        fill = Storage,
        text = paste(
          "Model:",
          iPhone_Model,
          "<br>Storage:",
          Storage,
          "<br>Revenue:",
          dollar(Revenue)
        )
      )
    ) +
      
      geom_col(
        position = "dodge"
      ) +
      
      labs(
        x = "iPhone Model",
        y = "Revenue",
        fill = "Storage"
      ) +
      
      theme_minimal() +
      
      theme(
        axis.text.x =
          element_text(
            angle = 45,
            hjust = 1
          )
      )
    
    ggplotly(
      p,
      tooltip = "text"
    )
    
  })
  
  
  # ==========================================================
  # DATA TABLE
  # ==========================================================
  
  output$salesTable <- renderDT({
    
    datatable(
      
      filtered_data(),
      
      options = list(
        pageLength = 10,
        scrollX = TRUE
      ),
      
      rownames = FALSE,
      
      filter = "top"
      
    )
    
  })
  
  
  # ==========================================================
  # DOWNLOAD CSV
  # ==========================================================
  
  output$downloadData <- downloadHandler(
    
    filename = function() {
      
      paste0(
        "Ecommerce_Sales_Filtered_",
        Sys.Date(),
        ".csv"
      )
      
    },
    
    content = function(file) {
      
      write.csv(
        filtered_data(),
        file,
        row.names = FALSE
      )
      
    }
    
  )
  
}


# ============================================================
# RUN APP
# ============================================================

shinyApp(
  ui = ui,
  server = server
)