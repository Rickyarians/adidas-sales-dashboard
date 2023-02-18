sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Overview", tabName = "page1", icon = icon("video")),
    menuItem("Data By Location", tabName = "page2", icon = icon("youtube")),
    menuItem("Data", tabName = "page3", icon = icon("table"))
  )
)


body <- dashboardBody(tabItems(
  tabItem(tabName = "page1",
          fluidRow(
            valueBox(nrow(data), 
                     "Total Transaction", 
                     icon = icon("file-invoice-dollar"), 
                     color = 'aqua', 
                     width = 4),
            valueBox(glue("USD {comma(total_sales)}"), 
                    "Total Sales",
                    icon = icon("money-bill"), 
                    color = 'aqua', 
                    width = 4),
            valueBox(glue("{sum(data$`Units Sold`)} Item"), 
                     "Total Product Sold",
                     icon = icon("cart-shopping"), 
                     color = 'aqua', 
                     width = 4),
          ),
          fluidRow(
            box(
              width = 6,
              title = "Adidas Total Sales By Product Categories",
              plotlyOutput(outputId = 'product_ranking_sales')
            ),
            box(
              width = 6,
              title = "Adidas Total Sold Unit By Product Categories",
              plotlyOutput(outputId = 'product_ranking_sold')
            )
          ),
          fluidRow(
            width = 12,
            box(
              width = 12,
              selectInput(
                inputId = 'select_year',
                label = "Choose Year",
                choices = unique(data$year_invoice)
              ),
            ),
            box(
              width = 12,
              plotlyOutput(outputId = 'history_sales_month')
            )
          ),
          fluidRow(
            width = 12,
            box(
              width = 12,
              leafletOutput(outputId = 'map_sales')
            )
          ),
  ),
  
  tabItem(tabName = "page2",
          fluidRow(
            uiOutput('total_transaction_location'),
            uiOutput('total_sales_location'),
            uiOutput("total_sold_location")
          ),
          fluidRow(
            box(
              width = 12,
              status = "info",
              column(
                width = 4,
                selectInput(
                  inputId = 'select_region',
                  label = "Choose Region",
                  choices = as.vector(levels(data_filter$Region))
                ),
              ),
              column(
                width = 4,
                uiOutput('select_state')
              ),
              column(
                width = 4,
                uiOutput('select_city')
              ),
            )),
          fluidRow(
            box(
              width = 4,
              height = 500,
              title = "Market Share Retailer",
              plotOutput("market_share_pie")
            ),
            box(
              width = 8,
              height = 500,
              plotlyOutput(outputId = "market_share_ranking_retailer")
            )
          ),
          fluidRow(
            box(
              width = 6,
             
            ),
            box(
              width = 6,
             
            )
          )
  ),
  
  tabItem(tabName = "page3",
          fluidRow(
            box(
              width = 12,
              DT::dataTableOutput(outputId = 'datatable')
            )
          )
  ))
)


dashboardTitle <- dashboardHeader(
  title = "Adidas Sales Analysis"
)


dashboardPage(
  skin = 'black',
  dashboardTitle,
  sidebar,
  body
)