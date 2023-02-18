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
            width = 12,
            imageOutput("adidasLogo"),
            style='width: 99.9%;margin:auto'
          ),
          fluidRow(
            width = 12,
          ),
          fluidRow(
            style='margin-top:70px;',
            infoBox("TOTAL TRANSACTION", nrow(data), icon = icon("shopping-cart"), color = 'red', width = 4),
            infoBox("TOTAL SALES", glue("USD {comma(total_sales)}"), icon = icon("money-bill"), color = 'black', width = 4),
            infoBox("TOTAL PRODUCT SOLD", glue("{sum(data$`Units Sold`)} Item"), icon = icon("headset"), color = 'black', width = 4),
          ),
          fluidRow(
            box(
              width = 12
            )
          )
  ),
  
  tabItem(tabName = "page2",
          fluidRow(
            box(
              width = 12,
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
            uiOutput('total_transaction_location'),
            uiOutput('total_sales_location'),
            uiOutput("total_sold_location")
          ),
          fluidRow(
            box(
              width = 4,
              title = "Market Share Retailer",
              plotOutput("market_share_pie")
            ),
            box(
              width = 8,
              title = "Market Share Ranking",
              
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