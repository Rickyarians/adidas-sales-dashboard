sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Home", tabName = "page1", icon = icon("home")),
    menuItem("Overview", tabName = "page2", icon = icon("eye")),
    menuItem("Heatmap", tabName = "page3", icon = icon("map")),
    menuItem("Data By City", tabName = "page4", icon = icon("city")),
    menuItem("Data Table", tabName = "page5", icon = icon("table")),
    menuItem("Developer", tabName = "page6", icon = icon("person"))
  )
)


body <- dashboardBody(tabItems(
  tabItem(tabName = "page1",
          fluidRow(
            box(
              width = 12,
              status = "info",
              fluidRow(
                box(
                  height = 700,
                  width = 6,
                  imageOutput('adidasLogo'),
                  solidHeader = TRUE
                ),
                box(
                  width =6,
                  solidHeader = TRUE,
                  h1("Adidas Sales Dataset"),
                  p("An Adidas sales dataset is a collection of data that includes information on the sales of Adidas products. This type of dataset may include details such as the number of units sold, the total sales revenue, the location of the sales, the type of product sold, and any other relevant information."),
                  p("Adidas sales data can be useful for a variety of purposes, such as analyzing sales trends, identifying successful products or marketing campaigns, and developing strategies for future sales. It can also be used to compare Adidas sales to those of competitors, or to analyze the effectiveness of different marketing or sales channels."),
                  p("There are a variety of sources that could potentially provide an Adidas sales dataset, including Adidas itself, market research firms, government agencies, or other organizations that track sales data. The specific data points included in an Adidas sales dataset may vary depending on the source and the purpose for which it is being used.")
                )
              )
            )
          ),
          
  ),
  tabItem(tabName = "page2",
          fluidRow(
            box(
              width = 12,
              selectInput(
                inputId = 'select_year',
                label = "Choose Year",
                choices = unique(data$year_invoice)
              ),
            ),
            uiOutput('card_total_transaction_all'),
            uiOutput('card_total_sales_all'),
            uiOutput('card_total_sold_all'),
           
          ),
          fluidRow(
            box(
              width = 4,
              status = "info",
              # title = "Adidas Total Sales By Product Categories",
              plotOutput("market_share_overview")
            ),
            box(
              width = 4,
              status = "info",
              # title = "Adidas Total Sales By Product Categories",
              plotlyOutput(outputId = 'product_ranking_sales')
            ),
            box(
              width = 4,
              status = "info",
              # title = "Adidas Total Sold Unit By Product Categories",
              plotlyOutput(outputId = 'product_ranking_sold')
            ),
          ),
          fluidRow(
            box(
              width = 6,
              status = "info",
              plotlyOutput(outputId = 'history_sales_month')
            ),
            box(
              width = 6,
              status = "info",
              # title = "Adidas Total Sold Unit By Product Categories",
              plotlyOutput(outputId = 'scatter_plot')
            )
          ),
  ),
  
  tabItem(tabName = "page3",
          fluidRow(
            box(
              status = "info",
              width = 12,
                dateRangeInput("date_range", "From :",
                          start = min(data$`Invoice Date`),
                          end =  max(data$`Invoice Date`)
                          ),
            ),
          ),
          fluidRow(
            box(
              width = 12,
              solidHeader = T,
              collapsible = T,
              status = "info",
              title = "Sales Ranking",
              sliderInput("slider_input_heatmap",
                          "Limit Ranking",
                          value = 3,
                          min = 1,
                          max = length(unique(data$Product)),
                          step =1
                          ),
              fluidRow(
                box(
                   solidHeader = T,
                   plotlyOutput("heatmap_ranking_sales")
                ),
                box(
                  solidHeader = T,
                  plotlyOutput("heatmap_ranking_sold")
                )
              )
            ),
          
          ),
          fluidRow(
            box(
              width = 6,
              status = "info",
              solidHeader = T,
              collapsible = T,
              title = "Heatmap State by Total Sales",
              leafletOutput("heatmap_sales")
            ),
            box(
              width = 6,
              status = "info",
              solidHeader = T,
              collapsible = T,
              title = "Heatmap State by Total Unit Sold",
              leafletOutput("heatmap_unit_sold")
            )
          )
  ),
  
  tabItem(tabName = "page4",
          fluidRow(
            tags$head(tags$style(HTML(".small-box {height: 140px}"))),
            uiOutput('rank_start_city'),
            box(
              width = 10,
              title = "Filter Data By Location",
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
            uiOutput('total_transaction_location'),
            uiOutput("total_sold_location"),
            uiOutput('total_sales_location'),
            uiOutput("total_operating_profit")
          ),
          fluidRow(
            box(
              width = 4,
              solidHeader = T,
              collapsible = T,
              status = 'info',
              title = "Market Share",
              radioButtons("radio_value_select","By", c("Total Sales", "Units Sold"), inline=T),
              plotOutput("market_share_overview_location")
            ),
            box(
              width = 8,
              solidHeader = T,
              collapsible = T,
              status = 'info',
              height = 520,
              title = "Sales Characteristic",
              plotlyOutput(outputId = "sales_characterisctic")
            )
          ),
          fluidRow(
            box(
              width = 6,
              solidHeader = T,
              collapsible = T,
              status = 'info',
              title = "Sales Perfomance",
              radioButtons("radio_value_select1","By", c("Total Sales", "Units Sold"), inline=T),
              plotlyOutput(outputId = "sales_performance")
            ),
            box(
              width = 6,
              solidHeader = T,
              collapsible = T,
              status = 'info',
              title = "Sales Ranking",
              radioButtons("radio_value_select2","By", c("Total Sales", "Units Sold"), inline=T),
              plotlyOutput(outputId = "sales_ranking")
            )
          )
  ),
  
  tabItem(tabName = "page5",
          fluidRow(
            box(
              width = 12,
              DT::dataTableOutput(outputId = 'datatable')
            )
          )
  ),
  tabItem(
    tabName = "page6",
    fluidRow(
      box(
        width = 4,
        status = "info",
        solidHeader = T,
        title = "Creator",
        h3("Ricky Ariansyah"),
        br(),
        actionLink(inputId = 'linkedin', 
                   label = "linkedIn", 
                   class = "btn btn-info mt-3", 
                   onclick="window.open('https://www.linkedin.com/in/rickyarians/', '_blank')"),
        actionLink(inputId = 'linkedin', 
                   label = "github", 
                   class = "btn btn-primary mt-3", 
                   onclick="window.open('https://github.com/Rickyarians', '_blank')"),
      )
    )
  )
  
  
  ))


dashboardTitle <- dashboardHeader(
  title = "Adidas Sales Analysis"
)


dashboardPage(
  skin = 'black',
  dashboardTitle,
  sidebar,
  body
)