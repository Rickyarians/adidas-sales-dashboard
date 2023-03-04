#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)

# Define server logic required to draw a histogram
function(input, output, session) {

  # ----------------------------- Page 0 ----------------------------
  output$adidasLogo <- renderImage({  
    list(
      src = "assets/dataset-cover.jpg",
      contentType = "image/png",
      alt = "Logo",
      width = '100%'
    )
    
  }, deleteFile = FALSE)
  
  # ----------------------------- Page 0 ----------------------------
  
  # ----------------------------- Page 1 ----------------------------
  
  # Card 1 - Total Transaction All
  output$card_total_transaction_all = renderUI({
    total_trx_all <- data %>% filter(year_invoice <= input$select_year) %>% 
      group_by(year_invoice) %>% summarise(total_trx_all = n()) %>% arrange(-year_invoice) %>% head(2)
    
    if(nrow(total_trx_all) == 1) {
      html = glue("<span style='font-size: 16px'>Total Transaction in {input$select_year} </span><br/><span style='font-size: 14px'>(0%) Since last year</span>")
    } else {
      if(total_trx_all[1,]$ total_trx_all >=  total_trx_all[2,]$total_trx_all){
        html = glue("<span style='font-size: 16px'>Total Transaction in {input$select_year} </span><br/><span style='color: #3EFF8B; font-size: 14px;'>(+{round(((total_trx_all[1,]$total_trx_all/total_trx_all[2,]$total_trx_all * 100) - 100), 2)} %)</span> <span style='font-size: 14px'>Since last year</span> ")
      } else {
        html = glue("<span style='font-size: 16px'>Total Transaction in {input$select_year} </span><br/><span style='color: red; font-size: 14px;'>(-{round(((total_trx_all[1,]$total_trx_all/total_trx_all[2,]$total_trx_all * 100) - 100), 2)} %)</span> <span style='font-size: 14px'>Since last year</span> ")
      }
    }
    
    valueBox(glue("{comma(total_trx_all[1,]$total_trx_all, big.mark = '.')}"), 
             HTML(html), 
             icon = icon("file-invoice-dollar"), 
             color = 'aqua', 
             width = 4)
  })
  
  # Card 2 - Total Sales All
  output$card_total_sales_all = renderUI({
    total_sales_all <- data %>% select(`Total Sales`, year_invoice) %>%
      filter(year_invoice <= input$select_year) %>% 
      group_by(year_invoice) %>% summarise(total_sales_all = sum(`Total Sales`)) %>% arrange(-year_invoice) %>% head(2)
    
    
    if(nrow(total_sales_all) == 1) {
      html = glue("<span style='font-size: 16px'>Total Sales in {input$select_year} </span><br/><span style='font-size: 14px'>(0%) Since last year</span>")
    } else {
      if(total_sales_all[1,]$total_sales_all >= total_sales_all[2,]$total_sales_all){
        html = glue("<span style='font-size: 16px'>Total Sales in {input$select_year} </span><br/><span style='color: #3EFF8B; font-size: 14px;'>(+{round(((total_sales_all[1,]$total_sales_all/total_sales_all[2,]$total_sales_all * 100) - 100), 2)} %)</span> <span style='font-size: 14px'>Since last year</span> ")
      } else {
        html = glue("<span style='font-size: 16px'>Total Sales in {input$select_year} </span><br/><span style='color: red; font-size: 14px;'>(-{round(((total_sales_all[1,]$total_sales_all/total_sales_all[2,]$total_sales_all * 100) - 100), 2)} %)</span> <span style='font-size: 14px'>Since last year</span> ")
      }
    }
    valueBox(glue("{comma(total_sales_all[1,]$total_sales_all, big.mark = '.')} USD"), 
             HTML(html),
             icon = icon("money-bill"), 
             color = 'aqua', 
             width = 4)
  })
  
  # Card 3 - Total Unit Sold
  output$card_total_sold_all = renderUI({
      total_sold_all <- data %>% select(`Units Sold`, year_invoice) %>%
        filter(year_invoice <= input$select_year) %>% 
        group_by(year_invoice) %>% summarise(total_sold_all = sum(`Units Sold`)) %>% arrange(-year_invoice) %>% head(2)
      


      
      if(nrow(total_sold_all) == 1) {
         html = glue("<span style='font-size: 16px'>Total Unit Sold in {input$select_year} </span><br/><span style='font-size: 14px'>(0%) Since last year</span>")
      } else {
        if(total_sold_all[1,]$total_sold_all >= total_sold_all[2,]$total_sold_all){
          html = glue("<span style='font-size: 16px'>Total Unit Sold in {input$select_year} </span><br/><span style='color: #3EFF8B; font-size: 14px;'>(+{round(((total_sold_all[1,]$total_sold_all/total_sold_all[2,]$total_sold_all * 100) - 100), 2)} %)</span> <span style='font-size: 14px'>Since Last year</span> ")
        } else {
          html = glue("<span style='font-size: 16px'>Total Unit Sold in {input$select_year} </span><br/><span style='color: red; font-size: 14px;'>(-{round(((total_sold_all[1,]$total_sold_all/total_sold_all[2,]$total_sold_all * 100) - 100), 2)} %)</span> <span style='font-size: 14px'>Since Last year</span> ")
        }
      }
      
      return(
        valueBox(glue("{comma(total_sold_all[1,]$total_sold_all, big.mark = '.')} Item"), 
                 HTML(html),
                 icon = icon("cart-shopping"), 
                 color = 'aqua', 
                 width = 4)
      )
    
  })
  output$select_state = renderUI({
    data_state <- data_filter %>% 
      filter(Region %in% input$select_region)
    selectInput('select_state', 'Choose State', unique(data_state$State))
  })
  
  
  output$product_ranking_sales <- renderPlotly({
    data_sales_product <- data %>% group_by(Product, `Sales Method`) %>% 
      filter(year_invoice == input$select_year) %>% 
      summarise(total_product_sales = sum(`Total Sales`)) %>% 
      arrange(-total_product_sales) %>% 
      mutate(urutan = rank(x = total_product_sales, ties.method = "first"))
    
    data_sales_product <- data_sales_product %>% 
      mutate(label = glue(
        "Category Product: {Product}
       Total Sales Per Product: {comma(total_product_sales)} USD
       Sales Method: {`Sales Method`}"
      ))
    
    product_ranking_sales_plot <- ggplot(data = data_sales_product, aes(x = total_product_sales, 
                                                                        y = reorder(Product, total_product_sales), # reorder(A, berdasarkan B)                          
                                                                        text = label)) + # menambahkan tooltip dari glue
      geom_col(aes(fill = `Sales Method`, group=urutan, ), position = 'dodge') +
      scale_fill_manual(values =c("#00C0EF", "#0099C6","#00749F")) +
      scale_x_continuous(labels = label_number(big.mark = ".", suffix = " M USD", scale = 1e-6)) + 
      labs(title = NULL,
           x = "Total Sales",
           y = NULL) +
      theme_minimal() 
    
    ggplotly(product_ranking_sales_plot, tooltip = "text") %>% 
      style(hoverlabel = list(bgColor = "white", align="left")) %>% 
      layout(margin = 'm', title= list(text = glue("Top Total Sales", "<br>", "<sup>", "By Product & Sales method in {input$select_year} ", "</sup>"), y=0.9))
  })
  
  
  output$product_ranking_sold <- renderPlotly({
    data_sold_product <- data %>% group_by(Product, `Sales Method`) %>% 
      filter(year_invoice == input$select_year) %>% 
      summarise(total_product_sold = sum(`Units Sold`)) %>% 
      arrange(-total_product_sold)  %>% 
      mutate(urutan = rank(x = total_product_sold, ties.method = "first"))
    
    
    data_sold_product <-  data_sold_product %>% 
      mutate(label = glue(
        "Category Product: {Product}
         Total Sales Per Product: {comma(total_product_sold)} Unit
         Sales Method: {`Sales Method`}
        "
      ))
    
    
    product_ranking_sold_plot <- ggplot(data = data_sold_product, aes(x = total_product_sold, 
                                                                      y = reorder(Product, total_product_sold), # reorder(A, berdasarkan B)
                                                                      text = label)) + # menambahkan tooltip dari glue
      geom_col(aes(fill = `Sales Method`, group = urutan), position = "dodge") +
      scale_fill_manual(values =c("#00C0EF", "#0099C6","#00749F")) +
      scale_x_continuous(labels = label_number(big.mark = ".", suffix = "K Unit", scale = 1e-3)) + 
      labs(title = NULL,
           x = "Total Unit",
           y = NULL) +
      theme_minimal() 
    
    ggplotly(product_ranking_sold_plot, tooltip = "text")  %>% 
      style(hoverlabel = list(bgColor = "white", align="left")) %>% 
      layout(margin = 'm', title= list(text = glue("Top Unit Sold", "<br>", "<sup>", "By Product & Sales method in {input$select_year}", "</sup>"), y=0.9))
  })
  
  
  output$market_share_overview <- renderPlot({
    total_sales_2021 <- sum(data[data$year_invoice == input$select_year,]$`Total Sales`)
    
    market_share <- data %>% 
      filter(year_invoice == input$select_year) %>% 
      group_by(Retailer) %>% 
      summarise(percent = round(sum(`Total Sales`)/total_sales_2021 * 100)) %>% 
      mutate(label = glue("{Retailer} ({percent}%)"))
    
    
    
    pie(market_share$percent , 
        labels = market_share$label, 
        col = c("#30CEFD", "#00C0EF","#0099C6", "#00749F", "#005079", "#002F55"), main = glue("Market Share Retailer By Total Sales in {input$select_year}"))
  })
  
  output$scatter_plot <- renderPlotly({
    data_2021 <- data %>% filter(year_invoice == input$select_year) %>% 
      mutate(label = glue("Product Name : {Product}
                       Sales : {comma(`Total Sales`, big.mark='.', suffix = ' USD')}
                       Unit Sold : {comma(`Units Sold`, big.mark='.', suffix = ' Item')}
                       Sales Method : {`Sales Method`}
                       Region : {Region}
                       State : {State}
                       City : {City}
                       Retailer : {Retailer}
                      "))
    
    scatter_plot <- ggplot(data_2021, aes(x=`Total Sales`, y=`Units Sold`)) + 
      geom_point(mapping = aes(color = Product, text = label)) +
      scale_color_manual(values = c("#30CEFD", "#00C0EF","#0099C6", "#00749F", "#005079", "#002F55")) +
      geom_smooth(method=lm, color="black", se=TRUE) +
      scale_x_continuous(labels = label_number(big.mark = ".", suffix = " USD")) +
      scale_y_continuous(labels = label_number(big.mark = ".", suffix = " Item"))  
    
    ggplotly(scatter_plot, tooltip = "text") %>% 
      style(hoverlabel = list(bgColor = "white", align="left")) %>% 
      layout(margin = 'm', title= list(text = glue("Relation Between Total Sales & Units Sold", "<br>", "<sup>", "in {input$select_year}", "</sup>"), y=0.9))
  })
  
  
  output$history_sales_month <- renderPlotly({
    data_line_year <- data %>% 
      filter(year_invoice == input$select_year) %>% 
      group_by(month_invoice, Product) %>% 
      summarise(total_sales_line = sum(`Total Sales`), total_sold_unit = sum(`Units Sold`)) %>% 
      ungroup() %>% 
      arrange(-total_sales_line) %>% 
      mutate(label3 = glue("Total Unit Sold: {comma(total_sold_unit)}
                       Total Sales : {comma(total_sales_line)}
                       "))
    
    # Pembuatan plot statis 3
    plot3 <- ggplot(data = data_line_year, 
                    mapping = aes(x = month_invoice, 
                                  y = total_sales_line, color = Product)) +
      geom_line() + 
      geom_point(aes(text = label3)) +
      scale_y_continuous(labels = label_number(big.mark = ".", suffix = " USD"), breaks = seq(1000000, 20000000, 1000000)) + 
      scale_x_continuous(breaks = seq(1, 12, 1), labels = c("Jan", 
                                                            "Feb", 
                                                            "March", 
                                                            "April", 
                                                            "May",
                                                            "June",
                                                            "July",
                                                            "Aug",
                                                            "Sept",
                                                            "Oct",
                                                            "Nov",
                                                            "Des"
      ))+
      theme_minimal() +labs(title = NULL,
                            x = "Month",
                            y = NULL) 
    
    
    # Pembuatan plot interaktif 2
    ggplotly(plot3, tooltip = "text") %>% 
      style(hoverlabel = list(bgColor = "white", align="left")) %>% 
      layout(margin = 'm', title= list(text = glue("Sales Activity in {input$select_year}", "<br>", "<sup>", "based on total sales / month", "</sup>"), y=0.9))
  })
  
  
  # ----------------------------- Page 1 ----------------------------
  
  
  # ----------------------------- Page 2 ----------------------------
  output$select_city = renderUI({
    data_city <- data_filter %>% 
      filter(Region %in% input$select_region, State %in% input$select_state)
    selectInput('select_city', 'Choose City', unique(data_city$City))
  })
  
  output$total_transaction_location = renderUI({
    total_transaction_by_loc <- data %>% 
      filter(Region %in% input$select_region, 
             State %in% input$select_state, 
             City %in% input$select_city,
             year_invoice <= max(year_invoice)
             ) %>%  
      group_by(year_invoice) %>% summarise(total_transaction = n()) %>% arrange(-year_invoice) %>% head(2)
  
    if(nrow(total_transaction_by_loc) == 1) {
      html = glue("<span style='font-size: 16px'>Total Transaction in {max(total_transaction_by_loc$year_invoice)} </span><br/><span style='font-size: 14px'>(0%) Since last year</span>")
    } else {
      if(total_transaction_by_loc[1,]$total_transaction >= total_transaction_by_loc[2,]$total_transaction){
        html = glue("<span style='font-size: 16px'>Total Transaction in {max(total_transaction_by_loc$year_invoice)} </span><br/><span style='color: #3EFF8B; font-size: 14px;'>(+{round(((total_transaction_by_loc [1,]$total_transaction /total_transaction_by_loc[2,]$total_transaction  * 100) - 100), 2)} %)</span> <span style='font-size: 14px'>Since Last year</span> ")
      } else {
        html = glue("<span style='font-size: 16px'>Total Transaction in {max(total_transaction_by_loc$year_invoice)} </span><br/><span style='color: red; font-size: 14px;'>(-{round(((total_transaction_by_loc [1,]$total_transaction /total_transaction_by_loc[2,]$total_transaction  * 100) - 100), 2)} %)</span> <span style='font-size: 14px'>Since Last year</span> ")
      }
    }
    
    valueBox(glue("{comma(total_transaction_by_loc[1, ]$total_transaction, big.mark = '.')}"), 
             HTML(html), 
             color = 'aqua',
             width = 3,
             icon = icon("file-invoice-dollar"))
  })
  
  
  output$total_sold_location = renderUI({
    total_sold_by_loc <- data %>% 
      filter(Region %in% input$select_region, 
             State %in% input$select_state, 
             City %in% input$select_city,
             year_invoice <= max(year_invoice)
      ) %>%  
      group_by(year_invoice) %>% 
      summarise(total_unit_sold = sum(`Units Sold`)) %>% 
      arrange(-year_invoice) %>% 
      head(2)
    # total_sold_result <- sum(total_sold_by_loc$`Units Sold`)
    
    print(total_sold_by_loc)
    if(nrow(total_sold_by_loc) == 1) {
      html = glue("<span style='font-size: 16px'>Total Unit Sold in {max(total_sold_by_loc$year_invoice)} </span><br/><span style='font-size: 14px'>(0%) Since last year</span>")
    } else {
      if(total_sold_by_loc[1,]$total_unit_sold >= total_sold_by_loc[2,]$total_unit_sold){
        html = glue("<span style='font-size: 16px'>Total Unit Sold in {max(total_sold_by_loc$year_invoice)} </span><br/><span style='color: #3EFF8B; font-size: 14px;'>(+{round(((total_sold_by_loc[1,]$total_unit_sold /total_sold_by_loc[2,]$total_unit_sold  * 100) - 100), 2)} %)</span> <span style='font-size: 14px'>Since Last year</span> ")
      } else {
        html = glue("<span style='font-size: 16px'>Total Unit Sold in {max(total_sold_by_loc$year_invoice)} </span><br/><span style='color: red; font-size: 14px;'>(-{round(((total_sold_by_loc[1,]$total_unit_sold /total_sold_by_loc[2,]$total_unit_sold  * 100) - 100), 2)} %)</span> <span style='font-size: 14px'>Since Last year</span> ")
      }
    }
  
    valueBox(glue("{comma(total_sold_by_loc[1,]$total_unit_sold, big.mark='.', suffix = ' Item')}"), 
             HTML(html), 
             color = 'aqua',
             width = 3,
             icon = icon("cart-shopping"))
  })
  
  
  
  output$total_sales_location = renderUI({
    total_sales_by_loc <- data %>% 
      filter(Region %in% input$select_region, 
             State %in% input$select_state, 
             City %in% input$select_city,
             year_invoice <= max(year_invoice)
      ) %>%  
      group_by(year_invoice) %>% 
      summarise(total_sales = sum(`Total Sales`)) %>% 
      arrange(-year_invoice) %>% 
      head(2)
    
  
    if(nrow(total_sales_by_loc) == 1) {
      html = glue("<span style='font-size: 16px'>Total Sales in {max(total_sales_by_loc$year_invoice)} </span><br/><span style='font-size: 14px'>(0%) Since last year</span>")
    } else {
      if(total_sales_by_loc[1,]$total_sales >= total_sales_by_loc[2,]$total_sales){
        html = glue("<span style='font-size: 16px'>Total Sales in {max(total_sales_by_loc$year_invoice)} </span><br/><span style='color: #3EFF8B; font-size: 14px;'>(+{round(((total_sales_by_loc[1,]$total_sales /total_sales_by_loc[2,]$total_sales  * 100) - 100), 2)} %)</span> <span style='font-size: 14px'>Since Last year</span> ")
      } else {
        html = glue("<span style='font-size: 16px'>Total Sales in {max(total_sales_by_loc$year_invoice)} </span><br/><span style='color: red; font-size: 14px;'>(-{round(((total_sales_by_loc[1,]$total_sales /total_sales_by_loc[2,]$total_sales  * 100) - 100), 2)} %)</span> <span style='font-size: 14px'>Since Last year</span> ")
      }
    }
    
    req(total_sales_by_loc)
    valueBox(glue("{comma(total_sales_by_loc[1,]$total_sales, big.mark='.', suffix = ' USD')}"), 
              HTML(html), 
              color = 'aqua',
              width = 3,
              icon = icon("money-bill"))
  })
  
  output$total_operating_profit = renderUI({
    total_profit_by_loc <- data %>% 
      filter(Region %in% input$select_region, 
             State %in% input$select_state, 
             City %in% input$select_city,
             year_invoice <= max(year_invoice)
      ) %>%  
      group_by(year_invoice) %>% 
      summarise(total_profit = sum(`Operating Profit`)) %>% 
      arrange(-year_invoice) %>% 
      head(2)
    
    req(total_profit_by_loc)
    if(nrow(total_profit_by_loc) == 1) {
      html = glue("<span style='font-size: 16px'>Total Profit in {max(total_profit_by_loc$year_invoice)} </span><br/><span style='font-size: 14px'>(0%) Since last year</span>")
    } else {
      if(total_profit_by_loc[1,]$total_profit >= total_profit_by_loc[2,]$total_profit){
        html = glue("<span style='font-size: 16px'>Total Profit in {max(total_profit_by_loc$year_invoice)} </span><br/><span style='color: #3EFF8B; font-size: 14px;'>(+{round(((total_profit_by_loc[1,]$total_profit /total_profit_by_loc[2,]$total_profit  * 100) - 100), 2)} %)</span> <span style='font-size: 14px'>Since Last year</span> ")
      } else {
        html = glue("<span style='font-size: 16px'>Total Profit in {max(total_profit_by_loc$year_invoice)} </span><br/><span style='color: red; font-size: 14px;'>(-{round(((total_profit_by_loc[1,]$total_profit /total_profit_by_loc[2,]$total_profit  * 100) - 100), 2)} %)</span> <span style='font-size: 14px'>Since Last year</span> ")
      }
    }
    
    valueBox(glue("{comma(total_profit_by_loc[1,]$total_profit, big.mark='.', suffix = ' USD')}"), 
             HTML(html), 
             color = 'aqua',
             width = 3,
             icon = icon("money-bill"))
  })
  
  
  output$heatmap_sales <- renderLeaflet({
    m <- leaflet(states) %>%
      setView(-96, 37.8, 4) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))
    
    data_map1 <- data %>% group_by(State, density) %>% summarise(total_sales_state = sum(`Total Sales`))
    bins <- c(5000000,10000000,15000000,20000000,25000000,Inf)
    pal <- colorBin(c("#78C2AD", "#519A86","#297462", "#00503F", "#002E20"), domain = data_map1$total_sales_state, bins = bins)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%s</sup>",
      data_map1$State, glue("{comma(data_map1$total_sales_state)} USD")
    ) %>% lapply(htmltools::HTML)
    
    m %>% addPolygons(
      fillColor = ~pal(data_map1$total_sales_state),
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.5,
      highlightOptions = highlightOptions(
        weight = 2,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.1,
        bringToFront = TRUE),
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto")) %>% addLegend(pal = pal, values = ~density, opacity = 0.7, title = NULL,
                                           position = "bottomright")
  })
  
  
  # ----------------------------- Page 5 ----------------------------
  output$datatable <- DT::renderDataTable({
    x <- DT::datatable(
      data, width = 300,
      options = list(scrollX = TRUE)
    )
    x
  })
  
  
  
  output$heatmap_sales <- renderLeaflet({
    data_map1 <- data %>%  filter(between(`Invoice Date`, as.Date(input$date_range[1]), as.Date(input$date_range[2]))) %>% group_by(State) %>% summarise(total_sales_state = sum(`Total Sales`))
    data_baru <- merge(y = data_map1, x= states, by.y = 'State', by.x = 'name')
    m <- leaflet(data_baru) %>%
      setView(-96, 37.8, 4) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))
    
  
    bins <- c(0,5000000,10000000,15000000,20000000,Inf)
    pal <- colorBin(c("#30CEFD", "#00C0EF","#0099C6", "#00749F", "#005079", "#002F55"),  domain =  data_baru$total_sales_state, bins = bins)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%s</sup>",
      data_baru$name, glue("{comma(data_baru$total_sales_state)} USD")
    ) %>% lapply(htmltools::HTML)
    
    m %>% addPolygons(
      fillColor = ~pal(data_baru$total_sales_state),
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.5,
      highlightOptions = highlightOptions(
        weight = 2,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.1,
        bringToFront = TRUE),
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto")) %>% addLegend(pal = pal, values = ~total_sales_state, opacity = 0.7, title = NULL,
                                           position = "bottomright")
  })
  
  output$heatmap_unit_sold <- renderLeaflet({
    data_map1 <- data %>%  filter(between(`Invoice Date`, as.Date(input$date_range[1]), as.Date(input$date_range[2]))) %>% group_by(State) %>% summarise(total_unit_sold = sum(`Units Sold`))
    data_baru <- merge(y = data_map1, x= states, by.y = 'State', by.x = 'name')
     m <- leaflet(data_baru) %>%
      setView(-96, 37.8, 4) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))
    
    
    bins <- c(0,25000, 50000, 75000,125000,Inf)
    pal <- colorBin(c("#30CEFD", "#00C0EF","#0099C6", "#00749F", "#005079", "#002F55"), domain =  data_baru$total_unit_sold, bins = bins)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%s</sup>",
      data_baru$name, glue("{comma(data_baru$total_unit_sold)} Item")
    ) %>% lapply(htmltools::HTML)
    
    m %>% addPolygons(
      fillColor = ~pal(data_baru$total_unit_sold),
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.5,
      highlightOptions = highlightOptions(
        weight = 2,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.1,
        bringToFront = TRUE),
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto")) %>% addLegend(pal = pal, values = ~total_unit_sold, opacity = 0.7, title = NULL,
                                           position = "bottomright")
  })
  
  
  output$heatmap_ranking_sales <- renderPlotly({
    data_sales_product <- data %>% group_by(Product) %>% 
      filter(between(`Invoice Date`, as.Date(input$date_range[1]), as.Date(input$date_range[2]))) %>% 
      summarise(total_product_sales = sum(`Total Sales`)) %>% 
      arrange(-total_product_sales) %>% 
      head(input$slider_input_heatmap)
    
    data_sales_product <- data_sales_product %>% 
      mutate(label = glue(
        "Category Product: {Product}
       Total Sales Per Product: {comma(total_product_sales)} USD"
      ))
    
    product_ranking_sales_plot <- ggplot(data = data_sales_product, aes(x = total_product_sales, 
                                                                        y = reorder(Product, total_product_sales), # reorder(A, berdasarkan B)                          
                                                                        text = label)) + # menambahkan tooltip dari glue
      geom_col(aes(fill = total_product_sales)) +
      scale_x_continuous(labels = label_number(big.mark = ".", suffix = " M USD", scale = 1e-6)) + 
      labs(title = NULL,
           x = "Total Sales",
           y = NULL) +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(product_ranking_sales_plot, tooltip = "text") %>% 
      style(hoverlabel = list(bgColor = "white", align="left")) %>% 
      layout(margin = 'm', title= list(text = glue("Top Total Sales", "<br>", "<sup>", "By Product between {input$date_range[1]} to {input$date_range[2]} ", "</sup>"), y=0.9))
  })
  
  
  output$heatmap_ranking_sold <- renderPlotly({
    data_sold_product <- data %>% group_by(Product) %>% 
      filter(between(`Invoice Date`, as.Date(input$date_range[1]), as.Date(input$date_range[2]))) %>% 
      summarise(total_product_sold = sum(`Units Sold`)) %>% 
      arrange(-total_product_sold) %>% 
      head(input$slider_input_heatmap)
    
    
    data_sold_product <-  data_sold_product %>% 
      mutate(label = glue(
        "Category Product: {Product}
         Total Sales Per Product: {comma(total_product_sold)} Unit
        "
      ))
    
    
    product_ranking_sold_plot <- ggplot(data = data_sold_product, aes(x = total_product_sold, 
                                                                      y = reorder(Product, total_product_sold), # reorder(A, berdasarkan B)
                                                                      text = label)) + # menambahkan tooltip dari glue
      geom_col(aes(fill = total_product_sold)) +
      scale_x_continuous(labels = label_number(big.mark = ".", suffix = "K Unit", scale = 1e-3)) + 
      labs(title = NULL,
           x = "Total Unit",
           y = NULL) +
      theme_minimal() +
        theme(legend.position = "none")
    
    ggplotly(product_ranking_sold_plot, tooltip = "text")  %>% 
      style(hoverlabel = list(bgColor = "white", align="left")) %>% 
      layout(margin = 'm', title= list(text = glue("Top Unit Sold", "<br>", "<sup>", "By Product between {input$date_range[1]} to {input$date_range[2]}", "</sup>"), y=0.9))
  })
  
  
  output$rank_start_city <- renderUI({
    
    year <- max( data[data$City == input$select_city, ]$year_invoice)
    rank_sales <- data %>% filter(year_invoice == year) %>% group_by(City) %>%  
      summarise(total_sales_city = sum(`Total Sales`)) %>% 
      arrange(-total_sales_city)
    
  
    
    valueBox(glue("Rank # {which(rank_sales$City == input$select_city)} "), 
             HTML(
               glue("<span>From {nrow(rank_sales)} City in {year} </span> </br>
                     <span> By Total Sales </span>
                    ")
             ),
             color = "green",
             icon = icon("ranking-star"), 
             width = 2)
  })
  
  output$market_share_overview_location <- renderPlot({
   if(input$radio_value_select == 'Total Sales') {
     total_sales_max <- data %>% filter(Region %in% input$select_region, 
                                        State %in% input$select_state, 
                                        City %in% input$select_city,
                                        year_invoice <= max(year_invoice)
     ) %>%  group_by(year_invoice) %>% summarise(total_sales = sum(`Total Sales`))
     
     print(total_sales_max)
     market_share <- data %>% 
       filter(Region %in% input$select_region, 
              State %in% input$select_state, 
              City %in% input$select_city,
              year_invoice == total_sales_max[1, ]$year_invoice 
       ) %>%
       group_by(Retailer) %>% 
       summarise(percent = round(sum(`Total Sales`)/total_sales_max[1,]$total_sales * 100))  %>% 
       mutate(label = glue("{Retailer} ({percent}%)"))
     
   } else {
     total_sales_max <- data %>% filter(Region %in% input$select_region, 
                                        State %in% input$select_state, 
                                        City %in% input$select_city,
                                        year_invoice <= max(year_invoice)
     ) %>%  group_by(year_invoice) %>% summarise(total_sales = sum(`Units Sold`))
     
     print(total_sales_max)
     market_share <- data %>% 
       filter(Region %in% input$select_region, 
              State %in% input$select_state, 
              City %in% input$select_city,
              year_invoice == total_sales_max[1, ]$year_invoice 
       ) %>%
       group_by(Retailer) %>% 
       summarise(percent = round(sum(`Units Sold`)/total_sales_max[1,]$total_sales * 100))  %>% 
       mutate(label = glue("{Retailer} ({percent}%)"))
   }
    

    pie(market_share$percent , 
        labels = market_share$label, 
        col = c("#30CEFD", "#00C0EF","#0099C6", "#00749F", "#005079", "#002F55"), main = glue("Market Share Retailer By {input$radio_value_select} in {total_sales_max[1, ]$year_invoice}"))
  })
  
  
  output$sales_performance <- renderPlotly({
    year <- max( data[data$City == input$select_city, ]$year_invoice)
   
  
      data_line_year <- data %>% 
        filter(Region %in% input$select_region, 
                         State %in% input$select_state, 
                         City %in% input$select_city,year_invoice == year) %>% 
        group_by(month_invoice, Product) %>% 
        summarise(total_sales_line = sum(`Total Sales`), total_sold_unit = sum(`Units Sold`)) %>% 
        ungroup() %>% 
        arrange(-total_sales_line) %>% 
        mutate(label3 = glue("Total Unit Sold: {comma(total_sold_unit, big.mark = '.', suffix = ' Item')}
                       Total Sales : {comma(total_sales_line, big.mark = '.', suffix = ' USD')}
                       "))

      data_line_year <- data %>% 
        filter(Region %in% input$select_region, 
               State %in% input$select_state, 
               City %in% input$select_city,
               year_invoice == year) %>% 
        group_by(month_invoice, Product) %>% 
        summarise(total_sales_line = sum(`Total Sales`), total_sold_unit = sum(`Units Sold`)) %>% 
        ungroup() %>% 
        arrange(-total_sales_line) %>% 
        mutate(label3 = glue("Total Unit Sold: {comma(total_sold_unit, big.mark = '.', suffix = ' Item')}
                       Total Sales : {comma(total_sales_line, big.mark = '.', suffix = ' USD')}
                       "))
  
      if(input$radio_value_select1 == 'Total Sales') {
    # Pembuatan plot statis 3
    plot3 <- ggplot(data = data_line_year, 
                    mapping = aes(x = month_invoice, 
                                  y = total_sales_line, color = Product)) +
      geom_line() + 
      geom_point(aes(text = label3)) +
      scale_y_continuous(labels = label_number(big.mark = ".", suffix = " USD")) + 
      scale_x_continuous(breaks = seq(1, 12, 1), labels = c("Jan", 
                                                            "Feb", 
                                                            "March", 
                                                            "April", 
                                                            "May",
                                                            "June",
                                                            "July",
                                                            "Aug",
                                                            "Sept",
                                                            "Oct",
                                                            "Nov",
                                                            "Des"
      ))+
      theme_minimal() +labs(title = NULL,
                            x = "Month",
                            y = NULL) 
    
      } else {
        plot3 <- ggplot(data = data_line_year, 
                        mapping = aes(x = month_invoice, 
                                      y = total_sold_unit, color = Product)) +
          geom_line() + 
          geom_point(aes(text = label3)) +
          scale_y_continuous(labels = label_number(big.mark = ".", suffix = " Unit")) + 
          scale_x_continuous(breaks = seq(1, 12, 1), labels = c("Jan", 
                                                                "Feb", 
                                                                "March", 
                                                                "April", 
                                                                "May",
                                                                "June",
                                                                "July",
                                                                "Aug",
                                                                "Sept",
                                                                "Oct",
                                                                "Nov",
                                                                "Des"
          ))+
          theme_minimal() +labs(title = NULL,
                                x = "Month",
                                y = NULL) 
      }
    # Pembuatan plot interaktif 2
    ggplotly(plot3, tooltip = "text") %>% 
      style(hoverlabel = list(bgColor = "white", align="left")) %>% 
      layout(margin = 'm', title= list(text = glue("Sales Performance in {year}", "<br>", "<sup>", "based on {input$radio_value_select1} / month", "</sup>"), y=0.9))
  })
  
  
  output$sales_ranking <- renderPlotly({
    year <- max( data[data$City == input$select_city, ]$year_invoice)
    if(input$radio_value_select2 == "Total Sales") {
      data_sales_product <- data %>% group_by(Product, `Sales Method`) %>% 
        filter(Region %in% input$select_region, 
               State %in% input$select_state, 
               City %in% input$select_city,year_invoice == year) %>% 
        summarise(total_product_sales = sum(`Total Sales`)) %>% 
        arrange(-total_product_sales) %>% 
        mutate(urutan = rank(x = total_product_sales, ties.method = "first"))
      
      data_sales_product <- data_sales_product %>% 
        mutate(label = glue(
          "Category Product: {Product}
        Total Sales Per Product: {comma(total_product_sales, big.mark = '.')} USD
        Sales Method: {`Sales Method`}"
        ))
      
      product_ranking_sales_plot <- ggplot(data = data_sales_product, aes(x = total_product_sales, 
                                                                          y = reorder(Product, total_product_sales), # reorder(A, berdasarkan B)                          
                                                                          text = label)) + # menambahkan tooltip dari glue
        geom_col(aes(fill = `Sales Method`, group=urutan, ), position = 'dodge') +
        scale_fill_manual(values =c("#00C0EF", "#0099C6","#00749F")) +
        scale_x_continuous(labels = label_number(big.mark = ".", suffix = " M USD", scale = 1e-6)) + 
        labs(title = NULL,
             x = "Total Sales",
             y = NULL) +
        theme_minimal() 
      
        ggplotly(product_ranking_sales_plot, tooltip = "text") %>% 
        style(hoverlabel = list(bgColor = "white", align="left")) %>% 
        layout(margin = 'm', title= list(text = glue("Top {input$radio_value_select2}", "<br>", "<sup>", "By Product & Sales method in {year} ", "</sup>"), y=0.9))
    } else {
      data_sold_product <- data %>% group_by(Product, `Sales Method`) %>% 
        filter(Region %in% input$select_region, 
               State %in% input$select_state, 
               City %in% input$select_city,year_invoice == year) %>% 
        summarise(total_product_sold = sum(`Units Sold`)) %>% 
        arrange(-total_product_sold)  %>% 
        mutate(urutan = rank(x = total_product_sold, ties.method = "first"))
      
      
      data_sold_product <-  data_sold_product %>% 
        mutate(label = glue(
          "Category Product: {Product}
         Total Sales Per Product: {comma(total_product_sold)} Unit
         Sales Method: {`Sales Method`}
        "
        ))
      
      
      product_ranking_sold_plot <- ggplot(data = data_sold_product, aes(x = total_product_sold, 
                                                                        y = reorder(Product, total_product_sold), # reorder(A, berdasarkan B)
                                                                        text = label)) + # menambahkan tooltip dari glue
        geom_col(aes(fill = `Sales Method`, group = urutan), position = "dodge") +
        scale_fill_manual(values =c("#00C0EF", "#0099C6","#00749F")) +
        scale_x_continuous(labels = label_number(big.mark = ".", suffix = "K Unit", scale = 1e-3)) + 
        labs(title = NULL,
             x = "Total Unit",
             y = NULL) +
        theme_minimal() 
      
      ggplotly(product_ranking_sold_plot, tooltip = "text")  %>% 
        style(hoverlabel = list(bgColor = "white", align="left")) %>% 
        layout(margin = 'm', title= list(text = glue("Top {input$radio_value_select2}", "<br>", "<sup>", "By Product & Sales method in {year}", "</sup>"), y=0.9))
    }
   
  })
  
  output$sales_characterisctic <- renderPlotly({
    year <- max(data[data$City == input$select_city, ]$year_invoice)
    p <- data %>% 
      filter(Region %in% input$select_region, 
             State %in% input$select_state, 
             City %in% input$select_city,
             year_invoice == year)
    
    plothist <- ggplot(p, aes(x=`Price per Unit`)) +
      geom_histogram(binwidth = 5, fill="#0099C6", color = 'black') +
      theme_minimal() +
      labs(
        y = "Frequency",
        x = "Price Per Unit (USD)"
      ) +
      theme(
        plot.title = element_text(size=15)
      )
    
    ggplotly(plothist)
  })
  


 
}
