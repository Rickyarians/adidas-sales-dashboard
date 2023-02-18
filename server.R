#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
function(input, output, session) {
  
  output$select_state = renderUI({
    data_state <- data_filter %>% 
      filter(Region %in% input$select_region)
    selectInput('select_state', 'Choose State', unique(data_state$State))
  })
  
  output$select_city = renderUI({
    data_city <- data_filter %>% 
      filter(Region %in% input$select_region, State %in% input$select_state)
    selectInput('select_city', 'Choose City', unique(data_city$City))
  })
  
  output$total_transaction_location = renderUI({
    total_transaction_by_loc <- data %>% 
      filter(Region %in% input$select_region, 
             State %in% input$select_state, 
             City %in% input$select_city)
    valueBox(nrow(total_transaction_by_loc), 
             "Total Trannsaction", 
             color = 'aqua',
             width = 4,
             icon = icon("file-invoice-dollar"))
  })
  
  output$total_sold_location = renderUI({
    total_sold_by_loc <- data %>% 
      filter(Region %in% input$select_region, 
             State %in% input$select_state, 
             City %in% input$select_city)
    total_sold_result <- sum(total_sold_by_loc$`Units Sold`)
    valueBox( glue(" {total_sold_result} Item"), 
             "Total Sold Item", 
             color = 'aqua',
             width = 4,
             icon = icon("cart-shopping"))
  })
  
  
  
  output$total_sales_location = renderUI({
    total_sales_by_loc <- data %>% 
      filter(Region %in% input$select_region, 
             State %in% input$select_state, 
             City %in% input$select_city)
    
    total_sales_by_loc_total <- sum(total_sales_by_loc$`Total Sales`)
    valueBox(glue("USD {comma(total_sales_by_loc_total)}"), 
              "Total Sales", 
              color = 'aqua',
              width = 4,
              icon = icon("money-bill"))
  })
  
  
  
  output$datatable <- DT::renderDataTable({
    x <- DT::datatable(
      data, width = 300,
      options = list(scrollX = TRUE)
    )
    x
  })
  
  output$adidasLogo <- renderImage({  
    list(
      src = "assets/dataset-cover.jpg",
      contentType = "image/png",
      alt = "Logo",
      width = '100%',
      height = 450
    )
    
    }, deleteFile = FALSE)
  
  
  output$market_share_pie <- renderPlot({
    market_share <- data %>% 
      filter(Region %in% input$select_region, 
                                    State %in% input$select_state, 
                                    City %in% input$select_city ) %>% 
      group_by(Retailer) %>% 
      summarise(total_sales_retailer = sum(`Total Sales`)) %>% 
      arrange(-total_sales_retailer)
    
    total_sales <- sum(market_share$total_sales_retailer)
    market_share <- market_share %>% 
      mutate(prop = round(total_sales_retailer / sum(total_sales) *100, 2)) 
    
    ggplot(market_share, aes(x = "", y = prop, fill = Retailer)) +
      geom_col(color = "black") +
      geom_text(aes(label = glue("{prop} %")),
                size = 2.5,
                position = position_stack(vjust = 0.5)) +
      coord_polar(theta = "y") +
      scale_fill_brewer() +
      theme(axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            panel.grid = element_blank(),
            panel.background = element_rect(fill = "#ffffff"),
            plot.background = element_rect(fill = "#ffffff"),
            legend.background = element_rect(fill = "#ffffff"))
  })
  
  
  output$market_share_ranking_retailer <- renderPlotly({
    market_share_bar <- data %>% 
      filter(Region %in% input$select_region, 
             State %in% input$select_state, 
             City %in% input$select_city ) %>% 
      group_by(Retailer) %>% 
      summarise(total_sales_retailer = sum(`Total Sales`)) %>% 
      arrange(-total_sales_retailer)
    
    market_share_bar  <- market_share_bar %>% 
      mutate(label = glue(
        "Retailer: {Retailer}
        Total Sales Per Retailer: {total_sales_retailer} Unit"
      ))
    
    
    
    plot1 <- ggplot(data = market_share_bar, aes(x = total_sales_retailer, 
                                           y = reorder(Retailer, total_sales_retailer), # reorder(A, berdasarkan B)
                                           text = label,)) + # menambahkan tooltip dari glue
      geom_col(aes(fill = total_sales_retailer, )) +
      scale_x_continuous(labels = label_number(big.mark = ".", suffix = " Unit")) + 
      labs(title = "Adidas Unit Sold Per Product Categories",
           x = "Total Unit Sold",
           y = NULL) +
      theme_minimal() +
      theme(legend.position = "none") 
    
    ggplotly(plot1, tooltip = "text")
  })
  
  
  
  output$map_sales <- renderLeaflet({
    m <- leaflet(states) %>%
      setView(-96, 37.8, 4) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))
    
    data_map1 <- data_map %>% group_by(State, density) %>% summarise(total_sales_state = sum(`Total Sales`))
    bins <- c(5000000,10000000,15000000,20000000,25000000,Inf)
    pal <- colorBin("Blues", domain = data_map1$total_sales_state, bins = bins)
    
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
  
  
  
  output$product_ranking_sales <- renderPlotly(({
    data_sales_product <- data %>% group_by(Product, `Sales Method`) %>% 
      summarise(total_product_sales = sum(`Total Sales`)) %>% 
      arrange(-total_product_sales)
    
    
    data_sales_product <- data_sales_product %>% 
      mutate(label = glue(
     "Category Product: {Product}
       Total Sales Per Product: {comma(total_product_sales)} USD
       Sales Method: {`Sales Method`}"
      ))
    
    
    product_ranking_sales_plot <- ggplot(data = data_sales_product, aes(x = total_product_sales, 
                                                   y = reorder(Product, total_product_sales), # reorder(A, berdasarkan B)
                                                   text = label)) + # menambahkan tooltip dari glue
      geom_col(aes(fill = `Sales Method`, ), position = 'dodge') +
      scale_fill_manual(values = c("#97C4E0", "#71A6D0", "#5A84B8")) +
      scale_x_continuous(labels = label_number(big.mark = ".", suffix = " USD")) + 
      labs(title = NULL,
           x = "Total Sales",
           y = NULL) +
      theme_minimal() 
    
    ggplotly(product_ranking_sales_plot, tooltip = "text")
  }))
  
  
  output$product_ranking_sold <- renderPlotly(({
    data_sold_product <- data %>% group_by(Product, `Sales Method`) %>% 
      summarise(total_product_sold = sum(`Units Sold`)) %>% 
      arrange(-total_product_sold)
    
    
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
      geom_col(aes(fill = `Sales Method`,), position = "dodge") +
      scale_fill_manual(values = c("#97C4E0", "#71A6D0", "#5A84B8")) +
      scale_x_continuous(labels = label_number(big.mark = ".", suffix = " Unit")) + 
      labs(title = NULL,
           x = "Total Unit",
           y = NULL) +
      theme_minimal() 
    
    ggplotly(product_ranking_sold_plot, tooltip = "text")
  }))
  
  
  output$history_sales_month <- renderPlotly({
    data_line_year <- data %>% 
      filter(year_invoice == input$select_year) %>% 
      group_by(month_invoice, Product) %>% 
      summarise(total_sales_line = sum(`Total Sales`), total_sold_unit = sum(`Units Sold`)) %>% 
      ungroup() %>% 
      arrange(-total_sales_line) %>% 
      mutate(label3 = glue("Month Invoice: {month_invoice}
                       Total Unit Sold: {comma(total_sold_unit)}
                       Total Sales : {comma(total_sales_line)}
                       "))
    
    data_line_year
    
    # Pembuatan plot statis 3
    plot3 <- ggplot(data = data_line_year, 
                    mapping = aes(x = month_invoice, 
                                  y = total_sales_line, color = Product)) +
      geom_line() + 
      geom_point(aes(text = label3)) +
      scale_y_continuous(labels = label_number(big.mark = ".", suffix = " USD"), breaks = seq(1000000, 20000000, 1000000)) + 
      scale_x_continuous(breaks = seq(1, 12, 1))+
      labs(
        title = glue("Sales Activity of By Product in {input$select_year}"),
        x = "Month",
        y = "Total Sales"
      ) +
      theme_minimal()
    
    
    # Pembuatan plot interaktif 2
    ggplotly(plot3, tooltip = "text")
  })
  
}
