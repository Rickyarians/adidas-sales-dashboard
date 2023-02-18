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
    data_state <- data_filter %>% filter(Region %in% input$select_region)
    print(length(levels(data_state$State)))
    selectInput('select_state', 'Choose State', unique(data_state$State))
  })
  
  output$select_city = renderUI({
    data_city <- data_filter %>% filter(Region %in% input$select_region, State %in% input$select_state)
    print(length(levels(data_city$City)))
    selectInput('select_city', 'Choose City', unique(data_city$City))
  })
  
  output$total_transaction_location = renderUI({
    total_transaction_by_loc <- data %>% 
      filter(Region %in% input$select_region, 
             State %in% input$select_state, 
             City %in% input$select_city)
    infoBox("TOTAL TRANSACTION", nrow(total_transaction_by_loc), icon = icon("shopping-cart"), color = 'red', width = 4)
  })
  
  output$total_sold_location = renderUI({
    total_sold_by_loc <- data %>% 
      filter(Region %in% input$select_region, 
             State %in% input$select_state, 
             City %in% input$select_city)
    total_sold_result <- sum(total_sold_by_loc$`Units Sold`)
    infoBox("TOTAL PRODUCT SOLD", glue(" {total_sold_result} Item"), icon = icon("headset"), color = 'black', width = 4)
  })
  
  
  
  output$total_sales_location = renderUI({
    total_sales_by_loc <- data %>% 
      filter(Region %in% input$select_region, 
             State %in% input$select_state, 
             City %in% input$select_city)
    
    total_sales_by_loc_total <- sum(total_sales_by_loc$`Total Sales`)
    infoBox("TOTAL SALES", glue("USD {comma(total_sales_by_loc_total)}"), icon = icon("money-bill"), color = 'black', width = 4)
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
    
    total_sales <- sum(data$`Total Sales`)
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

}
