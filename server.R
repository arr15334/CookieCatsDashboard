library(shiny)
library(RColorBrewer)
library(ggplot2)
source('helpers.R')



shinyServer(function(input, output) {
  
  output$usersplit <- renderPlot({
    ds.control <- unique(ds$userid[ds$version == 'gate_30'])
    ds.treatment <- unique(ds$userid[ds$version == 'gate_40'])
    data <- data.frame(
      list(users=c(length(ds.control),
                   length(ds.treatment)),
           user.type=c('Control', 'Treatment')))
    data$fraction = round(data$users / sum(data$users) * 100,2)
    # Compute the cumulative percentages (top of each rectangle)
    data$ymax = cumsum(data$fraction)
    
    # Compute the bottom of each rectangle
    data$ymin = c(0, head(data$ymax, n=-1))
    
    data$labelPosition <- (data$ymax + data$ymin) / 2
    
    # Compute a good label
    data$label <- paste0(data$user.type, '\n value: ', data$fraction, '%')
    
    # Make the plot
    ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=user.type)) +
      geom_rect() +
      coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
      geom_label( x=3.5, aes(y=labelPosition, label=label), size=6) +
      scale_fill_brewer(palette=2) +
      theme_void() +
      theme(legend.position = "none") +
      xlim(c(2, 4)) # Try to remove that to see how to make a pie chart
  })
  
  output$onedayret <- renderPlot({
    retType <- input$retType
    p_df <- get_retention_df(retType)
    coul <- brewer.pal(3, "Pastel2") 
    ggplot(data = p_df) +
      geom_col(aes(x = group, y = value, fill = retention)) +
      theme_minimal() +
      geom_text(aes(x = group, y = value, label = value, group = retention),
                position = position_stack(vjust = .5))
    
  })
  
  output$abresult <- renderText({
    w1 <- input$gamesPlayed
    w2 <- input$oneWeight
    w3 <- input$SevenWeight
    validate(
      need( (w1+w2+w3) == 1,'The sum of the weights should be one')
    )
    metric <- input$metric
    ttest <- ifelse(metric=='GAMES' | metric =='ALL', T, F)
    pvalue <- AB_result(ds, w1,w2,w3,ttest,metric)
    if (pvalue < 0.05) {
      text <- paste0('There is a significant difference between the groups (p=',
                     round(pvalue,2), ')')
    } else {
      text <- paste0('There is no significant difference (p=', round(pvalue,2), ')')
    }
    text
  })
  
  output$upliftone <- renderValueBox({
    uplift <- get_uplift(ds, 'retention_1')
    valueBoxColor <- ifelse(uplift >= 0, 'green', 'red')
    uplift <- paste0(round(uplift, 2), '%')
    valueBox(uplift, 'Uplift one-day retention', icon = icon('thumbs-down'), color=valueBoxColor)
  })
  
  output$upliftseven <- renderValueBox({
    uplift <- get_uplift(ds, 'retention_7')
    valueBoxColor <- ifelse(uplift >= 0, 'green', 'red')
    uplift <- paste0(round(uplift, 2), '%')
    valueBox(uplift, 'Uplift seven-day retention', icon = icon('thumbs-down'), color=valueBoxColor)
  })
  
  output$upliftgames <- renderValueBox({
    uplift <- get_uplift(ds, 'sum_gamerounds')
    valueBoxColor <- ifelse(uplift >= 0, 'green', 'red')
    uplift <- paste0(round(uplift, 2), '%')
    valueBox("-0.2%", 'Uplift games played per user', icon = icon('thumbs-up'), color='yellow')
  })

})
