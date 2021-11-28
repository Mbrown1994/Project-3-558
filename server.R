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
shinyServer(function(input, output, session) {
  
  # Create the table for the Data tab
  output$table <- renderDataTable({
    specificPlayers <- unlist(input$specificPlayers)
    Shots <- unlist(input$Shots)
    selectedCols <- unlist(input$selectedCols)
    
    # Filter
    nba_shots %>% filter(player_name %in% specificPlayers,
                         shot_made_flag %in% Shots) %>% select(selectedCols)
  })
 
  # Allow the user to download the data
  output$Download <- downloadHandler(
  filename = function (){
    paste("NBAdata.csv")
  },
  content = function(file){
    write.csv(
      nba_shots %>% 
        filter(player_name %in% input$specificPlayers,
               shot_made_flag %in% input$Shots) %>%
        select(input$selectedCols),
      file,
      row.names = FALSE
    )
  }
  )
  
   # Create a plot for Data Exploration page
  output$BasketballPlot <- renderPlot({
    
    # This is the data for all shots by LeBron James in the 2017-2018 season
    if (input$player == "LeBron James" & input$shots == "All Shots" & input$season == "2017-18"){
    source("helpers.R")  
    gg_court = make_court()
    graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2017-18")
    gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                          aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "red", missed = "black"))}
    
    # This is the data for all shots by LeBron James in the 2016-2017 season
    else if (input$player == "LeBron James" & input$shots == "All Shots" & input$season == "2016-17"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2016-17")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "red", missed = "black"))}
    
    # This is the data for all shots by LeBron James in the 2015-2016 season
    else if (input$player == "LeBron James" & input$shots == "All Shots" & input$season == "2015-16"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2015-16")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "red", missed = "black"))}
    
    # This is the data for all shots by LeBron James in the 2014-2015 season
    else if (input$player == "LeBron James" & input$shots == "All Shots" & input$season == "2014-15"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2014-15")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "red", missed = "black"))}
    
    # This is the data for all shots by LeBron James in the 2013-2014 season
    else if (input$player == "LeBron James" & input$shots == "All Shots" & input$season == "2013-14"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2013-14")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "red", missed = "black"))}
    
    # This is the data for all shots by LeBron James in the 2012-2013 season
    else if (input$player == "LeBron James" & input$shots == "All Shots" & input$season == "2012-13"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2012-13")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "red", missed = "black"))}
    
    # This is the data for all shots by LeBron James in the 2011-2012 season
    else if (input$player == "LeBron James" & input$shots == "All Shots" & input$season == "2011-12"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2011-12")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "red", missed = "black"))}
    
    # This is the data for all shots by LeBron James in the 2010-2011 season
    else if (input$player == "LeBron James" & input$shots == "All Shots" & input$season == "2010-11"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2010-11")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "red", missed = "black"))}
    
    # This is the data for all shots by LeBron James in the 2009-2010 season
    else if (input$player == "LeBron James" & input$shots == "All Shots" & input$season == "2009-10"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2009-10")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "red", missed = "black"))}
    
    # This is the data for all shots by LeBron James in the 2008-2009 season
    else if (input$player == "LeBron James" & input$shots == "All Shots" & input$season == "2008-09"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2008-09")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "red", missed = "black"))}
    
    # This is the data for all shots by LeBron James in the 2007-2008 season
    else if (input$player == "LeBron James" & input$shots == "All Shots" & input$season == "2007-08"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2007-08")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "red", missed = "black"))}
    
    # This is the data for all shots by LeBron James in the 2006-2007 season
    else if (input$player == "LeBron James" & input$shots == "All Shots" & input$season == "2006-07"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2006-07")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "red", missed = "black"))}
    
    # This is the data for all shots by LeBron James in the 2005-2006 season
    else if (input$player == "LeBron James" & input$shots == "All Shots" & input$season == "2005-06"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2005-06")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "red", missed = "black"))}
    
    # This is the data for all shots by LeBron James in the 2004-2005 season
    else if (input$player == "LeBron James" & input$shots == "All Shots" & input$season == "2004-05"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2004-05")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "red", missed = "black"))}
    
    # This is the data for all shots by LeBron James in the 2003-2004 season
    else if (input$player == "LeBron James" & input$shots == "All Shots" & input$season == "2003-04"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2003-04")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "red", missed = "black"))}
    
    # This is the data for made shots by Lebron James in the 2017-2018 season
    else if (input$player == "LeBron James" & input$shots == "Made Shots" & input$season == "2017-18"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2017-18" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "red"))}
    
    # This is the data for made shots by LeBron James in the 2016-2017 season
    else if (input$player == "LeBron James" & input$shots == "Made Shots" & input$season == "2016-17"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2016-17" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "red"))}
    
    # This is the data for made shots by LeBron James in the 2015-2016 season
    else if (input$player == "LeBron James" & input$shots == "Made Shots" & input$season == "2015-16"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2015-16" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "red"))}
    
    # This is the data for made shots by LeBron James in the 2014-2015 season
    else if (input$player == "LeBron James" & input$shots == "Made Shots" & input$season == "2014-15"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2014-15" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "red"))}
    
    # This is the data for made shots by LeBron James in the 2013-2014 season
    else if (input$player == "LeBron James" & input$shots == "Made Shots" & input$season == "2013-14"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2013-14" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "red"))}
    
    # This is the data for made shots by LeBron James in the 2012-2013 season
    else if (input$player == "LeBron James" & input$shots == "Made Shots" & input$season == "2012-13"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2012-13" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "red"))}
    
    # This is the data for made shots by LeBron James in the 2011-2012 season
    else if (input$player == "LeBron James" & input$shots == "Made Shots" & input$season == "2011-12"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2011-12" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "red"))}
    
    # This is the data for made shots by LeBron James in the 2010-2011 season
    else if (input$player == "LeBron James" & input$shots == "Made Shots" & input$season == "2010-11"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2010-11" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "red"))}
    
    # This is the data for made shots by LeBron James in the 2009-2010 season
    else if (input$player == "LeBron James" & input$shots == "Made Shots" & input$season == "2009-10"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2009-10" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "red"))}
    
    # This is the data for made shots by LeBron James in the 2008-2009 season
    else if (input$player == "LeBron James" & input$shots == "Made Shots" & input$season == "2008-09"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2008-09" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "red"))}
    
    # This is the data for made shots by LeBron James in the 2007-2008 season
    else if (input$player == "LeBron James" & input$shots == "Made Shots" & input$season == "2007-08"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2007-08" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "red"))}
    
    # This is the data for made shots by LeBron James in the 2006-2007 season
    else if (input$player == "LeBron James" & input$shots == "Made Shots" & input$season == "2006-07"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2006-07" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "red"))}
    
    # This is the data for made shots by LeBron James in the 2005-2006 season
    else if (input$player == "LeBron James" & input$shots == "Made Shots" & input$season == "2005-06"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2005-06" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "red"))}
    
    # This is the data for made shots by LeBron James in the 2004-2005 season
    else if (input$player == "LeBron James" & input$shots == "Made Shots" & input$season == "2004-05"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2004-05" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "red"))}
    
    # This is the data for made shots by LeBron James in the 2003-2004 season
    else if (input$player == "LeBron James" & input$shots == "Made Shots" & input$season == "2003-04"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2003-04" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "red"))}
    
    # This is the data for missed shots by LeBron James in the 2017-2018 season
    else if (input$player == "LeBron James" & input$shots == "Missed Shots" & input$season == "2017-18"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2017-18" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "black"))}
    
    # This is the data for missed shots by LeBron James in the 2016-2017 season
    else if (input$player == "LeBron James" & input$shots == "Missed Shots" & input$season == "2016-17"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2016-17" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "black"))}
    
    # This is the data for missed shots by LeBron James in the 2015-2016 season
    else if (input$player == "LeBron James" & input$shots == "Missed Shots" & input$season == "2015-16"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2015-16" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "black"))}
    
    # This is the data for missed shots by LeBron James in the 2014-2015 season
    else if (input$player == "LeBron James" & input$shots == "Missed Shots" & input$season == "2014-15"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2014-15" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "black"))}
    
    # This is the data for missed shots by LeBron James in the 2013-2014 season
    else if (input$player == "LeBron James" & input$shots == "Missed Shots" & input$season == "2013-14"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2013-14" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "black"))}
    
    # This is the data for missed shots by LeBron James in the 2012-2013 season
    else if (input$player == "LeBron James" & input$shots == "Missed Shots" & input$season == "2012-13"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2012-13" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "black"))}
    
    # This is the data for missed shots by LeBron James in the 2011-2012 season
    else if (input$player == "LeBron James" & input$shots == "Missed Shots" & input$season == "2011-12"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2011-12" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "black"))}
    
    # This is the data for missed shots by LeBron James in the 2010-2011 season
    else if (input$player == "LeBron James" & input$shots == "Missed Shots" & input$season == "2010-11"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2010-11" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "black"))}
    
    # This is the data for missed shots by LeBron James in the 2009-2010 season
    else if (input$player == "LeBron James" & input$shots == "Missed Shots" & input$season == "2009-10"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2009-10" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "black"))}
    
    # This is the data for missed shots by LeBron James in the 2008-2009 season
    else if (input$player == "LeBron James" & input$shots == "Missed Shots" & input$season == "2008-09"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2008-09" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "black"))}
    
    # This is the data for missed shots by LeBron James in the 2007-2008 season
    else if (input$player == "LeBron James" & input$shots == "Missed Shots" & input$season == "2007-08"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2007-08" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "black"))}
    
    # This is the data for missed shots by LeBron James in the 2006-2007 season
    else if (input$player == "LeBron James" & input$shots == "Missed Shots" & input$season == "2006-07"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2006-07" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "black"))}
    
    # This is the data for missed shots by LeBron James in the 2005-2006 season
    else if (input$player == "LeBron James" & input$shots == "Missed Shots" & input$season == "2005-06"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2005-06" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "black"))}
    
    # This is the data for missed shots by LeBron James in the 2004-2005 season
    else if (input$player == "LeBron James" & input$shots == "Missed Shots" & input$season == "2004-05"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2004-05" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "black"))}
    
    # This is the data for missed shots by LeBron James in the 2003-2004 season
    else if (input$player == "LeBron James" & input$shots == "Missed Shots" & input$season == "2003-04"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2003-04" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "black"))}
    
    # This is the data for all shots by Russell Westbrook in the 2017-2018 season
    else if (input$player == "Russell Westbrook" & input$shots == "All Shots" & input$season == "2017-18"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2017-18")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "purple", missed = "yellow3"))}
    
    # This is the data for all shots by Russell Westbrook in the 2016-2017 season
    else if (input$player == "Russell Westbrook" & input$shots == "All Shots" & input$season == "2016-17"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2016-17")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "purple", missed = "yellow3"))}
    
    # This is the data for all shots by Russell Westbrook in the 2015-2016 season
    else if (input$player == "Russell Westbrook" & input$shots == "All Shots" & input$season == "2015-16"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2015-16")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "purple", missed = "yellow3"))}
    
    # This is the data for all shots by Russell Westbrook in the 2014-2015 season
    else if (input$player == "Russell Westbrook" & input$shots == "All Shots" & input$season == "2014-15"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2014-15")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "purple", missed = "yellow3"))}
    
    # This is the data for all shots by Russell Westbrook in the 2013-2014 season
    else if (input$player == "Russell Westbrook" & input$shots == "All Shots" & input$season == "2013-14"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2013-14")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "purple", missed = "yellow3"))}
    
    # This is the data for all shots by Russell Westbrook in the 2012-2013 season
    else if (input$player == "Russell Westbrook" & input$shots == "All Shots" & input$season == "2012-13"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2012-13")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "purple", missed = "yellow3"))}
    
    # This is the data for all shots by Russell Westbrook in the 2011-2012 season
    else if (input$player == "Russell Westbrook" & input$shots == "All Shots" & input$season == "2011-12"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2011-12")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "purple", missed = "yellow3"))}
    
    # This is the data for all shots by Russell Westbrook in the 2010-2011 season
    else if (input$player == "Russell Westbrook" & input$shots == "All Shots" & input$season == "2010-11"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2010-11")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "purple", missed = "yellow3"))}
    
    # This is the data for all shots by Russell Westbrook in the 2009-2010 season
    else if (input$player == "Russell Westbrook" & input$shots == "All Shots" & input$season == "2009-10"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2009-10")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "purple", missed = "yellow3"))}
    
    # This is the data for all shots by Russell Westbrook in the 2008-2009 season
    else if (input$player == "Russell Westbrook" & input$shots == "All Shots" & input$season == "2008-09"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2008-09")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "purple", missed = "yellow3"))}
    
    # This is the data for all shots by Russell Westbrook in the 2007-2008 season
    else if (input$player == "Russell Westbrook" & input$shots == "All Shots" & input$season == "2007-08"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2007-08")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "purple", missed = "yellow3"))}
    
    # This is the data for all shots by Russell Westbrook in the 2006-2007 season
    else if (input$player == "Russell Westbrook" & input$shots == "All Shots" & input$season == "2006-07"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2006-07")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "purple", missed = "yellow3"))}
    
    # This is the data for all shots by Russell Westbrook in the 2005-2006 season
    else if (input$player == "Russell Westbrook" & input$shots == "All Shots" & input$season == "2005-06"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2005-06")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "purple", missed = "yellow3"))}
    
    # This is the data for all shots by Russell Westbrook in the 2004-2005 season
    else if (input$player == "Russell Westbrook" & input$shots == "All Shots" & input$season == "2004-05"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2004-05")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "purple", missed = "yellow3"))}
    
    # This is the data for all shots by Russell Westbrook in the 2003-2004 season
    else if (input$player == "Russell Westbrook" & input$shots == "All Shots" & input$season == "2003-04"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2003-04")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "purple", missed = "yellow3"))}
    
    # This is the data for all made shots by Russell Westbrook in the 2017-2018 season
    else if (input$player == "Russell Westbrook" & input$shots == "Made Shots" & input$season == "2017-18"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2017-18" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "purple"))}
    
    # This is the data for all made shots by Russell Westbrook in the 2016-2017 season
    else if (input$player == "Russell Westbrook" & input$shots == "Made Shots" & input$season == "2016-17"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2016-17" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "purple"))}
    
    # This is the data for all made shots by Russell Westbrook in the 2015-2016 season
    else if (input$player == "Russell Westbrook" & input$shots == "Made Shots" & input$season == "2015-16"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2015-16" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "purple"))}
    
    # This is the data for all made shots by Russell Westbrook in the 2014-2015 season
    else if (input$player == "Russell Westbrook" & input$shots == "Made Shots" & input$season == "2014-15"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2014-15" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "purple"))}
    
    # This is the data for all made shots by Russell Westbrook in the 2013-2014 season
    else if (input$player == "Russell Westbrook" & input$shots == "Made Shots" & input$season == "2013-14"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2013-14" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "purple"))}
    
    # This is the data for all made shots by Russell Westbrook in the 2012-2013 season
    else if (input$player == "Russell Westbrook" & input$shots == "Made Shots" & input$season == "2012-13"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2012-13" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "purple"))}
    
    # This is the data for all made shots by Russell Westbrook in the 2011-2012 season
    else if (input$player == "Russell Westbrook" & input$shots == "Made Shots" & input$season == "2011-12"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2011-12" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "purple"))}
    
    # This is the data for all made shots by Russell Westbrook in the 2010-2011 season
    else if (input$player == "Russell Westbrook" & input$shots == "Made Shots" & input$season == "2010-11"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2010-11" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "purple"))}
    
    # This is the data for all made shots by Russell Westbrook in the 2009-2010 season
    else if (input$player == "Russell Westbrook" & input$shots == "Made Shots" & input$season == "2009-10"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2009-10" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "purple"))}
    
    # This is the data for all made shots by Russell Westbrook in the 2008-2009 season
    else if (input$player == "Russell Westbrook" & input$shots == "Made Shots" & input$season == "2008-09"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2008-09" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "purple"))}
    
    # This is the data for all made shots by Russell Westbrook in the 2007-2008 season
    else if (input$player == "Russell Westbrook" & input$shots == "Made Shots" & input$season == "2007-08"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2007-08" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "purple"))}
    
    # This is the data for all made shots by Russell Westbrook in the 2006-2007 season
    else if (input$player == "Russell Westbrook" & input$shots == "Made Shots" & input$season == "2006-07"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2006-07" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "purple"))}
    
    # This is the data for all made shots by Russell Westbrook in the 2005-2006 season
    else if (input$player == "Russell Westbrook" & input$shots == "Made Shots" & input$season == "2005-06"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2005-06" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "purple"))}
    
    # This is the data for all made shots by Russell Westbrook in the 2004-2005 season
    else if (input$player == "Russell Westbrook" & input$shots == "Made Shots" & input$season == "2004-05"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2004-05" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "purple"))}
    
    # This is the data for all made shots by Russell Westbrook in the 2003-2004 season
    else if (input$player == "Russell Westbrook" & input$shots == "Made Shots" & input$season == "2003-04"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2003-04" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "purple"))}
    
    # This is the data for all missed shots by Russell Westbrook in the 2017-2018 season
    else if (input$player == "Russell Westbrook" & input$shots == "Missed Shots" & input$season == "2017-18"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2017-18" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(missed = "yellow3"))}
    
    # This is the data for all missed shots by Russell Westbrook in the 2016-2017 season
    else if (input$player == "Russell Westbrook" & input$shots == "Missed Shots" & input$season == "2016-17"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2016-17" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(missed = "yellow3"))}
    
    # This is the data for all missed shots by Russell Westbrook in the 2015-2016 season
    else if (input$player == "Russell Westbrook" & input$shots == "Missed Shots" & input$season == "2015-16"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2015-16" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(missed = "yellow3"))}
    
    # This is the data for all missed shots by Russell Westbrook in the 2014-2015 season
    else if (input$player == "Russell Westbrook" & input$shots == "Missed Shots" & input$season == "2014-15"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2014-15" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(missed = "yellow3"))}
    
    # This is the data for all missed shots by Russell Westbrook in the 2013-2014 season
    else if (input$player == "Russell Westbrook" & input$shots == "Missed Shots" & input$season == "2013-14"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2013-14" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(missed = "yellow3"))}
    
    # This is the data for all missed shots by Russell Westbrook in the 2012-2013 season
    else if (input$player == "Russell Westbrook" & input$shots == "Missed Shots" & input$season == "2012-13"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2012-13" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(missed = "yellow3"))}
    
    # This is the data for all missed shots by Russell Westbrook in the 2011-2012 season
    else if (input$player == "Russell Westbrook" & input$shots == "Missed Shots" & input$season == "2011-12"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2011-12" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(missed = "yellow3"))}
    
    # This is the data for all missed shots by Russell Westbrook in the 2010-2011 season
    else if (input$player == "Russell Westbrook" & input$shots == "Missed Shots" & input$season == "2010-11"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2010-11" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(missed = "yellow3"))}
    
    # This is the data for all missed shots by Russell Westbrook in the 2009-2010 season
    else if (input$player == "Russell Westbrook" & input$shots == "Missed Shots" & input$season == "2009-10"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2009-10" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(missed = "yellow3"))}
    
    # This is the data for all missed shots by Russell Westbrook in the 2008-2009 season
    else if (input$player == "Russell Westbrook" & input$shots == "Missed Shots" & input$season == "2008-09"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2008-09" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(missed = "yellow3"))}
    
    # This is the data for all missed shots by Russell Westbrook in the 2007-2008 season
    else if (input$player == "Russell Westbrook" & input$shots == "Missed Shots" & input$season == "2007-08"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2007-08" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(missed = "yellow3"))}
    
    # This is the data for all missed shots by Russell Westbrook in the 2006-2007 season
    else if (input$player == "Russell Westbrook" & input$shots == "Missed Shots" & input$season == "2006-07"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2006-07" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(missed = "yellow3"))}
    
    # This is the data for all missed shots by Russell Westbrook in the 2005-2006 season
    else if (input$player == "Russell Westbrook" & input$shots == "Missed Shots" & input$season == "2005-06"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2005-06" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(missed = "yellow3"))}
    
    # This is the data for all missed shots by Russell Westbrook in the 2004-2005 season
    else if (input$player == "Russell Westbrook" & input$shots == "Missed Shots" & input$season == "2004-05"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2004-05" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(missed = "yellow3"))}
    
    # This is the data for all missed shots by Russell Westbrook in the 2003-2004 season
    else if (input$player == "Russell Westbrook" & input$shots == "Missed Shots" & input$season == "2003-04"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2003-04" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(missed = "yellow3"))}
    
    # This is the data for all shots by Kevin Durant in the 2017-2018 season
    else if (input$player == "Kevin Durant" & input$shots == "All Shots" & input$season == "2017-18"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2017-18")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "blue", missed = "green"))}
    
    # This is the data for all shots by Kevin Durant in the 2016-2017 season
    else if (input$player == "Kevin Durant" & input$shots == "All Shots" & input$season == "2016-17"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2016-17")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "blue", missed = "green"))}
    
    # This is the data for all shots by Kevin Durant in the 2015-2016 season
    else if (input$player == "Kevin Durant" & input$shots == "All Shots" & input$season == "2015-16"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2015-16")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "blue", missed = "green"))}
    
    # This is the data for all shots by Kevin Durant in the 2014-2015 season
    else if (input$player == "Kevin Durant" & input$shots == "All Shots" & input$season == "2014-15"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2014-15")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "blue", missed = "green"))}
    
    # This is the data for all shots by Kevin Durant in the 2013-2014 season
    else if (input$player == "Kevin Durant" & input$shots == "All Shots" & input$season == "2013-14"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2013-14")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "blue", missed = "green"))}
    
    # This is the data for all shots by Kevin Durant in the 2012-2013 season
    else if (input$player == "Kevin Durant" & input$shots == "All Shots" & input$season == "2012-13"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2012-13")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "blue", missed = "green"))}
    
    # This is the data for all shots by Kevin Durant in the 2011-2012 season
    else if (input$player == "Kevin Durant" & input$shots == "All Shots" & input$season == "2011-12"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2011-12")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "blue", missed = "green"))}
    
    # This is the data for all shots by Kevin Durant in the 2010-2011 season
    else if (input$player == "Kevin Durant" & input$shots == "All Shots" & input$season == "2010-11"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2010-11")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "blue", missed = "green"))}
    
    # This is the data for all shots by Kevin Durant in the 2009-2010 season
    else if (input$player == "Kevin Durant" & input$shots == "All Shots" & input$season == "2009-10"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2009-10")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "blue", missed = "green"))}
    
     # This is the data for all shots by Kevin Durant in the 2008-2009 season
    else if (input$player == "Kevin Durant" & input$shots == "All Shots" & input$season == "2008-09"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2008-09")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "blue", missed = "green"))}
    
    # This is the data for all shots by Kevin Durant in the 2007-2008 season
    else if (input$player == "Kevin Durant" & input$shots == "All Shots" & input$season == "2007-08"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2007-08")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "blue", missed = "green"))}
    
    # This is the data for all shots by Kevin Durant in the 2006-2007 season
    else if (input$player == "Kevin Durant" & input$shots == "All Shots" & input$season == "2006-07"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2006-07")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "blue", missed = "green"))}
    
    # This is the data for all shots by Kevin Durant in the 2005-2006 season
    else if (input$player == "Kevin Durant" & input$shots == "All Shots" & input$season == "2005-06"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2005-06")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "blue", missed = "green"))}
    
    # This is the data for all shots by Kevin Durant in the 2004-2005 season
    else if (input$player == "Kevin Durant" & input$shots == "All Shots" & input$season == "2004-05"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2004-05")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "blue", missed = "green"))}
    
    # This is the data for all shots by Kevin Durant in the 2003-2004 season
    else if (input$player == "Kevin Durant" & input$shots == "All Shots" & input$season == "2003-04"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2003-04")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "blue", missed = "green"))}
    
    # This is the data for all made shots by Kevin Durant in the 2017-2018 season
    else if (input$player == "Kevin Durant" & input$shots == "Made Shots" & input$season == "2017-18"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2017-18" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "blue"))}
    
    # This is the data for all made shots by Kevin Durant in the 2016-2017 season
    else if (input$player == "Kevin Durant" & input$shots == "Made Shots" & input$season == "2016-17"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2016-17" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "blue"))}
    
    # This is the data for all made shots by Kevin Durant in the 2015-2016 season
    else if (input$player == "Kevin Durant" & input$shots == "Made Shots" & input$season == "2015-16"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2015-16" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "blue"))}
    
    # This is the data for all made shots by Kevin Durant in the 2014-2015
    else if (input$player == "Kevin Durant" & input$shots == "Made Shots" & input$season == "2014-15"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2014-15" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "blue"))}
    
    # This is the data for all made shots by Kevin Durant in the 2013-2014 season
    else if (input$player == "Kevin Durant" & input$shots == "Made Shots" & input$season == "2013-14"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2013-14" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "blue"))}
    
    # This is the data for all made shots by Kevin Durant in the 2012-2013 season
    else if (input$player == "Kevin Durant" & input$shots == "Made Shots" & input$season == "2012-13"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2012-13" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "blue"))}
    
    # This is the data for all made shots by Kevin Durant in the 2011-2012 season
    else if (input$player == "Kevin Durant" & input$shots == "Made Shots" & input$season == "2011-12"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2011-12" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "blue"))}
    
    # This is the data for all made shots by Kevin Durant in the 2010-2011 season
    else if (input$player == "Kevin Durant" & input$shots == "Made Shots" & input$season == "2010-11"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2010-11" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "blue"))}
    
    # This is the data for all made shots by Kevin Durant in the 2009-2010 season
    else if (input$player == "Kevin Durant" & input$shots == "Made Shots" & input$season == "2009-10"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2009-10" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "blue"))}
    
    # This is the data for all made shots by Kevin Durant in the 2008-2009 season
    else if (input$player == "Kevin Durant" & input$shots == "Made Shots" & input$season == "2008-09"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2008-09" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "blue"))}
    
    # This is the data for all made shots by Kevin Durant in the 2007-2008 season
    else if (input$player == "Kevin Durant" & input$shots == "Made Shots" & input$season == "2007-08"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2007-08" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "blue"))}
    
    # This is the data for all made shots by Kevin Durant in the 2006-2007 season
    else if (input$player == "Kevin Durant" & input$shots == "Made Shots" & input$season == "2006-07"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2006-07" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "blue"))}
    
    # This is the data for all made shots by Kevin Durant in the 2005-2006 season
    else if (input$player == "Kevin Durant" & input$shots == "Made Shots" & input$season == "2005-06"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2005-06" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "blue"))}
    
    # This is the data for all made shots by Kevin Durant in the 2004-2005 season
    else if (input$player == "Kevin Durant" & input$shots == "Made Shots" & input$season == "2004-05"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2004-05" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "blue"))}
    
    # This is the data for all made shots by Kevin Durant in the 2003-2004 season
    else if (input$player == "Kevin Durant" & input$shots == "Made Shots" & input$season == "2003-04"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2003-04" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "blue"))}
    
    # This is the data for all missed shots by Kevin Durant in the 2017-2018 season
    else if (input$player == "Kevin Durant" & input$shots == "Missed Shots" & input$season == "2017-18"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2017-18" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "green"))}
    
    # This is the data for all missed shots by Kevin Durant in the 2016-2017 season
    else if (input$player == "Kevin Durant" & input$shots == "Missed Shots" & input$season == "2016-17"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2017-18" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "green"))}
    
    # This is the data for all missed shots by Kevin Durant in the 2015-2016 season
    else if (input$player == "Kevin Durant" & input$shots == "Missed Shots" & input$season == "2015-16"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2015-16" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "green"))}
    
    # This is the data for all missed shots by Kevin Durant in the 2014-2015 season
    else if (input$player == "Kevin Durant" & input$shots == "Missed Shots" & input$season == "2014-15"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2014-15" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "green"))}
    
    # This is the data for all missed shots by Kevin Durant in the 2013-2014 season
    else if (input$player == "Kevin Durant" & input$shots == "Missed Shots" & input$season == "2013-14"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2013-14" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "green"))}
    
    # This is the data for all missed shots by Kevin Durant in the 2012-2013 season
    else if (input$player == "Kevin Durant" & input$shots == "Missed Shots" & input$season == "2012-13"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2012-13" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "green"))}
    
    # This is the data for all missed shots by Kevin Durant in the 2011-2012 season
    else if (input$player == "Kevin Durant" & input$shots == "Missed Shots" & input$season == "2011-12"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2011-12" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "green"))}
    
    # This is the data for all missed shots by Kevin Durant in the 2010-2011 season
    else if (input$player == "Kevin Durant" & input$shots == "Missed Shots" & input$season == "2010-11"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2010-11" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "green"))}
    
    # This is the data for all missed shots by Kevin Durant in the 2009-2010 season
    else if (input$player == "Kevin Durant" & input$shots == "Missed Shots" & input$season == "2009-10"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2009-10" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "green"))}
    
    # This is the data for all missed shots by Kevin Durant in the 2008-2009 season
    else if (input$player == "Kevin Durant" & input$shots == "Missed Shots" & input$season == "2008-09"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2008-09" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "green"))}
    
    # This is the data for all missed shots by Kevin Durant in the 2007-2008 season
    else if (input$player == "Kevin Durant" & input$shots == "Missed Shots" & input$season == "2007-08"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2007-08" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "green"))}
    
    # This is the data for all missed shots by Kevin Durant in the 2006-2007 season
    else if (input$player == "Kevin Durant" & input$shots == "Missed Shots" & input$season == "2006-07"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2006-07" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "green"))}
    
    # This is the data for all missed shots by Kevin Durant in the 2005-2006 season
    else if (input$player == "Kevin Durant" & input$shots == "Missed Shots" & input$season == "2005-06"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2005-06" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "green"))}
    
    # This is the data for all missed shots by Kevin Durant in the 2004-2005 season
    else if (input$player == "Kevin Durant" & input$shots == "Missed Shots" & input$season == "2004-05"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2004-05" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "green"))}
    
    # This is the data for all missed shots by Kevin Durant in the 2003-2004 season
    else if (input$player == "Kevin Durant" & input$shots == "Missed Shots" & input$season == "2003-04"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2003-04" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "green"))}
    
    # This is the data for all shots by Stephen Curry in the 2017-2018 season
    else if (input$player == "Stephen Curry" & input$shots == "All Shots" & input$season == "2017-18"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2017-18")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "orange", missed = "steel blue"))}
    
    # This is the data for all shots by Stephen Curry in the 2016-2017 season
    else if (input$player == "Stephen Curry" & input$shots == "All Shots" & input$season == "2016-17"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2016-17")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "orange", missed = "steel blue"))}
    
    # This is the data for all shots by Stephen Curry in the 2015-2016 season
    else if (input$player == "Stephen Curry" & input$shots == "All Shots" & input$season == "2015-16"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2015-16")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "orange", missed = "steel blue"))}
    
    # This is the data for all shots by Stephen Curry in the 2014-2015 season
    else if (input$player == "Stephen Curry" & input$shots == "All Shots" & input$season == "2014-15"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2014-15")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "orange", missed = "steel blue"))}
    
    # This is the data for all shots by Stephen Curry in the 2013-2014 season
    else if (input$player == "Stephen Curry" & input$shots == "All Shots" & input$season == "2013-14"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2013-14")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "orange", missed = "steel blue"))}
    
    # This is the data for all shots by Stephen Curry in the 2012-2013 season
    else if (input$player == "Stephen Curry" & input$shots == "All Shots" & input$season == "2012-13"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2012-13")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "orange", missed = "steel blue"))}
    
    # This is the data for all shots by Stephen Curry in the 2011-2012 season
    else if (input$player == "Stephen Curry" & input$shots == "All Shots" & input$season == "2011-12"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2011-12")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "orange", missed = "steel blue"))}
    
    # This is the data for all shots by Stephen Curry in the 2010-2011 season
    else if (input$player == "Stephen Curry" & input$shots == "All Shots" & input$season == "2010-11"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2010-11")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "orange", missed = "steel blue"))}
    
    # This is the data for all shots by Stephen Curry in the 2009-2010
    else if (input$player == "Stephen Curry" & input$shots == "All Shots" & input$season == "2009-10"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2009-10")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "orange", missed = "steel blue"))}
    
    # This is the data for all shots by Stephen Curry in the 2008-2009 season
    else if (input$player == "Stephen Curry" & input$shots == "All Shots" & input$season == "2008-09"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2008-09")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "orange", missed = "steel blue"))}
    
    # This is the data for all shots by Stephen Curry in the 2007-2008 season
    else if (input$player == "Stephen Curry" & input$shots == "All Shots" & input$season == "2007-08"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2007-08")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "orange", missed = "steel blue"))}
    
    # This is the data for all shots by Stephen Curry in the 2006-2007 season
    else if (input$player == "Stephen Curry" & input$shots == "All Shots" & input$season == "2006-07"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2006-07")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "orange", missed = "steel blue"))}
    
    # This is the data for all shots by Stephen Curry in the 2005-2006 season
    else if (input$player == "Stephen Curry" & input$shots == "All Shots" & input$season == "2005-06"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2005-06")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "orange", missed = "steel blue"))}
    
    # This is the data for all shots by Stephen Curry in the 2004-2005 season
    else if (input$player == "Stephen Curry" & input$shots == "All Shots" & input$season == "2004-05"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2004-05")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "orange", missed = "steel blue"))}
    
    # This is the data for all shots by Stephen Curry in the 2003-2004 season
    else if (input$player == "Stephen Curry" & input$shots == "All Shots" & input$season == "2003-04"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2003-04")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "orange", missed = "steel blue"))}
    
    # This is the data for all made shots by Stephen Curry in the 2017-2018 season
    else if (input$player == "Stephen Curry" & input$shots == "Made Shots" & input$season == "2017-18"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2017-18" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "orange"))}
    
    # This is the data for all made shots by Stephen Curry in the 2016-2017 season
    else if (input$player == "Stephen Curry" & input$shots == "Made Shots" & input$season == "2016-17"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2016-17" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "orange"))}
    
    # This is the data for all made shots by Stephen Curry in the 2015-2016 season
    else if (input$player == "Stephen Curry" & input$shots == "Made Shots" & input$season == "2015-16"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2015-16" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "orange"))}
    
    # This is the data for all made shots by Stephen Curry in the 2014- 2015 season
    else if (input$player == "Stephen Curry" & input$shots == "Made Shots" & input$season == "2014-15"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2014-15" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "orange"))}
    
    # This is the data for all made shots by Stephen Curry in the 2013-2014 season
    else if (input$player == "Stephen Curry" & input$shots == "Made Shots" & input$season == "2013-14"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2013-14" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "orange"))}
    
    # This is the data for all made shots by Stephen Curry in the 2012-2013 season
    else if (input$player == "Stephen Curry" & input$shots == "Made Shots" & input$season == "2012-13"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2012-13" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "orange"))}
    
    # This is the data for all made shots by Stephen Curry in the 2011-2012
    else if (input$player == "Stephen Curry" & input$shots == "Made Shots" & input$season == "2011-12"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2011-12" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "orange"))}
    
    # This is the data for all made shots by Stephen Curry in the 2010-2011 season
    else if (input$player == "Stephen Curry" & input$shots == "Made Shots" & input$season == "2010-11"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2010-11" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "orange"))}
    
    # This is the data for all made shots by Stephen Curry in the 2009-2010 season
    else if (input$player == "Stephen Curry" & input$shots == "Made Shots" & input$season == "2009-10"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2009-10" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "orange"))}
    
    # This is the data for all made shots by Stephen Curry in the 2008-2009 season
    else if (input$player == "Stephen Curry" & input$shots == "Made Shots" & input$season == "2008-09"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2008-09" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "orange"))}
    
    # This is the data for all made shots by Stephen Curry in the 2007-2008 season
    else if (input$player == "Stephen Curry" & input$shots == "Made Shots" & input$season == "2007-08"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2007-08" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "orange"))}
    
    # This is the data for all made shots by Stephen Curry in the 2006-2007 season
    else if (input$player == "Stephen Curry" & input$shots == "Made Shots" & input$season == "2006-07"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2006-07" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "orange"))}
    
    # This is the data for all made shots by Stephen Curry in the 2005-2006 season
    else if (input$player == "Stephen Curry" & input$shots == "Made Shots" & input$season == "2005-06"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2005-06" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "orange"))}
    
    # This is the data for all made shots by Stephen Curry in the 2004-2005 season
    else if (input$player == "Stephen Curry" & input$shots == "Made Shots" & input$season == "2004-05"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2004-05" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "orange"))}
    
    # This is the data for all made shots by Stephen Curry in the 2003-2004 season
    else if (input$player == "Stephen Curry" & input$shots == "Made Shots" & input$season == "2003-04"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2003-04" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "orange"))}
    
    # This is the data for all missed shots by Stephen Curry in the 2017-2018 season
    else if (input$player == "Stephen Curry" & input$shots == "Missed Shots" & input$season == "2017-18"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2017-18" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "steel blue"))}
    
    # This is the data for all missed shots by Stephen Curry in the 2016-2017 season
    else if (input$player == "Stephen Curry" & input$shots == "Missed Shots" & input$season == "2016-17"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2016-17" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "steel blue"))}
    
    # This is the data for all missed shots by Stephen Curry in the 2015-2016 season
    else if (input$player == "Stephen Curry" & input$shots == "Missed Shots" & input$season == "2015-16"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2015-16" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "steel blue"))}
    
    # This is the data for all missed shots by Stephen Curry in the 2014-2015 season
    else if (input$player == "Stephen Curry" & input$shots == "Missed Shots" & input$season == "2014-15"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2014-15" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "steel blue"))}
    
    # This is the data for all missed shots by Stephen Curry in the 2013-2014 season
    else if (input$player == "Stephen Curry" & input$shots == "Missed Shots" & input$season == "2013-14"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2013-14" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "steel blue"))}
    
    # This is the data for all missed shots by Stephen Curry in the 2012-2013 season
    else if (input$player == "Stephen Curry" & input$shots == "Missed Shots" & input$season == "2012-13"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2012-13" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "steel blue"))}
    
    # This is the data for all missed shots by Stephen Curry in the 2011-2012 season
    else if (input$player == "Stephen Curry" & input$shots == "Missed Shots" & input$season == "2011-12"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2011-12" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "steel blue"))}
    
    # This is the data for all missed shots by Stephen Curry in the 2010-2011 season
    else if (input$player == "Stephen Curry" & input$shots == "Missed Shots" & input$season == "2010-11"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2010-11" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "steel blue"))}
    
    # This is the data for all missed shots by Stephen Curry in the 2009-2010 season
    else if (input$player == "Stephen Curry" & input$shots == "Missed Shots" & input$season == "2009-10"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2009-10" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "steel blue"))}
    
    # This is the data for all missed shots by Stephen Curry in the 2008-2009 season
    else if (input$player == "Stephen Curry" & input$shots == "Missed Shots" & input$season == "2008-09"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2008-09" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "steel blue"))}
    
    # This is the data for all missed shots by Stephen Curry in the 2007-2008 season
    else if (input$player == "Stephen Curry" & input$shots == "Missed Shots" & input$season == "2007-08"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2007-08" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "steel blue"))}
    
    # This is the data for all missed shots by Stephen Curry in the 2006-2007 season
    else if (input$player == "Stephen Curry" & input$shots == "Missed Shots" & input$season == "2006-07"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2006-07" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "steel blue"))}
    
    # This is the data for all missed shots by Stephen Curry in the 2005-2006 season
    else if (input$player == "Stephen Curry" & input$shots == "Missed Shots" & input$season == "2005-06"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2005-06" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "steel blue"))}
    
    # This is the data for all missed shots by Stephen Curry in the 2004-2005 season
    else if (input$player == "Stephen Curry" & input$shots == "Missed Shots" & input$season == "2004-05"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2004-05" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "steel blue"))}
    
    # This is the data for all missed shots by Stephen Curry in the 2003-2004 season
    else if (input$player == "Stephen Curry" & input$shots == "Missed Shots" & input$season == "2003-04"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2003-04" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "steel blue"))}
    
    # This is the data for all shots by Carmelo Anthony in the 2017-2018 season
    else if (input$player == "Carmelo Anthony" & input$shots == "All Shots" & input$season == "2017-18"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2017-18")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "goldenrod1", missed = "darkgreen"))}
    
    # This is the data for all shots by Carmelo Anthony in the 2016-2017 season
    else if (input$player == "Carmelo Anthony" & input$shots == "All Shots" & input$season == "2016-17"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2016-17")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "goldenrod1", missed = "darkgreen"))}
    
    # This is the data for all shots by Carmelo Anthony in the 2015-2016 season
    else if (input$player == "Carmelo Anthony" & input$shots == "All Shots" & input$season == "2015-16"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2015-16")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "goldenrod1", missed = "darkgreen"))}
    
    # This is the data for all shots by Carmelo Anthony in the 2014-2015 season
    else if (input$player == "Carmelo Anthony" & input$shots == "All Shots" & input$season == "2014-15"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2014-15")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "goldenrod1", missed = "darkgreen"))}
    
    # This is the data for all shots by Carmelo Anthony in the 2013-2014 season
    else if (input$player == "Carmelo Anthony" & input$shots == "All Shots" & input$season == "2013-14"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2013-14")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "goldenrod1", missed = "darkgreen"))}
    
    # This is the data for all shots by Carmelo Anthony in the 2012-2013 season
    else if (input$player == "Carmelo Anthony" & input$shots == "All Shots" & input$season == "2012-13"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2012-13")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "goldenrod1", missed = "darkgreen"))}
    
    # This is the data for all shots by Carmelo Anthony in the 2011-2012
    else if (input$player == "Carmelo Anthony" & input$shots == "All Shots" & input$season == "2011-12"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2011-12")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "goldenrod1", missed = "darkgreen"))}
    
    # This is the data for all shots by Carmelo Anthony in the 2010-2011 season
    else if (input$player == "Carmelo Anthony" & input$shots == "All Shots" & input$season == "2010-11"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2010-11")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "goldenrod1", missed = "darkgreen"))}
    
    # This is the data for all shots by Carmelo Anthony in the 2009-2010 season
    else if (input$player == "Carmelo Anthony" & input$shots == "All Shots" & input$season == "2009-10"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2009-10")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "goldenrod1", missed = "darkgreen"))}
    
    # This is the data for all shots by Carmelo Anthony in the 2008-2009 season
    else if (input$player == "Carmelo Anthony" & input$shots == "All Shots" & input$season == "2008-09"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2008-09")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "goldenrod1", missed = "darkgreen"))}
    
    # This is the data for all shots by Carmelo Anthony in the 2007-2008 season
    else if (input$player == "Carmelo Anthony" & input$shots == "All Shots" & input$season == "2007-08"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2007-08")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "goldenrod1", missed = "darkgreen"))}
      
      # This is the data for all shots by Carmelo Anthony in the 2006-2007 season
      else if (input$player == "Carmelo Anthony" & input$shots == "All Shots" & input$season == "2006-07"){
        source("helpers.R")
        gg_court = make_court()
        graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2006-07")
        gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                              aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "goldenrod1", missed = "darkgreen"))}
      
      # This is the data for all shots by Carmelo Anthony in the 2005-2006 season
      else if (input$player == "Carmelo Anthony" & input$shots == "All Shots" & input$season == "2005-06"){
        source("helpers.R")
        gg_court = make_court()
        graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2005-06")
        gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                              aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "goldenrod1", missed = "darkgreen"))}
      
      # This is the data for all shots by Carmelo Anthony in the 2004-2005 season
      else if (input$player == "Carmelo Anthony" & input$shots == "All Shots" & input$season == "2004-05"){
        source("helpers.R")
        gg_court = make_court()
        graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2004-05")
        gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                              aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "goldenrod1", missed = "darkgreen"))}
      
      # This is the data for all shots by Carmelo Anthony in the 2003-2004 season
      else if (input$player == "Carmelo Anthony" & input$shots == "All Shots" & input$season == "2003-04"){
        source("helpers.R")
        gg_court = make_court()
        graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2003-04")
        gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                              aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "goldenrod1", missed = "darkgreen"))}
    
    # This is the data for all made shots by Carmelo Anthony in the 2017-2018 season
    else if (input$player == "Carmelo Anthony" & input$shots == "Made Shots" & input$season == "2017-18"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2017-18" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "goldenrod1"))}
    
    # This is the data for all made shots by Carmelo Anthony in the 2016-2017 season
    else if (input$player == "Carmelo Anthony" & input$shots == "Made Shots" & input$season == "2016-17"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2016-17" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "goldenrod1"))}
    
    # This is the data for all made shots by Carmelo Anthony in the 2015-2016 season
    else if (input$player == "Carmelo Anthony" & input$shots == "Made Shots" & input$season == "2015-16"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2015-16" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "goldenrod1"))}
    
    # This is the data for all made shots by Carmelo Anthony in the 2014-2015 season
    else if (input$player == "Carmelo Anthony" & input$shots == "Made Shots" & input$season == "2014-15"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2014-15" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "goldenrod1"))}
    
    # This is the data for all made shots by Carmelo Anthony in the 2013-2014 season
    else if (input$player == "Carmelo Anthony" & input$shots == "Made Shots" & input$season == "2013-14"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2013-14" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "goldenrod1"))}
    
    # This is the data for all made shots by Carmelo Anthony in the 2012-2013 season
    else if (input$player == "Carmelo Anthony" & input$shots == "Made Shots" & input$season == "2012-13"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2012-13" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "goldenrod1"))}
    
    # This is the data for all made shots by Carmelo Anthony in the 2011-2012 season
    else if (input$player == "Carmelo Anthony" & input$shots == "Made Shots" & input$season == "2011-12"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2011-12" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "goldenrod1"))}
    
    # This is the data for all made shots by Carmelo Anthony in the 2010-2011 season
    else if (input$player == "Carmelo Anthony" & input$shots == "Made Shots" & input$season == "2010-11"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2010-11" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "goldenrod1"))}
    
    # This is the data for all made shots by Carmelo Anthony in the 2009-2010 season
    else if (input$player == "Carmelo Anthony" & input$shots == "Made Shots" & input$season == "2009-10"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2009-10" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "goldenrod1"))}
    
    # This is the data for all made shots by Carmelo Anthony in the 2008-2009 season
    else if (input$player == "Carmelo Anthony" & input$shots == "Made Shots" & input$season == "2008-09"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2008-09" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "goldenrod1"))}
    
    # This is the data for all made shots by Carmelo Anthony in the 2007-2008 season
    else if (input$player == "Carmelo Anthony" & input$shots == "Made Shots" & input$season == "2007-08"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2007-08" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "goldenrod1"))}
    
    # This is the data for all made shots by Carmelo Anthony in the 2006-2007 season
    else if (input$player == "Carmelo Anthony" & input$shots == "Made Shots" & input$season == "2006-07"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2006-07" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "goldenrod1"))}
    
    # This is the data for all made shots by Carmelo Anthony in the 2005-2006 season
    else if (input$player == "Carmelo Anthony" & input$shots == "Made Shots" & input$season == "2005-06"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2005-06" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "goldenrod1"))}
    
    # This is the data for all made shots by Carmelo Anthony in the 2004-2005 season
    else if (input$player == "Carmelo Anthony" & input$shots == "Made Shots" & input$season == "2004-05"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2004-05" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "goldenrod1"))}
    
    # This is the data for all made shots by Carmelo Anthony in the 2003-2004 season
    else if (input$player == "Carmelo Anthony" & input$shots == "Made Shots" & input$season == "2003-04"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2003-04" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "goldenrod1"))}
    
    # This is the data for all missed shots by Carmelo Anthony in the 2017-2018 season
    else if (input$player == "Carmelo Anthony" & input$shots == "Missed Shots" & input$season == "2017-18"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2017-18" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "darkgreen"))}
    
    # This is the data for all missed shots by Carmelo Anthony in the 2016-2017 season
    else if (input$player == "Carmelo Anthony" & input$shots == "Missed Shots" & input$season == "2016-17"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2016-17" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "darkgreen"))}
    
    # This is the data for all missed shots by Carmelo Anthony in the 2015-2016 season
    else if (input$player == "Carmelo Anthony" & input$shots == "Missed Shots" & input$season == "2015-16"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2015-16" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "darkgreen"))}
    
    # This is the data for all missed shots by Carmelo Anthony in the 2014-2015 season
    else if (input$player == "Carmelo Anthony" & input$shots == "Missed Shots" & input$season == "2014-15"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2014-15" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "darkgreen"))}
    
    # This is the data for all missed shots by Carmelo Anthony in the 2013-2014 season
    else if (input$player == "Carmelo Anthony" & input$shots == "Missed Shots" & input$season == "2013-14"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2013-14" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "darkgreen"))}
    
    # This is the data for all missed shots by Carmelo Anthony in the 2012-2013 season
    else if (input$player == "Carmelo Anthony" & input$shots == "Missed Shots" & input$season == "2012-13"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2012-13" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "darkgreen"))}
    
    # This is the data for all missed shots by Carmelo Anthony in the 2011-2012 season
    else if (input$player == "Carmelo Anthony" & input$shots == "Missed Shots" & input$season == "2011-12"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2011-12" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "darkgreen"))}
    
    # This is the data for all missed shots by Carmelo Anthony in the 2010-2011 season
    else if (input$player == "Carmelo Anthony" & input$shots == "Missed Shots" & input$season == "2010-11"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2010-11" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "darkgreen"))}
    
    # This is the data for all missed shots by Carmelo Anthony in the 2009-2010 season
    else if (input$player == "Carmelo Anthony" & input$shots == "Missed Shots" & input$season == "2009-10"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2009-10" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "darkgreen"))}
    
    # This is the data for all missed shots by Carmelo Anthony in the 2008-2009 season
    else if (input$player == "Carmelo Anthony" & input$shots == "Missed Shots" & input$season == "2008-09"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2008-09" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "darkgreen"))}
    
    # This is the data for all missed shots by Carmelo Anthony in the 2007-2008 season
    else if (input$player == "Carmelo Anthony" & input$shots == "Missed Shots" & input$season == "2007-08"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2007-08" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "darkgreen"))}
    
    # This is the data for all missed shots by Carmelo Anthony in the 2006-2007 season
    else if (input$player == "Carmelo Anthony" & input$shots == "Missed Shots" & input$season == "2006-07"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2006-07" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "darkgreen"))}
    
    # This is the data for all missed shots by Carmelo Anthony in the 2005-2006 season
    else if (input$player == "Carmelo Anthony" & input$shots == "Missed Shots" & input$season == "2005-06"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2005-06" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "darkgreen"))}
    
    # This is the data for all missed shots by Carmelo Anthony in the 2004-2005 season
    else if (input$player == "Carmelo Anthony" & input$shots == "Missed Shots" & input$season == "2004-05"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2004-05" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "darkgreen"))}
    
    # This is the data for all missed shots by Carmelo Anthony in the 2003-2004 season
  
    else{
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2003-04" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(missed = "darkgreen"))}
    
  })
  
# Make an image of each player pop up depending on which player input is selected
  output$image <- renderUI({
    if(input$player == "LeBron James"){
      img(
        src = "LeBron James.jpg",
        height = "180 px",
        width = "240 px"
      )
    }
    else if (input$player == "Russell Westbrook"){
      img(
        src = "Russell Westbrook.jpg",
        height = "180 px",
        width = "240 px"
      )
    }
    else if (input$player == "Kevin Durant"){
      img(
        src = "Kevin Durant.jpg",
        height = "175 px",
        width = "225 px"
      )
    }
    else if (input$player == "Stephen Curry"){
      img(
        src = "Stephen Curry.webp",
        height = "180 px",
        width = "240 px"
      )
    }
    else{
      img(
        src = "Carmelo Anthony.jpg",
        height = "180 px",
        width = "240 px"
      )
    }
  })

  })




    



    
    
    
    
    
    

  
