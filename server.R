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
    
    
    
    
    else if (input$player == "Russell Westbrook" & input$shots == "All Shots"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "purple", missed = "yellow"))}
    else if (input$player == "Kevin Durant" & input$shots == "All Shots"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "blue", missed = "green"))}
    else if (input$player == "Stephen Curry" & input$shots == "All Shots"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "orange", missed = "pink"))}
    else if (input$player == "Carmelo Anothony" & input$shots == "All Shots"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "brown", missed = "light blue"))}
    else if (input$player == "LeBron James" & input$shots == "Made Shots"){
      source("helpers.R")  
    gg_court = make_court()
    graph_data = filter(nba_shots, player_name == "LeBron James")
    gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                          aes(loc_x, loc_y, color = "made")) + scale_color_manual("", values = c(made = "red"))} 
    else if (input$player == "Russell Westbrook" & input$shots == "Made Shots"){
        source("helpers.R")
        gg_court = make_court()
        graph_data = filter(nba_shots, player_name == "Russell Westbrook")
        gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                              aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "purple"))}
    else if (input$player == "Kevin Durant" & input$shots == "Made Shots"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "blue"))}
    else if (input$player == "Stephen Curry" & input$shots == "Made Shots"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "orange"))}
    else if (input$player == "Carmelo Anothony" & input$shots == "Made Shots"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "brown"))}
    else if (input$player == "LeBron James" & input$shots == "Missed Shots"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = "missed")) + scale_color_manual("", values = c(missed = "black"))}
    else if (input$player == "Russell Westbrook" & input$shots == "Missed Shots"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(missed = "yellow"))}
    else if (input$player == "Kevin Durant" & input$shots == "Missed Shots"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(missed = "green"))}
    else if (input$player == "Stephen Curry" & input$shots == "Missed Shots"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(missed = "pink"))}
    else{
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(missed = "light blue"))}
    
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




    



    
    
    
    
    
    

  
