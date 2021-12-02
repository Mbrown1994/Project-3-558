# Packages used for this analysis
library(shinythemes)
library(tidyverse)
library(magrittr)
library(shiny)

# Read in the data
fileName <- "./NBAdata.csv"
nba_shots <- read_csv(
  fileName,
  col_types = cols()
)


# Define server logic 
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
  
  # Equation for Logistic Regression to be used in the modeling info tab
  output$logEq <- renderUI({
    withMathJax(
      helpText(
        "$$\\ln(\\frac{p_i}{1-p_i}) = \\beta_0 + \\Sigma^k_{j=1}\\beta_jx_{ij}$$"
      )
    )
  })
  
   # Create a plot for Data Exploration page
  output$BasketballPlot <- renderPlot({
    
    # This is the data for all shots by LeBron James in the 2017-2018 season
    if (input$player == "LeBron James" & input$shots == "All Shots" & input$season == "2017-18" & input$PlotChoice == "Basketball Court"){
    source("helpers.R")  
    gg_court = make_court()
    graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2017-18")
    gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                          aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "red", missed = "black"))}
    
    # This is the data for all shots by LeBron James in the 2016-2017 season
    else if (input$player == "LeBron James" & input$shots == "All Shots" & input$season == "2016-17" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2016-17" & input$PlotChoice == "Basketball Court")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "red", missed = "black"))}
    
    # This is the data for all shots by LeBron James in the 2015-2016 season
    else if (input$player == "LeBron James" & input$shots == "All Shots" & input$season == "2015-16" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2015-16")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "red", missed = "black"))}
    
    # This is the data for all shots by LeBron James in the 2014-2015 season
    else if (input$player == "LeBron James" & input$shots == "All Shots" & input$season == "2014-15" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2014-15")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "red", missed = "black"))}
    
    # This is the data for all shots by LeBron James in the 2013-2014 season
    else if (input$player == "LeBron James" & input$shots == "All Shots" & input$season == "2013-14" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2013-14")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "red", missed = "black"))}
    
    # This is the data for all shots by LeBron James in the 2012-2013 season
    else if (input$player == "LeBron James" & input$shots == "All Shots" & input$season == "2012-13" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2012-13")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "red", missed = "black"))}
    
    # This is the data for all shots by LeBron James in the 2011-2012 season
    else if (input$player == "LeBron James" & input$shots == "All Shots" & input$season == "2011-12" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2011-12")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "red", missed = "black"))}
    
    # This is the data for all shots by LeBron James in the 2010-2011 season
    else if (input$player == "LeBron James" & input$shots == "All Shots" & input$season == "2010-11" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2010-11")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "red", missed = "black"))}
    
    # This is the data for all shots by LeBron James in the 2009-2010 season
    else if (input$player == "LeBron James" & input$shots == "All Shots" & input$season == "2009-10" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2009-10")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "red", missed = "black"))}
    
    # This is the data for all shots by LeBron James in the 2008-2009 season
    else if (input$player == "LeBron James" & input$shots == "All Shots" & input$season == "2008-09" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2008-09")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "red", missed = "black"))}
    
    # This is the data for all shots by LeBron James in the 2007-2008 season
    else if (input$player == "LeBron James" & input$shots == "All Shots" & input$season == "2007-08" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2007-08")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "red", missed = "black"))}
    
    # This is the data for all shots by LeBron James in the 2006-2007 season
    else if (input$player == "LeBron James" & input$shots == "All Shots" & input$season == "2006-07" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2006-07")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "red", missed = "black"))}
    
    # This is the data for all shots by LeBron James in the 2005-2006 season
    else if (input$player == "LeBron James" & input$shots == "All Shots" & input$season == "2005-06" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2005-06")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "red", missed = "black"))}
    
    # This is the data for all shots by LeBron James in the 2004-2005 season
    else if (input$player == "LeBron James" & input$shots == "All Shots" & input$season == "2004-05" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2004-05")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "red", missed = "black"))}
    
    # This is the data for all shots by LeBron James in the 2003-2004 season
    else if (input$player == "LeBron James" & input$shots == "All Shots" & input$season == "2003-04" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2003-04")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "red", missed = "black"))}
    
    # This is the data for made shots by Lebron James in the 2017-2018 season
    else if (input$player == "LeBron James" & input$shots == "Made Shots" & input$season == "2017-18" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2017-18" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "red"))}
    
    # This is the data for made shots by LeBron James in the 2016-2017 season
    else if (input$player == "LeBron James" & input$shots == "Made Shots" & input$season == "2016-17" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2016-17" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "red"))}
    
    # This is the data for made shots by LeBron James in the 2015-2016 season
    else if (input$player == "LeBron James" & input$shots == "Made Shots" & input$season == "2015-16" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2015-16" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "red"))}
    
    # This is the data for made shots by LeBron James in the 2014-2015 season
    else if (input$player == "LeBron James" & input$shots == "Made Shots" & input$season == "2014-15" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2014-15" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "red"))}
    
    # This is the data for made shots by LeBron James in the 2013-2014 season
    else if (input$player == "LeBron James" & input$shots == "Made Shots" & input$season == "2013-14" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2013-14" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "red"))}
    
    # This is the data for made shots by LeBron James in the 2012-2013 season
    else if (input$player == "LeBron James" & input$shots == "Made Shots" & input$season == "2012-13" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2012-13" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "red"))}
    
    # This is the data for made shots by LeBron James in the 2011-2012 season
    else if (input$player == "LeBron James" & input$shots == "Made Shots" & input$season == "2011-12" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2011-12" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "red"))}
    
    # This is the data for made shots by LeBron James in the 2010-2011 season
    else if (input$player == "LeBron James" & input$shots == "Made Shots" & input$season == "2010-11" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2010-11" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "red"))}
    
    # This is the data for made shots by LeBron James in the 2009-2010 season
    else if (input$player == "LeBron James" & input$shots == "Made Shots" & input$season == "2009-10" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2009-10" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "red"))}
    
    # This is the data for made shots by LeBron James in the 2008-2009 season
    else if (input$player == "LeBron James" & input$shots == "Made Shots" & input$season == "2008-09" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2008-09" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "red"))}
    
    # This is the data for made shots by LeBron James in the 2007-2008 season
    else if (input$player == "LeBron James" & input$shots == "Made Shots" & input$season == "2007-08" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2007-08" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "red"))}
    
    # This is the data for made shots by LeBron James in the 2006-2007 season
    else if (input$player == "LeBron James" & input$shots == "Made Shots" & input$season == "2006-07" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2006-07" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "red"))}
    
    # This is the data for made shots by LeBron James in the 2005-2006 season
    else if (input$player == "LeBron James" & input$shots == "Made Shots" & input$season == "2005-06" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2005-06" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "red"))}
    
    # This is the data for made shots by LeBron James in the 2004-2005 season
    else if (input$player == "LeBron James" & input$shots == "Made Shots" & input$season == "2004-05" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2004-05" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "red"))}
    
    # This is the data for made shots by LeBron James in the 2003-2004 season
    else if (input$player == "LeBron James" & input$shots == "Made Shots" & input$season == "2003-04" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2003-04" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "red"))}
    
    # This is the data for missed shots by LeBron James in the 2017-2018 season
    else if (input$player == "LeBron James" & input$shots == "Missed Shots" & input$season == "2017-18" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2017-18" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "black"))}
    
    # This is the data for missed shots by LeBron James in the 2016-2017 season
    else if (input$player == "LeBron James" & input$shots == "Missed Shots" & input$season == "2016-17" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2016-17" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "black"))}
    
    # This is the data for missed shots by LeBron James in the 2015-2016 season
    else if (input$player == "LeBron James" & input$shots == "Missed Shots" & input$season == "2015-16" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2015-16" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "black"))}
    
    # This is the data for missed shots by LeBron James in the 2014-2015 season
    else if (input$player == "LeBron James" & input$shots == "Missed Shots" & input$season == "2014-15" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2014-15" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "black"))}
    
    # This is the data for missed shots by LeBron James in the 2013-2014 season
    else if (input$player == "LeBron James" & input$shots == "Missed Shots" & input$season == "2013-14" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2013-14" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "black"))}
    
    # This is the data for missed shots by LeBron James in the 2012-2013 season
    else if (input$player == "LeBron James" & input$shots == "Missed Shots" & input$season == "2012-13" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2012-13" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "black"))}
    
    # This is the data for missed shots by LeBron James in the 2011-2012 season
    else if (input$player == "LeBron James" & input$shots == "Missed Shots" & input$season == "2011-12" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2011-12" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "black"))}
    
    # This is the data for missed shots by LeBron James in the 2010-2011 season
    else if (input$player == "LeBron James" & input$shots == "Missed Shots" & input$season == "2010-11" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2010-11" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "black"))}
    
    # This is the data for missed shots by LeBron James in the 2009-2010 season
    else if (input$player == "LeBron James" & input$shots == "Missed Shots" & input$season == "2009-10" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2009-10" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "black"))}
    
    # This is the data for missed shots by LeBron James in the 2008-2009 season
    else if (input$player == "LeBron James" & input$shots == "Missed Shots" & input$season == "2008-09" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2008-09" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "black"))}
    
    # This is the data for missed shots by LeBron James in the 2007-2008 season
    else if (input$player == "LeBron James" & input$shots == "Missed Shots" & input$season == "2007-08" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2007-08" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "black"))}
    
    # This is the data for missed shots by LeBron James in the 2006-2007 season
    else if (input$player == "LeBron James" & input$shots == "Missed Shots" & input$season == "2006-07" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2006-07" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "black"))}
    
    # This is the data for missed shots by LeBron James in the 2005-2006 season
    else if (input$player == "LeBron James" & input$shots == "Missed Shots" & input$season == "2005-06" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2005-06" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "black"))}
    
    # This is the data for missed shots by LeBron James in the 2004-2005 season
    else if (input$player == "LeBron James" & input$shots == "Missed Shots" & input$season == "2004-05" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2004-05" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "black"))}
    
    # This is the data for missed shots by LeBron James in the 2003-2004 season
    else if (input$player == "LeBron James" & input$shots == "Missed Shots" & input$season == "2003-04" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")  
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "LeBron James" & season == "2003-04" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "black"))}
    
    # This is the data for all shots by Russell Westbrook in the 2017-2018 season
    else if (input$player == "Russell Westbrook" & input$shots == "All Shots" & input$season == "2017-18" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2017-18")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "purple", missed = "yellow3"))}
    
    # This is the data for all shots by Russell Westbrook in the 2016-2017 season
    else if (input$player == "Russell Westbrook" & input$shots == "All Shots" & input$season == "2016-17" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2016-17")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "purple", missed = "yellow3"))}
    
    # This is the data for all shots by Russell Westbrook in the 2015-2016 season
    else if (input$player == "Russell Westbrook" & input$shots == "All Shots" & input$season == "2015-16" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2015-16")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "purple", missed = "yellow3"))}
    
    # This is the data for all shots by Russell Westbrook in the 2014-2015 season
    else if (input$player == "Russell Westbrook" & input$shots == "All Shots" & input$season == "2014-15" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2014-15")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "purple", missed = "yellow3"))}
    
    # This is the data for all shots by Russell Westbrook in the 2013-2014 season
    else if (input$player == "Russell Westbrook" & input$shots == "All Shots" & input$season == "2013-14" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2013-14")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "purple", missed = "yellow3"))}
    
    # This is the data for all shots by Russell Westbrook in the 2012-2013 season
    else if (input$player == "Russell Westbrook" & input$shots == "All Shots" & input$season == "2012-13" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2012-13")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "purple", missed = "yellow3"))}
    
    # This is the data for all shots by Russell Westbrook in the 2011-2012 season
    else if (input$player == "Russell Westbrook" & input$shots == "All Shots" & input$season == "2011-12" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2011-12")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "purple", missed = "yellow3"))}
    
    # This is the data for all shots by Russell Westbrook in the 2010-2011 season
    else if (input$player == "Russell Westbrook" & input$shots == "All Shots" & input$season == "2010-11" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2010-11")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "purple", missed = "yellow3"))}
    
    # This is the data for all shots by Russell Westbrook in the 2009-2010 season
    else if (input$player == "Russell Westbrook" & input$shots == "All Shots" & input$season == "2009-10" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2009-10")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "purple", missed = "yellow3"))}
    
    # This is the data for all shots by Russell Westbrook in the 2008-2009 season
    else if (input$player == "Russell Westbrook" & input$shots == "All Shots" & input$season == "2008-09" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2008-09")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "purple", missed = "yellow3"))}
    
    # This is the data for all shots by Russell Westbrook in the 2007-2008 season
    else if (input$player == "Russell Westbrook" & input$shots == "All Shots" & input$season == "2007-08" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2007-08")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "purple", missed = "yellow3"))}
    
    # This is the data for all shots by Russell Westbrook in the 2006-2007 season
    else if (input$player == "Russell Westbrook" & input$shots == "All Shots" & input$season == "2006-07" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2006-07")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "purple", missed = "yellow3"))}
    
    # This is the data for all shots by Russell Westbrook in the 2005-2006 season
    else if (input$player == "Russell Westbrook" & input$shots == "All Shots" & input$season == "2005-06" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2005-06")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "purple", missed = "yellow3"))}
    
    # This is the data for all shots by Russell Westbrook in the 2004-2005 season
    else if (input$player == "Russell Westbrook" & input$shots == "All Shots" & input$season == "2004-05" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2004-05")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "purple", missed = "yellow3"))}
    
    # This is the data for all shots by Russell Westbrook in the 2003-2004 season
    else if (input$player == "Russell Westbrook" & input$shots == "All Shots" & input$season == "2003-04" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2003-04")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "purple", missed = "yellow3"))}
    
    # This is the data for all made shots by Russell Westbrook in the 2017-2018 season
    else if (input$player == "Russell Westbrook" & input$shots == "Made Shots" & input$season == "2017-18" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2017-18" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "purple"))}
    
    # This is the data for all made shots by Russell Westbrook in the 2016-2017 season
    else if (input$player == "Russell Westbrook" & input$shots == "Made Shots" & input$season == "2016-17" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2016-17" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "purple"))}
    
    # This is the data for all made shots by Russell Westbrook in the 2015-2016 season
    else if (input$player == "Russell Westbrook" & input$shots == "Made Shots" & input$season == "2015-16" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2015-16" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "purple"))}
    
    # This is the data for all made shots by Russell Westbrook in the 2014-2015 season
    else if (input$player == "Russell Westbrook" & input$shots == "Made Shots" & input$season == "2014-15" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2014-15" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "purple"))}
    
    # This is the data for all made shots by Russell Westbrook in the 2013-2014 season
    else if (input$player == "Russell Westbrook" & input$shots == "Made Shots" & input$season == "2013-14" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2013-14" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "purple"))}
    
    # This is the data for all made shots by Russell Westbrook in the 2012-2013 season
    else if (input$player == "Russell Westbrook" & input$shots == "Made Shots" & input$season == "2012-13" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2012-13" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "purple"))}
    
    # This is the data for all made shots by Russell Westbrook in the 2011-2012 season
    else if (input$player == "Russell Westbrook" & input$shots == "Made Shots" & input$season == "2011-12" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2011-12" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "purple"))}
    
    # This is the data for all made shots by Russell Westbrook in the 2010-2011 season
    else if (input$player == "Russell Westbrook" & input$shots == "Made Shots" & input$season == "2010-11" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2010-11" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "purple"))}
    
    # This is the data for all made shots by Russell Westbrook in the 2009-2010 season
    else if (input$player == "Russell Westbrook" & input$shots == "Made Shots" & input$season == "2009-10" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2009-10" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "purple"))}
    
    # This is the data for all made shots by Russell Westbrook in the 2008-2009 season
    else if (input$player == "Russell Westbrook" & input$shots == "Made Shots" & input$season == "2008-09" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2008-09" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "purple"))}
    
    # This is the data for all made shots by Russell Westbrook in the 2007-2008 season
    else if (input$player == "Russell Westbrook" & input$shots == "Made Shots" & input$season == "2007-08" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2007-08" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "purple"))}
    
    # This is the data for all made shots by Russell Westbrook in the 2006-2007 season
    else if (input$player == "Russell Westbrook" & input$shots == "Made Shots" & input$season == "2006-07" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2006-07" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "purple"))}
    
    # This is the data for all made shots by Russell Westbrook in the 2005-2006 season
    else if (input$player == "Russell Westbrook" & input$shots == "Made Shots" & input$season == "2005-06" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2005-06" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "purple"))}
    
    # This is the data for all made shots by Russell Westbrook in the 2004-2005 season
    else if (input$player == "Russell Westbrook" & input$shots == "Made Shots" & input$season == "2004-05" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2004-05" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "purple"))}
    
    # This is the data for all made shots by Russell Westbrook in the 2003-2004 season
    else if (input$player == "Russell Westbrook" & input$shots == "Made Shots" & input$season == "2003-04" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2003-04" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "purple"))}
    
    # This is the data for all missed shots by Russell Westbrook in the 2017-2018 season
    else if (input$player == "Russell Westbrook" & input$shots == "Missed Shots" & input$season == "2017-18" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2017-18" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(missed = "yellow3"))}
    
    # This is the data for all missed shots by Russell Westbrook in the 2016-2017 season
    else if (input$player == "Russell Westbrook" & input$shots == "Missed Shots" & input$season == "2016-17" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2016-17" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(missed = "yellow3"))}
    
    # This is the data for all missed shots by Russell Westbrook in the 2015-2016 season
    else if (input$player == "Russell Westbrook" & input$shots == "Missed Shots" & input$season == "2015-16" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2015-16" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(missed = "yellow3"))}
    
    # This is the data for all missed shots by Russell Westbrook in the 2014-2015 season
    else if (input$player == "Russell Westbrook" & input$shots == "Missed Shots" & input$season == "2014-15" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2014-15" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(missed = "yellow3"))}
    
    # This is the data for all missed shots by Russell Westbrook in the 2013-2014 season
    else if (input$player == "Russell Westbrook" & input$shots == "Missed Shots" & input$season == "2013-14" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2013-14" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(missed = "yellow3"))}
    
    # This is the data for all missed shots by Russell Westbrook in the 2012-2013 season
    else if (input$player == "Russell Westbrook" & input$shots == "Missed Shots" & input$season == "2012-13" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2012-13" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(missed = "yellow3"))}
    
    # This is the data for all missed shots by Russell Westbrook in the 2011-2012 season
    else if (input$player == "Russell Westbrook" & input$shots == "Missed Shots" & input$season == "2011-12" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2011-12" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(missed = "yellow3"))}
    
    # This is the data for all missed shots by Russell Westbrook in the 2010-2011 season
    else if (input$player == "Russell Westbrook" & input$shots == "Missed Shots" & input$season == "2010-11" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2010-11" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(missed = "yellow3"))}
    
    # This is the data for all missed shots by Russell Westbrook in the 2009-2010 season
    else if (input$player == "Russell Westbrook" & input$shots == "Missed Shots" & input$season == "2009-10" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2009-10" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(missed = "yellow3"))}
    
    # This is the data for all missed shots by Russell Westbrook in the 2008-2009 season
    else if (input$player == "Russell Westbrook" & input$shots == "Missed Shots" & input$season == "2008-09" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2008-09" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(missed = "yellow3"))}
    
    # This is the data for all missed shots by Russell Westbrook in the 2007-2008 season
    else if (input$player == "Russell Westbrook" & input$shots == "Missed Shots" & input$season == "2007-08" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2007-08" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(missed = "yellow3"))}
    
    # This is the data for all missed shots by Russell Westbrook in the 2006-2007 season
    else if (input$player == "Russell Westbrook" & input$shots == "Missed Shots" & input$season == "2006-07" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2006-07" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(missed = "yellow3"))}
    
    # This is the data for all missed shots by Russell Westbrook in the 2005-2006 season
    else if (input$player == "Russell Westbrook" & input$shots == "Missed Shots" & input$season == "2005-06" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2005-06" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(missed = "yellow3"))}
    
    # This is the data for all missed shots by Russell Westbrook in the 2004-2005 season
    else if (input$player == "Russell Westbrook" & input$shots == "Missed Shots" & input$season == "2004-05" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2004-05" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(missed = "yellow3"))}
    
    # This is the data for all missed shots by Russell Westbrook in the 2003-2004 season
    else if (input$player == "Russell Westbrook" & input$shots == "Missed Shots" & input$season == "2003-04" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Russell Westbrook" & season == "2003-04" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(missed = "yellow3"))}
    
    # This is the data for all shots by Kevin Durant in the 2017-2018 season
    else if (input$player == "Kevin Durant" & input$shots == "All Shots" & input$season == "2017-18" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2017-18")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "blue", missed = "green"))}
    
    # This is the data for all shots by Kevin Durant in the 2016-2017 season
    else if (input$player == "Kevin Durant" & input$shots == "All Shots" & input$season == "2016-17" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2016-17")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "blue", missed = "green"))}
    
    # This is the data for all shots by Kevin Durant in the 2015-2016 season
    else if (input$player == "Kevin Durant" & input$shots == "All Shots" & input$season == "2015-16" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2015-16")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "blue", missed = "green"))}
    
    # This is the data for all shots by Kevin Durant in the 2014-2015 season
    else if (input$player == "Kevin Durant" & input$shots == "All Shots" & input$season == "2014-15" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2014-15")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "blue", missed = "green"))}
    
    # This is the data for all shots by Kevin Durant in the 2013-2014 season
    else if (input$player == "Kevin Durant" & input$shots == "All Shots" & input$season == "2013-14" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2013-14")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "blue", missed = "green"))}
    
    # This is the data for all shots by Kevin Durant in the 2012-2013 season
    else if (input$player == "Kevin Durant" & input$shots == "All Shots" & input$season == "2012-13" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2012-13")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "blue", missed = "green"))}
    
    # This is the data for all shots by Kevin Durant in the 2011-2012 season
    else if (input$player == "Kevin Durant" & input$shots == "All Shots" & input$season == "2011-12" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2011-12")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "blue", missed = "green"))}
    
    # This is the data for all shots by Kevin Durant in the 2010-2011 season
    else if (input$player == "Kevin Durant" & input$shots == "All Shots" & input$season == "2010-11" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2010-11")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "blue", missed = "green"))}
    
    # This is the data for all shots by Kevin Durant in the 2009-2010 season
    else if (input$player == "Kevin Durant" & input$shots == "All Shots" & input$season == "2009-10" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2009-10")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "blue", missed = "green"))}
    
     # This is the data for all shots by Kevin Durant in the 2008-2009 season
    else if (input$player == "Kevin Durant" & input$shots == "All Shots" & input$season == "2008-09" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2008-09")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "blue", missed = "green"))}
    
    # This is the data for all shots by Kevin Durant in the 2007-2008 season
    else if (input$player == "Kevin Durant" & input$shots == "All Shots" & input$season == "2007-08" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2007-08")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "blue", missed = "green"))}
    
    # This is the data for all shots by Kevin Durant in the 2006-2007 season
    else if (input$player == "Kevin Durant" & input$shots == "All Shots" & input$season == "2006-07" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2006-07")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "blue", missed = "green"))}
    
    # This is the data for all shots by Kevin Durant in the 2005-2006 season
    else if (input$player == "Kevin Durant" & input$shots == "All Shots" & input$season == "2005-06" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2005-06")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "blue", missed = "green"))}
    
    # This is the data for all shots by Kevin Durant in the 2004-2005 season
    else if (input$player == "Kevin Durant" & input$shots == "All Shots" & input$season == "2004-05" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2004-05")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "blue", missed = "green"))}
    
    # This is the data for all shots by Kevin Durant in the 2003-2004 season
    else if (input$player == "Kevin Durant" & input$shots == "All Shots" & input$season == "2003-04" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2003-04")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "blue", missed = "green"))}
    
    # This is the data for all made shots by Kevin Durant in the 2017-2018 season
    else if (input$player == "Kevin Durant" & input$shots == "Made Shots" & input$season == "2017-18" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2017-18" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "blue"))}
    
    # This is the data for all made shots by Kevin Durant in the 2016-2017 season
    else if (input$player == "Kevin Durant" & input$shots == "Made Shots" & input$season == "2016-17" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2016-17" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "blue"))}
    
    # This is the data for all made shots by Kevin Durant in the 2015-2016 season
    else if (input$player == "Kevin Durant" & input$shots == "Made Shots" & input$season == "2015-16" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2015-16" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "blue"))}
    
    # This is the data for all made shots by Kevin Durant in the 2014-2015
    else if (input$player == "Kevin Durant" & input$shots == "Made Shots" & input$season == "2014-15" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2014-15" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "blue"))}
    
    # This is the data for all made shots by Kevin Durant in the 2013-2014 season
    else if (input$player == "Kevin Durant" & input$shots == "Made Shots" & input$season == "2013-14" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2013-14" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "blue"))}
    
    # This is the data for all made shots by Kevin Durant in the 2012-2013 season
    else if (input$player == "Kevin Durant" & input$shots == "Made Shots" & input$season == "2012-13" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2012-13" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "blue"))}
    
    # This is the data for all made shots by Kevin Durant in the 2011-2012 season
    else if (input$player == "Kevin Durant" & input$shots == "Made Shots" & input$season == "2011-12" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2011-12" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "blue"))}
    
    # This is the data for all made shots by Kevin Durant in the 2010-2011 season
    else if (input$player == "Kevin Durant" & input$shots == "Made Shots" & input$season == "2010-11" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2010-11" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "blue"))}
    
    # This is the data for all made shots by Kevin Durant in the 2009-2010 season
    else if (input$player == "Kevin Durant" & input$shots == "Made Shots" & input$season == "2009-10" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2009-10" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "blue"))}
    
    # This is the data for all made shots by Kevin Durant in the 2008-2009 season
    else if (input$player == "Kevin Durant" & input$shots == "Made Shots" & input$season == "2008-09" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2008-09" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "blue"))}
    
    # This is the data for all made shots by Kevin Durant in the 2007-2008 season
    else if (input$player == "Kevin Durant" & input$shots == "Made Shots" & input$season == "2007-08" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2007-08" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "blue"))}
    
    # This is the data for all made shots by Kevin Durant in the 2006-2007 season
    else if (input$player == "Kevin Durant" & input$shots == "Made Shots" & input$season == "2006-07" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2006-07" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "blue"))}
    
    # This is the data for all made shots by Kevin Durant in the 2005-2006 season
    else if (input$player == "Kevin Durant" & input$shots == "Made Shots" & input$season == "2005-06" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2005-06" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "blue"))}
    
    # This is the data for all made shots by Kevin Durant in the 2004-2005 season
    else if (input$player == "Kevin Durant" & input$shots == "Made Shots" & input$season == "2004-05" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2004-05" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "blue"))}
    
    # This is the data for all made shots by Kevin Durant in the 2003-2004 season
    else if (input$player == "Kevin Durant" & input$shots == "Made Shots" & input$season == "2003-04" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2003-04" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "blue"))}
    
    # This is the data for all missed shots by Kevin Durant in the 2017-2018 season
    else if (input$player == "Kevin Durant" & input$shots == "Missed Shots" & input$season == "2017-18" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2017-18" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "green"))}
    
    # This is the data for all missed shots by Kevin Durant in the 2016-2017 season
    else if (input$player == "Kevin Durant" & input$shots == "Missed Shots" & input$season == "2016-17" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2017-18" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "green"))}
    
    # This is the data for all missed shots by Kevin Durant in the 2015-2016 season
    else if (input$player == "Kevin Durant" & input$shots == "Missed Shots" & input$season == "2015-16" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2015-16" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "green"))}
    
    # This is the data for all missed shots by Kevin Durant in the 2014-2015 season
    else if (input$player == "Kevin Durant" & input$shots == "Missed Shots" & input$season == "2014-15" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2014-15" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "green"))}
    
    # This is the data for all missed shots by Kevin Durant in the 2013-2014 season
    else if (input$player == "Kevin Durant" & input$shots == "Missed Shots" & input$season == "2013-14" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2013-14" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "green"))}
    
    # This is the data for all missed shots by Kevin Durant in the 2012-2013 season
    else if (input$player == "Kevin Durant" & input$shots == "Missed Shots" & input$season == "2012-13" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2012-13" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "green"))}
    
    # This is the data for all missed shots by Kevin Durant in the 2011-2012 season
    else if (input$player == "Kevin Durant" & input$shots == "Missed Shots" & input$season == "2011-12" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2011-12" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "green"))}
    
    # This is the data for all missed shots by Kevin Durant in the 2010-2011 season
    else if (input$player == "Kevin Durant" & input$shots == "Missed Shots" & input$season == "2010-11" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2010-11" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "green"))}
    
    # This is the data for all missed shots by Kevin Durant in the 2009-2010 season
    else if (input$player == "Kevin Durant" & input$shots == "Missed Shots" & input$season == "2009-10" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2009-10" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "green"))}
    
    # This is the data for all missed shots by Kevin Durant in the 2008-2009 season
    else if (input$player == "Kevin Durant" & input$shots == "Missed Shots" & input$season == "2008-09" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2008-09" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "green"))}
    
    # This is the data for all missed shots by Kevin Durant in the 2007-2008 season
    else if (input$player == "Kevin Durant" & input$shots == "Missed Shots" & input$season == "2007-08" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2007-08" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "green"))}
    
    # This is the data for all missed shots by Kevin Durant in the 2006-2007 season
    else if (input$player == "Kevin Durant" & input$shots == "Missed Shots" & input$season == "2006-07" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2006-07" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "green"))}
    
    # This is the data for all missed shots by Kevin Durant in the 2005-2006 season
    else if (input$player == "Kevin Durant" & input$shots == "Missed Shots" & input$season == "2005-06" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2005-06" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "green"))}
    
    # This is the data for all missed shots by Kevin Durant in the 2004-2005 season
    else if (input$player == "Kevin Durant" & input$shots == "Missed Shots" & input$season == "2004-05" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2004-05" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "green"))}
    
    # This is the data for all missed shots by Kevin Durant in the 2003-2004 season
    else if (input$player == "Kevin Durant" & input$shots == "Missed Shots" & input$season == "2003-04" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Kevin Durant" & season == "2003-04" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "green"))}
    
    # This is the data for all shots by Stephen Curry in the 2017-2018 season
    else if (input$player == "Stephen Curry" & input$shots == "All Shots" & input$season == "2017-18" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2017-18")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "orange", missed = "steel blue"))}
    
    # This is the data for all shots by Stephen Curry in the 2016-2017 season
    else if (input$player == "Stephen Curry" & input$shots == "All Shots" & input$season == "2016-17" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2016-17")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "orange", missed = "steel blue"))}
    
    # This is the data for all shots by Stephen Curry in the 2015-2016 season
    else if (input$player == "Stephen Curry" & input$shots == "All Shots" & input$season == "2015-16" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2015-16")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "orange", missed = "steel blue"))}
    
    # This is the data for all shots by Stephen Curry in the 2014-2015 season
    else if (input$player == "Stephen Curry" & input$shots == "All Shots" & input$season == "2014-15" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2014-15")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "orange", missed = "steel blue"))}
    
    # This is the data for all shots by Stephen Curry in the 2013-2014 season
    else if (input$player == "Stephen Curry" & input$shots == "All Shots" & input$season == "2013-14" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2013-14")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "orange", missed = "steel blue"))}
    
    # This is the data for all shots by Stephen Curry in the 2012-2013 season
    else if (input$player == "Stephen Curry" & input$shots == "All Shots" & input$season == "2012-13" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2012-13")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "orange", missed = "steel blue"))}
    
    # This is the data for all shots by Stephen Curry in the 2011-2012 season
    else if (input$player == "Stephen Curry" & input$shots == "All Shots" & input$season == "2011-12" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2011-12")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "orange", missed = "steel blue"))}
    
    # This is the data for all shots by Stephen Curry in the 2010-2011 season
    else if (input$player == "Stephen Curry" & input$shots == "All Shots" & input$season == "2010-11" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2010-11")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "orange", missed = "steel blue"))}
    
    # This is the data for all shots by Stephen Curry in the 2009-2010
    else if (input$player == "Stephen Curry" & input$shots == "All Shots" & input$season == "2009-10" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2009-10")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "orange", missed = "steel blue"))}
    
    # This is the data for all shots by Stephen Curry in the 2008-2009 season
    else if (input$player == "Stephen Curry" & input$shots == "All Shots" & input$season == "2008-09" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2008-09")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "orange", missed = "steel blue"))}
    
    # This is the data for all shots by Stephen Curry in the 2007-2008 season
    else if (input$player == "Stephen Curry" & input$shots == "All Shots" & input$season == "2007-08" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2007-08")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "orange", missed = "steel blue"))}
    
    # This is the data for all shots by Stephen Curry in the 2006-2007 season
    else if (input$player == "Stephen Curry" & input$shots == "All Shots" & input$season == "2006-07" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2006-07")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "orange", missed = "steel blue"))}
    
    # This is the data for all shots by Stephen Curry in the 2005-2006 season
    else if (input$player == "Stephen Curry" & input$shots == "All Shots" & input$season == "2005-06" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2005-06")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "orange", missed = "steel blue"))}
    
    # This is the data for all shots by Stephen Curry in the 2004-2005 season
    else if (input$player == "Stephen Curry" & input$shots == "All Shots" & input$season == "2004-05" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2004-05")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "orange", missed = "steel blue"))}
    
    # This is the data for all shots by Stephen Curry in the 2003-2004 season
    else if (input$player == "Stephen Curry" & input$shots == "All Shots" & input$season == "2003-04" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2003-04")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "orange", missed = "steel blue"))}
    
    # This is the data for all made shots by Stephen Curry in the 2017-2018 season
    else if (input$player == "Stephen Curry" & input$shots == "Made Shots" & input$season == "2017-18" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2017-18" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "orange"))}
    
    # This is the data for all made shots by Stephen Curry in the 2016-2017 season
    else if (input$player == "Stephen Curry" & input$shots == "Made Shots" & input$season == "2016-17" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2016-17" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "orange"))}
    
    # This is the data for all made shots by Stephen Curry in the 2015-2016 season
    else if (input$player == "Stephen Curry" & input$shots == "Made Shots" & input$season == "2015-16" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2015-16" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "orange"))}
    
    # This is the data for all made shots by Stephen Curry in the 2014- 2015 season
    else if (input$player == "Stephen Curry" & input$shots == "Made Shots" & input$season == "2014-15" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2014-15" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "orange"))}
    
    # This is the data for all made shots by Stephen Curry in the 2013-2014 season
    else if (input$player == "Stephen Curry" & input$shots == "Made Shots" & input$season == "2013-14" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2013-14" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "orange"))}
    
    # This is the data for all made shots by Stephen Curry in the 2012-2013 season
    else if (input$player == "Stephen Curry" & input$shots == "Made Shots" & input$season == "2012-13" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2012-13" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "orange"))}
    
    # This is the data for all made shots by Stephen Curry in the 2011-2012
    else if (input$player == "Stephen Curry" & input$shots == "Made Shots" & input$season == "2011-12" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2011-12" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "orange"))}
    
    # This is the data for all made shots by Stephen Curry in the 2010-2011 season
    else if (input$player == "Stephen Curry" & input$shots == "Made Shots" & input$season == "2010-11" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2010-11" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "orange"))}
    
    # This is the data for all made shots by Stephen Curry in the 2009-2010 season
    else if (input$player == "Stephen Curry" & input$shots == "Made Shots" & input$season == "2009-10" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2009-10" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "orange"))}
    
    # This is the data for all made shots by Stephen Curry in the 2008-2009 season
    else if (input$player == "Stephen Curry" & input$shots == "Made Shots" & input$season == "2008-09" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2008-09" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "orange"))}
    
    # This is the data for all made shots by Stephen Curry in the 2007-2008 season
    else if (input$player == "Stephen Curry" & input$shots == "Made Shots" & input$season == "2007-08" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2007-08" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "orange"))}
    
    # This is the data for all made shots by Stephen Curry in the 2006-2007 season
    else if (input$player == "Stephen Curry" & input$shots == "Made Shots" & input$season == "2006-07" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2006-07" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "orange"))}
    
    # This is the data for all made shots by Stephen Curry in the 2005-2006 season
    else if (input$player == "Stephen Curry" & input$shots == "Made Shots" & input$season == "2005-06" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2005-06" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "orange"))}
    
    # This is the data for all made shots by Stephen Curry in the 2004-2005 season
    else if (input$player == "Stephen Curry" & input$shots == "Made Shots" & input$season == "2004-05" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2004-05" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "orange"))}
    
    # This is the data for all made shots by Stephen Curry in the 2003-2004 season
    else if (input$player == "Stephen Curry" & input$shots == "Made Shots" & input$season == "2003-04" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2003-04" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "orange"))}
    
    # This is the data for all missed shots by Stephen Curry in the 2017-2018 season
    else if (input$player == "Stephen Curry" & input$shots == "Missed Shots" & input$season == "2017-18" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2017-18" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "steel blue"))}
    
    # This is the data for all missed shots by Stephen Curry in the 2016-2017 season
    else if (input$player == "Stephen Curry" & input$shots == "Missed Shots" & input$season == "2016-17" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2016-17" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "steel blue"))}
    
    # This is the data for all missed shots by Stephen Curry in the 2015-2016 season
    else if (input$player == "Stephen Curry" & input$shots == "Missed Shots" & input$season == "2015-16" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2015-16" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "steel blue"))}
    
    # This is the data for all missed shots by Stephen Curry in the 2014-2015 season
    else if (input$player == "Stephen Curry" & input$shots == "Missed Shots" & input$season == "2014-15" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2014-15" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "steel blue"))}
    
    # This is the data for all missed shots by Stephen Curry in the 2013-2014 season
    else if (input$player == "Stephen Curry" & input$shots == "Missed Shots" & input$season == "2013-14" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2013-14" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "steel blue"))}
    
    # This is the data for all missed shots by Stephen Curry in the 2012-2013 season
    else if (input$player == "Stephen Curry" & input$shots == "Missed Shots" & input$season == "2012-13" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2012-13" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "steel blue"))}
    
    # This is the data for all missed shots by Stephen Curry in the 2011-2012 season
    else if (input$player == "Stephen Curry" & input$shots == "Missed Shots" & input$season == "2011-12" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2011-12" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "steel blue"))}
    
    # This is the data for all missed shots by Stephen Curry in the 2010-2011 season
    else if (input$player == "Stephen Curry" & input$shots == "Missed Shots" & input$season == "2010-11" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2010-11" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "steel blue"))}
    
    # This is the data for all missed shots by Stephen Curry in the 2009-2010 season
    else if (input$player == "Stephen Curry" & input$shots == "Missed Shots" & input$season == "2009-10" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2009-10" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "steel blue"))}
    
    # This is the data for all missed shots by Stephen Curry in the 2008-2009 season
    else if (input$player == "Stephen Curry" & input$shots == "Missed Shots" & input$season == "2008-09" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2008-09" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "steel blue"))}
    
    # This is the data for all missed shots by Stephen Curry in the 2007-2008 season
    else if (input$player == "Stephen Curry" & input$shots == "Missed Shots" & input$season == "2007-08" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2007-08" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "steel blue"))}
    
    # This is the data for all missed shots by Stephen Curry in the 2006-2007 season
    else if (input$player == "Stephen Curry" & input$shots == "Missed Shots" & input$season == "2006-07" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2006-07" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "steel blue"))}
    
    # This is the data for all missed shots by Stephen Curry in the 2005-2006 season
    else if (input$player == "Stephen Curry" & input$shots == "Missed Shots" & input$season == "2005-06" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2005-06" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "steel blue"))}
    
    # This is the data for all missed shots by Stephen Curry in the 2004-2005 season
    else if (input$player == "Stephen Curry" & input$shots == "Missed Shots" & input$season == "2004-05" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2004-05" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "steel blue"))}
    
    # This is the data for all missed shots by Stephen Curry in the 2003-2004 season
    else if (input$player == "Stephen Curry" & input$shots == "Missed Shots" & input$season == "2003-04" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Stephen Curry" & season == "2003-04" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "steel blue"))}
    
    # This is the data for all shots by Carmelo Anthony in the 2017-2018 season
    else if (input$player == "Carmelo Anthony" & input$shots == "All Shots" & input$season == "2017-18" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2017-18")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "goldenrod1", missed = "darkgreen"))}
    
    # This is the data for all shots by Carmelo Anthony in the 2016-2017 season
    else if (input$player == "Carmelo Anthony" & input$shots == "All Shots" & input$season == "2016-17" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2016-17")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "goldenrod1", missed = "darkgreen"))}
    
    # This is the data for all shots by Carmelo Anthony in the 2015-2016 season
    else if (input$player == "Carmelo Anthony" & input$shots == "All Shots" & input$season == "2015-16" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2015-16")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "goldenrod1", missed = "darkgreen"))}
    
    # This is the data for all shots by Carmelo Anthony in the 2014-2015 season
    else if (input$player == "Carmelo Anthony" & input$shots == "All Shots" & input$season == "2014-15" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2014-15")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "goldenrod1", missed = "darkgreen"))}
    
    # This is the data for all shots by Carmelo Anthony in the 2013-2014 season
    else if (input$player == "Carmelo Anthony" & input$shots == "All Shots" & input$season == "2013-14" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2013-14")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "goldenrod1", missed = "darkgreen"))}
    
    # This is the data for all shots by Carmelo Anthony in the 2012-2013 season
    else if (input$player == "Carmelo Anthony" & input$shots == "All Shots" & input$season == "2012-13" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2012-13")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "goldenrod1", missed = "darkgreen"))}
    
    # This is the data for all shots by Carmelo Anthony in the 2011-2012
    else if (input$player == "Carmelo Anthony" & input$shots == "All Shots" & input$season == "2011-12" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2011-12")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "goldenrod1", missed = "darkgreen"))}
    
    # This is the data for all shots by Carmelo Anthony in the 2010-2011 season
    else if (input$player == "Carmelo Anthony" & input$shots == "All Shots" & input$season == "2010-11" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2010-11")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "goldenrod1", missed = "darkgreen"))}
    
    # This is the data for all shots by Carmelo Anthony in the 2009-2010 season
    else if (input$player == "Carmelo Anthony" & input$shots == "All Shots" & input$season == "2009-10" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2009-10")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "goldenrod1", missed = "darkgreen"))}
    
    # This is the data for all shots by Carmelo Anthony in the 2008-2009 season
    else if (input$player == "Carmelo Anthony" & input$shots == "All Shots" & input$season == "2008-09" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2008-09")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "goldenrod1", missed = "darkgreen"))}
    
    # This is the data for all shots by Carmelo Anthony in the 2007-2008 season
    else if (input$player == "Carmelo Anthony" & input$shots == "All Shots" & input$season == "2007-08" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2007-08")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "goldenrod1", missed = "darkgreen"))}
      
      # This is the data for all shots by Carmelo Anthony in the 2006-2007 season
      else if (input$player == "Carmelo Anthony" & input$shots == "All Shots" & input$season == "2006-07" & input$PlotChoice == "Basketball Court"){
        source("helpers.R")
        gg_court = make_court()
        graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2006-07")
        gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                              aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "goldenrod1", missed = "darkgreen"))}
      
      # This is the data for all shots by Carmelo Anthony in the 2005-2006 season
      else if (input$player == "Carmelo Anthony" & input$shots == "All Shots" & input$season == "2005-06" & input$PlotChoice == "Basketball Court"){
        source("helpers.R")
        gg_court = make_court()
        graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2005-06")
        gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                              aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "goldenrod1", missed = "darkgreen"))}
      
      # This is the data for all shots by Carmelo Anthony in the 2004-2005 season
      else if (input$player == "Carmelo Anthony" & input$shots == "All Shots" & input$season == "2004-05" & input$PlotChoice == "Basketball Court"){
        source("helpers.R")
        gg_court = make_court()
        graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2004-05")
        gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                              aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "goldenrod1", missed = "darkgreen"))}
      
      # This is the data for all shots by Carmelo Anthony in the 2003-2004 season
      else if (input$player == "Carmelo Anthony" & input$shots == "All Shots" & input$season == "2003-04" & input$PlotChoice == "Basketball Court"){
        source("helpers.R")
        gg_court = make_court()
        graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2003-04")
        gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                              aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = c(made = "goldenrod1", missed = "darkgreen"))}
    
    # This is the data for all made shots by Carmelo Anthony in the 2017-2018 season
    else if (input$player == "Carmelo Anthony" & input$shots == "Made Shots" & input$season == "2017-18" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2017-18" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "goldenrod1"))}
    
    # This is the data for all made shots by Carmelo Anthony in the 2016-2017 season
    else if (input$player == "Carmelo Anthony" & input$shots == "Made Shots" & input$season == "2016-17" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2016-17" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "goldenrod1"))}
    
    # This is the data for all made shots by Carmelo Anthony in the 2015-2016 season
    else if (input$player == "Carmelo Anthony" & input$shots == "Made Shots" & input$season == "2015-16" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2015-16" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "goldenrod1"))}
    
    # This is the data for all made shots by Carmelo Anthony in the 2014-2015 season
    else if (input$player == "Carmelo Anthony" & input$shots == "Made Shots" & input$season == "2014-15" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2014-15" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "goldenrod1"))}
    
    # This is the data for all made shots by Carmelo Anthony in the 2013-2014 season
    else if (input$player == "Carmelo Anthony" & input$shots == "Made Shots" & input$season == "2013-14" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2013-14" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "goldenrod1"))}
    
    # This is the data for all made shots by Carmelo Anthony in the 2012-2013 season
    else if (input$player == "Carmelo Anthony" & input$shots == "Made Shots" & input$season == "2012-13" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2012-13" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "goldenrod1"))}
    
    # This is the data for all made shots by Carmelo Anthony in the 2011-2012 season
    else if (input$player == "Carmelo Anthony" & input$shots == "Made Shots" & input$season == "2011-12" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2011-12" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "goldenrod1"))}
    
    # This is the data for all made shots by Carmelo Anthony in the 2010-2011 season
    else if (input$player == "Carmelo Anthony" & input$shots == "Made Shots" & input$season == "2010-11" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2010-11" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "goldenrod1"))}
    
    # This is the data for all made shots by Carmelo Anthony in the 2009-2010 season
    else if (input$player == "Carmelo Anthony" & input$shots == "Made Shots" & input$season == "2009-10" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2009-10" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "goldenrod1"))}
    
    # This is the data for all made shots by Carmelo Anthony in the 2008-2009 season
    else if (input$player == "Carmelo Anthony" & input$shots == "Made Shots" & input$season == "2008-09" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2008-09" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "goldenrod1"))}
    
    # This is the data for all made shots by Carmelo Anthony in the 2007-2008 season
    else if (input$player == "Carmelo Anthony" & input$shots == "Made Shots" & input$season == "2007-08" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2007-08" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "goldenrod1"))}
    
    # This is the data for all made shots by Carmelo Anthony in the 2006-2007 season
    else if (input$player == "Carmelo Anthony" & input$shots == "Made Shots" & input$season == "2006-07" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2006-07" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "goldenrod1"))}
    
    # This is the data for all made shots by Carmelo Anthony in the 2005-2006 season
    else if (input$player == "Carmelo Anthony" & input$shots == "Made Shots" & input$season == "2005-06" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2005-06" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "goldenrod1"))}
    
    # This is the data for all made shots by Carmelo Anthony in the 2004-2005 season
    else if (input$player == "Carmelo Anthony" & input$shots == "Made Shots" & input$season == "2004-05" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2004-05" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "goldenrod1"))}
    
    # This is the data for all made shots by Carmelo Anthony in the 2003-2004 season
    else if (input$player == "Carmelo Anthony" & input$shots == "Made Shots" & input$season == "2003-04" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2003-04" & shot_made_flag == "made")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (made = "goldenrod1"))}
    
    # This is the data for all missed shots by Carmelo Anthony in the 2017-2018 season
    else if (input$player == "Carmelo Anthony" & input$shots == "Missed Shots" & input$season == "2017-18" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2017-18" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "darkgreen"))}
    
    # This is the data for all missed shots by Carmelo Anthony in the 2016-2017 season
    else if (input$player == "Carmelo Anthony" & input$shots == "Missed Shots" & input$season == "2016-17" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2016-17" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "darkgreen"))}
    
    # This is the data for all missed shots by Carmelo Anthony in the 2015-2016 season
    else if (input$player == "Carmelo Anthony" & input$shots == "Missed Shots" & input$season == "2015-16" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2015-16" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "darkgreen"))}
    
    # This is the data for all missed shots by Carmelo Anthony in the 2014-2015 season
    else if (input$player == "Carmelo Anthony" & input$shots == "Missed Shots" & input$season == "2014-15" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2014-15" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "darkgreen"))}
    
    # This is the data for all missed shots by Carmelo Anthony in the 2013-2014 season
    else if (input$player == "Carmelo Anthony" & input$shots == "Missed Shots" & input$season == "2013-14" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2013-14" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "darkgreen"))}
    
    # This is the data for all missed shots by Carmelo Anthony in the 2012-2013 season
    else if (input$player == "Carmelo Anthony" & input$shots == "Missed Shots" & input$season == "2012-13" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2012-13" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "darkgreen"))}
    
    # This is the data for all missed shots by Carmelo Anthony in the 2011-2012 season
    else if (input$player == "Carmelo Anthony" & input$shots == "Missed Shots" & input$season == "2011-12" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2011-12" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "darkgreen"))}
    
    # This is the data for all missed shots by Carmelo Anthony in the 2010-2011 season
    else if (input$player == "Carmelo Anthony" & input$shots == "Missed Shots" & input$season == "2010-11" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2010-11" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "darkgreen"))}
    
    # This is the data for all missed shots by Carmelo Anthony in the 2009-2010 season
    else if (input$player == "Carmelo Anthony" & input$shots == "Missed Shots" & input$season == "2009-10" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2009-10" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "darkgreen"))}
    
    # This is the data for all missed shots by Carmelo Anthony in the 2008-2009 season
    else if (input$player == "Carmelo Anthony" & input$shots == "Missed Shots" & input$season == "2008-09" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2008-09" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "darkgreen"))}
    
    # This is the data for all missed shots by Carmelo Anthony in the 2007-2008 season
    else if (input$player == "Carmelo Anthony" & input$shots == "Missed Shots" & input$season == "2007-08" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2007-08" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "darkgreen"))}
    
    # This is the data for all missed shots by Carmelo Anthony in the 2006-2007 season
    else if (input$player == "Carmelo Anthony" & input$shots == "Missed Shots" & input$season == "2006-07" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2006-07" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "darkgreen"))}
    
    # This is the data for all missed shots by Carmelo Anthony in the 2005-2006 season
    else if (input$player == "Carmelo Anthony" & input$shots == "Missed Shots" & input$season == "2005-06" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2005-06" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "darkgreen"))}
    
    # This is the data for all missed shots by Carmelo Anthony in the 2004-2005 season
    else if (input$player == "Carmelo Anthony" & input$shots == "Missed Shots" & input$season == "2004-05" & input$PlotChoice == "Basketball Court"){
      source("helpers.R")
      gg_court = make_court()
      graph_data = filter(nba_shots, player_name == "Carmelo Anthony" & season == "2004-05" & shot_made_flag == "missed")
      gg_court + geom_point(data = graph_data, alpha = 0.60, size = 2.0,
                            aes(loc_x, loc_y, color = shot_made_flag)) + scale_color_manual("", values = (missed = "darkgreen"))}
    
   # This is the beginning of the Boxplot data
    # This is the data for shot distance by all shots in the 2017-2018 season -  boxplot
    else if (input$season == "2017-18" & input$PlotChoice == "Boxplot"){
      graph_data = filter(nba_shots, season == "2017-18")
      p <- ggplot(graph_data, aes(x = player_name, y = shot_distance, fill = player_name)) + 
        geom_boxplot()
      p
    }
    
    # This is the data for shot distance in the 2016-2017 season - boxplot
    else if (input$season == "2016-17" & input$PlotChoice == "Boxplot"){
      graph_data = filter(nba_shots, season == "2016-17")
      p <- ggplot(graph_data, aes(x = player_name, y = shot_distance, fill = player_name)) + 
        geom_boxplot()
      p
    }
    
    # This is the data for shot distance in the 2015-2016 season - boxplot
    else if (input$shots == "All Shots" & input$season == "2015-16" & input$PlotChoice == "Boxplot"){
      graph_data = filter(nba_shots, season == "2015-16")
      p <- ggplot(graph_data, aes(x = player_name, y = shot_distance, fill = player_name)) + 
        geom_boxplot()
      p
    }
    
    # This is the data for shot distance in the 2014-2015 season - boxplot
    else if (input$season == "2014-15" & input$PlotChoice == "Boxplot"){
      graph_data = filter(nba_shots, season == "2014-15")
      p <- ggplot(graph_data, aes(x = player_name, y = shot_distance, fill = player_name)) + 
        geom_boxplot()
      p
    }
    
    # This is the data for shot distance in the 2013-2014 season - boxplot
    else if (input$season == "2013-14" & input$PlotChoice == "Boxplot"){
      graph_data = filter(nba_shots, season == "2013-14")
      p <- ggplot(graph_data, aes(x = player_name, y = shot_distance, fill = player_name)) + 
        geom_boxplot()
      p
    }
    
    # This is the data for shot distance in the 2012-2013 season - boxplot
    else if (input$season == "2012-13" & input$PlotChoice == "Boxplot"){
      graph_data = filter(nba_shots, season == "2012-13")
      p <- ggplot(graph_data, aes(x = player_name, y = shot_distance, fill = player_name)) + 
        geom_boxplot()
      p
    }
    
    # This is the data for shot distance in the 2011-2012 season -  boxplot
    else if (input$season == "2011-12" & input$PlotChoice == "Boxplot"){
      graph_data = filter(nba_shots, season == "2011-12")
      p <- ggplot(graph_data, aes(x = player_name, y = shot_distance, fill = player_name)) + 
        geom_boxplot()
      p
    }
    
    # This is the data for shot distance in the 2010-2011 season - Boxplot
    else if (input$season == "2010-11" & input$PlotChoice == "Boxplot"){
      graph_data = filter(nba_shots, season == "2010-11")
      p <- ggplot(graph_data, aes(x = player_name, y = shot_distance, fill = player_name)) + 
        geom_boxplot()
      p
    }
    
    # This is the data for shot distance in the 2009-2010 season - boxplot
    else if (input$season == "2009-10" & input$PlotChoice == "Boxplot"){
      graph_data = filter(nba_shots, season == "2009-10")
      p <- ggplot(graph_data, aes(x = player_name, y = shot_distance, fill = player_name)) + 
        geom_boxplot()
      p
    }
    
   # This is the data for shot distance in the 2008-2009 season - boxplot
    else if (input$season == "2008-09" & input$PlotChoice == "Boxplot"){
      graph_data = filter(nba_shots, season == "2008-09")
      p <- ggplot(graph_data, aes(x = player_name, y = shot_distance, fill = player_name)) + 
        geom_boxplot()
      p
    }
    
    # This is the data for shot distance in the 2007-2008 season - boxplot
    else if (input$season == "2007-08" & input$PlotChoice == "Boxplot"){
      graph_data = filter(nba_shots, season == "2007-08")
      p <- ggplot(graph_data, aes(x = player_name, y = shot_distance, fill = player_name)) + 
        geom_boxplot()
      p
    }
    
    # This is the data for shot distance in the 2006-2007 season
    else if (input$season == "2006-07" & input$PlotChoice == "Boxplot"){
      graph_data = filter(nba_shots, season == "2006-07")
      p <- ggplot(graph_data, aes(x = player_name, y = shot_distance, fill = player_name)) + 
        geom_boxplot()
      p
    }
    
    # This is the data for shot distance in the 2005-2006 season
    else if (input$season == "2005-06" & input$PlotChoice == "Boxplot"){
      graph_data = filter(nba_shots, season == "2005-06")
      p <- ggplot(graph_data, aes(x = player_name, y = shot_distance, fill = player_name)) + 
        geom_boxplot()
      p
    }
    
    # This is the data for shot distance in the 2004-2005 season
    else if (input$season == "2004-05" & input$PlotChoice == "Boxplot"){
      graph_data = filter(nba_shots, season == "2004-05")
      p <- ggplot(graph_data, aes(x = player_name, y = shot_distance, fill = player_name)) + 
        geom_boxplot()
      p
    }
    
    # This is the data for shot distance in the 2003-2004 season
    else if (input$season == "2003-04" & input$PlotChoice == "Boxplot"){
      graph_data = filter(nba_shots, season == "2003-04")
      p <- ggplot(graph_data, aes(x = player_name, y = shot_distance, fill = player_name)) + 
        geom_boxplot()
      p
    }
    
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
  
# Create the summary table for the data exploration tab
  output$SummaryTable <- renderDT({
    if(input$SummaryType == "Location"){
    specificPlayers <- unlist(input$specificPlayers)
    Shots <- unlist(input$Shots)
    NumericVariables <- unlist(input$NumericVariables)
    
    # Grab the rows from specific players and all shots
    FilteredData <- nba_shots %>%
      filter(
        player_name %in% specificPlayers,
        shot_made_flag %in% Shots
      ) %>%
      select(NumericVariables)
    
    numericSummary <- do.call(cbind, lapply(FilteredData, summary))
    as.data.frame(t(numericSummary))}
    
    else{
      # Get selected players 
      specificPlayers <- unlist(input$specificPlayers)
      
      
      # Show total made and missed shots by player
      nba_shots %>%
        filter(
          player_name %in% specificPlayers
        ) %>%
        select(player_name, shot_made_flag) %>% group_by(player_name) %>%
        summarize(
          "Total Shots" = length(shot_made_flag)
        ) 
      
    }
    
  })
  
  # Modeling and prediction section begins here
  # Remove NA values for modeling
  na.omit(nba_shots, na.action="omit")
  nba_shots[, c(4,5,6,12,13,18)] <- sapply(nba_shots[, c(4,5,6,12,13,18)], as.numeric)
  
  # Fit the models on the training set
  observeEvent(input$ModelFit,{
    
    # Create progress object
    progress <- Progress$new()
    
    # Close this when we exit reactive
    on.exit(progress$close())
    
    # Display this message to the user while the models are running
    progress$set(message = "Models Are Running", value = 0)
    
    # Store the variables that are needed
    LogVars <- input$LogVars
    ClassVars <- input$ClassVars
    ForestVars <- input$ForestVars
    
    # Store model parameters
    SetSeed <- input$SetSeed
    DataPro <- input$DataPro
    cvFolds <- input$cvFolds
    
    # Store mtry
    Selectmtry <- input$Selectmtry
    
    # Set seeed
    set.seed(SetSeed)
    
    # Store/Split the data into training and test set
    TrainSetInd <- sample(seq_len(nrow(nba_shots)),
                          size = floor(nrow(nba_shots)*DataPro))
    NBATrain <- nba_shots[TrainSetInd,]
    NBATest <- nba_shots[-TrainSetInd,]
    
    # Suppress the warnings I was getting for cross validation
    suppressWarnings(library(caret))
    
    # Store CV
    TrCtrl <- trainControl(
      method = "cv",
      number = cvFolds
    )
    
    #Logistic Regression Section
    
    # Update the progress bar
    progress$inc(0.3, detail = "Logistic Regression Model")
    
    LogisticModel <- train(shot_made_flag~.,
                           data = NBATrain[, c(c("shot_made_flag"), LogVars)],
                           method = "glm",
                           family = "binomial",
                           metric = "Accuracy",
                           trControl = TrCtrl)
    
    # Update the progress bar
    progress$inc(0.5, detail = "Tree Model")
    
    # Classification Tree Section
    TreeModel <- train(shot_made_flag~.,
                       data = NBATrain[, c(c("shot_made_flag"), ClassVars)],
                       method = "ctree2",
                       metric = "Accuracy",
                       trControl = TrCtrl,
                       tuneGrid = expand.grid(maxdepth = 3, mincriterion = 0.95))
    
    # Update the progress bar
    progress$inc(0.7, detail = "Random Forest Model")
    
    # Random Forest Section
    rfFit <- train(shot_made_flag~.,
                   data = NBATrain[,c(c("shot_made_flag"), ForestVars)],
                   method = "ranger",
                   metric = "Accuracy")
    
    # Update the progress bar
    progress$inc(1, detail = "Model Performance on Training Set is Complete")
    
    # Accuracy for the Logistic Regression
    output$accuracy <- renderDataTable({
      FitData <- (t(as.matrix(LogisticModel$results)))
      FitData
      
      colnames(FitData) <- c("Logistic Regression")
      FitData
    })
    
    # Accuracy for the Classification Tree
    output$TreeAccuracy <- renderDataTable({
      TreeAcc <- print(TreeModel)
      TreeSummary <- as.data.frame(TreeAcc)
      TreeSummary
    })
    
    # Tree plot output
    output$TreePlot <- renderPlot({
      FullTree <- plot(TreeModel$finalModel)
      FullTree
    })
    
    # Accuracy for RF
    output$RFAccuracy <- renderDataTable({
      rfAccuracy <- rfFit
      print(rfAccuracy)
    })
    
    # Variable of Importance for RF
    output$RanForVarImp <- renderPlot({
      ForestPlot <- ggplot(varImp(object = rfFit)) +
        ggtitle("Random Forest Variable of Importance")
      ForestPlot
    })
    
    # These are the test fit statistics placed into a table
    output$TestFit <- renderDataTable({
      comparisons <- round(t(rbind(LogTest$overall[1], TreeTest$overall[1], predRF[1])), 4)
      colnames(comparisons) <- c("Logistic Regression", "Classification Tree", "Random Forest")
      comparisons
    })
  })
  
  
  
  
})


 