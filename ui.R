
# Read in the Data 
load("~/Desktop/R Programming/git-project/Final Project/Project-3-558/nba_shots.RData")

# Convert made shots to a factor

# Define UI for application that draws a histogram
shinyUI(navbarPage(

    # Application title
    title = "NBA Shots Data",
        
    # Add a theme
    theme = shinytheme("cerulean"),  
    
    # Add in the different tabs
    tabsetPanel(
    
    # Create the tabs for the different sections of this app
    tabPanel(
        # Add the title
        title = "About",
        
        # Create a main panel for this specific tab
        mainPanel(
           
            # Add in the image
            img(
                src = "NBAlogo.png",
                height = '350 px',
                width = '400 px'
                
            ),
            # Describe what this app will be used for
            h3("The Main Purpose of This App"),
            "This app explores shots taken by 5 of the NBAs top players.", tags$br(),
            "The players explored include LeBron James, Kevrin Durant,", tags$br(),
            "Russell Westbrook, Stephen Curry, and Carmelo Anthony.", tags$br(),
            ),
        sidebarPanel(
            
            # Section for discussing the data
            h3("The Data"),
            "The data has 19 variables describing 81,383 shots",
            "by season, player, location, distance from", 
            "the basket, and more. All of the statistics",
            "to make this data can be found",
            a(href="https://www.nba.com/stats/", "here."),
            
            # Section for explaining the Tabs
            h3("The Tabs"),
            tags$ul(
                tags$li(
                    "Data: Exaplins the data that we are working with"
                ),
                tags$li(
                    "Data Exploration: Summarizes the data with visual explanations"
                ),
                tags$li(
                    "Modeling: Provides information on 3 different models,",
                    "depending on user input. The user can predict made shots,",
                    "based on the selected input."
                )
                ),
                
            ),
          
        ),
    tabPanel(
        title = "Data Exploration"
    ),
    
    tabPanel(
        title = "Modeling"
    )
    
    )))
