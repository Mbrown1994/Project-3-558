
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
        
        # Create the tab for the 'About' section of the app
    tabPanel(
        # Add the title
        title = "About",
        
        # Create a main panel for this specific tab
        mainPanel(
           
            # Add in the image
            img(
                src = "NBAlogo.png",
                height = '450 px',
                width = '500 px'
                
            ),
            # Describe what this app will be used for
            h3("The Main Purpose of This App"),
            "This app explores shots taken by 5 of the NBAs top players. The", tags$br(),
            "NBA is a national basketball league made of 30 teams in North America.", tags$br(),
            "The players explored in this app include LeBron James, Kevrin Durant,", tags$br(),
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
                    "Data: Explains the data that we are working with"
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
    
    # Create the tab for the 'Data' section of this app
    tabPanel(
        # Add the title
        title = "Data",
        
        # Create a side panel
        sidebarPanel(
            # Filter the data by player
            selectInput(
                inputId = "specificPlayers",
                label = "Filter by NBA Players",
                choices = unique(nba_shots$player_name),
                selected = unique(nba_shots$player_name),
                multiple = TRUE,
                selectize = TRUE
            ),
            
            # Filter the data by shots 
            selectInput(
                inputId = "Shots",
                label = "Filter by shots",
                choices = c("made", "missed"),
                selected = c("made", "missed"),
                multiple = TRUE,
                selectize = TRUE
            ),
            
            # Filter for the display of columns
            selectInput(
                inputId  = "selectedCols",
                label = "Filter Columns",
                choices = colnames(nba_shots),
                selected = colnames(nba_shots),
                multiple = TRUE,
                selectize = TRUE
            ),
            
            
        ),
        
        # Output the table
        mainPanel(
            dataTableOutput(outputId = "table")
        )
        
    ),
    
    tabPanel(
        title = "Data Exploration",
        sidebarLayout(
            sidebarPanel(
                h4("Player Selection:"),
                selectizeInput("player", "Player", selected = "LeBron James", choices = levels(as.factor(nba_shots$player_name))),
                uiOutput("image"),
                selectizeInput("season", "Season", selected = "2017-18", choices = levels(as.factor(nba_shots$season))),
                radioButtons(inputId = "shots", label = h4("Shot Status"), choices = list("All Shots", "Missed Shots", "Made Shots"))
                
            ),
            mainPanel(
                plotOutput("BasketballPlot")
                
            )
        )
    ),
    
    tabPanel(
        title = "Modeling"
    )
    
    )))
