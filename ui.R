
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
            
            # Allow the data to be downloaded by the user
            downloadButton("Download", "Download the data here")
            
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
    
    # Create 3 sub-tabs for the modeling tab
    navbarMenu(
        
        # Create the title
        title = "Modeling",
        
        # Modeling info tab
        tabPanel(
            title = "Modeling Information",
            mainPanel(fluidPage(
                br(),
                h4("Overview of the Modeling"),
                "The predictive modeling used within this app,",
                "categorized as unsupervised learning, allows us",
                "to learn about patterns and relationships within",
                "our data. The models used take in our existing data",
                "to predict values for new occurences",
                br(),
                br(),
                "The unsupervised methods used here are Logistic ",
                "Regression, Classification Trees, and Random Forests methods.",
                "You can learn more about each individual model used within this",
                "app in the sections below.",
                br(),
                br(),
                
        # Overview of logistic regression.
                h4("Model 1: Logistic Regression"),
        "Logistic Regression is one of the predictive modeling methods",
        "used in this app. In general, logistic regression is a type of",
        "regression analysis used to find the relationship between a",
        "dependent variable and either one independent variable or a",
        "series of independent variables.",
        br(),
        br(),
        
        # Overview of Classification Trees
        h4("Model 2: Classification Trees"),
        "Explain here.",
        br(),
        br(),
        "Explain more here",
        br(),
        br(),
        
        # Overview of Random Forests
        h4("Random Forests"),
        "Explain here.",
        br(),
        br(),
        "Explain more here",
        br(),
        br(),
                
            ))
        ),
        
        # Tab for fitting the models explained
        tabPanel(
            title = "Fitting the Models",
            sidebarPanel(
                h3( "Split the data: Train and Test"),
                numericInput(
                    inputId = "RandomSeed",
                    label = "Set a Random Seed",
                    value = 1,
                    min = -1000,
                    max = 1000,
                    step = 1
                ),
                
                # Select the data to be used for the test set
                numericInput(
                    inputId = "TestSet",
                    label = "Data used for Test Set",
                    value = 0.2,
                    min = 0.1,
                    max = 0.5,
                    step = 0.05
                ),
                
                # Cross-Validation
                h3("Cross-Validation"),
                div(
                    numericInput(
                        inputId = "Folds",
                        label = "Number of Folds",
                        value = 3,
                        min = 3,
                        max = 5,
                        step = 1
                    ),
                    style = "display:inline-block"
                ),
                
                # Logistic Regression
                h3("Logistic Regression"),
                selectInput(
                    inputId = "LogisticRegression",
                    label = "Variables:",
                    choices = colnames(nba_shots)[3:18],
                    selected = c("loc_x","loc_y","shot_distance","minutes_remaining","seconds_remaining","time_remaining"),
                    multiple = TRUE,
                    selectize = TRUE
                )
                
            )
            
        ),
        
        # Prediction Tab
        tabPanel(
            title = "Prediction"
        )
        
        
        
    )
    
    )))
