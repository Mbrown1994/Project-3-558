
# Packages used for this analysis
library(shinythemes)
library(tidyverse)
library(magrittr)
library(shiny)
library(caret)
library(tree)
library(dplyr)
library(DT)

# Ignore warnings
suppressWarnings(library(caret))

# Read in the data
fileName <- "./NBAdata.csv"
nba_shots <- read_csv(
    fileName,
    col_types = cols()
)



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
                h3("Visual Summary"),
                h4("Select a Plot View"),
                
                radioButtons(
                 inputId = "PlotChoice",
                 label = "Plot",
                 choices = list("Basketball Court", "Boxplot")
                ),
                
                h4("Player Selection:"),
                selectizeInput(
                inputId ="player", 
                label = "Player", 
                selected = "LeBron James", 
                choices = levels(as.factor(nba_shots$player_name))),
                
                uiOutput("image"),
                selectizeInput(
                    inputId = "season", 
                    label = "Season", 
                    selected = "2017-18", 
                    choices = levels(as.factor(nba_shots$season))),
                
               conditionalPanel(
                   condition = 'input.PlotChoice == "Basketball Court"',
                 radioButtons(
                    inputId = "shots", 
                    label = h4("Shot Status"), 
                    choices = list("All Shots", "Missed Shots", "Made Shots"))),
                
                # Summary portion of the sidebar
                h3("Numeric Summary"),
                radioButtons(
                    inputId = "SummaryType", 
                    label = h4("Type of Summary"),
                    choices = list("Total Shots", "Location"),
                    inline = TRUE
                ),
                
                # Show this when Location is selected
                conditionalPanel(
                    condition = 'input.SummaryType == "Location"',
                    selectInput(
                        inputId = "NumericVariables",
                        label = "Variables to Summarize:",
                        choices = colnames(nba_shots)[4:6],
                        selected = c("loc_x","loc_y","shot_distance"),
                        multiple = TRUE,
                        selectize = TRUE
                    )
                    
                ),
                
            ),
            mainPanel(
                # Put the plots in the main panel
                h3("Visual Summary"),
                plotOutput("BasketballPlot"),
                br(),
                br(),
                # Put the summary in the main panel
                h3("Numeric Summary"),
                    dataTableOutput("SummaryTable")
                ),
                
            
        
    )),
    

    
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
                "categorized as supervised learning, allows us",
                "to learn about patterns and relationships within",
                "our data. The models used take in our existing data",
                "to predict values for new occurences.",
                br(),
                br(),
                "The supervised methods used here are Logistic",
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
        "series of independent variables. The Logistic Regression equation",
        "is:",
        uiOutput("logEq"),
        br(),
        br(),
        tags$ul(
        "Advantages of Logistic Regression:",
        tags$li("Efficient to train and easy to implement"),
        tags$li("Less prone to over-fitting"),
        tags$li("Good performance when the dataset is linearly seperable"),
        br(),
        "Disadvantages of Logistic Regression:",
        tags$li("Can only be used to predict discrete functions"),
        tags$li("Overfitting can occur with high-dimensional datasets"),
        tags$li("Can't solve non-linear issues")
        ),
        br(),
        # Overview of Classification Trees
        h4("Model 2: Classification Trees"),
        "Tree based methods like Classification Trees splits up",
        "the predictor space into regions with different predictions",
        "for each region. The Classification Tree utilizes the most",
        "prevalent class as prediction in each given region.",
        br(),
        br(),
        tags$ul(
            "Advantages of Classification Trees:",
            tags$li("Trees are easy to explain/understand"),
            tags$li("Clear visualization after plotting"),
            tags$li("Works with continuous and categorical variables"),
            br(),
            "Disadvantages of Classification Trees:",
            tags$li("Overfitting can occur"),
            tags$li("High variance in the output due to overfitting"),
            tags$li("Not as suitable for large data sets")
        ),
         
        br(),
        # Overview of Random Forests
        h4("Random Forests"),
        "This method creates multiple trees from bootstrap samples and",
        "is generally better than bagging. Rather than utilizing all predictors,",
        "a random subset of predictors are used for each tree.",
        br(),
        br(),
        tags$ul(
            "Advantages of Random Forest modeling:",
            tags$li("Can be used for classification and regression problems"),
            tags$li("Improves accuracy"),
            tags$li("Handles large datasets efficiently"),
            br(),
            "Disadvantages of Random Forest modeling:",
            tags$li("Less interpretable compared to other methods"),
            tags$li("Computationally expensive"),
            tags$li("Any changes to the data can change the trees")
        ),
        br(),
        br(),
                
            ))
        ),
        

        tabPanel(
            title = "Fitting the Models",
            sidebarPanel(
            
            h4("Model Settings"),
            # Allow the user to choose model settings
                numericInput(
                    inputId = "SetSeed",
                    label = "Set Random Seed",
                    value = 50,
                    min = 1,
                    max = 100,
                    step = 1
                ),
            # This allows the user to choose which portion of the data to use from the training set
                numericInput(
                    inputId = "DataPro",
                    label = "Proportion of the data to include in the training set",
                    value = 0.70,
                    min = 0.10,
                    max = 0.90,
                    step = 0.10
                ),
            
            # This allows the user to select number of folds
                numericInput(
                    inputId = "cvFolds",
                    label = "Select Number of Folds",
                    value = 5,
                    min = 2,
                    max = 8,
                    step = 1
                    
                ),
            # Allow the user to choose Log parameters
            selectInput(
                inputId = "LogVars",
                label = "Predictors to use for logistic regression:",
                choices = colnames(nba_shots)[4:6&11:13],
                selected = c("loc_x", "loc_y", "shot_distance", "period", "minutes_remaining", "seconds_remaining"),
                multiple = TRUE,
                selectize = TRUE
            ),
            # Allow the user to choose Tree parameters
            selectInput(
                inputId = "ClassVars",
                label = "Predictors to use for Classification Tree:",
                choices = colnames(nba_shots)[4:6&11:13],
                selected = c("loc_x", "loc_y", "shot_distance", "period", "minutes_remaining", "seconds_remaining"),
                multiple = TRUE,
                selectize = TRUE
            ),
            # Allow the user to choose RF parameters
            selectInput(
                inputId = "ForestVars",
                label = "Predictors to use for Random Forest:",
                choices = colnames(nba_shots)[4:6&11:13],
                selected = c("loc_x", "loc_y", "shot_distance", "period", "minutes_remaining", "seconds_remaining"),
                multiple = TRUE,
                selectize = TRUE
            ),
            
                # This is the button for fitting the models
                actionButton(
                    inputId = "ModelFit",
                    label = "Fit Models",
                )
                
            ),
            
            # Main panel section to view outputs
            mainPanel(
                h4("Logistic Regression Model Fit Statistics on Training Data"),
                dataTableOutput("Accuracy"),
                br(),
                h4("Classification Tree Fit Statistics on Training Data"),
                dataTableOutput("TreeAccuracy"),
                br(),
                h4("Random Forest Fit Statistics"),
                dataTableOutput("RFAccuracy"),
                # Tree Plot
                h4("Tree Plot"),
                plotOutput("TreePlot"),
                br(),
                plotOutput("RanForVarImp"),
                br(),
                h4("Model Fit Statistics: Test Data"),
                dataTableOutput("TestFit")
            )
            
        ),
        
        # Prediction Tab
        tabPanel(
            title = "Prediction",
             # Create the sidebar for this specific tab
            sidebarPanel(
                
                # This button will fit the models
                actionButton(
                    inputId = "PredictionBegin",
                    label = "Predict"
                ),
                
                
            ),
            
            # This panel will show the predictions
            mainPanel(
               
                
            )
        )
        
        
        
    )
    
    )))
