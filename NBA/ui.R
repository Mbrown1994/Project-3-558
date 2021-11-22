
# Read in the Data 
load("~/Desktop/R Programming/git-project/Final Project/Project-3-558/nba_shots.RData")

# Convert made shots to a factor

# Define UI for application that draws a histogram
shinyUI(navbarPage(

    # Application title
    title = "NBA Shots Data",
    
    # Add a theme
    theme = shinytheme("cerulean"),  
    
    # Create the tabs for the different sections of this app
    tabPanel(
        # Add the title
        title = "About",
        
        # Create a main panel for this specific tab
        mainPanel(
            # Put the logo image in
            img(
                src = "NBA logo.png",
                height = 120,
                width = 300
            )
        )
    ),
    
    sidebarLayout(
        sidebarPanel(
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            
        )
    )
))
