#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(colourpicker)

# Define UI for application that draws a histogram
navbarPage(

    # Application title
    "Coded Mittens",
tabPanel("Step 1: Generate your mitten pattern",
         mainPanel(
         sliderInput("rule",
                     "Cellular Automata Rule:",
                     min = 1,
                     max = 255,
                     value = 195),
         plotOutput("mittens")
         ) # Close main panel
         ), # Close first tab
tabPanel("Step 2: Choose Your Colors",
         
         # Sidebar with a slider input for number of bins
         sidebarLayout(
           sidebarPanel(
             colourInput("col1", "Select main color", value = "#40567b"),
             colourInput("col2", "Select secondary color", value = "#97a7c0"),
             colourInput("col3", "Select tertiary color", value = "#b48639")
           ), # Close sidebar 
           
           # Show a plot of the generated distribution
           mainPanel(
             DT::dataTableOutput("yarn_table")   
           ) # Close main panel
         ) # Close sidebar panel
) # Close second tab
) # Close navbar page
