#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)

source('settings.R')
source('loadData.R')




# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("united"),
  
  # Application title
  titlePanel("Reproducibility Project: Psychology correlations"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("study", label = "Original study:",
                  choices = my.labs, multiple = FALSE),
      tags$hr(),
      selectInput("alpha", label = "Confidence Coefficient:",
                  choices = 
                    c("Std. err. (α=.32)"=pnorm(-1)*2,
                      "90% CI (α=.1)"=.1,
                      "95% CI (α=.05)"=.05,
                      "99% CI (α=.01)"=.01)
                    , multiple = FALSE,selected = .05),
      sliderInput("diff", label = "Abs. val. of true diff. (z)", min = 0, max = 2.5,step = .01, value=.3),
      checkboxInput("settings", "Show settings?", value = FALSE),
      conditionalPanel(
        condition = "input.settings == 1",
        checkboxInput("prop_to_n1", "Pointsize prop. to orig. n?", value = TRUE),
        checkboxInput("show_null", "Show null?", value = TRUE),
        sliderInput("refpow", label = "Reference power:", min = .5, max = .99, step = .01, value=.8),
        sliderInput("xlim", label="x limits (z diff.)", min = -5, max=5, step=.25, value = c(-1,3))
      )
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Summary", plotOutput("sumPlot", click = "myClick", height = 500)),
        tabPanel("Results table", tableOutput("table")),
        tabPanel("Power", plotOutput("powerPlot", height = 500)),
        tabPanel("p values", plotOutput("pPlot", height = 500))
      )
    )
  )
))
