#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)


#################
# UI
#################

# Define UI for application
ui <- fluidPage(
  # Application title
  titlePanel("InfusionPrep"),
  
  # Sidebar layout with input and output
  sidebarLayout(
    sidebarPanel(

      # Numeric input for "Tracer"
      textInput("tracer.name", 
                 "Tracer Compound", 
                 value = ""),
      
      # Expected Fcirc (Ra, nmol/min)
      numericInput("fcirc.per.mouse", 
                   "Expected Fcirc (Ra, nmol/min)", 
                   value = 10),
      
      # Numeric input for "Target Labeling"
      sliderInput("target.labeling", 
                   "Target Labeling (Fraction):", 
                  min = 0,
                  max = 1,
                  value = 0.2),
      
      # Length of infusion
      numericInput("hours",
                   "Hours", 
                   value = 2.5),
      
      # Volume rate of infusions (per mouse)
      numericInput("rate.ul.per.min",
                   "Rate (ul/min/mouse)",
                   value = 2.5),
      
      # Numeric input for "Mice"
      numericInput("mice", 
                   "Number of Mice:", 
                   value = 1),
    ),
    
    # Show the product of inputs
    mainPanel(
      textOutput("rate"),
      textOutput("total.tracer"),
      textOutput("recommended.tracer"),
      textOutput("concentration")
    )
  )
)


#################
# server
#################



# Define server logic
server <- function(input, output) {
  
  # calculate values
  maths <- reactive({
    #calculate nmol per min required given target labeling and Ra
    rate.nmol.per.min <- (input$target.labeling * input$fcirc.per.mouse) / (1 - input$target.labeling)
    
    # calculate total volume (mL)
    total.mL <- input$rate.ul.per.min / 1000 * input$hours * 60 * input$mice
    
    # calculate recommended volume (mL)
    recommended.mL <- if(total.mL < 4) {total.mL + 1} else {total.mL + 2}
    
    # calculate tracer concentration
    tracer.mM <- rate.nmol.per.min / input$rate.ul.per.min
    
    # output is a named list
    list.out <-
      list(rate.nmol.per.min = rate.nmol.per.min,
           total.mL = total.mL,
           recommended.mL = recommended.mL,
           tracer.mM = tracer.mM)
    
    # round all values to 3 digits
    lapply(list.out, FUN=round, digits=3)
  })
  
  
  # infusion rate values
  output$rate <- renderText({
    paste0("To target ", input$target.labeling, " labeling, infuse ", maths()$rate.nmol.per.min, " nmol per minute.")
  })
  
  
  # total volume of tracer required
  output$total.tracer <- renderText({
    paste0("Total volume required: ", maths()$total.mL, " mL")
  })

  # recommended volume to prepare
  output$recommended.tracer <- renderText({
    paste0("Recommended volume to prepare: ", maths()$recommended.mL, " mL")
  })
  
  # concentration of tracer
  output$concentration <- renderText({
    paste0("Tracer concentration in infusate: ", maths()$tracer.mM, " mM")
  })
  
  
}

# Run the application
shinyApp(ui = ui, server = server)