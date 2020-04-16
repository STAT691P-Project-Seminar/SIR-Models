# side bar panel for the model
modelsSidebarPanel <- function(){
  sidebarPanel(
    
    fluidRow(
      column(width=6,
             sliderInput("recoveryDays", "Duration of Recovery", 0, 30, 14, step=1, post = " days"),
             br(),
             hr()
             
      ),
      column(width=6,
             sliderInput("infectionRate", div(HTML("Infection Rate")), 0, 10, 0.3, step=0.01, post=""),
             hr()
             
      )
    ),
    h4(div(HTML("<em>Set Simulation Values</em>"))),
    #sliderInput("LogN", div(HTML("Total population size (log10)")), 1, 9, 3, step=0.1),
    #htmlOutput("N"),
    column(width=5,
           numericInput("N", div(HTML("Population size:")), value=7000000, max=10^10, min=1000, step=1000)
    ),
    column(width=5,
           numericInput("initInf","Initial # infected:",value = 1, min = 1, step = 1)
    ),
    #br(),
    sliderInput("Tmax", div(HTML("Maximum time")),0, 1000, 100, step=10, post=" days"),
    actionButton("reset", "Reset all"),    
    width=5
  )
}

# main panel
modelsMainPanel <- function(){
  mainPanel(
    
    #p(div(HTML("Test")))
    navbarPage("Output:",
               
               tabPanel("Spread",
                        fluidPage(
                          fluidRow(
                            
                            h4("Simulated Spread of Covid-19 in Massachusetts"),
                            
                            plotlyOutput("spreadPlot"),
                            br(),
                            br(),
                            column(width=12,
                                   radioButtons("yscale", "Y axis scale:",
                                                choices = list("Linear" = "linear","Proportional" = "proportional"),inline=TRUE)
                            ),
                            br(),
                            
                          )
                        )
               ),
               
               tabPanel("Model", br(),
                        fluidRow(column(12,
                                        withMathJax(),
                                        h4("Model Output"),
                                        plotOutput("plotSIR", height=200),
                                        p(HTML("<b>User instructions:</b>"))
                        )))
               
    ),
    width=7
  )
}