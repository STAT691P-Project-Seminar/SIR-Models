# side bar panel for the model
modelsSidebarPanel <- function(){
  sidebarPanel(
    fluidRow(
      column(
         width=12,
         sliderInput("recoveryDays", "Duration of Recovery", 0, 30, 14, step=1, post = " days"),
         br(),
         sliderInput("infectionRate", div(HTML("Infection Rate")), 0, 10, 0.3, step=0.01, post=""),
         hr()
      )
    ),
    h4(div(HTML("Simulation Values"))),
    fluidRow(
      column(
           width=12,
           numericInput("N", div(HTML("Population size:")), value=7000000, max=10^10, min=1000, step=1000),
           br(),
           numericInput("initInf","Initial # infected:",value = 1, min = 1, step = 1),
           hr()
      )
    ),
    fluidRow(
      column(
          width=12,
          sliderInput("Tmax", div(HTML("Maximum time")),0, 365, 150, step=10, post=" days"),
          actionButton("reset", "Reset all")  
          
      )
    )
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