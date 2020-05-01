# side bar panel for the model
modelsSidebarPanel <- function(){
  sidebarPanel(
    fluidRow(
      column(
         width=12,
         sliderInput("recoveryDays", "Duration of Recovery", 1, 30, 14, step=1, post = " days"),
         br(),
         sliderInput("infectionRate", div(HTML("Infection Rate")), 0.001, 1, 0.3, step=0.01, post=""),
         hr()
      )
    ),
    h4(div(HTML("Simulation Values"))),
    fluidRow(
      column(
           width=12,
           numericInput("N", div(HTML("Population size:")), value=6900000, max=10^10, min=1000, step=1000),
           br(),
           numericInput("initInf","Initial #infected:",value = 1, min = 1, step = 1),
           hr()
      )
    ),
    fluidRow(
      column(
          width=12,
          sliderInput("Tmax", div(HTML("Maximum time")),0, 365, 150, step=1, post=" days"),
          actionButton("reset", "Reset all")  
          
      )
    ),
    width = 4
  )
}

# main panel
modelsMainPanel <- function(){
  mainPanel(
    
    #p(div(HTML("Test")))
    navbarPage("Total Cases:",
               
               tabPanel("Infected + Recovered",
                        fluidRow(
                          column(
                            width=12,
                            h4(""),
                            
                            plotlyOutput("spreadPlot"),
                                 radioButtons("yscale", "Y axis scale:",
                                  choices = list("Linear" = "linear","Proportional" = "proportional"),inline=TRUE)
                          ),
                          br()

                        )
               ),

               tabPanel(
                 "Simulations", br(),
                        fluidRow(
                          column(
                                  width = 12,
                                  withMathJax(),
                                  h4("SIR Model Output"),
                                  plotlyOutput("plotSIR", height = 400),
                                  p(
                                    HTML(
                                          "<b>User instructions:</br>
                                         <b> 1. Choose value of Recovery Days for SIR Model </br>
                                         <b> 2. Choose value of Infection Rate for SIR Model </br>
                                         <b> 3. Choose value of N (Maximum population that is likely to be infected) </br>
                                         <b> 4. Choose value number of initially infected </br>
                                         <b> 5. Choose Maximum Time for Simulation </br>")
                                      )
                                  
                              )
                        )
                 )
               
    ),
    width=8
  )
}
