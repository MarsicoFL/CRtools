library(shiny)
library(shinydashboard)

# Define the Shiny application interface
ui <- dashboardPage(
  dashboardHeader(
    title = "Clinical Research Tools: Significance Test for differences between Odds Ratios",
    titleWidth = 400
  ),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem(
        "Test Configuration",
        tabName = "config",
        icon = icon("wrench")
      )
    ),
    tags$div(
      class = "sidebar-footer",
      p("Created by", em("Franco Marsico"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "config",
        fluidPage(
          titlePanel("Configuration"),
          box(
            title = "Input Parameters",
            solidHeader = TRUE,
            width = 6,
            status = "primary",
            background = "aqua",
            numericInput("OR1", "Odds Ratio Group 1:", value = 0.8),
            numericInput("OR2", "Odds Ratio Group 2:", value = 0.7),
            numericInput("events1", "Positive events Group 1:", value = 250),
            numericInput("nonevents1", "Negative events Group 1:", value = 1001),
            numericInput("events2", "Positive events Group 2:", value = 122),
            numericInput("nonevents2", "Negative events Group 2:", value = 302),
            actionButton("calculate", "Calculate", class = "btn-primary")
          ),
          br(),
          br(),
          br(),
          tags$div(
            class = "explanation",
            h4("Explanation:"),
            p("This application allows comparing Odds Ratios obtained from two different groups."),
            p("Assuming the odds ratios are independent, you can proceed as it is common in general estimations, only considering that log(odds) is normally distributed."),
            p("The difference of the compared log odds, is named δ. The standard error of δ is SE1^2 + SE2^2^0.5. Then you can obtain a p-value for the ratio z = δ / SE(δ) from the standard normal."),
            br(),
            p("The standard error of logOR is the square root of the sum of the reciprocals of the frequencies:"),
            p("SE(logOR) = ((1 / n1) + (1 / n2) + (1 / n3) + (1 / n4)) ^ 0.5"),
            p("being n1, number of positive events group 1, n2 number of negative events group 1, and n3 and n4 the corresponded number of events for group 2."),
            h4("References:"),
            tags$ol(
              tags$a(href = "https://www.thelancet.com/journals/eclinm/article/PIIS2589-5370(21)00406-5/fulltext", "[1] Gonzalez et al.  (2021). Effectiveness of the first component of Gam-COVID-Vac (Sputnik V) on reduction of SARS-CoV-2 confirmed infections, hospitalisations and mortality in patients aged 60-79: a retrospective cohort study in Argentina. eClinicalMedicine."),
              br(),
              tags$a(href = "https://www.sciencedirect.com/science/article/pii/S2667193X22001338?via%3Dihub",  "[2] Gonzalez et al., (2022). Effectiveness of BBIBP-CorV, BNT162b2 and mRNA-1273 vaccines against hospitalisations among children and adolescents during the Omicron outbreak in Argentina. The Lancet Regional Health: Americas.")
            )
            
          ),
          br(),
          br(),
          br(),
          uiOutput("resultSection")
        )
      )
    )
  )
)

# Define the Shiny application server
server <- function(input, output) {
  # Function
  test_significance2 <- function(OR1, OR2, num_events1, num_nonevents1, num_events2, num_nonevents2) {
    log_OR1 <- log(OR1)
    log_OR2 <- log(OR2)
    se <- sqrt((1/num_events1) + (1/num_events2) + (1/num_nonevents1) + (1/num_nonevents2))
    z_score <- (log_OR1 - log_OR2) / se
    p_value <- (1 - pnorm(abs(z_score))) * 2
    return(p_value)
  }
  # Calculate the p-value for the test
  observeEvent(input$calculate, {
    p_value <- test_significance2(input$OR1, input$OR2, input$events1, input$nonevents1, input$events2, input$nonevents2)
    
    output$resultSection <- renderUI({
      tagList(
        h3("Test Result"),
        verbatimTextOutput("result")
      )
    })
    
    output$result <- renderPrint({
      paste("The p-value is:", p_value)
    })
  })
}

# Run the Shiny application
shinyApp(ui = ui, server = server)
