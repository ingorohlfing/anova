
library(shiny)
library(ggplot2)
library(bslib)

ui <- fluidPage(
  theme = bs_theme(version = 4, bootswatch = "flatly"),
  tags$head(
    tags$style(HTML("
      .form-group {
        margin-bottom: 0px;  /* Reduce spacing between sliders */
      }
    "))
  ),
  titlePanel("ANOVA für Mittelwerte"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("val1", "A1", value = 1, min = 1, max = 10, step = 0.1),
      sliderInput("val2", "A2", value = 2, min = 1, max = 10, step = 0.1),
      sliderInput("val4", "B1", value = 3, min = 1, max = 10, step = 0.1),
      sliderInput("val5", "B2", value = 4, min = 1, max = 10, step = 0.1),
      sliderInput("val7", "C1", value = 5, min = 1, max = 10, step = 0.1),
      sliderInput("val8", "C2", value = 6, min = 1, max = 10, step = 0.1),
     # add a line break in sidebarpanel
    tags$br(),
    tags$p("Creator: Ingo Rohlfing",
           tags$a(href = "https://github.com/ingorohlfing", "(https://github.com/ingorohlfing)", target = "_blank")),
           width = 3
    ),
    mainPanel(
      plotOutput("anovaPlot"),
      div(style = "margin-top: 20px;",
          textOutput("infoText")),
      width = 7
    )
  )
)

server <- function(input, output) {
  output$anovaPlot <- renderPlot({
    df <- data.frame(
      case_id = c(1, 2, 4, 5, 7, 8),
      value = c(input$val1, input$val2, input$val4, input$val5, input$val7, input$val8)
    )
    df$mean <- mean(df$value)

    # Calculate variances
    total_var <- var(df$value)
    total_sums <- 5 * total_var

    # Calculate group means for A and B
    group_A_mean <- mean(c(input$val1, input$val2))
    group_B_mean <- mean(c(input$val4, input$val5))
    group_C_mean <- mean(c(input$val7, input$val8))
    overall_mean <- mean(df$value)

    # Between-groups variance
    between_var <- (2 *(group_A_mean - overall_mean)^2 +
                      2 *(group_B_mean - overall_mean)^2 +
                      2 * (group_C_mean - overall_mean)^2) / 2
    # between-groups sum of squares
    between_sums <- 2 * between_var

    # Within-groups variance
    within_A <- sum(c((input$val1 - group_A_mean)^2, (input$val2 - group_A_mean)^2))
    within_B <- sum(c((input$val4 - group_B_mean)^2, (input$val5 - group_B_mean)^2))
    within_C <- sum(c((input$val7 - group_C_mean)^2, (input$val8 - group_C_mean)^2))
    within_var <- sum(c(within_A, within_B, within_C))/ 3
    # within-groups sum of squares
    within_sums <- 3 * within_var
    F_score <- between_var / within_var

    ggplot(data = df, aes(x = case_id, y = value)) +
      geom_point() +
      geom_segment(aes(x = 4.5 - 0.5, xend = 4.5 + 0.5,
                       y = overall_mean, yend = overall_mean)) +
      geom_segment(aes(x = 1.25, xend = 1.75,
                       y = group_A_mean, yend = group_A_mean),
                   color = "skyBlue", size = 1) +
      geom_segment(aes(x = 4.25, xend = 4.75,
                       y = group_B_mean, yend = group_B_mean),
                   color = "orange", size = 1) +
      geom_segment(aes(x = 7.25, xend = 7.75,
                       y = group_C_mean, yend = group_C_mean),
                   color = "#CC79A7", size = 1) +
      scale_x_continuous(breaks = df$case_id,
                         labels = c("A1", "A2", "B1", "B2", "C1", "C2")) +
      scale_y_continuous(breaks = seq(1, 10, by = 1),
                         limits = c(1, 10)) +
      labs(x = "Beobachtungen", y = "Variablenwert",
           title = paste("Illustration der ANOVA (F = ", round(F_score, digits = 2), ")", sep = ""),
           caption = sprintf("Gesamtvarianz: %.2f (Gesamt-QS: %.2f)\nZwischen-Varianz: %.2f (Zwischen-QS: %.2f)\nBinnen-Varianz: %.2f (Binnen-QS: %.2f)",
                             total_var, total_sums,
                             between_var, between_sums,
                             within_var, within_sums)) +
      theme_classic()
  }, res = 96)
  output$infoText <- renderText({
    "A, B und C sind Bezeichnungen für drei Gruppen. 1 und 2 stellen jeweils zwei
    Beobachtungen in den Gruppen dar. Die schmalen Horizontalen zwischen zwei
    Beobachtungen sind die jeweiligen Gruppenmittelwerte. Die weitere, schmalere
    Horizontale ist der Gesamtgruppenmittelwert. QS bedeutet Quadratsumme."
  })
}

shinyApp(ui = ui, server = server)
