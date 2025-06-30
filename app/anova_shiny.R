
library(shiny)
library(ggplot2)
library(bslib)

ui <- fluidPage(
  theme = bs_theme(version = 4, bootswatch = "flatly"),
  titlePanel("ANOVA für Mittelwerte"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("val1", "A1", value = 1, min = 1, max = 10, step = 0.1),
      sliderInput("val2", "A2", value = 2, min = 1, max = 10, step = 0.1),
      sliderInput("val4", "B1", value = 3, min = 1, max = 10, step = 0.1),
      sliderInput("val5", "B2", value = 4, min = 1, max = 10, step = 0.1),
      width = 3,
    # add a line break in sidebarpanel
    tags$br(),
    # add text  in the sidebarpanel
    tags$p(HTML("Mögliche Werte für alle Fälle: <br>
           Minimum: 1; Maximum: 10; Schrittweite: 0.1"),
           width = 3)
    ),
    mainPanel(
      plotOutput("anovaPlot"),
      width = 6
    )
  )
)

server <- function(input, output) {
  output$anovaPlot <- renderPlot({
    df <- data.frame(
      case_id = c(1, 2, 4, 5),
      value = c(input$val1, input$val2, input$val4, input$val5)
    )
    df$mean <- mean(df$value)

    # Calculate variances
    total_var <- var(df$value)

    # Calculate group means for A and B
    group_A_mean <- mean(c(input$val1, input$val2))
    group_B_mean <- mean(c(input$val4, input$val5))
    overall_mean <- mean(df$value)

    # Between-groups variance
    between_var <- mean(c((group_A_mean - overall_mean)^2, (group_B_mean - overall_mean)^2))

    # Within-groups variance
    within_A <- mean(c((input$val1 - group_A_mean)^2, (input$val2 - group_A_mean)^2))
    within_B <- mean(c((input$val4 - group_B_mean)^2, (input$val5 - group_B_mean)^2))
    within_var <- mean(c(within_A, within_B))
    F_score <- between_var / within_var

    ggplot(data = df, aes(x = case_id, y = value)) +
      geom_point() +
      geom_segment(aes(x = 3 - 0.5, xend = 3 + 0.5,
                       y = overall_mean, yend = overall_mean)) +
      geom_segment(aes(x = 1.25, xend = 1.75,
                       y = group_A_mean, yend = group_A_mean),
                   color = "skyBlue", size = 1) +
      geom_segment(aes(x = 4.25, xend = 4.75,
                       y = group_B_mean, yend = group_B_mean),
                   color = "orange", size = 1) +
      scale_x_continuous(breaks = df$case_id,
                         labels = c("A1", "A2", "B1", "B2")) +
      scale_y_continuous(breaks = seq(1, 10, by = 1),
                         limits = c(1, 10)) +
      labs(x = "Beobachtungen", y = "Variablenwert",
           title = paste("Illustration der ANOVA (F = ", round(F_score, digits = 2), ")", sep = ""),
           caption = sprintf("Gesamtvarianz: %.2f\nZwischen-Varianz: %.2f\nBinnen-Varianz: %.2f",
                             total_var, between_var, within_var)) +
      theme_classic()
  }, res = 96)
}

shinyApp(ui = ui, server = server)
