#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(googlesheets4)
library(dplyr)

# Leer la hoja de Google Sheets
sheet_url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQFy-ameQXk6t28oLEGQN8jZ4xS1naprIubUXErAtal8qmdNSz91Q-siZ5hJNtYTfnoRLOL3Vex9B-V/pub?gid=0&single=true&output=csv"
questions <- read_csv(sheet_url)


ui <- fluidPage(
  titlePanel("Quiz App"),
  sidebarLayout(
    sidebarPanel(
      numericInput("num_questions", "Number of questions:", min = 1, max = nrow(questions), value = 5),
      actionButton("start_quiz", "Start Quiz"),
      br(),
      textOutput("question_number")
    ),
    mainPanel(
      uiOutput("quiz_ui"),
      textOutput("score"),
      actionButton("submit", "Submit"),
      actionButton("next_question", "Next"),
      useShinyjs()  # Para usar shinyjs
    )
  )
)

# Definir la lógica del servidor (Server)
server <- function(input, output, session) {
  selected_questions <- reactiveVal(data.frame())
  question_index <- reactiveVal(0)
  score <- reactiveVal(0)
  show_answer <- reactiveVal(FALSE)
  selected_option <- reactiveVal(NULL)
  
  observeEvent(input$start_quiz, {
    num_questions <- input$num_questions
    set.seed(123)  # Para reproducibilidad, opcional
    selected_questions(questions[sample(nrow(questions), num_questions), ])
    question_index(1)
    score(0)
    show_answer(FALSE)
    selected_option(NULL)
    load_question()
  })
  
  load_question <- function() {
    if (question_index() <= nrow(selected_questions())) {
      question <- selected_questions()[question_index(), ]
      output$quiz_ui <- renderUI({
        question_text <- h3(question$question)
        choices <- c(question$option_1, question$option_2, question$option_3, question$option_4)
        if (!show_answer() || is.null(selected_option())) {
          list(
            question_text,
            radioButtons("answers", "Options", choices = choices, selected = selected_option())
          )
        } else {
          correct_answer <- paste("Correct answer:", question[[question$correct_answer]])
          answer_color <- ifelse(selected_option() == question$correct_answer, "green", "red")
          list(
            question_text,
            radioButtons("answers", "Options", choices = choices, selected = selected_option()),
            HTML(paste("<span style='color:", answer_color, "'>", correct_answer, "</span>"))
          )
        }
      })
      output$question_number <- renderText({
        paste("Question", question_index(), "of", nrow(selected_questions()))
      })
    } else {
      output$quiz_ui <- renderUI({
        h3("Quiz Completed!")
      })
      output$score <- renderText({
        paste("Your final score is:", score(), "out of", nrow(selected_questions()))
      })
      output$next_question <- renderUI({
        NULL
      })
      output$submit <- renderUI({
        NULL
      })
    }
  }
  
  observeEvent(input$submit, {
    selected_option(input$answers)
    show_answer(TRUE)
    shinyjs::toggleState(id = "next_question", condition = TRUE)
    shinyjs::toggleState(id = "submit", condition = FALSE)
  })
  
  observeEvent(input$next_question, {
    question_index(question_index() + 1)
    show_answer(FALSE)
    shinyjs::toggleState(id = "next_question", condition = FALSE)
    shinyjs::toggleState(id = "submit", condition = TRUE)
    load_question()
  })
  
  observeEvent(input$answers, {
    selected_option(input$answers)
  })
  
  observe({
    if (question_index() > nrow(selected_questions())) {
      shinyjs::toggleState(id = "next_question", condition = FALSE)
      shinyjs::toggleState(id = "submit", condition = FALSE)
    } else {
      shinyjs::toggleState(id = "next_question", condition = FALSE)
      shinyjs::toggleState(id = "submit", condition = TRUE)
    }
  })
}



# Iniciar la aplicación Shiny
shinyApp(ui, server)
