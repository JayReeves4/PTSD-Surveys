#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(dplyr)
library(ggplot2)
library(tidyr)


# Read the survey questions
survey <- read.csv("Survey_Qs.csv")
survey <- survey[1,]

shinyServer(function(input, output) {
  
  # Create an empty vector to hold survey results
  results <<- rep("", nrow(survey))
  # Name each element of the vector based on the
  # second column of the survey questions
  names(results)  <<- survey[,2]
  # 
  # # Hit counter
  # output$counter <- 
  #   renderText({
  #     if (!file.exists("counter.Rdata")) counter <- 0
  #     if (file.exists("counter.Rdata")) load(file="counter.Rdata")
  #     counter <- counter <<- counter + 1
  #     
  #     save(counter, file="counter.Rdata")     
  #     paste0("Hits: ", counter)
  #   })
  # 
  # This renderUI function holds the primary actions of the
  # survey area.
  output$MainAction <- renderUI( {
    dynamicUi()
  })
  
  # Dynamic UI is the interface which changes as the survey
  # progresses.  
  dynamicUi <- reactive({
    # Initially it shows a welcome message. 
    if (input$Click.Counter == 0) 
      return(
        list(
          h2("Welcome to your PTSD Shiny Survey Tool"),
          br(),
          h4("Hit the 'Next' button when you are ready to begin")
        )
      )
    
    # Once the next button has been clicked once we see each question
    # of the survey.
    if (input$Click.Counter > 0 & input$Click.Counter <= nrow(survey))  
      return(
        list(
          h5(textOutput("Question")),
          selectInput("survey", "Please Select:", 
                       c( option.list(), "Prefer not to answer"))
        )
      )
    
    # Finally we see results of the survey as well as a
    # download button.
    if (input$Click.Counter > nrow(survey))
      return(
        list(
          h4("View aggregate results"),
          tableOutput("surveyresults"),
          h4("Please download your results and email them to your provider."),
          downloadButton('downloadData', 'Download Individual Results'),
          br(),
          h6("Haven't figured out how to get rid of 'next' button yet"),
          h6("Thanks for participating in the survey!")
        )
      )    
  })
  
  # This reactive function is concerned primarily with
  # saving the results of the survey for this individual.
  output$save.results <- renderText({
    # After each click, save the results of the radio buttons.
    if ((input$Click.Counter > 0) & (input$Click.Counter > !nrow(survey)))
      try(results[input$Click.Counter] <<- input$survey)
    # try is used because there is a brief moment in which
    # the if condition is true but input$survey = NULL
    
    # If the user has clicked through all of the survey questions
    # then R saves the results to the survey file.
    if (input$Click.Counter == nrow(survey) + 1) {
      if (file.exists("survey.results.Rdata")) 
        load(file = "survey.results.Rdata")
      if (!file.exists("survey.results.Rdata")) 
        presults <- NULL
      presults <- presults <<- rbind(presults, results)
      rownames(presults) <- rownames(presults) <<- 
        paste("User", 1:nrow(presults))
      save(presults, file = "survey.results.Rdata")
    }
  # Because there has to be a UI object to call this function I set up render text that displays the content of this funciton.
    ""
  })
  
  # This function renders the table of results from the survey.
  output$surveyresults <- renderTable({
    t(summary(presults))
  })
  
  # This renders the data downloader
  output$downloadData <- downloadHandler(
    filename = "IndividualData.csv",
    content = function(file) {
      write.csv(presults, file)
    }
  )
  
  # The option list is a reative list of elements that updates itself when the click counter is advanced.
  option.list <- reactive({
    qlist <- survey[input$Click.Counter, 3:ncol(survey)]
    # Remove items from the questions if the option is empty.
    # Also, convert the option list to matrix. 
    as.matrix(qlist[qlist != ""])
  })
  
  # This function show the question number (Q:)
  # Followed by the question text.
  output$question <- renderText({
    paste0(
      "Q", input$Click.Counter,":", 
      survey[input$Click.Counter, 2]
    )
  })
  
})