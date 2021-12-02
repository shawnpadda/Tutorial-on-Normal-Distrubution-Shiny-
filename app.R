#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(bslib)
library(magrittr)
library(htmltools)
library(ggplot2)

ui <- fluidPage(
    theme = bs_theme(bg ="#fff" , fg ="#1805ed", alpha = 2.4),
    # Use the titlePanel() function to define an app title.
    titlePanel(h1("A Lesson on Normal Distribution")),
    
    
    # Use the sidebarlayout() function to generate the sidebar and main panels.
    sidebarLayout(
      
      # Use a sidebarpanel() function that consist of input controls.
      sidebarPanel(
        
        
        # To input: select a dataset by user, use the selectInput() function.
        # To input: select a dataset by user, use the selectInput()     function. 
        #selectInput(inputId = "dataset",
                   # label = "Choose a dataset:",
                    #choices = c("Simulation", "original")),
        tabPanel("Introduction")
        ),
      
      # For displaying outputs, use a mainPanel().
      mainPanel("Normal Distribution Visualizations",
                
                navlistPanel(
                  
                  
                  tabPanel("Visualization 1",
                           fluidRow(textOutput("text1")),
                           br(),
                           fluidRow(
                             
                             selectInput(inputId = "bins",
                                         label = "Select number of bins",
                                         choices = c(10,20,30,40,50,60), selected = 20),
                             
                             
                             sliderInput(inputId = "mean",
                                         label = " Define population mean",min = 0, max = 100, value = 0)),
                           
                            br(),
                           
                            fluidRow(
                              
                              numericInput(inputId = "n",
                                           label = "Define sample size",
                                           value = 1000, min = 1),
                              
                             sliderInput(inputId = "sd",label = "Define population standard deviation",
                                         min = 0, max = 100, value = 1),
                             
                             plotOutput("plot"),
                             textOutput("text2")),
                             br(),
                           fluidRow(
                             verbatimTextOutput("summary1")
                            )), 
                           br(),
                           tabPanel("Visualization 2", fluidRow(
                             
                             selectInput(inputId = "Bins",
                                         label = "Select number of bins",
                                         choices = c(10,20,30,40,50,60), selected = 20),
                             plotOutput("plot1"),verbatimTextOutput("summary2")
                           ))
                    )
                          #,
                  #tabPanel("Summary",verbatimTextOutput("summary1")
                          # verbatimTextOutput("summary2")))#,
                  #tabPanel("Summary",verbatimTextOutput("summary1")),
                  #tabPanel("Tables",dataTableOutput("table"))
      )
    )
  )
  
  

  # Define a server function that provides the computer instructions 
  # for building a shinyapp.
server <- function(input, output){
    
  dat <- reactive({
    rnorm(input$n, mean = input$mean, sd = input$sd)
  })
  
 # new_dataset <- reactive({
    #switch(input$dataset,
          # "Simulation" = dat(),
           #"original" = mtcars)
  #})
  
  output$plot <- renderPlot({
    dataset <- dat()
    x <- data.frame(dataset)
    x %>%
    ggplot(aes(x = dataset))+ geom_histogram(aes(y = ..density..),bins = input$bins, color = "black", fill = "white") +
      #The default mean and standard deviation for the dnorm() function is 0 and 1, respectively.
      stat_function(fun = dnorm, args = list(mean = mean(dataset, na.rm = T), sd = sd(dataset, na.rm = T)),col = "red", size =1.2) + labs(title = paste("Histogram of",input$n,"Normal Distribution"), x = "x", y ="density") +
      theme(plot.title = element_text(face = "bold")) + geom_vline(xintercept = input$mean, linetype = "dashed", col = "red") + geom_vline(xintercept = input$sd, linetype = "dashed", col = "blue") 
  })
  
  output$plot1 <- renderPlot({
      ggplot(data = mtcars, aes(x = mpg)) + geom_histogram(aes(y = ..density..),bins = input$Bins, color = "black", fill = "white") + geom_density()+
      #The default mean and standard deviation for the dnorm() function is 0 and 1, respectively.
  labs(title = "Histogram of mt cars data set") +
      theme(plot.title = element_text(face = "bold"))  
  })
  output$summary2 <- renderPrint({
    summary(mtcars)
  })
  
  #output$text3 <- renderText({
    #paste("This is an original dataset.")
  #})
  # hist(rnorm(input$n, mean = input$mean, sd = input$sd), main = paste("Histogram of",input$n,"observations from normal distribution"), xlab = "x")
  
   output$text2 <- renderText({
    paste("Histogram of ", input$n,"observations from normal distribution")
   })
   
   output$text1 <- renderText({
     paste("This is a random sample from normal distribution of sample size defined by the user.")
   })
   
   output$summary1 <- renderPrint({
     summary(dat())
   })
  
  }
  
    
    # Create a plot of the desired dataset.
    #output$plot <- renderPlot({
     # dataset <- datasetInput()
      
      # Using the ggplot2 approach, display the plot.
      #ggplot(data = dataset, aes(x = when, y = temperature)) +
      #  geom_line(size = 2, col = "slate gray") + geom_smooth(se=FALSE)
      
    #})
    
    #output$table <- renderDataTable({
    #  dataset <- datasetInput()
   # })
    
    #output$summary1 <- renderPrint({
     # summary(dataset <- datasetInput())
    #})
    #output$text1 <- renderText({
      #paste("The two data frames are included in the package: whately_2015 and orchard_2015. These contain weather data averaged over every ten minute period for the calendar year 2015.")
    #})
  #}

options = list(width = 900, height = 900)

shinyApp(ui, server)
