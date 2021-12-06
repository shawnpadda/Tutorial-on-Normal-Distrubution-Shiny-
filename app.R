
# A SHINY DASHBOARD demonstrates the Normal Distribution.

# Load all of the essential libraries.
library(shiny)
library(ggplot2)
library(bslib)
library(tidyverse)
library(shinydashboard)
library(ISLR)
library(htmltools)
library(car)
library(plotly)
library(shinythemes)
library(openintro)


# Develop a user interface file that incorporates a dashboard page, 
# a dashboard header, a dashboard sidebar, and a dashboard body.
ui <- dashboardPage(  
      
      # To give the shiny app a title, use the dashboardHeader() function.
      dashboardHeader(title = "Normal Distribution "
      ),
  
      # Integrate multiple sidebar Menu items and menu sub items to create a dashboard sidebar, 
      # use a dashboardSidebar() function.
      dashboardSidebar(
         sidebarMenu(
         menuItem("Learning Objectives", tabName = "Objectives", icon = icon("fas fa-ruler-vertical")),
         menuItem("Learning Components", tabName = "Components", icon = icon("fas fa-server")),
         menuItem("Real", tabName = "Real", icon = icon("fas fa-chart-bar")),
         menuItem("Simulation", tabName = "Simulation", icon = icon("far fa-chart-bar")),
         menuItem("Findings", tabName = "Findings", icon = icon("far fa-file-image")),
         menuItem("Summary", tabName = "Summary", icon = icon("list-alt")),
         menuItem("Further References", startExpanded = TRUE,icon = icon("link"),
               menuSubItem("Link1", href = "https://www.simplypsychology.org/normal-distribution.html"),
               menuSubItem("Link2", href = "https://mathworld.wolfram.com/NormalDistribution.html"),
               menuSubItem("Link3", href = "https://www.annualreviews.org/doi/full/10.1146/annurev.publhealth.23.100901.140546#_i3"))
      )
     ),
  
     # Build a dashboard body by blending various tab items, navbar pages, fluid rows, 
     # tab panels, and tab boxes, use a dashboardBody() function.
     dashboardBody(
        tabItems(
      
        # Add a first tab to the tab list.
        tabItem(tabName = "Objectives",
              navbarPage("Learning Objectives", theme = shinytheme("united")), 
                         tabPanel("Key Learning objectives",htmlOutput("object"))
        ),
    
        # Add a second tab to the tab list.
         tabItem(tabName = "Components",
           navbarPage("Learning Components", theme = shinytheme("united")), 
             fluidRow(
                withMathJax(),
                tabBox(
                  tabPanel("Introduction", htmlOutput("Intro")),
                  tabPanel("Key terms", htmlOutput("keys")),
                  tabPanel("Empirical Rule", plotOutput("empirical"), htmlOutput("out")),
                  tabPanel("Formulas",htmlOutput("formula"),htmlOutput("formula1")),
                  tabPanel("Normality Tests", htmlOutput("tex")),height = 200, width = 350
            )
          )
        ),
    
        # Add a third tab to the tab list.
        tabItem(tabName = "Simulation", 
             navbarPage("Application of normal distribution in simulation dataset", 
                        theme = shinytheme("united")),
               fluidRow(  
                box(
                title = "Input controls", width = 6, solidHeader = TRUE, 
                status = "primary",
                background = "light-blue",
                sliderInput(inputId = "n",
                         label = "Define sample size:",
                         value = 277, min = 1, max = 500),
            selectInput(inputId = "bins",
                                       label = "Choose number of bins",
                                       choices = c(6,10,16,20,26), selected = 20),
            numericInput(inputId = "mean",
                          label = " Input mean:",min = 0, max = 500, value = 0),
            numericInput(inputId = "sd",label = "Input standard deviation:",
                         min = 0, max = 100, value = 1),
            selectInput(inputId = "Col1",
                        label = "Choose a color",
                        choices = c("blue","chocolate","burlywood", 
                                    "darkturquoise","darksalmon"), 
                        selected = "darksalmon"), height = 450
        ),
    
        tabBox(
            tabPanel("About",htmlOutput("text2")),
            tabPanel("Plot1",plotOutput("plot"),htmlOutput("text1")),
            tabPanel("Plot2",plotOutput("plo"),htmlOutput("txt1")),
            tabPanel("Plot3",plotOutput("pl"),htmlOutput("txt2")),
            tabPanel("Normality test",verbatimTextOutput("test1")),
            tabPanel("Summary",verbatimTextOutput("summary1"),
                     verbatimTextOutput("sum3")),height = 250
           )
         )
       ),
      
        # Add a fourth tab to the tab list. 
        tabItem(tabName = "Real",
            navbarPage("Application of normal distribution in real dataset", 
                       theme = shinytheme("united")),
              fluidRow(
               box(
               title = "Input controls", width = 6, solidHeader = TRUE, 
               status = "primary",
               background = "light-blue",
          
            numericInput(inputId = "M",
                               label = "Input Observations:",
                               value = 247, min = 1, max = 300),
            selectInput(inputId = "Col",
                              label = "Choose a color",
                              choices = c("blue","limegreen","khaki", 
                                          "tan","rosybrown"), 
                        selected = "blue"), height = 250),
        tabBox(
              tabPanel("About",htmlOutput("text3")),
              tabPanel("Plot1",plotOutput("plot1"),htmlOutput("txt3")),
              tabPanel("Plot2",plotOutput("plot2"),htmlOutput("txt4")),
              tabPanel("Plot3",plotOutput("plot3"),htmlOutput("txt5")),
              tabPanel("Normality test",verbatimTextOutput("test")),
              tabPanel("Summary",verbatimTextOutput("summary2"),
                       verbatimTextOutput("summary3")),height = 250)
           )
         ),
      
         # Add a fifth tab to the tab list.
         tabItem(tabName = "Findings",
              navbarPage("Outcomes of the Simulated and Real Dataset", 
                         theme = shinytheme("united")),
              tabPanel("Key Findings",htmlOutput("find"))
         ),
      
         # Add a sixth tab to the tab list.
         tabItem(tabName = "Summary",
              navbarPage("Synopsis of the Lesson", theme = shinytheme("united")),
              tabPanel("Summary",htmlOutput("sum")))
   )
  )
 )
  
    
    
 # Create a server function to convey computer instructions for developing a shinyapp.
 server <- function(input, output){
      
      # Create a dat reactive function that includes random values from the normal distribution.
      dat <- reactive({
          # Set the seed so that the best sample from a normal distribution is reproduced.
          set.seed(300)
          # To produce pseudo random numbers with a user-friendly size, mean, 
          # and standard deviation, use the rnorm() function.
          rnorm(input$n, mean = input$mean, sd = input$sd)
      })
      
      # To display the plot, use the renderPlot() function, and 
      # the ggplot2 package to actually create the plot.
      output$plot <- renderPlot({
          dataset <- dat()
          # Create a new variable called x and assign to a dataset data frame. Construct a pipeline.
          x <- data.frame(dataset)
          x %>%
          ggplot(aes(x = dataset)) + geom_histogram(aes(y = ..density..),
            bins = input$bins, color = "white", fill = input$Col1, size = 0.5) +
          stat_function(fun = dnorm, args = list(mean = mean(dataset, na.rm = T), 
                          sd = sd(dataset, na.rm = T)),col = "red", size =1.2) + 
          labs(title = "Histogram of simulated data set" ,subtitle = paste("A bar graph of",input$n,"observations"), x = "x", 
               y ="Density", x = " ") + theme_classic() + 
          theme(plot.title = element_text(face = "bold", hjust = 0.5),
                plot.subtitle = element_text(face = "italic", hjust = 0.5)) + 
            geom_vline(xintercept = input$mean, linetype = "dashed", 
                       col = "#95a0e1")
          # + scale_x_continuous(breaks = seq(from = 0, to = 190, by=40)) 
          # +  geom_vline(xintercept = input$sd, linetype = "dashed", 
          # col = "#9bc088") 
      })
      
      # To display the violin plot, use the renderPlot() function.
      output$plo <- renderPlot({
         dataset <- dat()
         x <- data.frame(dataset)
         # Create a violin plot with the ggplot2 package.
         ggplot(data = x, aes(x = dataset, y = 0)) +  
           geom_violin(trim=FALSE, fill = input$Col1) + 
           geom_boxplot(width = 0.1, outlier.colour = "red") + 
         coord_flip() + labs(title = "Violin Plot", subtitle = 
                               paste("A Violin plot of",input$n,"Observations"), 
                             x = "Simulated values")+
         theme(plot.title = element_text(face = "bold", hjust = 0.5),
               plot.subtitle = element_text(face = "italic", hjust = 0.5))
       
      })
      
      # Use the renderPrint() function to print the standard deviation.
      output$sum3 <- renderPrint({
        cat("The standard deviation is",sd(dat()),".")
      })
      
      # Use the renderPrint and summary() functions to print the summary statistics.
      output$summary1 <- renderPrint({
        summary(dat())
      })
      
      # To display the plot of the normal Q-Q plot, use the renderPlot() function.
      output$pl <- renderPlot({
        qqPlot(dat(), main = paste("Normal Q-Q Plot of",input$n,"Observations"), 
               ylab ="Sample Quantiles", 
               xlab ="Norm Quantiles",
               col = input$Col1)
      })
      
      
      # Conduct a Shapiro-Wilk test on a simulated data set.
      output$test1 <- renderPrint({
        Simulated_dataset <- dat()
        shapiro.test(Simulated_dataset)
      })
      
      # Use a renderText() function to provide the description about the simulated data set.
      output$text1 <- renderText({
        HTML(paste0("This is a random sample of ",input$n," ","observations from normal distribution. 
                    The default mean is set to 0, and the default standard deviation is 1. 
                    The probability density function (pdf) plotted as red curve."))
      })
      
      # To explain the component violin plot of a simulated dataset, use the renderText() function.
      output$txt1 <- renderText({
        HTML(paste0("<li> A violin plot of ",input$n," ","observations from normal distribution. 
                    It uses a Probability Density function (PDF) to show the shape of the data set.
                    The width of the PDF shows the number of times that value occurs in the data set. 
                    <li> A violin plot is a hybrid of a box plot, reveals the probability density function (PDF), 
                    interquartile range (IQR), and any outliers.The red dots located outside the whiskers 
                    of the boxplot that signify potential outliers. Overall, the violin plots provide the 
                    user more information."))
      })
      
      # To explain the element normal Q-Q plot of a simulated dataset, use the renderText() function.
      output$txt2 <- renderText({
        HTML(paste0("The normal Quantile Quantile (Q-Q) plot of ",input$n," ","observations from normal distribution. 
                    It indicates the data follows a straight line. The deviations from the straight line are minimal. 
                    The Q-Q plot indicates that the points in the graph that fall outside the confidence interval 
                    are potential outliers. "))
      })
      
      # To explain the elements of a simulated dataset, use the renderText() function.
      output$text2 <- renderText({
        HTML(paste("This section contains details about the simulated dataset, a description 
               of the population mean and standard deviation, plots, a normality test, and 
               summary statistics.
              <li><b> Description of the dataset:</b> The simulated dataset includes user-friendly pseudo-random 
              numbers generated from a normal distribution. </b> 
              <li><b> Description of the mean and standard deviation: </b>This area is designed to be user-friendly. 
              The user will input the actual mean and standard deviation from the 
              original dataset and see whether there are any significant discrepancies in the results.</b>
              <li><b> Description of the plots: </b>The histogram, normal Q-Q plot, and box plot are used in this 
              section to compare the results obtained in the real dataset with the results obtained in the simulated dataset. 
              These graphs can be compared to real datasets by the user.</b>
              <li><b> Interpretation of the normality test:</b> The normality test indicates the p-value is relatively large, which means the 
                    hypothesis of normality failed to reject, and the distribution of the data does not differ from the normal distribution.
              <li><b> Description of the summary statistics:</b> The summary statistics provide information about the 
                   simulated dataset's summary, standard deviation. "))
      })
      
      #  To highlight the description of a real-world data set, use the renderText() function.
      output$text3 <- renderText({
        HTML(paste("This section includes information about the bdims dataset from the openintro package in R, a description 
               of the target variable, plots, normality test, and 
               summary statistics.
              <li><b> Description of the dataset:</b> The bdims is a very famous dataset which includes the body girth measurements 
              and skeletal diameter measurements, as well as age, weight, height and gender, 
              are given for 507 physically active individuals - 247 men and 260 women. 
              This dataset is extremely useful for both education and data analysis. </b> 
              <li><b> Description of the target variable: </b>The target variable is the height of the male gender. 
              The gender/sex variable has been filtered to just include males.</b>
              <li><b> Description of the plots: </b>In this section, the target variable is targeted using three plots as follows: 
              smooth density plot, normal Q-Q plot, and violin plot.</b>
              <li><b> Interpretation of the normality test:</b> The normality test indicates the p-value is greater 
              than 0.05 (commonly used level of significance) we assume a normal distribution.
                    It indicates that the distribution of the data does not vary from the normal distribution.
              <li><b> Description of the summary statistics:</b> The summary statistics provide information about 
                   the dataset's summary, standard deviation, and dimensions. "))
      })
      
     
      
      # To display the smooth density plot, use the renderPlot() function.
      output$plot1 <- renderPlot({
        
        # Create a new dataframe called new_mdat. 
        # It includes the pipeline of bdims data set.
        new_mdat <-   bdims %>%
                      filter(sex == 1) %>%
                      drop_na()
        # Create a smooth density plot using a ggplot2 package.
        ggplot(data = new_mdat[1:input$M,], aes(x = hgt )) +
          geom_density(fill = input$Col, size = 0.2, lty= 3) + 
          labs(title= "Smooth Density Plot",subtitle = paste("A smooth density plot of",input$M,"observations"), 
               x = "Males Height", y ="Density") + 
          stat_function(fun = dnorm, args = list(mean = mean(new_mdat$hgt, 
          na.rm = T), sd = sd(new_mdat$hgt, na.rm = T)),col = "black", 
          size = 1.5) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5),
                plot.subtitle = element_text(face = "italic", hjust = 0.5)) + 
          geom_vline(xintercept = mean(new_mdat$hgt, na.rm = T) , 
                     linetype = "dashed", col = "slategrey") 
        #+ scale_x_continuous(breaks = seq(from = 0, to = 190, by=20))
         
      })
       
      # Use a renderPlot() function, the ggplot2 package and the bdims data set pipeline.
      output$plot3 <- renderPlot({
        new_mdat <- bdims %>%
          filter(sex == 1) %>%
          drop_na()
        # To generate the violin plot, use the ggplot() global statement and the extensions.
        ggplot(data = new_mdat[1:input$M,], aes(x = hgt,y=0)) + 
          geom_violin(trim=FALSE, fill = input$Col) + 
          geom_boxplot(width = 0.1, outlier.colour = "red") +coord_flip() + 
          labs(title="Violin PLot",subtitle = paste("A violin plot of",input$M,"observations"),
               x ="Height (in centimeters)", y= "Sex (Male)") +
          theme(plot.title = element_text(face = "bold", hjust = 0.5),
                plot.subtitle = element_text(face = "italic", hjust = 0.5)) 
        # geom_boxplot(col="black",fill = input$Col, outlier.colour = "red") 
      })
      
      # To print the summary, use the renderPrint() function. 
      # The bdims data set pipeline is included.
      output$summary2 <- renderPrint({
        new_mdat <- bdims %>%
           filter(sex == 1) %>%
           drop_na()
        # To display the summary of a variable called males height, 
        # use the summary() function.
        summary(new_mdat[1:input$M,]$hgt) 
      })
      
      # Concatenate the standard deviations using the renderPrint() function and cat().
      output$summary3 <- renderPrint({
        new_mdat <- bdims %>%
           filter(sex == 1) %>%
           drop_na()
        # Use a cat() function and show the string characters and Standard deviation.
        cat("The standard deviation is",
            sd(new_mdat[1:input$M,]$hgt),".","\n" ,
            "The dimensions of the dataset is", dim(new_mdat[1:input$M,]),".")
      })
      
      # Construct a pipeline using a bdims dataset and 
      # use a renderPlot() function to display the plot.
      output$plot2 <- renderPlot({
        new_mdat <- bdims %>%
          filter(sex == 1)%>%
          drop_na()
        #  Using a qqplot() function on a real data set, plot the normal Q-Q plot.
        qqPlot(new_mdat[1:input$M,]$hgt, col = input$Col, 
               main = paste("Normal Q-Q Plot of",input$M,"Observations"), 
               ylab = "Males height", xlab = "Norm Quantiles")
      })
      
      # Conduct a Shapiro-Wilk test on a real data set.
      output$test <- renderPrint({
        new_mdat <- bdims %>%
          filter(sex == 1)%>%
          drop_na()
        
        bdim <- new_mdat[1:input$M,]$hgt
        shapiro.test(bdim)
      })
      
      # Plot smooth density using a real data set.
      output$txt3 <- renderText({
        HTML(paste0("The smooth density plot of ",input$M," ","observations from real dataset. 
                    The smooth density plot accurately depicts the distribution of males height. 
                    It illustrates the kernel density estimation (a non-parametric approach) filled 
                    with a user friendly color and 
                    the probability density function curve plotted as a black line ."))
      })
      
      # An interpretation about a normal Q-Q plot.
      output$txt4 <- renderText({
        HTML(paste0("The normal Q-Q plot of ",input$M," ","observations from real dataset. 
                     It indicates the data follows a straight line. 
                    It also ensures that how well the data satisfy the normality assumption. 
                    The Q-Q plot indicates that the points in the graph that fall outside the 
                    confidence interval are potential outliers.
                    "))
      })
      
      # Description about a violin plot.
      output$txt5 <- renderText({
        HTML(paste0("<li>A violin plot of ",input$M," ","observations from real dataset. 
                     It indicates the shape of the data set by using a Probability Density function(PDF). 
                     The PDF's width indicates how frequently that value appears in the data set. 
                     The wider regions of the violin plot indicate values that appear frequently.
                     The narrower regions of a violin plot indicate values that appear less frequently.
                     <li>Violin plots combine the usefulness and flexibility of a boxplot with the ability 
                     to visualize the underlying distribution of a histogram. A violin plot combines with a 
                     box plot shows the Probability density function (PDF) ,
                     Interquartile range (IQR) and any potential outliers. The red dots are the potential outliers.
                     A violin plot is another way to observe the distribution and look for evidence of skewness in the data.
                     There is no evidence of skewness when the median equals the mean, and then normality property 
                     is considered as very well satisfied.
                     Ultimately, the violin plots give the user more information.
                    "))
      })
      
      # Define the learning objectives.
      output$object <- renderText({
        HTML(paste0("<h4><b>The primary learning objectives that will be addressed in 
                     this lesson are listed below :</b>","<br/>",
                    "<h4><li> An overview of normal distribution",
                    "<h4><li> Key terms related to normal distribution",
                    "<h4><li> The 68-95-97 rule, often known as the empirical rule",
                    "<h4><li> Important formulas for normal distribution",
                    "<h4><li> The Shapiro-Wilk Test, which is the most commonly used normality test",
                    "<h4><li> An investigation of how accurate summary statistics and plots of a 
                    simulation dataset are when compared to the real dataset"))
      })
      
      # Define the learning components.
      output$Intro <- renderText({
        HTML(paste0("<b>Introduction:</b>","<br/>", "The normal distribution is symmetrical on both sides of the mean, 
        and it is a continuous probability distribution. It's centered at mean, with the right-hand side mirroring 
        the left-hand side. It's also known as the Gaussian distribution because the German mathematician Carl Gauss 
        was the first who defined it. The total area under the distribution curve equals one, and the area under 
        the curve reflects probability. The tails are asymptotic, meaning they approach the horizon (x-axis) but 
        never quite meet it. The normal distribution often describes a symmetric and bell-shaped curve. 
        The normal distribution can be adjusted using two parameters: mean and standard deviation.","<br/>"," 
        <b>In this lesson, we'll learn about the following elements of normal distribution:</b>","<br/>","<li>
        Population Mean","<br/>", "<li> Sample Mean","<li> Population Variance","<li>Sample Variance","<li>Population 
        Size (N) and sample size (n) ","<li>Z-distribution","<li>Z-score","","<li>Population Standard Deviation", 
        "<li> Central Limit Theorem", "<br/>",
        "<b> Properties of normal distribution: </b> <li> The mean, mode, median are equal 
        <li> The curve is symmetric around the population mean 
        <li> Half of the values are to the left of center, and half are to the right of center
        <li> The total area under the curve is 1 "))
      })
      
      # An element of the learning component tab.
      output$formula <- renderUI({
       withMathJax(HTML(paste0("<li>The probability density function (PDF) of normal distribution is -", "<br/>", 
                               " $$\\frac{1}{\\sigma\\sqrt2\\pi}*e^{\\big(-\\frac{(x-\\mu)^2}{2\\sigma^2}\\big)}$$",
                               "<li> The cumulative distribution function (CDF) of normal distribution is -","<br/>",
                               "$$\\int_{-\\infty}^{x}\\frac{1}{\\sigma\\sqrt2\\pi}*e^{\\big(-\\frac{(x-\\mu)^2}{2\\sigma^2}\\big)}dx$$")))
      })
      
      # A component of the learning tab.
      output$formula1 <- renderUI({
        withMathJax(HTML(paste0("<li> The z-score (standard score) formula for the sample is -","<br/>",
                                "$$z = \\frac{(x-\\mu)}{\\sigma}$$ Where the denominator is the standard deviation.","\n","
                                <li> The population mean is -", "<br/>", " $$\\mu = \\frac{\\sum_{i}^n x_i}{N}$$ 
                                Where N is the population size.","<br/>","\n",
                                "<li> The sample mean is -","<br/>","$$\\bar{X} = \\frac{\\sum_{i}^nx_i}{n}$$","<br/>",
                                "Where n is the sample size.",
                                "<li> The sample variance is denoted by","<br/>","$$S^2 = \\frac{\\sum_{i}^n(x_i-\\bar{X})^2}{n-1}$$ 
                                Where n is the sample size.",
                                "<li> The population variance is denoted by","<br/>","$$\\sigma^2 = \\frac{\\sum_{i}^n(x_i-\\mu)^2}{N}$$ 
                                Where N is the population size.")))
      })
      
      # An element of the learning component tab.
      output$keys <- renderText({
        HTML(paste0("We will go over each term related to normal distribution in this part. The terms are as follows:","<br/>",
                    "<li><b>Population Mean - </b> Population mean is the average of group properties in the population. The group 
                     could be made up of a person, an item, or a thing. ","<br/>", 
                    "<li><b>Sample Mean - </b> The sample mean signifies the average of a sample drawn at random from the entire population.",
                    "<li><b>Population Variance - </b> Population variance is a measurement of the dispersion of the population data. The 
                     average of the distances from each data point in a population to the mean squared is known as population variance,
                    and it reveals how data points are dispersed out in the population.",
                    "<li><b>Sample Variance - </b> Sample variance is the squared difference of data points from the mean of the data set. It's 
                     an absolute measure of dispersion and is used to see how far data points deviate from the average.",
                    "<li><b>Population Size - </b> Population size is the number of people living within an arbitrarily designated geographical region. ",
                    "<li><b>Sample Size - </b> The sample size refers to the number of observations used to estimate a population.",
                    "<li><b>Z-Distribution - </b>The z-distribution (standard normal distribution) is a special case of normal distribution that has a 
                     mean of zero and a standard deviation of one.",
                    "<li><b>Z-score - </b>  The z-score of an observattion is the number of standard deviations it falls above or below the mean.",
                    "<li><b>Population Standard Deviation - </b> Population standard deviation is the squre root of the population variance.",
                    
                    withMathJax(HTML(paste0("<li><b>The Central Limit Theorem - </b>The central limit theorem states that when n is relatively large, 
                                            the sampling distribution of sample mean is approximately normally distributed with population mean and 
                                            the standard error (SE) $$\\text{}\\frac{\\sigma}{\\sqrt{n}}$$."))),"<br/>", 
                    withMathJax(HTML(paste0("The standard error is dependent on the population standard deviation. The standard deviation for sample
                    mean is $$\\frac{\\sigma}{\\sqrt{n}} Where \\ \\sigma \\ is \\ the \\ standard \\ deviation \\ of \\ the \\ population.$$","<br/>","
                    This is true regardless of the shape of the population distribution. ")))
                    ))
      })    
      
      # A component of the learning tab.
      output$tex <- renderText({
        HTML(paste0("<b> The Shapiro-Wilk Test : </b> <li>The Shapiro-Wilk Test is the most commonly used one-variable normality test, 
                    it has a higher power to diagnose non-normality. ","<br/>","<li>The test statistic W indicates
                    that the sample is non-normal if it has a small value.","<br/>" , "<li>The p-value is the most commonly 
                    accepted value. If the p-value is greater than 0.05 (level of significance), then we assume a normal distribution, 
                    and the distribution of the data does not vary from the normal distribution. 
                    <li> Likewise,  if the p-value is less than or equal to the commonly used alpha value of 0.05, then we do not assume a 
                    normal distribution.",
                    "<br/>"," <li> The Shapiro-Wilk Test will run on both the simulated and original datasets. 
                     "))
      })
      
      # Show an empirical rule (68-95-99.7 rule).
      output$empirical <- renderPlot({
        
        # Using the seq() function, define the sequence and length.
        x <- seq(-3,3, length = 200)
        y <- dnorm(x)
        # To plot an empirical rule, use the plot() function.
        plot(x,y, type = "l", main = "Empirical Rule", xlab = expression(mu), ylab = "Probability density")
        
        # Define the sequence and length with the seq() function.
        x=seq(-3,3,length=100)
        y=dnorm(x)
        polygon(c(-3,x,3),c(0,y,0), col = "lightblue")
        
        # Specify the sequence and length using the seq() function.
        x=seq(-2,2,length=100)
        y=dnorm(x)
        polygon(c(-2,x,2),c(0,y,0),col = "slategrey")
        
        # Set the sequence and length using the seq() function.
        x=seq(-1,1,length=100)
        y=dnorm(x)
        polygon(c(-1,x,1),c(0,y,0),col = "seagreen")
        
        # To show 68-95-99.7% in the plot, use text functions.
        text(0,0.21,"68%", cex = 1.5,col = "maroon")
        text(0,0.13,"95%",cex = 1.5,col = "blue")
        text(0,0.07,"99.7%",cex = 1.5,col = "purple")
        
        # To show the vertical lines at the designated points in the plot, 
        # use the abline function.
        abline(v = -1, col = "red", lty = 2)
        abline(v = 1,col = "red", lty = 2)
        abline(v = -2, col = "blue",lty = 2)
        abline(v = 2,col = "blue",lty = 2)
        abline(v = -3, col = "purple",lty = 2)
        abline(v = 3,col = "purple",lty = 2)
        
        # To show the line segments at the designated points in the plot, 
        # use the segments function.
        segments(x0 = -1, y0 = 0.19,x1 = 1, y1 = 0.19, col = "maroon")
        segments(x0 = -2, y0 = 0.11,x1 = 2, y1 = 0.11, col = "blue")
        segments(x0 = -3, y0 = 0.05,x1 = 3, y1 = 0.05, col = "pink")
        
        # To show the arrows at the designated points in the plot, 
        # use the arrows function.
        arrows(x0 =-1 ,y0 =0.19, x1 =1,y1 =0.19,col = "red")
        arrows(x0 =1 ,y0 =0.19, x1 =-1,y1 =0.19,col = "red")
        arrows(x0 = -2, y0 = 0.11,x1 = 2, y1 = 0.11, col = "blue")
        arrows(x0 = 2, y0 = 0.11,x1 = -2, y1 = 0.11, col = "blue")
        arrows(x0 = -3, y0 = 0.05,x1 = 3, y1 = 0.05, col = "purple")
        arrows(x0 = 3, y0 = 0.05,x1 = -3, y1 = 0.05, col = "purple")
      })
      
      # A component of learning tab.
      output$out <- renderText({
        HTML(paste0("In statistics, the empirical rule is also called the 68-95-99.7 rule. In statistics, the Empirical rule 
        allows researchers to estimate the proportion of data that fall within a specific range of deviations from the mean. 
        The three-sigma rule is another name for the empirical rule. It consists of the following elements:-
        <li>The first standard deviation from the mean accounts for 68% of the data. It means there's a 68% probability 
        of picking a score between -1 and +1 standard deviations from the mean at random.<li>95% of the data falls within two 
        standard deviations from the mean. It means there's a 95% probability of picking a score between -2 and +2 standard deviations 
        from the mean at random.<li> 99.7% of the data falls within three standard deviations from the mean. It means there's a 
        99.7% probability of picking a score between -3 and +3 standard deviations from the mean at random."))
      })
      
      
      # Reveal the Findings.
      output$find <- renderText({
        HTML(paste0("<h4><b> After carefully simulating the target variable from a real dataset,  
                    the following are the key findings highlighted in this lesson:</b>","<br/>",
                    "<h4><ul><li>The distribution of the simulated dataset appeared normal, and the median 
                    was almost equal to the mean. 
                    <h4><li>When compared to the real dataset mean, there is a minor difference found in the mean values, 
                    and it is obvious due to the random sample drawn from a normal distribution.
                    <h4><li>The simulation outcome, among other considerations, provides new insight into this lesson. 
                    <h4><li>The simulated dataset resulted in a close match to the real dataset outcome. 
                    The summary statistics and charts provide sufficient evidence that the simulated outcomes are 
                    very significant. When it comes to solving multiple problems, It may help users generate meaningful 
                    results from simulation and thus enhance the reliability of the conducted simulations.
                    <h4><li>After comparing the results of the real and simulated datasets, the user can generalize 
                    the outcomes from the simulation in this lesson 
                    to a larger population.
                    <h4><li> The Shapiro-Wilk Test ran on both datasets. The p-values were significantly greater 
                    than the commonly used alpha value (0.05), 
                    thus both datasets passed the Shapiro-Wilk Test.
                    <h4><li>The purpose of simulating the target variable in this lesson was to 
                    see how effective it is to apply the simulation to a target variable and generalize the results. 
                    As a result, it was highly effective. In this lesson, we learned that simulations are extremely 
                    important for research analysis in any field."
                    ))
      })
      
      #  Report the summary.
      output$sum <- renderText({
        HTML(paste0("<h4><b> A comprehensive overview of the lesson is provided in the summary section. 
                    The key points below outline the synopsis. </b>", "<br/>","
                    <h4><ul><li> 
                    For teaching purposes, this lesson covered normal distribution, which is an introductory level statistics topic. 
                    This lesson covered the following aspects: Objective, overview, key terms, 
                    formulas, tests, additional components in the two types of datasets, findings and supplememtary references.
                    <h4><li> The lesson began with a brief overview of the normal distribution and ends with a synopsis.
                    <h4><li> Normal distribution was thoroughly discussed in this lesson. 
                    Some real-world and simulated datasets were used to enhance the user's interest 
                    so that the user may grasp the material and apply it effectively.
                    <h4><li>Multiple plots were used to validate normality and depict the normal distribution.
                    <h4><li>This lesson used both simulated and real-world datasets to build a comparison.
                    <h4><li>The simulated dataset was user-friendly; the user was able to input the mean and standard deviation from the 
                    real dataset into the simulated dataset to check the results and investigate it closely. 
                    <h4><li> The user was able to compare the results of the real dataset with the results of the simulated dataset.
                    <h4><li> The findings addressed the differences observed when comparing the outcomes of real-world 
                     and simulated datasets.
                    <h4><li>This lesson provided the learner with both theoretical and practical knowledge.
                    <h4><li>Last but not least, I hope that the user gained adequate knowledge from this lesson and 
                    was able to implement the concept effectively."))
      })
      
    }
    
   # To adjust the width and height of the shiny app, use the options() function.
    options = list(width = 900, height = 900)
    
    # To call ui and server files, use shinyApp() function from the shiny package.
    shinyApp(ui, server)
    
    
    