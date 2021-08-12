setwd('E:/DPU/Data V project/Project Tables/combined/')
library(shiny)
library(dplyr)
library(ggplot2)

library(RColorBrewer)

server <- function(input, output, session) {
  
  lifeExpectancy = read.csv("underDevelopedAndDeveloped.csv", header = TRUE,  sep = ",")
  lifeExpectancy$Life.expectancy.at.birth..total..years. <- as.numeric(lifeExpectancy$Life.expectancy.at.birth..total..years.)
  
  #Summarize Data and then Plot
  data <- reactive({
    req(input$years_input)
    target <- c("Angola","Ethiopia","Africa","Bangladesh")

    newDFs<- lifeExpectancy %>% filter(Country.Name %in% target)
    df <- newDFs %>% filter(ï..Time %in% input$years_input) %>% group_by(Country.Name) %>% summarise(Life.expectancy.at.birth..total..years. = sum(Life.expectancy.at.birth..total..years.))
  })

  data1 <- reactive({
    target <- c("India","Germany","China","United States")
    req(input$years_input)
    newDF<- lifeExpectancy %>% filter(Country.Name %in% target)
    df <- newDF %>% filter(ï..Time %in% input$years_input) %>% group_by(Country.Name) %>% summarise(Life.expectancy.at.birth..total..years. = sum(Life.expectancy.at.birth..total..years.))
  })
  #Update SelectInput Dynamically
  observe({
    val <- 2
    # updateSelectInput(session, "years_input", choices = lifeExpectancy$ï..Time)
    updateSliderInput(session, "years_input", value = 2,
                      min = min(lifeExpectancy$ï..Time), max = max(lifeExpectancy$ï..Time), step = 10)
  })
  
  #Plot 
  output$plot <- renderPlot({
    g <- ggplot(data(), aes( y = Life.expectancy.at.birth..total..years., x = Country.Name,fill = Country.Name))
    g + geom_bar(stat = "sum",width = 0.5, position = position_dodge(0.8))+ylab("Life Expectancy at birth in years") + xlab("Developing Countries") +theme(legend.position = "none",panel.background = element_rect(fill = "#F8F8F8", colour = "black",
                                                                                                                 size = 1, linetype = "solid"),
                                                                                 panel.grid.major = element_line(size = 0.5, linetype = 'dotted',
                                                                                                                 colour = "grey"), 
                                                                                 panel.grid.minor = element_line(size = 0.25, linetype = 'dotted',
                                                                                                                 colour = "grey"),
                                                                                 axis.text=element_text(size=16,face="bold"),
                                                                                 axis.title.x = element_text(color="black", size=13,face="bold",margin=margin(20,20,20,20)),
                                                                                 axis.title.y = element_text(color="black", size=13,face="bold",margin=margin(10,10,10,10)))+coord_flip()+
      scale_fill_brewer(palette = "Dark2")+ylim(0, 80)
     
  })
  output$plot2 <- renderPlot({
    g <- ggplot(data1(), aes( y = Life.expectancy.at.birth..total..years., x = Country.Name,fill=Country.Name))
    g + geom_bar(stat = "sum",width = 0.5, position = position_dodge(0.8))+ylab("Life Expectancy at birth in years") + xlab("Developed Countries") + theme(legend.position = "none",panel.background = element_rect(fill = "#F8F8F8", colour = "black",
                                                                                                                 size = 1, linetype = "solid"),
                                                                                 panel.grid.major = element_line(size = 0.5, linetype = 'dotted',
                                                                                                                 colour = "grey"), 
                                                                                 panel.grid.minor = element_line(size = 0.25, linetype = 'dotted',
                                                                                                                 colour = "grey"),
                                                                                 axis.text=element_text(size=16,face="bold"),
                                                                                 axis.title.x = element_text(color="black", size=13,face="bold",margin=margin(20,20,20,20)),
                                                                                 axis.title.y = element_text(color="black", size=13,face="bold",margin=margin(10,10,10,10)))+coord_flip()+scale_fill_manual(values = c("gold", "grey", "seagreen3","navyblue"))+ylim(0, 80)
  })
 
}

ui <- basicPage(
  h1("Total Life expactancy in year between Developing and UnderDeveloped countries"),
   # selectInput(inputId = "years_input",
   #             label = "Choose Year for comparison",
   #             "Years"),
  
  sliderInput("years_input", "Total Years:",
              min=1960, max=2018, value=10,
              step=1
  ),
  plotOutput("plot"),
  plotOutput("plot2")
)

shinyApp(ui = ui, server = server)