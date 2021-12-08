install.packages(c(
  "gapminder", "ggforce", "gh", "globals", "openintro", "profvis", 
  "RSQLite", "shiny", "shinycssloaders", "shinyFeedback", 
  "shinythemes", "testthat", "thematic", "tidyverse", "vroom", 
  "waiter", "xml2", "zeallot" 
))

install.packages("shiny")
install.packages("ggplot2")
install.packages("viridisLite")
install.packages("viridis") 
library(viridis)
library(shiny)
library(ggplot2)
library(haven)
library(dplyr)
library(hrbrthemes)
library(lubridate)
library(processx)
library(gganimate)
library(plotly)


org_data<- read_dta("C:/Users/abrah/Desktop/R-Shiny-Experiments/JSTdatasetR5.dta")
mydata<-org_data
vars<-colnames(mydata)
countries<-unique(mydata$country)




scatter_plot<-function(Country="Belgium",X="rgdpmad",Y="pop"){
  mydata%>%filter(country==Country & year_org!=0 )%>% ggplot(aes(x = (get(X)), y = (get(Y))))+ 
    geom_point(aes(shape = country, color = country))+ 
    geom_line(aes(color = country)) #can have countries,choose variables and years.
  
  
  
}




hist_plot<-function(Year=2005,var="pop"){
  mydata %>% filter(year_org==Year) %>% plot_ly(x = ~(get(var)), y = ~country,color=~country) %>%
    add_bars() %>% layout(showlegend = FALSE)
}










for (var in vars){
  #mydata %<>% group_by(country) %>% filter(any(!is.na(!!var)))
  mydata %<>% arrange(country,year) %>% dplyr::group_by(country) %>% tidyr::fill((!!var),.direction="down")
}



#install.packages("papeR")
#library("papeR")
#l<-labels(org_data)
#l<-c(l)
#mydata<-set_labels(mydata,labels=l)


mydata$year_org=mydata$year
mydata$year  <- as.Date(as.character(mydata$year), format = "%Y") 
mydata <- mydata%>% mutate(exp_ss=expenditure/(xrusd*1000000))
mydata $exp_ss <- ifelse(mydata$country %in% c('USA'),mydata$exp_ss*1000, mydata$exp_ss)


year1=2000
year2=2010
data<-mydata%>% filter(country %in%countries ) %>% mutate(label = if_else(year == max(year), as.character(country), NA_character_)) %>%filter(year_org>=year1 &year_org<=year2)
countries= c("Sweden","Belgium","Norway","Finland")
var="pop"

line_plot<-function(var="rgdpmad",year1=1950,year2=2020,countries= c("Sweden","Belgium","Norway","Finland")){
  
  windowsFonts("Arial" = windowsFont("Arial"))
  
  
  data<-mydata%>% filter(country %in%countries ) %>%filter(year_org>=year1 &year_org<=year2)%>% mutate(label = if_else(year == max(year), as.character(country), NA_character_)) 

  
  
  
  GO1=ggplot(data, aes(x=year, y=(get(var)),group=country)) +
    geom_line( aes(color=country),size=0.9, alpha=0.9) +
    geom_point(aes(color=country),size=1.2)+
    theme_minimal()+
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(linetype = "dashed"),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.title.position = "plot",
          plot.title = element_text(face="bold"),
          legend.position = "none") +
    scale_x_date(labels = scales::date_format("%Y"),limits=as.Date(c(min(data$year),NA)))
  
  
  
  options(ggrepel.max.overlaps = Inf)
  library(ggrepel)
  GO1 +  
    geom_text_repel(data = . %>% filter(!is.na(label)),
                    aes(label = paste0("  ", label),
                        segment.curvature = -0.1,
                        min.segment.length = unit(0, 'lines'),
                        force             = 0.5,
                        
                        
                   ))

}





ui <- fluidPage(
  fluidRow(
    column(4, 
           "",
           selectInput(
             "variable1", "Variable of interest", vars,
             multiple = TRUE
           ), 
           selectInput(
             "countries1", "Countries", countries,
             multiple = TRUE
           ),
           sliderInput("years", "Years", value = c(2005,2015), min = 1870, max = 2017,step=1,sep = ""),
           
    ),
    column(8, plotOutput("lineplot1")
             )
  ),  fluidRow(
    column(4, 
           "",
           selectInput(
             "variable2", "X variable", vars,
             multiple = TRUE
           ), 
           selectInput(
             "variable3", "Y variable", vars,
             multiple = TRUE
           ), 
           selectInput(
             "country", "Countries", countries,
             multiple = FALSE
           ),
    ),
      column(8, plotOutput("plot2")
    )
  )
  , fluidRow(
    column(4, 
           "",
           selectInput(
             "variable4", "Variable of interest", vars,
             multiple = TRUE
           ),
           selectInput(
             inputId =  "year_choice", 
             label = "Select time period:", 
             choices = 2005:2015
           )
           
    ),column(8, plotlyOutput("plot3")
    )
  )
  
 )



server <- function(input, output, session) {
  output$lineplot1 <- renderPlot({

    line_plot(var=input$variable1,year1=input$years[1],countries=input$countries1,year2=input$years[2])
  }, res = 96)
  output$plot2 <- renderPlot({
    
    scatter_plot(Country=input$country,X=input$variable2,Y=input$variable3)
  })
  
  output$plot3 <- renderPlotly({
    
    hist_plot(Year=input$year_choice,var=input$variable4)
      
  })

}

shinyApp(ui, server)









