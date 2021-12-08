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

install.packages("hrbrthemes")
library(hrbrthemes)
library(lubridate)




org_data<- read_dta("C:/Users/abrah/Desktop/R-Shiny-Experiments/JSTdatasetR5.dta")

mydata<-org_data


vars<-colnames(mydata)
countries<-unique(mydata$country)


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



line_plot(year1=1900,year2=2015)




ui <- fluidPage(
  fluidRow(
    column(4, 
           "Exploring the Macrohistory Database",
           selectInput(
             "variable1", "Variable of interest", vars,
             multiple = TRUE
           ), 
           selectInput(
             "countries1", "Countries", countries,
             multiple = TRUE
           ),
           sliderInput("years", "Years", value = c(2005,2015), min = 1870, max = 2017,step=1,sep = ""),
           
    )
  ),
  fluidRow(
    column(9, plotOutput("lineplot1"))
  ),
  fluidRow(
    column(9, plotOutput("plot2"))
  ),
  fluidRow(
    column(9, plotlyOutput("plot3"))
  ),
  fluidRow(
    column(9, plotOutput("plot4"))
  )
)


server <- function(input, output, session) {
  output$lineplot1 <- renderPlot({

    line_plot(var=input$variable1,year1=input$years[1],countries=input$countries1,year2=input$years[2])
  }, res = 96)
  output$plot2 <- renderPlot({
    
    scatter_plot()
  }, res = 96)
  
  output$plot3 <- renderPlotly({
    
    hist_plot()
      
  })
  
  output$plot4 <- renderImage({
    # A temp file to save the output.
    # This file will be removed later by renderImage
    outfile <- tempfile(fileext='.gif')
    
    # now make the animation
    countries= c("Sweden","Belgium","Norway","Finland")
    
    p=mydata%>%
      arrange(desc(pop)) %>% filter(country %in%countries ) %>%
      ggplot(aes(x=pop, y=cpi, size=pop, fill=country,label=country))+
      geom_point(alpha=0.5, shape=21, color="black")+
      geom_text(hjust = 1, size =3.5, nudge_x = -0.5 , vjust=-0)+
      scale_size(range = c(5,25), name="Population (M)") +
      theme_minimal()+
      scale_fill_viridis(discrete=TRUE, guide=FALSE, option="viridis") +
      theme(legend.position="none")+
      labs(title = 'Inverse correlation between years of schooling and mortality - Year : {frame_time}', x = 'Mean Years of Schooling', y = 'Child Mortality') +
      transition_time(year_org) +
      ease_aes('linear')
    
    
    
    anim_save("outfile.gif", animate(p)) # New
    
    # Return a list containing the filename
    list(src = "outfile.gif",
         contentType = 'image/gif'
         # width = 400,
         # height = 300,
         # alt = "This is alternate text"
    )}, deleteFile = TRUE)

}

shinyApp(ui, server)

input$years[1]
line_plot()


library(processx)
library(gganimate)


anim_plot<-function(){

  mydata%>%
    arrange(desc(pop)) %>%
    ggplot(aes(x=pop, y=cpi, size=pop, fill=country,label=country))+
    geom_point(alpha=0.5, shape=21, color="black")+
    geom_text(hjust = 1, size =3.5, nudge_x = -0.5 , vjust=-0)+
    scale_size(range = c(5,25), name="Population (M)") +
    theme_minimal()+
    scale_fill_viridis(discrete=TRUE, guide=FALSE, option="viridis") +
    theme(legend.position="none")+
    labs(title = 'Inverse correlation between years of schooling and mortality - Year : {frame_time}', x = 'Mean Years of Schooling', y = 'Child Mortality') +
    transition_time(year_org) +
    ease_aes('linear')
  

  #animate(plot , height = 600, width =600)
 
  
}

anim_plot()


scatter_plot<-function(){
  mydata%>%filter(country=="Japan" & year_org!=0 )%>% ggplot(aes(x = year, y = exports))+ 
    geom_point(aes(shape = country, color = country))+ 
    geom_line(aes(color = country)) #can have countries,choose variables and years.
  
 
  
}
scatter_plot()
library(plotly)

hist_plot<-function(){
mydata %>% filter(year_org==2005) %>% plot_ly(x = ~pop, y = ~country,color=~country) %>%
  add_bars() %>% layout(showlegend = FALSE)
}

hist_plot()
 




