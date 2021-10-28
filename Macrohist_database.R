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

org_data<- read_dta("C:/Users/abrah/Desktop/R-Shiny-Experiments/JSTdatasetR5.dta")

mydata<-org_data
mydata %>% sapply(function(x) sum(is.na(x)))
vars<-colnames(mydata)


for (var in vars){
  #mydata %<>% group_by(country) %>% filter(any(!is.na(!!var)))
  mydata %<>% arrange(country,year) %>% dplyr::group_by(country) %>% tidyr::fill((!!var),.direction="down")
}

library(lubridate)

mydata<- mydata%>%mutate(gdp_mn=(gdp/xrusd)/1000000)

mydata %>% sapply(function(x) sum(is.na(x)))
library(zoo)
mydata$year  <- as.Date(as.character(mydata$year), format = "%Y") 


g<-mydata %>% filter(country=="Spain")





library(scales)

data<-mydata%>% filter(!(is.na(!!var)) & year>2000) 


windowsFonts("Arial" = windowsFont("Arial"))

data<-data %>% mutate(label = if_else(year == max(year), as.character(country), NA_character_)) 
data$label


GO1=ggplot(data, aes(x=year, y=gdp_mn,group=country)) +
  geom_line( aes(color=country),size=0.9, alpha=0.9) +
  geom_point()+
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "dashed"),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.title.position = "plot",
        plot.title = element_text(face="bold"),
        legend.position = "none") +
       scale_x_date(labels = scales::date_format("%Y"),limits=as.Date(c("1950-10-28","2021-10-28")))
  
  

options(ggrepel.max.overlaps = Inf)
library(ggrepel)
  GO1+  geom_text_repel(aes(label = gsub("^.*$", " ", label)), # This will force the correct position of the link's right end.
                      segment.curvature = -0.1,
                      segment.square = TRUE,
                      segment.color = 'grey',
                      box.padding = 0.1,
                      point.padding = 0.6,
                      nudge_x = 0.15,
                      nudge_y = 1,
                      force = 0.5,
                      hjust = 0,
                      direction="y",
                      na.rm = TRUE)+
              geom_text_repel(data = . %>% filter(!is.na(label)),
                  aes(label = paste0("  ", label)),
                  segment.alpha = 0, ## This will 'hide' the link
                  segment.curvature = -0.1,
                  segment.square = TRUE,
                  # segment.color = 'grey',
                  box.padding = 0.1,
                  point.padding = 0.6,
                  nudge_x = 0.15,
                  nudge_y = 1,
                  force = 0.5,
                  hjust = 0,
                  direction="y",
                  na.rm = TRUE)
    



ui <- fluidPage(
  fluidRow(
    column(4, 
           "Exploring the Macrohistory Database",
           selectInput(
             "variable1", "Variable of interest", vars,
             multiple = TRUE
           ),
           sliderInput("rng", "Years", value = c(10, 20), min = 0, max = 100),
           
    )
  )
)



server <- function(input, output, session) {

}
shinyApp(ui, server)









