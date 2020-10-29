#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)


header <- dashboardHeader(title ="Versuch 1")

sidebar <- dashboardSidebar(disable = T)
    


body <- dashboardBody(
    fluidRow(
    box(plotOutput("distPlot"),width = 9,title ="Die Bundesländer im Vergleich"),
    box(width = 3,selectInput("x", "X-Axis", choices = names(covid_state_data)),
        selectInput("y","Y-Axis", choices = names(covid_state_data)))),
    fluidRow(
    box(plotOutput("one"),width = 9,title = "Entwicklung im ausgewählten Bundesland"),
    box(width =3,selectInput("z",label = "Bundesland", choices = unique(covid_state_data$state), selected = "Bayern")
        , selectInput("bot","Y-Axis", choices = names(covid_state_data), selected = covid_state_data$totalcases))),
    fluidRow(
        box(plotOutput("two")), title = "Aktuelle Deutschlandkarte",
        box(selectInput("s",label="Variabel", choices=names(covid_state_data), selected ="totalcases"))))

ui<-fluidPage(titlePanel(
    h1("Corona Dashboard", align = "center")), theme = shinytheme("cerulean"),
    tags$footer("von Philipp Conrad", align = "center"),
    fluidRow(
        column(4, box(width =12,plotOutput("distPlot"),title ="Die Bundesländer im Vergleich",solidHeader = T)),
        column(4, box(width =12,plotOutput("one"),title = "Entwicklung im ausgewählten Bundesland")),
        column(4, box(width =12,plotOutput("two"), title = "Aktuelle Deutschlandkarte"))),
    hr(),
    fluidRow(
        column(4, selectInput("x", "X-Axis", choices = names(covid_state_data),selected = "date"),
                      selectInput("y","Y-Axis", choices = names(covid_state_data), selected = "cases_vs_population")),
        column(4, selectInput("z",label = "Bundesland", choices = unique(covid_state_data$state), selected = "Bayern")
                             , selectInput("bot","Y-Axis", choices = names(covid_state_data), selected = "totalcases")),
        column(4,selectInput("s",label="Variabel", choices=names(covid_state_data), selected ="totalcases"),
               selectizeInput("t", label = "Datum", choices = unique(covid_state_data$date), selected = "2020-10-24", multiple = FALSE,
                              options = NULL))
    ),
    hr(),
    fluidRow(
        DT::dataTableOutput("mytable")
    ),
    tags$footer("von Philipp Conrad", align = "right")
    )


ui<-fluidPage(
    fluidRow(
        column(4,
               box(plotOutput("distPlot")),
               box(selectInput("x", "X-Axis", choices = names(covid_state_data)),
                   selectInput("y","Y-Axis", choices = names(covid_state_data)))
        )
    )
)



ui<-dashboardPage(header, sidebar, body)


server <- function(input, output) {
    
    output$distPlot <- renderPlot({
        ggplot(covid_state_data,aes_string(x = input$x,y=input$y))+
            geom_point(aes(color =state))+
            labs(color="")+
            theme_minimal()
        
    })
    output$one <- renderPlot({
        covid_state_data %>%
            filter(state == input$z) %>% 
        ggplot(covid_state_data,mapping=aes(x=date))+
            geom_point(aes_string(y=input$bot))+
            labs(color="")+
            theme_minimal()
        
    })
    output$two <- renderPlot({
        shape_state %>% 
            right_join(covid_state_data, by ="state") %>% 
            filter(date == input$t) %>% 
            ggplot(aes_string(fill = input$s)) +
            geom_sf()+
            scale_fill_viridis()+
            theme_map()+
            theme(legend.background = element_blank(), legend.position = "right")
    })
    output$mytable = DT::renderDataTable({
        covid_state_data})
}


# Run the application 
shinyApp(ui = ui, server = server)



#
server <- function(input, output) {
    
    output$distPlot <- renderPlot({
        ggplot(covid_state_data,aes_string(x = input$x,y=input$y))+
            geom_point(aes(color =state))+
            labs(color="")+
            theme_minimal()
        
        
    })
    output$one <- renderPlot({
        covid_state_data %>%
            filter(state == input$z) %>% 
            ggplot(covid_state_data,mapping=aes(x=date))+
            geom_point(aes_string(y=input$bot))+
            labs(color="")+
            scale_fill_viridis()+
            theme_minimal()
    })}



