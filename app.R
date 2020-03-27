library(shiny)
library(tidyverse)
library(DT)
library(data.table)

# testdata <- tibble::tribble(
#   ~config,       ~construct, ~var,
#   "alpha,beta", "This is line 1",   12,
#   "beta,gamma,alpha", "This is line 2",   15,
#   "delta,alpha,tetra", "This is line 3",   21,
#   "quad,core ,delta", "This is line 4",   12,
#   "alpha,gamma", "This is line 5",   12,
#   "beta,core", "This is line 5",   11,
#   "delta,quad,tetra", "This is line 5",   21,
#   "quad,tetra", "This is line 5",   12
# )
testdata <- read_csv("Yummly_Raw_Data.csv")
testdata$config_split <- strsplit(testdata$ingredients,split = ",")
testdata <- rowwise(testdata)
testdata$website <- paste0("<a href='",testdata$url,"'>",testdata$url,"</a>")

config <- unique(unlist(strsplit(as.character(testdata$ingredients), ",")))
var <- unique(unlist(strsplit(as.character(testdata$cuisine), ",")))

ui <- fluidPage(
  title = 'Selectize examples',
  sidebarLayout(
    sidebarPanel(
      
      selectizeInput(
        'e2', '1.Select Ingredients', choices = config, multiple = TRUE
      )
      ,
      selectizeInput(
        'e3', '2. Select Cuisine', choices = var, multiple = FALSE
      )
    ),
    mainPanel(width = 12,
      # helpText('Output of the examples in the left:'),
      # verbatimTextOutput('ex_out'),
      DT::dataTableOutput("mytable")

    )
  )
)

server <- function(input, output) {
  # output$ex_out <- renderPrint({
  #   a <- str(sapply('e2', function(id) {
  #     input[[id]]
  #   }, simplify = FALSE))
  #   a
  #   print(a)
  # })
  

  output$mytable <- renderDataTable({
    
    
    mycars <- head(testdata)
    # browser()
    # if(isTruthy(input$e2))
    # {result <- map_dfr( input$e2,
    #                     ~filter(testdata,
    #                             any(config_split==.)))
    #print(result)
    
    if(isTruthy(input$e2))
    {result <- filter(testdata,
                      all(input$e2 %in% config_split)
    )
    r2<- result %>% ungroup %>% select(Recipe = recipeName,
                                       Ingredient = ingredients,
                                       cuisine = cuisine,
                                       `Preparation Time` = PrepTime,
                                       Website = website)
    }else {
      r2<- testdata %>% ungroup %>% select(Recipe = recipeName,
                                           Ingredient = ingredients,
                                           cuisine = cuisine,
                                           `Preparation Time` = PrepTime,
                                           Website = website)
    }
    print(r2)
    if(isTruthy(input$e3)){
      return(filter(r2,cuisine %in% input$e3))
    } else
      return(r2)
  },escape = FALSE)
  
  # observeEvent(input$mytable_rows_selected,
  #              {
  #                showModal(modalDialog(
  #                  title = "You will be cooking",
  #                  mycars[input$mytable_rows_selected,]
  #                ))
  #              })
}

shinyApp(ui = ui, server = server)