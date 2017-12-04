########## Food Bar Recommender Service ##########

###
### Libraries
###
library(shiny)
library(shinyjs)
library(shinythemes)
library(lubridate)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(recommenderlab)

###
### Data Processing
###

## If the new food bars file doesn't exist, we read the raw data to compute it
## Otherwise simply read it in
if (!file.exists("data/new_food_bars.csv")) {
    
    # Read and remove duplicates
    bars <- read_csv("data/food_bars.csv")
    bars <- bars[!duplicated(bars[,c("Food Bar", "Taster")]),]
    
    # Fix the date, which is in days
    bars$Date <- ymd("1899-12-30") + days(bars$Date)
    
    # Set the column names
    names(bars) <- c("Bar", "Taster", "Texture", "Natural", "Taste", "Appearance", "Branding", "AfterTaste", "Smell", "Overall", "Comments", "Date")
    
    # Write out the data
    write.csv(bars, file = "data/new_food_bars.csv", row.names = FALSE)
} else {
    bars <- read_csv("data/new_food_bars.csv")
}

## Set images resource path
addResourcePath("images", "images")

###
### User Interface
###
ui <- fluidPage(title = "Food Bars Recommender System", theme = shinytheme("cerulean"),
    
    titlePanel("Food Bars Recommender System"),
    
    useShinyjs(),
    
    sidebarLayout(
        
        sidebarPanel(
            a(href = "https://oaiti.org", target = "_blank", img(src = "images/oaiti_transparent.png", width = "135")),
            
            h4("About"),
            HTML("As a fun little experiment, we've sampled numerous food bars and collected data on our team's preferences on a number of different attributes (taste, texture smell, etc.) Using this data, we've built a recommender system that provides predicted ratings for the bars that each person hasn't yet rated. To begin, choose an attribute and click the buttons corresponding to the action you wish to take.<br><br>For more information, check out <a href='https://oaiti.org/2017/12/04/a-food-bar-recommender-service/' target='_blank'>our blog</a>."),
            hr(),
            h4("Configuration"),
            selectInput("attribute", "Attribute", choices = names(bars)[3:10], selected = "Overall"),
            hidden(checkboxInput("ready", "Ready")),

            conditionalPanel(condition = "input.tabs1 == 'Data'",
                             selectInput("user", "User", choices = unique(bars$Taster), selected = "Hieu"),
                             selectInput("bar", "Bar", choices = unique(bars$Bar), selected = "Bounce"),
                             sliderInput("rating", "Rating", value = 5, min = 0, max = 10, step = .5),
                             fluidRow(column(6, actionButton("add_data", "Add Data")), column(6, actionButton("remove_data", "Remove Data")))
            ),
            conditionalPanel(condition = "input.tabs1 == 'Recommendations'",
                             selectInput("method", "Recommender Method", choices = c("IBCF", "ALS", "POPULAR")),
                             selectInput("preduser", "User", choices = unique(bars$Taster), selected = "Hieu"),
                             actionButton("predinterest", "Predict Interests")
            )
        ),
        
        mainPanel(
            tabsetPanel(id = "tabs1",
                tabPanel("Data",
                         h3("Recommendation Data"),
                         helpText("The recommendation data below gives the standard wide format view of the data. The individual rows are the users, and the columns are the ratings on the attribute selected on the left. Where ratings are missing, the recommendation system will attempt to infer the rating of the user (See the recommendations tab)."),
                         dataTableOutput("rec_data"),
                         hr(),
                         h3("Raw Data"),
                         helpText("Here is the original raw data prior to transformation. In this case, the different attributes are along the columns and the bars and tasters are along the rows. We transform this data to get the above structure (with columns as bars and tasters as rows) in order to perform the recommendation."),
                         dataTableOutput("data")
                ),
                
                tabPanel("Recommendations",
                         conditionalPanel(condition = "!input.predinterest",
                                          h4("Click Predict Interests to see the predictions.")
                                          
                         ),
                         conditionalPanel(condition = "input.predinterest",
                                          h3("Predicted Ratings"),
                                          helpText("The predicted ratings for the top 5 bars for the selected user are given in the plot below."),
                                          hr(),
                                          plotOutput("predratplot"),
                                          hr(),
                                          h4("Table"),
                                          helpText("The same data from above is given in table format below."),
                                          dataTableOutput("predrat")
                         )
                )
            )
        )
        
    )
)

###
### Server Definition
###
server <- function(input, output, session) {
    
    # Store the user we are interested in predicting ratings for
    values <- reactiveValues(preduser = "Hieu")
    
    # Keep the data up to date with the addData endpoint is called
    bars <- reactiveFileReader(500, session, "data/new_food_bars.csv", read_csv)
    
    # Routine to remove data from the dataset
    observeEvent(input$remove_data, {
        barsdat <- as.data.frame(bars())
        
        # Remove a row
        if (any(barsdat$Taster == input$user & barsdat$Bar == input$bar)) {
            barsdat[,input$attribute][barsdat$Taster == input$user & barsdat$Bar == input$bar] <- NA
        }
        
        # Write out the new data
        write.csv(barsdat, file = "data/new_food_bars.csv", row.names = FALSE)
    })
    
    # Routine to add data to the dataset
    observeEvent(input$add_data, {
        barsdat <- as.data.frame(bars())
        
        # Add new row
        if (!any(barsdat$Taster == input$user & barsdat$Bar == input$bar)) {
            newdat <- data.frame(input$bar, input$user, NA, NA, NA, NA, NA, NA, NA, NA, NA, today())
            names(newdat) <- names(barsdat)
            newdat[,names(newdat) == input$attribute] <- input$rating
            
            barsdat <- rbind(barsdat, newdat)
        } else {
            barsdat[,input$attribute][barsdat$Taster == input$user & barsdat$Bar == input$bar] <- input$rating
        }

        # Write out the new data
        write.csv(barsdat, file = "data/new_food_bars.csv", row.names = FALSE)
    })
    
    # Some attributes are scaled to allow negative values
    observe({
        if (input$attribute %in% c("Smell", "AfterTaste")) {
            updateSliderInput(session, "rating", min = -10)
        } else {
            updateSliderInput(session, "rating", min = 0)
        }
    })
    
    # Routine to turn the data into a suitable format for a recommender system
    recdata <- reactive({
        my_var <- quo(input$attribute)
        
        bars() %>%
            select(Bar, Taster, !!my_var) %>%
            spread_(key = "Bar", value = input$attribute)
    })
    
    # Routine to get the recommendations using recommenderlab
    myrecs <- reactive({
        cleaned_occurrence <- as.matrix(select(recdata(), -Taster))
        ratingmat <- as(cleaned_occurrence, "realRatingMatrix")
        
        recommender_model_ibcf <- Recommender(ratingmat, method = input$method)
        
        recom_test <- predict(recommender_model_ibcf, ratingmat, type = "ratings")
        full_result <- cbind(Taster = recdata()$Taster, as.data.frame(as(recom_test, "matrix"))) %>%
            gather(key = Bar, value = PredictedRating, 2:ncol(.)) %>%
            filter(!is.na(PredictedRating), Taster != "Pete") %>%
            arrange(Taster, desc(PredictedRating)) %>%
            group_by(Taster) %>%
            slice(1:min(length(PredictedRating), 5)) %>%
            mutate(Rank = 1:min(length(PredictedRating), 5))
        
        final_result <- full_result %>%
            filter(Taster == values$preduser)

        write.csv(full_result, file = "data/food_recommendations.csv", row.names = FALSE)
        
        return(final_result)
    })
    
    # Routine to product a bar chart of the top five food bars
    output$predratplot <- renderPlot({
        req(input$predinterest)
        
        recdat <- myrecs()
        recdat$Bar <- factor(recdat$Bar, levels = rev(recdat$Bar))
        
        ggplot(recdat, aes(x = Bar, y = PredictedRating)) +
            theme_bw() +
            geom_bar(stat = "identity") +
            ggtitle(paste("Top Five Bar Recommendations based on", input$attribute, "ratings for", values$preduser)) +
            xlab("Bar") +
            ylab("Predicted Rating") +
            theme(axis.text.x = element_text(size = 14),
                  axis.text.y = element_text(size = 14)) +
            ylim(c(0, 10)) +
            coord_flip()
    })
    
    # Store the user which we are predicting
    observeEvent(input$predinterest, {
        values$preduser <- input$preduser
    })
    
    # Produces a table of the recommendations
    output$predrat <- renderDataTable({
        req(input$predinterest)
        
        return(myrecs())
    })
    
    # Table of the recommender data
    output$rec_data <- renderDataTable({
        return(recdata())
    })
    
    # Table of the raw data
    output$data <- renderDataTable({
        return(bars())
    })
        
}

shinyApp(ui = ui, server = server)
