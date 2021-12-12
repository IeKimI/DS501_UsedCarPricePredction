#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# setwd("/Users/eri/Desktop/Academic stuff/DS501/Case Study 3")
library(ggplot2)
library(caret)
library(visreg)
library('fastDummies')

car = read.csv('./X_train.csv')
car_y = read.csv('./y_train.csv')

test_x = read.csv('./X_test.csv')
test_y = read.csv('./y_test.csv')

train <- merge(car, car_y,by="carID")

test <- merge(test_x, test_y,by="carID")

total <- rbind(train, test)

brands = unique(total[c("brand")])

models = unique(total[c("model")])

total$brand.b_ <- factor(total$brand)
total$model.m_ <- factor(total$model)
total$transmission.t_ <- factor(total$transmission)
total$fuelType.f_ <- factor(total$fuelType)


model <- lm(log(price) ~ brand.b_ + year * mileage + engineSize * transmission.t_ + fuelType.f_, data = total)

library(shiny)
library(shinydashboard)
# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  dashboardHeader(title = "Used Car Price Prediction", 
                  titleWidth = 290),
  dashboardSidebar(width = 290,
                   sidebarMenu(menuItem("Algorithm", tabName = "algo", icon = icon('chart-line', lib="font-awesome")),
                               menuItem("Data Plots", tabName = "plots", icon = icon('poll')),
                               menuItem("Prediction", tabName = "data_pred", icon = icon('search-dollar', lib="font-awesome")))),
  dashboardBody(
    tabItems(
      tabItem('algo',
              box(status = 'primary', title = 'Mathematical/statistical details of the algorithm',
                  htmlOutput("descr")              
                  ),
              box(plotOutput("resid"))

      ),
      tabItem('plots',
        box(status = 'primary', title = 'Relationships between Price and Predictors',
            selectInput("aPlot", "Select the category to plot ", choices = c('Mileage', 'Year', 'Engine size', 'Brand', 'Transmission', 'Fuel Type')),
            htmlOutput("plot_descr")
        ),
        box(plotOutput("outputPlot"))
      ),
      tabItem('data_pred',
              box(status = 'primary', title = 'Filter for prediction value',
                  selectInput("brand", "Select the brand of the car", choices = unique(total$brand.b_)),
                  sliderInput("engineSize",
                              "Size of the engine:",
                              min = min(total$engineSize),
                              max = max(total$engineSize),
                              value = round(mean(total$engineSize), 1)),
                  numericInput("year", "Year of the car:", value = round(mean(total$year), 0), min = min(total$year), max=max(total$year), step=1),
                  numericInput("mileage", "Mileage of the car:", value=round(mean(total$mileage), 0), step=100, max=max(total$mileage), min=min(total$mileage)),
                  selectInput("transmission", "Select the transmission of the car", choices = c("Automatic", "Semi-Auto", "Manual")),
                  selectInput("fuelType", "Select the fuel type of the car", choices = unique(total$fuelType.f_) ),
                  box(htmlOutput("model_desc")),              
                  
              ),
              box(htmlOutput('prediction')),
              
              box(verbatimTextOutput('model_summary'))
              
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$outputPlot <- renderPlot({
        switch (input$aPlot,
          'Mileage' = plot(price ~ mileage, data = total, col = "red4", 
                           pch = 20, cex = 1.5, main = "Price vs Mileage"),
          'Year' = plot(price ~ year, data = total, col = "tan2", 
                        pch = 20, cex = 1.5, main = "Price vs Year"),
          'Engine size' = plot(price ~ engineSize, data = total, col = "seagreen", 
                               pch = 20, cex = 1.5, main = "Price vs Engine size"),
          'Brand' = plot(price ~ brand.b_, data = total, col = "steelblue4",
                         pch = 20, cex = 1.5, main = "Price vs Brand"),
          'Transmission' = plot(price ~ transmission.t_, data = total, col = "slateblue4",
                                pch = 20, cex = 1.5, main = "Price vs Transmission"),
          'Fuel Type' = plot(price ~ fuelType.f_, data = total, col = "purple4",
                                pch = 20, cex = 1.5, main = "Price vs Fuel Type")
        )
    })
    
    output$descr <- renderText({
      HTML(paste("The dataset I chose from Kaggle is a list of used cars. It has columns carID, brand, model, year, transmission, mileage, fuel type, tax, mpg, engine size, and price.",
                 "I am planning on getting a car that I was motivated to create a shiny app that can predict the price of a car given a few parameters.",
            "The goal of this case study is to predict the price of a used car with inputted variables by a user. 
            Regression focuses on the relationship between an outcome and its input variables.
            For this case study, linear regression was used to estimate a continuous value as a linear function of other variables. In order to avoid the price being negative, the natural log of price was used as a response variable. 
            The plots of relationships between price and the predictors can be found in the tab called 'Data Plots' on the left.",
            
            "Before modeling, there were some categorical predictors in the dataset that I used the factor() function to encode a category as a factor for predictors brand, transmission, and fuel type.", 
            "Then, lm() was used to predict the price and the result can be found in the 'Prediction' tab. To see how accurately the predictions were made, residuals of the model was plotted as shown on the right. Although there are some outliers, most of the residuals seem to be lying near 0.<hr/>", sep = "<br/><br/>"))
    })
    
    output$plot_descr <- renderText({
      switch (input$aPlot,
              'Mileage' = HTML("<hr/> As mileage increases, the price decreases exponentially."),
              'Year' = HTML("<hr/> As year increases, meaning it is newer, the price increases."),
              'Engine size' = HTML("<hr/> The general trend is that the price increases as the engine size gets bigger. However, it is hard to determine a true relationship."),
              'Brand' = HTML("<hr/> Audi has the highest price out of all brands available, and then bmw. Hyundai seems like the brand that has the lowest price."),
              'Transmission' = HTML("<hr/> Semi-auto cars are more expensive than auto or manual cars."),
              'Fuel Type' = HTML("<hr/> The mean prices of each fuel type are similar. However, the plot shows that electric and hybrid cars have a very small variance. ")
      )
    })
    
    output$model_desc <- renderText({
      HTML(paste("As shown on the right, represented with '***', most of the predictors used are statistically significant.", sep = "<br/><br/>"))
    })
    
    data <- reactive({
      data.frame(brand.b_ = input$brand, engineSize=input$engineSize,
                 year=input$year, mileage=input$mileage,
                 transmission.t_=input$transmission,
                 fuelType.f_=input$fuelType)
    })
    
    pred <- reactive({
      round(exp(predict(model, data())), 2)
    })
    
    output$prediction <- renderText({
      HTML(paste("<h2><strong>Predicted Car Price:", pred(), "USD</strong></h2>", sep=" "))
    })
    
    summary_table <- reactive({
      capture.output(summ)
    })
    
    output$model_summary <- renderPrint({
      summary_table()
    })
    
    output$resid <- renderPlot({
      plot(resid(model))
      abline(h=0, col="red")
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
