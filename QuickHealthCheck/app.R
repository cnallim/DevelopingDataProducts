#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   
     # Application title
   
   fluidRow(
     column(4,titlePanel("Quick Health Check")
            ),
     column(8, 
   tags$img(height = 200,
            width = 400,
src = "AimLogo.gif")
   )
   ),
   
   # Personal data inputs
  
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        h4("PLEASE INPUT FOLLOWING DATA:"),
         sliderInput("waist",
                     "a) Waist measure.
                     To correctly measure your waist, stand and place a tape measure around your middle, just above your hipbones. Measure your waist just after you breathe out.
                     Now enter your waist circumference[cm]:",
                     min = 30,
                     max = 250,
                     value = 30),
         numericInput("weight","b) Your weigth [kg]?:",
                      min = 20,
                      max = 200,
                      value= 0,
                      step = 0.5),
         numericInput("height","c) Your heigth [m]?:",
                      min = 20,
                      max = 220,
                      value= 1.75,
                      step = 0.05),
         radioButtons("sex","d) Your sex?:",
                            c("Male"="M",
                              "Female"="F")
                            ),
         submitButton(text = "Submit your data")
      ),
   
      
      # Show a plot of the generated distribution
      mainPanel(
         h3("RESULTS:"),
         h4("Your Body Mass Index (BMI) is:"),
         verbatimTextOutput("oBMI"),
         h4("Your condition according to BMI:"),
         verbatimTextOutput("ostatus"),
         h4("Your Obessity class (if any):"),
         verbatimTextOutput("oobesclass"),
         h4("Disease risk (relative to normal weight and waist circumference):"),
         verbatimTextOutput("odisrisk"),
         h5("For people who are considered obese or those who are overweight and have two or more risk factors(*), it is recommended that you lose weight. Even a small weight loss (between 5 and 10 percent of your current weight) will help lower your risk of developing diseases associated with obesity. People who are overweight, do not have a high waist measurement, and have fewer than two risk factors may need to prevent further weight gain rather than lose weight."),
         h6("(*)Additional risk factors: High blood pressure (hypertension) / High LDL cholesterol (bad cholesterol) / Low HDL cholesterol (good cholesterol) / High triglycerides / High blood glucose (sugar) / Family history of premature heart disease / Physical inactivity / Cigarette smoking"),
         tags$a(href="http://www.nhlbi.nih.gov/health/educational/lose_wt/risk.htm","Click here for furher references")
      )
   )
))



# Define server logic required to draw a 

server <- shinyServer(function(input, output) {

    output$oweight<-renderPrint({input$weight})
    output$oheight<-renderPrint({input$height})
    
    BMI<- reactive({
      input$weight/(input$height)^2
      })
    
    output$oBMI<-renderPrint({
    BMI()
   })
    
    cond<-c("Underweight", "Normal","Overweight", "Obesity","Obesity", "Extreme obesity")
    obclass<-c("-","-","-","I","II","III")
    disrisk1<-c("-" , "-", "Increased","High","Very High","Extremely High")
    disrisk2<-c("-" , "-" , "High","Very High","Very High","Extremely High")
    conditiondata<-data.frame(cond,obclass,disrisk1,disrisk2)
    conditiondata[]<-lapply(conditiondata,as.character)
    
    
    index<-reactive({
      detect<- function(x) {if(x<18.5){
      1
    }else if(x<25){
      2
    }else if(x<30){
      3
    }else if(x<35){
      4
    }else if (x<40){
      5
    }else {6}
    }
    
    detect(BMI())
    })
    
    circumindex<-reactive({
      choose<-function(x,y) {if((x=="M" && y<=102) || (x=="F" && y<=88)){
        3
      }else {4}
      }
      
      choose(input$sex,input$waist)
    })
    
    output$ostatus<-renderText({
      conditiondata[index(),1]
    })
     output$oobesclass<-renderText({
       conditiondata[index(),2]
     }) 

     output$odisrisk<-renderText({
       conditiondata[index(),circumindex()]
     })
})

# Run the application 
shinyApp(ui = ui, server = server)

