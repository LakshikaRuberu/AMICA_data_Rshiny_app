#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
  # Install
library(viridis)

#rapi_data=read.csv("rapi.csv")
Estimate_RAPI=c(0.196,0.214,2.557,-0.138, -0.095, -0.402, 0.589)
Estimate_CPQ=c(2.406,-0.068,-0.021,0.354,0.701,0.051,-0.065)
Estimate_HONC=c(-0.051, 0.914, -0.074, 1.178, -0.174, -3.212 ,0.375, -0.014 ,1.748, -0.925)

Estimate_CPQ=as.numeric(Estimate_CPQ)
                    
                    
                    

# Define UI for application that draws a histogram
ui <- fluidPage(
  
    # Application title
    titlePanel("Joint Risk Prediction for Hazardous Use of Alcohol, Cannabis, and Tobacco among Adolescents"),
    
  
    
    setSliderColor(c(plasma(4),viridis(2)), c(1, 2, 3,4,5,6)),
   
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("Age",
                        "Age:",
                        min = 12,
                        max = 18,
                        value =15 ),
        
        sliderInput("Maternal_education",
                    "Maternal education:",
                    min = 0,
                    max = 8,
                    value =3 ),
        
        sliderInput("Parental_Attachment",
                    "Parental Attachment:",
                    min = 0,
                    max = 24,
                    value =12 ),
   
        sliderInput("Age_of_first_use_of_tobacco",
                    "Age of first use of tobacco:",
                    min = 5,
                    max = 18,
                    value =10 ),
        
       
       
        
        sliderInput("Age_of_first_use_of_other_substances",
                    "Age of first use of other substances:",
                    min = 5,
                    max = 18,
                    value =5 ),
     
        sliderInput("Age_of_first_use_of_cannabis",
                    "Age of first use of cannabis:",
                    min = 5,
                    max = 18,
                    value =10 ),
        
        selectInput("Early_life_stress",
                    "Early life stress:",
                    c("Yes" = 1,
                      "No" = 0)),
        
        selectInput("Lifetime_use_of_other_substances", 
                    "Lifetime use of other substances:",
                    c("Yes" = 1,
                      "No" = 0)),

    # 
    # 
    selectInput("Family_use_of_cigarette", 
                "Family use of cigarette:",
                c("Yes" = 1,
                  "No" = 0)),

    selectInput("Family_history_of_hazardous_alcohol_use", 
                "Family history of hazardous alcohol use:",
                c("Yes" = 1,
                  "No" = 0)),
    
    selectInput("Family_history_of_hazardous_cannabis_use", 
                "Family history of hazardous cannabis use:",
                c("Yes" = 1,
                  "No" = 0)),
        ),    # Show a plot of the generated distribution
    
    
    
   
    
    
        mainPanel(
          textOutput("RAPI"),
          
          tags$head(tags$style("#RAPI{color: red;
                                 font-size: 20px;
                                 font-style: italic;
                                 }"
          )
          ),
          
          textOutput("CPQ"),
          tags$head(tags$style("#CPQ{color: blue;
                                 font-size: 20px;
                                 font-style: italic;
                                 }"
          )
          ),
          
          textOutput("HONC"),
          
          
          tags$head(tags$style("#HONC{color:black;
                                 font-size: 20px;
                                 font-style: italic;
                                 }"
          )
          ),
          
           br(),
           hr(),
           p("Welcome to our application where you could qunatitatively assess the risk of problematic use of alcohol, cannabis and tobacco for users of these three substances based on personlized risk factors.
             To use this app, manipulate the widgets on the side to adjust the risk predictors as applicable to you. Your final scores are based on a model which could jointly predicting quantitative scores on three measures of hazardous substance use namely Rutgers Alcohol Problems Index (RAPI), Adolescent Cannabis Problem Questionnaire (CPQ), and Hooked on Nicotine Checklist (HONC). 
             Please note that changing a given risk factors will change at least one quantitative score.Higher scores indicate a higher risk of problematic use"),
           
           tags$a(href="https://www.sciencedirect.com/science/article/pii/S221133552100365X?via%3Dihub", 
                  "Details of our model can be found here"),
           
           tags$ul(
             tags$li(tags$b("RAPI"), " - The possible score range is 0 to 92"),
             tags$li(tags$b("CPQ"), " - The possible score range is 0 to 27"),
             tags$li(tags$b("HONC"), " - The possible score range is 0 to 10"),
           )
           

           
           
        )
   
    
    
    
  )  
)

# Define server logic required to calculate final score
server <- function(input, output) {
 
    output$RAPI <- renderText({
      
      a= Estimate_RAPI[1]+Estimate_RAPI[2]*input$Age+
        Estimate_RAPI[3]*as.numeric(input$Lifetime_use_of_other_substances)+
        Estimate_RAPI[4]*input$Age_of_first_use_of_other_substances+
        Estimate_RAPI[5]*input$Age_of_first_use_of_tobacco+
        Estimate_RAPI[6]*as.numeric(input$Family_history_of_hazardous_alcohol_use)+
        Estimate_RAPI[7]*as.numeric(input$Family_history_of_hazardous_cannabis_use)
     
      paste0(c("Your RAPI score is",round(a^2,3)))
      
      
    }) 
      
    output$CPQ <- renderText({ 
      
      b=as.numeric(Estimate_CPQ[1])+Estimate_CPQ[2]*input$Maternal_education+
        Estimate_CPQ[3]*input$Parental_Attachment+
        Estimate_CPQ[4]*as.numeric(input$Early_life_stress)+
        Estimate_CPQ[5]*as.numeric(input$Lifetime_use_of_other_substances)+
        Estimate_CPQ[6]*input$Age_of_first_use_of_cannabis+
        Estimate_CPQ[7]*input$Age_of_first_use_of_tobacco
      
      paste0(c("Your CPQ score is",round(b^2,3)))
     
             
    })
      
      output$HONC <- renderText({   
      c=Estimate_HONC[1]+Estimate_HONC[2]*input$Age+
        Estimate_HONC[3]*input$Parental_Attachment+
        Estimate_HONC[4]*as.numeric(input$Early_life_stress)+
        Estimate_HONC[5]*input$Age_of_first_use_of_cannabis+
        Estimate_HONC[6]*input$Age_of_first_use_of_tobacco+
        Estimate_HONC[7]*input$Age_of_first_use_of_tobacco^2+
        Estimate_HONC[8]*input$Age_of_first_use_of_tobacco^3+
        Estimate_HONC[9]*as.numeric(input$Family_use_of_cigarette)+
        Estimate_HONC[10]*as.numeric(input$Family_history_of_hazardous_cannabis_use)
      
      paste0(c("Your HONC score is",round(c,3)))
      
     
      }) 
     
      
    
}


# Run the application 
shinyApp(ui = ui, server = server)
