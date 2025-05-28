library(shiny)

ui <- fluidPage(
  titlePanel("Recommendations for Drug Use for Heart Failure"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("method", "Select Judgement Method:",
                  choices = c("Three Depth Tree" = "method1",
                              "Four Depth Tree" = "method2")),
      
      # method1 input
      conditionalPanel(
        condition = "input.method == 'method1'",
        selectInput("HXAFIBN", 
                    "Is patient experiencing any symptoms of atrial fibrillation or flutter?",
                    choices = c("Yes" = "Y", "No" = "N")),
        selectInput("BNP_m", 
                    "BNP Measure:",
                    choices = c("NT-proBNP(pg/mL)" = "NTBNP", "BNP(pg/dL)" = "BNP")),
        numericInput("BNP", 
                     "BNP value:", 
                     value = 100, 
                     min = 0, 
                     max = 100000, 
                     step = 1),
      ),
      
      # method2 input
      conditionalPanel(
        condition = "input.method == 'method2'",
        selectInput("HXAFIBN", 
                    "Is patient experiencing any symptoms of atrial fibrillation or flutter?",
                    choices = c("Yes" = "Y", "No" = "N")),
        selectInput("BNP_m", 
                    "BNP Measure:",
                    choices = c("NT-proBNP(pg/mL)" = "NTBNP", "BNP(pg/dL)" = "BNP")),
        numericInput("BNP", 
                     "BNP value:", 
                     value = 100, 
                     min = 0, 
                     max = 100000, 
                     step = 1),
        selectInput("BMRAN", 
                    "Is patient ever taking aldosterone antagonists (MRAs)?",
                    choices = c("Yes" = "Y", "No" = "N")),
      ),
    
      actionButton("submit", "Submit")
    ),
    
    mainPanel(
      h3("Result"),
      textOutput("result")
    )
  )
)




server <- function(input, output, session) {
  result <- eventReactive(input$submit, {  
    req(input$method) 
    
    if (input$method == "method1") {
      req(input$HXAFIBN, input$BNP_m, input$BNP)
      if (input$BNP_m == "BNP"){
        BNPV <- (input$BNP-1523.727)/2971.394
      }
      else{
        BNPV <- (input$BNP-6667.768)/7956.389
      }
      if (input$HXAFIBN == "N" && BNPV < -0.25) {
        return("Torsemide")
      } else if (input$HXAFIBN == "N" && BNPV >= -0.25) {
        return("Foresemide")
      } else if (input$HXAFIBN == "Y" && BNPV < -0.25) {
        return("Furosemide")
      } else {
        return("Furosemide")
      }
    } else if (input$method == "method2") {
      req(input$HXAFIBN, input$BNP_m, input$BNP, input$BMRAN) 
      if (input$BNP_m == "BNP"){
        BNPV <- (input$BNP-1523.727)/2971.394
      }
      else{
        BNPV <- (input$BNP-6667.768)/7956.389
      }
      if (input$HXAFIBN == "N"){
        if (BNPV < -0.25){
          if (input$BMRAN == 'Y'){
            return("Torsemide")
          }
          else {return("Torsemide")}
        } else if (BNPV >= -0.25){
          if (input$BMRAN == 'Y'){
            return("Torsemide")
          }
          else {return("Furosemide")}
        }
      }else if (input$HXAFIBN == "Y"){
        if (BNPV < -0.25){
          if (input$BMRAN == 'Y'){
            return("Torsemide")
          }
          else {return("Furosemide")}
        } else if (BNPV >= -0.25){
          return("Furosemide")
        }
      }
    }
    
  })
  
  output$result <- renderText({
    result()  
  })
}


shinyApp(ui = ui, server = server)
