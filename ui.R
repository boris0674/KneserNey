








ui <- fluidPage(theme=shinytheme("cyborg"), column(3,offset = 4, titlePanel("My NLP forecaster")),
                
               
               
                
                
                
                mainPanel( tags$br(h5("Enter in the area below a text made of at least 4 words, Kneser-Ney algo will return the 3 most likely options for the following word in descending order")), 
                           br(), br(), br(),
                           textAreaInput("char_vec", label="Enter  your text here",  width="600px", resize="horizontal"),
                           br(), br(), br(),br(),
                           uiOutput("submit"),
                           
                           br(), br(), br(),
                           verbatimTextOutput("txt_pred")
                           
                )
                
)

