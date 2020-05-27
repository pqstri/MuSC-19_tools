#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(haven)

# source("https://raw.githubusercontent.com/pqstri/MuSC-19_tools/master/tbl1.R")
source("~/OneDrive/Documenti/SM/musc19/export_scripts/tbl1.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
    title = "MuSC-19 table creator tool",
    
    titlePanel("MuSC-19 table creator  tool"),
    
    h3("To use this tool:"),
    tags$ol(
        tags$li("Load your report-ready spss file"),
        tags$li("Wait for the preview table to load"),
        tags$li("Click \"Download .xlsx\"")
    ),
    
    # br(),
    # h4("Currently on Provider's Blacklist (not exported):"),
    # 
    # tags$ul(
    #     map(MPS_format$provider_blacklist, ~ tags$li(.))
    # ),
    
    br(),
    fileInput("file", NULL, accept = c(".sav")),
    
    br(),
    radioButtons("options", "Select conversion type:",
                 c("Table 1" = "tbl1"),
                 selected = "tbl1"),

    br(),
    br(),
    downloadButton("download", "Download .xlsx"),
    
    br(),
    br(),
    h2("Preview"),
    tableOutput("preview"),
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    data <- reactive({
        req(input$file)
        
        if(!endsWith(input$file$name, ".sav")) {
            validate("Invalid file; Please upload a .sav")}
        
        db <- haven::read_spss(input$file$datapath) %>% 
            filter_at(vars(grep("_\\$$", names(db), value = TRUE)), ~ . == 1)
        generate_table1(db)
    })
    
    output$preview <- renderTable({
        data()
    })
    
    output$download <- downloadHandler(
        
        filename = function() {
            paste("musc19", format(Sys.time(), "_%d%b%Y"), ".xlsx", sep = "")},
        
        content = function(file) {
            openxlsx::write.xlsx(data(), file)
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
