library(shiny)

ui <- fluidPage(
  titlePanel("CARD (scRNA-seq for the deconvolution of spatial transcriptomics)"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Step 1: Choose spatial count RData",
                accept=c('.RData, .Rds')
      ),
      fileInput("file2", "Step 2: Choose spatial location RData",
                accept=c('.RData, .Rds')
      ),
      fileInput("file3", "Step 3: Choose singcell count RData",
                accept=c('.RData, .Rds')
      ),
      fileInput("file4", "Step 4: Choose singcell metadata RData",
                accept=c('.RData, .Rds')
      ),
      hr(),
      sliderInput("minCountGene", "Step 4: The number of treatment associated dimensions",
                  min = 0, max = 10000,
                  value = 100),
      sliderInput("minCountSpot", "Step 5: The number of batch associated dimensions",
                  min = 0, max = 100,
                  value = 5),
      hr(),
      radioButtons("DownloadType", "Final step: Download type (matrix)",
                   c("Ratio matrix"="D")
      ),
      downloadButton("downloadData", "Download"),
      hr(),
      h5('Developer:'),
      h6(' YingMa0107 (method), Small runze (shiny app)'),
      br(),
      h5('Github: '),
      h6('https://github.com/YingMa0107 (YingMa0107)'),
      h6('https://github.com/hzaurzli (Small runze)'),
      br(),
      h5('Cition:'),
      h6('Spatially Informed Cell Type Deconvolution for Spatial Transcriptomics')
    ),
    mainPanel(
      h4("Deconvolution ratio"),
      br(),
      br(),
      shinycssloaders::withSpinner(
        plotOutput("card")
        )
      )
    )
  )


server <- function(input, output, session) {
  options(shiny.maxRequestSize=60*1024^2)
  dataInput1 <- reactive({
    sessionEnvir <- sys.frame()
    if (!is.null(input$file1)) eval(parse(text = load(input$file1$datapath, sessionEnvir)))
  })
  
  dataInput2 <- reactive({
    sessionEnvir <- sys.frame()
    if (!is.null(input$file2)) eval(parse(text = load(input$file2$datapath, sessionEnvir)))
  })
  
  dataInput3 <- reactive({
    sessionEnvir <- sys.frame()
    if (!is.null(input$file3)) eval(parse(text = load(input$file3$datapath, sessionEnvir)))
  })
  
  dataInput4 <- reactive({
    sessionEnvir <- sys.frame()
    if (!is.null(input$file4)) eval(parse(text = load(input$file4$datapath, sessionEnvir)))
  })
  
  
  output$card <- renderPlot({
    
    spatial_count = dataInput1()
    spatial_location = dataInput2()
    sc_count = dataInput3()
    sc_meta = dataInput4()
    
    library(CARD)
    # 设置全局变量方便下载文件
    CARD_obj <<- createCARDObject(
      sc_count = sc_count,
      sc_meta = sc_meta,
      spatial_count = spatial_count,
      spatial_location = spatial_location,
      ct.varname = "cellType",
      ct.select = unique(sc_meta$cellType),
      sample.varname = "sampleInfo",
      minCountGene = input$minCountGene,
      minCountSpot = input$minCountSpot) 
    
    # 设置全局变量方便下载文件
    CARD_obj <<- CARD_deconvolution(CARD_object = CARD_obj)
    
    p1 <- CARD.visualize.pie(
      proportion = CARD_obj@Proportion_CARD,
      spatial_location = CARD_obj@spatial_location) ### You can choose radius = NULL or your own radius number
    print(p1)
    rm()
    gc()
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      if (input$DownloadType == 'D') {
        write.csv(CARD_obj@Proportion_CARD,file,row.names = T,quote = F)
        rm()
        gc()
      } 
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)

