source("warnings.R")

checkparameterUI <- function(id) {
  ns <- NS(id)
  tagList(
    h6("Example Input File"),
    div(downloadButton(ns("Example_input.xlsx"),label="Download Template")),
    br(),
    br(),
    div(fileInput(ns("file1"), label="1. Input file (.xlsx)")),
    div(verbatimTextOutput(ns("print1")))
)}

checkparameter <- function(input, output, session, outfile) {

  vals <- reactiveValues(df = NULL)

  observeEvent(input$file1,{
    inFile <- input$file1
    df <- read_xlsx(inFile$datapath, col_names=FALSE)
    vals$df <- as.data.frame(df)
    vals$df
  })
  # output$downTemplate <- downloadHandler(
  #   filename = function(){
  #     if(outfile =="inp_first"){
  #       paste("inp_first","xlsx",sep=".")
  #       #print(results.f[[2]](), quote=F, row.names = FALSE)
  #     }else if (outfile=="inp_geneec"){
  #       paste("inp_geneec","xlsx",sep=".")
  #     }else if (outfile=="inp_scigrow"){
  #       paste("inp_scigrow","xlsx",sep=".")
  #     }
  #   },
  #   content = function(con){
  #     file.copy("Example_input.xlsx", con)
  #   })
   output$Example_input.xlsx <- downloadHandler(
     filename = function(){
       paste(outfile,"xlsx",sep=".")
     },
     content = function(con){
       file.copy(paste(outfile,"xlsx",sep="."), con)
     })

  # output$print2 <- renderPrint({
  #   req(vals$df)
  #   df2=as.matrix.data.frame(vals$df)
  #   n=dim(df2)[2]
  #   aa=c()
  #   for (i in 2:n){
  #     fails=which(df2[,i]=="Fail")
  #     a=rbind(aa,df2[fails,])
  #   }
  #   if(length(a)==0){
  #     a="All parameters passed!"
  #   }
  #   a
  # })

    output$print1 <- renderPrint({
    req(vals$df)
    df2=as.matrix.data.frame(vals$df)

    if(outfile =="inp_first"){
      warn=first_err(vals$df)
      #print(results.f[[2]](), quote=F, row.names = FALSE)
    }else if (outfile=="inp_geneec"){
      warn=geneec_err(vals$df)
    }else if (outfile=="inp_scigrow"){
      warn='No Errors!'
    }
    warn
    })

    results <- reactive({
      req(vals$df)
      if(outfile =="inp_first"){
        output1=first111(vals$df)

      }else if (outfile=="inp_geneec"){
        output1=geneec(vals$df)

      }else if (outfile=="inp_scigrow"){
        output1=scigrow(vals$df)
      }
      as.data.frame(output1)
      #capture.output(cat(output1, sep = "\n"))
    })


  # outputs <- reactive({
  #   req(vals$df)
  #   if(outfile =="inp_first"){
  #     output3=first111(vals$df)
  #
  #   }else if (outfile=="inp_geneec"){
  #     output3=geneec(vals$df)
  #
  #   }else if (outfile=="inp_scigrow"){
  #     output3=scigrow(vals$df)
  #   }
  #   as.data.frame(output3)
  #   #capture.output(cat(unlist(output3), sep = "\n"))
  # })

  # plot.data <- reactive({
  #   req(vals$df)
  #   if(outfile =="inp_first"){
  #     output1=first111(vals$df)[3]
  #   }else if (outfile=="inp_geneec"){
  #     output1=geneec(vals$df)[3]
  #   }else if (outfile=="inp_scigrow"){
  #     output1=scigrow(vals$df)[3]
  #   }
  #   as.data.frame(output1)
  #   #capture.output(cat(output1, sep = "\n"))
  # })

  return(list("results"=results))
}


