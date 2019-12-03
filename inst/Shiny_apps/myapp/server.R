source("first111.R")
source("geneec.R")
source("scigrow.R")
source("warnings.R")
source("checkparameter.R")

shinyServer(function(input, output, session) {

  results.s <- callModule(module = checkparameter, id = "SCIGROW", outfile="inp_scigrow")
  results.g <- callModule(module = checkparameter, id = "GENEEC", outfile="inp_geneec")
  results.f <- callModule(module = checkparameter, id = "FIRST", outfile="inp_first")
  #####################results
  output$print.s <- DT::renderDataTable({
    DT::datatable(results.s[[1]](), rownames = F,
                  colnames = c('GW conc (ppb)' = "GW_conc"),
                  options = list(
                    searching=F,
                    pageLength = 10,
                    lengthChange = FALSE),
                  style = "default",
                  caption = htmltools::tags$caption(
                    style = 'font.size = 14px; color:black; caption-side: top; text-align: center;',
                    htmltools::em('Groundwater concentration (unit: PPB)')))
  })

  output$print.g <- DT::renderDataTable({
    DT::datatable(results.g[[1]](), rownames = F,
                  colnames = c('Max conc (ppb)' = "Peak",
                               'Max 4 day ave. conc (ppb)' = "Max4",
                               'Max 21 day ave. conc (ppb)' = "Max21",
                               'Max 60 day ave. conc (ppb)' = "Max60",
                               'Max 90 day ave. conc (ppb)' = "Max90"),
                  options = list(
                    searching=F,
                    pageLength = 10,
                    lengthChange = FALSE),
                  style = "default",
                  caption = htmltools::tags$caption(
                    style = 'font.size = 14px; color:black; caption-side: top; text-align: center;',
                    htmltools::em("Acute and chronic generic expected environmental concentration values"))) %>%
      formatRound(columns = c('Max conc (ppb)',
                              'Max 4 day ave. conc (ppb)',
                              'Max 21 day ave. conc (ppb)',
                              'Max 60 day ave. conc (ppb)',
                              'Max 90 day ave. conc (ppb)'), digits=5)
  })

  output$print.f <- DT::renderDataTable({
    DT::datatable(results.f[[1]](), rownames = F,
                  colnames = c('Max conc (ppb) - Acute' = "Peak.day.accute",
                               'Annual Average conc (ppb) - Chronic' = "Annual.average.chronic"),
                  options = list(
                    searching=F,
                    pageLength = 10,
                    lengthChange = FALSE),
                  style = "default",
                  caption = htmltools::tags$caption(
                    style = 'font.size = 14px; color:black; caption-side: top; text-align: center;',
                    htmltools::em('FIRST estimates maximum values (acute) and long-term (chronic) average concentrations of pesticides in drinking water'))) %>%
      formatRound(columns = c('Max conc (ppb) - Acute',
                              'Annual Average conc (ppb) - Chronic'), digits=5)
  })
  ##################################
  res.s <- reactive({results.s[[1]]()})
  res.g <- reactive({results.g[[1]]()})
  res.f <- reactive({results.f[[1]]()})
  ###################################
  filname1 <- reactive({input$fname1 })
  filname2 <- reactive({input$fname2 })
  filname3 <- reactive({input$fname3 })
  ###################################
    observeEvent(input$submit1,{
       tempReport <- file.path(tempdir(), "report.Rmd")
       file.copy("report.Rmd", tempReport, overwrite = TRUE)
       rmarkdown::render(tempReport,
                         output_file=filname1(),
                         output_dir= choose.dir(default = "",
                                                caption = "Select folder"))
       })

    observeEvent(input$submit2,{
      tempReport <- file.path(tempdir(), "reportg.Rmd")
      file.copy("reportg.Rmd", tempReport, overwrite = TRUE)
      rmarkdown::render(tempReport,
                        output_file=filname2(),
                        output_dir= choose.dir(default = "",
                                               caption = "Select folder"))
      })

    observeEvent(input$submit3,{
      tempReport <- file.path(tempdir(), "reportf.Rmd")
      file.copy("reportf.Rmd", tempReport, overwrite = TRUE)
      rmarkdown::render(tempReport,
                        output_file=filname3(),
                        output_dir= choose.dir(default = "",
                                               caption = "Select folder"))
      })

    ############### PLOTS
    f1 <- list(
      family = "Arial, sans-serif",
      size = 16,
      color = "lightgrey")

    f2 <- list(
      family = "Arial, sans-serif",
      size = 20,
      color = "black")

    a <- list(
      titlefont = f1,
      showticklabels = TRUE,
      tickangle = 0,
      tickfont = f2,
      showgrid=FALSE)

    plot.s1 <- reactive({
      plot_ly(results.s[[1]](), x = ~Product, y=~GW_conc, type = 'bar', name = 'GW_conc') %>%
        layout(
          legend = list(bgcolor = "#ffffff00"),
          margin = list(l=110, r=20, b=10, t=10, pad=0),
          autosize = F,
          yaxis = list(title = 'Concentration (PPB)',a),
          barmode = 'group',
          xaxis = list(title = 'Product',a, type="category"))
    })

    plot.g1 <- reactive({
      plot_ly(results.g[[1]](), x = ~Product, y = ~Peak, type = 'bar', name = 'Max ave. conc') %>%
        add_trace(y = ~Max4, name = 'Max 4 day ave. conc') %>%
        add_trace(y = ~Max21, name = 'Max 21 day ave. conc') %>%
        add_trace(y = ~Max60, name = 'Max 60 day ave. conc') %>%
        add_trace(y = ~Max90, name = 'Max 90 day ave. conc') %>%
        layout(
          legend = list(bgcolor = "#ffffff00"),
          #margin = list(l=10, r=20, b=10, t=10, pad=0),
          autosize = F,
          yaxis = list(title = 'Concentration (PPB)', a),
          barmode = 'group',
          xaxis = list(title = 'Product', a, type="category"))
    })

    plot.f1 <- reactive({
      plot_ly(results.f[[1]](), x = ~Product, y = ~Peak.day.accute, type = 'bar', name = 'Max conc - Acutec') %>%
        add_trace(y = ~Annual.average.chronic, name = 'Annual Average conc - Chronic') %>%
        layout(
          legend = list(bgcolor = "#ffffff00"),
          margin = list(l=110, r=20, b=10, t=10, pad=10),
          autosize = F,
          #autosize = F, width =700, height = 400,
          yaxis = list(title = 'Concentration (PPB)', a),
          barmode = 'group',
          xaxis = list(title = 'Product', a, type="category"))
    })

    output$plot.s <- renderPlotly({plot.s1()})
    output$plot.g <- renderPlotly({plot.g1()})
    output$plot.f <- renderPlotly({plot.f1()})
    #####################################################

   output$pca2 <- renderPrint({
     pca1=readLines("pca.txt")
     capture.output(cat(pca1, sep = "\n"))
     print(pca1, quote=F)
   })

  output$ncorp <- renderPrint({
    ncorp=readLines("ncorp.txt")
    capture.output(cat(ncorp, sep = "\n"))
    print(ncorp, quote=F)
  })
  output$method <- renderPrint({
    method=readLines("method.txt")
    capture.output(cat(method, sep = "\n"))
    print(method, quote=F)
  })

  output$method2 <- renderPrint({
    method=readLines("method.txt")
    capture.output(cat(method, sep = "\n"))
    print(method, quote=F)
  })

  output$ncorp2 <- renderPrint({
    ncorp=readLines("ncorp.txt")
    capture.output(cat(ncorp, sep = "\n"))
    print(ncorp, quote=F)
  })

  output$airflg <- renderPrint({
    airflg=readLines("airflg.txt")
    capture.output(cat(airflg, sep = "\n"))
    print(airflg, quote=F)
  })

  session$onSessionEnded(function() {
    stopApp()
  })
})


