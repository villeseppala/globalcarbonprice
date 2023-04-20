

server <- function(input,output, session) {
  
  output$testy= renderText("testyy")
  
  
  output$dimension_display <- renderText({
    paste(input$dim[1], input$dim[2], input$dim[2]/input$dim[1])
  })
  
  # output$yearcc = renderUI({
  #   
  #   # sliderInput2 <- function(inputId, label, min, max, value, step=NULL, from_min, from_max){
  #   #   
  #   # sliderInput2("slider", "Slide:",
  #   #              min = 0, max = 100, value = 50, step = 5, from_min = 20, from_max = 80
  #   # )
  #   
  #   sliderInput2("yearc",
  #               label = NULL, min=1970, max=2100, value = 2080, step=1, 
  #               from_min=input$vuo[1], from_max=input$vuo[2], width="100%", animate=TRUE
  #                  )
  #   
  #   
  # })
  # 
  # 
  # 
  # output$yearcccc = renderUI({
  #   
  #   # sliderInput2 <- function(inputId, label, min, max, value, step=NULL, from_min, from_max){
  #   #   
  #   # sliderInput2("slider", "Slide:",
  #   #              min = 0, max = 100, value = 50, step = 5, from_min = 20, from_max = 80
  #   # )
  #   
  #   sliderInput("yearccc",
  #                label = NULL, min=1970, max=2100, value = 2080, step=1, 
  #               width="100%", animate=TRUE
  #   )
  #   
  #   
  # })
  
  
  output$lets <- renderUI({
    HTML("<p> Displaying greek letter delta as a symbol:<br> \u0394 
         </p>")
  })
  
  output$selitys2 = renderText({
    
    "In progress, finished in 2023. Tool to explore various scenarios for global carbon
    price/dividend. Funded by the Kone foundation. www.villeseppala.fi. Data: UN, IPCC, Friedlingstein et al. 2021, World Bank"
    
  })
  
  output$selitys = renderText({
    
    switch(input$lang, "eng"="In progress, finished in February 2023. Tool to explore various global carbon
    tax/dividend scenarios by adjusting the assumed population progression, the rate of emission
    reductions and the level of carbon tax. Funded by the Kone foundation. www.villeseppala.fi. Data: UN, IPCC, Friedlingstein et al. 2021, World Bank",
    "fin"="Kesken, valmistuu helmikuussa 2023. Työkalulla voi hahmotella erilaisia hiilen hinnoittelu ja hiiliosinko
           -skenaarioita säätämällä oletettua väestökehitystä, päästövähennyksiä ja hiiliveroa. Koneen säätiön rahoittama hanke. www.villeseppala.fi. Data: UN, IPCC, Friedlingstein et al. 2021, World Bank")
    # )
  })
  
  
  
  output$radioTest <- renderUI({
    
    #     
    # options= c(list("Logar"="logarithmic", 
    #                 c(tags$div(class = "empty-stars"))="percentual",
    #          "Linear"="linear"))
    
    
    cho = list(
      HTML(paste("<p style='color:white; transform: rotate(320deg);'> &curvearrowleft;</p>","Perc", "gggg")),
      HTML("<p style='color:white; transform: rotate(320deg);'>&#8630;</p>"),
      HTML("<p style='color:white; transform: rotate(320deg);'>&#8405;</p>")
    )
    cho2 = list("percentual", "linear", "logarithmic")
    
    
    radioButtons('pri', 'What item do you select ?', choiceNames = cho, choiceValues = cho2, selected = character(0))
    
  })
  
  
  rv <- reactiveValues(budget = NULL)
  rv <- reactiveValues(total = NULL)
  rv <- reactiveValues(lyear = NULL)
  rv <- reactiveValues(fyear = NULL)
  rv <- reactiveValues(time = NULL)
  rv <- reactiveValues(year = NULL)
  rv <- reactiveValues(x = NULL)
  rv <- reactiveValues(y = NULL)
  rv <- reactiveValues(paa = NULL)
  rv <- reactiveValues(prate = NULL)
  rv <- reactiveValues(pgrowth = NULL)
  rv <- reactiveValues(valu = NULL)
  rv <- reactiveValues(ale = 0)
  rv <- reactiveValues(alert4 = NULL)
  
  rv <- reactiveValues(yearc = NULL)
  rv <- reactiveValues(lastyear = NULL)
  
  rv <- reactiveValues(popc = 2)
  rv <- reactiveValues(showpop = NULL)
  rv <- reactiveValues(showprice = NULL)
  rv <- reactiveValues(showavgcost = NULL)
  rv <- reactiveValues(showdividend = NULL)
  rv <- reactiveValues(showavgnetcost = FALSE)
  rv <- reactiveValues(showuserfossil = FALSE)
  rv <- reactiveValues(showusercost = FALSE)
  rv <- reactiveValues(shownetcost = FALSE)
  rv <- reactiveValues(showfossil = TRUE)
  rv <- reactiveValues(showland = TRUE)
  rv <- reactiveValues(shownet = TRUE)
  rv <- reactiveValues(showavgfossil = TRUE)
  rv <- reactiveValues(showcountryfossil = FALSE)
  rv <- reactiveValues(showcountrycost = FALSE)
  rv <- reactiveValues(showcountrynetcost = FALSE)
  rv <- reactiveValues(showcountrypop = FALSE)
  
  
  rv <- reactiveValues(plot2 = NULL)
  rv <- reactiveValues(plot3 = NULL)
  rv <- reactiveValues(plot4 = NULL)
  rv <- reactiveValues(plot5 = NULL)
  rv <- reactiveValues(plot6 = NULL)
  
  rv <- reactiveValues(lastin = NULL)
  rv <- reactiveValues(info = NULL)
  rv <- reactiveValues(info3text = NULL)
  rv <- reactiveValues(info4text = NULL)
  rv <- reactiveValues(infofossil = NULL)
  
  rv$infofossiltext = c("Total fossil C02 emissions across all countries and individuals. Source for emissions from 2021 and before: Global Carbon Project 2022 (Friedlingstein et al. 2021)")
  rv$infolandtext = c("C02 emissions from land use change across all countries. Source for emissions from 2021 and before: Global Carbon Project 2022 (Friedlingstein et al. 2021)")
  rv$infonettext = c("Net C02 emissions from fossil and land use across all countries. Source for emissions from 2021 and before: Global Carbon Project 2022 (Friedlingstein et al. 2021)")
  rv$infopoptext = c("World population, billions. Statistics and projections from United Natioons.")
  rv$infoavgfossil =c("Average emissions across all individuals. Result of dividing total emissions with population size")
  rv$infopricetext =c("Global carbon price, €/t. Should be set high enough to achieve the chosen emission reductions.") 
  rv$infoavgcosttext = c("Average carbon cost across all individuals. Product of average emissions and carbon price.")
  rv$infodividendtext = c("Carbon dividend given to each world citizen.")
  rv$infoavgnetcosttext = c("Net average income across all individuals. Sum of average carbon cost and carbon dividend.")
  rv$infouserfossiltext = c("Individual emission path chosen by the user.")
  rv$infousercosttext = c("Carbon price cost for the user. Result of users emissions and carbon price.")
  rv$infonetcosttext = c("User's net income. Sum of user's carbon cost and carbon dividend.")  
  
  rv$info3text = c("Start start year carbon price defines")
  rv$info4text = c("Neutrality year carbon price defines")
  
  info3text = c("Start start year carbon price defines")
  
  
  infolist = c("info3", "info4", "infofossil", "infolul", "infonet", "infopop",
               "infoavgfossil","infoprice", "infoavgcost","infodividend",
               "infoavgnetcost", "infouserfossil", "infousercost", "infonetcost")
  
  
  lapply(
    X = infolist,
    FUN = function(i){
      observeEvent(input[[paste0(i)]], {
        updateBox("infobox", action = "restore")
        rv$info = rv[[paste0(i,"text")]]
      })})
  
  
  o <- observe({
    updateBox("infobox", action = "remove")
    # shinyjs::click("add")
    o$destroy() # destroy observer as it has no use after initial button click
  })
  
  
  # initially select carbon budget from left menu
  o <- observe({
    # observeEvent(input[[paste0(i)]], {
    updateNavlistPanel(session, id = "nok", selected = "1. Carbon budget")
    # shinyjs::click("add")
    o$destroy() # destroy observer as it has no use after initial button click
  })
  
  
  lllist =c("country","bud","vuo","paa", "muo", "sprice", "eprice","pri" ,"indi1" , "indi2", "muoindi", "indi","popc","con")
  
  
  observe({
    lapply(lllist, function(x) {
      observe({
        input[[x]]
        rv$lastin <- x
      })
    })
  })
  
  observeEvent(input$nok,{
    if (input$nok =="EXTRA: Country profiles"){
     rv$alert4 =TRUE
    o$destroy()
    }
    })


  
  
  
  observeEvent(input$vuo,{
    rv$fyear = input$vuo[1]
    rv$lyear = input$vuo[2]
    rv$time = (input$vuo[2] - input$vuo[1])
    rv$year = input$vuo[1]:input$vuo[2]
    rv$yearc = input$vuo[2]
  }    )
  
  observeEvent(input$popc,{
    rv$popc = input$popc
    
  }    )
  
  observeEvent(input$con, {
    updateSliderInput(
    inputId = "conb",
    value = input$con
    )
  })

  observeEvent(input$conb, {
    updateSliderInput(
      inputId = "con",
      value = input$conb
    )
  })
  
    
  observeEvent(input$indi1,{
    rv$indi1 = input$indi1
    
  }    )
  observeEvent(input$indi2,{
    rv$indi2 = input$indi2
    
  }    )   
  
  
  observeEvent(input$indi, {
    if (input$indi %in% ll2) {
      shinyjs::disable("indi1")
      shinyjs::disable("indi2")
      shinyjs::disable("muoindi")
      
      
    }
    else if (input$indi =="none"){
      shinyjs::enable("indi1")
      shinyjs::enable("indi2")
      shinyjs::enable("muoindi")
      
    }
  })
  
  observeEvent(input$yearc, {

    if (
       input$yearc >= 1970 & 
      input$yearc <= input$vuo[2]) {
      rv$yearc = input$yearc
    }

    else if  (input$yearc > input$vuo[2]){
      rv$yearc=input$vuo[2]
    }

    else if  (input$yearc < 1970){
      rv$yearc=1970
    }
  })

  
  
  observeEvent(input$next1, {
     updateNavlistPanel(session, inputId = "nok", selected = "2. Population projection")
  })
  
  
  observeEvent(input$next2, {
    updateNavlistPanel(session, inputId = "nok", selected = "3. Carbon tax")
  })
  
  observeEvent(input$next3, {
    updateNavlistPanel(session, inputId = "nok", selected = "4. User emissions")
  })

  observeEvent(input$next4, {
    updateNavlistPanel(session, inputId = "nok", selected = "EXTRA: Country profiles")
  })
  observeEvent(input$next5, {
    updateNavlistPanel(session, inputId = "nok", selected = "EXTRA: National dividend")
  })
  
  

  observeEvent(input$prev1, {
    updateNavlistPanel(session, inputId = "nok", selected = "1. Carbon budget")
  })
    
  observeEvent(input$prev2, {
    updateNavlistPanel(session, inputId = "nok", selected = "2. Population projection")
  })
  
  
  observeEvent(input$prev3, {
    updateNavlistPanel(session, inputId = "nok", selected = "3. Carbon tax")
  })
  
  observeEvent(input$prev4, {
    updateNavlistPanel(session, inputId = "nok", selected = "4. User emissions")
  })
  
  observeEvent(input$prev5, {
    updateNavlistPanel(session, inputId = "nok", selected = "EXTRA: Country profiles")
  })
  # 
  # observeEvent(input$yearc, {
  #   
  #   if ( input$yearc <= input$vuo[2]) {
  #     rv$yearc = input$yearc
  #   }
  #   
  #   else if  (input$yearc > input$vuo[2]){
  #     rv$yearc=input$vuo[2]
  #   }
  #   
  #   # else if  (input$yearc < input$vuo[1]){
  #   #   rv$yearc=input$vuo[1]
  #   # }
  # })
  # 
  
  
  showlista = c("showprice" , "showavgcost",
                "showdividend","showavgnetcost" , "showuserfossil","showusercost", "shownetcost","showpop",
                "showcountryfossil","showcountrycost", "showcountrynetcost","showcountrypop")
  
  lapply(
    X = c("showprice" , "showavgcost",
          "showdividend","showavgnetcost" , "showuserfossil","showusercost", "shownetcost","showpop", "showavgfossil",
          "showcountryfossil","showcountrycost", "showcountrynetcost","showcountrypop", "alert4" ),
    FUN = function(i){
      rv[[paste0(i)]] = FALSE
    } )
  
  showlistb = c("showfossil","showland", "shownet")
  lapply(
    X = showlistb,
    FUN = function(i){
      rv[[paste0(i)]] = TRUE
    } )
  
  
  
  
  showlist = c("showfossil","showland", "shownet", "showavgfossil","showprice" , "showavgcost",
               "showdividend","showavgnetcost" , "showuserfossil","showusercost", "shownetcost","showpop",
               "showcountryfossil","showcountrycost", "showcountrynetcost","showcountrypop")
  aat = showlist
  
  
  # observeEvent(
  #   
  #   rv[[]]I
  #   
  # )
  # values <- reactiveValues(loc = 2020)
  # 
  # observeEvent(input$plot_hover, {
  #     values$loc <- input$plot_hover$x
  #   
  #   # values$loc <- input$plot_hover$coords_css$x+1500
  # })
  # "paa", "prate", "pgrowth"
  # rv <- reactiveValues( = NULL)
  # 
  # 
  # 
  # observeEvent(input$paa, 
  #              rv$end =input$paa,
  #              rv$lend =(-1)*input$paa)
  # 
  
  
  lapply(
    X = showlist,
    FUN = function(i){
      observeEvent(input$visib,{
        
        if (input$visib ==TRUE
            
        ) {
          
          updateAwesomeCheckbox(
            session=session,
            inputId = i,
            value = rv[[paste0(i)]])
        }
        
      }     )
      
      observeEvent(input$showall, {
        updateAwesomeCheckbox(
          session=session,
          inputId = i,
          value = TRUE)
        
        updateAwesomeCheckbox(
          session=session,
          inputId = "visib",
          value = FALSE)
        
      })
      
      observeEvent(input$shownone, {
        updateAwesomeCheckbox(
          session=session,
          inputId = i,
          value = FALSE)
        
        updateAwesomeCheckbox(
          session=session,
          inputId = "visib",
          value = FALSE)
      })
      
    })
  
  
  
  lapply(
    X = showlist,
    FUN = function(i){
      
      observeEvent(rv[[paste0(i)]], {
        
        if (rv[[paste0(i)]] ==TRUE & input$visib ==TRUE) {
          
          updateAwesomeCheckbox(
            session=session,
            inputId = i,
            value = TRUE)
          
        }
      } )
      
    })
  
  observeEvent(input$nok,{
    if (input$nok =="1. Carbon budget"){
      rv$showfossil =TRUE
      rv$showland =TRUE
      rv$shownet =TRUE
      
    }
    
    else if (input$nok =="2. Population projection") {
      
      rv$showfossil =TRUE
      rv$showland =TRUE
      rv$shownet =TRUE
      rv$showpop =TRUE
      rv$showavgfossil =TRUE
      
    }
    
    else if (input$nok =="3. Carbon tax") {
      rv$showprice =TRUE
      rv$showdividend=TRUE
      rv$showavgcost=TRUE
      rv$showavgnetcost=TRUE
      rv$showpop =TRUE
      rv$showavgfossil =TRUE
      
    }
    
    else if (input$nok =="4. User emissions") {
      rv$showusercost =TRUE
      rv$showuserfossil=TRUE
      rv$shownetcost =TRUE
      
      rv$showprice =TRUE
      rv$showdividend=TRUE
      rv$showavgcost=TRUE
      rv$showavgnetcost=TRUE
      rv$showpop =TRUE
      rv$showavgfossil =TRUE
    }
    
    else if (input$nok =="EXTRA: Country profiles") {
      rv$showcountrycost =TRUE
      rv$showcountryfossil=TRUE
      rv$showcountrynetcost =TRUE
      rv$showcountrypop =FALSE
      
    }
    
    
  })
  
  # 
  # observeEvent(input$countr, {
  #   if (input$countr %in% c(ll2)) {
  #     rv$showcountryfossil = TRUE
  #     rv$showcountrycost = TRUE
  #     rv$showcountrynetcost = TRUE
  #   }
  # })
  # 
  
  # rv <- reactiveValues(showcountryfossil = FALSE)
  # rv <- reactiveValues(showcountrycost = FALSE)
  # rv <- reactiveValues(showcountrynetcost = FALSE)
  # rv <- reactiveValues(showcountrypop = FALSE)
  # 
  
  
  
  lapply(
    X = c("showfossil", "showland", "shownet"),
    FUN = function(i){
      
      observeEvent(input[[paste0(i)]], {
        
        if (input$showfossil == TRUE | input$showland==TRUE | input$shownet ==TRUE) {
          rv$plot2 = "plot2"}
        else {
          rv$plot2 = ""}
        
      } )
    } )
  
  
  lapply(
    X = c("showavgfossil", "showuserfossil"),
    FUN = function(i){
      
      observeEvent(input[[paste0(i)]], {
        
        if (input$showavgfossil == TRUE | input$showuserfossil==TRUE) {
          rv$plot3 = "plot3"}
        else {
          rv$plot3 = ""}
        
      } )
    } )
  # 
  lapply(
    X = c("showprice"),
    FUN = function(i){
      
      observeEvent(input[[paste0(i)]], {
        
        if (input$showprice == TRUE) {
          rv$plot4 = "plot4"}
        else {
          rv$plot4 = ""}
        
      } )
    } )
  # 
  lapply(
    X = c("showdividend", "showavgnetcost", "showavgcost", "showusercost", "shownetcost"),
    FUN = function(i){
      
      observeEvent(input[[paste0(i)]], {
        
        if (input$showavgcost == TRUE | input$showdividend ==TRUE | input$showavgnetcost==TRUE | input$showusercost==TRUE | input$shownetcost ==TRUE)   {
          rv$plot5 = "plot5"}
        else {
          rv$plot5 = ""}
        
      } )
    } )
  
  # observe({
  #   if (rv$showfossil == TRUE | rv$showland==TRUE | rv$shownet ==TRUE)   {
  #     rv$plot2 = "plot2"}
  #   else {
  #     rv$plot2 = ""}
  #   
  # })
  # 
  # observe({
  #   if (rv$showavgfossil == TRUE | rv$showuserfossil==TRUE)   {
  #     rv$plot3 = "plot3"  }
  #   else {
  #     rv$plot3 = ""}
  # })
  # 
  # observe({
  #   if (rv$showprice == TRUE)   {
  #     rv$plot4 = "plot4" }
  #   else {
  #     rv$plot4 = ""}
  # })
  # 
  # observe({
  #   if (rv$showavgcost == TRUE | rv$showdividend ==TRUE | rv$showavgnetcost==TRUE | rv$showusercost==TRUE | rv$shownetcost ==TRUE)   {
  #     rv$plot5 = "plot5" }
  #   else {
  #     rv$plot5 = ""}
  # })
  # observeEvent(input$alert,{
  #   rv$ale = input$alert
  # })
  
  # observeEvent(input$yearc,{
  #   rv$yearc = input$yearc
  # })
  # lapply(
  #   X = c("vuo",  "yearc"),
  #   
  #   FUN = function(i){
  #     observeEvent(input$bud, {
  #       # freezeReactiveValue(input, i)
  #       updateSliderInput(
  #         session = session,
  #         inputId =i,
  #         value = skenbbs()[sken==input$bud & nams==i, vals]
  #       )
  #     })
  #   } )
  # 
  # 
  # lapply(
  #   X = c("muo", "pri"),
  #   
  #   FUN = function(i){
  #     
  #     observeEvent(input$bud, {
  #       # freezeReactiveValue(input, i)
  #       
  #       updateRadioButtons(
  #         session = session,
  #         inputId =i,
  #         selected = skenbbs()[sken==input$bud & nams==i, vals]
  #       )
  #       
  #     })
  #   } )
  # 
  
  
  # lapply(
  #   X = c("paa",  "eprice"),
  #   
  #   FUN = function(i){
  #     
  #     observeEvent(input$bud, {
  #       # freezeReactiveValue(input, i)
  #       updateNumericInput(
  #         session = session,
  #         inputId =i,
  #         value = skenbbs()[sken==input$bud & nams==i, vals]
  #       )
  #       
  #     })
  #   } )
  # 
  
  
  # lapply(
  #   X = c("paa",  "eprice"),
  #   
  #   FUN = function(i){
  observeEvent(input$bud,{
    rv$budd = input$bud
    priority = 9
    
  }    ) 
  
  
  
      observeEvent(input$bud, {
         # freezeReactiveValue(input, "paa")
        
        updateSliderInput(
          
          session = session,
          inputId ="vuo",
          value = skenbbs()[sken==input$bud & nams=="vuo", vals]
        )
        
        # updateSliderInput(
        #   session = session,
        #   inputId ="yearc",
        #   value = skenbbs()[sken==input$bud & nams=="yearc", vals]
        # )
        # 
        updateNumericInput(
          session = session,
          inputId ="paa",
          value = skenbbs()[sken==input$bud & nams=="paa", vals]
        )
        
        updateNumericInput(
          session = session,
          inputId ="eprice",
          value = skenbbs()[sken==input$bud & nams=="eprice", vals]
        )
        
        

        updateRadioButtons(
          session = session,
          inputId ="muo",
          selected = skenbbs()[sken==input$bud & nams=="muo", vals]
        )
        updateRadioButtons(
          session = session,
          inputId ="pri",
          selected = skenbbs()[sken==input$bud & nams=="pri", vals]
        )
        priority = 10
        
      })
    # } )
    # 
  
  
  

  
  
  
  observeEvent(input$vuo, {
    updateSliderInput(
      session = session,
      inputId = "yearc",
      value = input$vuo[2]
    )
  })
  
  
  observeEvent(input$yearc,{
    if(input$yearc > input$vuo[2]){
      updateSliderInput(session, "yearc", value = input$vuo[2])}
  })
  observeEvent(input$yearc,{
    if(input$yearc < 1970){
      updateSliderInput(session, "yearc", value =1970)}
  })
  
  skenbbs = reactive({
    
    skenbs
  })
  
  
  
  dummy = reactive({input$dummy})
  
  populaatio = reactive({
    populaatio = copy(pop2)
    populaatio = populaatio[var ==rv$popc, ]
    populaatio
    
  })
  # year %in% rv$fyear:rv$lyear & 
  dats = reactive({
    # req(input$bud, cancelOutput = TRUE)
    # req(input$vuo, cancelOutput = TRUE)
    # req(input$muo, cancelOutput = TRUE)
    # req(input$pri, cancelOutput = TRUE)
    # req(input$paa, cancelOutput = TRUE)
    # req(input$eprice, cancelOutput = TRUE)
    # req(input$yearc, cancelOutput = TRUE)
    # 
    # print(rv$showfossil)
    # print(rv$showprice)
    # print(rv$showpop)
    
     # freezeReactiveValue(input, "bud")
    # req(input$vuo)

    
    # start=input$fstart
    # lstart=input$lstart
    
# AS of now, budget does not exclude emissions between 2020 and lastyear
    start=ppaa[year ==lastyear & sec =="fossil", yy]
    #land use emission start
    lstart = ppaa[year ==lastyear & sec =="land", yy]
    
    # years for calculation
    yearl = lastyear:(as.numeric(input$vuo[1]))
    
    # years for data set
    yearl2 = (lastyear+1):(as.numeric(input$vuo[1])-1)
    
    ll = max(as.numeric(length(yearl)),0)
    time = max(as.numeric(length(yearl)),0)
    
    fossil = seq(start, as.numeric(input$fstart), length.out= time)
    # [,-1]
    land = seq(lstart, as.numeric(input$lstart),length.out = time)
    # [,-1]
    fossil = fossil[-1]
    land = land[-1]
    
    fossil = head(fossil,-1)
    land = head(land,-1)  
    
    
    
    
    
    
    
      
   #   start=ppaa[year ==lastyear & sec =="fossil", yy]
   #  # 
   #   lstart = ppaa[year ==lastyear & sec =="land", yy]
   #  
   #   yearl = (lastyear+1):(as.numeric(input$vuo[1])-1)
   #  # ll = as.numeric(length(yearl))
   #  # yearl = lastyear:(as.numeric(input$))
   #  # time  = (lyear-lastyear)
   # # time = as.numeric(length(yearl))
   # time = max(as.numeric(length(yearl)),0)
   # 
   #  
   #  # fossil = seq(start, as.numeric(input$fstart), length.out= (time+2))
   #  # land = seq(lstart, as.numeric(input$lstart),length.out = (time+2))
   #  
   #  fossil = seq(start, as.numeric(input$fstart), length.out= time)
   #  # [,-1]
   #  land = seq(lstart, as.numeric(input$lstart),length.out = time)
    # fossil = rep(start, ll)
    # land = rep(lstart, ll)
    
    
    inter = data.frame(yearl2, fossil, land)
    inter$net = inter$fossil + inter$land
    inter=as.data.table(inter)
    
    sumnet = inter[,sum(net)]
    
    
    
    
    
    
    # lstart = input$lstart
    # start=ppaa[year ==lastyear & sec =="fossil", yy]
    # #land use emission start
    # lstart = ppaa[year ==lastyear & sec =="land", yy]
    # 
    # # years for calculation
    # yearl = lastyear:(as.numeric(input$vuo[1]))
    # 
    # # years for data set
    # yearl2 = (lastyear+1):(as.numeric(input$vuo[1])-1)
    # 
    # ll = max(as.numeric(length(yearl)),0)
    # time = max(as.numeric(length(yearl)),0)
    # 
    # fossil = seq(start, as.numeric(input$fstart), length.out= time)
    # # [,-1]
    # land = seq(lstart, as.numeric(input$lstart),length.out = time)
    # # [,-1]
    # fossil = fossil[-1]
    # land = land[-1]
    # 
    # fossil = head(fossil,-1)
    # land = head(land,-1)
    # 
    # # fossil = rep(start, ll)
    # # land = rep(lstart, ll)
    # net = fossil+land
    # ## must fix so  that population is from population projection and not constant   
    # ppaax = ppaa[1,]
    # ppax =do.call("rbind", replicate(ll-2, ppaax, simplify = FALSE))
    # ppax[sec=="fossil", yy:=fossil]
    # ppax[sec=="land", yy:=land]
    # ppax[sec=="net", yy:=net]
    # ppax$year = yearl2
    # 
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    #first year of simluation
    fyear <- as.numeric(input$vuo[1])
    
    #last year of simulation
    lyear <-  as.numeric(input$vuo[2])
    
    #lenght of simulation in years
    time = (lyear - fyear)
    
    #shape of emission graph
    muo = input$muo
    
    
    #lenght of simulation in years
    time = (lyear - fyear)
    
    #years in simulation
    year = fyear:lyear
    
    
    
    
    
    
    
    
    
    start = input$fstart
    lstart = input$lstart
    # start=ppaa[year ==lastyear & sec =="fossil", yy]
    # lstart = ppaa[year ==lastyear & sec =="land", yy]
    
    end<- as.numeric(input$paa)
    lend = (-1)*end
    budget<- as.numeric(input$bud)-sumnet
    
    if (input$muo == "percentual")  {
      f3 = f3 <- function(rate,start,time, end) {
        end - start * (1-rate/100)^(time+1)
      }
      
    } else if (input$muo=="linear") {
      f3 <- function(rate,start,time, end) {
        end - (start - rate*(time+1))
      }
    } 
    else if  (input$muo=="logarithmic") {
      f3 <- function(rate,start,time, end) {
        # end - (start - rate*log(time+1))
        end - (start - rate^(time+1))
        
      }}
    
    
    # emission rate solving with given values
    result <- uniroot(f3,start=start,time=time, end=end, lower=-0, upper=100)$root
    u = result[1]
    
    rate = u
    
    #  yearly emissions function
    if (input$muo == "percentual")  {
      g = function(start, rate, time) {
        c(start*(1-rate/100)^(time+1))
      }
    } else if (input$muo=="linear") {
      g = function(start, rate, time) {
        c(start - rate*(time+1))
      }
    } 
    else if  (input$muo=="logarithmic") {
      
      g = function(start, rate, time) {
        # c(start-rate*log(time+1))
        c(start-rate^(time+1))
        
      }}
    
    #applying yearly emission function to calculate emissions
    fossil = g(start, rate, 0:time)
    
    
    # function for emissions cumulation over time
    if (input$muo == "percentual")  {
      geomsuma = function(start, rate, time) {
        x = 0
        for(i in 0:time) x = x + start * (1-rate/100)^(i)
        return(x)  }
      
      
    } else if (input$muo=="linear") {
      geomsuma = function(start, rate, time) {
        x = 0
        for(i in 0:time) x = x + start - rate*(i)
        return(x)  }  } 
    else if (input$muo =="logarithmic") {
      geomsuma = function(start, rate, time) {
        x = 0
        for(i in 0:time) x = x + start - rate*log(i)
        
        # for(i in 0:time) x = x + start - rate*(i)
        # for(i in 1:time) x = x + start - rate*log((i))
        return(x)  }  
      
    }
    
    # applying function for emission cumulatin
    total = geomsuma(start, rate,time)
    
    
    # ## calculating landuse emissions
    # how much needed to absorb  by land based on cumulative emissions over time
    lbudget= budget - total
    
    n=time
    #normalize
    
    #time halved
    r=round(.40*n, 0)
    
    
    #   r
    #  rr = first third of years
    rr = (0:r)
    
    # hh = second third of year
    hh = (1:(n-r-r))
    
    # nn = thrid third of years
    # nn = (1:(n-r-(n-(1-r))))
    nn = (1:r)
    
    #e = lstart + b*r
    
    model <- function(theta){
      # b = speed of emission reduction, first half
      b <- theta[1]
      
      
      
      # g = speed of emission reduction, second half
      g  <- theta[2]
      
      # k = emissions, first half
      k  <- theta[3]
      
      # i emissions, second thrd
      i = theta[4]
      
      #emissions, tihrd third
      l  <- theta[5]
      
      
      # getting to right point in y axis
      F1 <- (lstart + r*b) - (-r*g + lend)
      
      # emissions from sections according to budget
      F2 = lbudget - k - l - i
      
      # first section budget equals sum from years
      F3 = k - sum(b*rr + lstart)
      
      #third section budget equal sums from years
      F4 = l - sum(g*nn+ lstart + b*r)
      
      #second sectin budget equals sum from years
      F5 = i - ((lstart+b*r)*length(hh))
      
      
      c(F1=F1,F2=F2, F3=F3, F4=F4, F5=F5)
    }
    
    (ss <- multiroot(f = model, start = c(-12, 5,-30,-200, -30)))
    
    
    fff = unlist(ss, use.names=FALSE)
    
    #ffa = speed of emission reduction, first half
    ffa= fff[1]
    
    #fb  = speed of emission reduction, second galf
    ffb=fff[2]
    
    # sa = emissions, first half
    sa = ffa*rr + lstart
    
    # emissions, second third
    sc = rep((ffa*r+lstart), length(hh))
    
    # sb = emissions, thired third
    sb = ffb*nn+ lstart + ffa*r
    
    
    
    # vector of all landive emissions
    land = c( sa,sc,sb)
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    muoindi=input$muoindi
    start<- as.numeric(input$indi1)
    end<- as.numeric(input$indi2)
    
    if (input$muoindi == "percentual" & start>=end)  {
      f3 = f3 <- function(rateindi,start,time, end) {
        end - start * (1-rateindi/100)^(time)
      } }
    
    else if (input$muoindi == "percentual" & start<end)  {
      f3 = f3 <- function(rateindi,start,time, end) {
        end - start * (1+rateindi/100)^(time)
      } }
    
    else if (input$muoindi =="linear" & start>=end) { 
      f3 <- function(rateindi,start,time, end) {
        end - (start- rateindi*time)
      }
    }
    else if (input$muoindi =="linear" & start<end) {
      f3 <- function(rateindi,start,time, end) {
        end - (start + rateindi*time)
      }
    }
    # emission rate solving with given values
    result <- uniroot(f3,start=start,time=time, end=end, lower=-0, upper=100)$root
    u = result[1]
    rateindi = u
    #    rate
    #  yearly emissions function
    if (input$muoindi == "percentual" & start>=end)  {
      g = function(start, rateindi, time) {
        c(start*(1-rateindi/100)^(time))
      }}
    
    else if (input$muoindi == "percentual" & start<end)  {
      g = function(start, rateindi, time) {
        c(start*(1+rateindi/100)^(time))
      }}
    
    else  if (input$muoindi =="linear" & start>=end) {
      g = function(start, rateindi, time) {
        c(start - rateindi*time)
      }
    }
    else  if (input$muoindi =="linear" & start<end) {
      g = function(start, rateindi, time) {
        c(start + rateindi*time)
      }
    }
    
    #applying yearly emission function to calculate emissions
    userfossil = g(start, rateindi, 0:time)
    
    
    # function for emissions cumulation over time
    # if (input$muoindi == "percentual")  {
    #   geomsuma = function(start, rateindi, time) {
    #     x = 0
    #     for(i in 0:time) x = x + start * (1-rateindi/100)^(i)
    #     #  for(i in 1:n) a1 =  a * r^(i-1)
    #     return(x)  }
    # 
    # 
    # } else  if (input$muoindi =="linear") {
    #   geomsuma = function(start, rateindi, time) {
    #     x = 0
    #     for(i in 0:time) x = x + start - rateindi*(i)
    #     #  for(i in 1:n) a1 =  a * r^(i-1)
    #     return(x)  }  }
    # 
    # # applying function for emission cumulatin
    # totalindi = geomsuma(start, rateindi,time)
    # 
    # 
    # endindi= rep(end,time+1)
    # totalindi  = rep(totalindi,time+1)
    
    
    
    
    
    
    
    pstart = input$sprice
    pend = input$eprice
    
    #  prate=  as.numeric(input$prate)
    # pgrowth = as.numeric(input$pgrowth)
    if (input$pri == "percentual")  {
      f3 = f3 <- function(prate,pstart,time, pend) {
        pend - pstart * (1-prate/100)^(time)
      }
      
    } else if (input$pri =="linear") {
      f3 <- function(prate,pstart,time, pend) {
        pend - (pstart + prate*time)
      }
    }
    else if (input$pri=="logarithmic") {
      f3 <- function(prate,pstart,time, pend) {
        pend - (pstart + prate*log(time+1))
      }
    }
    # emission rate solving with given values
    result <- uniroot(f3,pstart=pstart,time=time, pend=pend, lower=-100, upper=1000)$root
    u = result[1]
    
    prate = u
    
    
    if (input$pri == "percentual")  {
      g = function(pstart, prate, time) {
        c(pstart*(1-prate/100)^(time))
      }
    } else  if (input$pri =="linear") {
      g = function(pstart, prate, time) {
        c(pstart + prate*time)
      }
    }
    else  if (input$pri =="logarithmic") {
      g = function(pstart, prate, time) {
        c(pstart + prate*log(time+1))
      }
    }
    
    #applying yearly emission function to calculate emissions
    price = g(pstart, prate, 0:time)
    
   
    
    pop = populaatio()[year %in% rv$fyear:rv$lyear,pop]/1000000000
    
    total  = rep(total,time+1)
    
    lyear = rep(lyear,time+1)
    budget = rep(budget,time+1)
    lbudget = rep(lbudget,time+1)
    rate = rep(rate,time+1)
    end= rep(end,time+1)
    
    
    lend= rep(lend,time+1)
    
    dats = data.frame(year,  budget, rate, fossil, land,  pop, total, price, userfossil
                      
    )                
    
    dats = as.data.table(dats)
    
    dats[, net := fossil+land]
    dats[, avgfossil := fossil/pop]
    
    dats[, avgcost :=price*avgfossil]
    dats[, dividend :=avgcost]
    dats[,avgnetcost :=0]
    
    dats[, usercost := price*userfossil]
    dats[, netcost :=usercost-avgcost]
    
    dats = as.data.table(dats)
    
  })
  
  
  lux = reactive({
    lux = as.data.table(lu)
    #
    lux[sec =="pop", visi := input$showpop]
    lux[sec =="fossil", visi := input$showfossil]
    lux[sec =="land", visi := input$showland]
    lux[sec =="price", visi := input$showprice]
    lux[sec =="avgcost", visi := input$showavgcost]
    lux[sec =="avgfossil", visi := input$showavgfossil]
    lux[sec =="userfossil", visi := input$showuserfossil]
    lux[sec =="netcost", visi := input$shownetcost]
    lux[sec =="usercost", visi := input$showusercost]
    lux[sec =="net", visi := input$shownet]
    lux[sec =="dividend", visi := input$showdividend]
    lux[sec =="avgnetcost", visi := input$showavgnetcost]
    lux[sec =="countryfossil", visi := input$showcountryfossil]
    lux[sec =="countrynetcost", visi := input$showcountrynetcost]
    lux[sec =="countrycost", visi := input$showcountrycost]
    lux[sec =="countrypop", visi := input$showcountrypop]
    
  })
  
  
  
  
  
  
  
  
  
  datss = reactive({
    budd = rv$budd
    # ppaaa = ppaaa()
    ppaa = as.data.table(ppaa)
    dats = as.data.table(dats())
    populaatio = as.data.table(populaatio())
    
    dats <- gather(dats, sec, yy, "fossil":"netcost")
    dats = as.data.table(dats)
    
    lux = as.data.table(lux())
    

    
    # yearl = lastyear:(as.numeric(input$vuo[1])-1)
    # # ll = as.numeric(length(yearl))
    # # yearl = lastyear:(as.numeric(input$))
    # # time  = (lyear-lastyear)
    # time = as.numeric(length(yearl))
    # 
    # 
    # fossil = seq(start, as.numeric(input$fstart), length.out= time)
    # land = seq(lstart, as.numeric(input$lstart),length.out = time)
    # 
    
    # 
    
    
    
    # start = input$fstart
    # lstart = input$lstart
    start=ppaa[year ==lastyear & sec =="fossil", yy]
    #land use emission start
    lstart = ppaa[year ==lastyear & sec =="land", yy]
  
    # years for calculation
      yearl = lastyear:(as.numeric(input$vuo[1]))
  
    # years for data set
      yearl2 = (lastyear+1):(as.numeric(input$vuo[1])-1)
    
    ll = max(as.numeric(length(yearl)),0)
    time = max(as.numeric(length(yearl)),0)
    
    fossil = seq(start, as.numeric(input$fstart), length.out= time)
    # [,-1]
    land = seq(lstart, as.numeric(input$lstart),length.out = time)
    # [,-1]
    fossil = fossil[-1]
    land = land[-1]
    
    fossil = head(fossil,-1)
    land = head(land,-1)
    
    # fossil = rep(start, ll)
    # land = rep(lstart, ll)
    net = fossil+land
    ## must fix so  that population is from population projection and not constant   
    # ppaax = ppaa[year ==2000,]
    
    
    
    
    # 
    # ppax =do.call("rbind", replicate(ll-2, ppaax, simplify = FALSE))
    # ppax[sec=="fossil", yy:=fossil]
    # ppax[sec=="land", yy:=land]
    # ppax[sec=="net", yy:=net]
    # pop = populaatio[year %in% yearl2,pop]
    
    pop = populaatio()[year %in% yearl2,pop]/1000000000
    
    year = yearl2
    
    ppax = data.frame(year, fossil)
    
    
    
    # ppax$fossil = fossil
    ppax$land = land
    ppax$net = net
    
    ppax$pop = pop
    
    
    ppax <- gather(ppax, sec, yy, "fossil":"pop")
    
    ppax= as.data.table(ppax)
    # ppax[populaatio[sec=="pop"], yy:=i.pop, on=c("year")]
    # ppax[sec=="pop", yy:= populaatio[ppax,on=.(year), x.pop]]
    # pacu[datso[sec=="fossil"], wyy :=i.yy, on=c("year")]
    
    
    
    
    dats = rbind(ppaa, ppax, dats, fill=TRUE)
    
    dats = as.data.table(dats)
    dats = dats[lux, col:=i.col ,on=c("sec")]
    dats = dats[lux, ala:=i.ala ,on=c("sec")]
    dats = dats[lux, pos:=i.pos ,on=c("sec")]
    dats = dats[lux, label:=i.label ,on=c("sec")]
    dats = dats[lux, mark:=i.mark ,on=c("sec")]
    dats = dats[lux, le:=i.le, on=c("sec")]
    
    dats = dats[lux, visi:=i.visi, on=c("sec")]
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  pacu = reactive ({
    
    if (input$indi %in% c(ll2) || input$countr %in% c(ll2) || input$nationalcoun %in% c(ll2))  {
      
      datso = as.data.table(datss())
      
      # end is the global per capita emissions in neutarlity year
      end = datso[year ==rv$lyear & sec =="avgfossil", yy]
      paci = as.data.table(pack)
      
      paci = paci[var ==input$popc,]
      paci = paci[, yy:=co2cap]
      
      pacu = copy(paci)
      
      years = rv$years
      
      aat = c(ll2)
      
      cstart = paci[year ==lastyear & country =="Finland", bunkers/1000000000]
      
      start = input$fstart
      # lstart = input$lstart
      # start = unique(datso[sec =="fossil" & year == lastyear, yy])
      
      ratio = cstart/start
      cend = ratio*(input$paa)
      
      time= rv$time
      
      if (input$muo == "percentual" & cstart >=cend)  {
        f3 = f3 <- function(rate,cstart,time, cend) {
          cend - cstart * (1-rate/100)^(time+1)
        } }
      
      else if (input$muo == "percentual" & cstart <cend)  {
        f3 = f3 <- function(rate,cstart,time, cend) {
          cend - cstart * (1+rate/100)^(time+1)
        }
        
      } else if (input$muo=="linear" & cstart >=cend) {
        f3 <- function(rate,cstart,time, cend) {
          cend - (cstart - rate*(time+1))
        }
      }
      else if (input$muo=="linear"& cstart <cend) {
        f3 <- function(rate,cstart,time, cend) {
          cend - (cstart + rate*(time+1))
        }  }
      
      else if  (input$muo=="logarithmic" & cstart >=cend) {
        f3 <- function(rate,cstart,time, cend) {
          cend - (cstart - rate*log(time+1))
        }}
      
      else if  (input$muo=="logarithmic" & cstart <cend) {
        f3 <- function(rate,cstart,time, cend) {
          cend - (cstart + rate*log(time+1))
          
        }}
      # emission rate solving with given values
      result <- uniroot(f3,cstart=cstart,time=time, cend=cend, lower=-0, upper=100)$root
      u = result[1]
      
      rate = u
      
      #  yearly emissions function
      if (input$muo == "percentual" & cstart >=cend)  {
        g = function(cstart, rate, time) {
          cstart*(1-rate/100)^(time+1)
        }
      }
      else if (input$muo == "percentual" & cstart <cend)  {
        g = function(cstart, rate, time) {
          cstart*(1+rate/100)^(time+1)
        }
      }
      
      else if (input$muo=="linear" & cstart >=cend) {
        g = function(cstart, rate, time) {
          c(cstart - rate*(time+1))
        }
      }
      else if (input$muo=="linear" & cstart <cend) {
        g = function(cstart, rate, time) {
          c(cstart + rate*(time+1))
        }
      }
      else if  (input$muo=="logarithmic" & cstart >=cend) {
        g = function(cstart, rate, time) {
          c(cstart-rate*log(time+1))
        }}
      else if  (input$muo=="logarithmic" & cstart <cend) {
        g = function(cstart, rate, time) {
          c(cstart+rate*log(time+1))
        }}
      
      #
      bunkera =  paci[year ==lastyear & country =="Finland", bunkers/1000000000]
      
      bunker = g(cstart, rate, 0:time)
      
      
      arg = function(start, end, convergence, cstart) {
        
        end - (start-cstart)*(1-convergence)*(end/start)
        
      }
      
      withProgress( message="Calculating country trajectories",{
        
        dei2 = function(mm) {
          
          ## since last observation year to start year
          
          time= rv$fyear-lastyear
          
          # start, end, convergence, coustart
          cstart = pacu[country == mm & year == lastyear, yy]
          start =datso[sec =="avgfossil" & year == lastyear, yy]
          
          # end =datso[sec =="avgfossil" & year == lastyear, yy]
          
          end = datso[sec =="avgfossil" & year == rv$fyear, yy]
          # convergence =.01
          convergence =input$con*((rv$fyear-lastyear)/(rv$lyear-rv$fyear))
          
          cend = arg(start, end, convergence, cstart )
          
          if (input$muo == "percentual" & cstart >=cend)  {
            f3 = f3 <- function(rate,cstart,time, cend) {
              cend - cstart * (1-rate/100)^(time+1)
            } }
          
          else if (input$muo == "percentual" & cstart <cend)  {
            f3 = f3 <- function(rate,cstart,time, cend) {
              cend - cstart * (1+rate/100)^(time+1)
            }
            
            
          } else if (input$muo=="linear" & cstart >=cend) {
            f3 <- function(rate,cstart,time, cend) {
              cend - (cstart - rate*(time+1))
            }
          }
          else if (input$muo=="linear"& cstart <cend) {
            f3 <- function(rate,cstart,time, cend) {
              cend - (cstart + rate*(time+1))
            }
          }
          
          
          else if  (input$muo=="logarithmic" & cstart >=cend) {
            f3 <- function(rate,cstart,time, cend) {
              cend - (cstart - rate*log(time+1))
              
            }}
          
          else if  (input$muo=="logarithmic" & cstart <cend) {
            f3 <- function(rate,cstart,time, cend) {
              cend - (cstart + rate*log(time+1))
              
            }}
          result <- uniroot(f3,cstart=cstart,time=time, cend=cend, lower=-0, upper=100)$root
          u = result[1]
          
          rate = u
          
          if (input$muo == "percentual" & cstart >=cend)  {
            g = function(cstart, rate, time) {
              cstart*(1-rate/100)^(time+1)
            }
          }
          else if (input$muo == "percentual" & cstart <cend)  {
            g = function(cstart, rate, time) {
              cstart*(1+rate/100)^(time+1)
            }
          }
          
          else if (input$muo=="linear" & cstart >=cend) {
            g = function(cstart, rate, time) {
              c(cstart - rate*(time+1))
            }
          }
          else if (input$muo=="linear" & cstart <cend) {
            g = function(cstart, rate, time) {
              c(cstart + rate*(time+1))
            }
          }
          else if  (input$muo=="logarithmic" & cstart >=cend) {
            g = function(cstart, rate, time) {
              c(cstart-rate*log(time+1))
            }}
          else if  (input$muo=="logarithmic" & cstart <cend) {
            g = function(cstart, rate, time) {
              c(cstart+rate*log(time+1))
            }}
          
          
          ffossil = g(cstart, rate, 1:time)
          
          pacu[country ==mm & year %in% c((lastyear+1):rv$fyear), countryfossil := ffossil]
          
          
          time= rv$time
          start =datso[sec =="avgfossil" & year == rv$fyear, yy]
          
          # start, end, convergence, coustart
          cstart = pacu[country == mm & year == rv$fyear, countryfossil]
          end =datso[sec =="avgfossil" & year == rv$lyear, yy]
          convergence =input$con
          
          cend = arg(start, end, convergence, cstart )
          
          
          
          if (input$muo == "percentual" & cstart >=cend)  {
            f3 = f3 <- function(rate,cstart,time, cend) {
              cend - cstart * (1-rate/100)^(time+1)
            } }
          
          else if (input$muo == "percentual" & cstart <cend)  {
            f3 = f3 <- function(rate,cstart,time, cend) {
              cend - cstart * (1+rate/100)^(time+1)
            }
            
          } else if (input$muo=="linear" & cstart >=cend) {
            f3 <- function(rate,cstart,time, cend) {
              cend - (cstart - rate*(time+1))
            }
          }
          else if (input$muo=="linear"& cstart <cend) {
            f3 <- function(rate,cstart,time, cend) {
              cend - (cstart + rate*(time+1))
            }
          }
          else if  (input$muo=="logarithmic" & cstart >=cend) {
            f3 <- function(rate,cstart,time, cend) {
              cend - (cstart - rate*log(time+1))
              
            }}
          
          else if  (input$muo=="logarithmic" & cstart <cend) {
            f3 <- function(rate,cstart,time, cend) {
              cend - (cstart + rate*log(time+1))
              
            }}
          # emission rate solving with given values
          result <- uniroot(f3,cstart=cstart,time=time, cend=cend, lower=-0, upper=100)$root
          u = result[1]
          
          rate = u
          
          if (input$muo == "percentual" & cstart >=cend)  {
            g = function(cstart, rate, time) {
              cstart*(1-rate/100)^(time+1)
            }
          }
          else if (input$muo == "percentual" & cstart <cend)  {
            g = function(cstart, rate, time) {
              cstart*(1+rate/100)^(time+1)
            }
          }
          
          else if (input$muo=="linear" & cstart >=cend) {
            g = function(cstart, rate, time) {
              c(cstart - rate*(time+1))
            }
          }
          else if (input$muo=="linear" & cstart <cend) {
            g = function(cstart, rate, time) {
              c(cstart + rate*(time+1))
            }
          }
          else if  (input$muo=="logarithmic" & cstart >=cend) {
            g = function(cstart, rate, time) {
              c(cstart-rate*log(time+1))
            }}
          else if  (input$muo=="logarithmic" & cstart <cend) {
            g = function(cstart, rate, time) {
              c(cstart+rate*log(time+1))
            }}
          ffossil = g(cstart, rate, 1:time)
          
          pacu[country ==mm & year %in% c((rv$fyear+1):rv$lyear), countryfossil := ffossil]
          
          
          
          pacu[country ==mm &year %in% c(lastyear:rv$fyear), bunkers := bunkera]
          
          pacu[country ==mm &year %in% c(rv$fyear:rv$lyear), bunkers := bunker]
          
          
          incProgress(1/length(aat))     
          
        }
        
        rr3 = lapply(aat, dei2)
        
      })
      
      
      ## create correction factor to account for that emissions multiplied by population
      ## don't equal for world emissions needed for budget
      pacu[year %in% c(lastyear+1:rv$lyear) & country %in% ll2,
      yyy := sum(countryfossil*pop)/1000000000, by=c("year")]
      
      pacu[datso[sec=="fossil"], wyy :=i.yy, on=c("year")]
      
      pacu[,cor:=((wyy)/(yyy+bunkers))]
      #      pacu = pacu[,cor:=((wyy*1000000)/(yyy+bunkers))-1]
      
      pacu[, fossilcountry:=cor*countryfossil]
      pacu[year <=lastyear, fossilcountry:=countryfossil]
      
      pacu[country %in% aat, tots := sum(fossilcountry*pop)/1000000000, by=c("year")]
      
      # pacu = pacu[country %in% input$countr,]
      pacu[datso[sec=="dividend"], dividend :=i.yy, on=c("year")]
      pacu[datso[sec=="price"], price :=i.yy, on=c("year")]
      pacu[,countrycost:=fossilcountry*price ]
      pacu[, dividend:=(1-input$national/100)*dividend]
      pacu[, nationaldividend:=input$national/100*countrycost]
      
      pacu[,countrynetcost:=countrycost-dividend-nationaldividend]
      
      updateNumericInput(session, "dummy", value = 1)
      
      pacu
      
    } else {
      datss()
    }
  })
  
  
  datsss= reactive ({

    
    req(input$indi1, cancelOutput = TRUE)
    req(input$indi2, cancelOutput = TRUE)
    req(input$paa, cancelOutput = TRUE)
    req(input$sprice, cancelOutput = TRUE)
    req(input$eprice, cancelOutput = TRUE)
    
    lux = lux()
    dats = as.data.table(datss())
    
    dats$country = "t"
    
    if (input$indi %in% c(ll2)) {
      
      req(pacu())
      pacu = pacu()
      
      cour  = pacu[country ==input$indi & year %in% rv$fyear:rv$lyear, fossilcountry]
      dats[year %in% rv$fyear:rv$lyear & sec =="userfossil", yy:=cour ]
      
      cour1  = pacu[country ==input$indi & year %in% rv$fyear:rv$lyear, countrycost]
      cour2  = pacu[country ==input$indi & year %in% rv$fyear:rv$lyear, countrynetcost]
      dats[year %in% rv$fyear:rv$lyear & sec =="usercost", yy:=cour1 ]
      dats[year %in% rv$fyear:rv$lyear & sec =="netcost", yy:=cour2 ]

      
      
    }
    
    
    if (input$nationalcoun %in% c(ll2)) {
      req(pacu())
      pacu = pacu()

      # cour1  = pacu[country ==input$nationalcoun & year %in% rv$fyear:rv$lyear, countrycost]
      # cour2  = pacu[country ==input$nationalcoun & year %in% rv$fyear:rv$lyear, c(dividend+nationaldividend)]
      cour2  = pacu[country ==input$nationalcoun & year %in% rv$fyear:rv$lyear, c(dividend+nationaldividend)]
      usercost =  dats[year %in% rv$fyear:rv$lyear & sec =="usercost",yy]
        # dats[year %in% rv$fyear:rv$lyear & sec =="usercost", yy:=cour1 ]


      dats[year %in% rv$fyear:rv$lyear & sec =="netcost", yy:=usercost-cour2 ]


    }
    
    
    if (is.null(input$countr)) {
      
    } else {
      
      req(pacu())
      pacu = pacu()
      
      aat = c(input$countr)
      # prin
      dei2 = function(mm) {
        
        datsk = copy(dats)
        
        datsk = datsk[sec %in% c("userfossil", "pop", "netcost", "usercost") & year %in% rv$fyear:rv$lyear,]
        # 
        datsk$country = mm
        
        
        datsj = datsk[sec == "userfossil" & year %in% rv$fyear:rv$lyear,]
        countryfossil = pacu[country ==mm & year %in% rv$fyear:rv$lyear, fossilcountry]
        datsj[,sec:="countryfossil"]
        datsj[,yy:=countryfossil]
        datsj[,label:=paste0(mm, " emissions")]
        
        
        datsjj = datsk[sec == "usercost" & year %in% rv$fyear:rv$lyear,]
        countrycost = pacu[country ==mm & year %in% rv$fyear:rv$lyear, countrycost]
        datsjj[,sec:="countrycost"]
        datsjj[, yy:=countrycost]
        datsjj[,label:=paste0(mm, " costs")]
        
        datsjjj = datsk[sec == "netcost" & year %in% rv$fyear:rv$lyear,]
        countrynetcost = pacu[country==mm & year %in% rv$fyear:rv$lyear, countrynetcost]
        datsjjj[,sec:="countrynetcost"]
        datsjjj[, yy:=countrynetcost]
        datsjjj[,label:=paste0(mm, " net costs")]
        
        
        datsjjjj = datsk[sec == "pop" & year %in% rv$fyear:rv$lyear,]
        countrypop = pacu[country==mm & year %in% rv$fyear:rv$lyear, pop]/1000000000
        # pop:=i.pop/1000000000, on=c("year")
        datsjjjj[,sec:="countrypop"]
        datsjjjj[, yy:=countrypop]
        datsjjjj[,label:=paste0(mm, " population")]
        
        # 
        
        
        datsy = rbind(datsj, datsjj, datsjjj, datsjjjj)
        
        datsy
      }
      rr3 = lapply(aat, dei2)
      dak = do.call(rbind, rr3)
      
      dats = rbind(dats,dak)
    }
    
    
    lux = as.data.table(lux())
    
    
    
    dats = dats[lux, col:=i.col ,on=c("sec")]
    dats = dats[lux, ala:=i.ala ,on=c("sec")]
    dats = dats[lux, pos:=i.pos ,on=c("sec")]
    # dats = dats[lux, label:=i.label ,on=c("sec")]
    dats = dats[lux, mark:=i.mark ,on=c("sec")]
    dats = dats[lux, le:=i.le, on=c("sec")]
    
    dats = dats[lux, visi:=i.visi, on=c("sec")]
    
    sini=1
    vihr = 1
    pink = 1
    
    #make dummy data that has zero visibility to not make graph error when nothing is selected
    dummy = dats[sec =="fossil",]
    dummy[, visi:=1]
    dummy[, yy:=0.0001]
    dummy[, sec:="dummy"]
    dummy[, ala:=0]
    
    
    
    dats = rbind(dats, dummy)
    
    
    valus =as.numeric(unique(dats[sec %in% c("avgcost", "usercost", "netcost", "avgnetcost", "dividend",  "countrycost", "countrynetcost"),max(yy, na.rm=TRUE)]))
    dats[sec %in% c("avgcost", "usercost", "netcost", "dividend", "avgnetcost", "countrycost", "countrynetcost"), tyy:= yy/(valus/100)]
    
    dats[sec == "dividend", tyy:=-1*tyy]
    dats$valuso = valus
    
    # dats[sec =="countryfossil", visi:=1]
    dats = dats[visi==1,]
    
    
    valus =as.numeric(unique(dats[sec %in% c("price"),max(yy, na.rm=TRUE)]))
    dats[sec %in% c("price"), tyy:= yy/(valus/100)]
    
    valus =as.numeric(unique(dats[sec %in% c("pop", "countrypop"),max(yy, na.rm=TRUE)]))
    dats[sec %in% c("pop", "countrypop"), tyy:= yy/(valus/100)]
    
    valus = as.numeric(unique(dats[sec %in% c("fossil", "land", "net"),max(yy, na.rm=TRUE)]))
    dats[sec %in% c("fossil", "land", "net", "dummy"), tyy:= yy/(valus/100)]
    
    dats[sec =="countryfossil", visi:=1]
    
    
    valusk = as.numeric(unique(dats[sec %in% c("userfossil", "avgfossil", "countryfossil"),max(yy,  na.rm=TRUE)]))
    dats[sec %in% c("userfossil", "avgfossil", "countryfossil"), tyy:= yy/(valusk/100)]
    
    
    dats = dats[order(pos)]
    
    dats =  dats[, pos :=1:.N, by=year]
    
    pink =1
    
    
    dats = as.data.table(dats)
    
  })
  
  datssst = reactive({
    tab = as.data.table(datsss())
    tab = tab[order(pos)]
    
    seclist = c("fossil", "land", "net", "pop", "avgfossil",
          "price", "avgcost","dividend","avgnetcost","userfossil",
          "usercost",  "netcost")
    tab = tab[sec %in% seclist,]
    
    
    # tab[, lyy := paste0(format(round(yy, 1, " ", mark)]
    tab[, lyy:=  as.character(paste0(sprintf(paste0("%0.", (le+1),"f"),round(yy,le+1))," ", mark))]
    tab = tab[,c("year","lyy", "label")]
    

    
    
  tab = as.data.frame(tab)
    tab = spread(tab, key="label",value="lyy")
    # tab = as.data.table(tab)
    lalist = c("year","Fossil emissions", "Land emissions / sinks", "Net emissions","World population",
               "Average emissions",  "Carbon price", "Average carbon costs",
               "Carbon dividend","Mean net costs", "User emissions", "User carbon costs", "User net costs"
    )
    
    
    # lst1 <- list(tab, tab)
    # lst1 <- lapply(lst1, function(x) setcolorder(x, intersect(lalist, names(x))))
    setcolorder(tab, intersect(lalist, names(tab)))
    
    tab = as.data.table(tab)
    tab = tab[order(year, decreasing = TRUE)]
    
    # tab = 
    # tab = 
                   # lst1 <- lapply(lst1, function(x) setcolorder(x, intersect(list_name, names(x)))
                                  
    # setcolorder(tab,lalist)
    
  tab
    
  })
  
  
  datsl = reactive ({
    
    datsl= as.data.table(datsss())
    datsl[,yearu := as.numeric(rv$yearc)]
    
    datsl = datsl[year <= yearu,]
    # & year >=rv$fyear
    datsl[, ayy:=mean(yy), by=c("sec")]
    
    datsl[year < rv$fyear & !(sec=="dummy"), ala := .1]
    
    datsl
    
  })
  
  datsc = reactive ({
    datsc= as.data.table(datsl())
    
    datsc[,yearu := as.numeric(rv$yearc)]
    
    datsc = datsc[year == yearu,]
    # pacuc = pacuc[year == yearu,]
  
    datsc[, labu:=  as.character(paste0(sprintf(paste0("%0.",le,"f"),round(yy,le))," ", mark, ", ", label, ", ", year))]  
    # paste0(sprintf(paste0("%0.",le,"f"),round(yy,le))," ", mark, ", ", label, ", ", year)
    
# THIS AWAY:     
    
     # ppaa = ppaa[pop2[var==3,],pop:=i.pop/1000000000, on=c("year")]
   
    
    
     
    datsc
    
  })
  
  datsff = reactive ({
    datsf= as.data.table(datsl())
    
    datsf[,yearu := as.numeric(rv$fyear)]
    
    
    datsf = datsf[year == yearu,]
    datsf
    
  })
  
  datsfp = reactive({
    
    datsfp=datsff()
    rv$seku = input$paa
    datsfp
    
  })
  datsf = reactive({
    datsf = datsff()
    if (rv$lastin %in% llist) {
      datsfp = datsfp()
      datsfp= as.data.table(datsfp)
      datsf = datsf[datsfp, lyy :=i.yy, on=c("sec", "year")]
    }  
    
    datsf
  })
  
  
  style <- reactive({
     # if (is.null(input$countr)) { 
       if (rv$alert4 ==TRUE) {
      # if (input$nok =="EXTRA: Country profiles") { 
         "#tablu2 {visibility: visible}"
         
    }
    else if (rv$alert4 ==FALSE) {
      "#tablu2 {visibility: collapse}"
      
    }
    
  }
  )
  
  output$css_style <- renderUI({
    tags$head(
      tags$style(
        HTML(
          paste0(c(style()), collapse = "\n")
        )
      )
    )})
  
  
  
  output$c1 <- renderUI({
    
    
  })
  
  output$c2 <- renderUI({
    tagList(9)
    
    
  })
  
  
  
  output$mobileuser = renderText({
    c("If you are mobile user, landscape rotation is recommended")
  })
  output$info = renderText({
    rv$info
  })
  
  output$tablz = renderDataTable(server=FALSE,{
    dats()},
    extensions = 'Buttons',
    
    options = list(
      pageLength = 2000, 
      scrollX=T,
      scrollY=T,
      
      paging = TRUE,
      searching = TRUE,
      fixedColumns = TRUE,
      autoWidth = TRUE,
      ordering = TRUE,
      dom = 'tB',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    )
    
  )
  
  output$tablx = renderDataTable(server=FALSE,{
    pacu()},
    extensions = 'Buttons',
    
    options = list(
      pageLength = 15, 
      scrollX=T,
      scrollY=T,
      
      paging = TRUE,
      searching = TRUE,
      fixedColumns = TRUE,
      autoWidth = TRUE,
      ordering = TRUE,
      dom = 'tB',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    )
  )
  
  output$tably = DT::renderDataTable(server=FALSE,{
    datssst()},
    extensions = 'Buttons',
    
    options = list(
      pageLength = 200, 
      scrollX=T,
      scrollY="400px",
      
      paging = TRUE,
      searching = TRUE,
      fixedColumns = TRUE,
      autoWidth = TRUE,
      ordering = TRUE,
      dom = 'tB',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    )
  )
  
  teksvari = "white"
    
  
  observeEvent(input$plot_hover, {
    rv$x = input$plot_hover$x
    rv$y = input$plot_hover$y
  })  
  
  ht = reactive(as.numeric(session$clientData$output_plot_width*.7))
  ht = reactive(
    
    function() {
      session$clientData$output_plot_width*.75
    }
  )
  
  
  sec = reactive({ 
    
    mi = min(min((datsss()[,tyy]), na.rm=T)*1.2,-50)
    ma = 115
    mix = 1972
    max = 2130
    
    # if (input$isMobile=="FALSE") {
    
    si = 
      function(per) {
        per*session$clientData$output_plot_width*session$clientData$pixelratio/500
      }
    # }
    if (input$isMobile=="FALSE") {
      
      lsi = 
        function(per) {
          per*session$clientData$output_plot_width*session$clientData$pixelratio/500
        }
    }
    
    if (input$isMobile=="TRUE") {
      
      lsi = 
        function(per) {
          per*session$clientData$output_plot_width*session$clientData$pixelratio/1500
        }
    }
    
    
    # if (input$is_mobile_device=="TRUE") {
    #   
    #   si = 
    #     function(per) {
    #       per*session$clientData$output_plot_width*session$clientData$pixelratio/2000
    #     }
    # }
    
    ly = 1991
    ala = .3
    dis = 16.8
    seg = .3
    scas = 2.5
    lines = .7
    lines2 =.2
    points=1.1
    segalfa=.7
    lee = 0
    fam = "saira"
    
    
    plot1=   ggplot(datsf())+
      
      geom_text(data = datsss()[year %in% c(seq(1970, 2100, 10)) & sec =="dummy",], aes(x=year, y=-35, label=c(year)),
                color="White", angle=0,size =si(2.4), alpha=.6) +
      
      geom_segment(data = datsss()[year %in% seq(1970, 2100, 10) & sec =="dummy",],
                   (aes(x=year, xend = year, y=100, yend=-30)), 
                   color="white", linetype="dashed", linewidth=lsi(.6), alpha=.1) +
      
      geom_segment(data=datsc()[sec=="dummy",],aes(x=1970, xend=rv$lyear, y=0, yend=0), color="white", linetype ="dashed",linewidth=lsi(seg)*2, alpha=.3) +
      geom_segment(data=da,aes(x=1960, xend=2100, y=0, yend=0), color="white", linetype ="dashed",linewidth=lsi(seg), alpha=0) +
      geom_segment(data=datsc()[sec=="dummy",],aes(x=1970, xend=rv$lyear, y=100, yend=100), color="white", linetype ="dashed",linewidth=lsi(seg), alpha=.2) +
      geom_segment(data=datsc()[sec=="dummy",],aes(x=1970, xend=rv$lyear, y=75, yend=75), color="white", linetype ="dashed",linewidth=lsi(seg), alpha=.2) +
      geom_segment(data=datsc()[sec=="dummy",],aes(x=1970, xend=rv$lyear, y=50, yend=50), color="white", linetype ="dashed",linewidth=lsi(seg), alpha=.2) +
      geom_segment(data=datsc()[sec=="dummy",],aes(x=1970, xend=rv$lyear, y=25, yend=25), color="white", linetype ="dashed",linewidth=lsi(seg), alpha=.2) +
      
      geom_point(aes(x=2030, y=105), alpha=0) +
      # with_outer_glow(
      #   geom_point(data=datsl()[sec=="price"], 
      #              aes(y=tyy, x=year, group=sec, color=col), size=si(lines)),
      #   colour = tax, sigma = 5,  expand = 5 ) +
      # 
      # with_outer_glow(
      #   geom_point(data=datsl()[sec=="fossil"], 
      #              aes(y=tyy, x=year, group=sec, color=col), size=si(lines)),
      #   colour = fos, sigma = 5,  expand = 5 ) +
    # 
    # with_outer_glow(
    #   geom_point(data=datsl()[sec=="usercost"], 
    #              aes(y=tyy, x=year, group=sec, color=col), size=si(lines)),
    #   colour = taxfosindi, sigma = 5,  expand = 5 ) +
    geom_area(data=datsl(), aes(y=tyy, x=year, group=sec,  fill=col),
              size=si(points), alpha=.35/nrow(datsc()), position = 'identity') + 
      
      geom_line(data=datsl(), aes(y=tyy, x=year, group=sec, color=col, alpha=ala), linewidth=lsi(lines)) + 
      geom_point(data=datsl(), aes(y=tyy, x=year, group=sec, color=col, alpha=ala), size=lsi(points)) + 
      
      
      
       geom_line(data=datsss(), aes(y=tyy, x=year, group=interaction(sec, country), color=col), linewidth=lsi(lines), alpha=.1) + 
      # geom_line(data=datss()[(sec =="avgcost"),], aes(y=-tyy, x=year, group=sec, color=col, alpha=ala), size=si(lines)) + 
      
      geom_point(data=datsss(), aes(y=tyy, x=year, group=sec, color=col, alpha=ala), size=si(points), alpha=.1) + 
      
      # tusercost+5
      geom_segment(data=da,aes(x=rv$yearc, xend=rv$yearc, y=100, yend = mi), color="white", alpha=.2, linewidth=lsi(1.4))+
      
      geom_point(data=da,aes(x=2140,  y=100), color="white", alpha=0, size=si(2))+
      
      geom_text(data=datsc()[, .SD[which.max(tyy)]], aes(x=year+2, y=max(tyy)+12, label=paste0("Year ",year, " values:")), 
                color="white", hjust=0, size=si(2.5), fontface="bold",  family = fam) +
      
      
    geom_label_repel(data=datsc(),
                     aes(x=year+3, y=tyy,
                         # label=paste0(sprintf(paste0("%0.",le,"f"),round(yy,le)," ", mark, ", ", label, ", ", year)),
                         label=paste0(round(yy,le)," ", mark, ", ", label),
                         
                         color=col, alpha=ala*100),
                     fill=bgc,
                     hjust=0, size=si(2.4), fontface="bold",
                     family = fam,
                     segment.size =NA,
                     direction = "y",
                     label.padding =0,
                     # box.padding=.1,
                     # nudge_x=18,
                     xlim=c(1970,2177),
                     label.size=0,
                     max.iter=5000,
                     force=.01, force_pull=10,box.padding=.1 ,
                     seed=5) +
      

      geom_segment(data=da, aes(x=rv$fyear, xend=rv$fyear, y=110, yend = mi),
                   color="white", linewidth=lsi(.4), linetype = "dashed", alpha=.2 ) +
      geom_segment(data=da, aes(x=rv$lyear, xend=rv$lyear, y=110, yend = mi),
                   color="white", linewidth=lsi(.4), linetype = "dashed", alpha=.2 ) +  
      
      
      geom_text(data=da,aes(x=rv$fyear, y=118), label =paste0("Start: ", rv$fyear),
                color="white", hjust=.5, size=si(2.5), fontface="bold") +
      
      geom_text(data=da,aes(x=rv$lyear, y=118), label =paste0("Neutrality: ", rv$lyear),
                color="white", hjust=.5, size=si(2.5), fontface="bold") +
      
      geom_text(data=da,aes(x=1970, y=97), label = paste0("Scale max"),
                col="white", fontface="bold" ,  size =si(2.5), hjust =0, vjust=0.5, angle=c(0)) +
      
      geom_text(data=da,aes(x=1970, y=2), label = paste0("0"),
                col="white", fontface="bold" ,  size =si(2.5), hjust =0, vjust=0, angle=c(0)) +
      
      geom_segment(data=da,aes(x=2021.5, y=110, xend=2021.5, yend=mi), 
                   color="lightgreen",linewidth=lsi(.6), linetype="solid", alpha=.3) +
      geom_text(data=da,aes(x=2020.5, y=-42), label = paste0("< Observed <"),
                col="lightgreen", fontface="bold" ,  size =si(2.5), hjust =1, vjust=0.5, angle=c(0), alpha=.4) +
      
      geom_text(data=da,aes(x=2022.5, y=-42), label = paste0("> Simulated >"),
                col="lightgreen", fontface="bold" ,  size =si(2.5), hjust =0, vjust=0.5, angle=c(0), alpha=.4) +
      
      geom_text(data=da,aes(x=1971, y=-14), label = paste0("Funded by the Kone foundation. \nData: UN, IPCC, Friedlingstein et al. 2021, World Bank"),
                col="white", fontface="bold" ,  size =si(1.3), hjust =0, vjust=0.5, angle=c(0), alpha=.5) +
      
       
      
      
     
      scale_color_identity() + 
      scale_alpha_identity() + 
      
      scale_fill_identity() + 
      
      scale_x_continuous(breaks = seq(1970, 2100, 10)) +
      coord_cartesian( ylim=c(mi, ma), xlim = c(mix, max),clip ="off") +
      
      
      theme(
        #  plot.margin = unit(c(mar,mar,mar,mar), "cm"),
        axis.title.x=element_blank(),
        plot.title=element_text(lineheight = 0.8, hjust=0.5, size=33, family="Alegreya Sans Bold"),
        plot.subtitle=element_text(lineheight = 0.8, size=20, hjust=0.5),
        # axis.text.x=element_text(size=20),
        axis.text.x=element_blank(),
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        # panel.grid.major.x = element_line(),
        
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        plot.background = element_rect(fill =bgc, color=NA), 
        panel.background = element_rect(fill = bgc ,  color=NA)
        #,
        #plot.margin = unit(c(mar,mar,mar,mar), "cm")
      )
    
  
    
    if (input$showfossil==TRUE | input$showland==TRUE  | input$shownet==TRUE) {
      
      plot1 = plot1+
        geom_text(data=datsss()[sec %in% c("fossil", "land", "net"), .SD[which.max(yy)]],
                  aes(x=1970, y=103, label=paste0(round(max(yy, na.rm=TRUE), 1), "Gt"), color=col), 
                  size=si(2.2), hjust=0, fontface="bold") 
      # geom_text(data=datsss()[sec %in% c("fossil", "land", "net"), ],
      #           aes(x=1970, y=96/2-1, label=paste0(round(max(yy, na.rm=TRUE)/2, 1), " Gt"), color=col), size=si(2.5), hjust=0)
      
    }    
    if (input$showpop==TRUE) {
      
      plot1 = plot1+
        geom_text(data=datsss()[sec %in% c("pop"),.SD[which.max(yy)] ],
                  aes(x=1981, y=103, label=paste0(round(max(yy, na.rm=TRUE), 2), "B"), color=col), 
                  size=si(2.2), hjust=0, fontface="bold") 
      # geom_text(data=datsss()[sec %in% c("price"), ],
      #           aes(x=1970, y=86/2-5, label=paste0(round(max(yy, na.rm=TRUE)/2, 0), " €/t"), color=col), size=si(2.5), hjust=0)
      
    }    
    # | input$showcountryfossil==T
    if (input$showavgfossil==TRUE | input$showuserfossil==TRUE) {
      
      plot1 = plot1+
        geom_text(data=datsss()[sec %in% c("userfossil", "avgfossil"), .SD[which.max(yy)]],
                  aes(x=1992, y=103, label=paste0(format(round(max(yy, na.rm=TRUE), 2), nsmall=2), "t"), color=col),
                  size=si(2.2), hjust=0, fontface="bold") 
      # geom_text(data=datsss()[sec %in% c("userfossil", "avgfossil"), ],
      #           aes(x=1970, y=91/2-3, label=paste0(format(round(max(yy, na.rm=TRUE)/2, 2), nsmall=2), " t"), color=col), size=si(2.5), hjust=0) 
    }
    
    if (input$showprice==TRUE) {
      
      plot1 = plot1+
        geom_text(data=datsss()[sec %in% c("price"), .SD[which.max(yy)]],
                  aes(x=2000, y=103, label=paste0(round(max(yy, na.rm=TRUE), 0), "€/t"), color=col),
                  size=si(2.2), hjust=0, fontface="bold") 
      # geom_text(data=datsss()[sec %in% c("price"), ],
      #           aes(x=1970, y=86/2-5, label=paste0(round(max(yy, na.rm=TRUE)/2, 0), " €/t"), color=col), size=si(2.5), hjust=0)
      
    }
    
    if (input$showusercost==TRUE | input$shownetcost==TRUE  | input$showavgcost==TRUE | input$showdividend==TRUE |  input$showavgnetcost==TRUE) {
      
      plot1 = plot1+
        geom_text(data=datsss()[sec %in% c("avgcost", "netcost", "usercost", "dividend", "avgnetcost"), .SD[which.max(yy)]],
                  aes(x=2011, y=103, label=paste0(round(max(valuso, na.rm=TRUE), 0), "€"), color=col), 
                  size=si(2.2), hjust=0, fontface="bold") 
        # geom_text(data=datsss()[sec %in% c("avgcost", "netcost", "usercost", "dividend", "avgnetcost"), ],
        #           aes(x=1970, y=79/2-7, label=paste0(round(max(valuso, na.rm=TRUE)/2, 0), " €"), color=col), size=si(2.5), hjust=0) 
    }

    if (input$startvalue ==TRUE & input$yearc > rv$fyear) {
      
      plot1 = plot1 +
        geom_text(data=datsf()[, .SD[which.max(tyy)]], aes(x=year-2, y=max(tyy)+12, label=paste0("Year ",year, " values:")), 
                  color="white", hjust=1, size=si(2.5), fontface="bold",  family = fam) +
        
        
        geom_label_repel(data=datsf(),
                         aes(x=year-2, y=tyy, label=paste0(label,"    ", sprintf(paste0("%0.",le,"f"),round(yy,le)), " ",mark),
                             color=col, group =sec, alpha=ala),
                         size=si(2.4), fontface="bold", hjust=1,
                         family = fam,
                         fill=bgc,
                         # segment.size =NA,
                         segment.size =NA,
                         direction = "y",
                         label.padding =0,
                         # box.padding=.1,
                         # nudge_x=18,
                         xlim=c(1970,2177),
                         label.size=0,
                         max.iter=5000,
                         force=.01, force_pull=10,box.padding=.1 ,
                         seed=5
        ) 
      
      
    } 


    

    

    
    
    #  if (is.null(input$countr)) {
    #   # if (exists(pacu())) {
    #   plot1 = plot1 }
    # else {
    #   plot1 = plot1 +
    # geom_line(data=datsss()[country %in% c(input$countr) & sec =="countryfossil",],
    #                                      aes(y=tyy,x=year, group=country), color=cpop, size=si(lines), alpha=.99)  
    # }
    
    
    plot1
    
    # print(plot1 )
  }) 
  
  
  
  # # , width=auto
  
  output$plot<-renderPlot({
    # req(input$accordion3)
    
    # req(input$alert)
    
    sec()
    
  } 
  
  ,height=
    # ht()
    function() {
      session$clientData$output_plot_width*.6
    }
  )
  
  
  
  
  
  
  output$splot <- renderUI({
    
    plotOutput("plot" 
               # ,height=paste0(huhu)
               ,width = "auto"
               # ,width = "1000px"
               ,height=session$clientData[["output_plot1_width"]]
               
               ,hover = "plot_hover"
    ) }  )
  
  
  
  
  sec2=reactive({
    
    sik = 2.5
    
    # look =     function (si) 
    {
      # geom_line(data=datsl, aes(y=tyy, x=year, group=sec, color=col, alpha=ala), size=si(lines)) + 
      #   geom_point(data=datsl, aes(y=tyy, x=year, group=sec, color=col, alpha=ala), size=si(points)) + 
      #   geom_line(data=datsss, aes(y=tyy, x=year, group=sec, color=col), size=si(lines), alpha=.1) + 
      #   geom_point(data=datsss, aes(y=tyy, x=year, group=sec, color=col, alpha=ala), size=si(points), alpha=.1) + 
      #   
      #   
      #   # tusercost+5
      #   geom_segment(data=da,aes(x=rv$yearc, xend=rv$yearc, y=100, yend = mi), color="white", alpha=.2, size=si(1.4))+
      #   geom_point(data=da,aes(x=2140,  y=100), color="white", alpha=0, size=si(2))+
      #   
      #   geom_text(data=datsc, aes(x=year+2, y=max(tyy)+12, label=paste0("Year ",year, ":")), color="white", hjust=0, size=si(2), fontface="bold") +
      #   geom_text_repel(data=datsc,
      #                   aes(x=year+3, y=tyy, label=paste0(format(round(yy,2),nsmall=2), "  ", mark," = ", label), color=col), 
      #                   hjust=0, size=si(2), fontface="bold", 
      #                   direction = "y", max.iter=5000, force=.5, force_pull=5,box.padding=.1 ) +
      #   
      #   
      #   geom_text_repel(data=datsf,
      #                   aes(x=year-1.5, y=tyy, label=paste0("\U2192",sprintf(paste0("%0.",le,"f"),round(yy,le))), color=col, group =sec), 
      #                   size=si(2), fontface="bold", hjust=1,
      #                   direction = "y", max.iter=5000, force=.5, force_pull=5,box.padding=0, seed=3
      #   ) +
      #   # geom_text(data=datsc, aes(x=ly+dis, y=-15, label=paste0("Averages ", rv$fyear, "-",  year,":")), color="white", hjust=1, size=si(2.5), fontface="bold") +
      #   # geom_text(data=datsl, aes(x=ly, y=-15-4.1*pos*mi/-55, label = label, alpha=ala, color=col), 
      #   #           hjust=1, size=si(1.2), fontface="bold") +
      #   # geom_text(data=datsl,
      #   #           aes(x=ly+dis, y=-15-4.1*pos*mi/-55, label=paste0(round(ayy,2), " ", mark), color=col), 
      #   #           hjust=1, size=si(1.1), fontface="bold") +
      #   geom_segment(data=da,aes(x=1970, xend=2100, y=100, yend=100), color="white", linetype ="dashed",size=si(seg), alpha=segalfa) +
      #   geom_segment(data=da,aes(x=1970, xend=2100, y=50, yend=50), color="white", linetype ="dashed",size=si(seg), alpha=segalfa) +
      #   
      #   geom_segment(data=da,aes(x=1970, xend=2100, y=0, yend=0), color="white", linetype ="dashed",size=si(seg)) +
      #   geom_segment(data=da,aes(x=1960, xend=2100, y=0, yend=0), color="white", linetype ="dashed",size=si(seg), alpha=0) +
      #   
      #   # geom_hline(aes(yintercept=0), color="white", size=si(seg), linetype ="dashed") +
      #   geom_segment(data=da,aes(x=2022.5, y=110, xend=2022.5, yend=mi), 
      #                color="white",size=si(.4), linetype="dashed", alpha=.5) +
      #   geom_segment(data=da, aes(x=rv$fyear, xend=rv$fyear, y=110, yend = mi),
      #                color="white", size=si(.4), linetype = "dashed", alpha=.5 ) +
      #   geom_segment(data=da, aes(x=rv$lyear, xend=rv$lyear, y=110, yend = mi),
      #                color="white", size=si(.4), linetype = "dashed", alpha=.5 ) +  
      #   
      #   
      #   geom_text(data=da,aes(x=rv$fyear, y=118), label =paste0("Start: ", rv$fyear),
      #             color="white", hjust=.5, size=si(2), fontface="bold") +
      #   
      #   geom_text(data=da,aes(x=rv$lyear, y=118), label =paste0("Neutrality: ", rv$lyear),
      #             color="white", hjust=.5, size=si(2), fontface="bold") +
      #   # geom_text(data=da,aes(x=1983, y=3), label = paste0(" Costs"),
      #   #           col="white", fontface="bold" ,  size =si(2.5), hjust =0, vjust=0, angle=c(0)) +
      #   # geom_text(data=da,aes(x=1983, y=-3), label = paste0(" Benefits"),
      #   #           col="white", fontface="bold" ,  size =si(2.5), hjust =0, vjust=1,  angle=c(0)) +
      #   # 
      #   # geom_text(data=da,aes(x=1982, y=8), label = paste0("\U2192", ""),
      #   #           col="white", fontface="bold" ,  size =si(3.5), hjust =1, vjust=0, 
      #   #           family = "Arial Unicode MS", angle=c(90)) +
      #   # 
      #   # geom_text(data=da,aes(x=1979, y=3), label = paste0(" Emissions"),
      #   #           col="white", fontface="bold" ,  size =si(2.5), hjust =1, vjust=0, angle=c(0)) +
      # # 
      # # 
      # # geom_text(data=da,aes(x=1983.5, y=-8), label = paste0("\U2192", ""),
      # #           col="white", fontface="bold" ,  size =si(3.5), hjust =1, vjust=1, 
      # #           family = "Arial Unicode MS", angle=c(270)) +
      # # 
      # # geom_text(data=da,aes(x=1979, y=-3), label = paste0(" Sinks"),
      # #           col="white", fontface="bold" ,  size =si(2.5), hjust =1, vjust=1,  angle=c(0)) +
      # 
      # scale_color_identity() + 
      #   scale_alpha_identity() + 
      #   scale_fill_identity() + 
      #   
      #   scale_x_continuous(breaks = c(2022,2040, 2060, 2080, 2100)) +
      #   coord_cartesian( ylim=c(mi, ma),  xlim = c(mix, max), clip ="off") +
      #   
      #   theme(
      #     #  plot.margin = unit(c(mar,mar,mar,mar), "cm"),
      #     axis.title.x=element_blank(),
      #     plot.title=element_text(lineheight = 0.8, hjust=0.5, size=33, family="Alegreya Sans Bold"),
      #     plot.subtitle=element_text(lineheight = 0.8, size=20, hjust=0.5),
      #     # axis.text.x=element_text(size=20),
      #     axis.text.x=element_blank(),
      #     panel.border = element_blank(), 
      #     panel.grid.major = element_blank(),
      #     panel.grid.minor = element_blank(), 
      #     axis.title.y=element_blank(),
      #     axis.text.y=element_blank(),
      #     plot.background = element_rect(fill =bgc, color=NA), 
      #     panel.background = element_rect(fill = bgc ,  color=NA)
      #     #,
      #     #plot.margin = unit(c(mar,mar,mar,mar), "cm")
      #   )
    }
    
    
    
    
    plotlist=NULL
    
    # plotlist = c(rv$plot2, rv$plot3, rv$plot4, rv$plot5)
    # plotlist = list(rv$plot2, rv$plot3, rv$plot4, rv$plot5)
    # plotlist = c("plot2", "plot3")
    
    if (rv$plot2 == "plot2") 
    {
      {  
        inplot= c("fossil", "land", "net")
        datsl = datsl()[sec %in% inplot,]
        datsss = datsss()[sec %in% inplot,]
        datsc =datsc()[sec %in% inplot,]
        datsf =datsf()[sec %in% inplot,]
        
        
        mi = min(min((datsss[,tyy]), na.rm=T)*1.2,-10)
        ma = 115
        mix = 2002
        max= 2155      
        
        
        # dats =dats()[sec %in% inplot,]
        
        si = 
          function(per) {
            per*session$clientData$output_plot_width/500
          }
        
        
        # valus = unique(dats()[, valus])
        
        ly = 1991
        ala = .3
        dis = 16.8
        seg = .1 
        scas = 2.5
        lines = .5
        lines2 =.13
        points=.7
        segalfa=.7
        # si= 
        # sip= function(per) {
        #   per*session$clientData$output_splot_width/5000
        # }
        
        
        plot2=   ggplot(datsss, aes(x=year))+
          
          geom_line(data=datsl, aes(y=tyy, x=year, group=sec, color=col, alpha=ala), size=si(lines)) + 
          geom_point(data=datsl, aes(y=tyy, x=year, group=sec, color=col, alpha=ala), size=si(points)) + 
          geom_line(data=datsss, aes(y=tyy, x=year, group=sec, color=col), size=si(lines), alpha=.1) + 
          geom_point(data=datsss, aes(y=tyy, x=year, group=sec, color=col, alpha=ala), size=si(points), alpha=.1) + 
          
          geom_area(data=datsl, aes(y=tyy, x=year, group=sec,  fill=col),
                    size=si(points), alpha=.25/nrow(datsc), position = 'identity') + 
          
          # tusercost+5
          geom_segment(data=da,aes(x=rv$yearc, xend=rv$yearc, y=100, yend = mi), color="white", alpha=.2, size=si(1.4))+
          geom_point(data=da,aes(x=2140,  y=100), color="white", alpha=0, size=si(2))+
          
          geom_text(data=datsc, aes(x=year+2, y=max(tyy)+12, label=paste0("Year ",year, " values:")), color="white", hjust=0, size=si(sik), fontface="bold") +
          geom_text_repel(data=datsc,
                          aes(x=year+3, y=tyy, label=paste0(sprintf(paste0("%0.",le,"f"),round(yy,le)), "", mark," ", label), color=col), 
                          hjust=0, size=si(sik), fontface="bold", 
                          direction = "y", max.iter=5000, force=.5, force_pull=5,box.padding=.1 ) +
          
          
          geom_text_repel(data=datsf,
                          aes(x=year-1.5, y=tyy, label=paste0(sprintf(paste0("%0.",le,"f"),round(yy,le))), color=col, group =sec), 
                          size=si(sik), fontface="bold", hjust=1,
                          direction = "y", max.iter=5000, force=.5, force_pull=5,box.padding=0, seed=3
          ) +
          # geom_text(data=datsc, aes(x=ly+dis, y=-15, label=paste0("Averages ", rv$fyear, "-",  year,":")), color="white", hjust=1, size=si(2.5), fontface="bold") +
          # geom_text(data=datsl, aes(x=ly, y=-15-4.1*pos*mi/-55, label = label, alpha=ala, color=col), 
          #           hjust=1, size=si(1.2), fontface="bold") +
          # geom_text(data=datsl,
          #           aes(x=ly+dis, y=-15-4.1*pos*mi/-55, label=paste0(round(ayy,2), " ", mark), color=col), 
          #           hjust=1, size=si(1.1), fontface="bold") +
          geom_segment(data=da,aes(x=1995, xend=2100, y=100, yend=100), color="white", linetype ="dashed",size=si(seg), alpha=segalfa) +
          geom_segment(data=da,aes(x=1995, xend=2100, y=50, yend=50), color="white", linetype ="dashed",size=si(seg), alpha=segalfa) +
          
          geom_segment(data=da,aes(x=1995, xend=2100, y=0, yend=0), color="white", linetype ="dashed",size=si(seg)*2) +
          geom_segment(data=da,aes(x=1999, xend=2100, y=0, yend=0), color="white", linetype ="dashed",size=si(seg), alpha=0) +
          
          # geom_hline(aes(yintercept=0), color="white", size=si(seg), linetype ="dashed") +
          geom_segment(data=da,aes(x=2022.5, y=110, xend=2022.5, yend=mi), 
                       color="white",size=si(.4), linetype="dashed", alpha=.5) +
          geom_segment(data=da, aes(x=rv$fyear, xend=rv$fyear, y=110, yend = mi),
                       color="white", size=si(.4), linetype = "dashed", alpha=.5 ) +
          geom_segment(data=da, aes(x=rv$lyear, xend=rv$lyear, y=110, yend = mi),
                       color="white", size=si(.4), linetype = "dashed", alpha=.5 ) +  
          
          
          geom_text(data=da,aes(x=rv$fyear, y=118), label =paste0("Start: ", rv$fyear),
                    color="white", hjust=.5, size=si(2), fontface="bold") +
          
          geom_text(data=da,aes(x=rv$lyear, y=118), label =paste0("Neutrality: ", rv$lyear),
                    color="white", hjust=.5, size=si(2), fontface="bold") +
          # geom_text(data=da,aes(x=1983, y=3), label = paste0(" Costs"),
          #           col="white", fontface="bold" ,  size =si(2.5), hjust =0, vjust=0, angle=c(0)) +
          # geom_text(data=da,aes(x=1983, y=-3), label = paste0(" Benefits"),
          #           col="white", fontface="bold" ,  size =si(2.5), hjust =0, vjust=1,  angle=c(0)) +
          # 
          # geom_text(data=da,aes(x=1982, y=8), label = paste0("\U2192", ""),
          #           col="white", fontface="bold" ,  size =si(3.5), hjust =1, vjust=0, 
          #           family = "Arial Unicode MS", angle=c(90)) +
          # 
          # geom_text(data=da,aes(x=1979, y=3), label = paste0(" Emissions"),
          #           col="white", fontface="bold" ,  size =si(2.5), hjust =1, vjust=0, angle=c(0)) +
        # 
        # 
        # geom_text(data=da,aes(x=1983.5, y=-8), label = paste0("\U2192", ""),
        #           col="white", fontface="bold" ,  size =si(3.5), hjust =1, vjust=1, 
        #           family = "Arial Unicode MS", angle=c(270)) +
        # 
        # geom_text(data=da,aes(x=1979, y=-3), label = paste0(" Sinks"),
        #           col="white", fontface="bold" ,  size =si(2.5), hjust =1, vjust=1,  angle=c(0)) +
        
        scale_color_identity() + 
          scale_alpha_identity() + 
          scale_fill_identity() + 
          
          scale_x_continuous(breaks = c(2022,2040, 2060, 2080, 2100)) +
          coord_cartesian( ylim=c(mi, ma),  xlim = c(mix, max), clip ="off") +
          
          theme(
            #  plot.margin = unit(c(mar,mar,mar,mar), "cm"),
            axis.title.x=element_blank(),
            plot.title=element_text(lineheight = 0.8, hjust=0.5, size=33, family="Alegreya Sans Bold"),
            plot.subtitle=element_text(lineheight = 0.8, size=20, hjust=0.5),
            # axis.text.x=element_text(size=20),
            axis.text.x=element_blank(),
            panel.border = element_blank(), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), 
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            plot.background = element_rect(fill =bgc,  color=teksvari), 
            panel.background = element_rect(fill = bgc,  color=NA)
            #,
            #plot.margin = unit(c(mar,mar,mar,mar), "cm")
          )
        
        
        
        
        
        if (input$showfossil==TRUE | input$showland==TRUE  | input$shownet==TRUE) {
          
          plot2 = plot2+
            geom_text(data=datsss[sec %in% c("fossil", "land", "net"), ],
                      aes(x=1992, y=96, label=paste0(round(max(yy, na.rm=TRUE), 1), " Gt"), color=teksvari), size=si(sik), hjust=0) +
            geom_text(data=datsss[sec %in% c("fossil", "land", "net"), ],
                      aes(x=1992, y=96/2-1, label=paste0(round(max(yy, na.rm=TRUE)/2, 1), " Gt"), color=teksvari), size=si(sik), hjust=0)
          
        }
        
        
        
      }
      # plotlist=list()
      gplot2= ggplotGrob(plot2)
      li2 = list(gplot2)
      # li2 = list(plot2)
      # plotlist = append(plotlist, li2)
      plotlist = c(li2)
      
      # plotlist= list(plot2)      
    }
    
    if (rv$plot3 == "plot3") {
      
      {
        
        
        inplot= c("avgfossil", "userfossil")
        datsl = datsl()[sec %in% inplot,]
        datsss = datsss()[sec %in% inplot & year > 2000,]
        datsc =datsc()[sec %in% inplot,]
        datsf =datsf()[sec %in% inplot,]
        
        mi = min(min((datsss[,tyy]), na.rm=T)*1.2,-10)
        ma = 115
        mix=2000
        max = 2150
        
        # dats =dats()[sec %in% inplot,]
        
        si = 
          function(per) {
            per*session$clientData$output_plot_width/500
          }
        
        
        ly = 1991
        ala = .3
        dis = 16.8
        seg = .2
        scas = 2.5
        lines = .5
        lines2 =.13
        points=.7
        segalfa=.7
        # si= 
        # sip= function(per) {
        #   per*session$clientData$output_splot_width/5000
        # }
        
        
        plot3=   ggplot(datsss, aes(x=year))+
          
          geom_line(data=datsl, aes(y=tyy, x=year, group=sec, color=col, alpha=ala), size=si(lines)) + 
          geom_point(data=datsl, aes(y=tyy, x=year, group=sec, color=col, alpha=ala), size=si(points)) + 
          geom_line(data=datsss, aes(y=tyy, x=year, group=sec, color=col), size=si(lines), alpha=.1) + 
          geom_point(data=datsss, aes(y=tyy, x=year, group=sec, color=col, alpha=ala), size=si(points), alpha=.1) + 
          
          geom_area(data=datsl, aes(y=tyy, x=year, group=sec,  fill=col),
                    size=si(points), alpha=.25/nrow(datsc), position = 'identity') + 
          
          # tusercost+5
          geom_segment(data=da,aes(x=rv$yearc, xend=rv$yearc, y=100, yend = mi), color="white", alpha=.2, size=si(1.4))+
          geom_point(data=da,aes(x=2140,  y=100), color="white", alpha=0, size=si(2))+
          
          geom_text(data=datsc, aes(x=year+2, y=max(tyy)+12, label=paste0("Year ",year, " values:")), color="white", hjust=0, size=si(2), fontface="bold") +
          geom_text_repel(data=datsc,
                          aes(x=year+3, y=tyy, label=paste0(sprintf(paste0("%0.",le,"f"),round(yy,le)), "", mark," ", label), color=col), 
                          hjust=0, size=si(2), fontface="bold", 
                          direction = "y", max.iter=5000, force=.5, force_pull=5,box.padding=.1 ) +
          
          
          geom_text_repel(data=datsf,
                          aes(x=year-1.5, y=tyy, label=paste0(sprintf(paste0("%0.",le,"f"),round(yy,le))), color=col, group =sec), 
                          size=si(2), fontface="bold", hjust=1,
                          direction = "y", max.iter=5000, force=.5, force_pull=5,box.padding=0, seed=3
          ) +
          # geom_text(data=datsc, aes(x=ly+dis, y=-15, label=paste0("Averages ", rv$fyear, "-",  year,":")), color="white", hjust=1, size=si(2.5), fontface="bold") +
          # geom_text(data=datsl, aes(x=ly, y=-15-4.1*pos*mi/-55, label = label, alpha=ala, color=col), 
          #           hjust=1, size=si(1.2), fontface="bold") +
          # geom_text(data=datsl,
          #           aes(x=ly+dis, y=-15-4.1*pos*mi/-55, label=paste0(round(ayy,2), " ", mark), color=col), 
          #           hjust=1, size=si(1.1), fontface="bold") +
          geom_segment(data=da,aes(x=1995, xend=2100, y=100, yend=100), color="white", linetype ="dashed",size=si(seg), alpha=segalfa) +
          geom_segment(data=da,aes(x=1995, xend=2100, y=50, yend=50), color="white", linetype ="dashed",size=si(seg), alpha=segalfa) +
          
          geom_segment(data=da,aes(x=1995, xend=2100, y=0, yend=0), color="white", linetype ="dashed",size=si(seg)*2) +
          geom_segment(data=da,aes(x=1999, xend=2100, y=0, yend=0), color="white", linetype ="dashed",size=si(seg), alpha=0) +
          
          # geom_hline(aes(yintercept=0), color="white", size=si(seg), linetype ="dashed") +
          geom_segment(data=da,aes(x=2022.5, y=110, xend=2022.5, yend=mi), 
                       color="white",size=si(.4), linetype="dashed", alpha=.5) +
          geom_segment(data=da, aes(x=rv$fyear, xend=rv$fyear, y=110, yend = mi),
                       color="white", size=si(.4), linetype = "dashed", alpha=.5 ) +
          geom_segment(data=da, aes(x=rv$lyear, xend=rv$lyear, y=110, yend = mi),
                       color="white", size=si(.4), linetype = "dashed", alpha=.5 ) +  
          
          
          geom_text(data=da,aes(x=rv$fyear, y=118), label =paste0("Start: ", rv$fyear),
                    color="white", hjust=.5, size=si(2), fontface="bold") +
          
          geom_text(data=da,aes(x=rv$lyear, y=118), label =paste0("Neutrality: ", rv$lyear),
                    color="white", hjust=.5, size=si(2), fontface="bold") +
          # geom_text(data=da,aes(x=1983, y=3), label = paste0(" Costs"),
          #           col="white", fontface="bold" ,  size =si(2.5), hjust =0, vjust=0, angle=c(0)) +
          # geom_text(data=da,aes(x=1983, y=-3), label = paste0(" Benefits"),
          #           col="white", fontface="bold" ,  size =si(2.5), hjust =0, vjust=1,  angle=c(0)) +
          # 
          # geom_text(data=da,aes(x=1982, y=8), label = paste0("\U2192", ""),
          #           col="white", fontface="bold" ,  size =si(3.5), hjust =1, vjust=0, 
          #           family = "Arial Unicode MS", angle=c(90)) +
          # 
          # geom_text(data=da,aes(x=1979, y=3), label = paste0(" Emissions"),
          #           col="white", fontface="bold" ,  size =si(2.5), hjust =1, vjust=0, angle=c(0)) +
        # 
        # 
        # geom_text(data=da,aes(x=1983.5, y=-8), label = paste0("\U2192", ""),
        #           col="white", fontface="bold" ,  size =si(3.5), hjust =1, vjust=1, 
        #           family = "Arial Unicode MS", angle=c(270)) +
        # 
        # geom_text(data=da,aes(x=1979, y=-3), label = paste0(" Sinks"),
        #           col="white", fontface="bold" ,  size =si(2.5), hjust =1, vjust=1,  angle=c(0)) +
        
        scale_color_identity() + 
          scale_alpha_identity() + 
          scale_fill_identity() + 
          
          scale_x_continuous(breaks = c(2022,2040, 2060, 2080, 2100)) +
          coord_cartesian( ylim=c(mi, ma),  xlim = c(mix, max), clip ="off") +
          
          theme(
            #  plot.margin = unit(c(mar,mar,mar,mar), "cm"),
            axis.title.x=element_blank(),
            plot.title=element_text(lineheight = 0.8, hjust=0.5, size=33, family="Alegreya Sans Bold"),
            plot.subtitle=element_text(lineheight = 0.8, size=20, hjust=0.5),
            # axis.text.x=element_text(size=20),
            axis.text.x=element_blank(),
            panel.border = element_blank(), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), 
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            plot.background = element_rect(fill =bgc, color=NA), 
            panel.background = element_rect(fill = bgc ,  color=NA)
            #,
            #plot.margin = unit(c(mar,mar,mar,mar), "cm")
          )
        
        
        
        
        
        
        if (input$showavgfossil==TRUE | input$showuserfossil==TRUE ) {
          
          plot3 = plot3+
            geom_text(data=datsss()[sec %in% c("userfossil", "avgfossil"), ],
                      aes(x=1995, y=96, label=paste0(format(round(max(yy, na.rm=TRUE), 2), nsmall=2), " t"), color=col), size=si(2.3), hjust=0) +
            geom_text(data=datsss()[sec %in% c("userfossil", "avgfossil"), ],
                      aes(x=1995, y=96/2-1, label=paste0(format(round(max(yy, na.rm=TRUE)/2, 2), nsmall=2), " t"), color=col), size=si(2.3), hjust=0) 
        }
        if (is.null(input$countr)) {
          # if (exists(pacu())) {
          plot3 = plot3 }
        else {
          plot3 = plot3 +
            geom_line(data=pacu()[country %in% c(input$countr),],
                      aes(y=tyy,x=year, group=country), color=cpop, size=si(lines), alpha=.99)  
        }  
        
        
        
      }
      gplot3= ggplotGrob(plot3)
      li3 = list(gplot3)
      # li3 = list(plot3)
      # plotlist = append(plotlist,li3)  
      plotlist = c(plotlist,li3)  
      
      # plotlist=list(plot3)      
    }
    if (rv$plot4 == "plot4") {
      {
        
        
        inplot= c("price")
        datsl = datsl()[sec %in% inplot,]
        datsss = datsss()[sec %in% inplot,]
        datsc =datsc()[sec %in% inplot,]
        datsf =datsf()[sec %in% inplot,]
        
        
        mi = min(min((datsss[,tyy]), na.rm=T)*1.2,-5)
        ma = 115
        mix=2000
        max = 2150     
        # dats =dats()[sec %in% inplot,]
        
        si = 
          function(per) {
            per*session$clientData$output_plot_width/500
          }
        
        
        ly = 1991
        ala = .3
        dis = 16.8
        seg = .2
        scas = 2.5
        lines = .5
        lines2 =.13
        points=.7
        segalfa=.7
        # si= 
        # sip= function(per) {
        #   per*session$clientData$output_splot_width/5000
        # }
        
        
        plot4=   ggplot(datsss, aes(x=year))+
          
          geom_line(data=datsl, aes(y=tyy, x=year, group=sec, color=col, alpha=ala), size=si(lines)) + 
          geom_point(data=datsl, aes(y=tyy, x=year, group=sec, color=col, alpha=ala), size=si(points)) + 
          geom_line(data=datsss, aes(y=tyy, x=year, group=sec, color=col), size=si(lines), alpha=.1) + 
          geom_point(data=datsss, aes(y=tyy, x=year, group=sec, color=col, alpha=ala), size=si(points), alpha=.1) + 
          
          geom_area(data=datsl, aes(y=tyy, x=year, group=sec,  fill=col),
                    size=si(points), alpha=.25/nrow(datsc), position = 'identity') + 
          
          # tusercost+5
          geom_segment(data=da,aes(x=rv$yearc, xend=rv$yearc, y=100, yend = mi), color="white", alpha=.2, size=si(1.4))+
          geom_point(data=da,aes(x=2140,  y=100), color="white", alpha=0, size=si(2))+
          
          geom_text(data=datsc, aes(x=year+2, y=max(tyy)+12, label=paste0("Year ",year, " values:")), color="white", hjust=0, size=si(2), fontface="bold") +
          geom_text_repel(data=datsc,
                          aes(x=year+3, y=tyy, label=paste0(sprintf(paste0("%0.",le,"f"),round(yy,le)), "", mark," ", label), color=col), 
                          hjust=0, size=si(2), fontface="bold", 
                          direction = "y", max.iter=5000, force=.5, force_pull=5,box.padding=.1 ) +
          
          
          geom_text_repel(data=datsf,
                          aes(x=year-1.5, y=tyy, label=paste0(sprintf(paste0("%0.",le,"f"),round(yy,le))), color=col, group =sec), 
                          size=si(2), fontface="bold", hjust=1,
                          direction = "y", max.iter=5000, force=.5, force_pull=5,box.padding=0, seed=3
          ) +
          # geom_text(data=datsc, aes(x=ly+dis, y=-15, label=paste0("Averages ", rv$fyear, "-",  year,":")), color="white", hjust=1, size=si(2.5), fontface="bold") +
          # geom_text(data=datsl, aes(x=ly, y=-15-4.1*pos*mi/-55, label = label, alpha=ala, color=col), 
          #           hjust=1, size=si(1.2), fontface="bold") +
          # geom_text(data=datsl,
          #           aes(x=ly+dis, y=-15-4.1*pos*mi/-55, label=paste0(round(ayy,2), " ", mark), color=col), 
          #           hjust=1, size=si(1.1), fontface="bold") +
          geom_segment(data=da,aes(x=1995, xend=2100, y=100, yend=100), color="white", linetype ="dashed",size=si(seg), alpha=segalfa) +
          geom_segment(data=da,aes(x=1995, xend=2100, y=50, yend=50), color="white", linetype ="dashed",size=si(seg), alpha=segalfa) +
          
          geom_segment(data=da,aes(x=1995, xend=2100, y=0, yend=0), color="white", linetype ="dashed",size=si(seg)*2) +
          geom_segment(data=da,aes(x=1999, xend=2100, y=0, yend=0), color="white", linetype ="dashed",size=si(seg), alpha=0) +
          
          # geom_hline(aes(yintercept=0), color="white", size=si(seg), linetype ="dashed") +
          geom_segment(data=da,aes(x=2022.5, y=110, xend=2022.5, yend=mi), 
                       color="white",size=si(.4), linetype="dashed", alpha=.5) +
          geom_segment(data=da, aes(x=rv$fyear, xend=rv$fyear, y=110, yend = mi),
                       color="white", size=si(.4), linetype = "dashed", alpha=.5 ) +
          geom_segment(data=da, aes(x=rv$lyear, xend=rv$lyear, y=110, yend = mi),
                       color="white", size=si(.4), linetype = "dashed", alpha=.5 ) +  
          
          
          geom_text(data=da,aes(x=rv$fyear, y=118), label =paste0("Start: ", rv$fyear),
                    color="white", hjust=.5, size=si(2), fontface="bold") +
          
          geom_text(data=da,aes(x=rv$lyear, y=118), label =paste0("Neutrality: ", rv$lyear),
                    color="white", hjust=.5, size=si(2), fontface="bold") +
          # geom_text(data=da,aes(x=1983, y=3), label = paste0(" Costs"),
          #           col="white", fontface="bold" ,  size =si(2.5), hjust =0, vjust=0, angle=c(0)) +
          # geom_text(data=da,aes(x=1983, y=-3), label = paste0(" Benefits"),
          #           col="white", fontface="bold" ,  size =si(2.5), hjust =0, vjust=1,  angle=c(0)) +
          # 
          # geom_text(data=da,aes(x=1982, y=8), label = paste0("\U2192", ""),
          #           col="white", fontface="bold" ,  size =si(3.5), hjust =1, vjust=0, 
          #           family = "Arial Unicode MS", angle=c(90)) +
          # 
          # geom_text(data=da,aes(x=1979, y=3), label = paste0(" Emissions"),
          #           col="white", fontface="bold" ,  size =si(2.5), hjust =1, vjust=0, angle=c(0)) +
        # 
        # 
        # geom_text(data=da,aes(x=1983.5, y=-8), label = paste0("\U2192", ""),
        #           col="white", fontface="bold" ,  size =si(3.5), hjust =1, vjust=1, 
        #           family = "Arial Unicode MS", angle=c(270)) +
        # 
        # geom_text(data=da,aes(x=1979, y=-3), label = paste0(" Sinks"),
        #           col="white", fontface="bold" ,  size =si(2.5), hjust =1, vjust=1,  angle=c(0)) +
        
        
        
        scale_color_identity() + 
          scale_alpha_identity() + 
          scale_fill_identity() + 
          
          scale_x_continuous(breaks = c(2022,2040, 2060, 2080, 2100)) +
          coord_cartesian( ylim=c(mi, ma),  xlim = c(mix, max), clip ="off") +
          
          theme(
            #  plot.margin = unit(c(mar,mar,mar,mar), "cm"),
            axis.title.x=element_blank(),
            plot.title=element_text(lineheight = 0.8, hjust=0.5, size=33, family="Alegreya Sans Bold"),
            plot.subtitle=element_text(lineheight = 0.8, size=20, hjust=0.5),
            # axis.text.x=element_text(size=20),
            axis.text.x=element_blank(),
            panel.border = element_blank(), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), 
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            plot.background = element_rect(fill =bgc, color=NA), 
            panel.background = element_rect(fill = bgc ,  color=NA)
            #,
            #plot.margin = unit(c(mar,mar,mar,mar), "cm")
          )
        
        
        
        
        if (input$showprice==TRUE) {
          
          plot4 = plot4+
            geom_text(data=datsss[sec %in% c("price"), ],
                      aes(x=1994, y=96, label=paste0(round(max(yy, na.rm=TRUE), 0), " €/t"), color=col), size=si(2.3), hjust=0) +
            geom_text(data=datsss[sec %in% c("price"), ],
                      aes(x=1994, y=96/2-1, label=paste0(round(max(yy, na.rm=TRUE)/2, 0), " €/t"), color=col), size=si(2.3), hjust=0)
          
        }
        
        
        
      }
      gplot4= ggplotGrob(plot4)
      li4 = list(gplot4)
      # li4 = list(plot4)
      # plotlist = append(plotlist,li4)
      plotlist = c(plotlist,li4)
      
    }
    
    if (rv$plot5 == "plot5") {
      {
        
        
        inplot= c("avgcost", "dividend", "avgnetcost", "usercost", "netcost")
        datsl = datsl()[sec %in% inplot,]
        datsss = datsss()[sec %in% inplot,]
        datsc =datsc()[sec %in% inplot,]
        datsf =datsf()[sec %in% inplot,]
        
        
        mi = min(min((datsss[,tyy]), na.rm=T)*1.2,-55)
        ma = 115
        
        mix=2000
        max = 2150      
        # dats =dats()[sec %in% inplot,]
        
        si = 
          function(per) {
            per*session$clientData$output_plot_width/500
          }
        
        # lu[sec == "fossil", pos:=1]
        # lu[sec == "land", pos:=2]
        # lu[sec == "net", pos:=3]
        # lu[sec == "pop", pos:=4]
        # 
        # lu[sec == "avgfossil", pos:=5]
        # lu[sec == "price", pos:=6]
        # lu[sec == "avgcost", pos:=7]
        # lu[sec == "dividend", pos:=8]
        # lu[sec == "avgnetcost", pos:=9]
        # lu[sec == "userfossil", pos:=10]
        # lu[sec == "usercost", pos:=11]
        # lu[sec == "netcost", pos:=12]
        
        # valus = unique(dats()[, valus])
        
        # valus = unique(dats()[, valus]) 
        # valus = unique(dats()[, valus])
        
        ly = 1991
        ala = .3
        dis = 16.8
        seg = .2
        scas = 2.5
        lines = .5
        lines2 =.13
        points=.7
        segalfa=.7
        # si= 
        # sip= function(per) {
        #   per*session$clientData$output_splot_width/5000
        # }
        
        
        plot5=   ggplot(datsss, aes(x=year))+
          
          geom_line(data=datsl, aes(y=tyy, x=year, group=sec, color=col, alpha=ala), size=si(lines)) + 
          geom_point(data=datsl, aes(y=tyy, x=year, group=sec, color=col, alpha=ala), size=si(points)) + 
          geom_line(data=datsss, aes(y=tyy, x=year, group=sec, color=col), size=si(lines), alpha=.1) + 
          geom_point(data=datsss, aes(y=tyy, x=year, group=sec, color=col, alpha=ala), size=si(points), alpha=.1) + 
          
          geom_area(data=datsl, aes(y=tyy, x=year, group=sec,  fill=col),
                    size=si(points), alpha=.25/nrow(datsc), position = 'identity') + 
          
          # tusercost+5
          geom_segment(data=da,aes(x=rv$yearc, xend=rv$yearc, y=100, yend = mi), color="white", alpha=.2, size=si(1.4))+
          geom_point(data=da,aes(x=2140,  y=100), color="white", alpha=0, size=si(2))+
          
          geom_text(data=datsc, aes(x=year+2, y=max(tyy)+12, label=paste0("Year ",year, " values:")), color="white", hjust=0, size=si(2), fontface="bold") +
          geom_text_repel(data=datsc,
                          aes(x=year+3, y=tyy, label=paste0(sprintf(paste0("%0.",le,"f"),round(yy,le)), "", mark," ", label), color=col), 
                          hjust=0, size=si(2), fontface="bold", 
                          direction = "y", max.iter=5000, force=.5, force_pull=5,box.padding=.1 ) +
          
          
          geom_text_repel(data=datsf,
                          aes(x=year-1.5, y=tyy, label=paste0(sprintf(paste0("%0.",le,"f"),round(yy,le))), color=col, group =sec), 
                          size=si(2), fontface="bold", hjust=1,
                          direction = "y", max.iter=5000, force=.5, force_pull=5,box.padding=0, seed=3
          ) +
          # geom_text(data=datsc, aes(x=ly+dis, y=-15, label=paste0("Averages ", rv$fyear, "-",  year,":")), color="white", hjust=1, size=si(2.5), fontface="bold") +
          # geom_text(data=datsl, aes(x=ly, y=-15-4.1*pos*mi/-55, label = label, alpha=ala, color=col), 
          #           hjust=1, size=si(1.2), fontface="bold") +
          # geom_text(data=datsl,
          #           aes(x=ly+dis, y=-15-4.1*pos*mi/-55, label=paste0(round(ayy,2), " ", mark), color=col), 
          #           hjust=1, size=si(1.1), fontface="bold") +
          geom_segment(data=da,aes(x=1995, xend=2100, y=100, yend=100), color="white", linetype ="dashed",size=si(seg), alpha=segalfa) +
          geom_segment(data=da,aes(x=1995, xend=2100, y=50, yend=50), color="white", linetype ="dashed",size=si(seg), alpha=segalfa) +
          
          geom_segment(data=da,aes(x=1995, xend=2100, y=0, yend=0), color="white", linetype ="dashed",size=si(seg)*2) +
          geom_segment(data=da,aes(x=1999, xend=2100, y=0, yend=0), color="white", linetype ="dashed",size=si(seg), alpha=0) +
          
          # geom_hline(aes(yintercept=0), color="white", size=si(seg), linetype ="dashed") +
          geom_segment(data=da,aes(x=2022.5, y=110, xend=2022.5, yend=mi), 
                       color="white",size=si(.4), linetype="dashed", alpha=.5) +
          geom_segment(data=da, aes(x=rv$fyear, xend=rv$fyear, y=110, yend = mi),
                       color="white", size=si(.4), linetype = "dashed", alpha=.5 ) +
          geom_segment(data=da, aes(x=rv$lyear, xend=rv$lyear, y=110, yend = mi),
                       color="white", size=si(.4), linetype = "dashed", alpha=.5 ) +  
          
          
          geom_text(data=da,aes(x=rv$fyear, y=118), label =paste0("Start: ", rv$fyear),
                    color="white", hjust=.5, size=si(2), fontface="bold") +
          
          geom_text(data=da,aes(x=rv$lyear, y=118), label =paste0("Neutrality: ", rv$lyear),
                    color="white", hjust=.5, size=si(2), fontface="bold") +
          # geom_text(data=da,aes(x=1983, y=3), label = paste0(" Costs"),
          #           col="white", fontface="bold" ,  size =si(2.5), hjust =0, vjust=0, angle=c(0)) +
          # geom_text(data=da,aes(x=1983, y=-3), label = paste0(" Benefits"),
          #           col="white", fontface="bold" ,  size =si(2.5), hjust =0, vjust=1,  angle=c(0)) +
          # 
          # geom_text(data=da,aes(x=1982, y=8), label = paste0("\U2192", ""),
          #           col="white", fontface="bold" ,  size =si(3.5), hjust =1, vjust=0, 
          #           family = "Arial Unicode MS", angle=c(90)) +
          # 
          # geom_text(data=da,aes(x=1979, y=3), label = paste0(" Emissions"),
          #           col="white", fontface="bold" ,  size =si(2.5), hjust =1, vjust=0, angle=c(0)) +
        # 
        # 
        # geom_text(data=da,aes(x=1983.5, y=-8), label = paste0("\U2192", ""),
        #           col="white", fontface="bold" ,  size =si(3.5), hjust =1, vjust=1, 
        #           family = "Arial Unicode MS", angle=c(270)) +
        # 
        # geom_text(data=da,aes(x=1979, y=-3), label = paste0(" Sinks"),
        #           col="white", fontface="bold" ,  size =si(2.5), hjust =1, vjust=1,  angle=c(0)) +
        
        scale_color_identity() + 
          scale_alpha_identity() + 
          scale_fill_identity() + 
          
          scale_x_continuous(breaks = c(2022,2040, 2060, 2080, 2100)) +
          coord_cartesian( ylim=c(mi, ma),  xlim = c(mix, max), clip ="off") +
          
          theme(
            #  plot.margin = unit(c(mar,mar,mar,mar), "cm"),
            axis.title.x=element_blank(),
            plot.title=element_text(lineheight = 0.8, hjust=0.5, size=33, family="Alegreya Sans Bold"),
            plot.subtitle=element_text(lineheight = 0.8, size=20, hjust=0.5),
            # axis.text.x=element_text(size=20),
            axis.text.x=element_blank(),
            panel.border = element_blank(), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), 
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            plot.background = element_rect(fill =bgc, color=NA), 
            panel.background = element_rect(fill = bgc ,  color=NA)
            #,
            #plot.margin = unit(c(mar,mar,mar,mar), "cm")
          )
        
        
        
        if (input$showusercost==TRUE | input$shownetcost==TRUE  | input$showavgcost==TRUE | input$showdividend==TRUE |  input$showavgnetcost==TRUE) {
          
          plot5 = plot5+
            geom_text(data=datsss[sec %in% c("avgcost", "netcost", "usercost", "dividend", "avgnetcost"), ],
                      aes(x=1995, y=96, label=paste0(round(max(valuso, na.rm=TRUE), 0), " €"), color=col), size=si(2.3), hjust=0) +
            geom_text(data=datsss[sec %in% c("avgcost", "netcost", "usercost", "dividend", "avgnetcost"), ],
                      aes(x=1995, y=96/2-1, label=paste0(round(max(valuso, na.rm=TRUE)/2, 0), " €"), color=col), size=si(2.3), hjust=0) 
        }
        
        
      } 
      gplot5= ggplotGrob(plot5)
      li5 = list(gplot5)
      # plotlist = append(plotlist,li5)
      plotlist = c(plotlist,li5)
      
    }
    
    eplot = ggplot(NULL, aes(color = ""))+
      geom_blank()+
      scale_color_manual(values = "black", labels = "Something")+
      guides(color = guide_legend())+
      theme(
        plot.background = element_rect(fill =bgc, color=NA), 
        panel.background = element_rect(fill = bgc ,  color=NA)
      )
    geplot = ggplotGrob(eplot)
    lig = list(geplot)
    
    pll = length(plotlist)
    # 
    
    # if (pll >= 3) { 
    
    if (pll==1){
      lay=rbind(c(1))
      plotlist = c(plotlist)
      
    } else if (pll ==2) {
      lay = rbind(c(1,2))
      plotlist = c(plotlist)
    } else if (pll==3) {
      lay = rbind(c(1,2), c(3,4))
      plotlist = c(plotlist, lig)
      
    } else if (pll==4) {
      lay = rbind(c(1,2), c(3,4))
    }
    
    rv$pll = pll
    # }
    # else {
    #   
    #   if (pll==1){
    #     lay=rbind(c(1))
    #   } else if (pll ==2) {
    #     lay = rbind(c(1,2), c(3, 4))
    #     plotlist = c(plotlist, lig, lig)
    #   } else if (pll==3) {
    #     lay = rbind(c(1,2), c(3,4))
    #     plotlist = c(plotlist, lig)
    #     
    #   } else if (pll==4) {
    #     lay = rbind(c(1,2), c(3,4))
    #   }   
    #   
    #   
    # }
    
    # plotlist = list(plot2, plot3)
    # plotx = ggarrange(plotlist=plotlist, ncol=2, legend="none" )
    plotx = grid.arrange(grobs=plotlist, layout_matrix=lay)
    
    plotx
    
  })
  
  
  # old separate graphs
  { 
    
    
  }
  
  
  
  output$plots<-renderPlot({
    sec2()
  } 
  # ,width = "1000px"
  
  ,height=
    function() {
      
      # if (rv$pll <=2){ 
      # session$clientData$output_plots_width*.3
      # } else {
      session$clientData$output_plots_width*.6
      
      # }
    }
  )
  
  
  
  output$splot2 <- renderUI({
    
    plotOutput("plots" 
               # ,height=paste0(huhu)
               # ht()
               # width = "1000px"
               
               ,width = "auto"
               
               ,height=session$clientData[["output_plots_width"]]
               
               ,hover = "plot_hover"
    ) }  )
  
  # 
  
  #   
  
  output$yearc = renderUI({
    
    
    # args       <- list(inputId="yearc", label="slider :", ticks=c(1945:2130), value=c(2100), step=1)
    # 
    # args$min   <- 1970
    # args$max   <- 2100
    # args$width = "100%"
    # # args$step   <- 1
    # 
    # if (sessionInfo()$otherPkgs$shiny$Version>="0.11") {
    #   # this part works with shiny 1.5.0
    #   ticks <- paste0(args$ticks, collapse=',')
    #   args$ticks <- T
    #   html  <- do.call('sliderInput', args)
    #   vale = paste0(c(1970, 1990, 2010, 2100), collapse = ',')
    #   html$children[[1]]$attribs[['data-values']] <- vale;
    # 
    # } else {
    #   html  <- do.call('sliderInput', args)
    # }
    # # fluidRow(
    # #   # if (input$view ==1) {
    # #   style =  "margin-left: 0vw;
    # #           margin-top: -4.5vw; ",
    # html

    
    # sliderTextInput(
    #   inputId = "yearc",
    #   label = "Choose a value:", 
    #   choices = c(1970, 2100),
    #   grid = TRUE
      # ,
      # min = 1965,
      # max = 2138
    # )
    # )
    # sliderInput2 <- function(inputId, label, min, max, value, step=NULL, from_min, from_max){
    #
    # sliderInput2("slider", "Slide:",
    #              min = 0, max = 100, value = 50, step = 5, from_min = 20, from_max = 80
    # )
    if (input$view ==1) {
      fluidRow(
        # if (input$view ==1) {
        style =  "margin-left: 0vw;
              margin-top: -4.5vw; ",

        # margin-top: -4.5vw; "

        sliderInput("yearc", label=NULL,min = 1965, max = 2137,step=1,value=c(2100),
                    # from_min = 2023, from_max = 2100,
                    width="100%"
                     , animate=TRUE,
                    sep ="",
                    ticks=F
        )
      )
    }  else {
      fluidRow(

        style =  "margin-left: 0vw;
              margin-top: -30vw; ",

        # margin-top: -4.5vw; "

        sliderInput("yearc", label=NULL,min = 1993, max = 2155,step=1,value=c(2100),
                    # from_min = 2023, from_max = 2100,
                    width="50%"
                    # , animate=TRUE
                    # )
        )
      )

    }
    
    
    
    
  })
  
  # plotOutput("plot3"
  #            # ,height=paste0(huhu)
  #            # ht()
  #            ,height=session$clientData[["output_plot3_width"]]
  # 
  #            ,hover = "plot_hover"
  # ) }  )
  
}
# )
# shinyApp(ui = ui, server = server)
