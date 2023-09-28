

server <- function(input,output, session) {
  
  output$testy= renderText("testyy")
  output$pul= renderText(rv$luk)
  output$xx= renderText(rv$x)
  output$yy= renderText(rv$y)
   output$cor= renderText(rv$cor)
   # output$couexp= renderText(rv$coux)
   
   output$couexp = renderUI({
     rv$coux
   })
  # observe({
  # })
  # output$last= renderText({input$last_btn})
  output$last= renderText({input$changed})
  output$dimension_display <- renderText({
    paste(input$dim[1], input$dim[2], input$dim[2]/input$dim[1])
  })
  
  observeEvent(rv$yearc, {
    req(datsss())
    
  })
  
  observeEvent(input$view, {
    if (input$view ==3) {
      # req(datsss())
    }
  })
  
  lapply(
    X = c("bud", "vuo", "paa", "muo", "nonco2", "nonco2end", "fstart", "lstart", "nonco2start", "popc", "sprice", "eprice",
          "pri", "indi1", "indi2", "muoindi", "indi", "con", "national", "nationalcoun", "countr", "conb"),
    FUN = function(j){
      onBookmark(function(state) {
        state$values[[paste0(j)]] <- input[[paste0(j)]]
      })   
    }
  )
  
  
  # lapply(
  #   X = c("bud", "vuo", "paa", "muo", "nonco2", "nonco2end", "fstart", "lstart", "nonco2start", "popc", "sprice", "eprice",
  #         "pri", "indi1", "indi2", "muoindi", "indi", "con", "national", "nationalcoun", "countr", "conb"),
  #   FUN = function(j){
  #     
  #     
  #     
  #     onRestored(function(state) {
  #       
  #       
  #       # tiem = state$values$tiem
  #       
  #       # lek[[paste0(j)]] =  state$values[[paste0(j)]]
  #       shinyjs::delay(1000, {
  #         updateSliderInput(
  #           session = session,
  #           inputId =j,
  #           value = state$values[[paste0(j)]]
  #           
  #         )
  #         
  #         
  #         # input[[paste0(j)]] <- state$input[[paste0(j)]]
  #       })
  #       %>% 
  #       
  #     }
  #     )
  #     # })
  #   })
  # 
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
  
  
  output$lets <- renderUI({
    HTML("<p> Displaying greek letter delta as a symbol:<br> \u0394 
         </p>")
  })
  
  output$selitys2 = renderText({
    
    "In progress, finished in 2023. Tool to explore various scenarios for global carbon
    price/dividend. Funded by the Kone foundation. www.villeseppala.fi. Data: UN, IPCC, Friedlingstein et al. 2021, World Bank"
    
  })
  
  # output$selitys = renderText({
  #   
  #   switch(input$lang, "eng"="In progress, finished in February 2023. Tool to explore various global carbon
  #   tax/dividend scenarios by adjusting the assumed population progression, the rate of emission
  #   reductions and the level of carbon tax. Funded by the Kone foundation. www.villeseppala.fi. Data: UN, IPCC, Friedlingstein et al. 2021, World Bank",
  #   "fin"="Kesken, valmistuu helmikuussa 2023. Työkalulla voi hahmotella erilaisia hiilen hinnoittelu ja hiiliosinko
  #          -skenaarioita säätämällä oletettua väestökehitystä, päästövähennyksiä ja hiiliveroa. Koneen säätiön rahoittama hanke. www.villeseppala.fi. Data: UN, IPCC, Friedlingstein et al. 2021, World Bank")
  #   # )
  # })
  # 
  
  
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
  
  
  # rv$avgfossil= datsc[sec =="avgfossil", yy]
  # rv$price= datsc[sec =="price", yy]
  # rv$fossil= datsc[sec =="fossil", yy]
  # rv$userfossil= datsc[sec =="userfossil", yy]
  # rv$avgcost= datsc[sec =="avgcost", yy]
  # rv$usercost= datsc[sec =="usercost", yy]
  # rv$usernetcost= datsc[sec =="netcost", yy]
  # rv <- reactiveValues(trig =0)
  rv <- reactiveValues(trigu =0)
  rv <- reactiveValues(triggo =0)
  rv <- reactiveValues(triggor =0)
  
  
  rv <- reactiveValues(disctext =0)
  rv <- reactiveValues(lihh =.99)
  rv <- reactiveValues(lok =NULL)
  rv <- reactiveValues(landplus =NULL)
  rv <- reactiveValues(view =NULL)
  rv <- reactiveValues(autodraw =FALSE)
  rv <- reactiveValues(wii =NULL)
  rv <- reactiveValues(lastt =NULL)
   rv <- reactiveValues(x =NULL)
   rv <- reactiveValues(lurk =NULL)
   
  rv <- reactiveValues(avgfossil = NULL)
  rv <- reactiveValues(pop = NULL)
  rv <- reactiveValues(fossill = NULL)
  rv <- reactiveValues(landl = NULL)
  rv <- reactiveValues(netl = NULL)
  rv <- reactiveValues(ghgl = NULL)
  rv <- reactiveValues(nonco2l = NULL)
  rv <- reactiveValues(popl = NULL)
  rv <- reactiveValues(avgfossill = NULL)
  rv <- reactiveValues(pricel = NULL)
  rv <- reactiveValues(avgcostl = NULL)
  rv <- reactiveValues(dividendl = NULL)
  rv <- reactiveValues(avgnetcostl = NULL)
  rv <- reactiveValues(userfossill = NULL)
  rv <- reactiveValues(usercostl = NULL)
  rv <- reactiveValues(netcostl = NULL)
  rv <- reactiveValues(countryfossill = NULL)
  rv <- reactiveValues(countrycostl = NULL)
  rv <- reactiveValues(countynetcostl = NULL)
  rv <- reactiveValues(countrypopl = NULL)
  rv <- reactiveValues(countrydividendl = NULL)
  rv <- reactiveValues(averagedividendl = NULL)
  rv <- reactiveValues(usernatl = NULL)
  rv <- reactiveValues(nationall = 0)
  
  # rv$usernatl
  # rv <- reactiveValues(l = NULL)
  
  rv <- reactiveValues(price = NULL)
  rv <- reactiveValues(fossil = NULL)
  rv <- reactiveValues(userfossil = NULL)
  rv <- reactiveValues(avgcost = NULL)
  rv <- reactiveValues(usercost = NULL)
  rv <- reactiveValues(usernetcost = NULL)
  
  
  
  rv <- reactiveValues(lek = NULL)
  
  rv <- reactiveValues(lang = NULL)
  rv <- reactiveValues(rvtotal= NULL)
  rv <- reactiveValues(rvlbudget= NULL)
  rv <- reactiveValues(rvlsum= NULL)
  rv <- reactiveValues(rvtotals= NULL)
  rv <- reactiveValues(fosm= NULL)
  
  rv <- reactiveValues(budget = NULL)
  rv <- reactiveValues(sumnet = NULL)
  rv <- reactiveValues(afterbudget = NULL)
  
  rv <- reactiveValues(total = NULL)
  rv <- reactiveValues(lyear = NULL)
  rv <- reactiveValues(fyear = NULL)
  rv <- reactiveValues(time = NULL)
  rv <- reactiveValues(year = NULL)
  # rv <- reactiveValues(x = NULL)
  rv <- reactiveValues(y = NULL)
  rv <- reactiveValues(paa = NULL)
  rv <- reactiveValues(prate = NULL)
  rv <- reactiveValues(pgrowth = NULL)
  rv <- reactiveValues(valu = NULL)
  rv <- reactiveValues(ale = 0)
  rv <- reactiveValues(alert4 = FALSE)
  rv <- reactiveValues(alert6 = FALSE)
  rv <- reactiveValues(alert8 = FALSE)
  rv <- reactiveValues(alert10 = FALSE)
  
  rv <- reactiveValues(yearc = NULL)
  rv <- reactiveValues(lastyear = NULL)
  
  rv <- reactiveValues(popc = 2)
  rv <- reactiveValues(showpop = TRUE)
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
  
  rv <- reactiveValues(showghg = FALSE)
  rv <- reactiveValues(shownonco2 = FALSE)
  rv <- reactiveValues(shownewsink = FALSE)
  rv <- reactiveValues(showlandcost = FALSE)
  
  rv <- reactiveValues(showavgfossil = TRUE)
  rv <- reactiveValues(showcountryfossil = FALSE)
  rv <- reactiveValues(showcountrycost = FALSE)
  rv <- reactiveValues(showcountrynetcost = FALSE)
  rv <- reactiveValues(showcountrypop = FALSE)
  rv <- reactiveValues(averagedividend = FALSE)
  rv <- reactiveValues(countrydividend = FALSE)
  
  rv <- reactiveValues(warn = NULL)
  
  rv <- reactiveValues(plot2 = NULL)
  rv <- reactiveValues(plot3 = NULL)
  rv <- reactiveValues(plot4 = NULL)
  rv <- reactiveValues(plot5 = NULL)
  rv <- reactiveValues(plot6 = NULL)
  
  rv <- reactiveValues(lastin = NULL)
  rv <- reactiveValues(info = NULL)
  rv <- reactiveValues(infot = NULL)
  
  rv <- reactiveValues(infostartpricetext = NULL)
  rv <- reactiveValues(infoendpricetext = NULL)
  rv <- reactiveValues(infofossil = NULL)
  rv <- reactiveValues(labelfossil = NULL)
  
  rv <- reactiveValues(infofossil = NULL)
  rv <- reactiveValues(infodata = NULL)
  rv <- reactiveValues(budinfo = NULL)
  
  rv <- reactiveValues(ratepr = NULL)
  rv <- reactiveValues(rateli = NULL)
  rv <- reactiveValues(ratelo = NULL)
  rv <- reactiveValues(ratepri = NULL)
  rv <- reactiveValues(ratelii = NULL)
  rv <- reactiveValues(ratelip = NULL)
  rv <- reactiveValues(rateprp = NULL)
  
  rv <- reactiveValues(muosel = NULL)
  rv <- reactiveValues(muoseli = NULL)
  rv <- reactiveValues(muopri = NULL)
  
  rv <- reactiveValues(pll = NULL)
  rv <- reactiveValues(hix = NULL)
  
  
  rv = reactiveValues(ffyear= NULL)
  rv = reactiveValues(view= NULL)
  
  rv = reactiveValues(pressed=TRUE)
  
  rv =   reactiveValues(skip=1)

  # skip <- reactiveValues(1)
  
  rv$lurk =0
  rv$triggor=1
  
  rv$x =0
  rv$lek = FALSE
  rv$lang = "eng"
  rv$bgc = hsv(.1,.1,.7)
  rv$teksvari = "white"
  rv$obsvari = "lightgreen"
  
  rv$pll = 1
  rv$plll = 1
  
  observeEvent(input$last_btn, {
    rv$lastt =c(input$last_btn)
    
    
  })
  output$lastButtonCliked=renderText({input$last_btn})
  # observeEvent(rv$lang,{
  #   
  #   if  
  #   output$viewtext = renderText({
  #     
  #     rv$info
  #   })})
  
  # output$selitys = renderText({
  #   
  #   switch(input$lang, "eng"="In progress, finished in February 2023. Tool to explore various global carbon
  # tax/dividend scenarios by adjusting the assumed population progression, the rate of emission
  # reductions and the level of carbon tax. Funded by the Kone foundation. www.villeseppala.fi. Data: UN, IPCC, Friedlingstein et al. 2021, World Bank",
  # "fin"="Kesken, valmistuu helmikuussa 2023. Työkalulla voi hahmotella erilaisia hiilen hinnoittelu ja hiiliosinko
  #        -skenaarioita säätämällä oletettua väestökehitystä, päästövähennyksiä ja hiiliveroa. Koneen säätiön rahoittama hanke. www.villeseppala.fi. Data: UN, IPCC, Friedlingstein et al. 2021, World Bank")
  #   # )
  # })    
  # luu = reactive({
  #   
  #   lu
  # })
  # luu2 = reactive({
  #   
  #   lu2
  # })
  lu = as.data.table(lu)
  lu2 = as.data.table(lu2)
  
  lug = reactive({
    if (rv$lang=="eng") {
      lu } else if (rv$lang =="fin") {
        lu2
      }
  })
  # 
  observeEvent(input$comcou, {
    if (input$comcou !="none") {
      
      rv$lurk=1
    }
  })
  
  observeEvent(rv$x, { 
    rv$lurk=0
  })
  # secc = c("fossil", "land", "net", "pop", "averagedividend", "countrycost")
  secc=c("fossil", "land", "net", "ghg","nonco2","source", "sink", "newsink","price", "avgcost", "avgfossil", "userfossil", "netcost","usercost",
         "pop","dividend", "landcost", "avgnetcost", "countryfossil", "countrypop", "countrycost", "countrynetcost", "averagedividend", "countrydividend")
  
  observeEvent(input$autodraw, { 
    rv$autodraw=input$autodraw
  })
  
  observeEvent(input$view, { 
    
    if (input$view ==1) {
      rv$view = 1
    } else if (input$view ==2 ) {
      rv$view =2
    } else if (input$view ==3) { 
      rv$view =3}
    else if (input$view ==4) { 
      rv$view =4}
    
    if (input$view ==2) {
      
      rv$ffyear = 2000
      
    } else {
      rv$ffyear = 1980
    }
    
    
  })

  
  
  # output$fossill = renderText(rv$fossill)
  
  output$fossill = renderText(
    # {
    # div(style="font-weight:1000; ", 
    
    paste(
      # rv$yearc,": ",
      '<span style=\"color:',fos, '\"><b>', rv$fossill, " Gt",'</b></span>',
      # '<span style=\"color:',lul,'\">',  rv$landl,"Gt", '</span>',
      # "=",
      # '<span style=\"color:',net,'\">', rv$netl,"Gt", '</span>',
      sep = "")
    # )
    # }
  )
  
  output$landl = renderText(
    # div(style="font-weight:700; "),
    
    paste(
      # rv$yearc,": ",
      # '<span style=\"color:',fos, '\">', rv$fossill, "Gt",'</span>',
      '<span style=\"color:',lul,'\"><b>',  rv$landl," Gt", '</b></span>',
      # "=",
      # '<span style=\"color:',net,'\">', rv$netl,"Gt", '</span>',
      sep = "")
  )
  
  output$netl = renderText(
    paste(
      # rv$yearc,": ",
      '<span style=\"color:',fos, '\"><b>', rv$fossill, " Gt",'</b></span>',
      '<span style=\"color:',rv$teksvari,'\"><b>',  rv$landplus, '</b></span>',
      
      '<span style=\"color:',lul,'\"><b>',  rv$landl," Gt", '</b></span>',
      '<span style=\"color:',rv$teksvari,'\"><b>', " = ",'</span>',
      '<span style=\"color:',net,'\"><b>', rv$netl," Gt", '</b></span>',
      sep = "")
  )
  
  output$ghgl = renderText(
    paste(
      # rv$yearc,": ",
      '<span style=\"color:',fos, '\"><b>', rv$fossill, " Gt",'</b></span>',
      '<span style=\"color:',rv$teksvari,'\">',  " + ", '</b></span>',
      
      '<span style=\"color:',non,'\"><b>',  rv$nonco2l," Gt", '</b></span>',
      '<span style=\"color:',rv$teksvari,'\"><b>', " = ",'</b></span>',
      '<span style=\"color:',tot,'\"><b>', rv$ghgl," Gt", '</b></span>',
      sep = "")
  )
  output$nonco2l = renderText(
    paste(
      # rv$yearc,": ",
      # '<span style=\"color:',fos, '\">', rv$fossill, "Gt",'</span>',
      # '<span style=\"color:',rv$teksvari,'\">',  "+", '</span>',
      
      '<span style=\"color:',non,'\"><b>',  rv$nonco2l," Gt", '</b></span>',
      # '<span style=\"color:',rv$teksvari,'\">', "=",'</span>',
      # '<span style=\"color:',ghg,'\">', rv$ghgl,"Gt", '</span>',
      sep = "")
  )
  
  output$popl = renderText(
    paste(
      # rv$yearc,": ",
      '<span style=\"color:',pop,'\"><b>',  rv$popl," B", '</b></span>',
      sep = "")
  )
  
  output$avgfossill = renderText(
    paste(
      # rv$yearc,": ",
      '<span style=\"color:',rv$totalcolor, '\"><b>', rv$totall, " Gt",'</b></span>',
      '<span style=\"color:',rv$teksvari,'\"><b>',  " / ", '</b></span>',
      '<span style=\"color:',pop,'\"><b>',  rv$popl," B", '</b></span>',
      '<span style=\"color:',rv$teksvari,'\"><b>', " = ",'</span>',
      '<span style=\"color:',fpop,'\"><b>',  rv$avgfossill," t", '</b></span>',
      
      sep = "")
  )
  
  output$pricel = renderText(
    paste(
      # rv$yearc,": ",
      '<span style=\"color:',tax,'\"><b>',  rv$pricel," $", '</b></span>',
      sep = "")
  )
  
  output$avgcostl = renderText(
    paste(
      # rv$yearc,": ",
      '<span style=\"color:',fpop,'\"><b>',  rv$avgfossill," t", '</b></span>',
      '<span style=\"color:',rv$teksvari,'\"><b>',  " * ", '</b></span>',
      
      '<span style=\"color:',tax,'\"><b>',  rv$pricel," $", '</b></span>',
      '<span style=\"color:',rv$teksvari,'\"><b>', " = ",'</b></span>',
      '<span style=\"color:',avgcost,'\"><b>',  rv$avgcostl," $", '</b></span>',
      
      sep = "")
  )
  
  
  
  
  output$dividendl = renderText(
    
    if (input$luls==0) {
    
    if (input$national == 0) {
      
      paste(
        # rv$yearc,": ",
        
        '<span style=\"color:',fpop,'\"><b>',  rv$avgfossill," t", '</b></span>',
        '<span style=\"color:',rv$teksvari,'\"><b>',  " * ", '</b></span>',
        
        '<span style=\"color:',tax,'\"><b>',  rv$pricel," $", '</b></span>',
        
        
        '<span style=\"color:',rv$teksvari,'\"><b>', " = ",'</b></span>',
        '<span style=\"color:',dividend,'\"><b>',  rv$dividendl," $", '</b></span>',
        
        sep = "") } else {
          paste(
            # rv$yearc,": ",
            '<span style=\"color:',rv$teksvari,'\"><b>',  "(1-",rv$nationall,  ") * ", '</b></span>',
            
            '<span style=\"color:',fpop,'\"><b>',  rv$avgfossill," t", '</b></span>',
            '<span style=\"color:',rv$teksvari,'\"><b>',  " * ", '</b></span>',
            
            '<span style=\"color:',tax,'\"><b>',  rv$pricel," $", '</b></span>',
            '<span style=\"color:',rv$teksvari,'\"><b>',  "", '</b></span>',
            '<span style=\"color:',rv$teksvari,'\"><b>', " = ",'</b></span>',
            '<span style=\"color:',dividend,'\"><b>',  rv$dividendl," $", '</b></span>',
            
            sep = "") 
          
          
        }}
    else {
      
      if (input$national == 0) {
        
        paste(
          # rv$yearc,": ",
          
          '<span style=\"color:',avgcost,'\"><b>',  rv$avgcostl," $", '</b></span>',
          
          
          '<span style=\"color:',rv$teksvari,'\"><b>',  " - ", '</b></span>',
          
          '<span style=\"color:',landcost,'\"><b>',  rv$landcostl," $", '</b></span>',
          
          
          '<span style=\"color:',rv$teksvari,'\"><b>', " = ",'</b></span>',
          '<span style=\"color:',dividend,'\"><b>',  rv$dividendl," $", '</b></span>',
          
          sep = "") } else {
            paste(
              # rv$yearc,": ",
              '<span style=\"color:',rv$teksvari,'\"><b>',  "(1-",rv$nationall,  ") * ", '</b></span>',
              
              
              '<span style=\"color:',avgcost,'\"><b>',  rv$avgcostl," $", '</b></span>',
              
              
              '<span style=\"color:',rv$teksvari,'\"><b>',  " - ", '</b></span>',
              
              '<span style=\"color:',landcost,'\"><b>',  rv$landcostl," $", '</b></span>',
              
              
              '<span style=\"color:',rv$teksvari,'\"><b>', " = ",'</b></span>',
              '<span style=\"color:',dividend,'\"><b>',  rv$dividendl," $", '</b></span>',
              
              
              sep = "") 
            
            
          }
      
      
    }
    
  )
  
  output$avgnetcostl = renderText(
    paste(
      # rv$yearc,": ",
      '<span style=\"color:',avgcost,'\"><b>',  rv$avgcostl," $", '</b></span>',
      '<span style=\"color:',rv$teksvari,'\"><b>',  " - ", '</b></span>',
      
      '<span style=\"color:',dividend,'\"><b>',  rv$dividendl," $", '</b></span>',
      '<span style=\"color:',rv$teksvari,'\"><b>', " = ",'</span>',
      '<span style=\"color:',avgnetcost,'\"><b>',  rv$avgnetcostl," $", '</b></span>',
      
      sep = "")
  )
  
  output$userfossill = renderText(
    paste(
      # rv$yearc,": ",
      '<span style=\"color:',fosindi,'\"><b>',  rv$userfossill," t", '</b></span>',
      
      sep = "")
  )
  
  output$usercostl = renderText(
    paste(
      # rv$yearc,": ",
      
      '<span style=\"color:',fosindi,'\"><b>',  rv$userfossill," t", '</b></span>',
      '<span style=\"color:',rv$teksvari,'\"><b>',  " * ", '</b></span>',
      
      '<span style=\"color:',tax,'\"><b>',  rv$pricel," $", '</b></span>',
      '<span style=\"color:',rv$teksvari,'\"><b>', " = ",'</b></span>',
      
      '<span style=\"color:',taxfosindi,'\"><b>',  rv$usercostl," $", '</b></span>',
      
      sep = "")
  )
  
  
  output$netcostl = renderText(
    if (input$national == 0) {
      paste(
        # rv$yearc,": ",
        '<span style=\"color:',taxfosindi,'\"><b>',  rv$usercostl," $", '</b></span>',
        '<span style=\"color:',rv$teksvari,'\"><b>',  " - ", '</b></span>',
        
        '<span style=\"color:',dividend,'\"><b>',  rv$dividendl," $", '</b></span>',
        '<span style=\"color:',rv$teksvari,'\"><b>', " = ",'</span>',
        '<span style=\"color:',netcost,'\"><b>',  rv$netcostl," $", '</b></span>',
        
        
        sep = "")
      
      
    } else if (input$national != 0) {
      
      
      if (input$nationalcoun %in% c(ll2)) {
        paste(
          # rv$yearc,": ",
          '<span style=\"color:',taxfosindi,'\"><b>',  rv$usercostl," $", '</b></span>',
          '<span style=\"color:',rv$teksvari,'\"><b>',  " - ", '</b></span>',
          
          '<span style=\"color:',dividend,'\"><b>',  rv$dividendl," $", '</b></span>',
          '<span style=\"color:',rv$teksvari,'\"><b>',  " - ", '</b></span>',
          '<span style=\"color:',dividend,'\"><b>',  rv$usernatl," $", '</b></span>',
          
          '<span style=\"color:',rv$teksvari,'\"><b>', " = ",'</b></span>',
          '<span style=\"color:',netcost,'\"><b>',  rv$netcostl," $", '</b></span>',
          
          
          sep = "")
        
        
        
      } else {
        paste(
          # rv$yearc,": ",
          '<span style=\"color:',taxfosindi,'\"><b>',  rv$usercostl," $", '</b></span>',
          '<span style=\"color:',rv$teksvari,'\"><b>',  " - ", '</b></span>',
          
          '<span style=\"color:',dividend,'\"><b>',  rv$dividendl," $", '</b></span>',
          '<span style=\"color:',rv$teksvari,'\"><b>',  " - ", '</b></span>',
          '<span style=\"color:',dividend,'\"><b>',  rv$averagedividendl," $", '</b></span>',
          
          '<span style=\"color:',rv$teksvari,'\"><b>', " = ",'</b></span>',
          '<span style=\"color:',netcost,'\"><b>',  rv$netcostl," $", '</b></span>',
          
          
          sep = "")
        
      }
    }
  )
  
  

  
  
  
  
  output$countryfossill = renderText(
    paste(
      # rv$yearc,": ",
      '<span style=\"color:',countryfossil,'\"><b>',  rv$countryfossill," t", '</b></span>',
      
      sep = "")
  )
  
  output$countrycostl = renderText(
    paste(
      # rv$yearc,": ",
      '<span style=\"color:',countryfossil,'\"><b>',  rv$countryfossill," t", '</b></span>',
      '<span style=\"color:',rv$teksvari,'\"><b>',  "*", '</b></span>',
      
      '<span style=\"color:',tax,'\"><b>',  rv$pricel," $", '</b></span>',
      '<span style=\"color:',rv$teksvari,'\"><b>', "=",'</b></span>',
      '<span style=\"color:',countrycost,'\"><b>',  rv$countrycostl," $", '</b></span>',
      
      sep = "")
  )
  output$countrypopl = renderText(
    paste(
      # rv$yearc,": ",
      '<span style=\"color:',countrypop,'\"><b>',  rv$countrypopl," t", '</b></span>',
      
      sep = "")
  )
  
  
  
  
  output$countrynetcostl = renderText(
    if (input$national == 0) {
      # req(datsss())
      
      paste(
        # rv$yearc,": ",
        '<span style=\"color:',countrycost,'\"><b>',  rv$countrycostl," $", '</b></span>',
        '<span style=\"color:',rv$teksvari,'\"><b>',  "-", '</b></span>',
        
        '<span style=\"color:',dividend,'\"><b>',  rv$dividendl," $", '</b></span>',
        '<span style=\"color:',rv$teksvari,'\"><b>', "=",'</b></span>',
        '<span style=\"color:',countrynetcost,'\"><b>',  rv$countrynetcostl," $", '</b></span>',
        
        
        sep = "")
      
      
    } else if (input$national != 0) {
      # req(datsss())
      
      
      # if (input$nationalcoun %in% c(ll2)) {
      paste(
        # rv$yearc,": ",
        '<span style=\"color:',countrycost,'\"><b>',  rv$countrycostl," $", '</b></span>',
        '<span style=\"color:',rv$teksvari,'\"><b>',  "-", '</b></span>',
        
        '<span style=\"color:',dividend,'\"><b>',  rv$dividendl," $", '</b></span>',
        '<span style=\"color:',rv$teksvari,'\"><b>',  "-", '</b></span>',
        '<span style=\"color:',dividend,'\"><b>',  rv$countrydividendl," $", '</b></span>',
        
        '<span style=\"color:',rv$teksvari,'\"><b>', "=",'</b></span>',
        '<span style=\"color:',netcost,'\"><b>',  rv$netcostl," $", '</b></span>',
        
        
        sep = "")
      
      
    }
    
  )
  
  
  output$countrydividendl = renderText(
    paste(
      # rv$yearc,": ",
      '<span style=\"color:',rv$teksvari,'\"><b>',  rv$nationall,  "*", '</b></span>',
      '<span style=\"color:',countrycost,'\"><b>',  rv$countrycostl," $", '</b></span>',
      
      
      '<span style=\"color:',rv$teksvari,'\"><b>', "=",'</b></span>',
      '<span style=\"color:',countrydividend,'\"><b>',  rv$countrydividend," $", '</b></span>',
      
      sep = "")
  )
  
  output$averagedividendl = renderText(
    paste(
      # rv$yearc,": ",
      '<span style=\"color:',rv$teksvari,'\"><b>',  rv$nationall,  "*", '</b></span>',
      '<span style=\"color:',dividend,'\"><b>',  rv$avgcostl," $", '</b></span>',
      
      
      '<span style=\"color:',rv$teksvari,'\"><b>', "=",'</b></span>',
      '<span style=\"color:',averagedividend,'\"><b>',  rv$averagedividend," $", '</b></span>',
      
      sep = "")
  )
  
  output$newsinkl = renderText(
    paste(
      '<span style=\"color:',newsink,'\"><b>',  rv$newsinkl," Gt", '</b></span>',
      
      # rv$yearc,": ",
      # '<span style=\"color:',rv$teksvari,'\"><b>',  rv$nationall,  "*", '</b></span>',
      

      
      sep = "")
  )
  
  
  output$landcostl = renderText(
    paste(
      # rv$yearc,": ",
      # '<span style=\"color:',rv$teksvari,'\"><b>',  rv$nationall,  "*", '</b></span>',
      '<span style=\"color:',newsink,'\"><b>',  rv$newsinkl,"Gt", '</b></span>',
      
      
      '<span style=\"color:',rv$teksvari,'\"><b>', "*",'</b></span>',
      '<span style=\"color:',tax,'\"><b>',  rv$pricel,"$", '</b></span>',
      '<span style=\"color:',rv$teksvari,'\"><b>', "/",'</b></span>',
      '<span style=\"color:',pop,'\"><b>',  rv$popl,"B", '</b></span>',
      
      '<span style=\"color:',rv$teksvari,'\"><b>', "=",'</b></span>',
      
      '<span style=\"color:',landcost,'\"><b>',  rv$landcostl,"$", '</b></span>',
      
      sep = "")
  )
  
  
  
  output$land1 = renderText(
    paste(
    rv$yearc, " need for net land sink: ", input$lstart, "Gt + ", abs(as.numeric(rv$landl)), "Gt = ", rv$landneed, "Gt",
      sep = "")
  )
  
  output$land2 = renderText(
    paste(
      rv$yearc, " land emissions: ", input$sourcestart, "Gt - (", format(input$lulls/100, nsmall=2), "*", rv$landneed, "Gt) = ", rv$sourcel, "Gt",
      sep = "")
  )
  
  output$land3 = renderText(
    paste(
      2021, " old sinks+CCS: ", input$sinkstart, "Gt",
      sep = "")
  )
  
  output$land4 = renderText(
    paste(
      rv$yearc, " new sinks+CCS: ", rv$landneed, "Gt - (", format(input$lulls/100, nsmall=2), "*", rv$landneed, "Gt) = ",  rv$newsinkl, "Gt",
      sep = "")
  )
  output$land5 = renderText(
    paste(
      rv$yearc, " land+CCS CO2: ", rv$sourcel, "Gt ", input$sinkstart, "Gt", rv$newsinkl, "Gt = ",  rv$landl, "Gt",
      sep = "")
  )

  output$land6 = renderText(
    paste(
      2021, " land emissions: ", input$sourcestart, "Gt",
      sep = "")
  )

  
  observeEvent(rv$lang, {   
    
    if (rv$lang == "eng") {
      
      rv$pressed = TRUE
      
      lapply(
        X = secc,
        FUN = function(i){
          
          
          output[[paste0("label",i)]] = renderText({
            lu[sec==i, label]
            # paste0("Info: ",rv$infot)
          })  
          
        })
      
      
      
      
      updateTabsetPanel(session,  "nok", selected = "1. Global emissions")
      
      
      updateCheckboxInput("showmainplot", "Main results", session=session)
      updateCheckboxInput("showcolplot", "Compare countries (slows page)", session=session)
      

      # updateSliderTextInput("xyear", label="Näytä tiedot vuodelle", session=session)
      # updateSliderInput("conc", inf("Maiden päästöjen yhdentyminen", "infoconvergence2"),  session=session)
      updateRadioButtons("yvar", label= "Show country averages for",       c(
        # "GDP per capita" = "gdpcap",
        "Net costs per capita"= "countrynetcost",
        "Carbon costs per capita"="countrycost",
        
        "Emissions per capita" = "countryfossil"
        
      ),  session=session)
      updateRadioButtons("xvar", label="Arrange x-axis by 2021 value of:",       
                         c(
                           # "GDP per capita" = "gdpcap",
                           "Emissions per capita" = "countryfossil",
                           
                           "GDP per capita" = "gdpcap"
                           
                         ),  session=session)
      updateSelectInput("comcou", label = "Details for country", session=session)
      updateCheckboxInput("showexp", label = "Show how country outcomes are calculated", session=session)
      
      output$indicatorvisibility = renderText({  c("Indicator visibility")  })  
      output$showall = renderText({  c("Show all")  })  
      output$shownone = renderText({  c("Show none")  })  
      output$automatic = renderText({  c("Automatic")  })  
      output$redraw = renderText({  c("Redraw")  })  
      output$autoredraw = renderText({  c("Auto-Redraw")  })  
      
      output$startlabel = renderText({  c("Start year values")  })  
      
      output$obsyear = renderText({  c("Observe year:")  })  
      
      # output$simuset = renderText({  c("SIMULATION INPUTS")  })  
      
      
      # rv$labelfossil = lu[sec=="fossil", label]
      
      
      # rv$infofossiltext = c("Total fossil CO2 emissions across all countries and individuals. Other greenhouse gases (such as methane) not included. Source for emissions from 2021 and before: Global Carbon Project 2022 (Friedlingstein et al. 2021)")
      # rv$infofossiltextt = c("Fossil CO2 emissions")
      # output$viewtext = renderText("  VIEWING SETTINGS")
      
      output$titletext = renderText(" Global carbon price simulator (BETA)")
      # updateactionBttn("infodata", label="Data sources")
      updateActionButton("infodata", label="Data sources", session=session)
      updateActionButton("tutorial", label="Tutorial", session=session)
      # updateBollux("mobilebox",             title = "If you are using mobile, landscape rotation is recommended")
      # updateRadioButtons("view", label = "Graph separation",
      #                    c("One graph for all scales" = 1,
      #                      "Side by side graphs, two in row" = 2,
      #                      "Back to back graphs" = 3,
      #                      "Side by side graphs, all in one row" = 4),
      #                    # selected =3,
      #                    selected = rv$view,
      #                    
      #                    session = session)
      
      updateSelectInput("view", label = "Graph separation",
                         c("One graph for all scales" = 1,
                           "Side by side graphs, two in row" = 2,
                           "Back to back graphs" = 3,
                           "Side by side graphs, all in one row" = 4),
                         # selected =3,
                         selected = rv$view,
                         
                         session = session)
      
      
      output$simuset =  renderText(HTML("<font size='4'>",
                                        as.character(icon("far fa-hand-paper")), 
                                        
                                        "<b>  SIMULATION INPUTS  </b>","</font>"))
      
      # output$graview =  renderText(HTML("<font size='4'>",
      #                                   as.character(icon("far fa-hand-paper")), 
      #                                   
      #                                   "<b>  GRAPH VIEW  </b>","</font>"))
      # output$viewtext =  renderText(HTML("<font size='4'>",
      #                                    as.character(icon("far fa-eye")), 
      #                                    
      #                                    "  RESULT VISIBILITY  ",  
      #                                    # as.character( icon("fas fa-caret-square-down")),
      #                                    "</font>"))
      output$viewtext =  renderText( "RESULT INDICATORS")
      
      output$simuset =  renderText( "INPUTS")
      
      output$graview =  renderText( "GRAPH VIEW")
      
      
      # output$simuresults =  renderText(HTML("<font size='4'>",
      #                                   as.character(icon("far fa-hand-paper")), 
      #                                   
      #                                   "<b>  SIMULATION RESULTS  </b>","</font>"))
      
      output$simuresults =  renderText( "RESULTS")
      # renderOutput("muok")
      
      observeEvent(rv$rateli, {
        rateli <- rv$rateli
        ratepr <- rv$ratepr
        
        # Can also set the label and select items
        updateRadioButtons(session, "muo","Shape of the fossil emission curve",
                           # label = paste("Shape of the emission curve"),
                           choiceNames = list(paste0("Linear (", format(round(rateli,2)), " Gt each year)"),
                                              paste0("Percentual (", format(round( ratepr,2)), " % each year)")
                                              # , c("Double exponentiaul")
                                              
                           ),
                           choiceValues = list("linear", "percentual" 
                                               # , "exponential"
                           ),
                           selected = rv$muosel
                           
        )
      })
      
      
      
      observeEvent(rv$ratelii, {
        ratelii <- rv$ratelii
        ratepri <- rv$ratepri
        
        if (ratelii >=0) {
          sig = "+"
        } else {sig =""}
        updateRadioButtons(session, "muoindi","Shape of user emission curve",
                           choiceNames = list(paste0("Linear (",sig, format(round(ratelii,2)), " t each year)"), 
                                              paste0("Percentual (",sig, format(round( ratepri,2)), " % each year)")
                           ),
                           choiceValues = list("linear", "percentual"), 
                           
                           selected = rv$muoseli
                           
        )
      })
      
      observeEvent(rv$ratelip, {
        ratelip <- rv$ratelip
        rateprp <- rv$rateprp
        
        if (ratelip >=0) {
          sig = "+"
        } else {sig =""}
        updateRadioButtons(session, "pri","Shape of the carbon price curve",
                           # label = paste("Shape of the emission curve"),
                           choiceNames = list(paste0("Linear (",sig, format(round(ratelip,2)), " $ each year)"), 
                                              paste0("Percentual (",sig, format(round( rateprp,2)), " % each year)")
                                              , "Logarithmic"
                           ),
                           choiceValues = list("linear", "percentual"
                                               , "logarithmic"
                           )  , 
                           selected = rv$muopri
        )
        
      })
      
      
      updateTabsetPanel(session, "nok", selected = '1. Global emissions')
      
      updateCheckboxInput(session, "showtab", "Show the simulation results also in table below")
      # lug = luu()  
      
      
    }  else if (rv$lang =="fin") {
      rv$pressed = TRUE
      
      lapply(
        X = secc,
        FUN = function(i){
          
          
          output[[paste0("label",i)]] = renderText({
            lu2[sec==i, label]
            # paste0("Info: ",rv$infot)
          })  
          
        })
      updateTabsetPanel(session,  "nok", selected = "1. Globaalit päästöt")
      
      updateCheckboxInput(session, "showtab", "Näytä tulokset myös taulukossa alapuolella")

      
      updateCheckboxInput("showmainplot", "Päätulokset", session=session)
      updateCheckboxInput("showcolplot", "Vertaa maita (hidastaa sivua)", session=session)
      # updateSliderTextInput("xyear", label="Näytä tiedot vuodelle", session=session)
      # updateSliderInput("conc", inf("Maiden päästöjen yhdentyminen", "infoconvergence2"),  session=session)
      updateRadioButtons("yvar", label="Näytä maakeskiarvot:",         c(
        # "GDP per capita" = "gdpcap",
        "Nettokustannukset henkeä kohden"= "countrynetcost",
        "Päästökustannukset henkeä kohden"="countrycost",
        
        "Päästöt henkeä kohden" = "countryfossil"
        
      ),  session=session)
      updateRadioButtons("xvar", label="Järjestä maat vaaka-asteikolle 2021-arvon perusteella:",        
                         c(
        # "GDP per capita" = "gdpcap",
        "Päästöt henkeä kohden" = "countryfossil",
        
        "BKT henkeä kohden" = "gdpcap"
        
      ),  session=session)
      updateSelectInput("comcou", label = "Yksityiskohdat maalle", session=session)
      updateCheckboxInput("showexp", label = "Näytä miten maiden arvot on laskettu", session=session)
      
      # rv$labelfossil = lu2[sec=="fossil", label]
      
      
      # rv$labelfossil = "Fossiilipäästöt"
      # rv$infofossiltext = c("Kaikki fossiiliset CO2-päästöt kaikissa maissa. Ei sisällä muita kasvihuonekaasupäästöjä (kuten metaania).
      #                       Datalähde päästöille 2021 ja ennen: Global Carbon Project 2022 (Friedlingstein et al. 2021)")
      # rv$infofossiltextt = c("Fossiilipäästöt") 
      # output$viewtext = renderText("  NÄKYMÄASETUKSET")
      output$indicatorvisibility = renderText({ c("Kuvaajien näkyvyys")})
      output$showall = renderText({  c("Näytä kaikki")  })  
      output$shownone = renderText({  c("Piilota kaikki")  })  
      output$automatic = renderText({  c("Automattinen")  })  
      output$redraw = renderText({  c("Uusi piirto")  })  
      output$autoredraw = renderText({  c("Autom. piirto")  })  
      
      output$startlabel = renderText({  c("Lähtövuoden arvot")  })  
      # output$simuset = renderText({  c("SIMULAATIOVALINNAT")  })  
      output$obsyear = renderText({  c("Havainnoi vuotta:")  })  
      
      
      
      output$titletext = renderText(" Globaali hiilen hinta -simulaattori (BETA)")
      updateActionButton("infodata", label="Data-lähteet", session=session)
      updateActionButton("tutorial", label="Tutoriaali", session=session)
      # updateBox("mobilebox",   title = "Jos käytät sivua puhelimella, suosittelemme vaakanäkymää")
      
      
      # updateRadioButtons("view",label = "Graafien erittely",
      #                    c("Yksi graafi kaikille asteikoille" = 1,
      #                      "Vierekkäiset graafit eri asteikoille" = 2,
      #                      "Peräkkäiset graafit eri asteikoille" = 3,
      #                      "Vierekkäiset, kaikki samalla asteikoilla" = 4), 
      #                    
      #                    selected = rv$view,
      #                    # selected =3,
      #                    session = session)   
      
      updateSelectInput("view",label = "Graafien erittely",
                         c("Yksi graafi kaikille asteikoille" = 1,
                           "Vierekkäiset graafit eri asteikoille, kaksi per rivi" = 2,
                           "Peräkkäiset graafit eri asteikoille" = 3,
                           "Vierekkäiset graafit eri asteikoille, yksi rivi" = 4), 
                         
                         selected = rv$view,
                         # selected =3,
                         session = session)   
      
      
      
      output$viewtext =  renderText( "TULOS-MUUTTUJAT")
      
      output$simuset =  renderText( "VALINNAT")
      
      # output$graview =  renderText( "GRAAFINÄKYMÄ")
      
      
      
      output$simuresults =  renderText( "TULOKSET")
      
      
      # renderOutput("muok")
      
      observeEvent(rv$rateli, {
        # rateli <- rv$rateli
        # ratepr <- rv$ratepr
        
        updateRadioButtons(session, "muo","Fossiilipäästöjen käyrän muoto",
                           # label = paste("Shape of the emission curve"),
                           choiceNames = list(paste0("Lineaarinen (", format(round(rv$rateli,2)), " Gt per vuosi)"),
                                              paste0("Prosentuaalinen (", format(round( rv$ratepr,2)), " % per vuosi)")
                           ),
                           choiceValues = list("linear", "percentual"),
                           selected = rv$muosel
                           
        )
      })
      
      observeEvent(rv$ratelii, {
        ratelii <- rv$ratelii
        ratepri <- rv$ratepri
        
        if (ratelii >=0) {
          sig = "+"
        } else {sig =""}
        updateRadioButtons(session, "muoindi","Käyttäjäpäästöjen käyrän muoto",
                           choiceNames = list(paste0("Lineaarinen (",sig, format(round(ratelii,2)), " t per vuosi)"), 
                                              paste0("Prosentuaalinen (",sig, format(round( ratepri,2)), " % per vuosi)")
                           ),
                           choiceValues = list("linear", "percentual"), 
                           
                           selected = rv$muoseli
                           
        )
      })
      
      observeEvent(rv$ratelip, {
        ratelip <- rv$ratelip
        rateprp <- rv$rateprp
        
        if (ratelip >=0) {
          sig = "+"
        } else {sig =""}
        updateRadioButtons(session, "pri","Hiilen hinta -käyrän muoto",
                           # label = paste("Shape of the emission curve"),
                           choiceNames = list(paste0("Lineaarinen (",sig, format(round(ratelip,2)), " $ per vuosi)"), 
                                              paste0("Prosentuaalinen (",sig, format(round( rateprp,2)), " % per vuosi)")
                                              , "Logaritminen"
                           ),
                           choiceValues = list("linear", "percentual"
                                               , "logarithmic"
                           )  , 
                           selected = rv$muopri
        )
        
      })
      
      
      
      # lug = luu2()
    }})
  
  
  # muok = renderUI(
  #   
  #   if (lang =="eng"){  
  #     
  #   
  #   RadioButtons(session, "muo","Shape of the fossil emission curve",
  #                      # label = paste("Shape of the emission curve"),
  #                      choiceNames = list(paste0("Linear (", format(round(rateli,2)), " Gt each year)"),
  #                                         paste0("Percentual (", format(round( ratepr,2)), " % each year)")
  #                      ),
  #                      choiceValues = list("linear", "percentual"),
  #                      selected = rv$muosel
  #                      
  #                      # c((paste0("Linear drop",r)="linear"),
  #                      #   (paste0("Percentual drop", r)="percentual")),
  #                      # selected =
  #   )
  #   } else if (lang =="fin"){  
  #   RadioButtons(session, "muo","Fossiilipäästöjen käyrän muoto",
  #                      # label = paste("Shape of the emission curve"),
  #                      choiceNames = list(paste0("Lineaarinen (", format(round(rv$rateli,2)), " Gt per vuosi)"),
  #                                         paste0("Prosentuaalinen (", format(round( rv$ratepr,2)), " % per vuosi)")
  #                      ),
  #                      choiceValues = list("linear", "percentual"),
  #                      selected = rv$muosel
  #                      
  #                      # c((paste0("Linear drop",r)="linear"),
  #                      #   (paste0("Percentual drop", r)="percentual")),
  #                      # selected =
  #   )
  #   }
  #   
  # )
  
  
  
  observeEvent(rv$lang, {
    
    if (rv$lang == "eng") {
      
      rv$tutotextt = c("Tutorial")
      
      rv$tutotext = HTML("
       <b>What is this page?</b> 
       <br>
Global carbon price would be an efficient tool for stopping global warming. If the revenue from carbon pricing was distributed evenly as a global carbon dividend, it would also reduce global income inequality significantly. 

On this page, you can simulate emission and emission price pathways for reaching carbon neutrality and thus stopping global warming. Simulation choices have major implications on your personal carbon costs and the global income distribution. 
<br><br>


<b>Page structure</b>
<br>
RESULTS are shown at the graph section in the bottom right of the page. In addition to main result graph, you can compare country averages in a separate graph. You can also choose to show the results in a table. 
<br>

 RESULT INDICATORS section at the left side show the yearly results and control what variables are shown in the main results graph. 

<br>
INPUTS section is at the top of the page and its choices determine the results outcomes. 
Simulation inputs are divided to 4 thematic phases + an extra phase for country specific inputs. All inputs have predetermined sensible default values related to the chosen carbon budget so that you can focus on changing the inputs you are most interested in.
<br>
First input choice is the carbon budget. With this choice, you define the ambition level of the simulation. Carbon budget is a special type of input, which also changes some of the other inputs to their carbon budget specific defaults in phases 1 and 3. 
<br>

<br>
<b>Global emissions</b>
<br>
<br>
Global emissions inputs, together with the chosen budget, define the CO2 emissions for each year. Land use emissions and net emissions react to those choices and to the budget so that the cumulative net emissions equal to the carbon budget in the carbon neutrality year, when the warming stops. 
<br>
You can also choose to include non-CO2 emissions to the system so that they are included when calculatig individual specific emissions and pricing costs.
<br>

<br>
<b>Population projection</b>
<br>
Population projection choice defines how the world population will evolve. Mean emissions are the results of dividing the emission by the population. 
<br>
By default, the median projection is chosen, meaning that there is a 50% chance for both smaller and greater population in the upcoming years. Choice of 95% range upper limit projection means that there is a 2.5% chance that the future population will be greater than the projected value. Choosing a 80% range lower limit projection means that there is a 10%  chance that the population is smaller than in the chosen projection. And so on.
<br><br>
<b>Carbon price</b>
<br>
It is very difficult to estimate what level of carbon pricing is adequate for reaching specific emission targets. It is therefore recommended for the user to explore major changes in carbon pricing to see their effects on carbon costs. 
<br>
In reality, if a specific global carbon pricing trajectory is adopted, it will be readjusted down or up depending on whether the emissions evolve better or worse than in the estimated trajectory. Readjusting the pricing will then readjust the emissions. 

<br>
<br>
<b>User emissions</b>
<br>
In this phase, you can try out different paths for your own emissions and explore what kind of annual carbon costs they would result in. 

<br
If you are using an online carbon footprint calculator to estimate your current emissions for the starting year, please take into account
if they consider all greenhouse gas emissions, including non-CO2. If they do, you should also have them set on in the global emissions phase. 
<br
By choosing the country level emissions from the ALTERNATIVE selection you can use the predicted per capita emission path of a specific country as your own emissions. 


<br><br>
<b>EXTRA: Countries</b>
<br>
In this section, you can explore the effect of a global carbon price system in which some of the carbon price revenue is not divided equally across the world, but a chosen percentage is given as a dividend for the citizens of the country in which that specific revenue is collected in. 

Also, you can explore the mean emission and carbon cost trajectories across different countries, assuming that the mean emissions between the countries converge according to a predetermined model. The convergence model is highly speculative. 

<br>
<br>

<b>Info buttons</b>
<br>
Click the info buttons across the UI to receive more information on various input choices and resulting indicators. 


<br>
<br>

<b>Caveats / What is not included?</b>
<br>
Achieving the necessary carbon sinks will also cause costs, which, however, are not modeled in the simulation. Sinks can be achieved through the carbon price as well.
<br
Please give feedback by clicking the feedback button in the top of the page if you have ideas on how to involve sinks in the model. 


")
 
      
  rv$coux =HTML(        "
        It is assumed that countries' per capita CO2 emissions converge to global average over time from their 2021 historical value.
Convergence factor denotes how much the emissions converge by the carbon neutrality year.                
                          <br>
                        
      Formula for calculating country specific per capita emissions (COBS) at the neutrality year is:
      <br>
      COBS = WOBS - (1-CONV)*(WSTART-CSTART)*(WOBS/WSTART),
      <br>

      in which:
<br>
WOBS = World per capita emissions at observed year
      <br>
CSTART = Country per capita emissions at start year
      <br>
WSTART = World per capita emissions at start year
      <br>
CONV = Convergence factor chose by the user
      <br>
      <br>

Intermediate emissions between pricing start year and carbon neutrality year are calculated as a linear or percentual trajectory, depending on the choice of total fossil emission trajectory. 
      <br>
A correction multiplier is applied to the outputs from the calculation to have the emission sum across all individuals equal the annual global emissions. It is same for all countries in a given year. 
      <br>
      <br>
   Country per capita costs are calculated by multiplying thus received per capita emissions with the carbon price value (chosen by user) of the given year. 

      <br>
   Country per capita net costs are calculated by deducting the carbon dividend from the carbon costs.

     <br>
      <br>

GDP per capita for 2021 is calculated by dividing GDP in 2015 $ (World Bank) by UN population estimate.   

 


      
      "
  ) 
    
      rv$fortext = c("Formula for calculating country specific emissions:")
      rv$formula= c("COBS = CORR*WOBS*(1-CONV)*((WSTART-CSTART)/WSTART)")
      
      rv$cobs = c("COBS = Country per capita emissions at observed year")
      rv$wobs = c("WOBS = World per capita emissions at observed year")
      rv$cstart = c("CSTART = Country per capita emissions at start year")
      rv$wstart = c("WSTART = World per capita emissions at start year")
      rv$convergence = c("CONV = Convergence factor chose by the user")
      rv$correction = c("CORR = Correction factor to balance country emissions to global emissions at observed year. Same for all countries.")
      # 
      # rv$corr 
      # rv$conv
      # rv$wstar
      # rv$cstar
      # 
      #   rv$wob
      # rv$cob
      
      rv$infofossiltext = c("Total fossil CO2 emissions across all countries and individuals. Other greenhouse gases (such as methane) not included. Source for emissions from 2021 and before: Global Carbon Project 2022 (Friedlingstein et al. 2021)")
      rv$infofossiltextt = c("Fossil CO2 emissions")
      
      rv$infolultext = c("CO2 emissions and sinks from land use change across all countries. In future, also includes technological sinks, such as DACCS and BECCS. Source for emissions from 2021 and before: Global Carbon Project 2022 (Friedlingstein et al. 2021)")
      rv$infolultextt = c("Land use emissions and sinks")
      
      
      rv$infonettext = c("Net CO2 emissions, as net from fossil and land use across all countries. Source for emissions from 2021 and before: Global Carbon Project 2022 (Friedlingstein et al. 2021)")
      rv$infonettextt = c("Net CO2 emissions")
      
      
      rv$infoghgtext = c("Total greenhouse gas emissions in CO2-equivalent, calculated as sum of CO2 emissions from Global Carbon Project and non-CO2 emissions from PRIMAP")
      rv$infoghgtextt = c("Total emissions")
      
      rv$infononco2text = c("Other greenhouse gas emissions than CO2, such as methane. 
                           Source PRIMAP: Gütschow, J.; Pflüger, M. (2022): The PRIMAP-hist national historical emissions time series v2.4 (1750-2021). zenodo. doi:10.5281/zenodo.7179775.
Gütschow, J.; Jeffery, L.; Gieseke, R.; Gebel, R.; Stevens, D.; Krapp, M.; Rocha, M. (2016): The PRIMAP-hist national historical emissions time series, Earth Syst. Sci. Data, 8, 571-603, doi:10.5194/essd-8-571-2016")
      rv$infononco2textt = c("Non-CO2 emissions")
      
      rv$infononco2utext = c("Other greenhouse gas emissions than CO2, such as methane. 
                            There is considerable uncertainty on what are realistic values in simulated years. Range limited to 5-10 at CO2 neutrality year which is interpreatation of IPCC estimates in IPCC AR6 WGIII chapter 3 page 339 figure 3.20.
                              <br> Source for historic emissions PRIMAP: Gütschow, J.; Pflüger, M. (2022): The PRIMAP-hist national historical emissions time series v2.4 (1750-2021). zenodo. doi:10.5281/zenodo.7179775.
Gütschow, J.; Jeffery, L.; Gieseke, R.; Gebel, R.; Stevens, D.; Krapp, M.; Rocha, M. (2016): The PRIMAP-hist national historical emissions time series, Earth Syst. Sci. Data, 8, 571-603, doi:10.5194/essd-8-571-2016")
      rv$infononco2utextt = c("Non-CO2 emissions")
      
      
      
      rv$infopoptext = c("World population, billions. Statistics and projections from United Nations. 
                      By default the median projection is chosen, meaning that there is a 50% chance for both smaller and greater population in the upcoming years. Choice of 95% range upper limit projection means that there is a 2.5% chance
                      that the future population will be greater than the projected value. Choosing a 80% range lower limit projection means that there is a 10% 
                      chance that the population is smaller than in the chosen projection. And so on.
                      https://population.un.org/wpp/Graphs/Probabilistic/POP/TOT/900")
      rv$infopoptextt = c("World population")
      
      rv$infopoputext = c("World population, billions. Statistics and projections from United Nations. 
                      By default the median projection is chosen, meaning that there is a 50% chance for both smaller and greater population. Choice of 95% range upper limit projection means that there is a 2.5% chance
                      that the population higher than the projected value. Choosing a 80% range lower limit projection means that there is a 10% 
                      chance that the population is lower than in the chosen projection. And so on.
                      https://population.un.org/wpp/Graphs/Probabilistic/POP/TOT/900")
      rv$infopoputextt = c("World population projection")
      
      
      rv$infoavgfossiltext =c("Average fossil emissions across all individuals. Result of dividing fossil emissions with world population. Includes non-CO2 emissions if they are selected on at the first phase. Does not include land use/sinks")
      rv$infoavgfossiltextt =c("Mean fossil CO2 emissions")
      
      
      rv$infopricetext =c("Global carbon price, $/t, in 2015 US dollars. Should be set high enough to achieve the chosen fossil emission reductions. For default carbon price values in different budgets, figure 3.32 in page 360 of IPCC AR6 WGIII chapter 3 has been used as a loose reference point.") 
      rv$infopricetextt =c("Carbon price") 
      
      rv$infoavgcosttext = c("Average carbon cost across all individuals. Product of average emissions and carbon price.")
      rv$infoavgcosttextt= c("Mean carbon cost")
      
      
      rv$infodividendtext = c("Carbon dividend given to each world citizen. Equal to the mean carbon cost. Product of average emissions and carbon price.")
      rv$infodividendtextt = c("Carbon dividend")
      
      
      rv$infoavgnetcosttext = c("Net average income across all individuals. Sum of mean carbon cost and carbon dividend. Zero.")
      rv$infoavgnetcosttextt = c("Mean net cost")
      
      
      rv$infouserfossiltext = c("Individual emission path chosen by the user in phase 4. If you chose to include non-CO2 emissions in phase 1, you should also include non-CO2 emissions in user emissions")
      rv$infouserfossiltextt = c("User emissions")
      
      
      rv$infousercosttext = c("Carbon price cost for the user. Product of users emissions and carbon price.")
      rv$infousercosttextt = c("User carbon costs")
      
      
      rv$infonetcosttext = c("User's net income. Sum of user's carbon costs and carbon dividend.")  
      rv$infonetcosttextt = c("User net cost")  
      
      rv$infoaveragedividendtext = c("Average dividend across all countries when the chosen percentage of collected carbon price revenue is always distributed as a dividend
                                 in the country it is collected in instead of full global distribution.")  
      rv$infoaveragedividendtextt = c("Mean national dividend")  
      
      rv$infocountrydividendtext = c("Dividend in the specific country")  
      rv$infocountrydividendtextt = c("Country dividend")  
      
      rv$infocountryfossiltext = c("Mean emissions across residents in the selected country")  
      rv$infocountryfossiltextt = c("Mean emissions in selected country")  
      
      rv$infocountrypoptext = c("Population in selected country")  
      rv$infocountrypoptextt = c("Population in selected country")  
      
      rv$infocountrycosttext = c("Mean carbon cost in the selected country. Product of country's mean emissions and carbon price")  
      rv$infocountrycosttextt = c("Mean carbon cost in selected country")  
      
      rv$infocountrynetcosttext = c("Mean net carbon cost in selected country. Sum of mean carbon cost in selected country and carbon dividend")  
      rv$infocountrynetcosttextt = c("Mean net carbon cost in selected country")  
      
      # rv$text = c("")  
      # rv$textt = c("")  
      
      
      rv$infopricingtext = c("First value sets the year in which global carbon pricing system starts. Second year sets the year in which global net CO2 emissions reach zero and the global warming is estimated to stop. 
                         It is assumed that fossil and land use emissions (and thus net emissions) remain same as they were in the last observed year (2021) before the pricing start year.") 
      rv$infopricingtextt = c("Pricing start year and carbon neutrality year")  
      
      rv$infoemissionsinktext = c("At carbon neutrality year, CO2 emissions and sinks must be equal, meaning zero net CO2 emissions, meaning carbon neutrality, for warming to stop.
                              Set how big the emissions and sinks are. ")  
      rv$infoemissionsinktextt = c("Emissions / sink at carbon neutrality year")  
      
      rv$infostartpricetext = c("Sets the carbon price at the first year of carbon pricing in 2015 US dollars. 
                            For default carbon price values in different budgets, figure 3.32 in page 360 of IPCC AR6 WG1 chapter 3 has been used as a loose reference point. 
                            ")
      rv$infostartpricetextt = c("Start year carbon price")
      
      rv$infoendpricetext = c("Sets the carbon price at the carbon neutrality year in 2015 US dollars. For default carbon price values in different budgets, figure 3.32 in page 360 of IPCC AR6 WG III chapter 3 has been used as a loose reference point. 
The earlier the carbon neutrality year or the higher the population projection, the higher you should consider setting
the carbon neutrality year carbon price")
      rv$infoendpricetextt = c("Neutrality year carbon price")
      
      
      rv$infostartusertext = c("Set estimate of your own emissions at the start year. They could be similar or a bit lower than your current emissions. 
                               You can use various online carbon footprint calculators to estimate your emissions. 
                               Notice that if in the carbon budget section you left out other gases than CO2, you should leave them out from this estimate as well. 
                               If you have an estimate of your emissions including non-CO2 gases, you can approximate your CO2 emissions by multiplying the estimate with 0.83")
      rv$infostartusertextt = c("User start year emissions")
      rv$infoendusertext = c("Set estimate of your emissions at the carbon neutrality year. Estimate how greatly the chosen carbon price path affects your emissions.
                             Notice that if in the carbon budget section you left out other gases than CO2, you should leave them out from this estimate as well. ")
      rv$infoendusertextt = c("User end year emissions")
      
      
      
      
      rv$infonationaldivtext = c("What if some of the carbon price revenue collected by the countries is not put to the common global pool to be distributed as a global dividend but 
                             distributed as a national dividend to the residents of the countries in which the revenue is collected?
                             This will lower the global redistribution effect of the system. Individual incentives to reduce emissions still remain, but national incentives to reduce emissions are lower.")
      rv$infonationaldivtextt = c("National carbon dividend")
      
      rv$infoconvergencetext = c("It is assumed that countries' per capita emissions converge to global average over time from their 2021 historical value. 
                             <br/>
                             Convergence factor denotes how much the emissions converge by the carbon neutrality year.
                             
                             
                             Intermediate emissions between pricing start year and carbon neutrality year are calculated 
                             as a linear or percentual trajectory, depending on the choice of total fossil emission trajectory. 
                             A correction multiplier is applied to the outputs from the calculation to have the emission sum across all individuals
                             equal the annual global CO2 emissions")
      rv$infoconvergencetextt = c("Convergence of countries' emissions")
      
      rv$infoconvergence1text = c("It is assumed that countries' per capita emissions converge to global average over time from their 2021 historical value. 
                             <br/>
                             Convergence factor denotes how much the emissions converge by the carbon neutrality year.
                             
                             
                             Intermediate emissions between pricing start year and carbon neutrality year are calculated 
                             as a linear or percentual trajectory, depending on the choice of total fossil emission trajectory. 
                             A correction multiplier is applied to the outputs from the calculation to have the emission sum across all individuals
                             equal the annual global CO2 emissions")
      rv$infoconvergence1textt = c("Convergence of countries' emissions")
      
      rv$infoconvergence2text = c("It is assumed that countries' per capita CO2 emissions converge to global average over time from their 2021 historical value. 
                             <br/>
                             Convergence factor denotes how much the emissions converge by the carbon neutrality year.
                             
                             
                             Intermediate emissions between pricing start year and carbon neutrality year are calculated 
                             as a linear or percentual trajectory, depending on the choice of total fossil emission trajectory. 
                             A correction multiplier is applied to the outputs from the calculation to have the emission sum across all individuals
                             equal the annual global emissions")
      rv$infoconvergence2textt = c("Convergence of countries' emissions")
      
      rv$infoconvergence3text = c("It is assumed that countries' per capita CO2 emissions converge to global average over time from their 2021 historical value. 
                             <br/>
                             Convergence factor denotes how much the emissions converge by the carbon neutrality year.
                             
                             
                             Intermediate emissions between pricing start year and carbon neutrality year are calculated 
                             as a linear or percentual trajectory, depending on the choice of total fossil emission trajectory. 
                             A correction multiplier is applied to the outputs from the calculation to have the emission sum across all individuals
                             equal the annual global emissions")
      rv$infoconvergence3textt = c("Convergence of countries' emissions")
      
      
      
      rv$infodatatext = HTML("CO2 emission data: Global Carbon Project 2022 (Friedlingstein et al. 2021): 
                      https://www.icos-cp.eu/science-and-impact/global-carbon-budget/2022 \n
                      <br> 
                      <br> 
                      Total emission data: PRIMAP: Gütschow, J.; Pflüger, M. (2022): The PRIMAP-hist national historical emissions time series v2.4 (1750-2021). zenodo. doi:10.5281/zenodo.7179775.
Gütschow, J.; Jeffery, L.; Gieseke, R.; Gebel, R.; Stevens, D.; Krapp, M.; Rocha, M. (2016): The PRIMAP-hist national historical emissions time series, Earth Syst. Sci. Data, 8, 571-603, doi:10.5194/essd-8-571-2016\n
 <br> 
Non-CO2 emissions calculated by deducing CO2 emissions from Total emissions. 
<br>
<br>
                      Population data: Population data and population projections from UN: 
https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_TotalPopulationBySex.zip \n \n
<br>
<br>
IPCC reports have been used for carbon budgets and ballpark for carbon prices and land use emissions / sinks. \n
<br>
<br>
Carbon budgets from tablge 5.8 in page 753 of AR6 WG1 Chapter 5. \n
<br>
IPCC, 2021: Climate Change 2021: The Physical Science Basis. Contribution of Working Group I to the Sixth Assessment Report of the Intergovernmental Panel on Climate Change[Masson-Delmotte, V., P. Zhai, A. Pirani, S.L. Connors, C. Péan, S. Berger, N. Caud, Y. Chen, L. Goldfarb, M.I. Gomis, M. Huang, K. Leitzell, E. Lonnoy, J.B.R. Matthews, T.K. Maycock, T. Waterfield, O. Yelekçi, R. Yu, and B. Zhou (eds.)]. Cambridge University Press, Cambridge, United Kingdom and New York, NY, USA, In press, doi:10.1017/9781009157896.
\n
<br>
<br>
For default carbon price values, figure 3.32 in page 360 of AR6 WG III chapter 3 has been used as a loose reference point. \n
<br>
IPCC, 2022: Climate Change 2022: Mitigation of Climate Change. Contribution of Working Group III to the Sixth Assessment Report of the Intergovernmental Panel on Climate Change [P.R. Shukla, J. Skea, R. Slade, A. Al Khourdajie, R. van Diemen, D. McCollum, M. Pathak, S. Some, P. Vyas, R. Fradera, M. Belkacemi, A. Hasija, G. Lisboa, S. Luz, J. Malley, (eds.)]. Cambridge University Press, Cambridge, UK and New York, NY, USA. doi: 10.1017/9781009157926
<br>
<br>
GDP data for 2021 from World Bank, in 2015 US dollars:
https://data.worldbank.org/indicator/NY.GDP.MKTP.KD
")
      
      rv$infodatatextt = c("Data sources")
      
      info3text = c("Start start year carbon price defines")
      
      rv$infobudgettext = c("Carbon budget defines how much net CO2 emissions can still be emitted to stay under certain temperature target with
                     a certain probability. Budgets are defined from 2020 onwards. 
                                          The simulation scenarios are built so that the budgets are depleted exactly at the chosen neutrality year. 
                     If emissions stay stable after that or continue to decrease, then the temperature targets are not exceeded.

                     2020 and 2021 emissions are known and emissions from 2022 until the year before pricing start year are assumed to equal the 2021 emissions.
This leaves a remaining budget to be used from the pricing start year until the carbon neutrality year which, together with other simulation choices, 
defines the annual fossil emissions and land use emissions/sinks.                      
                     The probability estimates for budgets only concern the Transient climate response (TCRE), or the uncertainty in the expected response of additional warming to additional cumulative CO2, and assumes a normal distribution around the central estimate. Other factors such as the level of non-CO2 induced warming, the precise level of historical warming so far, feedback effects,  bring additional uncertainty. The level of these uncertainties is expressed in the source table for carbon budgets at IPCC AR6 WG1 C5 table 5.8 (p. 753).  
")
      
      rv$infobudgettextt = c("Carbon budgets")
      
      
      
      
      
      
      
      
      
    }  else if (rv$lang =="fin") {
      
      rv$tutotextt = c("Tutoriaali")
      
      rv$tutotext = HTML("
       <b>Mikä tämä sivu on?</b> 
       <br>
Globaali hiilen hinta olisi tehokas tapa ilmastonmuutoksen pysäyttämiseen. Mikäli hiilen hinnoittelun tuotot lisäksi jaettaisiin globaalina hiiliosinkona, maailman tuloerot kaventuisivat huomattavasti. Tällä sivulla voit tutkia erilaisia päästötavoitepolkuja ja havainnoida miten vaihtelu niihin vaadittavassa hiilen hinnoittelussa vaikuttaisi tuloihisi ja laajemmin globaaliin tulonjakoon.
<br><br>


<b>Rakenne</b>
<br>
TULOKSET näkyvät graafiosiossa oikeassa alakulmassa. Päätulosten lisäksi voit myös vertailla maakohtaisia päätuloksia tai näyttää päätulokset myös taulukkomuodossa. 
<br>

 TULOSMUUTTUJAT -asetuksissa vasemmassa reunassa näet tuloksia tiivistetysti ja voit säätää minkä muuttujien tuloksia haluat näyttää päätulosten graafeissa. 
<br>

Tulokset riippuvat  valinnoistasi VALINNAT-osiossa graafiosion yläpuolella. Simulaatiovalinnoissa on budjettivalinnan lisäksi neljä vaihetta + lisävaihe maakohtaisille valinnoille. Kaikissa valinnoissa on esitäytetyt oletusarvot, joten voit myös keskittyä muuttamaan tai kokeilemaan vain niitä valintoja, joista olet eniten kiinnostunut. 
<br>
Ensin valitaan hiilibudjetti. Tämä määrittää simulaation lämpötilatavoitteen eli kunnianhimon tason. Budjettivalinta on siitä erityinen, että että se muuttaa joitain muita valintoja niiden budjettikohtaisiin oletusarvoihiin vaiheissa 1 ja 3.  
<br>

<br>
<b>Globaalit päästöt</b>
<br>
<br>
Tämän osion valinnat, yhdessä hiilibudjetin kanssa, määrittävät hiilidioksidipäästöt kullekin vuodelle. Käyttäjänä kontrolloit fossiilipäästöjen käyrän muotoa, ja maankäytön päästöt ja nielu, ja edellisten summana laskettavat nettopäästöt, reagoivat valintoihisi, niin että kumulatiiviset nettopäästöt vastaavat valittua hiilibudjettia hiilineutraaliusvuonna, kun lämpeneminen pysähtyy.  
<br>
Voit myös sisällyttää muut kasvihuonekaasut kuin hiilidioksidin mukaan järjestelmään. Mikäli teet niin, ne näkyvät yksilökohtaisissa päästöissä ja järjestelmän kustannuksissa. 
<br><br>
<b>Maailman väestö</b>
<br>
Maailman väestö -valinnassa valitset jonkun YK:N väestön määrä -projektioista määrität miten maailman väestö tulee kehittymään yli ajan simulaatiossasi. Yhdessä globaalien CO2-päästöjen kanssa väestömäärä määrittää keskipäästöt kaikkien maailman ihmisten keskuudessa.
<br>
Oletuksena on valittu mediaaniskenaario. On siis 50% todennäköisyys, että väestömäärä on skenaarion arvoja suurempi ja 50% todennäköisyys, että se on skenaarion arvoja pienempi. 
Valitsemalla projektion “jakauman ylin 20%” on 20% todennäköisyys, että väestömäärä on skenaarion arvoja suurempi. Valitsemalla “jakauman alin 5%” on 5% todennäköisyys, että väestömäärä on projektion arvoja suurempi. Jne. 
<br><br>
<b>Hiilen hinta</b>
<br>
On hyvin vaikea arvioida, että mikä hiilen hinnan taso on riittävä saavuttamaan tietyn lämpötilatavoitteen. Kannattaakin tutkailla erilaisia hiilen hintoja ja niiden vaikutusta päästömenoihin. Mikäli globaali hiilen hinta otettaisiin käyttöön, niin sitä säädettäisiin aiottua polkua tiukemmaksi tai löysemmäksi riippuen siitä, miten päästöt kehittyvät odotettuun nähden. 
<br>
<br>
<b>Käyttäjän päästöt</b>
<br>
Tässä osiossa pääset asettamaan itsellesi henkilökohtaisia päästövähennyspolkuja ja tutkailemaan millaisia vuosittaisia päästökustannuksia niistä sinulle syntyisi. 

Valitsemalla maakohtaiset päästöt VAIHTOEHTO-kohdasta voit käyttää jonkin maan kansalaisten keskimääräistä estimoitua päästövähennyspolkua omana päästövähennyspolkunasi. Voit käyttää päästöjen arvioinnissa hyödyksi myös netin hiilijalanjälkilaskureita. Tällöin on tärkeä huomioida, että sisältääkö laskuri kaikki kasvihuonekaasupäästöt vai pelkän hiilidioksidin. Mikäli olet laskenut Globaalit päästöt osiossa mukaan muut päästöt, niin sinun tulee laskea ne mukaan tässäkin.
<br><br>
<b>EXTRA: Maat</b>
<br>
Tässä osiossa voit halutessasi havainnoida sellaista globaalia hiilen hinnoittelujärjestelmää, jossa kaikkea hinnoittelun tuottoa ei jaeta tasan kaikkien maailman kansalaisten kesken, vaan valitsemasi prosenttiosuus siitä annetaankin hiiliosinkona aina vain sen maan kansalaisille, jossa se on kerätty.

Voit myös tutkia keskipäästöjen ja -kustannusten polkuja eri maissa, olettaen että maiden keskipäästöt yhtenevät ennalta määrätyn mallin mukaisesti. 
<br>
<br>

<b>Info-näppäimet </b>
<br>
Klikkaamalla käyttöliittymän infonäppäimiä saat lisätietoa eri indikaattoreista ja simulaatiovalinnoista. 

<br>
<br>

<b>Puutteet / Mitä ei ole huomioitu</b>
<br>
Hiilineutraaliuuteen riittävien hiilinielujen saavuttamisesta koituu myös kustannuksia, joita ei kuitenkaan ole simulaatiossa mallinnettu. Nieluihin voidaan myös kannustaa hiilen hinnoittelun kautta, niin että sillä voimistetaan olemassa olevaa metsittymiskehitystä ja vähennetään metsäkatoa.
<br>
Annathan palautetta sivun yläreunan feedback-linkistä, mikäli sinulle on ideoita, että miten sisällyttää maankäytön muutoksen päästöt ja nielut malliin

")
      # rv$labelfossil = "Fossiilipäästöt"
      rv$infofossiltext = c("Kaikki fossiiliset CO2-päästöt kaikissa maissa. Ei sisällä muita kasvihuonekaasupäästöjä (kuten metaania).
                        Datalähde päästöille 2021 ja ennen: Global Carbon Project 2022 (Friedlingstein et al. 2021)")
      rv$infofossiltextt = c("Fossiilipäästöt")
      
      rv$infolultext = c("Hiilidioksidipäästöt ja -nielut maankäyttösektorilla koko maailmassa. Tulevaisuudessa sisältää
                         myös teknologiset nielut, kuten DACSS ja BECCS. Lähde tiedoille 2021 saakka:   Global Carbon Project 2022 (Friedlingstein et al. 2021)")
      rv$infolultextt = c("Maankäytön päästöt ja nielut")
      
      
      rv$infonettext = c("Nettohiilidioksidipäästöt, nettona fossilipäästöistä ja maankäyttöpäästöistä ja nieluista. Lähde tiedoille 2021 saakka: Global Carbon Project 2022 (Friedlingstein et al. 2021)")
      rv$infonettextt = c("Nettohiilidioksidipäästöt") 
      
      rv$infoghgtext = c("Laskettu summaamalla yhteen Globan Carbon Projectin hiilidioksidipäästöt ja PRIMAP:in ei-CO2-päästöt")
      rv$infoghgtextt = c("Kokonaispäästöt")
      
      rv$infononco2text = c("Muut kuin hiilidioksidipäästöt, esim metaani. Lähtötiedot PRIMAP: Gütschow, J.; Pflüger, M. (2022): The PRIMAP-hist national historical emissions time series v2.4 (1750-2021). zenodo. doi:10.5281/zenodo.7179775.
Gütschow, J.; Jeffery, L.; Gieseke, R.; Gebel, R.; Stevens, D.; Krapp, M.; Rocha, M. (2016): The PRIMAP-hist national historical emissions time series, Earth Syst. Sci. Data, 8, 571-603, doi:10.5194/essd-8-571-2016")
      rv$infononco2textt = c("Ei-CO2-päästöt")
      
      rv$infononco2utext = c("Muut kuin hiilidioksidipäästöt, esim metaani. Simuloituja päästöt hiilineutraaliusvuonna vaikea arvioida, tässä rajoitettu 5-10GtCO2-ekvivalentin väliin, jotta vastaa tulkintaa IPCC-arviosta Ei-CO2-päästöistä hiilineutraaliusvuonna IPCC AR6-raportin WGIII:n luku 3:n sivulla 339 (figure 3.20).  
                             <br> Lähtötiedot historiallisille päästöille PRIMAP: Gütschow, J.; Pflüger, M. (2022): The PRIMAP-hist national historical emissions time series v2.4 (1750-2021). zenodo. doi:10.5281/zenodo.7179775.
Gütschow, J.; Jeffery, L.; Gieseke, R.; Gebel, R.; Stevens, D.; Krapp, M.; Rocha, M. (2016): The PRIMAP-hist national historical emissions time series, Earth Syst. Sci. Data, 8, 571-603, doi:10.5194/essd-8-571-2016")
      rv$infononco2utextt = c("Ei-CO2-päästöt")
      
      rv$infopoptext = c("Maailman väestö, miljardia. Tilastot ja ennusteet: YK. 
                         <br>
                        Oletuksena on valittu mediaaniskenaario. On siis 50% todennäköisyys, että väestömäärä on skenaarion arvoja suurempi ja 
                        50% todennäköisyys, että se on skenaarion arvoja pienempi. Valitsemalla esim. 80% ja
                      https://population.un.org/wpp/Graphs/Probabilistic/POP/TOT/900")
      rv$infopoptextt = c("Maailman väestö")
      
      # rv$infopoptext = c("World population, billions. Statistics and projections from United Nations. 
      #                 By default the median projection is chosen, meaning that there is a 50% chance for both smaller and greater population in the upcoming years. Choice of 95% range upper limit projection means that there is a 2.5% chance
      #                 that the future population will be greater than the projected value. Choosing a 80% range lower limit projection means that there is a 10% 
      #                 chance that the population is smaller than in the chosen projection. And so on.
      #                 https://population.un.org/wpp/Graphs/Probabilistic/POP/TOT/900")
      # rv$infopoptextt = c("World population")
      
      
      
      rv$infoavgfossiltext =c("Keskimääräiset fossiilipäästöt kaikkien maailman yksilöiden kesken. Laskettu jakamalla fossiilipäästöt maailman väestöllä. Ei sisällä maankäytön päästöjä tai nieluja")
      rv$infoavgfossiltextt =c("Keskimääräiset hiilidioksidipäästöt")
      
      
      rv$infopricetext =c("Globaali hiilen hinta, $/t, vuoden 2015 dollareina. Hinta tulee asettaa riittävän korkeaksi, jotta saavutetaan valittu päästövähennyspolku. IPCC AR6 WG III raportin luvun 3 kuvaa 3.32 on käytetty löyhänä lähtökohtana eri hiilibudjettien oletushinnoille") 
      rv$infopricetextt =c("Hiilen hinta") 
      
      
      rv$infoavgcosttext = c("Keskimääräiset menot hiilen hinta -maksuista yksilötasolla. Keskipäästöjen ja hiilen hinnan tulo")
      rv$infoavgcosttextt= c("Keskimääräiset hiilimenot")
      
      rv$infodividendtext = c("Kaikille maailman kansalaisille jaettava hiiliosinko. Vastaa keskimääräisiä menoja hiilen hinta -maksuista, sillä maksujen tulot jaetaan kaikille tasan. Keskipäästöjen ja hiilen hinnan tulo.")
      rv$infodividendtextt = c("Hiiliosinko")   
      
      
      rv$infoavgnetcosttext = c("Keskimääräinen nettomeno. Keskimääräisten hiilimenojen ja hiiliosingon summa. Nolla")
      rv$infoavgnetcosttextt = c("Keskimääräiset nettomenot")      
      
      
      rv$infouserfossiltext = c("Käyttäjän vaiheessa 4 määrittämä yksilöllinen päästöpolku")
      rv$infouserfossiltextt = c("Käyttäjän päästöt")
      
      rv$infousercosttext = c("Käyttäjän menot hiilen hinta -maksuista. Käyttäjän päästöjen ja hiilen hinnan tulo")
      rv$infousercosttextt = c("Käyttäjän hiilimenot")
      
      
      
      rv$infonetcosttext = c("Käyttäjän nettomenot. Käyttäjän hiilimaksujen ja hiiliosingon summa")  
      rv$infonetcosttextt = c("Käyttäjän nettomenot") 
      
      
      rv$infoaveragedividendtext = c("Keskimääräinen kansallinen hiiliosinko kaikissa maissa, kun valittu osuus kerätyistä hiilen hinta -tuloista on
                                     jaettu maassa, jossa kerääminen tapahtuu globaalin jaon sijaan")  
      rv$infoaveragedividendtextt = c("Keskimääräinen kansallinen hiiliosinko")     
      
      
      rv$infocountrydividendtext = c("Hiiliosinko tietyssä maasa")  
      rv$infocountrydividendtextt = c("Maan hiiliosinko")  
      
      rv$infocountryfossiltext = c("Keskipäästöt valitussa maassa")  
      rv$infocountryfossiltextt = c("Keskipäästöt valitussa massa")      
      
      rv$infocountrypoptext = c("Väestön määrä valitussa maassa")  
      rv$infocountrypoptextt = c("Väestön määrä valitussa maassa")  
      
      rv$infocountrycosttext = c("Keskimääräiset hiilen hinta -maksut valitussa maassa - maan keskipäästöjen ja hiilen hinnan tulo")  
      rv$infocountrycosttextt = c("Keskimääräiset hiilen hinta -maksut valitussa maassa")  
      
      
      rv$infocountrynetcosttext = c("Keskimääräiset nettomaksut valitussa maassa. Keskimääräisen hiilimaksun ja hiilisosingon summa")  
      rv$infocountrynetcosttextt = c("Keskimääräiset nettomaksut valitussa maassa")  
      
      
      # rv$text = c("")  
      # rv$textt = c("")  
      
      rv$infopricingtext = c("Ensimmäinen arvo asettaa arvon, jolloin globaali hiilen hinta -systeemi käynnistyy.
                             Toinen arvo asettaa vuoden, jolloin globaalit nettohiilidioksidipäästöt saavuttavat nollan ja ilmaston lämpeneminen arvioidusti pysähtyy.
                             Ennen alkuvuotta oletetaan, että fossiilipäästöt ja maankäytön päästöt ja nielut (ja siten nettopäästöt) pysyvät samana kuin ne olivat viimeisenä havaintovuonna (2021)") 
      rv$infopricingtextt = c("Hinnoittelun alkuvuosi ja hiilineutraaliusvuosi (loppuvuosi)")  
      
      
      rv$infoemissionsinktext = c("Fossiilipäästöjen ja määnkäytön päästöjen ja nielun tulee olla yhtä suuret, eli nettopäästöjen tulee olla nollassa, loppuvuonna eli hiilineutraalisuvuonna,
                                  jotta hiilineutraalius saaavutetaan ja lämpeneminen loppuu. Aseta päästöjen ja nielujen määrä samanaikaisesti")  
      rv$infoemissionsinktextt = c("Päästöt/nielu hiilineutraaliusvuonna")   
      
      rv$infostartpricetext = c("Asettaa hiilen hinnan hinnoittelujärjestelmän ensimmäisenä vuonna, vuoden 2015 dollareiden arvossa.  IPCC AR6 WG III -raportin luvun 3 kuvaa 3.32 on käytetty löyhänä lähtökohtana eri hiilibudjettien oletushinnoille")
      rv$infostartpricetextt = c("Alkuvuoden hiilen hinta")
      
      
      rv$infoendpricetext = c("Asettaa hiilen hinnan hiilineutraaliusvuonna, vuoden 2015 dollareiden arvossa.  IPCC AR6 WG III -raportin luvun 3 kuvaa 3.32 on käytetty löyhänä lähtökohtana eri hiilibudjettien oletushinnoille
                              Mitä aikaisemmin hiilineutraalius saavutetaan tai mitä korkeampi väestön määrä on, sitä korkeammaksi kannattaa asettaa neutraaliusvuoden hiilen hinta.
                              ")
      rv$infoendpricetextt = c("Neutraaliusvuoden hiilen hinta")      
      
      rv$infostartusertext = c("Aseta arvio omista päästöistäsi päästöhinnoittelun alkuvuonna. Päästösi ovat silloin todennäköisesti samansuuntaiset 
                               kuin nyt tai hieman pienemmät. Voit käyttää arviointiin esimerkiksi netin hiilijalanjälkilaskureita. 
                               Huomioi, että mikäli olet hiilibudjetti-osiossa jättänyt pois muut kaasut kuin hiilidioksidin, sinun tulee jättää ne pois myös arviossa omista päästöistäsi.
                               Mikäli sinulla on arvio päästöistäsi niin että siinä on hiilidioksidi mukana, voit saada summittaisen arvion hiilidioksidipäästöistäsi kertomalla arviosi luvulla 0,83")
      rv$infostartusertextt = c("Käyttäjän päästöt vuonna, jolloin hinnoittelujärjestelmä käynnistyy")
      rv$infoendusertext = c("Aseta arvio omista päästöistä hiilineutraalisvuonna. Tähän vaikuttaa se miten paljon valittu hiilen hinta vaikuttaa päästöihisi.
                             Huomioi, että mikäli olet hiilibudjetti-osiossa jättänyt pois muut kaasut kuin hiilidioksidin, sinun tulee jättää ne pois myös arviossa omista päästöistäsi.
                             ")
      rv$infoendusertextt = c("Käyttäjän päästöt hiilineutraaliusvuonna") 
      
      
      
      rv$infonationaldivtext = c("Mitä jos osaa hiilen hinnoitteilusta kerätyistä tuotoista ei jaettaisikaan yhteisestä potista kaikille, vaan osuus siitä jaettaisiin
                                 kansallisina hiiliosinkoina, niin että kansallisen osuuden määrä riippuu kansallisesti kerätystä määrästä. Voit valita kuinka iso osa jaettaisiin kansallisesti.
                                 Kansallinen hiiliosinko vähentää järjestelmän globaaleja tulonjakovaikutuksia. Yksilöiden insentiivit vähentää päästöjä pysyvät ennallaan, mutta kansallisten toimijoiden insentiivit vähentää niitä pienenevät")
      rv$infonationaldivtextt = c("Kansallinen hiiliosinko") 
      
      
      rv$infoconvergencetext = c("Oletetaan, että maiden keskipäästöt yhtenevät ajan yli kohti globaalia keskiarvoa vuosien 2021 historiallisia arvoistaan. 
                                 Yhtenevyyskerroin kuvaa kuinka paljon maiden päästöt yhtenevät hiilineutraaliusvuoteen mennessä.
                                 
                                 Hinnoittelun alkuvuoden ja hiilineutraaliusvuoden väliset päästöt lasketaan lineaarisena tai prosentuaalisena jatkumona, riippuen päästöpolun muodon valinnasta vaiheessa 1. 
                                 Kansallisiin tuloksiin sovelletaan korjauskerrointa, jotta kansallisten tulosten summa vastaa globaaleja päästöjä")
      rv$infoconvergencetextt = c("Maiden päästöjen yhteneminen")
      
      
      
      rv$infoconvergence1text = c("Oletetaan, että maiden keskipäästöt yhtenevät ajan yli kohti globaalia keskiarvoa vuosien 2021 historiallisia arvoistaan. 
                                 Yhtenevyyskerroin kuvaa kuinka paljon maiden päästöt yhtenevät hiilineutraaliusvuoteen mennessä.
                                 
                                 Hinnoittelun alkuvuoden ja hiilineutraaliusvuoden väliset päästöt lasketaan lineaarisena tai prosentuaalisena jatkumona, riippuen päästöpolun muodon valinnasta vaiheessa 1. 
                                 Kansallisiin tuloksiin sovelletaan korjauskerrointa, jotta kansallisten tulosten summa vastaa globaaleja päästöjä")
      rv$infoconvergence1textt = c("Maiden päästöjen yhteneminen")
      
      rv$infoconvergence2text = c("Oletetaan, että maiden keskipäästöt yhtenevät ajan yli kohti globaalia keskiarvoa vuosien 2021 historiallisia arvoistaan. 
                                 Yhtenevyyskerroin kuvaa kuinka paljon maiden päästöt yhtenevät hiilineutraaliusvuoteen mennessä.
                                 
                                 Hinnoittelun alkuvuoden ja hiilineutraaliusvuoden väliset päästöt lasketaan lineaarisena tai prosentuaalisena jatkumona, riippuen päästöpolun muodon valinnasta vaiheessa 1. 
                                 Kansallisiin tuloksiin sovelletaan korjauskerrointa, jotta kansallisten tulosten summa vastaa globaaleja päästöjä")
      rv$infoconvergence2textt = c("Maiden päästöjen yhteneminen")
      
      rv$infoconvergence3text = c("Oletetaan, että maiden keskipäästöt yhtenevät ajan yli kohti globaalia keskiarvoa vuosien 2021 historiallisia arvoistaan. 
                                 Yhtenevyyskerroin kuvaa kuinka paljon maiden päästöt yhtenevät hiilineutraaliusvuoteen mennessä.
                                 
                                 Hinnoittelun alkuvuoden ja hiilineutraaliusvuoden väliset päästöt lasketaan lineaarisena tai prosentuaalisena jatkumona, riippuen päästöpolun muodon valinnasta vaiheessa 1. 
                                 Kansallisiin tuloksiin sovelletaan korjauskerrointa, jotta kansallisten tulosten summa vastaa globaaleja päästöjä")
      rv$infoconvergence3textt = c("Maiden päästöjen yhteneminen")
      
      rv$infodatatext = HTML("CO2-päästöt: Global Carbon Project 2022 (Friedlingstein et al. 2021): 
                      https://www.icos-cp.eu/science-and-impact/global-carbon-budget/2022 \n
                      <br>
                      <br>
                       Kokonaispäästödata: PRIMAP: Gütschow, J.; Pflüger, M. (2022): The PRIMAP-hist national historical emissions time series v2.4 (1750-2021). zenodo. doi:10.5281/zenodo.7179775.
Gütschow, J.; Jeffery, L.; Gieseke, R.; Gebel, R.; Stevens, D.; Krapp, M.; Rocha, M. (2016): The PRIMAP-hist national historical emissions time series, Earth Syst. Sci. Data, 8, 571-603, doi:10.5194/essd-8-571-2016\n
  <br>
Ei-CO2-päästöt on laskettu vähentämällä kokonaispäästöistä CO2-päästöt. 
<br><br>
                      
                      Väestö-data: Population data and population projections from UN: 
https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_TotalPopulationBySex.zip \n \n
<br><br>
IPCC:N raportteja on käytety hiilibudjettien valitsemiseen ja hiilen hinnan ja maankäytön päästöjen ja nielujen oletusarvojen löyhänä lähtökohtana\n
<br><br>Hiilibudjetit IPCC AR6 WGI -raportin luvun 5 taulukosta 5.8 sivulta 753 \n
<br>IPCC, 2021: Climate Change 2021: The Physical Science Basis. Contribution of Working Group I to the Sixth Assessment Report of the Intergovernmental Panel on Climate Change[Masson-Delmotte, V., P. Zhai, A. Pirani, S.L. Connors, C. Péan, S. Berger, N. Caud, Y. Chen, L. Goldfarb, M.I. Gomis, M. Huang, K. Leitzell, E. Lonnoy, J.B.R. Matthews, T.K. Maycock, T. Waterfield, O. Yelekçi, R. Yu, and B. Zhou (eds.)]. Cambridge University Press, Cambridge, United Kingdom and New York, NY, USA, In press, doi:10.1017/9781009157896.
\n
<br><br>
IPCC AR6 WG III raportin luvun 3 kuvaa 3.32 on käytetty löyhänä lähtökohtana eri hiilibudjettien oletushinnoille \n
<br>IPCC, 2022: Climate Change 2022: Mitigation of Climate Change. Contribution of Working Group III to the Sixth Assessment Report of the Intergovernmental Panel on Climate Change [P.R. Shukla, J. Skea, R. Slade, A. Al Khourdajie, R. van Diemen, D. McCollum, M. Pathak, S. Some, P. Vyas, R. Fradera, M. Belkacemi, A. Hasija, G. Lisboa, S. Luz, J. Malley, (eds.)]. Cambridge University Press, Cambridge, UK and New York, NY, USA. doi: 10.1017/9781009157926
<br>
<br>
BKT-data vuodelle 2021 Maaimanpankilta, vuoden 2015 dollareissa:
https://data.worldbank.org/indicator/NY.GDP.MKTP.KD

")
      
      rv$infodatatextt = c("Data-lähteet")      
      
      
      
      info3text = c("Start start year carbon price defines")
      
      rv$infobudgettext = c("Hiilibudjetti määrittää, että kuinka paljno nettohiilidioksidipäästöjä voidaan vielä enintään tuottaa, jotta pysytään tietyn lämpötilatavoitteen alla tietyllä 
                            todennäköisyydellä. Budjetit on määritetty vuodeesta 2020 eteenpäin. Simulaatiot on rakennettu niin, että budjetit loppuvat valittuna hiilineutraaliusvuonna.
                            Jos päästöt pysyvät tasaisina sen jälkeen tai pienentyvät, niin lämpötilatavoitteet eivät ylity.
                            Vuosien 2021 ja 2021 päästöt ovat jo tiedossa tilastoista, ja päästöt vuodesta 2022 eteenpäin päästöhinnnoittelun alkuvuoteen
                            saakka oletetaan yhtä suuriksi kuin vuoden 2021 päästöt. 
                            Jäljelle jäävä hiilibudjetti kulutetaan hinnoittelun alkuvuoden ja hiilineutraaliusvuoden välillä, ja yhdessä muiden simulaatiovalintojen kanssa
                            se määrittää vuosittaiset fossiilipäästöt ja vuosittaiset maankäytön päästöt ja nielut. 
                            Budjettien todennäköisyysarviot koskevat vain ns. Transient climate responsea (TCRE), eli ottavat huomioon epävarmuuden joka liityy lisäpäästöjen tuomaan lisälämpenemiseen,
                            ja oletuksena on, että todennäköisyysjakauma on normaalisti jakautunut. Muut tekijät, kuten muiden päästöjen kuin hiilidioksidin tuoma lämmitysvaikutus, tarkka toistaiseksi saavutettu lämmitysvaikutus ja takaisinkytkentävaikutukset tuovat 
                            lisäepävarmuutta estimaatteihin. Näiden epävarmuuksien suuruutta on ilmaistu IPCC:n AR6 WG1 -raportin luvun 5 taulukossa 5.8 (s. 753)")
      
      rv$infobudgettextt = c("Hiilibudjetit")
      
      
      # lug = luu2()
    }
  })
  
  observeEvent(input$reset, {
    reset("resu")
  })  
  # 
  

  
  
  observeEvent(rv$lang, {
    
    if (rv$lang == "eng") {
      
      
      
      output$simpan <- renderUI({
        div(id="resu",
            
            
            
            
            
            
            
     shiny::tabsetPanel(id="nok", 
                               selected = "1. Global emissions",
                               
                               tabPanel("1. Global emissions",
                                        
                                        p("Change details to the CO2 emission trajectory corresponding to the chosen carbon budget"),
                                        
                                        
                                        # Specify the CO2 emission trajectory corresponding to the chosen carbon budget
                                        # Määritä hiilibudjettia vastaavien CO2-päästöjen polku
                                        # 
                                        # Estimate your emission progression, GIVEN the chosen carbon price
                                        # Arvioi oma päästökehityksesi, olettaen valitun hiilen hinnan kehityksen
                                        
                                        
                                        fluidRow(   class="sox",  
                                                    
                                                    # column(8, id = "luu",
                                                           
                                                           
                                                           # fluidRow(
                                                    column(4
                                                           , 
                                                           id = "luu",
                                                           
                                                           sliderInput("vuo", 
                                                                       inf("Pricing start year and carbon neutrality year", "infopricing"),
                                                                       min = 2023, max = 2100, value = c(2024, 2080), dragRange=FALSE, ticks = FALSE), 
                                                           
                                                           # div(
                                                           #   class="rad",
                                                           #   radioButtons("bud", 
                                                           #                
                                                           #                inf("Carbon budget for net CO2 emissions since start of 2020", "infobudget"), 
                                                           #                
                                                           #                
                                                           #                c("400Gt (67% likelihood to stay below 1,5C)" = 400,
                                                           #                  "500Gt (50% likelihood to stay below 1,5C)" = 500,
                                                           #                  "1150Gt (67% likelihood to stay below 2,0C)" = 1150,
                                                           #                  "1350Gt (50% likelihood to stay below 2,0C)" = 1350
                                                           #                  
                                                           #                ),selected=1150
                                                           #   )),
                                                           # p("Note: Changing the budget will reset many other values")
                                                           
                                                    ),
                                                    column(3, 
                                                            id = "luu",
                                                           
                                                                                                                # hr(),
                                                           tags$div(id="sla",numericInput("paa", 
                                                                                          inf("CO2 emissions/sink at the carbon neutrality year", "infoemissionsink"), 
                                                                                          min = 0.1, max = 30,step=.1,value=c(6))),
                                                           hr(),
                                                           radioButtons("muo", "Shape of the fossil emission curve",
                                                                        choiceNames=   list(
                                                                          "Linear" ,
                                                                          "Percentual"
                                                                        ),
                                                                        
                                                                        choiceValues= list("linear", "percentual"
                                                                        ),
                                                                        selected = "percentual"
                                                                        
                                                           ) , 
                                                           
                                                           
                                                           
                                                           
                                                    # ) 
                                                    # )
                                                    ),
                                                    
                                                    column(5,  #luu { 
                                                         style=c("margin-right: .5vw;"),

                                                           tags$div(id="sla",
                                                                    checkboxInput("nonco2", 
                                                                                  label = "Include other greenhouse gases than just CO2 (recommended)",
                                                                                  value=TRUE
                                                                                  
                                                                    )
                                                                    
                                                           ), 
                                                           div(class="slug",    
                                                               conditionalPanel(
                                                                 
                                                                 condition="input.nonco2 == 1",
                                                                 tags$div(id="sla", class="slug",  numericInput("nonco2end",
                                                                                    p(class="slug", style="font-weight: 500 !important;", inf("Non-CO2 emissions at CO2 neutrality year", 
                                                                                                                        "infononco2u")),
                                                                                                                
                                                                                                                min = 5, max = 10,step=.1,value=c(7.5)))
                                                                 
                                                                 
                                                               )),
                                                           

                                                           hr(),
                                                           
                                                           tags$div(id="sla",
                                                                    checkboxInput("advance", "Advanced: Set emission for year before start year")), 
                                                           
                                                           div(class="rad",
                                                               conditionalPanel(
                                                                 
                                                                 condition="input.advance == 1",
                                                                 p("Note: Emissions from last observed year (2021) take a linear trajectory to emissions for year before start year."),
                                                                 
                                                                 tags$div(id="sla",numericInput("fstart", label=p("CO2 emissions"),min = 0.1, max = 50,step=.1,value=c(37.1))),
                                                                 
                                                                 tags$div(id="sla",numericInput("lstart", label=p("Land use emissions"),min = -5, max = 10,step=.1,value=c(3.9))),
                                                                 
                                                                 conditionalPanel(
                                                                   condition="input.nonco2 == 1",
                                                                   tags$div(id="sla",numericInput("nonco2start", label=p("Non-CO2 emissions"),min = 0.1, max = 20,step=.1,value=c(12.3)))
                                                                 ),    
                                                                 conditionalPanel(
                                                                   condition="input.luls == 1",
                                                                   tags$div(id="sla",numericInput("sourcestart", label=p("Land source"),min = 0, max = 20,step=.1,value=c(13.6))),
                                                                   tags$div(id="sla",numericInput("sinkstart", label=p("Land sink + CCS"),min = -20, max = 0,step=.1,value=c(-9.7)))
                                                                   
                                                                   
                                                                 )            
                                                                 
                                                                 
                                                                 ),  
                                                               
                                                               hr(),
                                                               
                                                               
                                                               tags$div(id="sla",
                                                                        checkboxInput("luls", "Advanced & experimental: Include costs from Land+CCS CO2", value = FALSE)),          
                                                               
                                                               
                                                               div(class="rad",
                                                                   conditionalPanel(
                                                                     
                                                                     condition="input.luls == 1",
                                                                     # p("Note: "),
                                                                     textOutput("land6"),
                                                                     textOutput("land3"),
                                                                     textOutput("land1"),
                                                                     
                                                                     sliderInput("lulls",
                                                                                 ("Share of land emission reductions of Land+CCS net reduction, %"), 
                                                                                 min = 0, max = 100, value = 70, step=1, ticks=FALSE),
                                                                     
                                                                     textOutput("land2"),
                                                                     
                                                                     textOutput("land4"),
                                                                     textOutput("land5")
                                                                     
                                                                   )               
                                                               ),
                                                               
                                                           )   
                                                           
                     
                                                           )
                                                    
                                        ), 
                                        
                               ),
                               
                               
                               tabPanel( "2. Population projection",
                                         
                                         radioButtons("popc", 
                                                      inf("Choose UN population projection", "infopopu"), 
                                                      
                                                      c(
                                                        "95% range upper limit (12.41 billion at 2100)" = 5,
                                                        "80% range upper limit (11.65 billion at 2100)" = 4,
                                                        "Median projection (10.35 billion at 2100)" = 3,
                                                        "80% range lower limit (9.32 billion at 2100)" = 2,
                                                        "95% range lower limit (8.84 billion at 2100)" = 1
                                                      ),selected=3
                                         )
                                         
                               ),
                               
                               tabPanel("3. Carbon price",
                                        p("Carbon price should be set high enough to achieve the chosen emission targets"),
                                        
                                        fluidRow(
                                          column(6, id = "luu",
                                                 tags$div(id="sla", class="slug",  numericInput("sprice",
                                                                                                p(class="slug", style="font-weight: 500 !important;", inf("Start year carbon price, $", 
                                                                                                                                                          "infostartprice")),
                                                                                                
                                                                                                
                                                                                                min = 1, max = 1000000,step=1,value=c(50))),
                                                 hr(),
                                                 # 
                                                 # tags$div(id="sla", numericInput("sprice",
                                                 #                                 inf("Alkuvuoden hiilen hinta, $", "infostartprice"),
                                                 #                                 inf("Muut päästöt hiilineutraaliusvuonna",
                                                 #                                     tags$div(id="sla",numericInput("eprice",
                                                 
                                                 
                                                 
                                                 tags$div(id="sla",numericInput("eprice", 
                                                                                inf("Neutrality year carbon price, $", "infoendprice"), 
                                                                                
                                                                                
                                                                                min = 1, max = 1000000,step=1,value=c(500))),
                                                 
                                                 
                                          ),
                                          
                                          column(6, 
                                                 radioButtons("pri", "Shape of the price curve",
                                                              choiceNames=list(
                                                                "Linear",
                                                                "Percentual"
                                                                ,
                                                                "Logarithmic"
                                                              ), 
                                                              choiceValues= list("linear", "percentual", "logarithmic")
                                                              
                                                 )
                                          )
                                        )
                                        
                               ),
                               
                        # Specify the CO2 emission trajectory corresponding to the chosen carbon budget
                        # Määritä hiilibudjettia vastaavien CO2-päästöjen polku
                        # 
                        # Estimate your emission progression, GIVEN the chosen carbon price
                        # Arvioi oma päästökehityksesi, olettaen valitun hiilen hinnan kehityksen               
                               
                               tabPanel("4. User emissions", 
                                        # Arvioi oma päästökehityksesi, olettaen valitun hiilen hinnan kehityksen               
                                        
                                     p("Estimate your emission progression, GIVEN the chosen carbon price"),   
                                        fluidRow(
                                          
                                          column(4, id = "luu",
                                                 tags$div(id="sla",numericInput("indi1", inf("Start year user emissions, t", "infostartuser"),min = .01, max = 40,step=.01,value=c(8.88))),
                                                 hr(),
                                                 tags$div(id="sla",numericInput("indi2", label=inf("Neutrality year user emissions, t","infoenduser" ),min = .01, max = 40,step=.01,value=c(1.5)))
                                                 
                                                 # hr(),
                                                 
                                          ), 
                                          column(4, id = "luu",
                                                 
                                                 radioButtons("muoindi", "Shape of user emission curve",
                                                              choiceNames=   list(
                                                                "Linear" ,
                                                                "Percentual" 
                                                                
                                                              ), 
                                                              choiceValues= list("linear", "percentual"),
                                                              selected = "percentual"
                                                              
                                                 )
                                                 
                                          ), 
                                          column(4,
                                                 selectInput("indi",
                                                             inf("ALTERNATIVE: Use country average emission path as your emission path (this will slower the page considerably) ", "infoconvergence"), 
                                                             
                                                             choices =c("none", paaco$country), selected="none", selectize=TRUE),
                                                 conditionalPanel(
                                                   
                                                   condition="input.indi != 'none'",
                                                   sliderInput("con",
                                                               
                                                               inf("Convergence of countries emissions", "infoconvergence1"), 
                                                               min = .01, max = 1, value = .5, step=.01)
                                                 )  ) )
                               )   
                               ,
                               tabPanel("EXTRA: Countries",
                                        fluidRow(
                                          column(4, id = "luu",
                                                 sliderInput(
                                                   inputId = "national",
                                                   inf("Allocate a percentage of collected carbon revenue nationally", "infonationaldiv")
                                                   
                                                   , min = 0, max = 100, value = 0, step=1, ticks=FALSE
                                                 ), 
                                                 conditionalPanel(
                                                   condition="input.national != 0",
                                                   hr(),
                                                   
                                                   
                                                   selectInput("nationalcoun", label = "User country of residence for national dividend (This will slower the page considerably)", choices =c("none", paaco$country), selected="none"),
                                                   
                                                 )),
                                          
                                          
                                          
                                          
                                          column(4,
                                                 tags$div(id ="countrr",
                                                          
                                                          pickerInput(
                                                            inputId = "countr",
                                                            label = "Show indicators for specific country/countries (This will slower the page considerably)",
                                                            choices = c(paaco$country),
                                                            selected= NULL,
                                                            options = pickerOptions(
                                                              `actions-box` = TRUE,container = "body"),
                                                            multiple = TRUE
                                                            
                                                          ))
                                                 
                                          ),
                                          column(4, 
                                                 conditionalPanel(
                                                   condition='output.lek == 1',
                                                   # condition="rv$lek == 1",
                                                   hr(),
                                                   
                                                   sliderInput("conb",
                                                               inf("Convergence of countries emissions", "infoconvergence2"), 
                                                               min = .01, max = 1, value = .5, step=.01)
                                                   
                                                 ) 
                                          )
                                        )
                               )
                               
                               
                               
            ) )}
      )
    } else if (rv$lang == "fin") {
      
      output$simpan <- renderUI({
        
        div(id="resu",
            
            
            tabsetPanel(id="nok",
                        
                        

  
                        
                        
                        selected = "1. Globaalit päästöt",
                        
                        tabPanel("1. Globaalit päästöt",
                                 p("Aseta yksityiskohtia hiilibudjettia vastaavien CO2-päästöjen polulle"),
                                 
                                 
                                 fluidRow(   class="sox",  
                                             # column(8,  
                                                    # id = "luu",
                                                 
                                                    # fluidRow(  
                                             column(4
                                                    , 
                                                    id = "luu",
                                                    
                                                    sliderInput("vuo", 
                                                                inf("Hinnoittelun aloitusvuosi ja hiilineutraalisuvuosi", "infopricing"),
                                                                min = 2023, max = 2100, value = c(2024, 2080), dragRange=FALSE, ticks = FALSE),
                                                    
                                                    
                                                    # div(
                                                    #   class="rad",
                                                    #   radioButtons("bud", 
                                                    #                
                                                    #                inf("Hiilibudjetti CO2-nettopäästöille vuoden 2020 alusta lähtien", "infobudget"),
                                                    #                
                                                    #                
                                                    #                c("400Gt (67% todennäköisyys pysyä alle 1,5C)" = 400,
                                                    #                  "500Gt (50% todennäköisyys pysyä alle 1,5C)" = 500,
                                                    #                  "1150Gt (67% todennäköisyys pysyä alle 2,0C)" = 1150,
                                                    #                  "1350Gt (50% todennäköisyys pysyä alle 2,0C)" = 1350
                                                    #                ),selected=1150
                                                    #   )),
                                                    # p("Huom: Budjetin muuttaminen resetoi monia muita valintoja")
                                                    
                                             ),
                                             column(4, 
                                                    
                                                    id = "luu",
                                                    # hr(),
                                                    
                                                    tags$div(id="sla",numericInput("paa", 
                                                                                   inf("CO2-päästöt/nielu loppuvuonna", "infoemissionsink"), 
                                                                                   min = 0.1, max = 30,step=.1,value=c(6))),
                                                    
                                                    hr(),
                                                    
                                                    radioButtons("muo", "Fossiilipäästöjen käyrän muoto",
                                                                 choiceNames=   list(
                                                                   "Lineaarinen" ,
                                                                   "Prosentuaalinen"
                                                                 ),
                                                                 
                                                                 choiceValues= list("linear", "percentual"
                                                                 ),
                                                                 selected = "percentual"
                                                                 
                                                    ) 
                                                    
                                                    
                                                    
                                             # )) 
                                             ),
                                             
                                             
                                             
                                             column(4,  
                                                    tags$div(id="sla",
                                                             checkboxInput("nonco2",  label = "Sisällytä muutkin kasvihuonekaasut kuin CO2 (suositeltua)"
                                                                   , value=TRUE        
                                                             )
                                                             
                                                    ), 
                                                    div(class="slug",    
                                                        conditionalPanel(
                                                          
                                                          condition="input.nonco2 == 1",
                                                          tags$div(id="sla", class="slug",  numericInput("nonco2end",
                                                                                                         p(class="slug", style="font-weight: 500 !important;",
                                                                                                           inf("Muut päästöt hiilineutraaliusvuonna", 
                                                                                                               "infononco2u")),
                                                                                                         
                                                                                                         min = 5, max = 10,step=.1,value=c(7.5)))
                                                          
                                                          
                                                        )),
                                                    
                                                    hr(),
                                                    
                                                    tags$div(id="sla",
                                                             checkboxInput("advance", "Aseta päästöt vuodelle ennen alkuvuotta")), 
                                                    
                                                    div(class="rad",
                                                        conditionalPanel(
                                                          
                                                          condition="input.advance == 1",
                                                          p("Huom: Päästöt etenevät lineaarisesti viimeisestä havaintovuodesta (2021) hinnoittelun alkua edeltävään vuoteen. Oletusarvoina on vuoden 2021 arvot."),
                                                          
                                                          tags$div(id="sla",numericInput("fstart", label=p("CO2-päästöt"),min = 0.1, max = 50,step=.1,value=c(37.1))),
                                                          
                                                          tags$div(id="sla",numericInput("lstart", label=p("Maankäytön päästöt"),min = -5, max = 10,step=.1,value=c(3.9))),
                                                          
                                                          conditionalPanel(
                                                            condition="input.nonco2 == 1",
                                                            tags$div(id="sla",numericInput("nonco2start", label=p("Muut päästöt"),min = 0.1, max = 20,step=.1,value=c(12.3)))
                                                          )    )               
                                                    )   )
                                             
                                 ), 
                                 
                        ),
                        
                        tabPanel(
                          
                          "2. Maailman väestö",
                          
                          
                          
                          radioButtons("popc", 
                                       inf("Valitse YK:n väestöprojektio", "infopopu"), 
                                       
                                       c(
                                         "95% jakauman yläraja (12.41 miljardia vuonna 2100)" = 5,
                                         "80% jakauman yläraja (11.65 miljardia vuonna 2100)" = 4,
                                         "Mediaani-projektio (10.35 miljardia vuonna 2100)" = 3,
                                         "80% jakauman alaraja (9.32 miljardia vuonna  2100)" = 2,
                                         "95% jakauman alaraja (8.84 miljardia vuonna 2100)" = 1
                                       ),selected=3
                          )
                          
                        ),
                        
                        tabPanel("3. Hiilen hinta",
                                 
                                 p("Hiilen hinta tulisi asettaa tarpeeksi korkeaksi, että sen avulla saavutetaan valitut päästötavoitteet"),
                                 
                                 
                                 fluidRow(
                                   column(6, id = "luu",
                                          tags$div(id="sla", numericInput("sprice",
                                                                          inf("Alkuvuoden hiilen hinta, $", "infostartprice"), 
                                                                          
                                                                          min = 1, max = 1000000,step=1,value=c(50))),
                                          hr(),
                                          
                                          tags$div(id="sla",numericInput("eprice", 
                                                                         inf("Loppuvuoden hiilen hinta, $", "infoendprice"), 
                                                                         
                                                                         
                                                                         min = 1, max = 1000000,step=1,value=c(500))),
                                          
                                          
                                   ),
                                   
                                   column(6, 
                                          radioButtons("pri", "Hintakäyrän muoto",
                                                       choiceNames=list(
                                                         "Lineaarinen",
                                                         "Prosentuaalinen"
                                                         ,
                                                         "Logaritminen"
                                                       ), 
                                                       choiceValues= list("linear", "percentual", "logarithmic")
                                                       
                                          )
                                   )
                                 )
                                 
                        ),
                        
                        
                        tabPanel("4. Käyttäjän päästöt",
                                 
                                 # Arvioi oma päästökehityksesi, olettaen valitun hiilen hinnan kehityksen               
                                 
                                 p("Arvioi oma päästökehityksesi, olettaen valitun hiilen hinnan kehityksen"),   
                                 
                                 fluidRow(
                                   
                            
                                   
                                   column(4, id = "luu",
                                          tags$div(id="sla",numericInput("indi1", label=inf("Alkuvuoden päästöt käyttäjälle, t", "infostartuser"),min = .01, max = 40,step=.01,value=c(8.88))),
                                          hr(),
                                          
                                          tags$div(id="sla",numericInput("indi2", label=inf("Loppuvuoden päästöt käyttäjälle, t","infoenduser" ),min = .01, max = 40,step=.01,value=c(1.5)))
                                          
                                          # hr(),
                                          
                                   ), 
                                   column(4, id = "luu",
                                          
                                          radioButtons("muoindi", "Käyttäjän päästöjen käyrän muoto",
                                                       choiceNames=   list(
                                                         "Lineaarinen" ,
                                                         "Prosentuaalinen"
                                                         
                                                       ), 
                                                       choiceValues= list("linear", "percentual"),
                                                       selected = "percentual"
                                                       
                                          )
                                          
                                   ), 
                                   column(4,
                                          selectInput("indi",
                                                      inf("VAIHTOEHTO: Käytä tietyn maan keskipäästöjä (tämä hidastaa sivua huomattavasti) ", "infoconvergence"), 
                                                      
                                                      choices =c("none", paaco$country), selected="none"),
                                          conditionalPanel(
                                            
                                            condition="input.indi != 'none'",
                                            sliderInput("con",
                                                        
                                                        inf("Maiden päästöjen yhdentymsien aste", "infoconvergence1"), 
                                                        min = .01, max = 1, value = .5, step=.01)
                                          )  ) )
                        )   
                        ,
                        
                        
                        
                        tabPanel("EXTRA: Maat",
                                 fluidRow(
                                   column(6, id = "luu",
                                          sliderInput(
                                            inputId = "national",
                                            inf("Jaa osuus kerätyistä hiilen hinnoittelutuloista kansallisesti", "infonationaldiv")
                                            
                                            , min = 0, max = 100, value = 0, step=1, ticks = FALSE
                                          ), 
                                          conditionalPanel(
                                            condition="input.national != 0",
                                            hr(),
                                            
                                            
                                            selectInput("nationalcoun", label = "Käyttäjän asuinmaa kansallista osinkoa varten (tämä hidastaa sivua huomattavasti)", choices =c("none", paaco$country), selected="none"),
                                            
                                          ),
                                          conditionalPanel(
                                            condition='output.lek',
                                            # condition="rv$lek == 'true'",
                                            
                                            hr(),
                                            
                                            sliderInput("conb",
                                                        inf("Maiden päästöjen yhdentymisen aste", "infoconvergence2"), 
                                                        min = .01, max = 1, value = .5, step=.01, ticks=FALSE)
                                            
                                          )),
                                   column(6,
                                          tags$div(id ="countrr",
                                                   
                                                   pickerInput(
                                                     inputId = "countr",
                                                     label = "Näytä indikaattorit tietylle maalle/maille (tämä hidastaa sivua huomattavasti)",
                                                     choices = c(paaco$country),
                                                     selected= NULL,
                                                     options = pickerOptions(
                                                       `actions-box` = TRUE,container = "body"),
                                                     multiple = TRUE
                                                     
                                                   ))
                                          
                                   )
                                 )
                        )
                        
                        
            )    )}
      )
      
    }
    
    
  })
  
  
  
  # update user emissions based on whether non-co2 emissions are included or not
  observeEvent(input$nonco2, {
    if (input$nonco2 ==1 & rv$pressed ==FALSE) {
      updateNumericInput(
        inputId = "indi1",
        value = input$indi1+1.5
      )
      updateNumericInput(
        inputId = "indi2",
        value = input$indi2+.7
      )
      
      rv$pressed = TRUE
    }
    else  if (input$nonco2 ==0 & rv$pressed ==TRUE & input$indi1 >= 1.5 & input$indi2 >=.7) {
      updateNumericInput(
        inputId = "indi1",
        value = input$indi1-1.5
      )
      updateNumericInput(
        inputId = "indi2",
        value = input$indi2-.7
      )
      
      rv$pressed =FALSE
    }
  })
  
  
  
  
  # infodatatext = c("Start start year carbon price defines")
  
  infolist = c("info3", "info4", "infofossil", "infolul", "infonet", "infoghg", "infononco2","infononco2u", "infopop",
               "infoavgfossil","infoprice", "infoavgcost","infodividend",
               "infoavgnetcost", "infouserfossil", "infousercost","infostartuser","infoenduser", "infonetcost", "infoaveragedividend", "infocountrydividend", "infocountryfossil",
               "infocountrypop","infocountrycost","infocountrynetcost",
               "infodata", "infobudget","infoemissionsink","infostartprice","infoendprice",
               "infopricing", "infopopu", "infoconvergence", "infoconvergence1", "infoconvergence2","infoconvergence3", "infonationaldiv")
  
  
  
  
  
  
  output$tutori = renderUI({
    rv$tutotext
  })
  output$tutorit = renderText({
    paste0(rv$tutotextt)
  })
  
  
  
  
  
  observeEvent(input$tutorial, {
    updateBox("tutobox", action = "restore")
  })
  
  # observeEvent(input$mobile, {
  #   if (input$mobile  == TRUE) {
  #     rv$lihh =.8
  #   } else {
  #     rv$lihh=.99
  #   }
  #   
  # })
  # 
  
  ogg <- observe({
    updateBox("tutobox", action = "remove")
    # shinyjs::click("add")
    ogg$destroy() # destroy observer as it has no use after initial button click
  })
  
  osg <- observe({
    click("go")
    # updateBox("tutobox", action = "remove")
    # shinyjs::click("add")
    osg$destroy() # destroy observer as it has no use after initial button click
  })
  
  observeEvent(input$tutorial, {
    updateBox("tutorialbox", action = "restore")
  })
  
  output$info = renderText({
    rv$info
  })
  
  output$infot = renderText({
    # div( style = ' font-size: .5vw',
    paste0("Info: ",rv$infot)
    # )
  })
  
  lapply(
    X = infolist,
    FUN = function(i){
      observeEvent(input[[paste0(i)]], {
        updateBox("infobox", action = "restore")
        rv$info = rv[[paste0(i,"text")]]
        rv$infot = rv[[paste0(i,"textt")]]
      })})
  
  o <- observe({
    updateBox("infobox", action = "remove")
    # shinyjs::click("add")
    o$destroy() # destroy observer as it has no use after initial button click
  })
  

#   ogger <- observeEvent( rv$triggor, {
#     if (rv$triggor==2) {
# 
#   shinyjs::disable("go")
# rv$triggo=0
#   ogger$destroy()
# 
#     } else { }
#     # }
#   }
#   )
  
  #  
  # initially select carbon budget from left menu
  # o <- observe({
  #   # observeEvent(input[[paste0(i)]], {
  #   updateTabsetPanel(session, id = "nok", selected = "1. Global emissions")
  #   # shinyjs::click("add")
  #   o$destroy() # destroy observer as it has no use after initial button click
  # })
  # 
  
  # ok <- observe({
  #   # observeEvent(input[[paste0(i)]], {
  #   updateRadioButtons(session,  "view", selected = "2")
  #   # shinyjs::click("add")
  #   ok$destroy() # destroy observer as it has no use after initial button click
  # })
  
  # 
  ok <- observe({
    # observeEvent(input[[paste0(i)]], {
    if (input$dim[1] > 1200) {
      rv$view==4
      
      updateRadioButtons(session,  "view", selected = "4")
    } else {
      rv$view==3
      
      # if (input$dimension > 1000) {
      updateRadioButtons(session,  "view", selected = "3")
      
    }
    # shinyjs::click("add")
    ok$destroy() # destroy observer as it has no use after initial button click
  })
  
  
  
  observeEvent(rv$fyear, {
    
    
    if (rv$lang == "eng") {
      updateCheckboxInput(session, "advance", label=paste0("Advanced: Set emission for year before start year (", rv$fyear-1,")"))
      
      
    }
    
    
    
    else if (rv$lang =="fin")
    {
      updateCheckboxInput(session, "advance", label=paste0("EXTRA (aseta päästöt vuodelle ennen alkuvuotta (", rv$fyear-1,")"))
      
      
      
    }
    
  })  
  
  
  

  # })
  # 
  bgc = hsv(.13,.13,.93)
  
  observeEvent(input$dark_mode, {
    if (input$dark_mode == TRUE) {
      
      rv$bgc = "#2f3c44"
      rv$teksvari = "white"
      rv$obsvari = "lightgreen"

      rv$green ="lightgreen"
      rv$red ="#db8aa7"
      
    } else {
      rv$bgc = "#D3DDE0"
      rv$teksvari = "#707070"
      rv$obsvari = "darkgreen"
      rv$green ="#3d9448"
      rv$red ="#b32758"
    }
    
  })
  # observeEvent(input$dark, {
  #   if (input$dark == TRUE) {
  #     rv$bgc = hsv(.3,.3,.3)
  #   } else {
  #     rv$bgc = hsv(.63,.13,.83)
  #     
  #   }
  # 
  # })
  
  
  lllist =c("country","bud","vuo","paa", "muo", "sprice", "eprice","pri" ,"indi1" , "indi2", "muoindi", "indi","popc","con")
  
  observeEvent(input$bud,{
    
    rv$budget = input$bud
    
    
    if (rv$lang=="eng") {
      
      
      # if (rv$lang == "eng") {
      
      # observeEvent(input$bud, {
      
      if (input$bud == 400) {
        rv$budinfo = c("1,5°C with 67% chance")
        
      } else if (input$bud ==500) {
        
        rv$budinfo = c("1,5°C with 50% chance")
      } else if (input$bud ==1150) {
        
        rv$budinfo = c("2°C with 67% chance")
      }
      else if (input$bud ==1350) {
        
        rv$budinfo = c("2°C with 50% chance")
      }
      # })
      
    } else if (rv$lang=="fin") {
      
      # observeEvent(input$bud, {
      if (input$bud == 400) {
        
        rv$budinfo = c("1,5°C 67% varmasti")
        
      } else if (input$bud ==500) {
        
        rv$budinfo = c("1,5°C 50% varmasti")
      } else if (input$bud ==1150) {
        
        rv$budinfo = c("2°C 67% varmasti")
      }
      else if (input$bud ==1350) {
        
        rv$budinfo = c("2°C 50% varmasti")
      } 
      # }) 
    }
    
    
    
    
    
  })
  
  observe({
    lapply(lllist, function(x) {
      observe({
        input[[x]]
        rv$lastin <- x
      })
    })
  })
  
  observeEvent(rv$lek,{
    if (rv$lek==TRUE){
      rv$alert4 =TRUE
      
      rv$showcountryfossil=TRUE   
      rv$showcountrycost=TRUE   
      rv$showcountrynetcost=TRUE   
      rv$showcountrypop=TRUE   
      
      
      
      # o$destroy()
    } else if (rv$lek==FALSE) {
      rv$showcountryfossil=FALSE  
      rv$showcountrycost=FALSE   
      rv$showcountrynetcost=FALSE   
      rv$showcountrypop=FALSE   
      rv$alert4 =FALSE
      
    }
  })
  
  
  
  # observeEvent()
  
  # observeEvent(input$national, {
  #   
  #   if (input$national != 0) {
  #     rv$showaveragedividend=TRUE
  #   }
  #   
  # })
  
  
  observeEvent(input$nationalcoun, {
    
    if (input$nationalcoun %in% c(ll2)) {
      rv$showcountrydividend=TRUE
    } else {
      rv$showcountrydividend=FALSE
      
    }
    
  })
  
  
  
  
  orr = observeEvent(input$national,{
    if (input$national != 0){
      rv$alert6 =TRUE
      
      # orr$destroy()
      rv$showaveragedividend=TRUE
      
    }
    if (input$national == 0){
      rv$alert6 =FALSE
      rv$showaveragedividend=FALSE
      
    }
    
  })
  
  
  
  
  
  orrb = observeEvent(input$nonco2,{
    if (input$nonco2 == 1){
      rv$alert8 =TRUE
      rv$shownonco2=TRUE
      rv$showghg=TRUE
      # orrb$destroy()
    }
    
    if (input$nonco2 == 0){
      rv$alert8 =FALSE
      rv$shownonco2=FALSE
      rv$showghg=FALSE
      # orrb$destroy()
    }
  })
  
  orrbb = observeEvent(input$luls, {
    if (input$luls == 1){
      rv$alert10 =TRUE
      rv$shownewsink=TRUE
      rv$showlandcost=TRUE
      # orrb$destroy()
    }
    
    if (input$luls == 0){
      rv$alert10 =FALSE
      
      rv$shownewsink=FALSE
      rv$showlandcost=FALSE
      # orrb$destroy()
    }
  })
  
  
  
  okku= observeEvent(rv$warn, {
    
    # req(input$budget, cancelOutput = FALSE)
    # freezeReactiveValue(input, "budget")
    
    if (rv$warn >=0){
      
      # 2.1515 3.0287 1.1981 1.8475
      # if (input$last_btn =="1350") {
      
      if ((rv$warn > 2.1515 & rv$warn < 2.1516 ) | (rv$warn > 3.0287 & rv$warn < 3.0288 ) | (rv$warn > 1.198 & rv$warn < 1.199 ) | (rv$warn > 1.8475 & rv$warn < 1.8476 ) | (rv$warn > 0.1625 & rv$warn < 0.1626 )) {
        
      } else {
        showNotification("Increasing land use emissions may be unrealistic. Consider setting later carbon neutrality year or increasing emissions/sink at the carbon neutrality year", duration =17)
        # rv$alert5 =TRUE
        
        okku$destroy()
      }
    }
    
  })
  
  
  # okk= observeEvent(input$nok, {
  #   if (input$nok =="4. User emissions"){
  #     showNotification("If the graph feels too crowded, hide some indicators from the RESULT VISIBILIY section at the left of the graph", duration =12)
  #     # rv$alert5 =TRUE
  #     
  #     okk$destroy()
  #     
  #   }
  #   
  # })
  
  observeEvent(rv$rateli, {
    rateli <- rv$rateli
    ratepr <- rv$ratepr
    
    if (rv$lang == "eng") {
      # Can also set the label and select items
      updateRadioButtons(session, "muo","Shape of the fossil emission curve",
                         # label = paste("Shape of the emission curve"),
                         choiceNames = list(paste0("Linear (", format(round(rateli,2)), " Gt each year)"),
                                            paste0("Percentual (", format(round( ratepr,2)), " % each year)")
                                            # ,
                                            # c("Double exponentiaul")
                                            
                         ),
                         choiceValues = list("linear", "percentual"
                                             # , "exponential"
                         ),
                         selected = rv$muosel)
    }
    else if (rv$lang =="fin")
    {
      updateRadioButtons(session, "muo","Fossiilipäästöjen käyrän muoto",
                         choiceNames = list(paste0("Lineaarinen (", format(round(rv$rateli,2)), " Gt per vuosi)"),
                                            paste0("Prosentuaalinen (", format(round( rv$ratepr,2)), " % per vuosi)")
                                            # ,
                                            # c("Double exponentiaul")
                                            
                         ),
                         choiceValues = list("linear", "percentual"
                                             # , "exponential"
                         ),
                         selected = rv$muosel
                         
      )  
      
    }
    
  })
  
  observeEvent(rv$ratelii, {
    ratelii <- rv$ratelii
    ratepri <- rv$ratepri
    
    if (ratelii >=0) {
      sig = "+"
    } else {sig =""}
    
    if (rv$lang=="eng") {
      updateRadioButtons(session, "muoindi","Shape of user emission curve",
                         choiceNames = list(paste0("Linear (",sig, format(round(ratelii,2)), " t each year)"), 
                                            paste0("Percentual (",sig, format(round( ratepri,2)), " % each year)")
                         ),
                         choiceValues = list("linear", "percentual"), 
                         
                         selected = rv$muoseli
                         
      ) }  else if (rv$lang=="fin") {
        
        
        updateRadioButtons(session, "muoindi","Käyttäjäpäästöjen käyrän muoto",
                           choiceNames = list(paste0("Lineaarinen (",sig, format(round(ratelii,2)), " t per vuosi)"), 
                                              paste0("Prosentuaalinen (",sig, format(round( ratepri,2)), " % per vuosi)")
                           ),
                           choiceValues = list("linear", "percentual"), 
                           
                           selected = rv$muoseli
                           
        )
        
      }
    
  })
  
  observeEvent(rv$ratelip, {
    ratelip <- rv$ratelip
    rateprp <- rv$rateprp
    
    if (ratelip >=0) {
      sig = "+"
    } else {sig =""}
    
    if (rv$lang =="eng") {
      updateRadioButtons(session, "pri","Shape of the carbon price curve",
                         # label = paste("Shape of the emission curve"),
                         choiceNames = list(paste0("Linear (",sig, format(round(ratelip,2)), " $ each year)"), 
                                            paste0("Percentual (",sig, format(round( rateprp,2)), " % each year)")
                                            , "Logarithmic"
                         ),
                         choiceValues = list("linear", "percentual"
                                             , "logarithmic"
                         )  , 
                         selected = rv$muopri)
      
    }  else if (rv$lang=="fin") {
      updateRadioButtons(session, "pri","Hiilen hinta -käyrän muoto",
                         # label = paste("Shape of the emission curve"),
                         choiceNames = list(paste0("Lineaarinen (",sig, format(round(ratelip,2)), " $ per vuosi)"), 
                                            paste0("Prosentuaalinen (",sig, format(round( rateprp,2)), " % per vuosi)")
                                            , "Logaritminen"
                         ),
                         choiceValues = list("linear", "percentual"
                                             , "logarithmic"
                         )  , 
                         selected = rv$muopri
      )
      
      
    }                   
    
    
    
  })
  
  
  
  
  
  
  
  
  
  
  observeEvent(rv$ratelii, {
    ratelii <- rv$ratelii
    ratepri <- rv$ratepri
    
    if (ratelii >=0) {
      sig = "+"
    } else {sig =""}
    updateRadioButtons(session, "muoindi","Käyttäjäpäästöjen käyrän muoto",
                       choiceNames = list(paste0("Lineaarinen (",sig, format(round(ratelii,2)), " t per vuosi)"), 
                                          paste0("Prosentuaalinen (",sig, format(round( ratepri,2)), " % per vuosi)")
                       ),
                       choiceValues = list("linear", "percentual"), 
                       
                       selected = rv$muoseli
                       
    )
  })
  
  observeEvent(rv$ratelip, {
    ratelip <- rv$ratelip
    rateprp <- rv$rateprp
    
    if (ratelip >=0) {
      sig = "+"
    } else {sig =""}
    updateRadioButtons(session, "pri","Hiilen hinta -käyrän muoto",
                       # label = paste("Shape of the emission curve"),
                       choiceNames = list(paste0("Lineaarinen (",sig, format(round(ratelip,2)), " $ per vuosi)"), 
                                          paste0("Prosentuaalinen (",sig, format(round( rateprp,2)), " % per vuosi)")
                                          , "Logaritminen"
                       ),
                       choiceValues = list("linear", "percentual"
                                           , "logarithmic"
                       )  , 
                       selected = rv$muopri
    )
    
  })
  
  # radioButtons("muoindi", "Shape of user emission curve",
  #              choiceNames=   list(
  #                "Linear" ,
  #                "Percentual" 
  #                # ,
  #                # "Logarithmic drop" = "logarithmic"
  #              ), 
  #              choiceValues= list("linear", "percentual")
  #              
  # ),
  
  # observeEvent(rv$rateli, {
  #   rateli <- rv$rateli
  #   ratepr <- rv$ratepr
  #   
  #   # Can also set the label and select items
  #   updateRadioButtons(session, "muo","Shape of the emission curve",
  #                      # label = paste("Shape of the emission curve"),
  #                      choiceNames = list(paste0("Linear drop (", format(round(rateli,1))), paste0("Percentual drop (", format(round( ratepr*100,0)))),
  #                      choiceValues = list("linear", "percentual")
  #                      
  #                      # c((paste0("Linear drop",r)="linear"), 
  #                      #   (paste0("Percentual drop", r)="percentual")),
  #                      # selected = 
  #   )
  # })
  # 
  # Shape of the emission curve",
  # c(
  #   "Linear drop" = "linear",
  #   "Percentual drop" = "percentual"
  
  observeEvent(input$fin,{
    rv$lang = c("fin")
  }    )
  
  observeEvent(input$eng,{
    rv$lang = c("eng")
  }    )
  
  observeEvent(input$popc,{
    rv$popc = input$popc
    
  }    )
  
  
  
  observeEvent(input$con, {
    updateSliderInput(
      inputId = "conb",
      value = input$con
    )
    updateSliderInput(
      inputId = "conc",
      value = input$con
    )
  })
  
  observeEvent(input$conb, {
    updateSliderInput(
      inputId = "con",
      value = input$conb
    )
    updateSliderInput(
      inputId = "conc",
      value = input$conb
    )
  })
  observeEvent(input$conc, {
    updateSliderInput(
      inputId = "con",
      value = input$conc
    )
    updateSliderInput(
      inputId = "conb",
      value = input$conc
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
  
  observeEvent(input$autodraw, {
    if (input$autodraw == TRUE) {
      shinyjs::disable("go")
      
    }
    else if (input$autodraw ==FALSE){
      shinyjs::enable("go")
      
    }
  })
  
 #  ogger <- observe({
 #    # updateBox("tutobox", action = "remove")
 #    # shinyjs::click("add")
 #    
 #    if (isolate(skip()==1)) {
 #      # skip first reactive sequence
 #      skip(0)
 #      # launch next reactive sequence
 #      invalidateLater(1000,session)
 #    } else {
 #      # hypothetical expensive computation
 #      # Sys.sleep(2)
 #      # 
 #      # # hypothetical plot
 #      # hist(rnorm(20))
 #      shinyjs::disable("go")
 #      
 #      ogger$destroy()
 #      
 #    }
 #    
 # # destroy observer as it has no use after initial button click
 #  })
  
# 
  #  eventReactive(input$go, {
  # 
  #    rv$triggo = 0
  # })
  
# rv$triggo = eventReactive(input$go, {
#      
#      0
#    })
#    
  observeEvent(input$go, {

    rv$triggo=0

  })
  
  observeEvent(rv$triggo, {
    
    if (rv$triggo==0) {
      shinyjs::disable("go")
      
    }
    else if (rv$triggo==1){
      
      if (input$autodraw==FALSE) {
      
      shinyjs::enable("go")
      }
    }
  })
  
  observeEvent(input$dim[1], {
    
    rv$triggo=1
    # click("go")
    
  })
  observeEvent( input$dim[2], {
    
    rv$triggo=1
    # click("go")
    
    
  })
  
  # input$dim[1], input$dim[2]
  
  
  
  
  # observeEvent(input$last_btn, {
  #   if (input$last_btn == 'go') {
  #     shinyjs::disable("go")
  #     
  #   }
  #   else if (input$last_btn != 'go'){
  #     shinyjs::enable("go")
  #     
  #   }
  # })
  # 
  # observeEvent(rv$triggo,{
  #   rv$yearc = rv$yearc
  # })
  # 
  # 
  
  observeEvent(input$vuo,{
    
    rv$fyear = input$vuo[1]
    rv$lyear = input$vuo[2]
    rv$time = (input$vuo[2] - input$vuo[1])
    rv$year = input$vuo[1]:input$vuo[2]
    rv$yearc = input$vuo[2]
  }    )
  
  # # update
  # observeEvent(input$vuo, {
  #   # if (input$vuo[2] < input$yearc) {
  #   
  #   updateSliderInput(
  #     session = session,
  #     inputId = "yearc",
  #     value = input$vuo[2]
  #   )
  #   # }
  # })
  
  observeEvent(input$vuo, {
    # if (input$vuo[2] < input$yearc) {
    
    updateSliderTextInput(
      session = session,
      inputId = "yearc",
      selected = input$vuo[2]
    )
    # }
  })
  
  # first to update just rv$yearc, then additionals with req()
  
  yearclist = c("yearc")
  
  # observeEvent(input$xvar, {
  #   
  #   
  #   if  (input$xvar=='gdpcap') {
  #     updateSliderInput(session, "xcen", value = 197, max=197)
  #     
  #   } else {
  #     updateSliderInput(session, "xcen", value = 218, max=218)
  #     
  #     
  #   }
  #   
  #   # req(datsss())
  #   
  # })
  
  # yearclist = c("yearc", "yearca")
  
  
  observeEvent(input$yearc, {
    
    if (
      input$yearc >= rv$ffyear & 
      input$yearc <= input$vuo[2]) {
      rv$yearc = input$yearc
    }
    
    else if  (input$yearc > input$vuo[2]){
      updateSliderTextInput(session, "yearc", selected = input$vuo[2])
      rv$yearc=input$vuo[2]
      
    }
    
    else if  (input$yearc < rv$ffyear){
      updateSliderTextInput(session, "yearc", selected =rv$ffyear)
      rv$yearc=rv$ffyear
      
    }

    
  })
  

  
  
  
  observeEvent(input$nonco2end, {
    
    if (input$nonco2end >= 10) {
      updateNumericInput(session, "nonco2end", value =10)
    }
    
    else if  (input$nonco2end < 5){
      updateNumericInput(session, "nonco2end", value =5)
      
    }
    # 
    # else {
    #   input$nonco2end = input$nonco2end
    #   
    #   
    # }
  })
  
  
  
  observeEvent(input$plot_click, {
    # if (input$plot_click$x < 1 & input$plot_click$x > 0) {
    updateSliderTextInput(
      session = session,
      inputId = "yearc",
      # selected = input$plot_click$x
      selected = round(input$plot_click$x,0)
      # round(
      # minyy+(maxyy-minyy)*input$plot_click$x
      # ,0)
    )
    # }
  })
  
  observeEvent(input$plotj_click, {
    # minyy = mminyy
    # maxyy =mmaxyy
    
    if (rv$pll >=2 ){
      
      if (input$plotj_click$x < .50 & input$plotj_click$x > -.02) {
        
        
        
        updateSliderTextInput(
          session = session,
          inputId = "yearc",
          selected = round(rv$ffyear+(mmaxyy-mminyy)*(input$plotj_click$x+.02)*1.88,0))
        
        
        
      } else if (input$plotj_click$x < 1 & input$plotj_click$x > .50) {
        
        updateSliderTextInput(
          session = session,
          inputId = "yearc",
          selected =  round(rv$ffyear+(mmaxyy-(mmaxyy-rv$ffyear)*0.04-rv$ffyear)*(input$plotj_click$x-.52)*2),0)
        # selected =  round(mminyy+(mmaxyy-mminyy)*(input$plotj_click$x-.5)*2),0)
        
      }
    }
    
    else if (rv$pll ==1) {
      updateSliderTextInput(
        session = session,
        inputId = "yearc",
        selected = round(rv$ffyear+(mmaxyy-(mmaxyy-rv$ffyear)*0.04-rv$ffyear)*input$plotj_click$x*1),0)
      
    }
    
  })
  #     
  observeEvent(input$plotk_click, {
    
    updateSliderTextInput(
      session = session,
      inputId = "yearc",
      selected = round(rv$ffyear+(mmmaxyy-(mmaxyy-rv$ffyear)*0.04-rv$ffyear)*(input$plotk_click$x-0.015),0)
    )
  })
  
  # observeEvent(input$plotl_click, {
  #   
  #   updateSliderTextInput(
  #     session = session,
  #     inputId = "yearc",
  #     selected = round(rv$ffyear+(mmmaxyy-(mmaxyy-rv$ffyear)*0.04-rv$ffyear)*(input$plotl_click$x-0.015),0)
  #   )
  # })
  
  # value = 1965+
  
  # 
  # observeEvent(input$plota_click, {
  #   updateSliderInput(
  #     session = session,
  #     inputId = "yearc",
  #     value = input$plota_click$x
  #   )
  # })
  # 
  # 
  # observeEvent(input$plotb_click, {
  #   updateSliderInput(
  #     session = session,
  #     inputId = "yearc",
  #     value = input$plotb_click$x
  #   )
  # })
  # 
  # observeEvent(input$plotc_click, {
  #   updateSliderInput(
  #     session = session,
  #     inputId = "yearc",
  #     value = input$plotc_click$x
  #   )
  # })
  # 
  # observeEvent(input$plotd_click, {
  #   updateSliderInput(
  #     session = session,
  #     inputId = "yearc",
  #     value = input$plotd_click$x
  #   )
  # })
  
  # observeEvent(input$plot2_click, {
  #   updateSliderInput(
  #     session = session,
  #     inputId = "yearc",
  #     value = input$plot2_click$x
  #   )
  # })
  # 
  
  output$slickr1 <- renderSlickR({
    imgs <- list.files("www/images/eng", pattern=".png", full.names = TRUE)
    slickR(imgs) + settings(dots=TRUE)
  })
  output$slickr1 <- renderSlickR({
    imgs <- list.files("www/images/fin", pattern=".png", full.names = TRUE)
    slickR(imgs) + settings(dots=TRUE)
  })
  
  # if (rv$lang=="eng") {
  #   
  #   
  # }
  
  # observe({
  #   # if (rv$lang=="eng") {
  # 
  #     if (input$nonco2 == FALSE) {
  # 
  #     rv$disctext ="Includes non-CO2 emissions. Does not include land use change emissions/sinks"
  # 
  #     } else {
  # 
  #       rv$disctext ="Does not include non-CO2 emissions or land use change emissions/sinks"
  # 
  #     }
  #   # }  else if (rv$lang =="fin"){
  #   # 
  #   #   if (input$nonco2 == TRUE) {
  #   # 
  #   #     rv$disctext ="Sisältää Ei-CO2-päästöt. Ei sisällä maankäytön muutoksen päästöjä/nieluja"
  #   # 
  #   # 
  #   #   } else {
  #   #     rv$disctext ="Ei sisällä Ei-CO2-päästöjä tai maankäytön muutoksen päästöjä/nieluja"
  #   # 
  #   #   }
  #   # }
  # })
  
  
  observeEvent(rv$lang, {
    if (rv$lang=="eng") {
      
      output$slickr1 <- renderSlickR({
        imgs <- list.files("www/images/eng", pattern=".png", full.names = TRUE)
        slickR(imgs,width = "70%", height="60%") 
        # + settings(dots=TRUE)
      })
      
      # updateBox("tutorialbox", 
      #           action = "update",
      #           
      #           options = list(
      #             style = 'overflow-x: hidden',
      #             # style = 'overflow-y: scroll',
      #             
      #             id = "tutorialbox",
      #             # dots = TRUE,
      #             width = NULL,
      #             title ="Tutorial",
      #             closable = TRUE,
      #             closed=TRUE,
      #             collapsible = FALSE,
      #             slickROutput("slickr1",width = "90%", height="90%")
      #             )              # ,
      #           
      #           # slickROutput("slickr1",width = "90%", height="90%")
      #           
      #           
      #           # action = "remove"
      #           )
      
      
    } else if (rv$lang =="fin") {
      output$slickr1 <- renderSlickR({
        imgs <- list.files("www/images/fin", pattern=".png", full.names = TRUE)
        slickR(imgs,width = "70%", height="60%") 
        # + settings(dots=TRUE)
      })
      
      # updateBox("tutorialbox",
      #           action = "update",
      #           
      #           options = list(
      #             style = 'overflow-x: hidden',
      #             # style = 'overflow-y: scroll',
      #             
      #             id = "tutorialbox",
      #             # dots = TRUE,
      #             width = NULL,
      #             title ="Tutoriaali",
      #             closable = FALSE,
      #             closed=TRUE,
      #             collapsible = FALSE,
      #             slickROutput("slickr2",width = "90%", height="90%")
      #             
      #             )
      #           
      #           # slickROutput("slickr2",width = "90%", height="90%")              
      #           )
      
      
    }
  })
  # output$tutbox = renderUI({
  # 
  #   if (rv$lang=="eng") {
  #   box(
  #     style = 'overflow-x: hidden',
  #     # style = 'overflow-y: scroll',
  # 
  #     id = "tutorialbox",
  #     # dots = TRUE,
  #     width = NULL,
  #     title ="Tutorial",
  #     closable = TRUE,
  #     closed=TRUE,
  #     collapsible = FALSE,
  #     slickROutput("slickr1",width = "90%", height="90%")
  #     # ,ignore.init=TRUE
  #     # ))
  #   )
  # }else if (rv$lang =="fin") {
  #     box(
  #       style = 'overflow-x: hidden',
  #       # style = 'overflow-y: scroll',
  # 
  #       id = "tutorialbox",
  #       # dots = TRUE,
  #       width = NULL,
  #       title ="Tutoriaali",
  #       closable = TRUE,
  #       closed=TRUE,
  #       collapsible = FALSE,
  #       slickROutput("slickr1",width = "90%", height="90%")
  #       # ,ignore.init=TRUE
  #       # ))
  #     )
  # 
  # }
  # })
  
  
  
  observeEvent(input$muo, {
    
    rv$muosel = input$muo
    
  })
  observeEvent(input$pri, {
    
    rv$muopri = input$pri
    
  })
  observeEvent(input$muoindi, {
    
    rv$muoseli = input$muoindi
    
  })
  
  
  observeEvent(rv$lang, {
    if (rv$lang=="eng") {
      
      output$yearc= renderText(paste("Values for ",rv$yearc))
      
      # if (rv$lang == "eng") {
      
      observeEvent(input$bud, {
        
        if (input$bud == 400) {
          rv$budinfo = c("1,5°C with 67% chance")
          
        } else if (input$bud ==500) {
          
          rv$budinfo = c("1,5°C with 50% chance")
        } else if (input$bud ==1150) {
          
          rv$budinfo = c("2°C with 67% chance")
        }
        else if (input$bud ==1350) {
          
          rv$budinfo = c("2°C with 50% chance")
        }
      })
      # } else if (rv$lang == "fin") {
      
      
      
      
      # }
      
      # tutbox = renderUI({
      #   box(
      #     style = 'overflow-x: hidden',
      #     # style = 'overflow-y: scroll',
      #     
      #     id = "tutorialbox",
      #     # dots = TRUE,
      #     width = NULL,
      #     title ="Tutoriaali",
      #     closable = TRUE,
      #     closed=TRUE,
      #     collapsible = FALSE,
      #     slickROutput("slickr",width = "90%", height="90%") 
      #     # ,ignore.init=TRUE
      #     # ))
      #   )
      # })
      
      # if (rv$lang == "eng") {
      
      
      # }
      # if (rv$lang == "fin") {
      
      # }
      
      
      
      
      
      observeEvent(input$next1, {
        updateTabsetPanel(session, inputId = "nok", selected = "2. Population projection")
      })
      
      
      observeEvent(input$next2, {
        updateTabsetPanel(session, inputId = "nok", selected = "3. Carbon price")
      })
      
      observeEvent(input$next3, {
        updateTabsetPanel(session, inputId = "nok", selected = "4. User emissions")
      })
      
      observeEvent(input$next4, {
        updateTabsetPanel(session, inputId = "nok", selected = "EXTRA: Countries")
      })
      
      
      
      # observeEvent(input$prev0, {
      #   updateTabsetPanel(session, inputId = "nok", selected = "1. Global emissions")
      # })  
      
      observeEvent(input$prev1, {
        updateTabsetPanel(session, inputId = "nok", selected = "1. Global emissions")
      })
      
      observeEvent(input$prev2, {
        updateTabsetPanel(session, inputId = "nok", selected = "2. Population projection")
      })
      
      
      observeEvent(input$prev3, {
        updateTabsetPanel(session, inputId = "nok", selected = "3. Carbon price")
      })
      
      observeEvent(input$prev4, {
        updateTabsetPanel(session, inputId = "nok", selected = "4. User emissions")
      })
    } else if (rv$lang=="fin") {
      
      output$yearc= renderText(paste("Arvot vuodelle ",rv$yearc))
      
      
      observeEvent(input$bud, {
        if (input$bud == 400) {
          
          rv$budinfo = c("1,5°C 67% varmasti")
          
        } else if (input$bud ==500) {
          
          rv$budinfo = c("1,5°C 50% varmasti")
        } else if (input$bud ==1150) {
          
          rv$budinfo = c("2°C 67% varmasti")
        }
        else if (input$bud ==1350) {
          
          rv$budinfo = c("2°C 50% varmasti")
        } 
      })
      observeEvent(input$next1, {
        updateTabsetPanel(session, inputId = "nok", selected = "2. Maailman väestö")
      })
      
      
      observeEvent(input$next2, {
        updateTabsetPanel(session, inputId = "nok", selected = "3. Hiilen hinta")
      })
      
      observeEvent(input$next3, {
        updateTabsetPanel(session, inputId = "nok", selected = "4. Käyttäjän päästöt")
      })
      
      observeEvent(input$next4, {
        updateTabsetPanel(session, inputId = "nok", selected = "EXTRA: Maat")
      })
      
      
      
      # observeEvent(input$prev0, {
      #   updateTabsetPanel(session, inputId = "nok", selected = "1. Global emissions")
      # })  
      
      observeEvent(input$prev1, {
        updateTabsetPanel(session, inputId = "nok", selected = "1. Globaalit päästöt")
      })
      
      observeEvent(input$prev2, {
        updateTabsetPanel(session, inputId = "nok", selected = "2. Maailman väestö")
      })
      
      
      observeEvent(input$prev3, {
        updateTabsetPanel(session, inputId = "nok", selected = "3. Hiilen hinta")
      })
      
      observeEvent(input$prev4, {
        updateTabsetPanel(session, inputId = "nok", selected = "4. Käyttäjän päästöt")
      })
      
    } 
  })
  # 
  rv$labelfossil = "Total co23"
  # labellist = c("labelfossil")
  
  
  # lapply(
  #   X = labellist,
  #   FUN = function(i){
  #     
  #     observeEvent(rv[[paste0(i)]], {
  #       
  #       # if (rv[[paste0(i)]] ==TRUE & input$visib ==TRUE) {
  #     rv    
  #         updateTextInput(
  #           session=session,
  #           inputId = i,
  #           label = rv[[paste0(i)]])
  #         
  #       # }
  #     } )
  #     
  #   })
  # 
  
  
  
  showlista = c(
    "showcountryfossil","showcountrycost", "showcountrynetcost","showcountrypop")
  
  
  
  lapply(
    X = c(
      "showcountryfossil","showcountrycost", "showcountrynetcost","showcountrypop", "shownewsink", "showlandcost", "alert4", "alert6", "alert8", "alert10" ),
    FUN = function(i){
      rv[[paste0(i)]] = FALSE
    } )
  
  #
  showlistb = c("showfossil","showland", "shownet",  "showghg", "shownonco2", "showprice" , "showavgcost",
                "showdividend","showavgnetcost" , "showuserfossil","showusercost", "shownetcost", "showpop", "showavgfossil")
  lapply(
    X = showlistb,
    FUN = function(i){
      rv[[paste0(i)]] = TRUE
    } )
  
  
  
  
  showlist = c("showfossil","showland", "shownet", "showghg", "shownonco2","shownewsink", 
               "showlandcost",   "showavgfossil","showprice" , "showavgcost",
               "showdividend","showavgnetcost" , "showuserfossil","showusercost", "shownetcost","showpop",
               "showcountryfossil","showcountrycost", "showcountrynetcost","showcountrypop", "showaveragedividend", "showcountrydividend")
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
  
  
  # uncheck variables if pressed "Show all"
  
  showlistc = c("showfossil","showland", "shownet", "showavgfossil","showprice" , "showavgcost",
                "showdividend","showavgnetcost" , "showuserfossil","showusercost", "shownetcost","showpop"
  )
  showlistd = c( "showghg", "shownonco2")
  showlistdd = c( "shownewsink", "showlandcost")
  
  showliste = c(
    "showcountryfossil","showcountrycost", "showcountrynetcost","showcountrypop")
  
  showlistf = c("showaveragedividend")
  showlistg = c("showcountrydividend")
  
  # 
  # observeEvent(input$nationalcoun, {
  #   
  #   if (input$nationalcoun %in% c(ll2)) {
  #     rv$showcountrydividend=TRUE
  #   } else {
  #     rv$showcountrydividend=FALSE
  #     
  #   }
  #   
  # })
  # 
  # 
  # 
  # 
  # orr = observeEvent(input$national,{
  #   if (input$national != 0){
  #     rv$alert6 =TRUE
  #     
  #     # orr$destroy()
  #     rv$showaveragedividend=TRUE
  #     
  #   }
  #   if (input$national != 0){
  #     rv$alert8 =FALSE
  #     rv$showaveragedividend=FALSE
  #     
  #   }
  #   
  # })
  
  lapply(
    X = showlistg,
    FUN = function(i){
      observeEvent(input$nationalcoun, {
        if (input$nationalcoun %in% c(ll2)) {
          updateAwesomeCheckbox(
            session=session,
            inputId = i,
            value = TRUE)
        }
        
        
      })
      
    })
  
  lapply(
    X = showlistf,
    FUN = function(i){
      observeEvent(input$national, {
        if (input$national!=0) {
          updateAwesomeCheckbox(
            session=session,
            inputId = i,
            value = TRUE)
        }
        
        
      })
      
    })
  
  
  
  lapply(
    X = showliste,
    FUN = function(i){
      observeEvent(input$showall, {
        if (rv$lek==TRUE) {
          updateAwesomeCheckbox(
            session=session,
            inputId = i,
            value = TRUE)
        }
        
        
      })
      
    })
  
  
  lapply(
    X = showlistc,
    FUN = function(i){
      observeEvent(input$showall, {
        updateAwesomeCheckbox(
          session=session,
          inputId = i,
          value = TRUE)
        
      })
      
    })
  
  
  lapply(
    X = showlistd,
    FUN = function(i){
      observeEvent(input$showall, {
        if (input$nonco2==TRUE) {
          updateAwesomeCheckbox(
            session=session,
            inputId = i,
            value = TRUE)
        } 
        
        
      })
      
    })
  
  lapply(
    X = showlistdd,
    FUN = function(i){
      observeEvent(input$showall, {
        if (input$luls==TRUE) {
          updateAwesomeCheckbox(
            session=session,
            inputId = i,
            value = TRUE)
        } 
        
        
      })
      
    })
  # 
  # observeEvent(input$showall, {
  # 
  #   updateAwesomeCheckbox(
  #     session=session,
  #     inputId = "visib",
  #     value = FALSE)
  #   
  # })
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
  
  
  
  # lapply(
  #   X = showlist,
  #   FUN = function(i){
  #     observeEvent(input$visib,{
  #       
  #       if (input$visib ==TRUE
  #           
  #       ) {
  #         
  #         updateAwesomeCheckbox(
  #           session=session,
  #           inputId = i,
  #           value = rv[[paste0(i)]])
  #       }
  #       
  #     }     )
  #     
  #     observeEvent(input$showall, {
  #       updateAwesomeCheckbox(
  #         session=session,
  #         inputId = i,
  #         value = TRUE)
  #       
  #       updateAwesomeCheckbox(
  #         session=session,
  #         inputId = "visib",
  #         value = FALSE)
  #       
  #     })
  #     
  #     observeEvent(input$shownone, {
  #       updateAwesomeCheckbox(
  #         session=session,
  #         inputId = i,
  #         value = FALSE)
  #       
  #       updateAwesomeCheckbox(
  #         session=session,
  #         inputId = "visib",
  #         value = FALSE)
  #     })
  #     
  #   })
  # 
  
  
  
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
  lapply(
    X = showlist,
    FUN = function(i){
      
      observeEvent(rv[[paste0(i)]], {
        
        if (rv[[paste0(i)]] ==FALSE) {
          
          updateAwesomeCheckbox(
            session=session,
            inputId = i,
            value = FALSE)
          
        }
      } )
      
    })
  
  
  
  
  observeEvent(input$nok,{
    if (input$nok %in% c("1. Global emissions", "1. Globaalit päästöt")) {
      rv$showfossil =TRUE
      rv$showland =TRUE
      rv$shownet =TRUE
      
    }
    
    # else if (input$nok =="2. Emission trajectory") {
    #   
    #   rv$showfossil =TRUE
    #   rv$showland =TRUE
    #   rv$shownet =TRUE
    #   
    # }
    
    
    else if (input$nok %in% c("2. Population projection", "2. Maailman väestö")) {
      
      # rv$showfossil =TRUE
      # rv$showland =TRUE
      rv$showfossil =TRUE
      rv$showpop =TRUE
      rv$showavgfossil =TRUE
      
    }
    
    else if (input$nok %in% c("3. Carbon price", "3. Hiilen hinta")) {
      rv$showprice =TRUE
      rv$showdividend=TRUE
      rv$showavgcost=TRUE
      rv$showavgnetcost=TRUE
      # rv$showpop =TRUE
      rv$showavgfossil =TRUE
      
    }
    
    else if (input$nok %in% c("4. User emissions", "Käyttäjän päästöt")) {
      rv$showusercost =TRUE
      rv$showuserfossil=TRUE
      rv$shownetcost =TRUE
      
      rv$showprice =TRUE
      rv$showdividend=TRUE
      
    }
    
    # else if (input$nok %in% c("EXTRA: Countries", "EXTRA: Maat")) {
    #   rv$showcountrycost =TRUE
    #   rv$showcountryfossil=TRUE
    #   rv$showcountrynetcost =TRUE
    #   rv$showcountrypop =FALSE
    #   
    # }
    
  })
  
  
  # 
  
  
  
  
  lapply(
    X = c("showfossil", "showland", "shownet", "showghg","shownonco2", "shownewsink"),
    FUN = function(i){
      
      observeEvent(input[[paste0(i)]], {
        
        if (input$showfossil == TRUE || input$showland==TRUE || input$shownet ==TRUE || input$showghg==TRUE || input$shownonco2 ==TRUE || input$shownewsink ==TRUE) {
          rv$plot2 = "plot2"}
        else {
          rv$plot2 = ""}
        
      } )
    } )
  
  
  lapply(
    X = c("showpop", "showcountrypop"),
    FUN = function(i){
      
      observeEvent(input[[paste0(i)]], {
        
        if (input$showpop== TRUE || input$showpop ==TRUE) {
          rv$plot3 = "plot3"}
        else {
          rv$plot3 = ""}
        
      } )
    } )
  # 
  
  lapply(
    X = c("showavgfossil", "showuserfossil", "showcountryfossil"),
    FUN = function(i){
      
      observeEvent(input[[paste0(i)]], {
        
        if (input$showavgfossil == TRUE || input$showuserfossil==TRUE || input$showcountryfossil==TRUE) {
          rv$plot4 = "plot4"}
        else {
          rv$plot4 = ""}
        
      } )
    } )
  # 
  lapply(
    X = c("showprice"),
    FUN = function(i){
      
      observeEvent(input[[paste0(i)]], {
        
        if (input$showprice == TRUE) {
          rv$plot5 = "plot5"}
        else {
          rv$plot5 = ""}
        
      } )
    } )
  # 
  lapply(
    X = c("showdividend", "showavgnetcost", "showavgcost", "showusercost", 
          "shownetcost", "showcountrycost", "showaveragedividend", "showcountrydividend",
          "showcountrynetcost", "landcost"),
    FUN = function(i){
      
      observeEvent(input[[paste0(i)]], {
        
        if (input$showavgcost == TRUE || input$showdividend ==TRUE || input$showavgnetcost==TRUE ||
            input$showusercost==TRUE || input$shownetcost ==TRUE || input$showcountrycost ==TRUE || input$showlandcost ==TRUE
            || input$showaveragedividend ==TRUE || input$showcountrydividend ==TRUE || input$showcountrynetcost ==TRUE)   {
          rv$plot6 = "plot6"}
        else {
          rv$plot6 = ""}
        
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
    priority = 1
    
    
    
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
      inputId ="sprice",
      value = skenbbs()[sken==input$bud & nams=="sprice", vals]
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
  
  
  
  # 
  
  
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
    
    # 
    # print(rv$showfossil)
    # print(rv$showprice)
    # print(rv$showpop)
    
    # freezeReactiveValue(input, "bud")
    # req(input$vuo)
    
    
    # start=input$fstart
    # lstart=input$lstart
    
    # AS of now, budget does not exclude emissions between 2020 and lastyear
    
    
    
    # create data from last observed year to year before pricing begins
    start=ppaa[year ==lastyear & sec =="fossil", yy]
    #land use emission start
    lstart = ppaa[year ==lastyear & sec =="land", yy]
    
    
    sinkstart = ppaa[year ==lastyear & sec =="sink", yy]
    sourcestart = ppaa[year ==lastyear & sec =="source", yy]
    
    
    # years for calculation
    yearl = lastyear:(as.numeric(input$vuo[1]))
    
    # years for data set
    yearl2 = (lastyear+1):(as.numeric(input$vuo[1])-1)
    
    ll = max(as.numeric(length(yearl)),0)
    time = max(as.numeric(length(yearl)),0)
    
    fossil = seq(start, as.numeric(input$fstart), length.out= time)
    # [,-1]
    land = seq(lstart, as.numeric(input$lstart),length.out = time)
    
    sink = seq(sinkstart, as.numeric(input$sinkstart),length.out = time)
    source = seq(sourcestart, as.numeric(input$sourcestart),length.out = time)
    
    # [,-1]
    # take out the calculation start and end point that will be in the data anyway
    fossil = fossil[-1]
    land = land[-1]
    sink = sink[-1]
    source = source[-1]
    
    
    fossil = head(fossil,-1)
    land = head(land,-1)  
    sink = head(sink,-1)  
    source = head(source,-1)  
    
    
    # emissions from 2020 to last observed year 
    historyfossil = ppaa[year %in% budgetyear:lastyear & sec =="fossil", yy]
    historyland = ppaa[year %in% budgetyear:lastyear & sec =="land",yy]
    historysink = ppaa[year %in% budgetyear:lastyear & sec =="sink",yy]
    historysource = ppaa[year %in% budgetyear:lastyear & sec =="source",yy]
    
    historyyear = ppaa[year %in% budgetyear:lastyear & sec =="land",year]
    
    # combine historical and intermediate simulated emissions
    fossil = c(historyfossil, fossil)
    land = c(historyland, land)
    sink = c(historysink, sink)
    source = c(historysource, source)
    
    yearl2 = c(historyyear, yearl2)
    
    
    
    
    
    
    
    
    
    
    
    #combine data frames to one
    
    inter = data.frame(yearl2, fossil, land, sink, source)
    inter$net = inter$fossil + inter$land
    inter=as.data.table(inter)
    
    
    # net emissions used before the observation year 
    sumnet = inter[,sum(net)]
    rv$sumnet = sumnet
    
    
    
    
    # non-co2
    
    
    
    
    
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
    
    # time = lenght(year)
    
    
    
    
    
    
    
    # fossil totals: 
    
    start = input$fstart
    lstart = input$lstart
    
    
    end<- as.numeric(input$paa)
    lend = (-1)*end
    budget<- as.numeric(input$bud)-sumnet
    
    
    # if (input$muo == "percentual")  {
    f3 = f3 <- function(rate,start,time, end) {
      end - start * (1-rate/100)^(time+1)
    }
    result <- uniroot(f3,start=start,time=time, end=end, lower=-0, upper=100)$root
    u = result[1]
    
    rv$ratepr = -1*u
    # } 
    # else if (input$muo=="linear") {
    f3 <- function(rate,start,time, end) {
      end - (start - rate*(time+1))
    }
    result <- uniroot(f3,start=start,time=time, end=end, lower=-0, upper=100)$root
    u = result[1]
    
    rv$rateli = -1*u
    # } 
    # else if  (input$muo=="logarithmic") {
    f3 <- function(rate,start,time, end) {
      # end - (start - rate*log(time+1))
      end - (start - rate^(time+1))
      
    }
    result <- uniroot(f3,start=start,time=time, end=end, lower=-0, upper=100)$root
    u = result[1]
    
    rv$ratelo = u
    # }
    
    
    # emission rate solving with given values
    
    
    
    
    if (input$muo != "exponential") {
      
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
      # else if  (input$muo=="exponential") {
      #   # need to calculate the area under the curve. For it need to know how much the budget is for emissions and LUC.
      #   # 
      #   
      #   f3 <- function(rate,start,time, end) {
      #     # end - (start - rate*log(time+1))
      #     end - (start - rate^(time+1))
      #     
      #   }}
      # 
      # emission rate solving with given values
      
      
      
      result <- uniroot(f3,start=start,time=time, end=end, lower=-0, upper=100)$root
      u = result[1]
      
      
      
      rate = u
      # rv$rate = rate
      
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
          
        }
        
        
      }
      fossil = g(start, rate, 0:time)
      
      
      
      
      #applying yearly emission function to calculate emissions
      
      
      # function for emissions cumulation over time
      if (input$muo == "percentual")  {
        geomsuma = function(start, rate, time) {
          x = 0
          for(i in 0:time) x = x + start * (1-rate/100)^(i+1)
          return(x)  }
        
        
      } else if (input$muo=="linear") {
        geomsuma = function(start, rate, time) {
          x = 0
          for(i in 0:time) x = x + start - rate*(i+1)
          return(x)  }  } 
      else if (input$muo =="logarithmic") {
        geomsuma = function(start, rate, time) {
          x = 0
          for(i in 0:time) x = x + start - rate*log(i+1)
          
          # for(i in 0:time) x = x + start - rate*(i)
          # for(i in 1:time) x = x + start - rate*log((i))
          return(x)  }  
        
      }
      
      # applying function for emission cumulatin
      
      
      total = geomsuma(start, rate,time)
      
      
      
      
      start = input$nonco2start
      # lstart = input$lstart
      
      
      end<- as.numeric(input$nonco2end)
      # lend = (-1)*end
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
      # rv$rate = rate
      
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
      nonco2 = g(start, rate, 0:time)
      
      
      rv$trigu = rv$trigu+1
    }
    
    
    
    
    
    
    
    
    
    
    else if (input$muo == "exponential") {
      
      rate = 1
      times = (lyear - fyear)
      # fstart= start
      tim = 0:times
      end<- as.numeric(input$paa)
      # SpanFast=(fstart-end)*.30
      # 
      # SpanSlow=(fstart-end)*.70
      
      # f3 = f3 <- function(rate,start,time, end) {
      #   end - start*(rate)^((time+1)^(1/1.48))
      # }    
      # 
      # result <- uniroot(f3,start=start,time=time, end=end, lower=-0, upper=100)$root
      # u = result[1]
      # rate = u    
      # g = function(start, rate, time) {
      #   c(start*(rate)^((time+1)^(1/1.48)))
      # }
      # 
      # fossil = g(start, rate, 0:time)
      
      model <- function(theta){
        # a = function thing
        a <- theta[1]
        # b = other function thing
        b  <- theta[2]
        
        c  <- theta[3]
        
        # SpanFast=(Y0-Plateau)*PercentFast*.01
        #
        # SpanSlow=(Y0-Plateau)*(100-PercentFast)*.01
        #
        # Y=Plateau + SpanFast*exp(-KFast*X) + SpanSlow*exp(-KSlow*X)
        # Y=end + SpanFast*exp(-a*(times+1)) + SpanSlow*exp(-b*(times+1))
        
        
        # F1 = end - Y
        # F2 = input$ala - sum(end + SpanFast*exp(-a*tim) + SpanSlow*exp(-b*tim))
        # curve goes from start to finish
        # F1 = end - start*(rate)^((time+1)^(b))
        # 
        # 
        # F2 =  input$ala - sum(start*(rate)^((tim+1)^(b)))
        
        F1 = end - start + a*(time+1) + b*(time+1)^2 + c*(time+1)^3
        # F3 = end - start + a*(time+1) + b*(time+1)^2 + c*(time+1)^3
        
        F2 =  input$ala - sum(start + a*(time+1) + b*(time+1)^2 + c*(time+1)^3)
        F3 =  input$ala - sum(start + a*(time+1) + b*(time+1)^2 + c*(time+1)^3)
        
        # area below curve corresponds to necessary carbon reduction for emissions reductions
        # F2 = input$ala - sum(a^(b*tim)+ fstart)
        # curve goes from start to finish
        # F1 = end - (fstart - b^(b^times))
        #
        # # area below curve corresponds to necessary carbon reduction for emissions reductions
        # F2 = input$ala - sum(a^(b*tim)+ fstart)
        
        
        c(F1=F1,F2=F2, F3=F3)
      }
      
      (ss <- multiroot(f = model, start = c(.0015, -.00005, -.0001)))
      
      fff = unlist(ss, use.names=FALSE)
      a= fff[1]
      
      #fb  = speed of emission reduction, second galf
      b=fff[2]
      c=fff[3]
      
      ga = function(start,a, b,c, time) {
        
        # c(end + SpanFast*exp(-a*(times+1)) + SpanSlow*exp(-b*(times+1)))
        
        # c( start*(rate)^((time+1)^(b)))
        c(start + a*(time+1) + b*(time+1)^2 + c*(time+1)^3)
        
        # c(start-rate*log(time+1))
        # c(start-rate^(time+1))
      }
      fossil = ga(start,a, b,c, time=(0:time))
      geomsuma = function(start, a, b, c,time) {
        x = 0
        # for(i in 0:time) x = x + start*(rate)^((i+1)^(b))
        for(i in 0:time) x = x + start + a*(i+1) + b*(i+1)^2 + c*(i+1)^3
        
        # for(i in 0:time) x = x + start - rate*(i)
        # for(i in 1:time) x = x + start - rate*log((i))
        return(x)  }
      total = geomsuma(start, a, b,c, time)
      # total = sum(fossil)
      # total = 1500
      
      
      
      # else if (input$muo =="logarithmic") {
      #   geomsuma = function(start, rate, time) {
      #     x = 0
      #     for(i in 0:time) x = x + start - rate*log(i+1)
      # 
      #     # for(i in 0:time) x = x + start - rate*(i)
      #     # for(i in 1:time) x = x + start - rate*log((i))
      #     return(x)  }
      # 
      # }
      
      # applying function for emission cumulatin
      
      
      # total = geomsuma(start, rate,time)
      
      
      
      
      
      
      
      
      fstart = input$nonco2start
      # lstart = input$lstart
      
      
      end<- as.numeric(input$nonco2end)
      
      model <- function(theta){
        
        # a = function thing
        a <- theta[1]
        # b = other function thing
        b  <- theta[2]
        
        # curve goes from start to finish
        F1 = end - (fstart - a^(b*times))
        
        # area below curve corresponds to necessary carbon reduction for emissions reductions
        F2 = input$ala - sum(a^(b*tim)+ fstart)
        
        
        c(F1=F1,F2=F2)
      }
      
      (ss <- multiroot(f = model, start = c(2, -.3)))
      
      fff = unlist(ss, use.names=FALSE)
      a= fff[1]
      
      #fb  = speed of emission reduction, second galf
      b=fff[2]
      
      ga = function(fstart, a, b, times) {
        
        c(a^(b*(times+1)) + fstart)
        # c(start-rate*log(time+1))
        # c(start-rate^(time+1))
        
        
      }
      nonco2= ga(fstart,  a, b, times=(0:times))
    }
    
    
    # fossil  =  ffa^(ffb*time)+ start
    
    #  
    
    
    
    # total = geomsuma(start, ffa, ffb,time)
    
    # result <- uniroot(f3,start=start,time=time, end=end, lower=-0, upper=100)$root
    # u = result[1]
    # } 
    
    
    
    
    
    
    # end<- as.numeric(input$end)
    # lend = (-1)*end
    # ## calculating landuse emissions
    # how much needed to absorb  by land based on cumulative emissions over time
    lbudget= budget - total
    rv$rvlbudget = lbudget
    rv$totals = total
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
    rv$warn =ffa
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
    
    
    
    
    
    
    
    
    # nonco2 emissions
    
    
    # fossil totals: 
    
    
    # budget<- as.numeric(input$bud)-sumnet
    
    
    # if (input$muo == "percentual")  {
    # f3 = f3 <- function(rate,start,time, end) {
    #   end - start * (1-rate/100)^(time+1)
    # }
    # result <- uniroot(f3,start=start,time=time, end=end, lower=-0, upper=100)$root
    # u = result[1]
    # 
    # rv$ratepr = -1*u
    # # } 
    # # else if (input$muo=="linear") {
    # f3 <- function(rate,start,time, end) {
    #   end - (start - rate*(time+1))
    # }
    # result <- uniroot(f3,start=start,time=time, end=end, lower=-0, upper=100)$root
    # u = result[1]
    # 
    # rv$rateli = -1*u
    # # } 
    # # else if  (input$muo=="logarithmic") {
    # f3 <- function(rate,start,time, end) {
    #   # end - (start - rate*log(time+1))
    #   end - (start - rate^(time+1))
    #   
    # }
    # result <- uniroot(f3,start=start,time=time, end=end, lower=-0, upper=100)$root
    # u = result[1]
    # 
    # rv$ratelo = u
    # }
    
    
    # emission rate solving with given values
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    muoindi=input$muoindi
    start<- as.numeric(input$indi1)
    end<- as.numeric(input$indi2)
    
    
    if (start >= end) {
      f3 = f3 <- function(rateindi,start,time, end) {
        end - start * (1-rateindi/100)^(time)
      }
      result <- uniroot(f3,start=start,time=time, end=end, lower=-0, upper=100)$root
      rv$ratepri = -1*result[1]  
      
      f3 <- function(rateindi,start,time, end) {
        end - (start- rateindi*time)
      }
      result <- uniroot(f3,start=start,time=time, end=end, lower=-0, upper=100)$root
      rv$ratelii = -1*result[1]  
      
      
    }
    
    if (start < end) {
      f3 = f3 <- function(rateindi,start,time, end) {
        end - start * (1+rateindi/100)^(time)
      }
      result <- uniroot(f3,start=start,time=time, end=end, lower=-0, upper=100)$root
      rv$ratepri = result[1]  
      
      f3 <- function(rateindi,start,time, end) {
        end - (start + rateindi*time)
      }
      result <- uniroot(f3,start=start,time=time, end=end, lower=-0, upper=100)$root
      rv$ratelii = result[1]  
      
      
    }
    
    
    
    
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
    
    
    
    #   } }
    # 
    pstart = input$sprice
    pend = input$eprice
    
    f3 = f3 <- function(prate,pstart,time, pend) {
      pend - pstart * (1+prate/100)^(time)
    }
    result <- uniroot(f3,pstart=pstart,time=time, pend=pend, lower=-100, upper=1000)$root
    
    rv$rateprp  = result[1]
    
    
    f3 <- function(prate,pstart,time, pend) {
      pend - (pstart + prate*time)
    }
    result <- uniroot(f3,pstart=pstart,time=time, pend=pend, lower=-100, upper=1000)$root
    # u = result[1]
    rv$ratelip = result[1]
    
    # 
    # f3 <- function(prate,pstart,time, pend) {
    #   pend - (pstart + prate*log(time+1))
    # }
    # result <- uniroot(f3,pstart=pstart,time=time, pend=pend, lower=-100, upper=1000)$root
    # ratelop = result[1]
    
    #  prate=  as.numeric(input$prate)
    # pgrowth = as.numeric(input$pgrowth)
    if (input$pri == "percentual")  {
      f3 = f3 <- function(prate,pstart,time, pend) {
        pend - pstart * (1+prate/100)^(time)
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
        c(pstart*(1+prate/100)^(time))
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
    # ghg  = rep(ghg,time+1)
    
    total  = rep(total,time+1)
    
    lyear = rep(lyear,time+1)
    budget = rep(budget,time+1)
    lbudget = rep(lbudget,time+1)
    rate = rep(rate,time+1)
    end= rep(end,time+1)
    
    
    lend= rep(lend,time+1)
    
    dats = data.frame(year,  budget, rate, fossil, nonco2,  land,  pop, total, price, userfossil
                      
    )                
    
    
    
    
    dats = as.data.table(dats)
    
  if (input$luls ==TRUE) {
      
      dats[, source := input$sourcestart+(land-input$lstart)*input$lulls/100]
      # dats[, source := 5]
      
      dats[source <0, source :=0]
      
      dats[, sink := input$sinkstart+((land-input$lstart)+(input$sourcestart-source))]
      
      dats[, newsink := sink-input$sinkstart]
      
    } else {
      dats[, source := 0]
      # dats[, source := 5]
      
      dats[source <0, source :=0]
      
      dats[, sink :=0]
      
      dats[, newsink :=0]

    }

    
    
    dats[, net := fossil+land]
    dats[, ghg := fossil+nonco2]
    
    
    if (input$nonco2 == 0) {
      dats[, avgfossil := fossil/pop]
    } else { 
      dats[, avgfossil := ghg/pop]
      
    }
    
    
    
    
    
    dats[, avgcost :=price*avgfossil]
    
    dats[, landcost := price*newsink/pop*-1]
    
    dats[, dividend :=avgcost-landcost]
    
    dats[, usercost := price*userfossil]
    dats[,avgnetcost :=avgcost-dividend]
    
    dats[, netcost :=usercost-dividend]
    
    
    
    dats = as.data.table(dats)
    
    rv$afterbudget = dats[year==rv$lyear, budget]
    
    rv$rvtotal = dats[year %in% c(rv$fyear:rv$yearc) ,sum(net)]
    rv$fosm = dats[year %in% c(rv$fyear:rv$yearc) ,sum(fossil)]
    
    rv$rvlsum = dats[year %in% c(rv$fyear:rv$yearc) ,sum(land)]
    
    # rv$afterbudget = dats[year==rv$lyear, budget]
    # rv$rvtotal =dats[year==rv$lyear, budget]
    
    # rv$triggo = 1
    
    
    dats
    
    
    
  })
  
  
  
  lux = reactive({
    lux = as.data.table(lug())
    
    
    
    
    #
    lux[sec =="pop", visi := input$showpop]
    lux[sec =="fossil", visi := input$showfossil]
    lux[sec =="land", visi := input$showland]
    lux[sec =="ghg", visi := input$showghg]
    lux[sec =="nonco2", visi := input$shownonco2]
    lux[sec =="source", visi := input$shownewsink]
    lux[sec =="sink", visi := input$shownewsink]
    lux[sec =="newsink", visi := input$shownewsink]
    lux[sec =="landcost", visi := input$shownewsink]
    
    # lux[sec =="source", visi := 1]
    # lux[sec =="sink", visi := 1]
    # lux[sec =="newsink", visi := 1]
    # lux[sec =="landcost", visi := 1]
    
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
    lux[sec =="averagedividend", visi := input$showaveragedividend]
    lux[sec =="countrydividend", visi := input$showcountrydividend]
    
    
    
    
    
    
  
    
    
    lux
    
  })
  output$lek <- reactive({
    rv$lek==TRUE
  })
  outputOptions(output, "lek", suspendWhenHidden = FALSE)
  
  
  cait = reactive ({
    
    
    
    
    
  })
  
  
  
  
  # creating intermediate data and combining data from different periods, need it for national stuff
  
  datss = reactive({
    budd = rv$budd
    # ppaaa = ppaaa()
    ppaa = as.data.table(ppaa)
    
    if (input$nonco2 == 1) {
      ppaa$avgfossil = ppaa$avgghg
    }
    
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
    
    ## making intermediate data
    
   
    
    
    
    # create data from last observed year to year before pricing begins
    start=ppaa[year ==lastyear & sec =="fossil", yy]
    nstart=ppaa[year ==lastyear & sec =="nonco2", yy]
    #land use emission start
    lstart = ppaa[year ==lastyear & sec =="land", yy]
    
    
    sinkstart = ppaa[year ==lastyear & sec =="sink", yy]
    sourcestart = ppaa[year ==lastyear & sec =="source", yy]
    
    
    # years for calculation
    yearl = lastyear:(as.numeric(input$vuo[1]))
    
    # years for data set
    yearl2 = (lastyear+1):(as.numeric(input$vuo[1])-1)
    
    ll = max(as.numeric(length(yearl)),0)
    time = max(as.numeric(length(yearl)),0)
    
    fossil = seq(start, as.numeric(input$fstart), length.out= time)
    nonco2 = seq(nstart, as.numeric(input$nonco2start), length.out= time)
    
    # [,-1]
    land = seq(lstart, as.numeric(input$lstart),length.out = time)
    
    sink = seq(sinkstart, as.numeric(input$sinkstart),length.out = time)
    source = seq(sourcestart, as.numeric(input$sourcestart),length.out = time)
    
    # [,-1]
    # take out the calculation start and end point that will be in the data anyway
    fossil = fossil[-1]
    land = land[-1]
    sink = sink[-1]
    source = source[-1]
    nonco2 = nonco2[-1]
    
    fossil = head(fossil,-1)
    land = head(land,-1)  
    nonco2=head(nonco2,-1)
    sink = head(sink,-1)  
    source = head(source,-1)  
    
    
    # fossil = rep(start, ll)
    # land = rep(lstart, ll)
    net = fossil+land
    ghg = fossil + nonco2 
    ## must fix so  that population is from population projection and not constant   
    
    
    pop = populaatio()[year %in% yearl2,pop]/1000000000
    
    year = yearl2
    
    ppax = data.frame(year, fossil)
    
    
    
    # ppax$fossil = fossil
    ppax$land = land
    ppax$net = net
    
    ppax$pop = pop
    ppax$nonco2 = nonco2
    ppax$ghg = ghg
    
    ppax$source = source
    ppax$sink = sink
    ppax$newsink = NA
    ppax$landcost = NA
    
    
    
    # choose according to gas specification  
    
    if (input$nonco2 ==0) {
      ppax$avgfossil = ppax$fossil/ppax$pop
    } else {
      ppax$avgfossil = ppax$ghg/ppax$pop
    }
    
    ppax$avgcost =NA
    ppax$dividend =NA
    ppax$avgnetcost =NA
    
    ppax$usercost = NA
    ppax$netcost =NA
    
    
    
    
    ppax <- gather(ppax, sec, yy, "fossil":"netcost")
    
    ppax= as.data.table(ppax)
    
    ppaa=as.data.table(ppaa)
    ppaa[sec=="pops", sec:="pop"]
    
    if (input$nonco2 ==0) {
      # W   ppaa[sec =="avgghg", yy:=NULL]
    } else {
      
      put = ppaa[sec=="avgghg",]
      ppaa[sec =="avgfossil", yy:=put$yy]
      
      
    }
    ppaa$landcost=NA
    
    dats = rbind(ppaa, ppax, dats, fill=TRUE)
    
    # rv$triggo = 1
    
    
    dats = as.data.table(dats)
    dats = dats[lux(), col:=i.col ,on=c("sec")]
    dats = dats[lux(), ala:=i.ala ,on=c("sec")]
    dats = dats[lux(), pos:=i.pos ,on=c("sec")]
    dats = dats[lux(), label:=i.label ,on=c("sec")]
    dats = dats[lux(), mark:=i.mark ,on=c("sec")]
    dats = dats[lux(), le:=i.le, on=c("sec")]
    
    dats = dats[lux(), visi:=i.visi, on=c("sec")]
    
    dats = dats[lux(), labbi:= i.labbi, on=c("sec")]
    
    
  })
  
  
  
  
  
  
  pacu = reactive ({
    
    #rv$lek to indicate whether to show country specific variables in left hand side
    
    # test if pacu is needed (if lax==1) then create pacu otherwise dont
    lax = 0
    
    lek = length(input$countr)
    rv$luk = lek
    if (lek > 1) {
      rv$lek =TRUE
      lax =1
    } else if (lek ==1) {
      
      if (input$countr %in% c(ll2)) {
        lax = 1
        rv$lek =TRUE
        
      } else {
        lax = 0
        # rv$lek =FALSE
        
        
      }
    } else if (lek==0) {
      rv$lek=FALSE
    }
    
    if (input$indi %in% c(ll2)) {
      lax = 1
    }
    if (input$nationalcoun %in% c(ll2)) {
      lax = 1
      rv$lek =TRUE
      
    }
    
    if (input$showcolplot ==TRUE) {
      lax=1
    }
    
    
    
    if (input$nationalcoun %in% c("none") & lek==0)  {
      rv$lek == FALSE
    }       
    if (lax ==1)  {
      
      # if (input$indi %in% c(ll2) || input$countr %in% c(ll2) || input$nationalcoun %in% c(ll2))  {
      
      datso = as.data.table(datss())
      
      # end is the global per capita emissions in neutarlity year
      end = datso[year ==rv$lyear & sec =="avgfossil", yy]
      paci = as.data.table(pack)
      
      paci = paci[var ==input$popc,]
      
      
      
      
      if (input$nonco2 ==0) {
        paci = paci[, yy:=co2cap]
        pacu = copy(paci)
        
      } else {
        paci = paci[, yy:=ghgcap]
        pacu = copy(paci)
        
      }
      
      years = rv$years
      
      
      
      
      ## bunkers
      
      cstart = paci[year ==lastyear & country =="Finland", bunkers/1000000000]
      
      start = input$fstart
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
      
      
      # }
      
      
      
      
      
      
      arg = function(start, end, convergence, cstart) {
        
        end - (start-cstart)*(1-convergence)*(end/start)
        
      }
      
      
      aat = c(ll2)
      
      
      withProgress( message="Calculating country trajectories, please wait",{
        
        dei2 = function(mm) {
          
          
          pacu[country ==mm & year %in% c(rv$ffyear:lastyear), countryfossil := yy]
          
          
          ## since last observation year to start year
          
          time= rv$fyear-lastyear
          
          # start, end, convergence, coustart
          cstart = pacu[country == mm & year == lastyear, yy]
          start =datso[sec =="avgfossil" & year == lastyear, yy]
          # end =datso[sec =="avgfossil" & year == lastyear, yy]
          
          end = datso[sec =="avgfossil" & year == rv$fyear, yy]
          # convergence =.01
          # have milder convergence for the period before the last year lyear is included
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
          
          # 
          # else if  (input$muo=="logarithmic" & cstart >=cend) {
          #   f3 <- function(rate,cstart,time, cend) {
          #     cend - (cstart - rate*log(time+1))
          #     
          #   }}
          # 
          # else if  (input$muo=="logarithmic" & cstart <cend) {
          #   f3 <- function(rate,cstart,time, cend) {
          #     cend - (cstart + rate*log(time+1))
          #     
          #   }}
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
          # else if  (input$muo=="logarithmic" & cstart >=cend) {
          #   g = function(cstart, rate, time) {
          #     c(cstart-rate*log(time+1))
          #   }}
          # else if  (input$muo=="logarithmic" & cstart <cend) {
          #   g = function(cstart, rate, time) {
          #     c(cstart+rate*log(time+1))
          #   }}
          
          
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
          
          # else if  (input$muo=="logarithmic" & cstart >=cend) {
          #   f3 <- function(rate,cstart,time, cend) {
          #     cend - (cstart - rate*log(time+1))
          #     
          #   }}
          # 
          # else if  (input$muo=="logarithmic" & cstart <cend) {
          #   f3 <- function(rate,cstart,time, cend) {
          #     cend - (cstart + rate*log(time+1))
          #     
          #   }}
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
          
          # else if  (input$muo=="logarithmic" & cstart >=cend) {
          #   g = function(cstart, rate, time) {
          #     c(cstart-rate*log(time+1))
          #   }}
          # else if  (input$muo=="logarithmic" & cstart <cend) {
          #   g = function(cstart, rate, time) {
          #     c(cstart+rate*log(time+1))
          #   }}
          
          ffossil = g(cstart, rate, 1:time)
          
          pacu[country ==mm & year %in% c((rv$fyear+1):rv$lyear), countryfossil := ffossil]
          
          
          # if (input$nonco2=="0") { 
          pacu[country ==mm &year %in% c(lastyear:rv$fyear), bunkers := bunkera]
          
          pacu[country ==mm &year %in% c(rv$fyear:rv$lyear), bunkers := bunker]
          # } else if (input$nonco2 =="1"){
          #   pacu[country ==mm &year %in% c(lastyear:rv$fyear), bunkers :=0]
          #   
          #   pacu[country ==mm &year %in% c(rv$fyear:rv$lyear), bunkers := 0]
          #   
          # }
          
          incProgress(1/length(aat))     
          
        }
        
        rr3 = lapply(aat, dei2)
        
      })
      
      
      ## create correction factor to account for that emissions multiplied by population
      ## don't equal for world emissions needed for budget
    
        pacu[year %in% c(lastyear+1:rv$lyear) & country %in% ll2,
           yyy := sum(countryfossil*pop)/1000000000, by=c("year")]
      
      if (input$nonco2==0){
        pacu[datso[sec=="fossil"], wyy :=i.yy, on=c("year")]
      } else {
        pacu[datso[sec=="ghg"], wyy :=i.yy, on=c("year")]
      }
      
      pacu[,cor:=((wyy)/(yyy+bunkers))]
      rv$cor = unique(pacu[year ==rv$yearc, cor])
      
      #      pacu = pacu[,cor:=((wyy*1000000)/(yyy+bunkers))-1]
      pacu[, fossilcountry:=cor*countryfossil]
      pacu[year <=lastyear, fossilcountry:=countryfossil]
      
      pacu[country %in% aat, tots := sum(fossilcountry*pop)/1000000000, by=c("year")]
      
      # pacu = pacu[country %in% input$countr,]
      pacu[datso[sec=="dividend"], dividend :=i.yy, on=c("year")]
      pacu[datso[sec=="price"], price :=i.yy, on=c("year")]
      pacu[datso[sec=="landcost"], landcost :=i.yy, on=c("year")]
      
      pacu[,countrycost:=fossilcountry*price+landcost ]
      pacu[, dividend:=(1-input$national/100)*dividend]
      pacu[, countrydividend:=input$national/100*countrycost]
      pacu[, averagedividend:=(input$national/100)*dividend]
      
      pacu[,countrynetcost:=countrycost-dividend-countrydividend+landcost]
      
      updateNumericInput(session, "dummy", value = 1)
      
      pacu
      
    } else {
      datss()
    }
  })
  
  # rv$pll = length(unique(datsss()$labbi))
  # observe({
  datsss= reactive ({
    rv$trigu
    
    req(input$indi1, cancelOutput = TRUE)
    req(input$indi2, cancelOutput = TRUE)
    req(input$paa, cancelOutput = TRUE)
    req(input$sprice, cancelOutput = TRUE)
    req(input$eprice, cancelOutput = TRUE)
    
    lux = lux()
    dats = as.data.table(datss())
    
    dats$country = "t"
    
    
    withProgress( message="Calculating country trajectories, please wait",{
    
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
    
    if (input$national != 0) {
      datsk = copy(dats)
      datsk = datsk[sec %in% c("dividend") & year %in% rv$fyear:rv$lyear,]
      datsk[, sec:="averagedividend"]
      datsk[, yy:=input$national/100*yy]
      datsk[, label:="Mean national dividend"]
      
      rv$nationall = c(input$national/100)
      
      dats[sec %in% "dividend", yy:=yy*(1-input$national/100)]
      
      dats=rbind(dats,datsk)
      
    }
    
    
    
    rv$avgfossil= dats[sec =="avgfossil" & year == rv$lyear, yy]
    rv$pop= dats[sec =="pop" & year == rv$lyear, yy]
    
    rv$price= dats[sec =="price" & year == rv$lyear, yy]
    rv$fossil= dats[sec =="fossil" & year == rv$lyear, yy]
    rv$userfossil= dats[sec =="userfossil" & year == rv$lyear, yy]
    rv$avgcost= dats[sec =="avgcost" & year == rv$lyear, yy]
    rv$usercost= dats[sec =="usercost" & year == rv$lyear, yy]
    rv$usernetcost= dats[sec =="netcost" & year == rv$lyear, yy]
    
    
    #avaragedividend, countrydividend
    #here rows for averagenational
    
    
    # here make new rows for country national
    
    ### ADD HERE years before fyear 
    
    if (input$nationalcoun %in% c(ll2)) {
      req(pacu())
      pacu = pacu()
      
      # cour1  = pacu[country ==input$nationalcoun & year %in% rv$fyear:rv$lyear, countrycost]
      # cour2  = pacu[country ==input$nationalcoun & year %in% rv$fyear:rv$lyear, c(dividend+nationaldividend)]
      cour2  = pacu[country ==input$nationalcoun & year %in% rv$fyear:rv$lyear, c(dividend+countrydividend)]
      rv$usernatl  = pacu[country ==input$nationalcoun & year %in% rv$yearc, c(countrydividend)]
      
      usercost =  dats[year %in% rv$fyear:rv$lyear & sec =="usercost",yy]
      # dats[year %in% rv$fyear:rv$lyear & sec =="usercost", yy:=cour1 ]
      
      
      dats[year %in% rv$fyear:rv$lyear & sec =="netcost", yy:=usercost-cour2 ]
      
      
      # countrydividend = pacu[country ==input$nationalcoun & year %in% rv$fyear:rv$lyear, countrydividend]
      # rv$usernatdividendl = pacu[country ==input$nationalcoun & year %in% rv$yearc, countrydividend] 
      # datsk = copy(dats)
      # datsk = datsk[sec=="dividend" & year %in% rv$fyear:rv$lyear,]
      # countrydividend = pacu[country ==input$nationalcoun & year %in% rv$fyear:rv$lyear, countrydividend]
      # datsk[, sec:="countrydividend"]
      # datsk[, yy:=countrydividend]
      # datsk[, label:=paste0(input$nationalcoun," national dividend")]
      # 
      # dats = rbind(dats, datsk)
      
      
      
    }
    
    # here make new rows for country national if national > 0 ja jos ei ole jo tehny yhta ylempana   
    if (is.null(input$countr)) {
      
    } else {
      
      req(pacu())
      pacu = pacu()
      
      aat = c(input$countr)
      # prin
      dei2 = function(mm) {
        
        datsk = copy(dats)
        
        datsk = datsk[sec %in% c("fossil", "pop", "netcost", "usercost", "dividend") & year %in% rv$ffyear:rv$lyear,]
        # 
        datsk$country = mm
        
        
        datsj = datsk[sec == "fossil" & year %in% rv$ffyear:rv$lyear,]
        countryfossil = pacu[country ==mm & year %in% rv$ffyear:rv$lyear, fossilcountry]
        datsj[,sec:="countryfossil"]
        datsj[,yy:=countryfossil]
        
        
        datsjj = datsk[sec == "usercost" & year %in% rv$fyear:rv$lyear,]
        countrycost = pacu[country ==mm & year %in% rv$fyear:rv$lyear, countrycost]
        datsjj[,sec:="countrycost"]
        datsjj[, yy:=countrycost]
        
        
        
        
        
        datsjjj = datsk[sec == "netcost" & year %in% rv$fyear:rv$lyear,]
        countrynetcost = pacu[country==mm & year %in% rv$fyear:rv$lyear, countrynetcost]
        datsjjj[,sec:="countrynetcost"]
        datsjjj[, yy:=countrynetcost]
        
        
        datsjjjj = datsk[sec == "pop" & year %in% rv$fyear:rv$lyear,]
        countrypop = pacu[country==mm & year %in% rv$fyear:rv$lyear, pop]/1000000000
        # pop:=i.pop/1000000000, on=c("year")
        datsjjjj[,sec:="countrypop"]
        datsjjjj[, yy:=countrypop]
        
        datsjjjjj = datsk[sec == "dividend" & year %in% rv$fyear:rv$lyear,]
        countrydividend = pacu[country==mm & year %in% rv$fyear:rv$lyear, countrydividend]
        # pop:=i.pop/1000000000, on=c("year")
        datsjjjjj[,sec:="countrydividend"]
        datsjjjjj[, yy:=countrydividend]
        
        
        
        if (rv$lang =="eng") {
          datsj[,label:=paste0(mm, " emissions")]
          datsjj[,label:=paste0(mm, " mean costs")]
          datsjjj[,label:=paste0(mm, " mean net costs")]
          datsjjjj[,label:=paste0(mm, " population")]
          datsjjjjj[,label:=paste0(mm, " dividend")]
          
          
        } else if (rv$lang =="fin") {
          datsj[,label:=paste0(mm, " keskipäästöt")]
          datsjj[,label:=paste0(mm, " keskimenot")]
          datsjjj[,label:=paste0(mm, " nettokeskimenot")]
          datsjjjj[,label:=paste0(mm, " väestö")]
          datsjjjjj[,label:=paste0(mm, " hiiliosinko")]
          
        }
        # 
        
        
        datsy = rbind(datsj, datsjj, datsjjj, datsjjjj, datsjjjjj)
        
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
    
    # numbers to use in result indicator sector and elsewhere
    rv$fossill= format(round(dats[sec=="fossil" & year ==rv$yearc, yy], 1), nsmall=1)
    rv$landl = format(round(dats[sec=="land" & year ==rv$yearc, yy], 1), nsmall=1)
    rv$netl = format(round(dats[sec=="net" & year ==rv$yearc, yy], 1), nsmall=1)
    rv$ghgl = format(round(dats[sec=="ghg" & year ==rv$yearc, yy], 1), nsmall=1)
    rv$nonco2l = format(round(dats[sec=="nonco2" & year ==rv$yearc, yy], 1), nsmall=1)
    rv$popl = format(round(dats[sec=="pop" & year ==rv$yearc, yy], 1), nsmall=1)
    rv$avgfossill = format(round(dats[sec=="avgfossil" & year ==rv$yearc, yy], 2), nsmall=2)
    rv$pricel = format(round(dats[sec=="price" & year ==rv$yearc, yy], 0), nsmall=0)
    rv$avgcostl = format(round(dats[sec=="avgcost" & year ==rv$yearc, yy], 0), nsmall=0)
    rv$dividendl = format(round(dats[sec=="dividend" & year ==rv$yearc, yy], 0), nsmall=0)
    rv$avgnetcostl = format(round(dats[sec=="avgnetcost" & year ==rv$yearc, yy], 0), nsmall=0)
    rv$userfossill = format(round(dats[sec=="userfossil" & year ==rv$yearc, yy], 2), nsmall=2)
    rv$usercostl = format(round(dats[sec=="usercost" & year ==rv$yearc, yy], 0), nsmall=0)
    rv$netcostl = format(round(dats[sec=="netcost" & year ==rv$yearc, yy], 0), nsmall=0)
    rv$countryfossill = format(round(dats[sec=="countryfossil" & year ==rv$yearc, yy], 2), nsmall=2)
    rv$countrycostl = format(round(dats[sec=="countrycost" & year ==rv$yearc, yy], 0), nsmall=0)
    rv$countrynetcostl = format(round(dats[sec=="countrynetcost" & year ==rv$yearc, yy], 0), nsmall=0)
    rv$countrypopl = format(round(dats[sec=="countrypop" & year ==rv$yearc, yy], 1), nsmall=1)
    rv$countrydividendl = format(round(dats[sec=="countrydividend" & year ==rv$yearc, yy], 0), nsmall=0)
    rv$averagedividendl = format(round(dats[sec=="averagedividend" & year ==rv$yearc, yy], 0), nsmall=0)
    
    rv$sourcel = format(round(dats[sec=="source" & year ==rv$yearc, yy], 1), nsmall=1)
    rv$sinkl = format(round(dats[sec=="sink" & year ==rv$yearc, yy], 1), nsmall=1)
    rv$newsinkl = format(round(dats[sec=="newsink" & year ==rv$yearc, yy], 1), nsmall=1)
    rv$landneed = format(round(input$lstart - dats[sec=="land" & year ==rv$yearc, yy], 1), nsmall=1)

    rv$landcostl = format(round(dats[sec=="landcost" & year ==rv$yearc, yy], 0), nsmall=0)
    
    
    
    if (input$nonco2 ==0) {
      rv$totall =rv$fossill
      
      rv$totalcolor =fos
    } else {
      rv$totall =rv$ghgl
      
      rv$totalcolor =tot
      
    }
    
    
    
    
    if (dats[sec=="land" & year ==rv$yearc, yy]>0) {
      rv$landplus = c("+")
    } else { rv$landplus = c("")}
    
    # if (dats[sec=="land" & year ==rv$yearc, yy]>0) {
    #   rv$landplus = c("+")
    # } else { rv$landplus = c("")}
    # 
    
    dats = dats[visi==1,]
    
    
    
    
    
    # rv$pll = nrow(distinct(dats, labbi))
    valus =as.numeric(unique(dats[sec %in% c("avgcost", "usercost", "netcost", "avgnetcost", "dividend", "landcost", "countrycost", "countrynetcost", "averagedividend", "countrydividend"),max(yy, na.rm=TRUE)]))
    dats[sec %in% c("avgcost", "usercost", "netcost", "dividend", "avgnetcost", "landcost", "countrycost", "countrynetcost", "averagedividend", "countrydividend"), tyy:= yy/(valus/100)]
    
    dats[sec %in% c("dividend", "averagedividend", "countrydividend"), tyy:=-1*tyy]
    dats$valuso = valus
    
    # dats[sec =="countryfossil", visi:=1]
    # dats = dats[visi==1,]
    
    
    valus =as.numeric(unique(dats[sec %in% c("price"),max(yy, na.rm=TRUE)]))
    dats[sec %in% c("price"), tyy:= yy/(valus/100)]
    
    valus =as.numeric(unique(dats[sec %in% c("pop", "countrypop"),max(yy, na.rm=TRUE)]))
    dats[sec %in% c("pop", "countrypop"), tyy:= yy/(valus/100)]
    
    valus = as.numeric(unique(dats[sec %in% c("fossil", "land", "net", "ghg", "nonco2", "source", "sink", "newsink"),max(yy, na.rm=TRUE)]))
    dats[sec %in% c("fossil", "land", "net", "ghg", "nonco2", "dummy", "source", "sink", "newsink"), tyy:= yy/(valus/100)]
    
    dats[sec =="countryfossil", visi:=1]
    
    
    valusk = as.numeric(unique(dats[sec %in% c("userfossil", "avgfossil", "countryfossil"),max(yy,  na.rm=TRUE)]))
    dats[sec %in% c("userfossil", "avgfossil", "countryfossil"), tyy:= yy/(valusk/100)]
    
    
    dats = dats[order(pos)]
    
    dats =  dats[, pos :=1:.N, by=year]
    
    
    xx=0
    if (nrow(dats[sec %in% c("fossil", "land", "net", "ghg", "nonco2", "source", "sink", "newsink"),]) >0) {
      xx=xx+1
      dats[sec %in% c("fossil", "land", "net", "ghg", "nonco2", "source", "sink", "newsink"), prio:=xx]
    }
    if (nrow(dats[sec %in% c("pop"),]) >0) {
      xx=xx+1
      dats[sec %in% c("pop"), prio:=xx]
    }
    if (nrow(dats[sec %in% c("avgfossil", "userfossil"),]) >0) {
      xx=xx+1
      dats[sec %in% c("avgfossil", "userfossil"), prio:=xx]
    }
    if (nrow(dats[sec %in% c("price"),]) >0) {
      xx=xx+1
      dats[sec %in% c("price"), prio:=xx]
    }
    if (nrow(dats[sec %in% c("avgcost", "dividend", "avgnetcost", "usercost","landcost", "netcost"),]) >0) {
      xx=xx+1
      dats[sec %in% c("avgcost", "dividend", "avgnetcost", "usercost", "landcost", "netcost"), prio:=xx]
    }
    pink =1
    
    rv$triggo = 1
    
    
    # ogger <- observe({
    #   # updateBox("tutobox", action = "remove")
    #   # shinyjs::click("add")
    # 
    #   # if (isolate(rv$skip==1)) {
    #   if (rv$skip==1) {
    # 
    # 
    #     # skip first reactive sequence
    #     rv$skip=0
    #     # launch next reactive sequence
    #     invalidateLater(100,session)
    #   } else {
    #     # hypothetical expensive computation
    #     # Sys.sleep(2)
    # 
    #     # hypothetical plot
    #     # hist(rnorm(20))
    #     shinyjs::disable("go")
    # 
    #     ogger$destroy()
    # 
    #   }
    # }
    # )
    

     rv$triggor=2
    
    })
    
    dats = as.data.table(dats)
    
  })
  
  

  
  
  # ogger <- observeEvent(rv$triggor, {
  #   if (rv$triggor==2) {
  #   
  #   shinyjs::disable("go")
  # 
  #   ogger$destroy()
  #   }
  #   # }
  # }
  # )
  # 

  
  
  datssst = reactive({
    tab = as.data.table(datsss())
    tab = tab[order(pos)]
    
    seclist = c("fossil", "land", "net", "ghg", "nonco2", "pop", "avgfossil",
                "price", "avgcost","dividend","avgnetcost","userfossil","landcost",
                "usercost",  "netcost", "averagedividend", "nationaldividend", "source", "sink",
                "newsink")
    tab = tab[sec %in% seclist,]
    
    
    # tab[, lyy := paste0(format(round(yy, 1, " ", mark)]
    tab[, lyy:=  as.character(paste0(sprintf(paste0("%0.", (le+1),"f"),round(yy,le+1))," ", mark))]
    tab = tab[,c("year","lyy", "label")]
    
    
    
    
    tab = as.data.frame(tab)
    tab = spread(tab, key="label",value="lyy")
    # tab = as.data.table(tab)
    lalist = c("year","Fossil emissions", "Land emissions/sinks", "Net emissions","Total emissions","Non-CO2 emissions", "World population",
               "Mean fossil emissions",  "Carbon price", "Mean carbon costs",
               "Carbon dividend", "Mean national dividend", "Dividend for chosen coutnry", "Mean net costs", "User fossil emissions", "User carbon costs", "User net costs",
               "Land emissions", "ggg", "ggggg"
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
  
  ogug <- observe({
    rv$pll = length(unique(datsl()$labbi))
    # shinyjs::click("add")
    ogug$destroy() # destroy observer as it has no use after initial button click
  })
  
  datsl = reactive ({
    
    datsl= as.data.table(datsss())
    
    
    datsl[,yearu := as.numeric(rv$yearc)]
    
    datsl = datsl[year <= yearu,]
    # & year >=rv$fyear
    
    rv$pll = length(unique(datsl$labbi))
    # rv$pll = length(unique(datsl$labbi))
    
    
    datsl[, ayy:=mean(yy), by=c("sec")]
    
    datsl[year < rv$fyear & !(sec=="dummy"), ala := .1]
    
    if (rv$lang == "eng") {
      
      datsl[, labu:=  as.character(paste0(sprintf(paste0("%0.",le,"f"),round(yy,le))," ", mark, ", ", label))]  
      
      if (input$view ==2){
        
        if (rv$pll < 2){
          datsl[, labuf:=  as.character(paste0(label," ", sprintf(paste0("%0.",le,"f"),round(yy,le))," ", mark))]  
        } else   if (rv$pll > 1) {
          datsl[, labuf:=  as.character(paste0(sprintf(paste0("%0.",le,"f"),round(yy,le))," ", mark))]  
          
        }
      }
      if (input$view==3) {
        datsl[, labuf:=  as.character(paste0(label," ", sprintf(paste0("%0.",le,"f"),round(yy,le))," ", mark))]  
        
        
      }
      
      if (input$view==4) {
        datsl[, labuf:=  as.character(paste0(label," ", sprintf(paste0("%0.",le,"f"),round(yy,le))," ", mark))]  
        
        
      }
      if (input$view==1) {
        datsl[, labuf:=  as.character(paste0(label, " ", format(round(yy,le), decimal.mark=",", nsmall=le) ," ", mark)), by=sec]  
        
        
      }
    }
    else if (rv$lang =="fin") {
      # datsc[, labu:=  as.character(paste0(sprintf(paste0("%0.",le,"f"), round(yy,le))," ", mark, ", ", label))]  
      
      # datsc[, labu:=  as.character(paste0(sprintf(paste0("%0.",le,"s"),format(round(yy,le), decimal.mark=",")) ," ", mark, ", ", label))]  
      # datsl[, labu:=  as.character(paste0(format(sprintf(paste0("%0.",le,"s"),round(yy,le)), decimal.mark=",") ," ", mark, ", ", label))]  
      # datsl[, labu:=  as.character(paste0( sprintf(paste0("%0.",5,"s"),format(yy,decimal.mark=",")) ," ", mark, ", ", label))]  
      # sprintf(paste0("%0.",4,"s"),format(1.511,decimal.mark=","))
      datsl[, labu:=  as.character(paste0(format(round(yy,le), decimal.mark=",", nsmall=le) ," ", mark, ", ", label)), by=sec]  
      
      if (input$view ==2){
        
        if (rv$pll < 2){
          datsl[, labuf:=  as.character(paste0(label, " ", format(round(yy,le), decimal.mark=",", nsmall=le) ," ", mark)), by=sec]  
          
          
          # datsl[, labuf:=  as.character(paste0(label," " , sprintf(paste0("%0.",le,"s"),format(round(yy,le), decimal.mark=",")) ," ", mark))]  
        } else  if (rv$pll > 1) {
          datsl[, labuf:=  as.character(paste0(format(round(yy,le), decimal.mark=",", nsmall=le) ," ", mark)), by=sec]  
          
          # datsl[, labuf:=  as.character(paste0(sprintf(paste0("%0.",le,"s"),format(round(yy,le), decimal.mark=",")) ," ", mark))]  
          
        }
      }
      if (input$view==3) {
        # datsl[, labuf:=  as.character(paste0(sprintf(paste0("%0.",le,"s"),format(round(yy,le), decimal.mark=",")) ," ", mark))]  
        datsl[, labuf:=  as.character(paste0(label, " ", format(round(yy,le), decimal.mark=",", nsmall=le) ," ", mark)), by=sec]  
        
      }
      if (input$view==4) {
        # datsl[, labuf:=  as.character(paste0(sprintf(paste0("%0.",le,"s"),format(round(yy,le), decimal.mark=",")) ," ", mark))]  
        datsl[, labuf:=  as.character(paste0(label, " ", format(round(yy,le), decimal.mark=",", nsmall=le) ," ", mark)), by=sec]  
        
      }
      if (input$view==1) {
        datsl[, labuf:=  as.character(paste0(label, " ", format(round(yy,le), decimal.mark=",", nsmall=le) ," ", mark)), by=sec]  
        
        
      }
      
      # datsc[, labu:=  as.character(paste0(format(round(yy,le), decimal.mark=",", nsmall=le )," ", mark, ", ", label))]  
      
      
    }
    
    datsl
    
  })
  
  # eventReactive(input$go, {
  #   
  #   rv$pll = length(unique(datsss()$labbi))
  # })
  # 
  datsc = reactive ({
    datsc= as.data.table(datsl())
    
    datsc[,yearu := as.numeric(rv$yearc)]
    
    datsc = datsc[year == yearu,]
    # pacuc = pacuc[year == yearu,]
    
    
    # observe({
    
    # })
    #this dropping?
    
    # if (rv$lang =="eng"){
    
    # } ,decimal.mark=","
    # if (rv$lang=="fin") {
    # datsc[, labu:=  as.character(paste0(sprintf(paste0("%0.",le,"f"),round(yy,le))," ", mark, ", ", label))]  
    # 
    # }
    # paste0(sprintf(paste0("%0.",le,"f"),round(yy,le))," ", mark, ", ", label, ", ", year)
    
    # THIS AWAY:     
    
    # ppaa = ppaa[pop2[var==3,],pop:=i.pop/1000000000, on=c("year")]
    
    datsc[, alas :=(year-rv$fyear)/100]
    
    
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
  
  
  stylerota <- reactive({
    # if (is.null(input$countr)) { 
    if (input$view ==4) {
      # if (input$nok =="EXTRA: Country profiles") { 
      "#rota {transform:rotateX(180deg);}

#rota img { transform:rotateX(180deg);}
      
#rota .shiny-output-error {transform:rotateX(180deg);}
"
    }   else {
      "     #rota {transform:rotateX(0deg);}
      
      #rota img { transform:rotateX(0deg);}
      
      #rota .shiny-output-error {transform:rotateX(0deg);}
      "
      
    }
    
  }
  )
  
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
  
  
  style2 <- reactive({
    # if (is.null(input$countr)) { 
    if (rv$alert6 ==TRUE) {
      # if (input$nok =="EXTRA: Country profiles") { 
      "#tablu3 {visibility: visible}"
      
    }
    else if (rv$alert6 ==FALSE) {
      "#tablu3 {visibility: collapse}"
      
    }
    
  }
  )
  
  style3 <- reactive({
    # if (is.null(input$countr)) { 
    if (rv$alert8 ==TRUE) {
      # if (input$nok =="EXTRA: Country profiles") { 
      "#tablu4 {visibility: visible}"
      
    }
    else if (rv$alert8 ==FALSE) {
      "#tablu4 {visibility: collapse}"
      
    }
    
  }
  )
  
  
  
  
  style4 <- reactive({
    # if (is.null(input$countr)) { 
    if (rv$alert10 ==TRUE) {
      # if (input$nok =="EXTRA: Country profiles") { 
      "#tablu5 {visibility: visible}"
      
    }
    else if (rv$alert10 ==FALSE) {
      "#tablu5 {visibility: collapse}"
      
    }
    
  }
  )
  
  rootcss <- reactive({
    # if (is.null(input$countr)) { 
    # if (rv$alert6 ==TRUE) {
    # if (input$nok =="EXTRA: Country profiles") { 
    "
    :root,
.theme--light{
    @each $varName, $value in $themeBase__map {
        --#{$varName}: $value;
    }
}    
    
    
    :root {--colb: red}
    
    
    
    
    
    
    
    "
    
    # }
    # else if (rv$alert6 ==FALSE) {
    # "#tablu3 {visibility: collapse}"
    # 
    # }
    
  }
  )
  
  # 
  
  
  
  
  # 
  # output$engtab <- renderUI({
  #    tagList(
  #     # bs4Dash::bs4Table(
  #     # # "nerf",
  #     # 
  #     # cardWrap = TRUE,
  #     # bordered = TRUE,
  #     # striped = TRUE,
  #     # 
  #     # list(
  #     # 
  #     #   headTitles = 
  #         list(
  # 
  #         cuk(fos, "labelfossil", "infofossil", "showfossil", NULL, TRUE),
  #         cuk(lul, "Land emissions/sinks", "infolul", "showland", NULL, TRUE),
  #         cuk(net, "Net emissions", "infonet", "shownet", NULL, TRUE),
  #         cuk(pop, "Population", "infopop", "showpop", NULL, FALSE),
  #         cuk(fpop, "Mean fossil emissions", "infoavgfossil", "showavgfossil", NULL, TRUE),
  #         cuk(tax,"Carbon price", "infoprice", "showprice", NULL, FALSE)
  #         # cuk(, "", "", "", NULL, TRUE),
  # 
  # 
  #        
  #       # )
  #     # )
  #     )
  # 
  #    )
  # 
  # })
  # 
  # output$ggg <- renderUI({
  #   tags$head(
  #     tags$style(
  #       HTML(
  #         paste0(c(style()), collapse = "\n")
  #       )
  #     )
  #   )})
  # 
  output$stylerota <- renderUI({
    tags$head(
      tags$style(
        HTML(
          paste0(c(stylerota()), collapse = "\n")
        )
      )
    )})
  
  output$css_style <- renderUI({
    tags$head(
      tags$style(
        HTML(
          paste0(c(style()), collapse = "\n")
        )
      )
    )})
  
  output$css_style2 <- renderUI({
    tags$head(
      tags$style(
        HTML(
          paste0(c(style2()), collapse = "\n")
        )
      )
    )})
  
  output$css_style3 <- renderUI({
    tags$head(
      tags$style(
        HTML(
          paste0(c(style3()), collapse = "\n")
        )
      )
    )})
  
  output$css_style4 <- renderUI({
    tags$head(
      tags$style(
        HTML(
          paste0(c(style4()), collapse = "\n")
        )
      )
    )})
  
  output$cssroot <- renderUI({
    tags$head(
      tags$style(
        HTML(
          paste0(c(rootcss()), collapse = "\n")
        )
      )
    )})
  
  #   output$muor <- renderUI({
  #     tagList(
  #     radioButtons("muo", "Shape of the emission curve",
  #                  c(
  #                     paste0(
  #                      "Linear drop ("
  #                       ,rv$rateli, " Gt per year)") 
  #                    = "linear",
  #                    # paste0(
  #                      "Percentual drop ("
  #                      # , rv$ratepr, 
  #                      # "% per year")
  #                    = "percentual"
  #                    # ,
  #                    # "Logarithmic drop" = "logarithmic"
  #                  # )
  #                  )
  # ) )
  #   })
  # 
  # output$c2 <- renderUI({
  #   tagList(9)
  #   
  #   
  # })
  # 
  
  output$hovers = renderText({
    paste0(rv$y,"  ", rv$x)
  })
  output$mobileuser = renderText({
    c("Mobile use is not recommended, but if done, please use landscape rotation")
  })
  
  
  # 
  output$tablx = renderDataTable(server=FALSE,{
    pacu()},
    extensions = 'Buttons',
    
    options = list(
      pageLength = 20,
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
      pageLength = 100,
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
  teksvari = "black"
  teksvari = hsv(.23, 0, .45)
  
  
  # observeEvent(input$colplot_hover, {
  #   rv$x = input$colplot_hover$x
  #   rv$y = input$colplot_hover$y
  # })  
  
  ht = reactive(as.numeric(session$clientData$output_plot_width*.7))
  ht = reactive(
    
    function() {
      session$clientData$output_plot_width*.75
    }
  )
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  sec = reactive({ 
    withProgress( message="Drawing graph, please wait",{
      
      mil = rv$ffyear
      
      datsf = datsf()
      datsc = datsc()
      datsl = datsl()[year >= mil,]
      datsss = datsss()[year >= mil,]
      
      si = 
        function(per) {
          per*session$clientData$output_plot_width*session$clientData$pixelratio/.8/
            (570+.15*session$clientData$output_plot_width)
        }
      # }
      if (input$isMobile=="FALSE") {
        
        lsi = 
          function(per) {
            per*session$clientData$output_plot_width*session$clientData$pixelratio/500
          }
        
        linehi = 1
      }
      
      if (input$isMobile=="TRUE") {
        
        lsi = 
          function(per) {
            per*session$clientData$output_plot_width*session$clientData$pixelratio/1500
          }
        linehi = .8
        
      }
      
      mi = min(min((datsss[,tyy]), na.rm=T)*1.2,-40)
      ma = 137
      # hi = ma - mi
      
      
      mix = mil
      max = 2130
      max = 2142
      
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
      # fam = "roboto"
      fam = fam
      bgc = rv$bgc
      teksvari = rv$teksvari
      obsvari = rv$obsvari
      
      
      plot1=   ggplot(datsf)+
        
        # graph limits
        geom_segment(data=da,
                     aes(x=mil-10, xend=2100, y=0, yend=0), 
                     color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=0) +
        
        geom_point(data=da,
                   aes(x=2140,  y=100),
                   color=teksvari, alpha=0, size=si(2))+
        #bottom years
        geom_text(data = datsss[year %in% c(seq(mil, 2100, 10)) & sec =="dummy",], 
                  aes(x=year, y=.76*mi, label=c(year)),
                  color=teksvari, angle=0,size =si(2.4), alpha=.6) +
        
        # vertical lines
        geom_segment(data = datsss[year %in% seq(mil, 2100, 10) & sec =="dummy",],
                     (aes(x=year, xend = year, y=100, yend=.72*mi)), 
                     color=teksvari, linetype="dashed", linewidth=lsi(.4), alpha=.08) +
        
        geom_text(data=da,
                  aes(x=mil, y=-4), label = paste0("0"),
                  col=teksvari, fontface="bold" ,  size =si(2.3), hjust =0, vjust=0, angle=c(0)) +
        # horizontal lines
        
        geom_segment(data=datsc[sec=="dummy",],
                     aes(x=mil, xend=rv$lyear, y=0, yend=0), 
                     color=teksvari, linetype ="dashed",linewidth=lsi(seg)*2, alpha=.3) +
        
        geom_segment(data=datsc[sec=="dummy",],
                     aes(x=mil, xend=rv$lyear, y=100, yend=100),
                     color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=.2) +
        geom_segment(data=datsc[sec=="dummy",],
                     aes(x=mil, xend=rv$lyear, y=75, yend=75), 
                     color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=.2) +
        geom_segment(data=datsc[sec=="dummy",],
                     aes(x=mil, xend=rv$lyear, y=50, yend=50), 
                     color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=.2) +
        geom_segment(data=datsc[sec=="dummy",],
                     aes(x=mil, xend=rv$lyear, y=25, yend=25), 
                     color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=.2) +
        
        geom_point(data=da, aes(x=2030, y=124), alpha=0) +
        
        # blue line for yearc      
        geom_segment(data=da,
                     aes(x=rv$yearc, xend=rv$yearc, y=ma, yend = mi), 
                     color=blu, alpha=.4, linewidth=lsi(1.4))+
        
        
        # geom_text(data=da,
        #           aes(x=mil, y=44), label = rv$warn,
        #           col=teksvari, fontface="bold" ,  size =si(2.3), hjust =0, vjust=0, angle=c(0)) +
        
        
        geom_point(data=datsc, 
                   aes(y=tyy, x=year, group=interaction(sec, country),  alpha=ala), 
                   size=lsi(points*2), color=blu) + 
        
        # graphs for fyear to yearc
        geom_area(data=datsl[year < rv$fyear+1,], 
                  aes(y=tyy, x=year, group=interaction(sec, country),  fill=col),
                  size=si(points), alpha=.15/nrow(datsc), position = 'identity') + 
        geom_area(data=datsl[year > rv$fyear-1,], 
                  aes(y=tyy, x=year, group=interaction(sec, country),  fill=col),
                  size=si(points), alpha=.35/nrow(datsc), position = 'identity') + 
        
        geom_line(data=datsl,
                  aes(y=tyy, x=year, group=interaction(sec, country), color=col, alpha=ala), 
                  linewidth=lsi(lines)) + 
        geom_point(data=datsl, 
                   aes(y=tyy, x=year, group=interaction(sec, country), color=col,  alpha=ala),
                   size=lsi(points)) + 
        
        
        # graphs for first year to lyear 
        geom_line(data=datsss, 
                  aes(y=tyy, x=year, group=interaction(sec, country), color=col), 
                  linewidth=lsi(lines), alpha=.1) + 
        geom_point(data=datsss, 
                   aes(y=tyy, x=year, group=interaction(sec, country), color=col, alpha=ala),
                   size=lsi(points), alpha=.1) + 
        
        
        
        # pricing and neutrality vertical lines
        geom_segment(data=da,
                     aes(x=rv$fyear, xend=rv$fyear, y=110, yend = mi),
                     color=teksvari, linewidth=lsi(.4), linetype = "dashed", alpha=.2 ) +
        geom_segment(data=da, 
                     aes(x=rv$lyear, xend=rv$lyear, y=110, yend = mi),
                     color=teksvari, linewidth=lsi(.4), linetype = "dashed", alpha=.2 ) +  
        
        # observed and simulated vertical line
        
        
        geom_segment(data=da,
                     aes(x=2021.5, y=100, xend=2021.5, yend=mi), 
                     color= obsvari,linewidth=lsi(.6), linetype="dashed", alpha=.2) +
        
        # yearc labels
        geom_label_repel(data=datsc,
                         aes(x=year+3, y=tyy,
                             label=labu,
                             color=col, alpha=ala*100),
                         fill=bgc,
                         hjust=0, size=si(2.2), fontface="bold",
                         family = fam,
                         segment.size =NA,
                         direction = "y",
                         label.padding =0,
                         xlim=c(mil,2177),
                         label.size=0,
                         max.iter=5000,
                         force=.01, force_pull=10,box.padding=.1 ,
                         seed=5) +
        
        
        
        scale_color_identity() + 
        scale_alpha_identity() + 
        
        scale_fill_identity() + 
        
        scale_x_continuous(breaks = seq(mil, 2100, 10)) +
        coord_cartesian( ylim=c(mi, ma), xlim = c(mix, max),clip ="off") +
        
        
        theme(
          axis.title.x=element_blank(),
          plot.title=element_text(lineheight = 0.8, hjust=0.5, size=33, family="Alegreya Sans Bold"),
          plot.subtitle=element_text(lineheight = 0.8, size=20, hjust=0.5),
          axis.text.x=element_blank(),
          plot.margin = margin(-5*input$dim[1]/2990,-5*input$dim[1]/90,-5*input$dim[1]/290,-5*input$dim[1]/200, unit = "pt"),
          panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          plot.background = element_rect(fill =bgc, color=NA), 
          panel.background = element_rect(fill = bgc ,  color=NA)
        )
      
      
      
      if (rv$lang =="eng") {
        
        plot1 = plot1 +
          geom_text(data=da,
                    aes(x=rv$fyear, y=116), label =paste0("Pricing starts: ", rv$fyear),
                    color=teksvari, hjust=.5, size=si(2.2), fontface="bold") +
          
          geom_text(data=da,
                    aes(x=rv$lyear, y=116), label =paste0("Neutrality: ", rv$lyear),
                    color=teksvari, hjust=.5, size=si(2.2), fontface="bold") +
          
          
          
          
          
          
          geom_text(data=da,
                    aes(x=mil+10, y= 131),
                    label = paste0(rv$budget, " Gt: Net CO2 emission budget from 2020 onwards to keep temperature increase below ",rv$budinfo ),
                    size=si(1.7), color=net, hjust=0, fontface="bold") +
          
          
          geom_text(data=da,
                    aes(x=rv$fyear, y= 127),
                    label = paste0( format(round(rv$sumnet,1), nsmall=1), " Gt used from 2020 to ", rv$fyear-1
                    ),
                    size=si(1.7), color=net, hjust=1, fontface="bold") +
          
          
          
          
          geom_text(data=datsc[, .SD[which.max(tyy)]],
                    aes(x=year+2, y=max(tyy)+9, label=paste0("Year ",year, " values:")),
                    color=teksvari, hjust=0, size=si(2.2), fontface="bold",  family = fam) +
          
          
          
          geom_text(data=da,
                    aes(x=2020.5, y=.9*mi), label = paste0("Historical\n <=2021"),
                    col= obsvari, fontface="bold" ,  size =si(1.9), hjust =1, vjust=0.5, angle=c(0),
                    alpha=.5, lineheight=linehi) +
          
          geom_text(data=da,
                    aes(x=2022.5, y=.9*mi), label = paste0("Simulated\n2022=>"),
                    col= obsvari, fontface="bold" ,  size =si(1.9), hjust =0, vjust=0.5,
                    angle=c(0), alpha=.5, lineheight=linehi) 
        
        # geom_text(data=da,
        #           aes(x=mil-5, y=.9*mi), label = paste0("www.globalcarbonprice.com \nData: UN, IPCC, Friedlingstein et al. 2022"),
        #           col=teksvari, fontface="bold", lineheight=linehi+.2 ,  size =si(1.5), hjust =0, vjust=0.5, angle=c(0), alpha=.5)
        
        if (rv$yearc >= rv$fyear) {
          plot1 = plot1 + 
            geom_text(data=da,
                      aes(x=rv$yearc, y= 123),  label = paste0(
                        format(round(rv$rvtotal,1), nsmall=1), " Gt used from ", rv$fyear, " to " , rv$yearc
                      ) ,
                      size=si(1.7), color=net, hjust=1, fontface="bold") 
        }  
        
      }
      
      if (rv$lang == "fin") {
        
        plot1 = plot1 +
          geom_text(data=da,
                    aes(x=rv$fyear, y=116), label =paste0("Hinnoittelu alkaa: ", rv$fyear),
                    color=teksvari, hjust=.5, size=si(2.2), fontface="bold") +
          
          geom_text(data=da,
                    aes(x=rv$lyear, y=116), label =paste0("Neutraalius: ", rv$lyear),
                    color=teksvari, hjust=.5, size=si(2.2), fontface="bold") +
          
          
          
          
          geom_text(data=da,
                    aes(x=mil+10, y= 131),
                    label = paste0(rv$budget, " Gt: Nettopäästöjen (CO2) budjetti 2020 eteenpäin, jotta lämpötilanousu pysyy alle ",rv$budinfo ),
                    size=si(1.7), color=net, hjust=0, fontface="bold") +
          
          
          geom_text(data=da,
                    aes(x=rv$fyear, y= 127),
                    label = paste0( format(round(rv$sumnet,1), nsmall=1), " Gt käytetty vuodesta 2020 vuoteen ", rv$fyear
                    ),
                    size=si(1.7), color=net, hjust=1, fontface="bold") +
          
          
          geom_text(data=datsc[, .SD[which.max(tyy)]],
                    aes(x=year+2, y=max(tyy)+9, label=paste0("Vuoden ",year, " arvot:")),
                    color=teksvari, hjust=0, size=si(2.2), fontface="bold",  family = fam) +
          
          geom_text(data=da,
                    aes(x=2020.5, y=.9*mi), label = paste0("Havainnoitu\n <=2021", rv$lang),
                    col= obsvari, fontface="bold" ,  size =si(1.9), hjust =1, vjust=0.5, angle=c(0),
                    alpha=.5, lineheight=lihh) +
          
          geom_text(data=da,
                    aes(x=2022.5, y=.9*mi), label = paste0("Simuloitu\n2022=>"),
                    col= obsvari, fontface="bold" ,  size =si(1.9), hjust =0, vjust=0.5,
                    angle=c(0), alpha=.5, lineheight=lihh) 
        
        # geom_text(data=da,
        #           aes(x=mil-5, y=.9*mi), label = paste0("www.globalcarbonprice.com \nData: UN, IPCC, Friedlingstein et al. 2022"),
        #           col=teksvari, fontface="bold" ,  size =si(1.2), hjust =0, vjust=0.5, angle=c(0), alpha=.5)
        
        
        if (rv$yearc >= rv$fyear) {
          plot1 = plot1 + 
            geom_text(data=da,
                      aes(x=rv$yearc, y= 123),
                      label = paste0(
                        format(round(rv$rvtotal,1), nsmall=1), " Gt käytetty vuodesta ", rv$fyear, " ja vuoteen " , rv$yearc
                      ),
                      size=si(1.7), color=net, hjust=1, fontface="bold") 
        }   
      }
      
      
      if (input$startvalue ==TRUE & rv$yearc > rv$fyear) {
        plot1 = plot1 +
          
          geom_label_repel(data=datsf,
                           aes(x=year-2, y=tyy, label=paste0(labuf),
                               color=col, group =sec, alpha=ala),
                           size=si(2.2), fontface="bold", hjust=1,
                           family = fam,
                           fill=bgc,
                           # segment.size =NA,
                           segment.size =NA,
                           direction = "y",
                           label.padding =0,
                           # box.padding=.1,
                           # nudge_x=18,
                           xlim=c(mil,2177),
                           label.size=0,
                           max.iter=5000,
                           force=.01, force_pull=10,box.padding=.1 ,
                           seed=5
          )
        
        if (rv$lang =="eng") {
          plot1 = plot1 +
            
            geom_text(data=da,
                      aes(x=mil, y=103), label = paste0("Maximums"),
                      col=teksvari, fontface="bold" ,  size =si(2.2), hjust =0, vjust=0.5, angle=c(0)) +
            
            geom_text(data=datsf[, .SD[which.max(tyy)]], aes(x=year-3, y=max(tyy)+9, label=paste0("Year ",year, " values:")), 
                      color=teksvari, hjust=1, size=si(2.2), fontface="bold",  family = fam) 
          
          
        }  else if (rv$lang=="fin") {
          plot1 = plot1 +
            
            geom_text(data=da,
                      aes(x=mil, y=103), label = paste0("Maksimit"),
                      col=teksvari, fontface="bold" ,  size =si(2.2), hjust =0, vjust=0.5, angle=c(0)) +
            
            geom_text(data=datsf[, .SD[which.max(tyy)]], aes(x=year-3, y=max(tyy)+9, label=paste0("Vuoden ",year, " arvot:")), 
                      color=teksvari, hjust=1, size=si(2.2), fontface="bold",  family = fam) 
        }
        
        
        
        if (input$showfossil==TRUE || input$showland==TRUE  || input$shownet==TRUE) {
          
          plot1 = plot1+
            geom_text(data=datsss[sec %in% c("fossil", "land", "net", "ghg", "nonco2", "newsink", "sink", "source"), .SD[which.max(yy)]],
                      aes(x=mil, y=103-6*prio, label=paste0(round(max(yy, na.rm=TRUE), 1), "Gt"), color=col), 
                      size=si(2.2), hjust=0, fontface="bold") 
          
        }    
        if (input$showpop==TRUE) {
          
          plot1 = plot1+
            geom_text(data=datsss[sec %in% c("pop"),.SD[which.max(yy)] ],
                      aes(x=mil, y=103-6*prio, label=paste0(round(max(yy, na.rm=TRUE), 2), "B"), color=col), 
                      size=si(2.2), hjust=0, fontface="bold") 
          
        }    
        if (input$showavgfossil==TRUE || input$showuserfossil==TRUE) {
          
          plot1 = plot1+
            geom_text(data=datsss[sec %in% c("userfossil", "avgfossil"), .SD[which.max(yy)]],
                      aes(x=mil, y=103-6*prio, label=paste0(format(round(max(yy, na.rm=TRUE), 2), nsmall=2), "t"), color=col),
                      size=si(2.2), hjust=0, fontface="bold") 
        }
        
        if (input$showprice==TRUE) {
          
          plot1 = plot1+
            geom_text(data=datsss[sec %in% c("price"), .SD[which.max(yy)]],
                      aes(x=mil, y=103-6*prio, label=paste0(round(max(yy, na.rm=TRUE), 0), "$/t"), color=col),
                      size=si(2.2), hjust=0, fontface="bold") 
          
        }
        
        if (input$showusercost==TRUE || input$shownetcost==TRUE  || input$showavgcost==TRUE || input$showdividend==TRUE ||  input$showavgnetcost==TRUE ||  input$showlandcost==TRUE) {
          
          plot1 = plot1+
            geom_text(data=datsss[sec %in% c("avgcost", "netcost", "usercost", "dividend", "avgnetcost", "landcost"), .SD[which.max(yy)]],
                      aes(x=mil, y=103-6*prio, label=paste0(round(max(yy, na.rm=TRUE), 0), "$"), color=col), 
                      size=si(2.2), hjust=0, fontface="bold") 
        }    
        
        
        
      } else {
        plot1 = plot1+
          geom_text(data=da,
                    aes(x=mil, y=108), label = paste0("Max"),
                    col=teksvari, fontface="bold" ,  size =si(2.3), hjust =0, vjust=0.5, angle=c(0)) 
        
        
        if (input$showfossil==TRUE || input$showland==TRUE  || input$shownet==TRUE) {
          
          plot1 = plot1+
            geom_text(data=datsss[sec %in% c("fossil", "land", "net", "ghg", "nonco2"), .SD[which.max(yy)]],
                      aes(x=(mil-5+10*prio), y=103, label=paste0(round(max(yy, na.rm=TRUE), 1), "Gt"), color=col), 
                      size=si(2.2), hjust=0.5, fontface="bold") 
        }    
        if (input$showpop==TRUE) {
          
          plot1 = plot1+
            geom_text(data=datsss[sec %in% c("pop"),.SD[which.max(yy)] ],
                      aes(x=(mil-5+10*prio), y=103, label=paste0(round(max(yy, na.rm=TRUE), 2), "B"), color=col), 
                      size=si(2.2), hjust=0.5, fontface="bold") 
          
        }    
        if (input$showavgfossil==TRUE || input$showuserfossil==TRUE) {
          
          plot1 = plot1+
            geom_text(data=datsss[sec %in% c("userfossil", "avgfossil"), .SD[which.max(yy)]],
                      aes(x=(mil-5+10*prio), y=103, label=paste0(format(round(max(yy, na.rm=TRUE), 2), nsmall=2), "t"), color=col),
                      size=si(2.2), hjust=0.5, fontface="bold") 
        }
        
        if (input$showprice==TRUE) {
          
          plot1 = plot1+
            geom_text(data=datsss[sec %in% c("price"), .SD[which.max(yy)]],
                      aes(x=(mil-5+10*prio), y=103, label=paste0(round(max(yy, na.rm=TRUE), 0), "$/t"), color=col),
                      size=si(2.2), hjust=0.5, fontface="bold") 
          
        }
        
        if (input$showusercost==TRUE || input$shownetcost==TRUE  || input$showavgcost==TRUE || input$showdividend==TRUE ||  input$showavgnetcost==TRUE) {
          
          plot1 = plot1+
            geom_text(data=datsss[sec %in% c("avgcost", "netcost", "usercost", "dividend", "avgnetcost"), .SD[which.max(yy)]],
                      aes(x=(mil-5+10*prio), y=103, label=paste0(round(max(yy, na.rm=TRUE), 0), "$"), color=col), 
                      size=si(2.2), hjust=0.5, fontface="bold") 
        }
      }
      
      plot1
    }) 
  }) 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  sec2 = reactive({ 
    # rv$pll = length(unique(datsl()$labbi))
    # rv$plll = length(unique(datsl()$labbi))
    
    # if (input$autodraw==TRUE) {
    #   rv$plll = length(unique(datsl()$labbi))
    # } else
    # {
    #   eventReactive(input$go==TRUE, {
    # 
    #     rv$plll = length(unique(datsl()$labbi))
    # 
    #   })
    # 
    # }
    
    if (input$autodraw==TRUE) {
      rv$pll = length(unique(datsl()$labbi))
    } else
    {
      eventReactive(input$go==TRUE, {
        
        rv$pll = length(unique(datsl()$labbi))
        rv$plll = length(unique(datsl()$labbi))
        # rv$plll = rv$pll
        
      })
      
    }
    
    # rv$pll = length(unique(datsss()$labbi))
    
    withProgress( message="Drawing graph, please wait",{
      # req(rv$pll, cancelOutput = TRUE)
      
      if (rv$pll > 1)  {
        si =
          function(per) {
            per*session$clientData$output_plotj_width*session$clientData$pixelratio/1.1/
              (870+.05*session$clientData$output_plotj_width)
            # per*session$clientData$output_plotj_width*session$clientData$pixelratio/.8/
            #   (570+.15*session$clientData$output_plotj_width)
          }
        # }
        if (input$isMobile=="FALSE") {
          
          lsi =
            function(per) {
              per*session$clientData$output_plotj_width*session$clientData$pixelratio/800
            }
        }
        
        if (input$isMobile=="TRUE") {
          
          lsi =
            function(per) {
              per*session$clientData$output_plotj_width*session$clientData$pixelratio/2000
            }
        }
        
        
      } else {
        
        si = 
          function(per) {
            per*session$clientData$output_plotj_width*session$clientData$pixelratio/.8/
              (570+.15*session$clientData$output_plotj_width)
          }
        # }
        if (input$isMobile=="FALSE") {
          
          lsi = 
            function(per) {
              per*session$clientData$output_plotj_width*session$clientData$pixelratio/500
            }
        }
        
        if (input$isMobile=="TRUE") {
          
          lsi = 
            function(per) {
              per*session$clientData$output_plotj_width*session$clientData$pixelratio/1500
            }
        }
        
      }
      
      rv$wii = si(3)
      
      
      mil = rv$ffyear
      
      
      if (rv$pll < 2) {
        plotma = margin(-5*input$dim[1]/1000,-15*input$dim[1]/1000,-12*input$dim[1]/1000,-15*input$dim[1]/1000, unit = "pt")
      } else {
        plotma = margin(-2*input$dim[1]/1000,-5*input$dim[1]/1000,-1*input$dim[1]/1000,-7*input$dim[1]/1000, unit = "pt")
        
      }
      ma = 141
      
      mix = rv$ffyear
      max = 2143
      bgc = rv$bgc
      teksvari = rv$teksvari
      obsvari = rv$obsvari
      
      
      if (rv$plot2 == "plot2")
      {
        
        
        inplot= c("fossil", "land", "net", "dummy", "ghg","nonco2", "source", "sink", "newsink")
        datsl = datsl()[sec %in% inplot & year >= mil,]
        datsss = datsss()[sec %in% inplot & year >= mil,]
        datsc =datsc()[sec %in% inplot,]
        datsf =datsf()[sec %in% inplot,]
        
        
        
        # if (input$is_mobile_device=="TRUE") {
        #
        #   si =
        #     function(per) {
        #       per*session$clientData$output_plot_width*session$clientData$pixelratio/2000
        #     }
        # }
        
        mi = min(min((datsss[,tyy]), na.rm=T)*1.18,-18)
        
        
        # hi = ma - mi
        hi = ma-mi
        
        
        
        
        
        ly = 1991
        ala = .3
        dis = 16.8
        seg = .3
        scas = 2.5
        lines = .7
        lines2 =.2
        points=1.05
        segalfa=.7
        lee = 0
        fam = fam
        
        
        
        
        
        
        plot2=   ggplot(datsf)+
          
          
          # graph limits
          geom_segment(data=da,
                       aes(x=mix, xend=2100, y=0, yend=0), 
                       color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=0) +
          
          geom_point(data=da,
                     aes(x=2140,  y=100),
                     color=teksvari, alpha=0, size=si(2))+
          
          
          
          geom_text(data = datsss[, .SD[which.max(yy)]], 
                    aes(x=mix+.05, y=136, color=col, label = labbi),
                    hjust= 0, vjust=0, size = si(4), fontface="bold") + 
          
          
          
          # geom_text(data=da,
          #           aes(x=mix+(max-mix)*.95, y=136), label = paste0("www.globalcarbonprice.com \nData: UN, IPCC, Friedlingstein et al. 2022"),
          #           col=teksvari, fontface="bold", lineheight=1 ,  
          #           size =si(1.6), hjust =1, vjust=0, angle=c(0), alpha=.6) +       
          
          #bottom years
          geom_text(data = datsss[year %in% c(seq(mil, 2100, 10)) & sec =="dummy",], 
                    aes(x=year, y=-3, label=c(year)),
                    color=teksvari, angle=0,size =si(2), alpha=.50, vjust=1) +
          
          geom_segment(data = datsss[year %in% seq(mil, 2100, 10) & sec =="dummy",],
                       (aes(x=year, xend = year, y=100, yend=.72*mi)), 
                       color=teksvari, linetype="dashed", linewidth=lsi(.4), alpha=.08) +
          
          
          
          # vertical lines
          
          
          
          # horizontal lines
          
          geom_segment(data=datsc[sec=="dummy",],
                       aes(x=mil, xend=rv$lyear, y=0, yend=0), 
                       color=teksvari, linetype ="dashed",linewidth=lsi(seg)*1.5, alpha=.3) +
          
          
          
          geom_segment(data=datsc[sec=="dummy",],
                       aes(x=mil, xend=rv$lyear, y=100, yend=100),
                       color=teksvari, linetype ="dashed",linewidth=lsi(seg)*1.5, alpha=.35) +
          
          geom_segment(data=datsc[sec=="dummy",],
                       aes(x=mil, xend=rv$lyear, y=75, yend=75), 
                       color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=.2) +
          geom_segment(data=datsc[sec=="dummy",],
                       aes(x=mil, xend=rv$lyear, y=50, yend=50), 
                       color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=.2) +
          geom_segment(data=datsc[sec=="dummy",],
                       aes(x=mil, xend=rv$lyear, y=25, yend=25), 
                       color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=.2) +
          
          geom_point(data=da, aes(x=2030, y=124), alpha=0) +
          
          # blue line for yearc      
          geom_segment(data=da,
                       aes(x=rv$yearc, xend=rv$yearc, y=123, yend = mi), 
                       color=blu, alpha=.4, linewidth=lsi(1.4))+
          
          geom_point(data=datsc, 
                     aes(y=tyy, x=year, group=sec,  alpha=ala), 
                     size=lsi(points*2), color=blu) + 
          
          # graphs for fyear to yearc
          # geom_area(data=datsl, 
          #           aes(y=tyy, x=year, group=sec,  fill=col),
          #           size=si(points), alpha=.35/nrow(datsc), position = 'identity') + 
          
          geom_area(data=datsl[year < rv$fyear+1,], 
                    aes(y=tyy, x=year, group=sec,  fill=col),
                    size=si(points), alpha=.15/nrow(datsc), position = 'identity') + 
          geom_area(data=datsl[year > rv$fyear-1,], 
                    aes(y=tyy, x=year, group=sec,  fill=col),
                    size=si(points), alpha=.35/nrow(datsc), position = 'identity') + 
          
          geom_line(data=datsl,
                    aes(y=tyy, x=year, group=sec, color=col, alpha=ala), 
                    linewidth=lsi(lines)) + 
          geom_point(data=datsl, 
                     aes(y=tyy, x=year, group=sec, color=col,  alpha=ala),
                     size=lsi(points)) + 
          
          
          # graphs for first year to lyear 
          geom_line(data=datsss, 
                    aes(y=tyy, x=year, group=interaction(sec, country), color=col), 
                    linewidth=lsi(lines), alpha=.1) + 
          geom_point(data=datsss, 
                     aes(y=tyy, x=year, group=sec, color=col, alpha=ala),
                     size=lsi(points), alpha=.1) + 
          
          
          # pricing and neutrality vertical lines
          geom_segment(data=da,
                       aes(x=rv$fyear, xend=rv$fyear, y=110, yend = mi),
                       color=teksvari, linewidth=lsi(.4), linetype = "dashed", alpha=.4 ) +
          geom_segment(data=da, 
                       aes(x=rv$lyear, xend=rv$lyear, y=110, yend = mi),
                       color=teksvari, linewidth=lsi(.4), linetype = "dashed", alpha=.4 ) +  
          
          # observed and simulated vertical line
          geom_segment(data=da,
                       aes(x=2021.5, y=100, xend=2021.5, yend=mi), 
                       color= obsvari,linewidth=lsi(.6), linetype="dashed", alpha=.2) +
          
          # yearc labels
          geom_label_repel(data=datsc,
                           aes(x=year+3, y=tyy,
                               label=labu,
                               color=col, alpha=ala*100),
                           fill=bgc,
                           hjust=0, 
                           size=si(2.4),
                           # size=si(2.4), 
                           
                           fontface="bold",
                           family = fam,
                           segment.size =NA,
                           direction = "y",
                           label.padding =0,
                           xlim=c(mil,2177),
                           label.size=0,
                           max.iter=5000,
                           force=.01, force_pull=10,box.padding=.1 ,
                           seed=5) +
          
          
          
          
          
          
          scale_color_identity() + 
          scale_alpha_identity() + 
          
          scale_fill_identity() + 
          
          scale_x_continuous(breaks = seq(mil, 2100, 10)) +
          coord_cartesian( ylim=c(mi, ma), xlim = c(mix, max),clip ="off") +
          
          
          theme(
            axis.title.x=element_blank(),
            plot.title=element_text(lineheight = 0.8, hjust=0.5, size=33, family="Alegreya Sans Bold"),
            plot.subtitle=element_text(lineheight = 0.8, size=20, hjust=0.5),
            axis.text.x=element_blank(),
            # plot.margin = margin(-5,-4,-5,-4),
            
            # plot.margin = margin(-2,-4,-2,-4),
            plot.margin = plotma,
            panel.border = element_blank(), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), 
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            plot.background = element_rect(fill =bgc,  color=NA), 
            panel.background = element_rect(fill = bgc ,  color="grey")
          )
        
        
        if (rv$pll > 1) {
          plot2 = plot2 + 
            geom_text(data=da,
                      aes(x=mil, y=103), label = paste0("Max"),
                      col=teksvari, fontface="bold" ,  size =si(2.0), hjust =0, vjust=0.5, angle=c(0)) +
            geom_text(data=datsss[, .SD[which.max(yy)]],
                      aes(x=mil, y=97, label=paste0(round(max(yy, na.rm=TRUE), le)), color=col), 
                      size=si(2.0), hjust=0, fontface="bold") 
        }
        
        
        if (rv$lang =="eng") {
          
          
          
          if (rv$yearc > rv$fyear) {
            plot2 = plot2 +
              geom_text(data=da,
                        aes(x=rv$yearc, y= 123),  label = paste0(
                          format(round(rv$rvtotal,1), nsmall=1), " Gt used ", rv$fyear, "-" , rv$yearc
                        )
                        ,
                        size=si(1.7), color=net, hjust=1, fontface="bold") 
            
          }
          plot2 = plot2 +
            
            # headline 
            
            
            
            geom_text(data=da,
                      aes(x=2005, y= 131),
                      label = paste0(rv$budget, " Gt: Net CO2 emission budget from 2020 onwards to keep temperature increase below ",rv$budinfo ),
                      size=si(1.7), color=net, hjust=0, fontface="bold") +
            
            
            
            geom_text(data=da,
                      aes(x=rv$fyear, y= 127),
                      label = paste0( format(round(rv$sumnet,1), nsmall=1), " Gt used 2020-", rv$fyear-1
                      ),
                      size=si(1.7), color=net, hjust=0, fontface="bold") +
            
            
            geom_text(data=da,
                      aes(x=rv$fyear, y=116), label =paste0("Pricing starts: ", rv$fyear),
                      color=teksvari, hjust=.5, size=si(2.2), fontface="bold") +
            
            geom_text(data=da,
                      aes(x=rv$lyear, y=116), label =paste0("Neutrality: ", rv$lyear),
                      color=teksvari, hjust=.5, size=si(2.2), fontface="bold") +
            
            
            geom_text(data=datsc[, .SD[which.max(tyy)]],
                      aes(x=year+2, y=max(tyy)+9, label=paste0("Year ",year, " values:")),
                      color=teksvari, hjust=0, size=si(2.2), fontface="bold",  family = fam) +
            
            
            geom_text(data=da,
                      aes(x=2020.5, y=max(-8,mi*.6)), label = paste0("Historical\n <=2021"),
                      col= obsvari, fontface="bold" ,  size =si(1.9), hjust =1, vjust=1, angle=c(0),
                      alpha=.5, lineheight=lihh) +
            
            geom_text(data=da,
                      aes(x=2022.5, y=max(-8,mi*.6)), label = paste0("Simulated\n2022=>"),
                      col= obsvari, fontface="bold" ,  size =si(1.9), hjust =0, vjust=1,
                      angle=c(0), alpha=.5, lineheight=lihh)  
          
          
          
        }
        
        if (rv$lang == "fin") {
          
          if (rv$yearc > rv$fyear) {
            plot2 = plot2 +
              geom_text(data=da,
                        aes(x=rv$yearc, y= 123),
                        label = paste0(
                          format(round(rv$rvtotal,1), nsmall=1), " Gt käytetty ", rv$fyear, "-" , rv$yearc
                        ),
                        size=si(1.7), color=net, hjust=1, fontface="bold")
            
          }
          
          plot2 = plot2 +
            
            
            geom_text(data=da,
                      aes(x=2005, y= 131),
                      label = paste0(rv$budget, " Gt: Nettopäästöjen budjetti 2020 lähtien, jotta lämpötilanousu pysyy alle ",rv$budinfo ),
                      size=si(1.7), color=net, hjust=0, fontface="bold") +
            
            
            geom_text(data=da,
                      aes(x=rv$fyear, y= 127),
                      label = paste0( format(round(rv$sumnet,1), nsmall=1), " Gt käytetty 2020-", rv$fyear
                      ),
                      size=si(1.7), color=net, hjust=0, fontface="bold") +
            
            
            
            
            geom_text(data=da,
                      aes(x=rv$fyear, y=116), label =paste0("Hinnoittelu alkaa: ", rv$fyear),
                      color=teksvari, hjust=.5, size=si(2.2), fontface="bold") +
            
            geom_text(data=da,
                      aes(x=rv$lyear, y=116), label =paste0("Neutraalius: ", rv$lyear),
                      color=teksvari, hjust=.5, size=si(2.2), fontface="bold") +
            
            
            
            
            geom_text(data=datsc[, .SD[which.max(tyy)]],
                      aes(x=year+2, y=max(tyy)+9, label=paste0("Vuoden ",year, " arvot:")),
                      color=teksvari, hjust=0, size=si(2.2), fontface="bold",  family = fam) +
            
            geom_text(data=da,
                      aes(x=2020.5, y=max(-8,mi*.6)), label = paste0("Havainnoitu\n <=2021"),
                      col= obsvari, fontface="bold" ,  size =si(1.9), hjust =1, vjust=1, angle=c(0),
                      alpha=.5, lineheight=.99) +
            
            geom_text(data=da,
                      aes(x=2022.5, y=max(-8,mi*.6)), label = paste0("Simuloitu\n2022=>"),
                      col= obsvari, fontface="bold" ,  size =si(1.9), hjust =0, vjust=1,
                      angle=c(0), alpha=.5, lineheight=.99) 
          
          
        }
        
        
        if (input$startvalue ==TRUE & rv$yearc > rv$fyear) {
          
          # if (rv$pll < 2) {
          
          plot2 = plot2 +
            
            
            
            geom_label_repel(data=datsf,
                             aes(x=year-2, y=tyy, label=paste0(labuf),
                                 color=col, group =sec, alpha=ala),
                             size=si(2.2), fontface="bold", hjust=1,
                             family = fam,
                             fill=bgc,
                             # segment.size =NA,
                             segment.size =NA,
                             direction = "y",
                             label.padding =0,
                             # box.padding=.1,
                             # nudge_x=18,
                             xlim=c(mil,2177),
                             label.size=0,
                             max.iter=5000,
                             force=.01, force_pull=10,box.padding=.1 ,
                             seed=5
            )
          # }
          
          if (rv$lang =="eng") {
            plot2 = plot2 +
              
              
              
              geom_text(data=datsf[, .SD[which.max(tyy)]], aes(x=year-2, y=max(tyy)+9, label=paste0(year, ":")), 
                        color=teksvari, hjust=1, size=si(2.2), fontface="bold",  family = fam) 
            
            
          }  else if (rv$lang=="fin") {
            plot2 = plot2 +
              
              
              
              geom_text(data=datsf[, .SD[which.max(tyy)]], aes(x=year-2, y=max(tyy)+9, label=paste0(year, ":")), 
                        color=teksvari, hjust=1, size=si(2.2), fontface="bold",  family = fam) 
          }
          
          
        } 
        
        
        
        gplot2= ggplotGrob(plot2)
        li2 = list(gplot2)
        # li2 = list(plot2)
        # plotlist = append(plotlist, li2)
        plotlist = c(li2)
        
        # plot1
        
      } 
      
      
      
      
      
      if (rv$plot3 == "plot3")
      {
        
        
        inplot= c("pop", "dummy", "countrypop")
        datsl = datsl()[sec %in% inplot & year >= mil,]
        datsss = datsss()[sec %in% inplot & year >= mil,]
        datsc =datsc()[sec %in% inplot,]
        datsf =datsf()[sec %in% inplot,]
        
        
        
        mi = min(min((datsss[,tyy]), na.rm=T)*1.15,-15)
        # hi = ma - mi
        hi = ma-mi
        
        
        
        # mix = rv$ffyear
        # max = 2130
        
        
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
        fam = fam
        
        
        
        plot3=   ggplot(datsf)+
          
          
          # graph limits
          geom_segment(data=da,
                       aes(x=mix, xend=2100, y=0, yend=0), 
                       color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=0) +
          
          geom_point(data=da,
                     aes(x=2140,  y=100),
                     color=teksvari, alpha=0, size=si(2))+
          #bottom years
          geom_text(data = datsss[, .SD[which.max(yy)]], 
                    aes(x=mix+.05, y=136, color=col, label = labbi),
                    hjust= 0, vjust=0, size = si(4), fontface="bold") + 
          
          # geom_text(data=da,
          #           aes(x=mix+(max-mix)*.95, y=136), label = paste0("www.globalcarbonprice.com \nData: UN, IPCC, Friedlingstein et al. 2022"),
          #           col=teksvari, fontface="bold", lineheight=1 ,  
          #           size =si(1.6), hjust =1, vjust=0, angle=c(0), alpha=.6) +       
          
          #bottom years
          geom_text(data = datsss[year %in% c(seq(mil, 2100, 10)) & sec =="dummy",], 
                    aes(x=year, y=-3, label=c(year)),
                    color=teksvari, angle=0,size =si(2), alpha=.50, vjust=1) +
          
          geom_segment(data = datsss[year %in% seq(mil, 2100, 10) & sec =="dummy",],
                       (aes(x=year, xend = year, y=100, yend=.72*mi)), 
                       color=teksvari, linetype="dashed", linewidth=lsi(.4), alpha=.08) +
          
          
          # horizontal lines
          
          geom_segment(data=datsc[sec=="dummy",],
                       aes(x=mil, xend=rv$lyear, y=0, yend=0), 
                       color=teksvari, linetype ="dashed",linewidth=lsi(seg)*1.5, alpha=.3) +
          
          
          
          geom_segment(data=datsc[sec=="dummy",],
                       aes(x=mil, xend=rv$lyear, y=100, yend=100),
                       color=teksvari, linetype ="dashed",linewidth=lsi(seg)*1.5, alpha=.3) +
          
          geom_segment(data=datsc[sec=="dummy",],
                       aes(x=mil, xend=rv$lyear, y=75, yend=75), 
                       color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=.2) +
          geom_segment(data=datsc[sec=="dummy",],
                       aes(x=mil, xend=rv$lyear, y=50, yend=50), 
                       color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=.2) +
          geom_segment(data=datsc[sec=="dummy",],
                       aes(x=mil, xend=rv$lyear, y=25, yend=25), 
                       color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=.2) +
          
          geom_point(data=da, aes(x=2030, y=124), alpha=0) +
          
          # blue line for yearc      
          geom_segment(data=da,
                       aes(x=rv$yearc, xend=rv$yearc, y=123, yend = mi), 
                       color=blu, alpha=.4, linewidth=lsi(1.4))+
          
          
          
          geom_point(data=datsc, 
                     aes(y=tyy, x=year, group=interaction(sec, country),  alpha=ala), 
                     size=lsi(points*2), color=blu) + 
          
          # graphs for fyear to yearc
          geom_area(data=datsl[year < rv$fyear+1,], 
                    aes(y=tyy, x=year, group=interaction(sec, country),  fill=col),
                    size=si(points), alpha=.15/nrow(datsc), position = 'identity') + 
          geom_area(data=datsl[year > rv$fyear-1,], 
                    aes(y=tyy, x=year, group=interaction(sec, country),  fill=col),
                    size=si(points), alpha=.35/nrow(datsc), position = 'identity') + 
          
          geom_line(data=datsl,
                    aes(y=tyy, x=year, group=interaction(sec, country), color=col, alpha=ala), 
                    linewidth=lsi(lines)) + 
          geom_point(data=datsl, 
                     aes(y=tyy, x=year, group=interaction(sec, country), color=col,  alpha=ala),
                     size=lsi(points)) + 
          
          
          # graphs for first year to lyear 
          geom_line(data=datsss, 
                    aes(y=tyy, x=year, group=interaction(sec, country), color=col), 
                    linewidth=lsi(lines), alpha=.1) + 
          geom_point(data=datsss, 
                     aes(y=tyy, x=year, group=interaction(sec, country), color=col, alpha=ala),
                     size=lsi(points), alpha=.1) + 
          
          
          
          
          
          
          # pricing and neutrality vertical lines
          geom_segment(data=da,
                       aes(x=rv$fyear, xend=rv$fyear, y=110, yend = mi),
                       color=teksvari, linewidth=lsi(.4), linetype = "dashed", alpha=.4 ) +
          geom_segment(data=da, 
                       aes(x=rv$lyear, xend=rv$lyear, y=110, yend = mi),
                       color=teksvari, linewidth=lsi(.4), linetype = "dashed", alpha=.4 ) +  
          
          # observed and simulated vertical line
          geom_segment(data=da,
                       aes(x=2021.5, y=100, xend=2021.5, yend=mi), 
                       color= obsvari,linewidth=lsi(.6), linetype="dashed", alpha=.2) +
          
          # yearc labels
          
          
          # geom_text(data=da,
          #           aes(x=mil, y=103), label = paste0("Max"),
          #           col=teksvari, fontface="bold" ,  size =si(2.1), hjust =0, vjust=0.5, angle=c(0)) +
          # geom_text(data=datsss[, .SD[which.max(yy)]],
          #           aes(x=mil, y=103-6*1, label=paste0(round(max(yy, na.rm=TRUE), le)), color=col), 
          #           size=si(2.1), hjust=0, fontface="bold") +
          
        geom_label_repel(data=datsc,
                         aes(x=year+3, y=tyy,
                             label=labu,
                             color=col, alpha=ala*100),
                         fill=bgc,
                         hjust=0, size=si(2.4), fontface="bold",
                         family = fam,
                         segment.size =NA,
                         direction = "y",
                         label.padding =0,
                         xlim=c(mil,2177),
                         label.size=0,
                         max.iter=5000,
                         force=.01, force_pull=10,box.padding=.1 ,
                         seed=5) +
          
          
          scale_color_identity() + 
          scale_alpha_identity() + 
          
          scale_fill_identity() + 
          
          scale_x_continuous(breaks = seq(mil, 2100, 10)) +
          coord_cartesian( ylim=c(mi, ma), xlim = c(mix, max),clip ="off") +
          
          
          theme(
            axis.title.x=element_blank(),
            plot.title=element_text(lineheight = 0.8, hjust=0.5, size=33, family="Alegreya Sans Bold"),
            plot.subtitle=element_text(lineheight = 0.8, size=20, hjust=0.5),
            axis.text.x=element_blank(),
            plot.margin = plotma,
            
            # plot.margin = margin(-2,0,-2,0),
            panel.border = element_blank(), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), 
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            plot.background = element_rect(fill =bgc, color=NA), 
            panel.background = element_rect(fill = bgc ,  color="grey")
          )
        
        if (rv$pll > 1) {
          plot3 = plot3 + 
            geom_text(data=da,
                      aes(x=mil, y=103), label = paste0("Max"),
                      col=teksvari, fontface="bold" ,  size =si(2.0), hjust =0, vjust=0.5, angle=c(0)) +
            geom_text(data=datsss[, .SD[which.max(yy)]],
                      aes(x=mil, y=97, label=paste0(round(max(yy, na.rm=TRUE), le)), color=col), 
                      size=si(2.0), hjust=0, fontface="bold") 
        }
        
        if (rv$lang =="eng") {
          
          plot3 = plot3 +
            
            
            geom_text(data=da,
                      aes(x=rv$fyear, y=prinet), label =paste0("Pricing starts: ", rv$fyear),
                      color=teksvari, hjust=.5, size=si(2.2), fontface="bold") +
            
            geom_text(data=da,
                      aes(x=rv$lyear, y=prinet), label =paste0("Neutrality: ", rv$lyear),
                      color=teksvari, hjust=.5, size=si(2.2), fontface="bold") +
            
            
            geom_text(data=datsc[, .SD[which.max(tyy)]],
                      aes(x=year+2, y=max(tyy)+9, label=paste0("Year ",year, " values:")),
                      color=teksvari, hjust=0, size=si(2.2), fontface="bold",  family = fam) +
            
            
            geom_text(data=da,
                      aes(x=2020.5, y=max(-8,mi*.6)), label = paste0("Historical\n <=2021"),
                      col= obsvari, fontface="bold" ,  size =si(1.9), hjust =1, vjust=1, angle=c(0),
                      alpha=.5, lineheight=.99) +
            
            geom_text(data=da,
                      aes(x=2022.5, y=max(-8,mi*.6)), label = paste0("Simulated\n2022=>"),
                      col= obsvari, fontface="bold" ,  size =si(1.9), hjust =0, vjust=1,
                      angle=c(0), alpha=.5, lineheight=.99)  
        }
        
        if (rv$lang == "fin") {
          
          plot3 = plot3 +
            
            
            geom_text(data=da,
                      aes(x=rv$fyear, y=prinet), label =paste0("Hinnoittelu alkaa: ", rv$fyear),
                      color=teksvari, hjust=.5, size=si(2.2), fontface="bold") +
            
            geom_text(data=da,
                      aes(x=rv$lyear, y=prinet), label =paste0("Neutraalius: ", rv$lyear),
                      color=teksvari, hjust=.5, size=si(2.2), fontface="bold") +
            
            
            
            
            geom_text(data=datsc[, .SD[which.max(tyy)]],
                      aes(x=year+2, y=max(tyy)+9, label=paste0("Vuoden ",year, " arvot:")),
                      color=teksvari, hjust=0, size=si(2.2), fontface="bold",  family = fam) +
            
            geom_text(data=da,
                      aes(x=2020.5, y=max(-8,mi*.6)), label = paste0("Havainnoitu\n <=2021"),
                      col= obsvari, fontface="bold" ,  size =si(1.9), hjust =1, vjust=1, angle=c(0),
                      alpha=.5, lineheight=.99) +
            
            geom_text(data=da,
                      aes(x=2022.5, y=max(-8,mi*.6)), label = paste0("Simuloitu\n2022=>"),
                      col= obsvari, fontface="bold" ,  size =si(1.9), hjust =0, vjust=1,
                      angle=c(0), alpha=.5, lineheight=.99) 
          
          
        }
        
        
        if (input$startvalue ==TRUE & rv$yearc > rv$fyear) {
          
          plot3 = plot3 +
            # geom_text(data=da,
            #           aes(x=mil, y=103), label = paste0("Max"),
            #           col=teksvari, fontface="bold" ,  size =si(2.2), hjust =0, vjust=0.5, angle=c(0)) +
            geom_label_repel(data=datsf,
                             aes(x=year-2, y=tyy, label=paste0(labuf),
                                 color=col, group =sec, alpha=ala),
                             size=si(2.2), fontface="bold", hjust=1,
                             family = fam,
                             fill=bgc,
                             # segment.size =NA,
                             segment.size =NA,
                             direction = "y",
                             label.padding =0,
                             # box.padding=.1,
                             # nudge_x=18,
                             xlim=c(mil,2177),
                             label.size=0,
                             max.iter=5000,
                             force=.01, force_pull=10,box.padding=.1 ,
                             seed=5
            )
          # }
          
          if (rv$lang =="eng") {
            plot3 = plot3 +
              
              
              
              geom_text(data=datsf[, .SD[which.max(tyy)]], aes(x=year-2, y=max(tyy)+9, label=paste0(year, ":")), 
                        color=teksvari, hjust=1, size=si(2.2), fontface="bold",  family = fam) 
            
            
          }  else if (rv$lang=="fin") {
            plot3 = plot3 +
              
              
              
              geom_text(data=datsf[, .SD[which.max(tyy)]], aes(x=year-2, y=max(tyy)+9, label=paste0(year, ":")), 
                        color=teksvari, hjust=1, size=si(2.2), fontface="bold",  family = fam) 
          }
          
        }
        
        
        
        
        
        gplot3= ggplotGrob(plot3)
        li3 = list(gplot3)
        
        if (exists("plotlist")) {
          plotlist = c(plotlist,li3)
          
        } else {        plotlist = c(li3)
        
        }
      }
      
      
      
      
      
      
      
      
      
      
      if (rv$plot4 == "plot4")
      {
        
        
        inplot= c("userfossil", "avgfossil", "dummy", "countryfossil")
        datsl = datsl()[sec %in% inplot & year >= mil,]
        datsss = datsss()[sec %in% inplot & year >= mil,]
        datsc =datsc()[sec %in% inplot,]
        datsf =datsf()[sec %in% inplot,]
        
        
        
        mi = min(min((datsss[,tyy]), na.rm=T)*1.15,-15)
        # hi = ma - mi
        
        hi = ma-mi
        
        
        # mix = mil+3
        # max = 2130
        
        
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
        fam = fam
        
        
        
        plot4=   ggplot(datsf)+
          
          
          # graph limits
          geom_segment(data=da,
                       aes(x=mix, xend=2100, y=0, yend=0), 
                       color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=0) +
          
          geom_point(data=da,
                     aes(x=2140,  y=100),
                     color=teksvari, alpha=0, size=si(2))+
          #bottom years
          geom_text(data = datsss[, .SD[which.max(yy)]], 
                    aes(x=mix+.05, y=136, color=col, label = labbi),
                    hjust= 0, vjust=0, size = si(4), fontface="bold") + 
          
          # geom_text(data=da,
          #           aes(x=mix+(max-mix)*.95, y=136), label = paste0("www.globalcarbonprice.com \nData: UN, IPCC, Friedlingstein et al. 2022"),
          #           col=teksvari, fontface="bold", lineheight=1.0 ,  
          #           size =si(1.6), hjust =1, vjust=0, angle=c(0), alpha=.6) +       
          
          #bottom years
          geom_text(data = datsss[year %in% c(seq(mil, 2100, 10)) & sec =="dummy",], 
                    aes(x=year, y=-3, label=c(year)),
                    color=teksvari, angle=0,size =si(2), alpha=.50, vjust=1) +
          
          
          # vertical lines
          geom_segment(data = datsss[year %in% seq(mil, 2100, 10) & sec =="dummy",],
                       (aes(x=year, xend = year, y=100, yend=.72*mi)), 
                       color=teksvari, linetype="dashed", linewidth=lsi(.4), alpha=.08) +
          
          # horizontal lines
          
          geom_segment(data=datsc[sec=="dummy",],
                       aes(x=mil, xend=rv$lyear, y=0, yend=0), 
                       color=teksvari, linetype ="dashed",linewidth=lsi(seg)*1.5, alpha=.3) +
          
          
          
          geom_segment(data=datsc[sec=="dummy",],
                       aes(x=mil, xend=rv$lyear, y=100, yend=100),
                       color=teksvari, linetype ="dashed",linewidth=lsi(seg)*1.5, alpha=.3) +
          
          geom_segment(data=datsc[sec=="dummy",],
                       aes(x=mil, xend=rv$lyear, y=75, yend=75), 
                       color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=.2) +
          geom_segment(data=datsc[sec=="dummy",],
                       aes(x=mil, xend=rv$lyear, y=50, yend=50), 
                       color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=.2) +
          geom_segment(data=datsc[sec=="dummy",],
                       aes(x=mil, xend=rv$lyear, y=25, yend=25), 
                       color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=.2) +
          
          geom_point(data=da, aes(x=2030, y=124), alpha=0) +
          
          # blue line for yearc      
          geom_segment(data=da,
                       aes(x=rv$yearc, xend=rv$yearc, y=123, yend = mi), 
                       color=blu, alpha=.4, linewidth=lsi(1.4))+
          geom_point(data=datsc, 
                     aes(y=tyy, x=year, group=interaction(sec, country),  alpha=ala), 
                     size=lsi(points*2), color=blu) + 
          
          # graphs for fyear to yearc
          geom_area(data=datsl[year < rv$fyear+1,], 
                    aes(y=tyy, x=year, group=interaction(sec, country),  fill=col),
                    size=si(points), alpha=.15/nrow(datsc), position = 'identity') + 
          geom_area(data=datsl[year > rv$fyear-1,], 
                    aes(y=tyy, x=year, group=interaction(sec, country),  fill=col),
                    size=si(points), alpha=.35/nrow(datsc), position = 'identity') + 
          
          geom_line(data=datsl,
                    aes(y=tyy, x=year, group=interaction(sec, country), color=col, alpha=ala), 
                    linewidth=lsi(lines)) + 
          geom_point(data=datsl, 
                     aes(y=tyy, x=year, group=interaction(sec, country), color=col,  alpha=ala),
                     size=lsi(points)) + 
          
          
          # graphs for first year to lyear 
          geom_line(data=datsss, 
                    aes(y=tyy, x=year, group=interaction(sec, country), color=col), 
                    linewidth=lsi(lines), alpha=.1) + 
          geom_point(data=datsss, 
                     aes(y=tyy, x=year, group=interaction(sec, country), color=col, alpha=ala),
                     size=lsi(points), alpha=.1) + 
          
          
          # pricing and neutrality vertical lines
          geom_segment(data=da,
                       aes(x=rv$fyear, xend=rv$fyear, y=110, yend = mi),
                       color=teksvari, linewidth=lsi(.4), linetype = "dashed", alpha=.4 ) +
          geom_segment(data=da, 
                       aes(x=rv$lyear, xend=rv$lyear, y=110, yend = mi),
                       color=teksvari, linewidth=lsi(.4), linetype = "dashed", alpha=.4 ) +  
          
          # observed and simulated vertical line
          geom_segment(data=da,
                       aes(x=2021.5, y=100, xend=2021.5, yend=mi), 
                       color= obsvari,linewidth=lsi(.6), linetype="dashed", alpha=.2) +
          
          # yearc labels
          
          # geom_text(data=da,
          #           aes(x=mil, y=103), label = paste0("Max"),
          #           col=teksvari, fontface="bold" ,  size =si(2.1), hjust =0, vjust=0.5, angle=c(0)) +
          # geom_text(data=datsss[, .SD[which.max(yy)]],
          #           aes(x=mil, y=103-6*1, label=paste0(round(max(yy, na.rm=TRUE), le)), color=col), 
          #           size=si(2.1), hjust=0, fontface="bold") +
          
          geom_label_repel(data=datsc,
                           aes(x=year+3, y=tyy,
                               label=labu,
                               color=col, alpha=ala*100),
                           fill=bgc,
                           hjust=0, size=si(2.4), fontface="bold",
                           family = fam,
                           segment.size =NA,
                           direction = "y",
                           label.padding =0,
                           xlim=c(mil,2177),
                           label.size=0,
                           max.iter=5000,
                           force=.01, force_pull=10,box.padding=.1 ,
                           seed=5) +
          
          
          
          scale_color_identity() + 
          scale_alpha_identity() + 
          
          scale_fill_identity() + 
          
          scale_x_continuous(breaks = seq(mil, 2100, 10)) +
          coord_cartesian( ylim=c(mi, ma), xlim = c(mix, max),clip ="off") +
          
          
          theme(
            axis.title.x=element_blank(),
            plot.title=element_text(lineheight = 0.8, hjust=0.5, size=33, family="Alegreya Sans Bold"),
            plot.subtitle=element_text(lineheight = 0.8, size=20, hjust=0.5),
            axis.text.x=element_blank(),
            plot.margin = plotma,
            
            # plot.margin = margin(-2,0,-2,0),
            panel.border = element_blank(), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), 
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            plot.background = element_rect(fill =bgc, color=NA), 
            panel.background = element_rect(fill = bgc ,  color="grey")
          )
        
        if (rv$pll > 1) {
          plot4 = plot4 + 
            geom_text(data=da,
                      aes(x=mil, y=103), label = paste0("Max"),
                      col=teksvari, fontface="bold" ,  size =si(2.0), hjust =0, vjust=0.5, angle=c(0)) +
            geom_text(data=datsss[, .SD[which.max(yy)]],
                      aes(x=mil, y=97, label=paste0(round(max(yy, na.rm=TRUE), le)), color=col), 
                      size=si(2.0), hjust=0, fontface="bold") 
        }
        
        if (rv$lang =="eng") {
          
          plot4 = plot4 +
            
            
            geom_text(data=da,
                      aes(x=rv$fyear, y=prinet), label =paste0("Pricing starts: ", rv$fyear),
                      color=teksvari, hjust=.5, size=si(2.2), fontface="bold") +
            
            geom_text(data=da,
                      aes(x=rv$lyear, y=prinet), label =paste0("Neutrality: ", rv$lyear),
                      color=teksvari, hjust=.5, size=si(2.2), fontface="bold") +
            
            
            geom_text(data=datsc[, .SD[which.max(tyy)]],
                      aes(x=year+2, y=max(tyy)+9, label=paste0("Year ",year, " values:")),
                      color=teksvari, hjust=0, size=si(2.2), fontface="bold",  family = fam) +
            
            
            geom_text(data=da,
                      aes(x=2020.5, y=max(-8,mi*.6)), label = paste0("Historical\n <=2021"),
                      col= obsvari, fontface="bold" ,  size =si(1.9), hjust =1, vjust=1, angle=c(0),
                      alpha=.5, lineheight=.99) +
            
            geom_text(data=da,
                      aes(x=2022.5, y=max(-8,mi*.6)), label = paste0("Simulated\n2022=>"),
                      col= obsvari, fontface="bold" ,  size =si(1.9), hjust =0, vjust=1,
                      angle=c(0), alpha=.5, lineheight=.99)  
        }
        
        if (rv$lang == "fin") {
          
          plot4 = plot4 +
            
            
            geom_text(data=da,
                      aes(x=rv$fyear, y=prinet), label =paste0("Hinnoittelu alkaa: ", rv$fyear),
                      color=teksvari, hjust=.5, size=si(2.2), fontface="bold") +
            
            geom_text(data=da,
                      aes(x=rv$lyear, y=prinet), label =paste0("Neutraalius: ", rv$lyear),
                      color=teksvari, hjust=.5, size=si(2.2), fontface="bold") +
            
            
            
            
            geom_text(data=datsc[, .SD[which.max(tyy)]],
                      aes(x=year+2, y=max(tyy)+9, label=paste0("Vuoden ",year, " arvot:")),
                      color=teksvari, hjust=0, size=si(2.2), fontface="bold",  family = fam) +
            
            geom_text(data=da,
                      aes(x=2020.5, y=max(-8,mi*.6)), label = paste0("Havainnoitu\n <=2021"),
                      col= obsvari, fontface="bold" ,  size =si(1.9), hjust =1, vjust=1, angle=c(0),
                      alpha=.5, lineheight=.99) +
            
            geom_text(data=da,
                      aes(x=2022.5, y=max(-8,mi*.6)), label = paste0("Simuloitu\n2022=>"),
                      col= obsvari, fontface="bold" ,  size =si(1.9), hjust =0, vjust=1,
                      angle=c(0), alpha=.5, lineheight=.99) 
          
        }
        
        
        if (input$startvalue ==TRUE & rv$yearc > rv$fyear) {
          
          plot4 = plot4 +
            geom_label_repel(data=datsf,
                             aes(x=year-2, y=tyy, label=paste0(labuf),
                                 color=col, group =sec, alpha=ala),
                             size=si(2.2), fontface="bold", hjust=1,
                             family = fam,
                             fill=bgc,
                             # segment.size =NA,
                             segment.size =NA,
                             direction = "y",
                             label.padding =0,
                             # box.padding=.1,
                             # nudge_x=18,
                             xlim=c(mil,2177),
                             label.size=0,
                             max.iter=5000,
                             force=.01, force_pull=10,box.padding=.1 ,
                             seed=5
            )
          # }
          
          if (rv$lang =="eng") {
            plot4 = plot4 +
              
              
              
              geom_text(data=datsf[, .SD[which.max(tyy)]], aes(x=year-2, y=max(tyy)+9, label=paste0(year, ":")), 
                        color=teksvari, hjust=1, size=si(2.2), fontface="bold",  family = fam) 
            
            
          }  else if (rv$lang=="fin") {
            plot4 = plot4 +
              
              
              
              geom_text(data=datsf[, .SD[which.max(tyy)]], aes(x=year-2, y=max(tyy)+9, label=paste0(year, ":")), 
                        color=teksvari, hjust=1, size=si(2.2), fontface="bold",  family = fam) 
          }
          
          
          
        }
        
        
        
        gplot4= ggplotGrob(plot4)
        li4 = list(gplot4)
        
        
        if (exists("plotlist")) {
          plotlist = c(plotlist,li4)
          
        } else {        plotlist = c(li4)
        
        }
        
        
      }
      
      
      
      
      if (rv$plot5 == "plot5")
      {
        
        
        inplot= c("price", "dummy")
        datsl = datsl()[sec %in% inplot & year >= mil,]
        datsss = datsss()[sec %in% inplot & year >= mil,]
        datsc =datsc()[sec %in% inplot,]
        datsf =datsf()[sec %in% inplot,]
        
        
        
        mi = min(min((datsss[,tyy]), na.rm=T)*1.15,-15)
        # hi = ma - mi
        hi = ma-mi
        
        
        
        # mix = mil+3
        # max = 2130
        # 
        
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
        fam = fam
        
        
        
        plot5=   ggplot(datsf)+
          
          
          # graph limits
          geom_segment(data=da,
                       aes(x=mix, xend=2100, y=0, yend=0), 
                       color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=0) +
          
          geom_point(data=da,
                     aes(x=2140,  y=100),
                     color=teksvari, alpha=0, size=si(2))+
          #bottom years
          geom_text(data = datsss[, .SD[which.max(yy)]], 
                    aes(x=mix+.05, y=136, color=col, label = labbi),
                    hjust= 0, vjust=0, size = si(4), fontface="bold") + 
          
          # geom_text(data=da,
          #           aes(x=mix+(max-mix)*.95, y=136), label = paste0("www.globalcarbonprice.com \nData: UN, IPCC, Friedlingstein et al. 2022"),
          #           col=teksvari, fontface="bold", lineheight=1.0 ,  
          #           size =si(1.6), hjust =1, vjust=0, angle=c(0), alpha=.6) +       
          
          #bottom years
          geom_text(data = datsss[year %in% c(seq(mil, 2100, 10)) & sec =="dummy",], 
                    aes(x=year, y=-3, label=c(year)),
                    color=teksvari, angle=0,size =si(2), alpha=.50, vjust=1) +
          
          
          # vertical lines
          geom_segment(data = datsss[year %in% seq(mil, 2100, 10) & sec =="dummy",],
                       (aes(x=year, xend = year, y=100, yend=.72*mi)), 
                       color=teksvari, linetype="dashed", linewidth=lsi(.4), alpha=.08) +
          
          
          # horizontal lines
          
          geom_segment(data=datsc[sec=="dummy",],
                       aes(x=mil, xend=rv$lyear, y=0, yend=0), 
                       color=teksvari, linetype ="dashed",linewidth=lsi(seg)*1.5, alpha=.3) +
          
          
          
          geom_segment(data=datsc[sec=="dummy",],
                       aes(x=mil, xend=rv$lyear, y=100, yend=100),
                       color=teksvari, linetype ="dashed",linewidth=lsi(seg)*1.5, alpha=.3) +
          
          geom_segment(data=datsc[sec=="dummy",],
                       aes(x=mil, xend=rv$lyear, y=75, yend=75), 
                       color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=.2) +
          geom_segment(data=datsc[sec=="dummy",],
                       aes(x=mil, xend=rv$lyear, y=50, yend=50), 
                       color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=.2) +
          geom_segment(data=datsc[sec=="dummy",],
                       aes(x=mil, xend=rv$lyear, y=25, yend=25), 
                       color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=.2) +
          
          geom_point(data=da, aes(x=2030, y=124), alpha=0) +
          
          # blue line for yearc      
          geom_segment(data=da,
                       aes(x=rv$yearc, xend=rv$yearc, y=123, yend = mi), 
                       color=blu, alpha=.4, linewidth=lsi(1.4))+
          
          geom_point(data=datsc, 
                     aes(y=tyy, x=year, group=sec,  alpha=ala), 
                     size=lsi(points*2), color=blu) + 
          
          # graphs for fyear to yearc
          geom_area(data=datsl[year < rv$fyear+1,], 
                    aes(y=tyy, x=year, group=sec,  fill=col),
                    size=si(points), alpha=.15/nrow(datsc), position = 'identity') + 
          geom_area(data=datsl[year > rv$fyear-1,], 
                    aes(y=tyy, x=year, group=sec,  fill=col),
                    size=si(points), alpha=.35/nrow(datsc), position = 'identity') + 
          
          geom_line(data=datsl,
                    aes(y=tyy, x=year, group=sec, color=col, alpha=ala), 
                    linewidth=lsi(lines)) + 
          geom_point(data=datsl, 
                     aes(y=tyy, x=year, group=sec, color=col,  alpha=ala),
                     size=lsi(points)) + 
          
          
          # graphs for first year to lyear 
          geom_line(data=datsss, 
                    aes(y=tyy, x=year, group=interaction(sec, country), color=col), 
                    linewidth=lsi(lines), alpha=.1) + 
          geom_point(data=datsss, 
                     aes(y=tyy, x=year, group=sec, color=col, alpha=ala),
                     size=lsi(points), alpha=.1) + 
          
          
          # pricing and neutrality vertical lines
          geom_segment(data=da,
                       aes(x=rv$fyear, xend=rv$fyear, y=110, yend = mi),
                       color=teksvari, linewidth=lsi(.4), linetype = "dashed", alpha=.4 ) +
          geom_segment(data=da, 
                       aes(x=rv$lyear, xend=rv$lyear, y=110, yend = mi),
                       color=teksvari, linewidth=lsi(.4), linetype = "dashed", alpha=.4 ) +  
          
          # observed and simulated vertical line
          geom_segment(data=da,
                       aes(x=2021.5, y=100, xend=2021.5, yend=mi), 
                       color= obsvari,linewidth=lsi(.6), linetype="dashed", alpha=.2) +
          
          # yearc labels
          # geom_text(data=da,
          #           aes(x=mil, y=103), label = paste0("Max"),
          #           col=teksvari, fontface="bold" ,  size =si(2.1), hjust =0, vjust=0.5, angle=c(0)) +
          # geom_text(data=datsss[, .SD[which.max(yy)]],
          #           aes(x=mil, y=103-6*1, label=paste0(round(max(yy, na.rm=TRUE), le)), color=col), 
          #           size=si(2.1), hjust=0, fontface="bold") +
          
          geom_label_repel(data=datsc,
                           aes(x=year+3, y=tyy,
                               label=labu,
                               color=col, alpha=ala*100),
                           fill=bgc,
                           hjust=0, size=si(2.4), fontface="bold",
                           family = fam,
                           segment.size =NA,
                           direction = "y",
                           label.padding =0,
                           xlim=c(mil,2177),
                           label.size=0,
                           max.iter=5000,
                           force=.01, force_pull=10,box.padding=.1 ,
                           seed=5) +
          
          
          
          scale_color_identity() + 
          scale_alpha_identity() + 
          
          scale_fill_identity() + 
          
          scale_x_continuous(breaks = seq(mil, 2100, 10)) +
          coord_cartesian( ylim=c(mi, ma), xlim = c(mix, max),clip ="off") +
          
          
          theme(
            axis.title.x=element_blank(),
            plot.title=element_text(lineheight = 0.8, hjust=0.5, size=33, family="Alegreya Sans Bold"),
            plot.subtitle=element_text(lineheight = 0.8, size=20, hjust=0.5),
            axis.text.x=element_blank(),
            plot.margin = plotma,
            
            # plot.margin = margin(-2,0,-2,0),
            panel.border = element_blank(), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), 
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            plot.background = element_rect(fill =bgc, color=NA), 
            panel.background = element_rect(fill = bgc ,  color="grey")
          )
        
        if (rv$pll > 1) {
          plot5 = plot5 + 
            geom_text(data=da,
                      aes(x=mil, y=103), label = paste0("Max"),
                      col=teksvari, fontface="bold" ,  size =si(2.0), hjust =0, vjust=0.5, angle=c(0)) +
            geom_text(data=datsss[, .SD[which.max(yy)]],
                      aes(x=mil, y=97, label=paste0(round(max(yy, na.rm=TRUE), le)), color=col), 
                      size=si(2.0), hjust=0, fontface="bold") 
        }
        
        if (rv$lang =="eng") {
          
          plot5 = plot5 +
            
            
            geom_text(data=da,
                      aes(x=rv$fyear, y=prinet), label =paste0("Pricing starts: ", rv$fyear),
                      color=teksvari, hjust=.5, size=si(2.2), fontface="bold") +
            
            geom_text(data=da,
                      aes(x=rv$lyear, y=prinet), label =paste0("Neutrality: ", rv$lyear),
                      color=teksvari, hjust=.5, size=si(2.2), fontface="bold") +
            
            
            geom_text(data=datsc[, .SD[which.max(tyy)]],
                      aes(x=year+2, y=max(tyy)+9, label=paste0("Year ",year, " values:")),
                      color=teksvari, hjust=0, size=si(2.2), fontface="bold",  family = fam) +
            
            
            geom_text(data=da,
                      aes(x=2020.5, y=max(-8,mi*.6)), label = paste0("Historical\n <=2021"),
                      col= obsvari, fontface="bold" ,  size =si(1.9), hjust =1, vjust=1, angle=c(0),
                      alpha=.5, lineheight=.99) +
            
            geom_text(data=da,
                      aes(x=2022.5, y=max(-8,mi*.6)), label = paste0("Simulated\n2022=>"),
                      col= obsvari, fontface="bold" ,  size =si(1.9), hjust =0, vjust=1,
                      angle=c(0), alpha=.5, lineheight=.99)  
        }
        
        if (rv$lang == "fin") {
          
          plot5 = plot5 +
            
            
            geom_text(data=da,
                      aes(x=rv$fyear, y=prinet), label =paste0("Hinnoittelu alkaa: ", rv$fyear),
                      color=teksvari, hjust=.5, size=si(2.2), fontface="bold") +
            
            geom_text(data=da,
                      aes(x=rv$lyear, y=prinet), label =paste0("Neutraalius: ", rv$lyear),
                      color=teksvari, hjust=.5, size=si(2.2), fontface="bold") +
            
            
            
            
            geom_text(data=datsc[, .SD[which.max(tyy)]],
                      aes(x=year+2, y=max(tyy)+9, label=paste0("Vuoden ",year, " arvot:")),
                      color=teksvari, hjust=0, size=si(2.2), fontface="bold",  family = fam) +
            
            geom_text(data=da,
                      aes(x=2020.5, y=max(-8,mi*.6)), label = paste0("Havainnoitu\n <=2021"),
                      col= obsvari, fontface="bold" ,  size =si(1.9), hjust =1, vjust=1, angle=c(0),
                      alpha=.5, lineheight=.99) +
            
            geom_text(data=da,
                      aes(x=2022.5, y=max(-8,mi*.6)), label = paste0("Simuloitu\n2022=>"),
                      col= obsvari, fontface="bold" ,  size =si(1.9), hjust =0, vjust=1,
                      angle=c(0), alpha=.5, lineheight=.99) 
          
          
        }
        
        
        if (input$startvalue ==TRUE & rv$yearc > rv$fyear) {
          
          plot5 = plot5 +
            
            geom_label_repel(data=datsf,
                             aes(x=year-2, y=tyy, label=paste0(labuf),
                                 color=col, group =sec, alpha=ala),
                             size=si(2.2), fontface="bold", hjust=1,
                             family = fam,
                             fill=bgc,
                             # segment.size =NA,
                             segment.size =NA,
                             direction = "y",
                             label.padding =0,
                             # box.padding=.1,
                             # nudge_x=18,
                             xlim=c(mil,2177),
                             label.size=0,
                             max.iter=5000,
                             force=.01, force_pull=10,box.padding=.1 ,
                             seed=5
            )
          # }
          
          if (rv$lang =="eng") {
            plot5 = plot5 +
              
              
              
              geom_text(data=datsf[, .SD[which.max(tyy)]], aes(x=year-2, y=max(tyy)+9, label=paste0(year, ":")), 
                        color=teksvari, hjust=1, size=si(2.2), fontface="bold",  family = fam) 
            
            
          }  else if (rv$lang=="fin") {
            plot5 = plot5 +
              
              
              
              geom_text(data=datsf[, .SD[which.max(tyy)]], aes(x=year-2, y=max(tyy)+9, label=paste0(year, ":")), 
                        color=teksvari, hjust=1, size=si(2.2), fontface="bold",  family = fam) 
          }
          
          
          
        }
        
        
        
        gplot5= ggplotGrob(plot5)
        li5 = list(gplot5)
        
        
        
        if (exists("plotlist")) {
          plotlist = c(plotlist,li5)
          
        } else {        plotlist = c(li5)
        
        }
        
        
      }
      
      
      
      
      
      if (rv$plot6 == "plot6")
      {
        
        
        inplot= c("dummy", "averagedividend", "countrydividend", "avgcost",
                  "netcost","usercost","dividend","avgnetcost","countrynetcost", "landcost",
                  "countrycost")
        datsl = datsl()[sec %in% inplot & year >= mil,]
        datsss = datsss()[sec %in% inplot & year >= mil,]
        datsc =datsc()[sec %in% inplot,]
        datsf =datsf()[sec %in% inplot,]
        
        
        
        mi = min(min((datsss[,tyy]), na.rm=T)*1.15,-15)
        hi = ma - mi
        
        
        
        # mix = mil+3
        # max = 2130
        
        
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
        fam = fam
        
        
        
        plot6=   ggplot(datsf)+
          
          # graph limits
          geom_segment(data=da,
                       aes(x=mix, xend=2100, y=0, yend=0), 
                       color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=0) +
          
          geom_point(data=da,
                     aes(x=2140,  y=100),
                     color=teksvari, alpha=0, size=si(2))+
          #bottom years
          geom_text(data = datsss[, .SD[which.max(yy)]], 
                    aes(x=mix+.05, y=136, color=col, label = labbi),
                    hjust= 0, vjust=0, size = si(4), fontface="bold") + 
          
          # geom_text(data=da,
          #           aes(x=mix+(max-mix)*.95, y=136), label = paste0("www.globalcarbonprice.com \nData: UN, IPCC, Friedlingstein et al. 2022"),
          #           col=teksvari, fontface="bold", lineheight=1.0 ,  
          #           size =si(1.6), hjust =1, vjust=0, angle=c(0), alpha=.6) +       
          
          #bottom years
          geom_text(data = datsss[year %in% c(seq(mil, 2100, 10)) & sec =="dummy",], 
                    aes(x=year, y=-3, label=c(year)),
                    color=teksvari, angle=0,size =si(2), alpha=.50, vjust=1) +
          
          # vertical lines
          geom_segment(data = datsss[year %in% seq(mil, 2100, 10) & sec =="dummy",],
                       (aes(x=year, xend = year, y=100, yend=.72*mi)), 
                       color=teksvari, linetype="dashed", linewidth=lsi(.4), alpha=.08) +
          
          # horizontal lines
          
          geom_segment(data=datsc[sec=="dummy",],
                       aes(x=mil, xend=rv$lyear, y=0, yend=0), 
                       color=teksvari, linetype ="dashed",linewidth=lsi(seg)*1.5, alpha=.3) +
          
          
          geom_segment(data=datsc[sec=="dummy",],
                       aes(x=mil, xend=rv$lyear, y=100, yend=100),
                       color=teksvari, linetype ="dashed",linewidth=lsi(seg)*1.5, alpha=.3) +
          
          geom_segment(data=datsc[sec=="dummy",],
                       aes(x=mil, xend=rv$lyear, y=75, yend=75), 
                       color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=.2) +
          geom_segment(data=datsc[sec=="dummy",],
                       aes(x=mil, xend=rv$lyear, y=50, yend=50), 
                       color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=.2) +
          geom_segment(data=datsc[sec=="dummy",],
                       aes(x=mil, xend=rv$lyear, y=25, yend=25), 
                       color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=.2) +
          
          geom_point(data=da, aes(x=2030, y=124), alpha=0) +
          
          # blue line for yearc      
          geom_segment(data=da,
                       aes(x=rv$yearc, xend=rv$yearc, y=123, yend = mi), 
                       color=blu, alpha=.4, linewidth=lsi(1.4))+
          
          
          
          
          geom_point(data=datsc, 
                     aes(y=tyy, x=year, group=interaction(sec, country),  alpha=ala), 
                     size=lsi(points*2), color=blu) + 
          
          # graphs for fyear to yearc
          geom_area(data=datsl[year < rv$fyear+1,], 
                    aes(y=tyy, x=year, group=interaction(sec, country),  fill=col),
                    size=si(points), alpha=.15/nrow(datsc), position = 'identity') + 
          geom_area(data=datsl[year > rv$fyear-1,], 
                    aes(y=tyy, x=year, group=interaction(sec, country),  fill=col),
                    size=si(points), alpha=.35/nrow(datsc), position = 'identity') + 
          
          geom_line(data=datsl,
                    aes(y=tyy, x=year, group=interaction(sec, country), color=col, alpha=ala), 
                    linewidth=lsi(lines)) + 
          geom_point(data=datsl, 
                     aes(y=tyy, x=year, group=interaction(sec, country), color=col,  alpha=ala),
                     size=lsi(points)) + 
          
          
          # graphs for first year to lyear 
          geom_line(data=datsss, 
                    aes(y=tyy, x=year, group=interaction(sec, country), color=col), 
                    linewidth=lsi(lines), alpha=.1) + 
          geom_point(data=datsss, 
                     aes(y=tyy, x=year, group=interaction(sec, country), color=col, alpha=ala),
                     size=lsi(points), alpha=.1) + 
          
          
          
          
          
          # pricing and neutrality vertical lines
          geom_segment(data=da,
                       aes(x=rv$fyear, xend=rv$fyear, y=110, yend = mi),
                       color=teksvari, linewidth=lsi(.4), linetype = "dashed", alpha=.4 ) +
          geom_segment(data=da, 
                       aes(x=rv$lyear, xend=rv$lyear, y=110, yend = mi),
                       color=teksvari, linewidth=lsi(.4), linetype = "dashed", alpha=.4 ) +  
          
          # observed and simulated vertical line
          geom_segment(data=da,
                       aes(x=2021.5, y=100, xend=2021.5, yend=mi), 
                       color= obsvari,linewidth=lsi(.6), linetype="dashed", alpha=.2) +
          
          # yearc labels
          # geom_text(data=da,
          #           aes(x=mil, y=103), label = paste0("Max"),
          #           col=teksvari, fontface="bold" ,  size =si(2.1), hjust =0, vjust=0.5, angle=c(0)) +
          # geom_text(data=datsss[, .SD[which.max(yy)]],
          #           aes(x=mil, y=103-6*1, label=paste0(round(max(yy, na.rm=TRUE), le)), color=col), 
          #           size=si(2.1), hjust=0, fontface="bold") +
          
          
          geom_label_repel(data=datsc,
                           aes(x=year+3, y=tyy,
                               label=labu,
                               color=col, alpha=ala*100),
                           fill=bgc,
                           hjust=0, size=si(2.4), fontface="bold",
                           family = fam,
                           segment.size =NA,
                           direction = "y",
                           label.padding =0,
                           xlim=c(mil,2177),
                           label.size=0,
                           max.iter=5000,
                           force=.01, force_pull=10,box.padding=.1 ,
                           seed=5) +
          
          
          
          scale_color_identity() + 
          scale_alpha_identity() + 
          
          scale_fill_identity() + 
          
          scale_x_continuous(breaks = seq(mil, 2100, 10)) +
          coord_cartesian( ylim=c(mi, ma), xlim = c(mix, max),clip ="off") +
          
          
          theme(
            axis.title.x=element_blank(),
            plot.title=element_text(lineheight = 0.8, hjust=0.5, size=33, family="Alegreya Sans Bold"),
            plot.subtitle=element_text(lineheight = 0.8, size=20, hjust=0.5),
            axis.text.x=element_blank(),
            plot.margin = plotma,
            
            # plot.margin = margin(-2,0,-2,0),
            panel.border = element_blank(), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), 
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            plot.background = element_rect(fill =bgc, color=NA), 
            panel.background = element_rect(fill = bgc ,  color="grey")
          )
        
        if (rv$pll > 1) {
          plot6 = plot6 + 
            geom_text(data=da,
                      aes(x=mil, y=100+.03*hi), label = paste0("Max"),
                      col=teksvari, fontface="bold" ,  size =si(2.0), hjust =0, vjust=0.5, angle=c(0)) +
            geom_text(data=datsss[, .SD[which.max(yy)]],
                      aes(x=mil, y=100-.03*hi, label=paste0(round(max(yy, na.rm=TRUE), le)), color=col), 
                      size=si(2.0), hjust=0, fontface="bold") 
        }
        
        if (rv$lang =="eng") {
          
          plot6 = plot6 +
            
            
            
            geom_text(data=da,
                      aes(x=rv$fyear, y=prinet), label =paste0("Pricing starts: ", rv$fyear),
                      color=teksvari, hjust=.5, size=si(2.2), fontface="bold") +
            
            geom_text(data=da,
                      aes(x=rv$lyear, y=prinet), label =paste0("Neutrality: ", rv$lyear),
                      color=teksvari, hjust=.5, size=si(2.2), fontface="bold") +
            
            
            geom_text(data=datsc[, .SD[which.max(tyy)]],
                      aes(x=year+2, y=max(tyy)+.09*hi, label=paste0("Year ",year, " values:")),
                      color=teksvari, hjust=0, size=si(2.2), fontface="bold",  family = fam) +
            
            
            geom_text(data=da,
                      aes(x=2020.5, y=max(-8,mi*.6)), label = paste0("Historical\n <=2021"),
                      col= obsvari, fontface="bold" ,  size =si(1.9), hjust =1, vjust=1, angle=c(0),
                      alpha=.5, lineheight=.99) +
            
            geom_text(data=da,
                      aes(x=2022.5, y=max(-8,mi*.6)), label = paste0("Simulated\n2022=>"),
                      col= obsvari, fontface="bold" ,  size =si(1.9), hjust =0, vjust=1,
                      angle=c(0), alpha=.5, lineheight=.99)  
          
          
        }
        
        if (rv$lang == "fin") {
          
          plot6 = plot6 +
            
            
            geom_text(data=da,
                      aes(x=rv$fyear, y=prinet), label =paste0("Hinnoittelu alkaa: ", rv$fyear),
                      color=teksvari, hjust=.5, size=si(2.2), fontface="bold") +
            
            geom_text(data=da,
                      aes(x=rv$lyear, y=prinet), label =paste0("Neutraalius: ", rv$lyear),
                      color=teksvari, hjust=.5, size=si(2.2), fontface="bold") +
            
            
            
            
            geom_text(data=datsc[, .SD[which.max(tyy)]],
                      aes(x=year+2, y=max(tyy)+.09*hi, label=paste0("Vuoden ",year, " arvot:")),
                      color=teksvari, hjust=0, size=si(2.2), fontface="bold",  family = fam) +
            
            geom_text(data=da,
                      aes(x=2020.5, y=max(-8,mi*.6)), label = paste0("Havainnoitu\n <=2021"),
                      col= obsvari, fontface="bold" ,  size =si(1.9), hjust =1, vjust=1, angle=c(0),
                      alpha=.5, lineheight=.99) +
            
            geom_text(data=da,
                      aes(x=2022.5, y=max(-8,mi*.6)), label = paste0("Simuloitu\n2022=>"),
                      col= obsvari, fontface="bold" ,  size =si(1.9), hjust =0, vjust=1,
                      angle=c(0), alpha=.5, lineheight=.99) 
          
          
          
          
        }
        
        
        if (input$startvalue ==TRUE & rv$yearc > rv$fyear) {
          
          plot6 = plot6 +
            # geom_text(data=da,
            #           aes(x=mil, y=103), label = paste0("Max"),
            #           col=teksvari, fontface="bold" ,  size =si(2.2), hjust =0, vjust=0.5, angle=c(0)) +
            geom_label_repel(data=datsf,
                             aes(x=year-2, y=tyy, label=paste0(labuf),
                                 color=col, group =sec, alpha=ala),
                             size=si(2.2), fontface="bold", hjust=1,
                             family = fam,
                             fill=bgc,
                             # segment.size =NA,
                             segment.size =NA,
                             direction = "y",
                             label.padding =0,
                             # box.padding=.1,
                             # nudge_x=18,
                             xlim=c(mil,2177),
                             label.size=0,
                             max.iter=5000,
                             force=.01, force_pull=10,box.padding=.1 ,
                             seed=5
            )
          # }
          
          if (rv$lang =="eng") {
            plot6 = plot6 +
              
              
              
              geom_text(data=datsf[, .SD[which.max(tyy)]], aes(x=year-2, y=max(tyy)+.09*hi, label=paste0(year, ":")), 
                        color=teksvari, hjust=1, size=si(2.2), fontface="bold",  family = fam) 
            
            
          }  else if (rv$lang=="fin") {
            plot6 = plot6 +
              
              
              
              geom_text(data=datsf[, .SD[which.max(tyy)]], aes(x=year-2, y=max(tyy)+.09*hi, label=paste0(year, ":")), 
                        color=teksvari, hjust=1, size=si(2.2), fontface="bold",  family = fam) 
          }
          
          
          
        }
        
        
        
        gplot6= ggplotGrob(plot6)
        li6 = list(gplot6)
        
        
        if (exists("plotlist")) {
          plotlist = c(plotlist,li6)
          
        } else {        plotlist = c(li6)
        
        }
        
        
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
      
      if (exists("plotlist")) {
        plotlist = plotlist
        
      } else {       
        
        lgeplot = list(geplot)
        plotlist = c(lgeplot)
        
      }
      
      
      lig = list(geplot)
      
      # pll = length(plotlist)
      pll = rv$pll
      
      #
      count = 1
      
      # if (pll >= 3) {
      if (input$view==2) {
        
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
        } else if (pll==5) {
          lay = rbind(c(1,2), c(3,4), c(5,6))
          plotlist = c(plotlist, lig)
          
        }
      } else if (input$view==3) {
        
        if (pll==1){
          lay=rbind(c(1))
          plotlist = c(plotlist)
          count = 1
          
        } else if (pll ==2) {
          lay = rbind(c(1), c(2))
          plotlist = c(plotlist)
          count = 2
          
        } else if (pll==3) {
          lay = rbind(c(1), c(2), c(3))
          # plotlist = c(plotlist, lig)
          plotlist = c(plotlist)
          count = 3
          
          
        } else if (pll==4) {
          lay = rbind(c(1), c(2), c(3), c(4))
          plotlist = c(plotlist)
          count = 4
          
          
        } else if (pll==5) {
          lay = rbind(c(1), c(2), c(3), c(4), c(5))
          plotlist = c(plotlist)
          count = 5
          
          
        }
      } else if (input$view==4) {
        
        if (pll==1){
          lay=rbind(c(1))
          plotlist = c(plotlist)
          count = 1
          
        } else if (pll ==2) {
          lay = rbind(c(1,2))
          plotlist = c(plotlist)
          count = 2
          
        } else if (pll==3) {
          lay =  rbind(c(1,2,3))
          # plotlist = c(plotlist, lig)
          plotlist = c(plotlist)
          count = 3
          
          
        } else if (pll==4) {
          lay =  rbind(c(1,2,3,4))
          plotlist = c(plotlist)
          count = 4
          
          
        } else if (pll==5) {
          lay =  rbind(c(1,2,3,4,5))
          plotlist = c(plotlist)
          count = 5
          
          
        }
        
      }
      
      
      plotx = grid.arrange(grobs=plotlist, layout_matrix=lay)
      
      plotx
    }) 
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # 
  
  
  
  
  
  sec3 = reactive({
    # rv$pll = length(unique(datsss()$labbi))
    
    if (input$autodraw==TRUE) {
      rv$pll = length(unique(datsl()$labbi))
    } else
    {
      eventReactive(input$go==TRUE, {
        
        rv$pll = length(unique(datsl()$labbi))
        rv$plll = length(unique(datsl()$labbi))
        # rv$plll = rv$pll
        
      })
      
    }
    withProgress( message="Drawing graph, please wait",{
      
      if (input$view ==3) {
        
        
        if (rv$pll <7) {
          si = 
            function(per) {
              per*session$clientData$output_plotk_width*session$clientData$pixelratio/.8/
                (570+.15*session$clientData$output_plotk_width)
            }
          # }
          if (input$isMobile=="FALSE") {
            
            lsi = 
              function(per) {
                per*session$clientData$output_plotk_width*session$clientData$pixelratio/500
              }
          }
          
          if (input$isMobile=="TRUE") {
            
            lsi = 
              function(per) {
                per*session$clientData$output_plotk_width*session$clientData$pixelratio/1500
              }
          }
        }
      } 
      
      if (input$view ==4) {
        
        
        
        if (rv$pll <7) {
          si =
            function(per) {
              per*session$clientData$output_plotl_height*session$clientData$pixelratio/.5/
                (570+.15*session$clientData$output_plotl_height)
            }
          # }
          if (input$isMobile=="FALSE") {
            
            lsi =
              function(per) {
                per*session$clientData$output_plotl_height*session$clientData$pixelratio/250
              }
          }
          
          if (input$isMobile=="TRUE") {
            
            lsi =
              function(per) {
                per*session$clientData$output_plotl_height*session$clientData$pixelratio/750
              }
          }
        }
      }
      
      
      # if (rv$pll <7) {
      #   si = 
      #     function(per) {
      #       per*session$clientData$output_plotl_width*session$clientData$pixelratio/.8/
      #         (570+.15*session$clientData$output_plotl_width)
      #     }
      #   # }
      #   if (input$isMobile=="FALSE") {
      #     
      #     lsi = 
      #       function(per) {
      #         per*session$clientData$output_plotl_width*session$clientData$pixelratio/500
      #       }
      #   }
      #   
      #   if (input$isMobile=="TRUE") {
      #     
      #     lsi = 
      #       function(per) {
      #         per*session$clientData$output_plotl_width*session$clientData$pixelratio/1500
      #       }
      #   }
      # }
      
      
      
      
      
      ma = 141
      # hi = ma - mi
      
      
      mil =rv$ffyear
      
      # rv$triggo
      
      
      mix = mil
      max = 2135
      
      bgc = rv$bgc
      teksvari = rv$teksvari
      obsvari = rv$obsvari
      
      
      ly = 1991
      ala = .3
      dis = 16.8
      seg = .3
      scas = 2.5
      lines = .7
      lines2 =.2
      points=.95
      segalfa=.7
      lee = 0
      fam = fam
      labsize= 2.3
      
      if (rv$plot2 == "plot2")
      {
        
        
        
        inplot= c("fossil", "land", "net", "dummy", "ghg", "nonco2", "source", "sink", "newsink")
        
        datsl = datsl()[sec %in% inplot & year >= mil,]
        datsss = datsss()[sec %in% inplot & year >= mil,]
        datsc =datsc()[sec %in% inplot,]
        datsf =datsf()[sec %in% inplot,]
        
        
        
        mi = min(min((datsss[,tyy]), na.rm=T)*1.1,-25)
        hi = ma-mi
        
        plot2=   ggplot(datsf)+
          
          
          # graph limits
          geom_segment(data=da,
                       aes(x=mil, xend=2100, y=0, yend=0), 
                       color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=0) +
          
          geom_point(data=da,
                     aes(x=2140,  y=100),
                     color=teksvari, alpha=0, size=si(2))+
          #bottom years
          geom_text(data = datsss[year %in% c(seq(mil, 2100, 10)) & sec =="dummy",], 
                    aes(x=year, y=.76*mi, label=c(year)),
                    color=teksvari, angle=0,size =si(2.4), alpha=.6) +
          
          # vertical lines
          geom_segment(data = datsss[year %in% seq(mil, 2100, 10) & sec =="dummy",],
                       (aes(x=year, xend = year, y=100, yend=.72*mi)), 
                       color=teksvari, linetype="dashed", linewidth=lsi(.4), alpha=.08) +
          
          geom_text(data=da,
                    aes(x=mil, y=-4), label = paste0("0"),
                    col=teksvari, fontface="bold" ,  size =si(2.3), hjust =0, vjust=0, angle=c(0)) +
          # horizontal lines
          geom_text(data = datsss[, .SD[which.max(yy)]], 
                    aes(x=mix+(max-mix)*.5, y=138, color=col, label = labbi),  size = si(4), fontface="bold") + 
          
          
          geom_segment(data=datsc[sec=="dummy",],
                       aes(x=mil, xend=rv$lyear, y=0, yend=0), 
                       color=teksvari, linetype ="dashed",linewidth=lsi(seg)*1.5, alpha=.3) +
          
          geom_segment(data=datsc[sec=="dummy",],
                       aes(x=mil, xend=rv$lyear, y=100, yend=100),
                       color=teksvari, linetype ="dashed",linewidth=lsi(seg)*1.5, alpha=.3) +
          geom_segment(data=datsc[sec=="dummy",],
                       aes(x=mil, xend=rv$lyear, y=75, yend=75), 
                       color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=.2) +
          geom_segment(data=datsc[sec=="dummy",],
                       aes(x=mil, xend=rv$lyear, y=50, yend=50), 
                       color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=.2) +
          geom_segment(data=datsc[sec=="dummy",],
                       aes(x=mil, xend=rv$lyear, y=25, yend=25), 
                       color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=.2) +
          
          geom_point(data=da, aes(x=2030, y=124), alpha=0) +
          
          # blue line for yearc      
          geom_segment(data=da,
                       aes(x=rv$yearc, xend=rv$yearc, y=123, yend = mi), 
                       color=blu, alpha=.4, linewidth=lsi(1.4))+
          
          
          
          geom_point(data=datsc, 
                     aes(y=tyy, x=year, group=interaction(sec, country),  alpha=ala), 
                     size=lsi(points*2), color=blu) + 
          
          # graphs for fyear to yearc
          geom_area(data=datsl[year < rv$fyear+1,], 
                    aes(y=tyy, x=year, group=interaction(sec, country),  fill=col),
                    size=si(points), alpha=.15/nrow(datsc), position = 'identity') + 
          geom_area(data=datsl[year > rv$fyear-1,], 
                    aes(y=tyy, x=year, group=interaction(sec, country),  fill=col),
                    size=si(points), alpha=.35/nrow(datsc), position = 'identity') + 
          
          geom_line(data=datsl,
                    aes(y=tyy, x=year, group=interaction(sec, country), color=col, alpha=ala), 
                    linewidth=lsi(lines)) + 
          geom_point(data=datsl, 
                     aes(y=tyy, x=year, group=interaction(sec, country), color=col,  alpha=ala),
                     size=lsi(points)) + 
          
          
          # graphs for first year to lyear 
          geom_line(data=datsss, 
                    aes(y=tyy, x=year, group=interaction(sec, country), color=col), 
                    linewidth=lsi(lines), alpha=.1) + 
          geom_point(data=datsss, 
                     aes(y=tyy, x=year, group=interaction(sec, country), color=col, alpha=ala),
                     size=lsi(points), alpha=.1) + 
          
          
          # pricing and neutrality vertical lines
          geom_segment(data=da,
                       aes(x=rv$fyear, xend=rv$fyear, y=110, yend = mi),
                       color=teksvari, linewidth=lsi(.4), linetype = "dashed", alpha=.4 ) +
          geom_segment(data=da, 
                       aes(x=rv$lyear, xend=rv$lyear, y=110, yend = mi),
                       color=teksvari, linewidth=lsi(.4), linetype = "dashed", alpha=.4 ) +  
          
          # observed and simulated vertical line
          geom_segment(data=da,
                       aes(x=2021.5, y=100, xend=2021.5, yend=mi), 
                       color= obsvari,linewidth=lsi(.6), linetype="dashed", alpha=.2) +
          
          geom_text(data=da,
                    aes(x=mil, y=103), label = paste0("Max"),
                    col=teksvari, fontface="bold" ,  size =si(2.2), hjust =0, vjust=0.5, angle=c(0)) +
          
          geom_text(data=datsss[, .SD[which.max(yy)]],
                    aes(x=mil, y=103-6*prio, label=paste0(round(max(yy, na.rm=TRUE), le), "",mark), color=col), 
                    size=si(2.2), hjust=0, fontface="bold") +
          # yearc labels
          geom_label_repel(data=datsc,
                           aes(x=year+3, y=tyy,
                               label=labu,
                               color=col, alpha=ala*100),
                           fill=bgc,
                           hjust=0, size=si(labsize), 
                           fontface="bold",
                           family = fam,
                           segment.size =NA,
                           direction = "y",
                           label.padding =0,
                           xlim=c(mil,2177),
                           label.size=0,
                           max.iter=5000,
                           force=.01, force_pull=10,box.padding=.1 ,
                           seed=5) +
          
          
          
          scale_color_identity() + 
          scale_alpha_identity() + 
          
          scale_fill_identity() + 
          
          scale_x_continuous(breaks = seq(mil, 2100, 10)) +
          coord_cartesian( ylim=c(mi, ma), xlim = c(mix, max),clip ="off") +
          
          
          theme(
            axis.title.x=element_blank(),
            plot.title=element_text(lineheight = 0.8, hjust=0.5, size=33, family="Alegreya Sans Bold"),
            plot.subtitle=element_text(lineheight = 0.8, size=20, hjust=0.5),
            axis.text.x=element_blank(),
            plot.margin = margin(-5,0,-5,0),
            panel.border = element_blank(), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), 
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            plot.background = element_rect(fill =bgc, color="grey"), 
            panel.background = element_rect(fill = bgc ,  color=NA)
          )
        
        
        
        if (rv$lang =="eng") {
          
          plot2 = plot2 +
            geom_text(data=da,
                      aes(x=rv$fyear, y=116), label =paste0("Pricing starts: ", rv$fyear),
                      color=teksvari, hjust=.5, size=si(2.2), fontface="bold") +
            
            geom_text(data=da,
                      aes(x=rv$lyear, y=116), label =paste0("Neutrality: ", rv$lyear),
                      color=teksvari, hjust=.5, size=si(2.2), fontface="bold") +
            
            
            
            
            
            
            geom_text(data=da,
                      aes(x=mil, y= 131),
                      label = paste0(rv$budget, " Gt: Net CO2 emission budget from 2020 onwards to keep temperature increase below ",rv$budinfo ),
                      size=si(1.7), color=net, hjust=0, fontface="bold") +
            
            
            geom_text(data=da,
                      aes(x=rv$fyear, y= 127),
                      label = paste0( format(round(rv$sumnet,1), nsmall=1), " Gt used from 2020 to ", rv$fyear-1
                      ),
                      size=si(1.7), color=net, hjust=1, fontface="bold") +
            
            
            
            
            geom_text(data=datsc[, .SD[which.max(tyy)]],
                      aes(x=year+2, y=max(tyy)+9, label=paste0("Year ",year, " values:")),
                      color=teksvari, hjust=0, size=si(2.2), fontface="bold",  family = fam) +
            
            
            
            # geom_text(data=da,
            #           aes(x=2020.5, y=-10), label = paste0("Historical\n <=2021"),
            #           col= obsvari, fontface="bold" ,  size =si(1.9), hjust =1, vjust=0.5, angle=c(0),
            #           alpha=.5, lineheight=.99) +
            # 
            # geom_text(data=da,
            #           aes(x=2022.5, y=-10), label = paste0("Simulated\n2022=>"),
            #           col= obsvari, fontface="bold" ,  size =si(1.9), hjust =0, vjust=0.5,
          #           angle=c(0), alpha=.5, lineheight=.99) 
          
          
          geom_label(data=da,
                     aes(x=2020.5, y=-10), label = paste0("Historical\n <=2021"),
                     col= alpha(obsvari, .4), fontface="bold" ,  size =si(1.9), hjust =1, vjust=0.5, angle=c(0),
                     alpha=.5, lineheight=.99, fill = bgc) +
            
            geom_label(data=da,
                       aes(x=2022.5, y=-10), label = paste0("Simulated\n2022=>"),
                       col= alpha(obsvari,.4), fontface="bold" ,  size =si(1.9), hjust =0, vjust=0.5,
                       angle=c(0), alpha=.7, lineheight=.99, fill = bgc) 
          
          # geom_text(data=da,
          #           aes(x=mil-5, y=-10), label = paste0("www.globalcarbonprice.com \nData: UN, IPCC, Friedlingstein et al. 2022"),
          #           col=teksvari, fontface="bold", lineheight=1.2 ,  size =si(1.5), hjust =0, vjust=0.5, angle=c(0), alpha=.5)
          
          if (rv$yearc >= rv$fyear) {
            plot2 = plot2 + 
              geom_text(data=da,
                        aes(x=rv$yearc, y= 123),  label = paste0(
                          format(round(rv$rvtotal,1), nsmall=1), " Gt used from ", rv$fyear, " to " , rv$yearc
                        )
                        ,
                        size=si(1.7), color=net, hjust=1, fontface="bold")
          }     
          
        }
        
        if (rv$lang == "fin") {
          
          plot2 = plot2 +
            geom_text(data=da,
                      aes(x=rv$fyear, y=116), label =paste0("Hinnoittelu alkaa: ", rv$fyear),
                      color=teksvari, hjust=.5, size=si(2.2), fontface="bold") +
            
            geom_text(data=da,
                      aes(x=rv$lyear, y=116), label =paste0("Neutraalius: ", rv$lyear),
                      color=teksvari, hjust=.5, size=si(2.2), fontface="bold") +
            
            
            
            
            geom_text(data=da,
                      aes(x=mil, y= 131),
                      label = paste0(rv$budget, " Gt: Nettopäästöjen (CO2) budjetti 2020 eteenpäin, jotta lämpötilanousu pysyy alle ",rv$budinfo ),
                      size=si(1.7), color=net, hjust=0, fontface="bold") +
            
            
            geom_text(data=da,
                      aes(x=rv$fyear, y= 127),
                      label = paste0( format(round(rv$sumnet,1), nsmall=1), " Gt käytetty vuodesta 2020 vuoteen ", rv$fyear
                      ),
                      size=si(1.7), color=net, hjust=1, fontface="bold") +
            
            
            geom_text(data=datsc[, .SD[which.max(tyy)]],
                      aes(x=year+2, y=max(tyy)+9, label=paste0("Vuoden ",year, " arvot:")),
                      color=teksvari, hjust=0, size=si(2.2), fontface="bold",  family = fam) +
            
            geom_text(data=da,
                      aes(x=2020.5, y=-10), label = paste0("Havainnoitu\n <=2021"),
                      col= obsvari, fontface="bold" ,  size =si(1.9), hjust =1, vjust=0.5, angle=c(0),
                      alpha=.5, lineheight=.99) +
            
            geom_text(data=da,
                      aes(x=2022.5, y=-10), label = paste0("Simuloitu\n2022=>"),
                      col= obsvari, fontface="bold" ,  size =si(1.9), hjust =0, vjust=0.5,
                      angle=c(0), alpha=.5, lineheight=.99) 
          
          # geom_text(data=da,
          #           aes(x=mil-5, y=-10), label = paste0("www.globalcarbonprice.com \nData: UN, IPCC, Friedlingstein et al. 2022"),
          #           col=teksvari, fontface="bold" ,  size =si(1.2), hjust =0, vjust=0.5, angle=c(0), alpha=.5)
          
          if (rv$yearc >= rv$fyear) {
            plot2 = plot2 + 
              
              geom_text(data=da,
                        aes(x=rv$yearc, y= 123),
                        label = paste0(
                          format(round(rv$rvtotal,1), nsmall=1), " Gt käytetty vuodesta ", rv$fyear, " ja vuoteen " , rv$yearc
                        ),
                        size=si(1.7), color=net, hjust=1, fontface="bold") 
          }     
          
        }
        
        
        if (input$startvalue ==TRUE & rv$yearc > rv$fyear) {
          plot2 = plot2 +
            
            geom_label_repel(data=datsf,
                             aes(x=year-2, y=tyy, label=paste0(labuf),
                                 color=col, group =sec, alpha=ala),
                             size=si(labsize), fontface="bold", hjust=1,
                             family = fam,
                             fill=bgc,
                             # segment.size =NA,
                             segment.size =NA,
                             direction = "y",
                             label.padding =0,
                             # box.padding=.1,
                             # nudge_x=18,
                             xlim=c(mil,2177),
                             label.size=0,
                             max.iter=5000,
                             force=.01, force_pull=10,box.padding=.1 ,
                             seed=5
            )
          
          if (rv$lang =="eng") {
            plot2 = plot2 +
              
              
              
              geom_text(data=datsf[, .SD[which.max(tyy)]], aes(x=year-3, y=max(tyy)+9, label=paste0("Year ",year, " values:")), 
                        color=teksvari, hjust=1, size=si(2.2), fontface="bold",  family = fam) 
            
            
          }  else if (rv$lang=="fin") {
            plot2 = plot2 +
              
              
              geom_text(data=datsf[, .SD[which.max(tyy)]], aes(x=year-3, y=max(tyy)+9, label=paste0("Vuoden ",year, " arvot:")), 
                        color=teksvari, hjust=1, size=si(2.2), fontface="bold",  family = fam) 
          }
          
        }
        
        gplot2 = ggplotGrob(plot2)
        li2 = list(gplot2)
        
        # plotlist = c(li2)
        
        if (exists("plotlist")) {
          plotlist = c(plotlist,li2)
          
        } else {        plotlist = c(li2)
        
        }
        
        
      }
      
      
      
      
      if (rv$plot3 == "plot3")
      {
        
        inplot= c("pop", "dummy", "countrypop")
        
        datsl = datsl()[sec %in% inplot & year >= mil,]
        datsss = datsss()[sec %in% inplot & year >= mil,]
        datsc =datsc()[sec %in% inplot,]
        datsf =datsf()[sec %in% inplot,]
        
        
        
        mi = min(min((datsss[,tyy]), na.rm=T)*1.1,-20)
        hi = ma-mi
        
        plot3=   ggplot(datsf)+
          
          
          # graph limits
          geom_segment(data=da,
                       aes(x=mil, xend=2100, y=0, yend=0), 
                       color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=0) +
          
          geom_point(data=da,
                     aes(x=2140,  y=100),
                     color=teksvari, alpha=0, size=si(2))+
          #bottom years
          geom_text(data = datsss[year %in% c(seq(mil, 2100, 10)) & sec =="dummy",], 
                    aes(x=year, y=.76*mi, label=c(year)),
                    color=teksvari, angle=0,size =si(2.4), alpha=.6) +
          
          # vertical lines
          geom_segment(data = datsss[year %in% seq(mil, 2100, 10) & sec =="dummy",],
                       (aes(x=year, xend = year, y=100, yend=.72*mi)), 
                       color=teksvari, linetype="dashed", linewidth=lsi(.4), alpha=.08) +
          
          geom_text(data=da,
                    aes(x=mil, y=-4), label = paste0("0"),
                    col=teksvari, fontface="bold" ,  size =si(2.3), hjust =0, vjust=0, angle=c(0)) +
          # horizontal lines
          geom_text(data = datsss[, .SD[which.max(yy)]], 
                    aes(x=mix+(max-mix)*.5, y=138, color=col, label = labbi),  size = si(4), fontface="bold") + 
          
          geom_segment(data=datsc[sec=="dummy",],
                       aes(x=mil, xend=rv$lyear, y=0, yend=0), 
                       color=teksvari, linetype ="dashed",linewidth=lsi(seg)*1.5, alpha=.3) +
          
          geom_segment(data=datsc[sec=="dummy",],
                       aes(x=mil, xend=rv$lyear, y=100, yend=100),
                       color=teksvari, linetype ="dashed",linewidth=lsi(seg)*1.5, alpha=.3) +
          geom_segment(data=datsc[sec=="dummy",],
                       aes(x=mil, xend=rv$lyear, y=75, yend=75), 
                       color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=.2) +
          geom_segment(data=datsc[sec=="dummy",],
                       aes(x=mil, xend=rv$lyear, y=50, yend=50), 
                       color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=.2) +
          geom_segment(data=datsc[sec=="dummy",],
                       aes(x=mil, xend=rv$lyear, y=25, yend=25), 
                       color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=.2) +
          
          geom_point(data=da, aes(x=2030, y=124), alpha=0) +
          
          # blue line for yearc      
          geom_segment(data=da,
                       aes(x=rv$yearc, xend=rv$yearc, y=123, yend = mi), 
                       color=blu, alpha=.4, linewidth=lsi(1.4))+
          
          geom_point(data=datsc, 
                     aes(y=tyy, x=year, group=interaction(sec, country),  alpha=ala), 
                     size=lsi(points*2), color=blu) + 
          
          # graphs for fyear to yearc
          geom_area(data=datsl[year < rv$fyear+1,], 
                    aes(y=tyy, x=year, group=interaction(sec, country),  fill=col),
                    size=si(points), alpha=.15/nrow(datsc), position = 'identity') + 
          geom_area(data=datsl[year > rv$fyear-1,], 
                    aes(y=tyy, x=year, group=interaction(sec, country),  fill=col),
                    size=si(points), alpha=.35/nrow(datsc), position = 'identity') + 
          
          geom_line(data=datsl,
                    aes(y=tyy, x=year, group=interaction(sec, country), color=col, alpha=ala), 
                    linewidth=lsi(lines)) + 
          geom_point(data=datsl, 
                     aes(y=tyy, x=year, group=interaction(sec, country), color=col,  alpha=ala),
                     size=lsi(points)) + 
          
          
          # graphs for first year to lyear 
          geom_line(data=datsss, 
                    aes(y=tyy, x=year, group=interaction(sec, country), color=col), 
                    linewidth=lsi(lines), alpha=.1) + 
          geom_point(data=datsss, 
                     aes(y=tyy, x=year, group=interaction(sec, country), color=col, alpha=ala),
                     size=lsi(points), alpha=.1) + 
          
          
          # pricing and neutrality vertical lines
          geom_segment(data=da,
                       aes(x=rv$fyear, xend=rv$fyear, y=110, yend = mi),
                       color=teksvari, linewidth=lsi(.4), linetype = "dashed", alpha=.4 ) +
          geom_segment(data=da, 
                       aes(x=rv$lyear, xend=rv$lyear, y=110, yend = mi),
                       color=teksvari, linewidth=lsi(.4), linetype = "dashed", alpha=.4 ) +  
          
          # observed and simulated vertical line
          geom_segment(data=da,
                       aes(x=2021.5, y=100, xend=2021.5, yend=mi), 
                       color= obsvari,linewidth=lsi(.6), linetype="dashed", alpha=.2) +
          
          geom_text(data=da,
                    aes(x=mil, y=103), label = paste0("Max"),
                    col=teksvari, fontface="bold" ,  size =si(2.2), hjust =0, vjust=0.5, angle=c(0)) +
          
          geom_text(data=datsss[, .SD[which.max(yy)]],
                    aes(x=mil, y=103-6*1, label=paste0(round(max(yy, na.rm=TRUE), le), "",mark), color=col), 
                    size=si(2.2), hjust=0, fontface="bold") +
          # yearc labels
          # geom_text(data=da,
          #           aes(x=mil-5, y=-10), label = paste0("www.globalcarbonprice.com \nData: UN, IPCC, Friedlingstein et al. 2022"),
          #           col=teksvari, fontface="bold" ,  size =si(1.2), hjust =0, vjust=0.5, angle=c(0), alpha=.5)+
          
          geom_label_repel(data=datsc,
                           aes(x=year+3, y=tyy,
                               label=labu,
                               color=col, alpha=ala*100),
                           fill=bgc,
                           hjust=0, size=si(labsize), fontface="bold",
                           family = fam,
                           segment.size =NA,
                           direction = "y",
                           label.padding =0,
                           xlim=c(mil,2177),
                           label.size=0,
                           max.iter=5000,
                           force=.01, force_pull=10,box.padding=.1 ,
                           seed=5) +
          
          
          
          scale_color_identity() + 
          scale_alpha_identity() + 
          
          scale_fill_identity() + 
          
          scale_x_continuous(breaks = seq(mil, 2100, 10)) +
          coord_cartesian( ylim=c(mi, ma), xlim = c(mix, max),clip ="off") +
          
          
          theme(
            axis.title.x=element_blank(),
            plot.title=element_text(lineheight = 0.8, hjust=0.5, size=33, family="Alegreya Sans Bold"),
            plot.subtitle=element_text(lineheight = 0.8, size=20, hjust=0.5),
            axis.text.x=element_blank(),
            plot.margin = margin(-5,0,-5,0),
            panel.border = element_blank(), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), 
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            plot.background = element_rect(fill =bgc, color=NA), 
            panel.background = element_rect(fill = bgc ,  color="grey")
          )
        
        
        
        if (rv$lang =="eng") {
          
          plot3 = plot3 +
            geom_text(data=da,
                      aes(x=rv$fyear, y=116), label =paste0("Pricing starts: ", rv$fyear),
                      color=teksvari, hjust=.5, size=si(2.2), fontface="bold") +
            
            geom_text(data=da,
                      aes(x=rv$lyear, y=116), label =paste0("Neutrality: ", rv$lyear),
                      color=teksvari, hjust=.5, size=si(2.2), fontface="bold") +
            
            geom_text(data=datsc[, .SD[which.max(tyy)]],
                      aes(x=year+2, y=max(tyy)+9, label=paste0("Year ",year, " values:")),
                      color=teksvari, hjust=0, size=si(2.2), fontface="bold",  family = fam) +
            
            
            
            geom_text(data=da,
                      aes(x=2020.5, y=.3*mi), label = paste0("Historical\n <=2021"),
                      col= obsvari, fontface="bold" ,  size =si(1.9), hjust =1, vjust=0.5, angle=c(0),
                      alpha=.5, lineheight=.99) +
            
            geom_text(data=da,
                      aes(x=2022.5, y=.3*mi), label = paste0("Simulated\n2022=>"),
                      col= obsvari, fontface="bold" ,  size =si(1.9), hjust =0, vjust=0.5,
                      angle=c(0), alpha=.5, lineheight=.99) 
          
          
        }
        
        if (rv$lang == "fin") {
          
          plot3 = plot3 +
            geom_text(data=da,
                      aes(x=rv$fyear, y=116), label =paste0("Hinnoittelu alkaa: ", rv$fyear),
                      color=teksvari, hjust=.5, size=si(2.2), fontface="bold") +
            
            geom_text(data=da,
                      aes(x=rv$lyear, y=116), label =paste0("Neutraalius: ", rv$lyear),
                      color=teksvari, hjust=.5, size=si(2.2), fontface="bold") +
            geom_text(data=datsc[, .SD[which.max(tyy)]],
                      aes(x=year+2, y=max(tyy)+9, label=paste0("Vuoden ",year, " arvot:")),
                      color=teksvari, hjust=0, size=si(2.2), fontface="bold",  family = fam) +
            
            geom_text(data=da,
                      aes(x=2020.5, y=-10), label = paste0("Havainnoitu\n <=2021"),
                      col= obsvari, fontface="bold" ,  size =si(1.9), hjust =1, vjust=0.5, angle=c(0),
                      alpha=.5, lineheight=.99) +
            
            geom_text(data=da,
                      aes(x=2022.5, y=-10), label = paste0("Simuloitu\n2022=>"),
                      col= obsvari, fontface="bold" ,  size =si(1.9), hjust =0, vjust=0.5,
                      angle=c(0), alpha=.5, lineheight=.99) 
          
          
          
        }
        
        
        if (input$startvalue ==TRUE & rv$yearc > rv$fyear) {
          plot3 = plot3 +
            
            geom_label_repel(data=datsf,
                             aes(x=year-2, y=tyy, label=paste0(labuf),
                                 color=col, group =sec, alpha=ala),
                             size=si(labsize), fontface="bold", hjust=1,
                             family = fam,
                             fill=bgc,
                             # segment.size =NA,
                             segment.size =NA,
                             direction = "y",
                             label.padding =0,
                             # box.padding=.1,
                             # nudge_x=18,
                             xlim=c(mil,2177),
                             label.size=0,
                             max.iter=5000,
                             force=.01, force_pull=10,box.padding=.1 ,
                             seed=5
            )
          
          if (rv$lang =="eng") {
            plot3 = plot3 +
              
              
              
              geom_text(data=datsf[, .SD[which.max(tyy)]], aes(x=year-3, y=max(tyy)+9, label=paste0("Year ",year, " values:")), 
                        color=teksvari, hjust=1, size=si(2.2), fontface="bold",  family = fam) 
            
            
          }  else if (rv$lang=="fin") {
            plot3 = plot3 +
              
              
              geom_text(data=datsf[, .SD[which.max(tyy)]], aes(x=year-3, y=max(tyy)+9, label=paste0("Vuoden ",year, " arvot:")), 
                        color=teksvari, hjust=1, size=si(2.2), fontface="bold",  family = fam) 
          }
          
        }
        
        gplot3 = ggplotGrob(plot3)
        li3 = list(gplot3)
        
        if (exists("plotlist")) {
          plotlist = c(plotlist,li3)
          
        } else {        plotlist = c(li3)
        
        }
        
        
      }
      
      
      
      
      
      
      if (rv$plot4 == "plot4")
      {
        
        inplot= c("avgfossil", "userfossil", "dummy", "countryfossil")
        
        datsl = datsl()[sec %in% inplot & year >= mil,]
        datsss = datsss()[sec %in% inplot & year >= mil,]
        datsc =datsc()[sec %in% inplot,]
        datsf =datsf()[sec %in% inplot,]
        
        
        
        mi = min(min((datsss[,tyy]), na.rm=T)*1.1,-20)
        hi = ma-mi
        
        plot4=   ggplot(datsf)+
          
          
          # graph limits
          geom_segment(data=da,
                       aes(x=mil, xend=2100, y=0, yend=0), 
                       color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=0) +
          
          geom_point(data=da,
                     aes(x=2140,  y=100),
                     color=teksvari, alpha=0, size=si(2))+
          #bottom years
          geom_text(data = datsss[year %in% c(seq(mil, 2100, 10)) & sec =="dummy",], 
                    aes(x=year, y=.76*mi, label=c(year)),
                    color=teksvari, angle=0,size =si(2.4), alpha=.6) +
          
          # vertical lines
          geom_segment(data = datsss[year %in% seq(mil, 2100, 10) & sec =="dummy",],
                       (aes(x=year, xend = year, y=100, yend=.72*mi)), 
                       color=teksvari, linetype="dashed", linewidth=lsi(.4), alpha=.08) +
          
          geom_text(data=da,
                    aes(x=mil, y=-4), label = paste0("0"),
                    col=teksvari, fontface="bold" ,  size =si(2.3), hjust =0, vjust=0, angle=c(0)) +
          # horizontal lines
          geom_text(data = datsss[, .SD[which.max(yy)]], 
                    aes(x=mix+(max-mix)*.5, y=138, color=col, label = labbi),  size = si(4), fontface="bold") + 
          
          geom_segment(data=datsc[sec=="dummy",],
                       aes(x=mil, xend=rv$lyear, y=0, yend=0), 
                       color=teksvari, linetype ="dashed",linewidth=lsi(seg)*1.5, alpha=.3) +
          
          geom_segment(data=datsc[sec=="dummy",],
                       aes(x=mil, xend=rv$lyear, y=100, yend=100),
                       color=teksvari, linetype ="dashed",linewidth=lsi(seg)*1.5, alpha=.3) +
          geom_segment(data=datsc[sec=="dummy",],
                       aes(x=mil, xend=rv$lyear, y=75, yend=75), 
                       color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=.2) +
          geom_segment(data=datsc[sec=="dummy",],
                       aes(x=mil, xend=rv$lyear, y=50, yend=50), 
                       color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=.2) +
          geom_segment(data=datsc[sec=="dummy",],
                       aes(x=mil, xend=rv$lyear, y=25, yend=25), 
                       color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=.2) +
          
          geom_point(data=da, aes(x=2030, y=124), alpha=0) +
          
          # blue line for yearc      
          geom_segment(data=da,
                       aes(x=rv$yearc, xend=rv$yearc, y=123, yend = mi), 
                       color=blu, alpha=.4, linewidth=lsi(1.4))+
          
          geom_point(data=datsc, 
                     aes(y=tyy, x=year, group=interaction(sec, country),  alpha=ala), 
                     size=lsi(points*2), color=blu) + 
          
          # graphs for fyear to yearc
          geom_area(data=datsl[year < rv$fyear+1,], 
                    aes(y=tyy, x=year, group=interaction(sec, country),  fill=col),
                    size=si(points), alpha=.15/nrow(datsc), position = 'identity') + 
          geom_area(data=datsl[year > rv$fyear-1,], 
                    aes(y=tyy, x=year, group=interaction(sec, country),  fill=col),
                    size=si(points), alpha=.35/nrow(datsc), position = 'identity') + 
          
          geom_line(data=datsl,
                    aes(y=tyy, x=year, group=interaction(sec, country), color=col, alpha=ala), 
                    linewidth=lsi(lines)) + 
          geom_point(data=datsl, 
                     aes(y=tyy, x=year, group=interaction(sec, country), color=col,  alpha=ala),
                     size=lsi(points)) + 
          
          
          # graphs for first year to lyear 
          geom_line(data=datsss, 
                    aes(y=tyy, x=year, group=interaction(sec, country), color=col), 
                    linewidth=lsi(lines), alpha=.1) + 
          geom_point(data=datsss, 
                     aes(y=tyy, x=year, group=interaction(sec, country), color=col, alpha=ala),
                     size=lsi(points), alpha=.1) + 
          
          
          # pricing and neutrality vertical lines
          geom_segment(data=da,
                       aes(x=rv$fyear, xend=rv$fyear, y=110, yend = mi),
                       color=teksvari, linewidth=lsi(.4), linetype = "dashed", alpha=.4 ) +
          geom_segment(data=da, 
                       aes(x=rv$lyear, xend=rv$lyear, y=110, yend = mi),
                       color=teksvari, linewidth=lsi(.4), linetype = "dashed", alpha=.4 ) +  
          
          # observed and simulated vertical line
          geom_segment(data=da,
                       aes(x=2021.5, y=100, xend=2021.5, yend=mi), 
                       color= obsvari,linewidth=lsi(.6), linetype="dashed", alpha=.2) +
          
          geom_text(data=da,
                    aes(x=mil, y=103), label = paste0("Max"),
                    col=teksvari, fontface="bold" ,  size =si(2.2), hjust =0, vjust=0.5, angle=c(0)) +
          
          geom_text(data=datsss[, .SD[which.max(yy)]],
                    aes(x=mil, y=103-6*1, label=paste0(round(max(yy, na.rm=TRUE), le), "",mark), color=col), 
                    size=si(2.2), hjust=0, fontface="bold") +
          # yearc labels
          # geom_text(data=da,
          #           aes(x=mil-5, y=-10), label = paste0("www.globalcarbonprice.com \nData: UN, IPCC, Friedlingstein et al. 2022"),
          #           col=teksvari, fontface="bold" ,  size =si(1.2), hjust =0, vjust=0.5, angle=c(0), alpha=.5)+
          
          geom_label_repel(data=datsc,
                           aes(x=year+3, y=tyy,
                               label=labu,
                               color=col, alpha=ala*100),
                           fill=bgc,
                           hjust=0, size=si(labsize), fontface="bold",
                           family = fam,
                           segment.size =NA,
                           direction = "y",
                           label.padding =0,
                           xlim=c(mil,2177),
                           label.size=0,
                           max.iter=5000,
                           force=.01, force_pull=10,box.padding=.1 ,
                           seed=5) +
          
          
          
          scale_color_identity() + 
          scale_alpha_identity() + 
          
          scale_fill_identity() + 
          
          scale_x_continuous(breaks = seq(mil, 2100, 10)) +
          coord_cartesian( ylim=c(mi, ma), xlim = c(mix, max),clip ="off") +
          
          
          theme(
            axis.title.x=element_blank(),
            plot.title=element_text(lineheight = 0.8, hjust=0.5, size=33, family="Alegreya Sans Bold"),
            plot.subtitle=element_text(lineheight = 0.8, size=20, hjust=0.5),
            axis.text.x=element_blank(),
            plot.margin = margin(-5,0,-5,0),
            panel.border = element_blank(), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), 
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            plot.background = element_rect(fill =bgc, color=NA), 
            panel.background = element_rect(fill = bgc ,  color="grey")
          )
        
        
        
        if (rv$lang =="eng") {
          
          plot4 = plot4 +
            geom_text(data=da,
                      aes(x=rv$fyear, y=116), label =paste0("Pricing starts: ", rv$fyear),
                      color=teksvari, hjust=.5, size=si(2.2), fontface="bold") +
            
            geom_text(data=da,
                      aes(x=rv$lyear, y=116), label =paste0("Neutrality: ", rv$lyear),
                      color=teksvari, hjust=.5, size=si(2.2), fontface="bold") +
            
            geom_text(data=datsc[, .SD[which.max(tyy)]],
                      aes(x=year+2, y=max(tyy)+9, label=paste0("Year ",year, " values:")),
                      color=teksvari, hjust=0, size=si(2.2), fontface="bold",  family = fam) +
            
            
            
            geom_text(data=da,
                      aes(x=2020.5, y=.3*mi), label = paste0("Historical\n <=2021"),
                      col= obsvari, fontface="bold" ,  size =si(1.9), hjust =1, vjust=0.5, angle=c(0),
                      alpha=.5, lineheight=.99) +
            
            geom_text(data=da,
                      aes(x=2022.5, y=.3*mi), label = paste0("Simulated\n2022=>"),
                      col= obsvari, fontface="bold" ,  size =si(1.9), hjust =0, vjust=0.5,
                      angle=c(0), alpha=.5, lineheight=.99) 
          
          # geom_text(data=da, aes(x=rv$lyear, y=122), 
          #           label = paste0(rv$lyear, " mean emissions: ", format(round(rv$fossil, 1), nsmall=1),"/",format(round(rv$pop, 1), nsmall=1),
          #                          " = ",format(round(rv$avgfossil, 1), nsmall=1)), 
          #           hjust=1, color=fpop, size= 2.1 )
          
          # , format(round(yy,le), decimal.mark=",", nsmall=le
        }
        
        if (rv$lang == "fin") {
          
          plot4 = plot4 +
            geom_text(data=da,
                      aes(x=rv$fyear, y=116), label =paste0("Hinnoittelu alkaa: ", rv$fyear),
                      color=teksvari, hjust=.5, size=si(2.2), fontface="bold") +
            
            geom_text(data=da,
                      aes(x=rv$lyear, y=116), label =paste0("Neutraalius: ", rv$lyear),
                      color=teksvari, hjust=.5, size=si(2.2), fontface="bold") +
            geom_text(data=datsc[, .SD[which.max(tyy)]],
                      aes(x=year+2, y=max(tyy)+9, label=paste0("Vuoden ",year, " arvot:")),
                      color=teksvari, hjust=0, size=si(2.2), fontface="bold",  family = fam) +
            
            geom_text(data=da,
                      aes(x=2020.5, y=-10), label = paste0("Havainnoitu\n <=2021"),
                      col= obsvari, fontface="bold" ,  size =si(1.9), hjust =1, vjust=0.5, angle=c(0),
                      alpha=.5, lineheight=.99) +
            
            geom_text(data=da,
                      aes(x=2022.5, y=-10), label = paste0("Simuloitu\n2022=>"),
                      col= obsvari, fontface="bold" ,  size =si(1.9), hjust =0, vjust=0.5,
                      angle=c(0), alpha=.5, lineheight=.99) 
          
          
          
        }
        
        
        if (input$startvalue ==TRUE & rv$yearc > rv$fyear) {
          plot4 = plot4 +
            
            geom_label_repel(data=datsf,
                             aes(x=year-2, y=tyy, label=paste0(labuf),
                                 color=col, group =sec, alpha=ala),
                             size=si(labsize), fontface="bold", hjust=1,
                             family = fam,
                             fill=bgc,
                             # segment.size =NA,
                             segment.size =NA,
                             direction = "y",
                             label.padding =0,
                             # box.padding=.1,
                             # nudge_x=18,
                             xlim=c(mil,2177),
                             label.size=0,
                             max.iter=5000,
                             force=.01, force_pull=10,box.padding=.1 ,
                             seed=5
            )
          
          if (rv$lang =="eng") {
            plot4 = plot4 +
              
              
              
              geom_text(data=datsf[, .SD[which.max(tyy)]], aes(x=year-3, y=max(tyy)+9, label=paste0("Year ",year, " values:")), 
                        color=teksvari, hjust=1, size=si(2.2), fontface="bold",  family = fam) 
            
            
          }  else if (rv$lang=="fin") {
            plot4 = plot4 +
              
              
              geom_text(data=datsf[, .SD[which.max(tyy)]], aes(x=year-3, y=max(tyy)+9, label=paste0("Vuoden ",year, " arvot:")), 
                        color=teksvari, hjust=1, size=si(2.2), fontface="bold",  family = fam) 
          }
          
        }
        
        gplot4 = ggplotGrob(plot4)
        li4 = list(gplot4)
        
        if (exists("plotlist")) {
          plotlist = c(plotlist,li4)
          
        } else {        plotlist = c(li4)
        
        }      
      }
      
      
      
      
      if (rv$plot5 == "plot5")
      {
        
        inplot= c("price", "dummy")
        
        datsl = datsl()[sec %in% inplot & year >= mil,]
        datsss = datsss()[sec %in% inplot & year >= mil,]
        datsc =datsc()[sec %in% inplot,]
        datsf =datsf()[sec %in% inplot,]
        
        
        
        mi = min(min((datsss[,tyy]), na.rm=T)*1.1,-20)
        hi = ma-mi
        
        plot5=   ggplot(datsf)+
          
          
          # graph limits
          geom_segment(data=da,
                       aes(x=mil, xend=2100, y=0, yend=0), 
                       color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=0) +
          
          geom_point(data=da,
                     aes(x=2140,  y=100),
                     color=teksvari, alpha=0, size=si(2))+
          #bottom years
          geom_text(data = datsss[year %in% c(seq(mil, 2100, 10)) & sec =="dummy",], 
                    aes(x=year, y=.76*mi, label=c(year)),
                    color=teksvari, angle=0,size =si(2.4), alpha=.6) +
          
          # vertical lines
          geom_segment(data = datsss[year %in% seq(mil, 2100, 10) & sec =="dummy",],
                       (aes(x=year, xend = year, y=100, yend=.72*mi)), 
                       color=teksvari, linetype="dashed", linewidth=lsi(.4), alpha=.08) +
          
          geom_text(data=da,
                    aes(x=mil, y=-4), label = paste0("0"),
                    col=teksvari, fontface="bold" ,  size =si(2.3), hjust =0, vjust=0, angle=c(0)) +
          # horizontal lines
          geom_text(data = datsss[, .SD[which.max(yy)]], 
                    aes(x=mix+(max-mix)*.5, y=138, color=col, label = labbi),  size = si(4), fontface="bold") + 
          
          geom_segment(data=datsc[sec=="dummy",],
                       aes(x=mil, xend=rv$lyear, y=0, yend=0), 
                       color=teksvari, linetype ="dashed",linewidth=lsi(seg)*1.5, alpha=.3) +
          
          geom_segment(data=datsc[sec=="dummy",],
                       aes(x=mil, xend=rv$lyear, y=100, yend=100),
                       color=teksvari, linetype ="dashed",linewidth=lsi(seg)*1.5, alpha=.3) +
          geom_segment(data=datsc[sec=="dummy",],
                       aes(x=mil, xend=rv$lyear, y=75, yend=75), 
                       color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=.2) +
          geom_segment(data=datsc[sec=="dummy",],
                       aes(x=mil, xend=rv$lyear, y=50, yend=50), 
                       color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=.2) +
          geom_segment(data=datsc[sec=="dummy",],
                       aes(x=mil, xend=rv$lyear, y=25, yend=25), 
                       color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=.2) +
          
          geom_point(data=da, aes(x=2030, y=124), alpha=0) +
          
          # blue line for yearc      
          geom_segment(data=da,
                       aes(x=rv$yearc, xend=rv$yearc, y=123, yend = mi), 
                       color=blu, alpha=.4, linewidth=lsi(1.4))+
          
          geom_point(data=datsc, 
                     aes(y=tyy, x=year, group=sec,  alpha=ala), 
                     size=lsi(points*2), color=blu) + 
          
          # graphs for fyear to yearc
          
          
          geom_area(data=datsl[year < rv$fyear+1,], 
                    aes(y=tyy, x=year, group=sec,  fill=col),
                    size=si(points), alpha=.15/nrow(datsc), position = 'identity') + 
          geom_area(data=datsl[year > rv$fyear-1,], 
                    aes(y=tyy, x=year, group=sec,  fill=col),
                    size=si(points), alpha=.35/nrow(datsc), position = 'identity') + 
          
          geom_line(data=datsl,
                    aes(y=tyy, x=year, group=sec, color=col, alpha=ala), 
                    linewidth=lsi(lines)) + 
          geom_point(data=datsl, 
                     aes(y=tyy, x=year, group=sec, color=col,  alpha=ala),
                     size=lsi(points)) + 
          
          
          # graphs for first year to lyear 
          geom_line(data=datsss, 
                    aes(y=tyy, x=year, group=interaction(sec, country), color=col), 
                    linewidth=lsi(lines), alpha=.1) + 
          geom_point(data=datsss, 
                     aes(y=tyy, x=year, group=sec, color=col, alpha=ala),
                     size=lsi(points), alpha=.1) + 
          
          
          # pricing and neutrality vertical lines
          geom_segment(data=da,
                       aes(x=rv$fyear, xend=rv$fyear, y=110, yend = mi),
                       color=teksvari, linewidth=lsi(.4), linetype = "dashed", alpha=.4 ) +
          geom_segment(data=da, 
                       aes(x=rv$lyear, xend=rv$lyear, y=110, yend = mi),
                       color=teksvari, linewidth=lsi(.4), linetype = "dashed", alpha=.4 ) +  
          
          # observed and simulated vertical line
          geom_segment(data=da,
                       aes(x=2021.5, y=100, xend=2021.5, yend=mi), 
                       color= obsvari,linewidth=lsi(.6), linetype="dashed", alpha=.2) +
          
          geom_text(data=da,
                    aes(x=mil, y=103), label = paste0("Max"),
                    col=teksvari, fontface="bold" ,  size =si(2.2), hjust =0, vjust=0.5, angle=c(0)) +
          
          geom_text(data=datsss[, .SD[which.max(yy)]],
                    aes(x=mil, y=103-6*1, label=paste0(round(max(yy, na.rm=TRUE), le), "",mark), color=col), 
                    size=si(2.2), hjust=0, fontface="bold") +
          # yearc labels
          # geom_text(data=da,
          #           aes(x=mil-5, y=-10), label = paste0("www.globalcarbonprice.com \nData: UN, IPCC, Friedlingstein et al. 2022"),
          #           col=teksvari, fontface="bold" ,  size =si(1.2), hjust =0, vjust=0.5, angle=c(0), alpha=.5)+
          
          geom_label_repel(data=datsc,
                           aes(x=year+3, y=tyy,
                               label=labu,
                               color=col, alpha=ala*100),
                           fill=bgc,
                           hjust=0, size=si(labsize), fontface="bold",
                           family = fam,
                           segment.size =NA,
                           direction = "y",
                           label.padding =0,
                           xlim=c(mil,2177),
                           label.size=0,
                           max.iter=5000,
                           force=.01, force_pull=10,box.padding=.1 ,
                           seed=5) +
          
          
          
          scale_color_identity() + 
          scale_alpha_identity() + 
          
          scale_fill_identity() + 
          
          scale_x_continuous(breaks = seq(mil, 2100, 10)) +
          coord_cartesian( ylim=c(mi, ma), xlim = c(mix, max),clip ="off") +
          
          
          theme(
            axis.title.x=element_blank(),
            plot.title=element_text(lineheight = 0.8, hjust=0.5, size=33, family="Alegreya Sans Bold"),
            plot.subtitle=element_text(lineheight = 0.8, size=20, hjust=0.5),
            axis.text.x=element_blank(),
            plot.margin = margin(-5,0,-5,0),
            panel.border = element_blank(), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), 
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            plot.background = element_rect(fill =bgc, color=NA), 
            panel.background = element_rect(fill = bgc ,  color="grey")
          )
        
        
        
        if (rv$lang =="eng") {
          
          plot5 = plot5 +
            geom_text(data=da,
                      aes(x=rv$fyear, y=116), label =paste0("Pricing starts: ", rv$fyear),
                      color=teksvari, hjust=.5, size=si(2.2), fontface="bold") +
            
            geom_text(data=da,
                      aes(x=rv$lyear, y=116), label =paste0("Neutrality: ", rv$lyear),
                      color=teksvari, hjust=.5, size=si(2.2), fontface="bold") +
            
            geom_text(data=datsc[, .SD[which.max(tyy)]],
                      aes(x=year+2, y=max(tyy)+9, label=paste0("Year ",year, " values:")),
                      color=teksvari, hjust=0, size=si(2.2), fontface="bold",  family = fam) +
            
            
            
            geom_text(data=da,
                      aes(x=2020.5, y=.3*mi), label = paste0("Historical\n <=2021"),
                      col= obsvari, fontface="bold" ,  size =si(1.9), hjust =1, vjust=0.5, angle=c(0),
                      alpha=.5, lineheight=.99) +
            
            geom_text(data=da,
                      aes(x=2022.5, y=.3*mi), label = paste0("Simulated\n2022=>"),
                      col= obsvari, fontface="bold" ,  size =si(1.9), hjust =0, vjust=0.5,
                      angle=c(0), alpha=.5, lineheight=.99) 
          
          
        }
        
        if (rv$lang == "fin") {
          
          plot5 = plot5 +
            geom_text(data=da,
                      aes(x=rv$fyear, y=116), label =paste0("Hinnoittelu alkaa: ", rv$fyear),
                      color=teksvari, hjust=.5, size=si(2.2), fontface="bold") +
            
            geom_text(data=da,
                      aes(x=rv$lyear, y=116), label =paste0("Neutraalius: ", rv$lyear),
                      color=teksvari, hjust=.5, size=si(2.2), fontface="bold") +
            geom_text(data=datsc[, .SD[which.max(tyy)]],
                      aes(x=year+2, y=max(tyy)+9, label=paste0("Vuoden ",year, " arvot:")),
                      color=teksvari, hjust=0, size=si(2.2), fontface="bold",  family = fam) +
            
            geom_text(data=da,
                      aes(x=2020.5, y=-10), label = paste0("Havainnoitu\n <=2021"),
                      col= obsvari, fontface="bold" ,  size =si(1.9), hjust =1, vjust=0.5, angle=c(0),
                      alpha=.5, lineheight=.99) +
            
            geom_text(data=da,
                      aes(x=2022.5, y=-10), label = paste0("Simuloitu\n2022=>"),
                      col= obsvari, fontface="bold" ,  size =si(1.9), hjust =0, vjust=0.5,
                      angle=c(0), alpha=.5, lineheight=.99) 
          
          
          
        }
        
        
        if (input$startvalue ==TRUE & rv$yearc > rv$fyear) {
          plot5 = plot5 +
            
            geom_label_repel(data=datsf,
                             aes(x=year-2, y=tyy, label=paste0(labuf),
                                 color=col, group =sec, alpha=ala),
                             size=si(labsize), fontface="bold", hjust=1,
                             family = fam,
                             fill=bgc,
                             # segment.size =NA,
                             segment.size =NA,
                             direction = "y",
                             label.padding =0,
                             # box.padding=.1,
                             # nudge_x=18,
                             xlim=c(mil,2177),
                             label.size=0,
                             max.iter=5000,
                             force=.01, force_pull=10,box.padding=.1 ,
                             seed=5
            )
          
          if (rv$lang =="eng") {
            plot5 = plot5 +
              
              
              
              geom_text(data=datsf[, .SD[which.max(tyy)]], aes(x=year-3, y=max(tyy)+9, label=paste0("Year ",year, " values:")), 
                        color=teksvari, hjust=1, size=si(2.2), fontface="bold",  family = fam) 
            
            
          }  else if (rv$lang=="fin") {
            plot5 = plot5 +
              
              
              geom_text(data=datsf[, .SD[which.max(tyy)]], aes(x=year-3, y=max(tyy)+9, label=paste0("Vuoden ",year, " arvot:")), 
                        color=teksvari, hjust=1, size=si(2.2), fontface="bold",  family = fam) 
          }
          
        }
        
        gplot5 = ggplotGrob(plot5)
        li5 = list(gplot5)
        
        
        if (exists("plotlist")) {
          plotlist = c(plotlist,li5)
          
        } else {        plotlist = c(li5)
        
        }
        
        
      }
      
      
      
      
      
      if (rv$plot6 == "plot6")
      {
        
        inplot= c( "averagedividend", "countrydividend", "avgcost", "netcost", "landcost",
                   "usercost","dividend","avgnetcost","countrynetcost","countrycost", "dummy")
        
        datsl = datsl()[sec %in% inplot & year >= mil,]
        datsss = datsss()[sec %in% inplot & year >= mil,]
        datsc =datsc()[sec %in% inplot,]
        datsf =datsf()[sec %in% inplot,]
        # ma = 141
        mi = min(min((datsss[,tyy]), na.rm=T)*1.18,-18)
        
        
        
        # mi = min(min((datsss[,tyy]), na.rm=T)*1.1,-20)
        hi = ma-mi
        
        plot6=   ggplot(datsf)+
          
          
          # graph limits
          geom_segment(data=da,
                       aes(x=mil, xend=2100, y=0, yend=0), 
                       color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=0) +
          
          geom_point(data=da,
                     aes(x=2140,  y=100),
                     color=teksvari, alpha=0, size=si(2))+
          #bottom years
          geom_text(data = datsss[year %in% c(seq(mil, 2100, 10)) & sec =="dummy",], 
                    aes(x=year, y=.76*mi, label=c(year)),
                    color=teksvari, angle=0,size =si(2.4), alpha=.6) +
          
          # vertical lines
          geom_segment(data = datsss[year %in% seq(mil, 2100, 10) & sec =="dummy",],
                       (aes(x=year, xend = year, y=100, yend=.72*mi)), 
                       color=teksvari, linetype="dashed", linewidth=lsi(.4), alpha=.08) +
          
          geom_text(data=da,
                    aes(x=mil, y=-4), label = paste0("0"),
                    col=teksvari, fontface="bold" ,  size =si(2.3), hjust =0, vjust=0, angle=c(0)) +
          # horizontal lines
          geom_text(data = datsss[, .SD[which.max(yy)]], 
                    aes(x=mix+(max-mix)*.5, y=138, color=col, label = labbi),  size = si(4), fontface="bold") + 
          
          
          
          geom_segment(data=datsc[sec=="dummy",],
                       aes(x=mil, xend=rv$lyear, y=0, yend=0), 
                       color=teksvari, linetype ="dashed",linewidth=lsi(seg)*1.5, alpha=.3) +
          
          geom_segment(data=datsc[sec=="dummy",],
                       aes(x=mil, xend=rv$lyear, y=100, yend=100),
                       color=teksvari, linetype ="dashed",linewidth=lsi(seg)*1.5, alpha=.3) +
          geom_segment(data=datsc[sec=="dummy",],
                       aes(x=mil, xend=rv$lyear, y=75, yend=75), 
                       color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=.2) +
          geom_segment(data=datsc[sec=="dummy",],
                       aes(x=mil, xend=rv$lyear, y=50, yend=50), 
                       color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=.2) +
          geom_segment(data=datsc[sec=="dummy",],
                       aes(x=mil, xend=rv$lyear, y=25, yend=25), 
                       color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=.2) +
          
          geom_point(data=da, aes(x=2030, y=124), alpha=0) +
          
          # blue line for yearc      
          geom_segment(data=da,
                       aes(x=rv$yearc, xend=rv$yearc, y=123, yend = mi), 
                       color=blu, alpha=.4, linewidth=lsi(1.4))+
          
          geom_point(data=datsc, 
                     aes(y=tyy, x=year, group=interaction(sec, country),  alpha=ala), 
                     size=lsi(points*2), color=blu) + 
          
          # graphs for fyear to yearc
          geom_area(data=datsl[year < rv$fyear+1,], 
                    aes(y=tyy, x=year, group=interaction(sec, country),  fill=col),
                    size=si(points), alpha=.15/nrow(datsc), position = 'identity') + 
          geom_area(data=datsl[year > rv$fyear-1,], 
                    aes(y=tyy, x=year, group=interaction(sec, country),  fill=col),
                    size=si(points), alpha=.35/nrow(datsc), position = 'identity') + 
          
          geom_line(data=datsl,
                    aes(y=tyy, x=year, group=interaction(sec, country), color=col, alpha=ala), 
                    linewidth=lsi(lines)) + 
          geom_point(data=datsl, 
                     aes(y=tyy, x=year, group=interaction(sec, country), color=col,  alpha=ala),
                     size=lsi(points)) + 
          
          
          # graphs for first year to lyear 
          geom_line(data=datsss, 
                    aes(y=tyy, x=year, group=interaction(sec, country), color=col), 
                    linewidth=lsi(lines), alpha=.1) + 
          geom_point(data=datsss, 
                     aes(y=tyy, x=year, group=interaction(sec, country), color=col, alpha=ala),
                     size=lsi(points), alpha=.1) + 
          
          # pricing and neutrality vertical lines
          geom_segment(data=da,
                       aes(x=rv$fyear, xend=rv$fyear, y=110, yend = mi),
                       color=teksvari, linewidth=lsi(.4), linetype = "dashed", alpha=.4 ) +
          geom_segment(data=da, 
                       aes(x=rv$lyear, xend=rv$lyear, y=110, yend = mi),
                       color=teksvari, linewidth=lsi(.4), linetype = "dashed", alpha=.4 ) +  
          
          # observed and simulated vertical line
          geom_segment(data=da,
                       aes(x=2021.5, y=100, xend=2021.5, yend=mi), 
                       color= obsvari,linewidth=lsi(.6), linetype="dashed", alpha=.2) +
          
          geom_text(data=da,
                    aes(x=mil, y=103), label = paste0("Max"),
                    col=teksvari, fontface="bold" ,  size =si(2.2), hjust =0, vjust=0.5, angle=c(0)) +
          
          geom_text(data=datsss[, .SD[which.max(yy)]],
                    aes(x=mil, y=103-6*1, label=paste0(round(max(yy, na.rm=TRUE), le), "",mark), color=col), 
                    size=si(2.2), hjust=0, fontface="bold") +
          # yearc labels
          # geom_text(data=da,
          #           aes(x=mil-5, y=-10), label = paste0("www.globalcarbonprice.com \nData: UN, IPCC, Friedlingstein et al. 2022"),
          #           col=teksvari, fontface="bold" ,  size =si(1.2), hjust =0, vjust=0.5, angle=c(0), alpha=.5)+
          
          geom_label_repel(data=datsc,
                           aes(x=year+3, y=tyy,
                               label=labu,
                               color=col, alpha=ala*100),
                           fill=bgc,
                           hjust=0, size=si(2.5), fontface="bold",
                           family = fam,
                           segment.size =NA,
                           direction = "y",
                           label.padding =0,
                           xlim=c(mil,2177),
                           label.size=0,
                           max.iter=5000,
                           force=.01, force_pull=10,box.padding=.1 ,
                           seed=5) +
          
          
          
          scale_color_identity() + 
          scale_alpha_identity() + 
          
          scale_fill_identity() + 
          
          scale_x_continuous(breaks = seq(mil, 2100, 10)) +
          coord_cartesian( ylim=c(mi, ma), xlim = c(mix, max),clip ="off") +
          
          
          theme(
            axis.title.x=element_blank(),
            plot.title=element_text(lineheight = 0.8, hjust=0.5, size=33, family="Alegreya Sans Bold"),
            plot.subtitle=element_text(lineheight = 0.8, size=20, hjust=0.5),
            axis.text.x=element_blank(),
            plot.margin = margin(-5,0,-5,0),
            panel.border = element_blank(), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), 
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            plot.background = element_rect(fill =bgc, color=NA), 
            panel.background = element_rect(fill = bgc ,  color="grey")
          )
        
        
        
        if (rv$lang =="eng") {
          
          plot6 = plot6 +
            geom_text(data=da,
                      aes(x=rv$fyear, y=prinet), label =paste0("Pricing starts: ", rv$fyear),
                      color=teksvari, hjust=.5, size=si(2.2), fontface="bold") +
            
            geom_text(data=da,
                      aes(x=rv$lyear, y=prinet), label =paste0("Neutrality: ", rv$lyear),
                      color=teksvari, hjust=.5, size=si(2.2), fontface="bold") +
            
            geom_text(data=datsc[, .SD[which.max(tyy)]],
                      aes(x=year+2, y=max(tyy)+.09*hi, label=paste0("Year ",year, " values:")),
                      color=teksvari, hjust=0, size=si(2.2), fontface="bold",  family = fam) +
            
            geom_text(data = datsss[, .SD[which.max(yy)]], 
                      aes(x=mix+(max-mix)*.5, y=128), color=teksvari, 
                      label = "(Does not include costs from land use change)",  size = si(2.0), fontface="bold") + 
            
            
            geom_text(data=da,
                      aes(x=2020.5, y=.3*mi), label = paste0("Historical\n <=2021"),
                      col= obsvari, fontface="bold" ,  size =si(1.9), hjust =1, vjust=0.5, angle=c(0),
                      alpha=.5, lineheight=.99) +
            
            geom_text(data=da,
                      aes(x=2022.5, y=.3*mi), label = paste0("Simulated\n2022=>"),
                      col= obsvari, fontface="bold" ,  size =si(1.9), hjust =0, vjust=0.5,
                      angle=c(0), alpha=.5, lineheight=.99) 
          
          
        }
        
        if (rv$lang == "fin") {
          
          plot6 = plot6 +
            geom_text(data=da,
                      aes(x=rv$fyear, y=prinet), label =paste0("Hinnoittelu alkaa: ", rv$fyear),
                      color=teksvari, hjust=.5, size=si(2.2), fontface="bold") +
            
            geom_text(data=da,
                      aes(x=rv$lyear, y=prinet), label =paste0("Neutraalius: ", rv$lyear),
                      color=teksvari, hjust=.5, size=si(2.2), fontface="bold") +
            geom_text(data=datsc[, .SD[which.max(tyy)]],
                      aes(x=year+2, y=  max(tyy)+.09*hi, label=paste0("Vuoden ",year, " arvot:")),
                      color=teksvari, hjust=0, size=si(2.2), fontface="bold",  family = fam) +
            
            geom_text(data = datsss[, .SD[which.max(yy)]], 
                      aes(x=mix+(max-mix)*.5, y=128), color=teksvari, 
                      label = "(Ei sisällä kuluja maankäytön muutoksesta)",  size = si(2.0), fontface="bold") + 
            
            
            geom_text(data=da,
                      aes(x=2020.5, y=-10), label = paste0("Havainnoitu\n <=2021"),
                      col= obsvari, fontface="bold" ,  size =si(1.9), hjust =1, vjust=0.5, angle=c(0),
                      alpha=.5, lineheight=.99) +
            
            geom_text(data=da,
                      aes(x=2022.5, y=-10), label = paste0("Simuloitu\n2022=>"),
                      col= obsvari, fontface="bold" ,  size =si(1.9), hjust =0, vjust=0.5,
                      angle=c(0), alpha=.5, lineheight=.99) 
          
          
          
          
        }
        
        
        if (input$startvalue ==TRUE & rv$yearc > rv$fyear) {
          plot6 = plot6 +
            
            geom_label_repel(data=datsf,
                             aes(x=year-2, y=tyy, label=paste0(labuf),
                                 color=col, group =sec, alpha=ala),
                             size=si(2.5), fontface="bold", hjust=1,
                             family = fam,
                             fill=bgc,
                             # segment.size =NA,
                             segment.size =NA,
                             direction = "y",
                             label.padding =0,
                             # box.padding=.1,
                             # nudge_x=18,
                             xlim=c(mil,2177),
                             label.size=0,
                             max.iter=5000,
                             force=.01, force_pull=10,box.padding=.1 ,
                             seed=5
            )
          
          if (rv$lang =="eng") {
            plot6 = plot6 +
              
              
              
              geom_text(data=datsf[, .SD[which.max(tyy)]], aes(x=year-3, y=max(tyy)+.09*hi, label=paste0("Year ",year, " values:")), 
                        color=teksvari, hjust=1, size=si(2.2), fontface="bold",  family = fam) 
            
            
          }  else if (rv$lang=="fin") {
            plot6 = plot6 +
              
              
              geom_text(data=datsf[, .SD[which.max(tyy)]], aes(x=year-3, y=max(tyy)+.09*hi, label=paste0("Vuoden ",year, " arvot:")), 
                        color=teksvari, hjust=1, size=si(2.2), fontface="bold",  family = fam) 
          }
          
        }
        
        gplot6 = ggplotGrob(plot6)
        li6 = list(gplot6)
        
        if (exists("plotlist")) {
          plotlist = c(plotlist,li6)
          
        } else {        plotlist = c(li6)
        
        }
        
        
      }
      
      
      
      
      
      eplot = ggplot(NULL, aes(color = ""))+
        geom_blank()+
        scale_color_manual(values = "black", labels = "Something")+
        guides(color = guide_legend())+
        theme(
          plot.background = element_rect(fill =bgc, color=NA),
          panel.background = element_rect(fill = bgc ,  color="grey")
        )
      geplot = ggplotGrob(eplot)
      lig = list(geplot)
      
      if (exists("plotlist")) {
        plotlist = plotlist
        
      } else {        plotlist = c(lig)
      
      }
      
      
      pll = length(plotlist)
      #
      count = 1
      # if (pll >= 3) {
      if (input$view==2) {
        
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
        } else if (pll==5) {
          lay = rbind(c(1,2), c(3,4), c(5,6))
          plotlist = c(plotlist, lig)
          
        }
      } else if (input$view==3) {
        
        if (pll==1){
          lay=rbind(c(1))
          plotlist = c(plotlist)
          count = 1
          
        } else if (pll ==2) {
          lay = rbind(c(1), c(2))
          plotlist = c(plotlist)
          count = 2
          
        } else if (pll==3) {
          lay = rbind(c(1), c(2), c(3))
          # plotlist = c(plotlist, lig)
          plotlist = c(plotlist)
          count = 3
          
          
        } else if (pll==4) {
          lay = rbind(c(1), c(2), c(3), c(4))
          plotlist = c(plotlist)
          count = 4
          
          
        } else if (pll==5) {
          lay = rbind(c(1), c(2), c(3), c(4), c(5))
          plotlist = c(plotlist)
          count = 5
          
          
        }
      } else if (input$view==4) {
        
        if (pll==1){
          lay=rbind(c(1))
          plotlist = c(plotlist)
          count = 1
          
        } else if (pll ==2) {
          lay = rbind(c(1,2))
          plotlist = c(plotlist)
          count = 2
          
        } else if (pll==3) {
          lay =  rbind(c(1,2,3))
          # plotlist = c(plotlist, lig)
          plotlist = c(plotlist)
          count = 3
          
          
        } else if (pll==4) {
          lay =  rbind(c(1,2,3,4))
          plotlist = c(plotlist)
          count = 4
          
          
        } else if (pll==5) {
          lay =  rbind(c(1,2,3,4,5))
          plotlist = c(plotlist)
          count = 5
          
          
        }
        
      }
      
      # rv$pll =pll
      # }
      
      plotx = grid.arrange(grobs=plotlist, layout_matrix=lay)
      
      plotx
      
    })
    
  })
  
  {
    
    {
      #   sec4 = reactive({
      #   # rv$pll = length(unique(datsss()$labbi))
      #    
      #    # rv$trig
      #   if (input$autodraw==TRUE) {
      #     rv$pll = length(unique(datsl()$labbi))
      #   } else
      #   {
      #     eventReactive(input$go==TRUE, {
      #       req(datsss())
      #       
      #       rv$pll = length(unique(datsl()$labbi))
      #       rv$plll = length(unique(datsl()$labbi))
      #       # rv$plll = rv$pll
      # 
      #     })
      # 
      #   }
      #   withProgress( message="Drawing graph, please wait",{
      # 
      #     if (input$view ==3) {
      # 
      # 
      #       if (rv$pll <7) {
      #         si =
      #           function(per) {
      #             per*session$clientData$output_plotk_width*session$clientData$pixelratio/.8/
      #               (570+.15*session$clientData$output_plotk_width)
      #           }
      #         # }
      #         if (input$isMobile=="FALSE") {
      # 
      #           lsi =
      #             function(per) {
      #               per*session$clientData$output_plotk_width*session$clientData$pixelratio/500
      #             }
      #         }
      # 
      #         if (input$isMobile=="TRUE") {
      # 
      #           lsi =
      #             function(per) {
      #               per*session$clientData$output_plotk_width*session$clientData$pixelratio/1500
      #             }
      #         }
      #       }}
      # 
      #     if (input$view ==4) {
      # 
      # 
      # 
      #       if (rv$pll <7) {
      #         si =
      #           function(per) {
      #             per*session$clientData$output_plotl_height*session$clientData$pixelratio/.5/
      #               (570+.15*session$clientData$output_plotl_height)
      #           }
      #         # }
      #         if (input$isMobile=="FALSE") {
      # 
      #           lsi =
      #             function(per) {
      #               per*session$clientData$output_plotl_height*session$clientData$pixelratio/250
      #             }
      #         }
      # 
      #         if (input$isMobile=="TRUE") {
      # 
      #           lsi =
      #             function(per) {
      #               per*session$clientData$output_plotl_height*session$clientData$pixelratio/750
      #             }
      #         }
      #       }
      #     }
      # 
      # 
      #     # if (rv$pll <7) {
      #     #   si =
      #     #     function(per) {
      #     #       per*session$clientData$output_plotl_width*session$clientData$pixelratio/.8/
      #     #         (570+.15*session$clientData$output_plotl_width)
      #     #     }
      #     #   # }
      #     #   if (input$isMobile=="FALSE") {
      #     #
      #     #     lsi =
      #     #       function(per) {
      #     #         per*session$clientData$output_plotl_width*session$clientData$pixelratio/500
      #     #       }
      #     #   }
      #     #
      #     #   if (input$isMobile=="TRUE") {
      #     #
      #     #     lsi =
      #     #       function(per) {
      #     #         per*session$clientData$output_plotl_width*session$clientData$pixelratio/1500
      #     #       }
      #     #   }
      #     # }
      # 
      # 
      # 
      # 
      # 
      #     ma = 141
      #     # hi = ma - mi
      # 
      # 
      #     mil =rv$ffyear
      # 
      # 
      # 
      # 
      #     mix = mil
      #     max = 2135
      # 
      #     bgc = rv$bgc
      #     teksvari = rv$teksvari
      #     obsvari = rv$obsvari
      # 
      # 
      #     ly = 1991
      #     ala = .3
      #     dis = 16.8
      #     seg = .3
      #     scas = 2.5
      #     lines = .7
      #     lines2 =.2
      #     points=1.1
      #     segalfa=.7
      #     lee = 0
      #     fam = fam
      #     labsize= 2.3
      # 
      #     if (rv$plot2 == "plot2")
      #     {
      # 
      # 
      # 
      #       inplot= c("fossil", "land", "net", "dummy", "ghg", "nonco2")
      # 
      #       datsl = datsl()[sec %in% inplot & year >= mil,]
      #       datsss = datsss()[sec %in% inplot & year >= mil,]
      #       datsc =datsc()[sec %in% inplot,]
      #       datsf =datsf()[sec %in% inplot,]
      # 
      # 
      # 
      #       mi = min(min((datsss[,tyy]), na.rm=T)*1.1,-25)
      #       hi = ma-mi
      #       
      #       plot2=   ggplot(datsf)+
      # 
      # 
      #         # graph limits
      #         geom_segment(data=da,
      #                      aes(x=mil, xend=2100, y=0, yend=0),
      #                      color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=0) +
      # 
      #         geom_point(data=da,
      #                    aes(x=2140,  y=100),
      #                    color=teksvari, alpha=0, size=si(2))+
      #         #bottom years
      #         geom_text(data = datsss[year %in% c(seq(mil, 2100, 10)) & sec =="dummy",],
      #                   aes(x=year, y=.76*mi, label=c(year)),
      #                   color=teksvari, angle=0,size =si(2.4), alpha=.6) +
      # 
      #         # vertical lines
      #         geom_segment(data = datsss[year %in% seq(mil, 2100, 10) & sec =="dummy",],
      #                      (aes(x=year, xend = year, y=100, yend=.72*mi)),
      #                      color=teksvari, linetype="dashed", linewidth=lsi(.4), alpha=.08) +
      # 
      #         geom_text(data=da,
      #                   aes(x=mil, y=-4), label = paste0("0"),
      #                   col=teksvari, fontface="bold" ,  size =si(2.3), hjust =0, vjust=0, angle=c(0)) +
      #         # horizontal lines
      #         geom_text(data = datsss[, .SD[which.max(yy)]],
      #                   aes(x=mix+(max-mix)*.5, y=138, color=col, label = labbi),  size = si(4), fontface="bold") +
      # 
      # 
      #         geom_segment(data=datsc[sec=="dummy",],
      #                      aes(x=mil, xend=rv$lyear, y=0, yend=0),
      #                      color=teksvari, linetype ="dashed",linewidth=lsi(seg)*1.5, alpha=.3) +
      # 
      #         geom_segment(data=datsc[sec=="dummy",],
      #                      aes(x=mil, xend=rv$lyear, y=100, yend=100),
      #                      color=teksvari, linetype ="dashed",linewidth=lsi(seg)*1.5, alpha=.3) +
      #         geom_segment(data=datsc[sec=="dummy",],
      #                      aes(x=mil, xend=rv$lyear, y=75, yend=75),
      #                      color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=.2) +
      #         geom_segment(data=datsc[sec=="dummy",],
      #                      aes(x=mil, xend=rv$lyear, y=50, yend=50),
      #                      color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=.2) +
      #         geom_segment(data=datsc[sec=="dummy",],
      #                      aes(x=mil, xend=rv$lyear, y=25, yend=25),
      #                      color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=.2) +
      # 
      #         geom_point(data=da, aes(x=2030, y=124), alpha=0) +
      # 
      #         # blue line for yearc
      #         geom_segment(data=da,
      #                      aes(x=rv$yearc, xend=rv$yearc, y=123, yend = mi),
      #                      color=blu, alpha=.4, linewidth=lsi(1.4))+
      # 
      # 
      # 
      #         geom_point(data=datsc,
      #                    aes(y=tyy, x=year, group=interaction(sec, country),  alpha=ala),
      #                    size=lsi(points*2), color=blu) +
      # 
      #         # graphs for fyear to yearc
      #         geom_area(data=datsl[year < rv$fyear+1,],
      #                   aes(y=tyy, x=year, group=interaction(sec, country),  fill=col),
      #                   size=si(points), alpha=.15/nrow(datsc), position = 'identity') +
      #         geom_area(data=datsl[year > rv$fyear-1,],
      #                   aes(y=tyy, x=year, group=interaction(sec, country),  fill=col),
      #                   size=si(points), alpha=.35/nrow(datsc), position = 'identity') +
      # 
      #         geom_line(data=datsl,
      #                   aes(y=tyy, x=year, group=interaction(sec, country), color=col, alpha=ala),
      #                   linewidth=lsi(lines)) +
      #         geom_point(data=datsl,
      #                    aes(y=tyy, x=year, group=interaction(sec, country), color=col,  alpha=ala),
      #                    size=lsi(points)) +
      # 
      # 
      #         # graphs for first year to lyear
      #         geom_line(data=datsss,
      #                   aes(y=tyy, x=year, group=interaction(sec, country), color=col),
      #                   linewidth=lsi(lines), alpha=.1) +
      #         geom_point(data=datsss,
      #                    aes(y=tyy, x=year, group=interaction(sec, country), color=col, alpha=ala),
      #                    size=lsi(points), alpha=.1) +
      # 
      # 
      #         # pricing and neutrality vertical lines
      #         geom_segment(data=da,
      #                      aes(x=rv$fyear, xend=rv$fyear, y=110, yend = mi),
      #                      color=teksvari, linewidth=lsi(.4), linetype = "dashed", alpha=.4 ) +
      #         geom_segment(data=da,
      #                      aes(x=rv$lyear, xend=rv$lyear, y=110, yend = mi),
      #                      color=teksvari, linewidth=lsi(.4), linetype = "dashed", alpha=.4 ) +
      # 
      #         # observed and simulated vertical line
      #         geom_segment(data=da,
      #                      aes(x=2021.5, y=100, xend=2021.5, yend=mi),
      #                      color= obsvari,linewidth=lsi(.6), linetype="dashed", alpha=.2) +
      # 
      #         geom_text(data=da,
      #                   aes(x=mil, y=103), label = paste0("Max"),
      #                   col=teksvari, fontface="bold" ,  size =si(2.2), hjust =0, vjust=0.5, angle=c(0)) +
      # 
      #         geom_text(data=datsss[, .SD[which.max(yy)]],
      #                   aes(x=mil, y=103-6*prio, label=paste0(round(max(yy, na.rm=TRUE), le), "",mark), color=col),
      #                   size=si(2.2), hjust=0, fontface="bold") +
      #         # yearc labels
      #         geom_label_repel(data=datsc,
      #                          aes(x=year+3, y=tyy,
      #                              label=labu,
      #                              color=col, alpha=ala*100),
      #                          fill=bgc,
      #                          hjust=0, size=si(labsize),
      #                          fontface="bold",
      #                          family = fam,
      #                          segment.size =NA,
      #                          direction = "y",
      #                          label.padding =0,
      #                          xlim=c(mil,2177),
      #                          label.size=0,
      #                          max.iter=5000,
      #                          force=.01, force_pull=10,box.padding=.1 ,
      #                          seed=5) +
      # 
      # 
      # 
      #         scale_color_identity() +
      #         scale_alpha_identity() +
      # 
      #         scale_fill_identity() +
      # 
      #         scale_x_continuous(breaks = seq(mil, 2100, 10)) +
      #         coord_cartesian( ylim=c(mi, ma), xlim = c(mix, max),clip ="off") +
      # 
      # 
      #         theme(
      #           axis.title.x=element_blank(),
      #           plot.title=element_text(lineheight = 0.8, hjust=0.5, size=33, family="Alegreya Sans Bold"),
      #           plot.subtitle=element_text(lineheight = 0.8, size=20, hjust=0.5),
      #           axis.text.x=element_blank(),
      #           plot.margin = margin(-5,0,-5,0),
      #           panel.border = element_blank(),
      #           panel.grid.major = element_blank(),
      #           panel.grid.minor = element_blank(),
      #           axis.title.y=element_blank(),
      #           axis.text.y=element_blank(),
      #           plot.background = element_rect(fill =bgc, color="grey"),
      #           panel.background = element_rect(fill = bgc ,  color=NA)
      #         )
      # 
      # 
      # 
      #       if (rv$lang =="eng") {
      # 
      #         plot2 = plot2 +
      #           geom_text(data=da,
      #                     aes(x=rv$fyear, y=116), label =paste0("Pricing starts: ", rv$fyear),
      #                     color=teksvari, hjust=.5, size=si(2.2), fontface="bold") +
      # 
      #           geom_text(data=da,
      #                     aes(x=rv$lyear, y=116), label =paste0("Neutrality: ", rv$lyear),
      #                     color=teksvari, hjust=.5, size=si(2.2), fontface="bold") +
      # 
      # 
      # 
      # 
      # 
      # 
      #           geom_text(data=da,
      #                     aes(x=mil, y= 131),
      #                     label = paste0(rv$budget, " Gt: Net CO2 emission budget from 2020 onwards to keep temperature increase below ",rv$budinfo ),
      #                     size=si(1.7), color=net, hjust=0, fontface="bold") +
      # 
      # 
      #           geom_text(data=da,
      #                     aes(x=rv$fyear, y= 127),
      #                     label = paste0( format(round(rv$sumnet,1), nsmall=1), " Gt used from 2020 to ", rv$fyear-1
      #                     ),
      #                     size=si(1.7), color=net, hjust=1, fontface="bold") +
      # 
      # 
      # 
      # 
      #           geom_text(data=datsc[, .SD[which.max(tyy)]],
      #                     aes(x=year+2, y=max(tyy)+9, label=paste0("Year ",year, " values:")),
      #                     color=teksvari, hjust=0, size=si(2.2), fontface="bold",  family = fam) +
      # 
      # 
      # 
      #           # geom_text(data=da,
      #           #           aes(x=2020.5, y=-10), label = paste0("Historical\n <=2021"),
      #           #           col= obsvari, fontface="bold" ,  size =si(1.9), hjust =1, vjust=0.5, angle=c(0),
      #           #           alpha=.5, lineheight=.99) +
      #           #
      #           # geom_text(data=da,
      #           #           aes(x=2022.5, y=-10), label = paste0("Simulated\n2022=>"),
      #           #           col= obsvari, fontface="bold" ,  size =si(1.9), hjust =0, vjust=0.5,
      #         #           angle=c(0), alpha=.5, lineheight=.99)
      # 
      # 
      #         geom_label(data=da,
      #                    aes(x=2020.5, y=-10), label = paste0("Historical\n <=2021"),
      #                    col= alpha(obsvari, .4), fontface="bold" ,  size =si(1.9), hjust =1, vjust=0.5, angle=c(0),
      #                    alpha=.5, lineheight=.99, fill = bgc) +
      # 
      #           geom_label(data=da,
      #                      aes(x=2022.5, y=-10), label = paste0("Simulated\n2022=>"),
      #                      col= alpha(obsvari,.4), fontface="bold" ,  size =si(1.9), hjust =0, vjust=0.5,
      #                      angle=c(0), alpha=.7, lineheight=.99, fill = bgc)
      # 
      #         # geom_text(data=da,
      #         #           aes(x=mil-5, y=-10), label = paste0("www.globalcarbonprice.com \nData: UN, IPCC, Friedlingstein et al. 2022"),
      #         #           col=teksvari, fontface="bold", lineheight=1.2 ,  size =si(1.5), hjust =0, vjust=0.5, angle=c(0), alpha=.5)
      # 
      #         if (rv$yearc >= rv$fyear) {
      #           plot2 = plot2 +
      #             geom_text(data=da,
      #                       aes(x=rv$yearc, y= 123),  label = paste0(
      #                         format(round(rv$rvtotal,1), nsmall=1), " Gt used from ", rv$fyear, " to " , rv$yearc
      #                       )
      #                       ,
      #                       size=si(1.7), color=net, hjust=1, fontface="bold")
      #         }
      # 
      #       }
      # 
      #       if (rv$lang == "fin") {
      # 
      #         plot2 = plot2 +
      #           geom_text(data=da,
      #                     aes(x=rv$fyear, y=116), label =paste0("Hinnoittelu alkaa: ", rv$fyear),
      #                     color=teksvari, hjust=.5, size=si(2.2), fontface="bold") +
      # 
      #           geom_text(data=da,
      #                     aes(x=rv$lyear, y=116), label =paste0("Neutraalius: ", rv$lyear),
      #                     color=teksvari, hjust=.5, size=si(2.2), fontface="bold") +
      # 
      # 
      # 
      # 
      #           geom_text(data=da,
      #                     aes(x=mil, y= 131),
      #                     label = paste0(rv$budget, " Gt: Nettopäästöjen (CO2) budjetti 2020 eteenpäin, jotta lämpötilanousu pysyy alle ",rv$budinfo ),
      #                     size=si(1.7), color=net, hjust=0, fontface="bold") +
      # 
      # 
      #           geom_text(data=da,
      #                     aes(x=rv$fyear, y= 127),
      #                     label = paste0( format(round(rv$sumnet,1), nsmall=1), " Gt käytetty vuodesta 2020 vuoteen ", rv$fyear
      #                     ),
      #                     size=si(1.7), color=net, hjust=1, fontface="bold") +
      # 
      # 
      #           geom_text(data=datsc[, .SD[which.max(tyy)]],
      #                     aes(x=year+2, y=max(tyy)+9, label=paste0("Vuoden ",year, " arvot:")),
      #                     color=teksvari, hjust=0, size=si(2.2), fontface="bold",  family = fam) +
      # 
      #           geom_text(data=da,
      #                     aes(x=2020.5, y=-10), label = paste0("Havainnoitu\n <=2021"),
      #                     col= obsvari, fontface="bold" ,  size =si(1.9), hjust =1, vjust=0.5, angle=c(0),
      #                     alpha=.5, lineheight=.99) +
      # 
      #           geom_text(data=da,
      #                     aes(x=2022.5, y=-10), label = paste0("Simuloitu\n2022=>"),
      #                     col= obsvari, fontface="bold" ,  size =si(1.9), hjust =0, vjust=0.5,
      #                     angle=c(0), alpha=.5, lineheight=.99)
      # 
      #         # geom_text(data=da,
      #         #           aes(x=mil-5, y=-10), label = paste0("www.globalcarbonprice.com \nData: UN, IPCC, Friedlingstein et al. 2022"),
      #         #           col=teksvari, fontface="bold" ,  size =si(1.2), hjust =0, vjust=0.5, angle=c(0), alpha=.5)
      # 
      #         if (rv$yearc >= rv$fyear) {
      #           plot2 = plot2 +
      # 
      #             geom_text(data=da,
      #                       aes(x=rv$yearc, y= 123),
      #                       label = paste0(
      #                         format(round(rv$rvtotal,1), nsmall=1), " Gt käytetty vuodesta ", rv$fyear, " ja vuoteen " , rv$yearc
      #                       ),
      #                       size=si(1.7), color=net, hjust=1, fontface="bold")
      #         }
      # 
      #       }
      # 
      # 
      #       if (input$startvalue ==TRUE & rv$yearc > rv$fyear) {
      #         plot2 = plot2 +
      # 
      #           geom_label_repel(data=datsf,
      #                            aes(x=year-2, y=tyy, label=paste0(labuf),
      #                                color=col, group =sec, alpha=ala),
      #                            size=si(labsize), fontface="bold", hjust=1,
      #                            family = fam,
      #                            fill=bgc,
      #                            # segment.size =NA,
      #                            segment.size =NA,
      #                            direction = "y",
      #                            label.padding =0,
      #                            # box.padding=.1,
      #                            # nudge_x=18,
      #                            xlim=c(mil,2177),
      #                            label.size=0,
      #                            max.iter=5000,
      #                            force=.01, force_pull=10,box.padding=.1 ,
      #                            seed=5
      #           )
      # 
      #         if (rv$lang =="eng") {
      #           plot2 = plot2 +
      # 
      # 
      # 
      #             geom_text(data=datsf[, .SD[which.max(tyy)]], aes(x=year-3, y=max(tyy)+9, label=paste0("Year ",year, " values:")),
      #                       color=teksvari, hjust=1, size=si(2.2), fontface="bold",  family = fam)
      # 
      # 
      #         }  else if (rv$lang=="fin") {
      #           plot2 = plot2 +
      # 
      # 
      #             geom_text(data=datsf[, .SD[which.max(tyy)]], aes(x=year-3, y=max(tyy)+9, label=paste0("Vuoden ",year, " arvot:")),
      #                       color=teksvari, hjust=1, size=si(2.2), fontface="bold",  family = fam)
      #         }
      # 
      #       }
      # 
      #       gplot2 = ggplotGrob(plot2)
      #       li2 = list(gplot2)
      # 
      #       # plotlist = c(li2)
      # 
      #       if (exists("plotlist")) {
      #         plotlist = c(plotlist,li2)
      # 
      #       } else {        plotlist = c(li2)
      # 
      #       }
      # 
      # 
      #     }
      # 
      # 
      # 
      # 
      #     if (rv$plot3 == "plot3")
      #     {
      # 
      #       inplot= c("pop", "dummy", "countrypop")
      # 
      #       datsl = datsl()[sec %in% inplot & year >= mil,]
      #       datsss = datsss()[sec %in% inplot & year >= mil,]
      #       datsc =datsc()[sec %in% inplot,]
      #       datsf =datsf()[sec %in% inplot,]
      # 
      # 
      # 
      #       mi = min(min((datsss[,tyy]), na.rm=T)*1.1,-20)
      #       hi = ma-mi
      #       
      #       plot3=   ggplot(datsf)+
      # 
      # 
      #         # graph limits
      #         geom_segment(data=da,
      #                      aes(x=mil, xend=2100, y=0, yend=0),
      #                      color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=0) +
      # 
      #         geom_point(data=da,
      #                    aes(x=2140,  y=100),
      #                    color=teksvari, alpha=0, size=si(2))+
      #         #bottom years
      #         geom_text(data = datsss[year %in% c(seq(mil, 2100, 10)) & sec =="dummy",],
      #                   aes(x=year, y=.76*mi, label=c(year)),
      #                   color=teksvari, angle=0,size =si(2.4), alpha=.6) +
      # 
      #         # vertical lines
      #         geom_segment(data = datsss[year %in% seq(mil, 2100, 10) & sec =="dummy",],
      #                      (aes(x=year, xend = year, y=100, yend=.72*mi)),
      #                      color=teksvari, linetype="dashed", linewidth=lsi(.4), alpha=.08) +
      # 
      #         geom_text(data=da,
      #                   aes(x=mil, y=-4), label = paste0("0"),
      #                   col=teksvari, fontface="bold" ,  size =si(2.3), hjust =0, vjust=0, angle=c(0)) +
      #         # horizontal lines
      #         geom_text(data = datsss[, .SD[which.max(yy)]],
      #                   aes(x=mix+(max-mix)*.5, y=138, color=col, label = labbi),  size = si(4), fontface="bold") +
      # 
      #         geom_segment(data=datsc[sec=="dummy",],
      #                      aes(x=mil, xend=rv$lyear, y=0, yend=0),
      #                      color=teksvari, linetype ="dashed",linewidth=lsi(seg)*1.5, alpha=.3) +
      # 
      #         geom_segment(data=datsc[sec=="dummy",],
      #                      aes(x=mil, xend=rv$lyear, y=100, yend=100),
      #                      color=teksvari, linetype ="dashed",linewidth=lsi(seg)*1.5, alpha=.3) +
      #         geom_segment(data=datsc[sec=="dummy",],
      #                      aes(x=mil, xend=rv$lyear, y=75, yend=75),
      #                      color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=.2) +
      #         geom_segment(data=datsc[sec=="dummy",],
      #                      aes(x=mil, xend=rv$lyear, y=50, yend=50),
      #                      color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=.2) +
      #         geom_segment(data=datsc[sec=="dummy",],
      #                      aes(x=mil, xend=rv$lyear, y=25, yend=25),
      #                      color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=.2) +
      # 
      #         geom_point(data=da, aes(x=2030, y=124), alpha=0) +
      # 
      #         # blue line for yearc
      #         geom_segment(data=da,
      #                      aes(x=rv$yearc, xend=rv$yearc, y=123, yend = mi),
      #                      color=blu, alpha=.4, linewidth=lsi(1.4))+
      # 
      #         geom_point(data=datsc,
      #                    aes(y=tyy, x=year, group=interaction(sec, country),  alpha=ala),
      #                    size=lsi(points*2), color=blu) +
      # 
      #         # graphs for fyear to yearc
      #         geom_area(data=datsl[year < rv$fyear+1,],
      #                   aes(y=tyy, x=year, group=interaction(sec, country),  fill=col),
      #                   size=si(points), alpha=.15/nrow(datsc), position = 'identity') +
      #         geom_area(data=datsl[year > rv$fyear-1,],
      #                   aes(y=tyy, x=year, group=interaction(sec, country),  fill=col),
      #                   size=si(points), alpha=.35/nrow(datsc), position = 'identity') +
      # 
      #         geom_line(data=datsl,
      #                   aes(y=tyy, x=year, group=interaction(sec, country), color=col, alpha=ala),
      #                   linewidth=lsi(lines)) +
      #         geom_point(data=datsl,
      #                    aes(y=tyy, x=year, group=interaction(sec, country), color=col,  alpha=ala),
      #                    size=lsi(points)) +
      # 
      # 
      #         # graphs for first year to lyear
      #         geom_line(data=datsss,
      #                   aes(y=tyy, x=year, group=interaction(sec, country), color=col),
      #                   linewidth=lsi(lines), alpha=.1) +
      #         geom_point(data=datsss,
      #                    aes(y=tyy, x=year, group=interaction(sec, country), color=col, alpha=ala),
      #                    size=lsi(points), alpha=.1) +
      # 
      # 
      #         # pricing and neutrality vertical lines
      #         geom_segment(data=da,
      #                      aes(x=rv$fyear, xend=rv$fyear, y=110, yend = mi),
      #                      color=teksvari, linewidth=lsi(.4), linetype = "dashed", alpha=.4 ) +
      #         geom_segment(data=da,
      #                      aes(x=rv$lyear, xend=rv$lyear, y=110, yend = mi),
      #                      color=teksvari, linewidth=lsi(.4), linetype = "dashed", alpha=.4 ) +
      # 
      #         # observed and simulated vertical line
      #         geom_segment(data=da,
      #                      aes(x=2021.5, y=100, xend=2021.5, yend=mi),
      #                      color= obsvari,linewidth=lsi(.6), linetype="dashed", alpha=.2) +
      # 
      #         geom_text(data=da,
      #                   aes(x=mil, y=103), label = paste0("Max"),
      #                   col=teksvari, fontface="bold" ,  size =si(2.2), hjust =0, vjust=0.5, angle=c(0)) +
      # 
      #         geom_text(data=datsss[, .SD[which.max(yy)]],
      #                   aes(x=mil, y=103-6*1, label=paste0(round(max(yy, na.rm=TRUE), le), "",mark), color=col),
      #                   size=si(2.2), hjust=0, fontface="bold") +
      #         # yearc labels
      #         # geom_text(data=da,
      #         #           aes(x=mil-5, y=-10), label = paste0("www.globalcarbonprice.com \nData: UN, IPCC, Friedlingstein et al. 2022"),
      #         #           col=teksvari, fontface="bold" ,  size =si(1.2), hjust =0, vjust=0.5, angle=c(0), alpha=.5)+
      # 
      #         geom_label_repel(data=datsc,
      #                          aes(x=year+3, y=tyy,
      #                              label=labu,
      #                              color=col, alpha=ala*100),
      #                          fill=bgc,
      #                          hjust=0, size=si(labsize), fontface="bold",
      #                          family = fam,
      #                          segment.size =NA,
      #                          direction = "y",
      #                          label.padding =0,
      #                          xlim=c(mil,2177),
      #                          label.size=0,
      #                          max.iter=5000,
      #                          force=.01, force_pull=10,box.padding=.1 ,
      #                          seed=5) +
      # 
      # 
      # 
      #         scale_color_identity() +
      #         scale_alpha_identity() +
      # 
      #         scale_fill_identity() +
      # 
      #         scale_x_continuous(breaks = seq(mil, 2100, 10)) +
      #         coord_cartesian( ylim=c(mi, ma), xlim = c(mix, max),clip ="off") +
      # 
      # 
      #         theme(
      #           axis.title.x=element_blank(),
      #           plot.title=element_text(lineheight = 0.8, hjust=0.5, size=33, family="Alegreya Sans Bold"),
      #           plot.subtitle=element_text(lineheight = 0.8, size=20, hjust=0.5),
      #           axis.text.x=element_blank(),
      #           plot.margin = margin(-5,0,-5,0),
      #           panel.border = element_blank(),
      #           panel.grid.major = element_blank(),
      #           panel.grid.minor = element_blank(),
      #           axis.title.y=element_blank(),
      #           axis.text.y=element_blank(),
      #           plot.background = element_rect(fill =bgc, color=NA),
      #           panel.background = element_rect(fill = bgc ,  color="grey")
      #         )
      # 
      # 
      # 
      #       if (rv$lang =="eng") {
      # 
      #         plot3 = plot3 +
      #           geom_text(data=da,
      #                     aes(x=rv$fyear, y=116), label =paste0("Pricing starts: ", rv$fyear),
      #                     color=teksvari, hjust=.5, size=si(2.2), fontface="bold") +
      # 
      #           geom_text(data=da,
      #                     aes(x=rv$lyear, y=116), label =paste0("Neutrality: ", rv$lyear),
      #                     color=teksvari, hjust=.5, size=si(2.2), fontface="bold") +
      # 
      #           geom_text(data=datsc[, .SD[which.max(tyy)]],
      #                     aes(x=year+2, y=max(tyy)+9, label=paste0("Year ",year, " values:")),
      #                     color=teksvari, hjust=0, size=si(2.2), fontface="bold",  family = fam) +
      # 
      # 
      # 
      #           geom_text(data=da,
      #                     aes(x=2020.5, y=.3*mi), label = paste0("Historical\n <=2021"),
      #                     col= obsvari, fontface="bold" ,  size =si(1.9), hjust =1, vjust=0.5, angle=c(0),
      #                     alpha=.5, lineheight=.99) +
      # 
      #           geom_text(data=da,
      #                     aes(x=2022.5, y=.3*mi), label = paste0("Simulated\n2022=>"),
      #                     col= obsvari, fontface="bold" ,  size =si(1.9), hjust =0, vjust=0.5,
      #                     angle=c(0), alpha=.5, lineheight=.99)
      # 
      # 
      #       }
      # 
      #       if (rv$lang == "fin") {
      # 
      #         plot3 = plot3 +
      #           geom_text(data=da,
      #                     aes(x=rv$fyear, y=116), label =paste0("Hinnoittelu alkaa: ", rv$fyear),
      #                     color=teksvari, hjust=.5, size=si(2.2), fontface="bold") +
      # 
      #           geom_text(data=da,
      #                     aes(x=rv$lyear, y=116), label =paste0("Neutraalius: ", rv$lyear),
      #                     color=teksvari, hjust=.5, size=si(2.2), fontface="bold") +
      #           geom_text(data=datsc[, .SD[which.max(tyy)]],
      #                     aes(x=year+2, y=max(tyy)+9, label=paste0("Vuoden ",year, " arvot:")),
      #                     color=teksvari, hjust=0, size=si(2.2), fontface="bold",  family = fam) +
      # 
      #           geom_text(data=da,
      #                     aes(x=2020.5, y=-10), label = paste0("Havainnoitu\n <=2021"),
      #                     col= obsvari, fontface="bold" ,  size =si(1.9), hjust =1, vjust=0.5, angle=c(0),
      #                     alpha=.5, lineheight=.99) +
      # 
      #           geom_text(data=da,
      #                     aes(x=2022.5, y=-10), label = paste0("Simuloitu\n2022=>"),
      #                     col= obsvari, fontface="bold" ,  size =si(1.9), hjust =0, vjust=0.5,
      #                     angle=c(0), alpha=.5, lineheight=.99)
      # 
      # 
      # 
      #       }
      # 
      # 
      #       if (input$startvalue ==TRUE & rv$yearc > rv$fyear) {
      #         plot3 = plot3 +
      # 
      #           geom_label_repel(data=datsf,
      #                            aes(x=year-2, y=tyy, label=paste0(labuf),
      #                                color=col, group =sec, alpha=ala),
      #                            size=si(labsize), fontface="bold", hjust=1,
      #                            family = fam,
      #                            fill=bgc,
      #                            # segment.size =NA,
      #                            segment.size =NA,
      #                            direction = "y",
      #                            label.padding =0,
      #                            # box.padding=.1,
      #                            # nudge_x=18,
      #                            xlim=c(mil,2177),
      #                            label.size=0,
      #                            max.iter=5000,
      #                            force=.01, force_pull=10,box.padding=.1 ,
      #                            seed=5
      #           )
      # 
      #         if (rv$lang =="eng") {
      #           plot3 = plot3 +
      # 
      # 
      # 
      #             geom_text(data=datsf[, .SD[which.max(tyy)]], aes(x=year-3, y=max(tyy)+9, label=paste0("Year ",year, " values:")),
      #                       color=teksvari, hjust=1, size=si(2.2), fontface="bold",  family = fam)
      # 
      # 
      #         }  else if (rv$lang=="fin") {
      #           plot3 = plot3 +
      # 
      # 
      #             geom_text(data=datsf[, .SD[which.max(tyy)]], aes(x=year-3, y=max(tyy)+9, label=paste0("Vuoden ",year, " arvot:")),
      #                       color=teksvari, hjust=1, size=si(2.2), fontface="bold",  family = fam)
      #         }
      # 
      #       }
      # 
      #       gplot3 = ggplotGrob(plot3)
      #       li3 = list(gplot3)
      # 
      #       if (exists("plotlist")) {
      #         plotlist = c(plotlist,li3)
      # 
      #       } else {        plotlist = c(li3)
      # 
      #       }
      # 
      # 
      #     }
      # 
      # 
      # 
      # 
      # 
      # 
      #     if (rv$plot4 == "plot4")
      #     {
      # 
      #       inplot= c("avgfossil", "userfossil", "dummy", "countryfossil")
      # 
      #       datsl = datsl()[sec %in% inplot & year >= mil,]
      #       datsss = datsss()[sec %in% inplot & year >= mil,]
      #       datsc =datsc()[sec %in% inplot,]
      #       datsf =datsf()[sec %in% inplot,]
      # 
      # 
      # 
      #       mi = min(min((datsss[,tyy]), na.rm=T)*1.1,-20)
      #       hi = ma-mi
      #       
      #       plot4=   ggplot(datsf)+
      # 
      # 
      #         # graph limits
      #         geom_segment(data=da,
      #                      aes(x=mil, xend=2100, y=0, yend=0),
      #                      color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=0) +
      # 
      #         geom_point(data=da,
      #                    aes(x=2140,  y=100),
      #                    color=teksvari, alpha=0, size=si(2))+
      #         #bottom years
      #         geom_text(data = datsss[year %in% c(seq(mil, 2100, 10)) & sec =="dummy",],
      #                   aes(x=year, y=.76*mi, label=c(year)),
      #                   color=teksvari, angle=0,size =si(2.4), alpha=.6) +
      # 
      #         # vertical lines
      #         geom_segment(data = datsss[year %in% seq(mil, 2100, 10) & sec =="dummy",],
      #                      (aes(x=year, xend = year, y=100, yend=.72*mi)),
      #                      color=teksvari, linetype="dashed", linewidth=lsi(.4), alpha=.08) +
      # 
      #         geom_text(data=da,
      #                   aes(x=mil, y=-4), label = paste0("0"),
      #                   col=teksvari, fontface="bold" ,  size =si(2.3), hjust =0, vjust=0, angle=c(0)) +
      #         # horizontal lines
      #         geom_text(data = datsss[, .SD[which.max(yy)]],
      #                   aes(x=mix+(max-mix)*.5, y=138, color=col, label = labbi),  size = si(4), fontface="bold") +
      # 
      #         geom_segment(data=datsc[sec=="dummy",],
      #                      aes(x=mil, xend=rv$lyear, y=0, yend=0),
      #                      color=teksvari, linetype ="dashed",linewidth=lsi(seg)*1.5, alpha=.3) +
      # 
      #         geom_segment(data=datsc[sec=="dummy",],
      #                      aes(x=mil, xend=rv$lyear, y=100, yend=100),
      #                      color=teksvari, linetype ="dashed",linewidth=lsi(seg)*1.5, alpha=.3) +
      #         geom_segment(data=datsc[sec=="dummy",],
      #                      aes(x=mil, xend=rv$lyear, y=75, yend=75),
      #                      color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=.2) +
      #         geom_segment(data=datsc[sec=="dummy",],
      #                      aes(x=mil, xend=rv$lyear, y=50, yend=50),
      #                      color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=.2) +
      #         geom_segment(data=datsc[sec=="dummy",],
      #                      aes(x=mil, xend=rv$lyear, y=25, yend=25),
      #                      color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=.2) +
      # 
      #         geom_point(data=da, aes(x=2030, y=124), alpha=0) +
      # 
      #         # blue line for yearc
      #         geom_segment(data=da,
      #                      aes(x=rv$yearc, xend=rv$yearc, y=123, yend = mi),
      #                      color=blu, alpha=.4, linewidth=lsi(1.4))+
      # 
      #         geom_point(data=datsc,
      #                    aes(y=tyy, x=year, group=interaction(sec, country),  alpha=ala),
      #                    size=lsi(points*2), color=blu) +
      # 
      #         # graphs for fyear to yearc
      #         geom_area(data=datsl[year < rv$fyear+1,],
      #                   aes(y=tyy, x=year, group=interaction(sec, country),  fill=col),
      #                   size=si(points), alpha=.15/nrow(datsc), position = 'identity') +
      #         geom_area(data=datsl[year > rv$fyear-1,],
      #                   aes(y=tyy, x=year, group=interaction(sec, country),  fill=col),
      #                   size=si(points), alpha=.35/nrow(datsc), position = 'identity') +
      # 
      #         geom_line(data=datsl,
      #                   aes(y=tyy, x=year, group=interaction(sec, country), color=col, alpha=ala),
      #                   linewidth=lsi(lines)) +
      #         geom_point(data=datsl,
      #                    aes(y=tyy, x=year, group=interaction(sec, country), color=col,  alpha=ala),
      #                    size=lsi(points)) +
      # 
      # 
      #         # graphs for first year to lyear
      #         geom_line(data=datsss,
      #                   aes(y=tyy, x=year, group=interaction(sec, country), color=col),
      #                   linewidth=lsi(lines), alpha=.1) +
      #         geom_point(data=datsss,
      #                    aes(y=tyy, x=year, group=interaction(sec, country), color=col, alpha=ala),
      #                    size=lsi(points), alpha=.1) +
      # 
      # 
      #         # pricing and neutrality vertical lines
      #         geom_segment(data=da,
      #                      aes(x=rv$fyear, xend=rv$fyear, y=110, yend = mi),
      #                      color=teksvari, linewidth=lsi(.4), linetype = "dashed", alpha=.4 ) +
      #         geom_segment(data=da,
      #                      aes(x=rv$lyear, xend=rv$lyear, y=110, yend = mi),
      #                      color=teksvari, linewidth=lsi(.4), linetype = "dashed", alpha=.4 ) +
      # 
      #         # observed and simulated vertical line
      #         geom_segment(data=da,
      #                      aes(x=2021.5, y=100, xend=2021.5, yend=mi),
      #                      color= obsvari,linewidth=lsi(.6), linetype="dashed", alpha=.2) +
      # 
      #         geom_text(data=da,
      #                   aes(x=mil, y=103), label = paste0("Max"),
      #                   col=teksvari, fontface="bold" ,  size =si(2.2), hjust =0, vjust=0.5, angle=c(0)) +
      # 
      #         geom_text(data=datsss[, .SD[which.max(yy)]],
      #                   aes(x=mil, y=103-6*1, label=paste0(round(max(yy, na.rm=TRUE), le), "",mark), color=col),
      #                   size=si(2.2), hjust=0, fontface="bold") +
      #         # yearc labels
      #         # geom_text(data=da,
      #         #           aes(x=mil-5, y=-10), label = paste0("www.globalcarbonprice.com \nData: UN, IPCC, Friedlingstein et al. 2022"),
      #         #           col=teksvari, fontface="bold" ,  size =si(1.2), hjust =0, vjust=0.5, angle=c(0), alpha=.5)+
      # 
      #         geom_label_repel(data=datsc,
      #                          aes(x=year+3, y=tyy,
      #                              label=labu,
      #                              color=col, alpha=ala*100),
      #                          fill=bgc,
      #                          hjust=0, size=si(labsize), fontface="bold",
      #                          family = fam,
      #                          segment.size =NA,
      #                          direction = "y",
      #                          label.padding =0,
      #                          xlim=c(mil,2177),
      #                          label.size=0,
      #                          max.iter=5000,
      #                          force=.01, force_pull=10,box.padding=.1 ,
      #                          seed=5) +
      # 
      # 
      # 
      #         scale_color_identity() +
      #         scale_alpha_identity() +
      # 
      #         scale_fill_identity() +
      # 
      #         scale_x_continuous(breaks = seq(mil, 2100, 10)) +
      #         coord_cartesian( ylim=c(mi, ma), xlim = c(mix, max),clip ="off") +
      # 
      # 
      #         theme(
      #           axis.title.x=element_blank(),
      #           plot.title=element_text(lineheight = 0.8, hjust=0.5, size=33, family="Alegreya Sans Bold"),
      #           plot.subtitle=element_text(lineheight = 0.8, size=20, hjust=0.5),
      #           axis.text.x=element_blank(),
      #           plot.margin = margin(-5,0,-5,0),
      #           panel.border = element_blank(),
      #           panel.grid.major = element_blank(),
      #           panel.grid.minor = element_blank(),
      #           axis.title.y=element_blank(),
      #           axis.text.y=element_blank(),
      #           plot.background = element_rect(fill =bgc, color=NA),
      #           panel.background = element_rect(fill = bgc ,  color="grey")
      #         )
      # 
      # 
      # 
      #       if (rv$lang =="eng") {
      # 
      #         plot4 = plot4 +
      #           geom_text(data=da,
      #                     aes(x=rv$fyear, y=116), label =paste0("Pricing starts: ", rv$fyear),
      #                     color=teksvari, hjust=.5, size=si(2.2), fontface="bold") +
      # 
      #           geom_text(data=da,
      #                     aes(x=rv$lyear, y=116), label =paste0("Neutrality: ", rv$lyear),
      #                     color=teksvari, hjust=.5, size=si(2.2), fontface="bold") +
      # 
      #           geom_text(data=datsc[, .SD[which.max(tyy)]],
      #                     aes(x=year+2, y=max(tyy)+9, label=paste0("Year ",year, " values:")),
      #                     color=teksvari, hjust=0, size=si(2.2), fontface="bold",  family = fam) +
      # 
      # 
      # 
      #           geom_text(data=da,
      #                     aes(x=2020.5, y=.3*mi), label = paste0("Historical\n <=2021"),
      #                     col= obsvari, fontface="bold" ,  size =si(1.9), hjust =1, vjust=0.5, angle=c(0),
      #                     alpha=.5, lineheight=.99) +
      # 
      #           geom_text(data=da,
      #                     aes(x=2022.5, y=.3*mi), label = paste0("Simulated\n2022=>"),
      #                     col= obsvari, fontface="bold" ,  size =si(1.9), hjust =0, vjust=0.5,
      #                     angle=c(0), alpha=.5, lineheight=.99) +
      # 
      #           geom_text(data=da, aes(x=rv$lyear, y=122),
      #                     label = paste0(rv$lyear, " mean emissions: ", format(round(rv$fossil, 1), nsmall=1),"/",format(round(rv$pop, 1), nsmall=1),
      #                                    " = ",format(round(rv$avgfossil, 1), nsmall=1)),
      #                     hjust=1, color=fpop, size= 2.1 )
      # 
      #         # , format(round(yy,le), decimal.mark=",", nsmall=le
      #       }
      # 
      #       if (rv$lang == "fin") {
      # 
      #         plot4 = plot4 +
      #           geom_text(data=da,
      #                     aes(x=rv$fyear, y=116), label =paste0("Hinnoittelu alkaa: ", rv$fyear),
      #                     color=teksvari, hjust=.5, size=si(2.2), fontface="bold") +
      # 
      #           geom_text(data=da,
      #                     aes(x=rv$lyear, y=116), label =paste0("Neutraalius: ", rv$lyear),
      #                     color=teksvari, hjust=.5, size=si(2.2), fontface="bold") +
      #           geom_text(data=datsc[, .SD[which.max(tyy)]],
      #                     aes(x=year+2, y=max(tyy)+9, label=paste0("Vuoden ",year, " arvot:")),
      #                     color=teksvari, hjust=0, size=si(2.2), fontface="bold",  family = fam) +
      # 
      #           geom_text(data=da,
      #                     aes(x=2020.5, y=-10), label = paste0("Havainnoitu\n <=2021"),
      #                     col= obsvari, fontface="bold" ,  size =si(1.9), hjust =1, vjust=0.5, angle=c(0),
      #                     alpha=.5, lineheight=.99) +
      # 
      #           geom_text(data=da,
      #                     aes(x=2022.5, y=-10), label = paste0("Simuloitu\n2022=>"),
      #                     col= obsvari, fontface="bold" ,  size =si(1.9), hjust =0, vjust=0.5,
      #                     angle=c(0), alpha=.5, lineheight=.99)
      # 
      # 
      # 
      #       }
      # 
      # 
      #       if (input$startvalue ==TRUE & rv$yearc > rv$fyear) {
      #         plot4 = plot4 +
      # 
      #           geom_label_repel(data=datsf,
      #                            aes(x=year-2, y=tyy, label=paste0(labuf),
      #                                color=col, group =sec, alpha=ala),
      #                            size=si(labsize), fontface="bold", hjust=1,
      #                            family = fam,
      #                            fill=bgc,
      #                            # segment.size =NA,
      #                            segment.size =NA,
      #                            direction = "y",
      #                            label.padding =0,
      #                            # box.padding=.1,
      #                            # nudge_x=18,
      #                            xlim=c(mil,2177),
      #                            label.size=0,
      #                            max.iter=5000,
      #                            force=.01, force_pull=10,box.padding=.1 ,
      #                            seed=5
      #           )
      # 
      #         if (rv$lang =="eng") {
      #           plot4 = plot4 +
      # 
      # 
      # 
      #             geom_text(data=datsf[, .SD[which.max(tyy)]], aes(x=year-3, y=max(tyy)+9, label=paste0("Year ",year, " values:")),
      #                       color=teksvari, hjust=1, size=si(2.2), fontface="bold",  family = fam)
      # 
      # 
      #         }  else if (rv$lang=="fin") {
      #           plot4 = plot4 +
      # 
      # 
      #             geom_text(data=datsf[, .SD[which.max(tyy)]], aes(x=year-3, y=max(tyy)+9, label=paste0("Vuoden ",year, " arvot:")),
      #                       color=teksvari, hjust=1, size=si(2.2), fontface="bold",  family = fam)
      #         }
      # 
      #       }
      # 
      #       gplot4 = ggplotGrob(plot4)
      #       li4 = list(gplot4)
      # 
      #       if (exists("plotlist")) {
      #         plotlist = c(plotlist,li4)
      # 
      #       } else {        plotlist = c(li4)
      # 
      #       }
      #     }
      # 
      # 
      # 
      # 
      #     if (rv$plot5 == "plot5")
      #     {
      # 
      #       inplot= c("price", "dummy")
      # 
      #       datsl = datsl()[sec %in% inplot & year >= mil,]
      #       datsss = datsss()[sec %in% inplot & year >= mil,]
      #       datsc =datsc()[sec %in% inplot,]
      #       datsf =datsf()[sec %in% inplot,]
      # 
      # 
      # 
      #       mi = min(min((datsss[,tyy]), na.rm=T)*1.1,-20)
      #       hi = ma-mi
      #       
      #       plot5=   ggplot(datsf)+
      # 
      # 
      #         # graph limits
      #         geom_segment(data=da,
      #                      aes(x=mil, xend=2100, y=0, yend=0),
      #                      color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=0) +
      # 
      #         geom_point(data=da,
      #                    aes(x=2140,  y=100),
      #                    color=teksvari, alpha=0, size=si(2))+
      #         #bottom years
      #         geom_text(data = datsss[year %in% c(seq(mil, 2100, 10)) & sec =="dummy",],
      #                   aes(x=year, y=.76*mi, label=c(year)),
      #                   color=teksvari, angle=0,size =si(2.4), alpha=.6) +
      # 
      #         # vertical lines
      #         geom_segment(data = datsss[year %in% seq(mil, 2100, 10) & sec =="dummy",],
      #                      (aes(x=year, xend = year, y=100, yend=.72*mi)),
      #                      color=teksvari, linetype="dashed", linewidth=lsi(.4), alpha=.08) +
      # 
      #         geom_text(data=da,
      #                   aes(x=mil, y=-4), label = paste0("0"),
      #                   col=teksvari, fontface="bold" ,  size =si(2.3), hjust =0, vjust=0, angle=c(0)) +
      #         # horizontal lines
      #         geom_text(data = datsss[, .SD[which.max(yy)]],
      #                   aes(x=mix+(max-mix)*.5, y=138, color=col, label = labbi),  size = si(4), fontface="bold") +
      # 
      #         geom_segment(data=datsc[sec=="dummy",],
      #                      aes(x=mil, xend=rv$lyear, y=0, yend=0),
      #                      color=teksvari, linetype ="dashed",linewidth=lsi(seg)*1.5, alpha=.3) +
      # 
      #         geom_segment(data=datsc[sec=="dummy",],
      #                      aes(x=mil, xend=rv$lyear, y=100, yend=100),
      #                      color=teksvari, linetype ="dashed",linewidth=lsi(seg)*1.5, alpha=.3) +
      #         geom_segment(data=datsc[sec=="dummy",],
      #                      aes(x=mil, xend=rv$lyear, y=75, yend=75),
      #                      color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=.2) +
      #         geom_segment(data=datsc[sec=="dummy",],
      #                      aes(x=mil, xend=rv$lyear, y=50, yend=50),
      #                      color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=.2) +
      #         geom_segment(data=datsc[sec=="dummy",],
      #                      aes(x=mil, xend=rv$lyear, y=25, yend=25),
      #                      color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=.2) +
      # 
      #         geom_point(data=da, aes(x=2030, y=124), alpha=0) +
      # 
      #         # blue line for yearc
      #         geom_segment(data=da,
      #                      aes(x=rv$yearc, xend=rv$yearc, y=123, yend = mi),
      #                      color=blu, alpha=.4, linewidth=lsi(1.4))+
      # 
      #         geom_point(data=datsc,
      #                    aes(y=tyy, x=year, group=sec,  alpha=ala),
      #                    size=lsi(points*2), color=blu) +
      # 
      #         # graphs for fyear to yearc
      # 
      # 
      #         geom_area(data=datsl[year < rv$fyear+1,],
      #                   aes(y=tyy, x=year, group=sec,  fill=col),
      #                   size=si(points), alpha=.15/nrow(datsc), position = 'identity') +
      #         geom_area(data=datsl[year > rv$fyear-1,],
      #                   aes(y=tyy, x=year, group=sec,  fill=col),
      #                   size=si(points), alpha=.35/nrow(datsc), position = 'identity') +
      # 
      #         geom_line(data=datsl,
      #                   aes(y=tyy, x=year, group=sec, color=col, alpha=ala),
      #                   linewidth=lsi(lines)) +
      #         geom_point(data=datsl,
      #                    aes(y=tyy, x=year, group=sec, color=col,  alpha=ala),
      #                    size=lsi(points)) +
      # 
      # 
      #         # graphs for first year to lyear
      #         geom_line(data=datsss,
      #                   aes(y=tyy, x=year, group=interaction(sec, country), color=col),
      #                   linewidth=lsi(lines), alpha=.1) +
      #         geom_point(data=datsss,
      #                    aes(y=tyy, x=year, group=sec, color=col, alpha=ala),
      #                    size=lsi(points), alpha=.1) +
      # 
      # 
      #         # pricing and neutrality vertical lines
      #         geom_segment(data=da,
      #                      aes(x=rv$fyear, xend=rv$fyear, y=110, yend = mi),
      #                      color=teksvari, linewidth=lsi(.4), linetype = "dashed", alpha=.4 ) +
      #         geom_segment(data=da,
      #                      aes(x=rv$lyear, xend=rv$lyear, y=110, yend = mi),
      #                      color=teksvari, linewidth=lsi(.4), linetype = "dashed", alpha=.4 ) +
      # 
      #         # observed and simulated vertical line
      #         geom_segment(data=da,
      #                      aes(x=2021.5, y=100, xend=2021.5, yend=mi),
      #                      color= obsvari,linewidth=lsi(.6), linetype="dashed", alpha=.2) +
      # 
      #         geom_text(data=da,
      #                   aes(x=mil, y=103), label = paste0("Max"),
      #                   col=teksvari, fontface="bold" ,  size =si(2.2), hjust =0, vjust=0.5, angle=c(0)) +
      # 
      #         geom_text(data=datsss[, .SD[which.max(yy)]],
      #                   aes(x=mil, y=103-6*1, label=paste0(round(max(yy, na.rm=TRUE), le), "",mark), color=col),
      #                   size=si(2.2), hjust=0, fontface="bold") +
      #         # yearc labels
      #         # geom_text(data=da,
      #         #           aes(x=mil-5, y=-10), label = paste0("www.globalcarbonprice.com \nData: UN, IPCC, Friedlingstein et al. 2022"),
      #         #           col=teksvari, fontface="bold" ,  size =si(1.2), hjust =0, vjust=0.5, angle=c(0), alpha=.5)+
      # 
      #         geom_label_repel(data=datsc,
      #                          aes(x=year+3, y=tyy,
      #                              label=labu,
      #                              color=col, alpha=ala*100),
      #                          fill=bgc,
      #                          hjust=0, size=si(labsize), fontface="bold",
      #                          family = fam,
      #                          segment.size =NA,
      #                          direction = "y",
      #                          label.padding =0,
      #                          xlim=c(mil,2177),
      #                          label.size=0,
      #                          max.iter=5000,
      #                          force=.01, force_pull=10,box.padding=.1 ,
      #                          seed=5) +
      # 
      # 
      # 
      #         scale_color_identity() +
      #         scale_alpha_identity() +
      # 
      #         scale_fill_identity() +
      # 
      #         scale_x_continuous(breaks = seq(mil, 2100, 10)) +
      #         coord_cartesian( ylim=c(mi, ma), xlim = c(mix, max),clip ="off") +
      # 
      # 
      #         theme(
      #           axis.title.x=element_blank(),
      #           plot.title=element_text(lineheight = 0.8, hjust=0.5, size=33, family="Alegreya Sans Bold"),
      #           plot.subtitle=element_text(lineheight = 0.8, size=20, hjust=0.5),
      #           axis.text.x=element_blank(),
      #           plot.margin = margin(-5,0,-5,0),
      #           panel.border = element_blank(),
      #           panel.grid.major = element_blank(),
      #           panel.grid.minor = element_blank(),
      #           axis.title.y=element_blank(),
      #           axis.text.y=element_blank(),
      #           plot.background = element_rect(fill =bgc, color=NA),
      #           panel.background = element_rect(fill = bgc ,  color="grey")
      #         )
      # 
      # 
      # 
      #       if (rv$lang =="eng") {
      # 
      #         plot5 = plot5 +
      #           geom_text(data=da,
      #                     aes(x=rv$fyear, y=116), label =paste0("Pricing starts: ", rv$fyear),
      #                     color=teksvari, hjust=.5, size=si(2.2), fontface="bold") +
      # 
      #           geom_text(data=da,
      #                     aes(x=rv$lyear, y=116), label =paste0("Neutrality: ", rv$lyear),
      #                     color=teksvari, hjust=.5, size=si(2.2), fontface="bold") +
      # 
      #           geom_text(data=datsc[, .SD[which.max(tyy)]],
      #                     aes(x=year+2, y=max(tyy)+9, label=paste0("Year ",year, " values:")),
      #                     color=teksvari, hjust=0, size=si(2.2), fontface="bold",  family = fam) +
      # 
      # 
      # 
      #           geom_text(data=da,
      #                     aes(x=2020.5, y=.3*mi), label = paste0("Historical\n <=2021"),
      #                     col= obsvari, fontface="bold" ,  size =si(1.9), hjust =1, vjust=0.5, angle=c(0),
      #                     alpha=.5, lineheight=.99) +
      # 
      #           geom_text(data=da,
      #                     aes(x=2022.5, y=.3*mi), label = paste0("Simulated\n2022=>"),
      #                     col= obsvari, fontface="bold" ,  size =si(1.9), hjust =0, vjust=0.5,
      #                     angle=c(0), alpha=.5, lineheight=.99)
      # 
      # 
      #       }
      # 
      #       if (rv$lang == "fin") {
      # 
      #         plot5 = plot5 +
      #           geom_text(data=da,
      #                     aes(x=rv$fyear, y=116), label =paste0("Hinnoittelu alkaa: ", rv$fyear),
      #                     color=teksvari, hjust=.5, size=si(2.2), fontface="bold") +
      # 
      #           geom_text(data=da,
      #                     aes(x=rv$lyear, y=116), label =paste0("Neutraalius: ", rv$lyear),
      #                     color=teksvari, hjust=.5, size=si(2.2), fontface="bold") +
      #           geom_text(data=datsc[, .SD[which.max(tyy)]],
      #                     aes(x=year+2, y=max(tyy)+9, label=paste0("Vuoden ",year, " arvot:")),
      #                     color=teksvari, hjust=0, size=si(2.2), fontface="bold",  family = fam) +
      # 
      #           geom_text(data=da,
      #                     aes(x=2020.5, y=-10), label = paste0("Havainnoitu\n <=2021"),
      #                     col= obsvari, fontface="bold" ,  size =si(1.9), hjust =1, vjust=0.5, angle=c(0),
      #                     alpha=.5, lineheight=.99) +
      # 
      #           geom_text(data=da,
      #                     aes(x=2022.5, y=-10), label = paste0("Simuloitu\n2022=>"),
      #                     col= obsvari, fontface="bold" ,  size =si(1.9), hjust =0, vjust=0.5,
      #                     angle=c(0), alpha=.5, lineheight=.99)
      # 
      # 
      # 
      #       }
      # 
      # 
      #       if (input$startvalue ==TRUE & rv$yearc > rv$fyear) {
      #         plot5 = plot5 +
      # 
      #           geom_label_repel(data=datsf,
      #                            aes(x=year-2, y=tyy, label=paste0(labuf),
      #                                color=col, group =sec, alpha=ala),
      #                            size=si(labsize), fontface="bold", hjust=1,
      #                            family = fam,
      #                            fill=bgc,
      #                            # segment.size =NA,
      #                            segment.size =NA,
      #                            direction = "y",
      #                            label.padding =0,
      #                            # box.padding=.1,
      #                            # nudge_x=18,
      #                            xlim=c(mil,2177),
      #                            label.size=0,
      #                            max.iter=5000,
      #                            force=.01, force_pull=10,box.padding=.1 ,
      #                            seed=5
      #           )
      # 
      #         if (rv$lang =="eng") {
      #           plot5 = plot5 +
      # 
      # 
      # 
      #             geom_text(data=datsf[, .SD[which.max(tyy)]], aes(x=year-3, y=max(tyy)+9, label=paste0("Year ",year, " values:")),
      #                       color=teksvari, hjust=1, size=si(2.2), fontface="bold",  family = fam)
      # 
      # 
      #         }  else if (rv$lang=="fin") {
      #           plot5 = plot5 +
      # 
      # 
      #             geom_text(data=datsf[, .SD[which.max(tyy)]], aes(x=year-3, y=max(tyy)+9, label=paste0("Vuoden ",year, " arvot:")),
      #                       color=teksvari, hjust=1, size=si(2.2), fontface="bold",  family = fam)
      #         }
      # 
      #       }
      # 
      #       gplot5 = ggplotGrob(plot5)
      #       li5 = list(gplot5)
      # 
      # 
      #       if (exists("plotlist")) {
      #         plotlist = c(plotlist,li5)
      # 
      #       } else {        plotlist = c(li5)
      # 
      #       }
      # 
      # 
      #     }
      # 
      # 
      # 
      # 
      # 
      #     if (rv$plot6 == "plot6")
      #     {
      # 
      #       inplot= c( "averagedividend", "countrydividend", "avgcost", "netcost",
      #                  "usercost","dividend","avgnetcost","countrynetcost","countrycost", "dummy")
      # 
      #       datsl = datsl()[sec %in% inplot & year >= mil,]
      #       datsss = datsss()[sec %in% inplot & year >= mil,]
      #       datsc =datsc()[sec %in% inplot,]
      #       datsf =datsf()[sec %in% inplot,]
      #       
      # 
      # 
      #       mi = min(min((datsss[,tyy]), na.rm=T)*1.1,-20)
      #       hi = ma-mi
      #       
      #       plot6=   ggplot(datsf)+
      # 
      # 
      #         # graph limits
      #         geom_segment(data=da,
      #                      aes(x=mil, xend=2100, y=0, yend=0),
      #                      color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=0) +
      # 
      #         geom_point(data=da,
      #                    aes(x=2140,  y=100),
      #                    color=teksvari, alpha=0, size=si(2))+
      #         #bottom years
      #         geom_text(data = datsss[year %in% c(seq(mil, 2100, 10)) & sec =="dummy",],
      #                   aes(x=year, y=.76*mi, label=c(year)),
      #                   color=teksvari, angle=0,size =si(2.4), alpha=.6) +
      # 
      #         # vertical lines
      #         geom_segment(data = datsss[year %in% seq(mil, 2100, 10) & sec =="dummy",],
      #                      (aes(x=year, xend = year, y=100, yend=.72*mi)),
      #                      color=teksvari, linetype="dashed", linewidth=lsi(.4), alpha=.08) +
      # 
      #         geom_text(data=da,
      #                   aes(x=mil, y=-4), label = paste0("0"),
      #                   col=teksvari, fontface="bold" ,  size =si(2.3), hjust =0, vjust=0, angle=c(0)) +
      #         # horizontal lines
      #         geom_text(data = datsss[, .SD[which.max(yy)]],
      #                   aes(x=mix+(max-mix)*.5, y=138, color=col, label = labbi),  size = si(4), fontface="bold") +
      # 
      # 
      # 
      #         geom_segment(data=datsc[sec=="dummy",],
      #                      aes(x=mil, xend=rv$lyear, y=0, yend=0),
      #                      color=teksvari, linetype ="dashed",linewidth=lsi(seg)*1.5, alpha=.3) +
      # 
      #         geom_segment(data=datsc[sec=="dummy",],
      #                      aes(x=mil, xend=rv$lyear, y=100, yend=100),
      #                      color=teksvari, linetype ="dashed",linewidth=lsi(seg)*1.5, alpha=.3) +
      #         geom_segment(data=datsc[sec=="dummy",],
      #                      aes(x=mil, xend=rv$lyear, y=75, yend=75),
      #                      color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=.2) +
      #         geom_segment(data=datsc[sec=="dummy",],
      #                      aes(x=mil, xend=rv$lyear, y=50, yend=50),
      #                      color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=.2) +
      #         geom_segment(data=datsc[sec=="dummy",],
      #                      aes(x=mil, xend=rv$lyear, y=25, yend=25),
      #                      color=teksvari, linetype ="dashed",linewidth=lsi(seg), alpha=.2) +
      # 
      #         geom_point(data=da, aes(x=2030, y=124), alpha=0) +
      # 
      #         # blue line for yearc
      #         geom_segment(data=da,
      #                      aes(x=rv$yearc, xend=rv$yearc, y=123, yend = mi),
      #                      color=blu, alpha=.4, linewidth=lsi(1.4))+
      # 
      #         geom_point(data=datsc,
      #                    aes(y=tyy, x=year, group=interaction(sec, country),  alpha=ala),
      #                    size=lsi(points*2), color=blu) +
      # 
      #         # graphs for fyear to yearc
      #         geom_area(data=datsl[year < rv$fyear+1,],
      #                   aes(y=tyy, x=year, group=interaction(sec, country),  fill=col),
      #                   size=si(points), alpha=.15/nrow(datsc), position = 'identity') +
      #         geom_area(data=datsl[year > rv$fyear-1,],
      #                   aes(y=tyy, x=year, group=interaction(sec, country),  fill=col),
      #                   size=si(points), alpha=.35/nrow(datsc), position = 'identity') +
      # 
      #         geom_line(data=datsl,
      #                   aes(y=tyy, x=year, group=interaction(sec, country), color=col, alpha=ala),
      #                   linewidth=lsi(lines)) +
      #         geom_point(data=datsl,
      #                    aes(y=tyy, x=year, group=interaction(sec, country), color=col,  alpha=ala),
      #                    size=lsi(points)) +
      # 
      # 
      #         # graphs for first year to lyear
      #         geom_line(data=datsss,
      #                   aes(y=tyy, x=year, group=interaction(sec, country), color=col),
      #                   linewidth=lsi(lines), alpha=.1) +
      #         geom_point(data=datsss,
      #                    aes(y=tyy, x=year, group=interaction(sec, country), color=col, alpha=ala),
      #                    size=lsi(points), alpha=.1) +
      # 
      #         # pricing and neutrality vertical lines
      #         geom_segment(data=da,
      #                      aes(x=rv$fyear, xend=rv$fyear, y=110, yend = mi),
      #                      color=teksvari, linewidth=lsi(.4), linetype = "dashed", alpha=.4 ) +
      #         geom_segment(data=da,
      #                      aes(x=rv$lyear, xend=rv$lyear, y=110, yend = mi),
      #                      color=teksvari, linewidth=lsi(.4), linetype = "dashed", alpha=.4 ) +
      # 
      #         # observed and simulated vertical line
      #         geom_segment(data=da,
      #                      aes(x=2021.5, y=100, xend=2021.5, yend=mi),
      #                      color= obsvari,linewidth=lsi(.6), linetype="dashed", alpha=.2) +
      # 
      #         geom_text(data=da,
      #                   aes(x=mil, y=100+.03*hi), label = paste0("Max"),
      #                   col=teksvari, fontface="bold" ,  size =si(2.2), hjust =0, vjust=0.5, angle=c(0)) +
      # 
      #         geom_text(data=datsss[, .SD[which.max(yy)]],
      #                   aes(x=mil, y=100-.03*hi, label=paste0(round(max(yy, na.rm=TRUE), le), "",mark), color=col),
      #                   size=si(2.2), hjust=0, fontface="bold") +
      #         # yearc labels
      #         # geom_text(data=da,
      #         #           aes(x=mil-5, y=-10), label = paste0("www.globalcarbonprice.com \nData: UN, IPCC, Friedlingstein et al. 2022"),
      #         #           col=teksvari, fontface="bold" ,  size =si(1.2), hjust =0, vjust=0.5, angle=c(0), alpha=.5)+
      # 
      #         geom_label_repel(data=datsc,
      #                          aes(x=year+3, y=tyy,
      #                              label=labu,
      #                              color=col, alpha=ala*100),
      #                          fill=bgc,
      #                          hjust=0, size=si(2.5), fontface="bold",
      #                          family = fam,
      #                          segment.size =NA,
      #                          direction = "y",
      #                          label.padding =0,
      #                          xlim=c(mil,2177),
      #                          label.size=0,
      #                          max.iter=5000,
      #                          force=.01, force_pull=10,box.padding=.1 ,
      #                          seed=5) +
      # 
      # 
      # 
      #         scale_color_identity() +
      #         scale_alpha_identity() +
      # 
      #         scale_fill_identity() +
      # 
      #         scale_x_continuous(breaks = seq(mil, 2100, 10)) +
      #         coord_cartesian( ylim=c(mi, ma), xlim = c(mix, max),clip ="off") +
      # 
      # 
      #         theme(
      #           axis.title.x=element_blank(),
      #           plot.title=element_text(lineheight = 0.8, hjust=0.5, size=33, family="Alegreya Sans Bold"),
      #           plot.subtitle=element_text(lineheight = 0.8, size=20, hjust=0.5),
      #           axis.text.x=element_blank(),
      #           plot.margin = margin(-5,0,-5,0),
      #           panel.border = element_blank(),
      #           panel.grid.major = element_blank(),
      #           panel.grid.minor = element_blank(),
      #           axis.title.y=element_blank(),
      #           axis.text.y=element_blank(),
      #           plot.background = element_rect(fill =bgc, color=NA),
      #           panel.background = element_rect(fill = bgc ,  color="grey")
      #         )
      # 
      # 
      # 
      #       if (rv$lang =="eng") {
      # 
      #         plot6 = plot6 +
      #           geom_text(data=da,
      #                     aes(x=rv$fyear, y=116), label =paste0("Pricing starts: ", rv$fyear),
      #                     color=teksvari, hjust=.5, size=si(2.2), fontface="bold") +
      # 
      #           geom_text(data=da,
      #                     aes(x=rv$lyear, y=116), label =paste0("Neutrality: ", rv$lyear),
      #                     color=teksvari, hjust=.5, size=si(2.2), fontface="bold") +
      # 
      #           geom_text(data=datsc[, .SD[which.max(tyy)]],
      #                     aes(x=year+2, y=max(tyy)+10, label=paste0("Year ",year, " values:")),
      #                     color=teksvari, hjust=0, size=si(2.2), fontface="bold",  family = fam) +
      # 
      #           geom_text(data = datsss[, .SD[which.max(yy)]],
      #                     aes(x=mix+(max-mix)*.5, y=130), color=teksvari,
      #                     label = "(Does not include costs from land use change)",  size = si(2.3), fontface="bold") +
      # 
      # 
      #           geom_text(data=da,
      #                     aes(x=2020.5, y=.3*mi), label = paste0("Historical\n <=2021"),
      #                     col= obsvari, fontface="bold" ,  size =si(1.9), hjust =1, vjust=0.5, angle=c(0),
      #                     alpha=.5, lineheight=.99) +
      # 
      #           geom_text(data=da,
      #                     aes(x=2022.5, y=.3*mi), label = paste0("Simulated\n2022=>"),
      #                     col= obsvari, fontface="bold" ,  size =si(1.9), hjust =0, vjust=0.5,
      #                     angle=c(0), alpha=.5, lineheight=.99)
      # 
      # 
      #       }
      # 
      #       if (rv$lang == "fin") {
      # 
      #         plot6 = plot6 +
      #           geom_text(data=da,
      #                     aes(x=rv$fyear, y=116), label =paste0("Hinnoittelu alkaa: ", rv$fyear),
      #                     color=teksvari, hjust=.5, size=si(2.2), fontface="bold") +
      # 
      #           geom_text(data=da,
      #                     aes(x=rv$lyear, y=116), label =paste0("Neutraalius: ", rv$lyear),
      #                     color=teksvari, hjust=.5, size=si(2.2), fontface="bold") +
      #           geom_text(data=datsc[, .SD[which.max(tyy)]],
      #                     aes(x=year+2, y=max(tyy)+10, label=paste0("Vuoden ",year, " arvot:")),
      #                     color=teksvari, hjust=0, size=si(2.2), fontface="bold",  family = fam) +
      # 
      #           geom_text(data = datsss[, .SD[which.max(yy)]],
      #                     aes(x=mix+(max-mix)*.5, y=130), color=teksvari,
      #                     label = "(Ei sisällä kuluja maankäytön muutoksesta)",  size = si(2.3), fontface="bold") +
      # 
      # 
      #           geom_text(data=da,
      #                     aes(x=2020.5, y=-10), label = paste0("Havainnoitu\n <=2021"),
      #                     col= obsvari, fontface="bold" ,  size =si(1.9), hjust =1, vjust=0.5, angle=c(0),
      #                     alpha=.5, lineheight=.99) +
      # 
      #           geom_text(data=da,
      #                     aes(x=2022.5, y=-10), label = paste0("Simuloitu\n2022=>"),
      #                     col= obsvari, fontface="bold" ,  size =si(1.9), hjust =0, vjust=0.5,
      #                     angle=c(0), alpha=.5, lineheight=.99)
      # 
      # 
      # 
      #       }
      # 
      # 
      #       if (input$startvalue ==TRUE & rv$yearc > rv$fyear) {
      #         plot6 = plot6 +
      # 
      #           geom_label_repel(data=datsf,
      #                            aes(x=year-2, y=tyy, label=paste0(labuf),
      #                                color=col, group =sec, alpha=ala),
      #                            size=si(2.5), fontface="bold", hjust=1,
      #                            family = fam,
      #                            fill=bgc,
      #                            # segment.size =NA,
      #                            segment.size =NA,
      #                            direction = "y",
      #                            label.padding =0,
      #                            # box.padding=.1,
      #                            # nudge_x=18,
      #                            xlim=c(mil,2177),
      #                            label.size=0,
      #                            max.iter=5000,
      #                            force=.01, force_pull=10,box.padding=.1 ,
      #                            seed=5
      #           )
      # 
      #         if (rv$lang =="eng") {
      #           plot6 = plot6 +
      # 
      # 
      # 
      #             geom_text(data=datsf[, .SD[which.max(tyy)]], aes(x=year-3, y=max(tyy)+9, label=paste0("Year ",year, " values:")),
      #                       color=teksvari, hjust=1, size=si(2.2), fontface="bold",  family = fam)
      # 
      # 
      #         }  else if (rv$lang=="fin") {
      #           plot6 = plot6 +
      # 
      # 
      #             geom_text(data=datsf[, .SD[which.max(tyy)]], aes(x=year-3, y=max(tyy)+9, label=paste0("Vuoden ",year, " arvot:")),
      #                       color=teksvari, hjust=1, size=si(2.2), fontface="bold",  family = fam)
      #         }
      # 
      #       }
      # 
      #       gplot6 = ggplotGrob(plot6)
      #       li6 = list(gplot6)
      # 
      #       if (exists("plotlist")) {
      #         plotlist = c(plotlist,li6)
      # 
      #       } else {        plotlist = c(li6)
      # 
      #       }
      # 
      # 
      #     }
      # 
      # 
      # 
      # 
      # 
      #     eplot = ggplot(NULL, aes(color = ""))+
      #       geom_blank()+
      #       scale_color_manual(values = "black", labels = "Something")+
      #       guides(color = guide_legend())+
      #       theme(
      #         plot.background = element_rect(fill =bgc, color=NA),
      #         panel.background = element_rect(fill = bgc ,  color="grey")
      #       )
      #     geplot = ggplotGrob(eplot)
      #     lig = list(geplot)
      # 
      #     if (exists("plotlist")) {
      #       plotlist = plotlist
      # 
      #     } else {        plotlist = c(lig)
      # 
      #     }
      # 
      # 
      #     pll = length(plotlist)
      #     #
      #     count = 1
      #     # if (pll >= 3) {
      #     if (input$view==2) {
      # 
      #       if (pll==1){
      #         lay=rbind(c(1))
      #         plotlist = c(plotlist)
      # 
      #       } else if (pll ==2) {
      #         lay = rbind(c(1,2))
      #         plotlist = c(plotlist)
      #       } else if (pll==3) {
      #         lay = rbind(c(1,2), c(3,4))
      #         plotlist = c(plotlist, lig)
      # 
      #       } else if (pll==4) {
      #         lay = rbind(c(1,2), c(3,4))
      #       } else if (pll==5) {
      #         lay = rbind(c(1,2), c(3,4), c(5,6))
      #         plotlist = c(plotlist, lig)
      # 
      #       }
      #     } else if (input$view==3) {
      # 
      #       if (pll==1){
      #         lay=rbind(c(1))
      #         plotlist = c(plotlist)
      #         count = 1
      # 
      #       } else if (pll ==2) {
      #         lay = rbind(c(1), c(2))
      #         plotlist = c(plotlist)
      #         count = 2
      # 
      #       } else if (pll==3) {
      #         lay = rbind(c(1), c(2), c(3))
      #         # plotlist = c(plotlist, lig)
      #         plotlist = c(plotlist)
      #         count = 3
      # 
      # 
      #       } else if (pll==4) {
      #         lay = rbind(c(1), c(2), c(3), c(4))
      #         plotlist = c(plotlist)
      #         count = 4
      # 
      # 
      #       } else if (pll==5) {
      #         lay = rbind(c(1), c(2), c(3), c(4), c(5))
      #         plotlist = c(plotlist)
      #         count = 5
      # 
      # 
      #       }
      #     } else if (input$view==4) {
      # 
      #       if (pll==1){
      #         lay=rbind(c(1))
      #         plotlist = c(plotlist)
      #         count = 1
      # 
      #       } else if (pll ==2) {
      #         lay = rbind(c(1,2))
      #         plotlist = c(plotlist)
      #         count = 2
      # 
      #       } else if (pll==3) {
      #         lay =  rbind(c(1,2,3))
      #         # plotlist = c(plotlist, lig)
      #         plotlist = c(plotlist)
      #         count = 3
      # 
      # 
      #       } else if (pll==4) {
      #         lay =  rbind(c(1,2,3,4))
      #         plotlist = c(plotlist)
      #         count = 4
      # 
      # 
      #       } else if (pll==5) {
      #         lay =  rbind(c(1,2,3,4,5))
      #         plotlist = c(plotlist)
      #         count = 5
      # 
      # 
      #       }
      # 
      #     }
      # 
      #     # rv$pll =pll
      #     # }
      # 
      #     plotx = grid.arrange(grobs=plotlist, layout_matrix=lay)
      # 
      #     plotx
      # 
      #   })
      # 
      # })
    }
  }
  
  
 
  
  
  # observeEvent(input$colplot_hover, {
  #   rv$x = input$colplot_hover$x
  #   rv$y = input$colplot_hover$y
  # }) 
  
  
  observeEvent(input$colplot_click, {
    if (input$colplot_click$x >0 & input$colplot_click$x <300) {
    rv$x = input$colplot_click$x
    summ = round(as.numeric(rv$x), 0)
    
    }
    rv$y = input$colplot_click$y
    
  }) 
  
  
  
  observe({
    
    
  output$colplot = renderPlot({
    

      
    
    req(pacu())
     # summ=0
    
    si =
      function(per) {
        per*session$clientData$output_colplot_width*session$clientData$pixelratio/1.1/
          (700+.10*session$clientData$output_colplot_width)
        # per*session$clientData$output_plotj_width*session$clientData$pixelratio/.8/
        #   (570+.15*session$clientData$output_plotj_width)
      }
    # }
    if (input$isMobile=="FALSE") {
      
      lsi =
        function(per) {
          per*session$clientData$output_colplot_width*session$clientData$pixelratio/800
        }
    }
    
    if (input$isMobile=="TRUE") {
      
      lsi =
        function(per) {
          per*session$clientData$output_colplot_width*session$clientData$pixelratio/2000
        }
    }
    
    bgc = rv$bgc
    teksvari = rv$teksvari
    sep =20
    
    ffor = as.data.table(pacu())
    
    ffor= ffor[country %in% c(ll2) ,]
    
    # xcen = input$xcen
    
    if (input$xvar == "gdpcap") {
    xcen = 197
    } else {
    xcen = 218
    }
    
    
    
    ffoa = ffor[year ==2021 & country %in% c(ll2) ,]
    if (input$xvar=="gdpcap") {
      ffoa = ffoa[gdpcap >0,]
    }
    
    ffoa <- ffoa[order(ffoa[[input$xvar]],  decreasing = TRUE)]
    ffoa[, placeb:= seq_len(.N)]
    
    
    
    ffoa <- ffoa[order(ffoa[[input$xvar]])]

    ffoa[, place:= seq_len(.N)]

    ffoa$cats <- cut(ffoa$place, breaks=xcen)
    
    
    ffo = ffor[ffoa, cats := i.cats, on=c("country") ]
    ffo = ffo[ffoa, place := i.place, on=c("country") ]
    ffo = ffo[ffoa, placeb := i.placeb, on=c("country") ]
    
    ffo = ffo[place >=0, ]
    # ffo = ffo[ffoa, an := i.an, on=c("country") ]
    
    
    if (input$yvar =="countryfossil") {
      ffo[,meanyy:=mean(countryfossil, na.rm=TRUE) ,by=.(cats, year)]
    }
    if (input$yvar =="countrycost") {
      ffo[,meanyy:=mean(countrycost, na.rm=TRUE) ,by=.(cats, year)]
    }
    if (input$yvar =="countrynetcost") {
      ffo[,meanyy:=mean(countrynetcost, na.rm=TRUE) ,by=.(cats, year)]
    }
  
    ffo[year >=rv$fyear,maxyy:=max(meanyy, na.rm=TRUE) ,by=.(cats)]
    ffo = ffo[ffo[year >=rv$fyear & meanyy==maxyy], mayear :=i.year, on=c("cats")] 

    ffo[year >=rv$fyear,minyy:=min(meanyy, na.rm=TRUE) ,by=.(cats)]
    ffo = ffo[ffo[year >=rv$fyear & meanyy==minyy], miyear :=i.year, on=c("cats")] 
    

    ffo = distinct(ffo, year, cats, .keep_all = TRUE)
    

    ffo[meanyy <=0, cols :=rv$green]
    ffo[meanyy >=0, cols :=rv$red]
    
    dats =ffo[year ==input$xyear,]
    dats$zero =0
    
    # datsk = as.data.frame(dats)
    # 
    # hover <- input$colplot_click
    # 
    # hover <- input$colplot_click
    # 
    # point <- nearPoints(datsk, hover, xvar= "place", yvar="zero", 
    #                     # threshold =7000,
    #                     maxpoints = 1, 
    #                     addDist = TRUE)
    # 
    # point=as.data.table(point)
    
    # if (nrow(point) == 0) return(NULL)
    
    maxy = max(dats$maxyy)
    miny = min(dats$minyy)
    tall = maxy-miny
    hi = function(he) {
      miny+tall*he/100
    }
    
    maxx = max(dats$place)
    minx = min(dats$place)
    wide = maxx-minx
    wi = function(we) {
      minx+wide*we/100
    }
    
  
    
    # dats = dats[lux(), label:=i.label ,on=c("sec")]

    ylab = lux()[sec==input$yvar, label]
    
    lek = " t"
    
    if (input$yvar == "countrynetcost") {
      lek = " $"
      roy = 0
      if (rv$lang =="fin") {
        ylab = "Nettokustannukset henkeä kohden"
      }
      if (rv$lang =="eng") {
        ylab = "Net costs per capita"
      }
    }
    
    if (input$yvar == "countrycost") {
      lek = " $"
      roy = 0
      
      if (rv$lang =="fin") {
        ylab = "Päästökustannukset henkeä kohden"
      }
      if (rv$lang =="eng") {
        ylab = "Carbon costs per capita"
      }
    }
    
    
    if (input$yvar == "countryfossil") {
      lek = " t"
      roy = 2
      
      if (rv$lang =="fin") {
        ylab = "Päästöt henkeä kohden"
      }
      if (rv$lang =="eng") {
        ylab = "Emissions per capita"
      }
    }
    
    
    if (input$xvar == "gdpcap") {
      dats$xval =ffo[year ==2021, gdpcap]
      lekx = " $"
      rox = 0
      if (rv$lang =="fin") {
        xlab = "BKT henkeä kohden"
      }
      if (rv$lang =="eng") {
        xlab = "GDP per capita"
      }
    }
    
    if (input$xvar == "countryfossil") {
     dats$xval = ffo[year ==2021, countryfossil]
      # dats[dats[year ==2021, ], xval := i.countryfossil, 
      lekx = " t"
      rox = 2
      
      if (rv$lang =="fin") {
        xlab = "Päästöt henkeä kohden"
      }
      if (rv$lang =="eng") {
        xlab = "Emissions per capita"
      }
    }
    
    # datspla = dats[place %in% summ, ]
    if (rv$lurk==0) {
    datspla = dats[place %in% round(as.numeric(rv$x), 0), ]
    }
    
    if (rv$lurk==1){
      datspla = dats[country==input$comcou,]
     }
    

      
    # xlab = "gfgg"
    # hae vuodet jotka oli min ja max
    #  merkkaa nykyisen vuoden, max-vuoden ja min-vuoden arvot 
    # eri teksti riippuen ett' onko net benefit vai net cost
    # suomen tiedot, jos kielen' on suomi
    # ability to close and open comparisons
    # x-akselille populaatio 
    # yk 
    
    
    if (rv$lang=="fin") {
     rara = "Raha-arvot v. 2015 dollareissa\nKustannukset eivät sisällä maankäytön muutoksen hinnoittelun kustannuksia"
      ord = "Järjestys: "
      prom = "Klikkaa kuvaa nähdäksesi tietyn maan arvoja  "
      yea = " vuonna "
      datsfin = dats[country=="Finland",]
    } else if (rv$lang=="eng") {
      rara = "Monetary values in 2015 US dollars\nCosts don't include costs from land use change pricing"
      ord = "Ordered by: "
      
      prom = "Click the image to see details for certain country  "
      
      yea = " in "
      
    }
    
    gencol ="orange"
    fincol ="#70a8c2"
    sis=2.7
    # datspla[]
    
    plotcol = ggplot(dats) +
      
      geom_col(aes(x=cats, y=meanyy, group=cats, fill=cols)) +
      
      geom_point(aes(x=cats, y=maxyy, group=cats), color=teksvari) +
      
      geom_point(aes(x=cats, y=minyy, group=cats), color=teksvari) +
      
      
      geom_hline(aes(yintercept=0), color=teksvari) + 
      
      geom_text(data=datspla,
                aes(x=wi(98), y=hi(-1), 
                    label = rara),
                hjust=1, size=si(3), color=teksvari) +
      
      
      # coord_flip() +
      scale_color_identity() + 
      scale_alpha_identity() + 
      
      scale_fill_identity() + 
      
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
      
      
      theme(
        # axis.title.x=element_blank(),
        plot.title=element_text(lineheight = 0.8, hjust=0.5, size=33, family="Alegreya Sans Bold"),
        plot.subtitle=element_text(lineheight = 0.8, size=20, hjust=0.5),
        # axis.text.x=element_blank(),
        # plot.margin = margin(-5*input$dim[1]/2990,-5*input$dim[1]/90,-5*input$dim[1]/290,-5*input$dim[1]/200, unit = "pt"),
        panel.border = element_blank(), 
        panel.grid.major.y = element_line(linetype="dashed"),
        
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.title.y=element_blank(),
        axis.text.y=element_text(color="grey", size= si(10)),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        plot.background = element_rect(fill =bgc, color=NA), 
        panel.background = element_rect(fill = bgc ,  color=NA)
      )
    
    

     # if (exists("summ")) {
           if (round(as.numeric(rv$x), 0) > 0 || input$comcou != "none") {
        
      
      plotcol = plotcol + 
        
        geom_vline(data=datspla, aes(xintercept=place),
                   color=gencol, linewidth = lsi(1), alpha=.6) +
        
         annotate("rect",xmin=wi(1), xmax=wi(45), ymin=hi(68),ymax=hi(97), alpha=0.6, fill=bgc)  +
        
        annotate("rect",xmin=wi(1), xmax=wi(45), ymin=hi(68),ymax=hi(86), alpha=0.5,color=gencol, fill=bgc)  +
        
        
        geom_text(data=dats[country=="Finland",],
                  aes(x=wi(10), y=hi(95), 
                      label = paste0(ylab, yea, year, ",", lekx)),
                  hjust=0, size=si(4), color=teksvari) +
        
        geom_text(data=dats[country=="Finland",],
                  aes(x=wi(10), y=hi(90), 
                      label = paste0(ord, xlab, yea, " 2021")),
                  hjust=0, size=si(3.3), color=teksvari) +
        
        
        
        geom_text(data=datspla,
                  aes(x=wi(30), y=hi(83.5), 
                      label = paste0(country, ":")),
                  hjust=1, size=si(3), color=teksvari) +
        
        geom_text(data=datspla,
                  aes(x=wi(30), y=hi(80), 
                      label = paste0(ylab, yea, year, ": ")),
                  hjust=1, size=si(sis), color=teksvari) +
        
        geom_text(data=datspla,
                  aes(x=wi(31), y=hi(80), color=cols, 
                      label = paste0(format(round(meanyy,roy), big.mark=" "), lek)),
                  hjust=0, size=si(sis)) +
        
        geom_text(data=datspla,
                  aes(x=wi(30), y=hi(77), 
                      label = paste0(ylab, " max (",mayear, "): ")),
                  hjust=1, size=si(sis), color=teksvari) +
        
        geom_text(data=datspla,
                  aes(x=wi(31), y=hi(77), color=cols, 
                      label = paste0(format(round(maxyy,roy),big.mark=" "), lek)),
                  hjust=0, size=si(sis)) +
        
        geom_text(data=datspla,
                  aes(x=wi(30), y=hi(74),
                      label = paste0(ylab, " min (",miyear, "): ")),
                  hjust=1, size=si(sis), color=teksvari) +
        
        geom_text(data=datspla,
                  aes(x=wi(31), y=hi(74), color=cols,
                      label = paste0(format(round(minyy,roy), big.mark=" "), lek)),
                  hjust=0, size=si(sis)) +
        
        geom_text(data=datspla,
                  aes(x=wi(30), y=hi(71), 
                      label = paste0(xlab,yea,"2021:")),
                  hjust=1, size=si(sis), color=teksvari) +
        
        geom_text(data=datspla,
                  aes(x=wi(31), y=hi(71), 
                      label = paste0( format(round(xval,rox), big.mark=" "),lekx, "  (",placeb, "./", xcen,")"  )),
                  hjust=0, size=si(sis), color=teksvari) 
      
      
           } else {
             
             plotcol = plotcol + 
               
               annotate("rect",xmin=wi(1), xmax=wi(45), ymin=hi(68),ymax=hi(97), alpha=0.6, fill=bgc)  +
               
               
               geom_text(data=dats[country=="Finland",],
                         aes(x=wi(10), y=hi(95), 
                             label = paste0(ylab, yea, year, ",",lek)),
                         hjust=0, size=si(4), color=teksvari) +
               
               geom_text(data=dats[country=="Finland",],
                         aes(x=wi(10), y=hi(90), 
                             label = paste0(ord, xlab, yea, " 2021")),
                         hjust=0, size=si(4), color=teksvari) +
               
               
               
             geom_label(data=dats[country=="Finland",],
                       aes(x=wi(5), y=hi(83.5), 
                           label = prom),
                       hjust=0, size=si(5), fill="white")  
             
     }
    
    
    if (rv$lang=="fin") {
      plotcol = plotcol +
        
        geom_vline(data=datsfin, aes(xintercept=place),
                   color=fincol, linewidth = lsi(1), alpha=.5) +
        
        annotate("rect",xmin=wi(1), xmax=wi(45), ymin=hi(68-sep),
                 ymax=hi(86-sep), alpha=0.6,color=fincol, fill=bgc)  +

        geom_text(data=datsfin,
                  aes(x=wi(30), y=hi(83.5-sep), 
                      label = paste0(country, ":")),
                  hjust=1, size=si(3), color=teksvari) +
        
        geom_text(data=datsfin,
                  aes(x=wi(30), y=hi(80-sep), 
                      label = paste0(ylab, yea, year, ": ")),
                  hjust=1, size=si(sis), color=teksvari) +
        
        geom_text(data=datsfin,
                  aes(x=wi(31), y=hi(80-sep), color=cols, 
                      label = paste0(round(meanyy,roy), lek)),
                  hjust=0, size=si(sis)) +
        
        geom_text(data=datsfin,
                  aes(x=wi(30), y=hi(77-sep), 
                      label = paste0(ylab, " max (",mayear, "): ")),
                  hjust=1, size=si(sis), color=teksvari) +
        
        geom_text(data=datsfin,
                  aes(x=wi(31), y=hi(77-sep), color=cols, 
                      label = paste0(round(maxyy,roy), lek)),
                  hjust=0, size=si(sis)) +
        
        geom_text(data=datsfin,
                  aes(x=wi(30), y=hi(74-sep),
                      label = paste0(ylab, " min (",miyear, "): ")),
                  hjust=1, size=si(sis), color=teksvari) +
        
        geom_text(data=datsfin,
                  aes(x=wi(31), y=hi(74-sep), color=cols,
                      label = paste0(round(minyy,roy), lek)),
                  hjust=0, size=si(sis)) +
        
        geom_text(data=datsfin,
                  aes(x=wi(30), y=hi(71-sep), 
                      label = paste0(xlab,yea,"2021:")),
                  hjust=1, size=si(sis), color=teksvari) +
        
        geom_text(data=datsfin,
                  aes(x=wi(31), y=hi(71-sep), 
                      label = paste0( format(round(xval,rox), big.mark=" "),lekx, "  (",placeb, "./", xcen,")"  )),
                  
                      # label = paste0( round(xval,rox),lekx, "  (",place, "./", xcen,")"  )),
                  hjust=0, size=si(sis), color=teksvari) 
      
    }
    

    withProgress( message="Drawing graph, please wait",{
      
    plotcol
    })
    
    # cols()
    
    
  }, 
  width ="auto", 
  height=input$dim[2]/1.6
  # ,hover = "colplot_hover"
  
  
  
  
  )

    
  })
  # output$colsplot <- renderUI({
  #   # req(input$accordion3)
  #   
  #   # req(input$alert)
  #   # hhh = ht()
  #   # hhh = as.numeric(reactive(session$clientData$output_plot_width*.7))
  #   # hhh = as.numeric(session$clientData$output_plot_width*.7)
  #   # huh =  function() {
  #   #   session$clientData$output_plot_width*.7
  #   # }
  #   # huhu = huh()
  #   plotOutput("colplot" 
  #              # ,height=paste0(huhu)
  #              # ht()
  #              # ,height=session$clientData[["output_plot1_width"]]
  #              # ,height=ht()
  #              # 
  #              ,hover = "colplot_hover"
  #              
  #   )
  #   
  #   
  # }
  # 
  # )
  # 
  
  
  
  
  
  
  output$plot<-renderPlot({
    sec()
  } 
  ,height=
    function() {
      session$clientData$output_plot_width*.52
    }
  )
  
  output$splot <- renderUI({
    div(          style =  "margin-left: 0vw;
              margin-top: -.2vw; ",
              plotOutput("plot" 
                         ,width = "auto"
                         ,height=session$clientData[["output_plot1_width"]]
                         
                         ,click = "plot_click"
              )
    ) 
  }  )
  
  # eventReactive(input$go, {
  #   
  # })
  
  observe({
    if (rv$autodraw == TRUE & input$view==2) {
      
      output$plotj<-renderPlot({
        
        sec2()
        
        
      }
      , height= 
        function() {
          # if (input$autodraw == FALSE)
          # {
          # req(sec2b())}
          if (rv$pll ==2 ){
            session$clientData$output_plotj_width*hih/2
          } else if (rv$pll ==3) {
            session$clientData$output_plotj_width*hih
          } else if (rv$pll==5) {
            session$clientData$output_plotj_width*hih*1.33
          } else if (rv$pll==1) {
            session$clientData$output_plotj_width*(hih-.15)
          } else if (rv$pll==4) {
            session$clientData$output_plotj_width*hih
          }
        }
      
      )
    }  else if (rv$autodraw == FALSE & input$view==2) {
      
      
      # req(sec2b(), cancelOutput = TRUE)
      # if(!is.null(sec2b())) {
      # rv$plll = length(unique(datsl()$labbi))
      # eventReactive(input$go, { 
      output$plotj<-renderPlot({
        # if (input$autodraw==TRUE) {
        #   rv$plll = length(unique(datsl()$labbi))
        # } else
        # {
        #   eventReactive(input$go==TRUE, {
        #     
        #     rv$plll = length(unique(datsl()$labbi))
        #     
        #   })
        #   
        # }
        # req(sec2())
        sec2b()
      }
      ,height= 
        function() {
          # if (input$autodraw == FALSE)
          # {
          # req(sec2b())}
          if (rv$pll ==2 ){
            session$clientData$output_plotj_width*hih/2
          } else if (rv$pll ==3) {
            session$clientData$output_plotj_width*hih
          } else if (rv$pll==5) {
            session$clientData$output_plotj_width*hih*1.33
          } else if (rv$pll==1) {
            session$clientData$output_plotj_width*(hih-.15)
          } else if (rv$pll==4) {
            session$clientData$output_plotj_width*hih
          }
        }
      )
      # }
      # } )
      
    }
    
    # sec2()
    # hih = .6
    
    
    # sec2()
    # }
    #  )
  })
  
  # observe({
  # if (input$autodraw==TRUE) {
  #   rv$plll = length(unique(datsl()$labbi))
  #   # rv$plll = length(unique(datsl()$labbi))
  #   
  # } else
  # {
  eventReactive(input$go==TRUE, {
    # rv$trig = 1
    
    # req(datsss())
    # rv$plll = length(unique(datsl()$labbi))
    rv$plll = rv$pll
    
  })
  
  # } 
  #   })
  
  observe({
    # 
    rats = 1.07
    
    
    # eventReactive(input$go, { 
    
    if  (rv$autodraw == TRUE & input$view==2)  {
      output$plotjj <- renderUI({ 
        
        {  
          if (rv$pll ==1) {
            heeh = "auto"
            # session$clientData[["output_plotk_width"]]
          }
          
          else if
          (rv$pll ==2 ){
            
            if (input$dim[2]/rats >  session$clientData$output_plotj_width*hih/2) {
              
              heeh =   session$clientData$output_plotj_width*hih/2
              # heeh =  c(session$clientData$output_plotj_height)/.91
            } else  {
              heeh =  input$dim[2]/rats
            }
            
            
          } else if (rv$pll ==3) {
            if (input$dim[2]/rats >  session$clientData$output_plotj_width*hih) {
              
              heeh =  session$clientData$output_plotj_width*hih
              # heeh =  c(session$clientData$output_plotj_height)/.91
            } else  {
              heeh =  input$dim[2]/rats
            }
            
            
          } else if (rv$pll==5) {
            if (input$dim[2]/rats >  session$clientData$output_plotj_width) {
              
              heeh =  session$clientData$output_plotj_width
              # heeh =  c(session$clientData$output_plotj_height)/.91
            } else  {
              heeh =  input$dim[2]/rats
            }
            
          } else if (rv$pll==1) {
            
            if (input$dim[2]/rats >  session$clientData$output_plotj_width*(hih-.15)) {
              
              heeh =  session$clientData$output_plotj_width*(hih-.15)
              # heeh =  c(session$clientData$output_plotj_height)/.91
            } else  {
              heeh =  input$dim[2]/rats
            }
            
          } else if (rv$pll==4) {
            
            if (input$dim[2]/rats >  session$clientData$output_plotj_width*hih) {
              heeh =   session$clientData$output_plotj_width*hih
              # heeh =  c(session$clientData$output_plotj_height)/.91
            } else  {
              heeh =  input$dim[2]/rats
            }
            
          }
          
          
          div(        
            
            plotOutput("plotj"
                       ,width = "auto"
                       # ,height = "auto"
                       ,height=heeh
                       # ,height="100%"
                       
                       # ,hover = "plot_hover"
                       # ,click = "plotj_click"
                       
            ) 
          )
          
          
        }
      }) 
    } 
    
    else  if (rv$autodraw == FALSE & input$view==2)  {
      
      
      output$plotjj <- renderUI({ 
        # if(!is.null(sec2b())) {
        
        
        {  
          if (rv$plll ==1) {
            heeh = "auto"
            # session$clientData[["output_plotk_width"]]
          }
          
          else if
          (rv$plll ==2 ){
            
            if (input$dim[2]/rats >  session$clientData$output_plotj_width*hih/2) {
              
              heeh =   session$clientData$output_plotj_width*hih/2
              # heeh =  c(session$clientData$output_plotj_height)/.91
            } else  {
              heeh =  input$dim[2]/rats
            }
            
            
          } else if (rv$plll ==3) {
            if (input$dim[2]/rats >  session$clientData$output_plotj_width*hih) {
              
              heeh =  session$clientData$output_plotj_width*hih
              # heeh =  c(session$clientData$output_plotj_height)/.91
            } else  {
              heeh =  input$dim[2]/rats
            }
            
            
          } else if (rv$plll==5) {
            if (input$dim[2]/rats >  session$clientData$output_plotj_width) {
              
              heeh =  session$clientData$output_plotj_width
              # heeh =  c(session$clientData$output_plotj_height)/.91
            } else  {
              heeh =  input$dim[2]/rats
            }
            
            # } else if (rv$plll==1) {
            #   
            #   if (input$dim[2]/rats >  session$clientData$output_plotj_width*(hih-.15)) {
            #     
            #     heeh =  session$clientData$output_plotj_width*(hih-.15)
            #     # heeh =  c(session$clientData$output_plotj_height)/.91
            #   } else  {
            #     heeh =  input$dim[2]/rats
            #   }
            
          } else if (rv$plll==4) {
            
            if (input$dim[2]/rats >  session$clientData$output_plotj_width*hih) {
              heeh =   session$clientData$output_plotj_width*hih
              # heeh =  c(session$clientData$output_plotj_height)/.91
            } else  {
              heeh =  input$dim[2]/rats
            }
            
          }
          
          
          div(        
            
            plotOutput("plotj"
                       ,width = "auto"
                       # ,height = "auto"
                       ,height=heeh
                       # ,height="100%"
                       
                       # ,hover = "plot_hover"
                       # ,click = "plotj_click"
                       
            ) 
          )
          
          
        }
        # }
      }  ) 
      # }  ) 
      
    }
    # }  ) 
  }  ) 
  
  
  
  
  
  
  
  
  
  
  
  
  
  sec2a =  reactive(sec2())
  sec2b =  eventReactive(input$go, {
    # rv$plll = rv$pll
    
    sec2()
  })
  sec3a =  reactive(sec3())
  sec3b =  eventReactive(input$go, {
    sec3()
  })
  
  # sec4 =  reactive(sec3())
  sec4b =  eventReactive(input$go, {
    
    # req(datsss())
    
    sec3()
  })
  
  sec4a =   reactive( {
    # req(datsss())
    
    sec3()})
  
  
  
  
  # 
  
  
  
  
  
  
  
  
  
  
  observe({
    if  (rv$autodraw == TRUE & input$view==3)  {
      
      output$plotk<-renderPlot({
        sec3()
      }
      ,height=
        function() {
          session$clientData$output_plotk_width*.5*rv$pll
        }
      )
    } 
    else if (rv$autodraw == FALSE & input$view==3) 
    {
      output$plotk<-renderPlot({
        sec3b()
      }
      ,height=
        function() {
          session$clientData$output_plotk_width*.5*rv$pll
        }
      )
    }
  })
  
  observe({
    # 
    rats = 1.07
    
    
    # eventReactive(input$go, { 
    
    # if  (rv$autodraw == TRUE & input$view==3)  {  
    
    
    output$plotkk <- renderUI({
      rats = 1.25
      # if (rv$pll ==1) {
      #    heeh = "auto"
      # 
      # }   else    {
      #   
      #   if (input$dim[2]/rats >  session$clientData$output_plotk_width*.5*rv$pll) {
      #     
      #     heeh =   session$clientData$output_plotk_width*.5*rv$pll
      #     # heeh =  c(session$clientData$output_plotj_height)/.91
      #   } else  {
      #     heeh =  input$dim[2]/rats
      #   }
      #     }
      div(        
        plotOutput("plotk"
                   ,width = "auto"
                   # ,height=heeh
                   ,height="auto"
                   
                   ,hover = "plot_hover"
                   ,click = "plotk_click"
        ) 
      )
    }  
    )
    
  })
  
  
  
  observe({
    if (rv$autodraw == TRUE & input$view==4)  {
      
      output$plotl<-renderPlot({
        sec4a()
      }
      ,width=
        function() {
          session$clientData$output_plotl_height*1.8*rv$pll
        }
      )
    } 
    else if (rv$autodraw == FALSE & input$view==4) 
    {
      output$plotl<-renderPlot({
        sec4b()
      }
      ,width=
        function() {
          session$clientData$output_plotl_height*1.8*rv$pll
        }
      )
    }
  })
  
  observe({
    
    output$plotll <- renderUI({
      # rv$trig
      rats = 1.25
      # if (rv$pll ==1) {
      #   heeh = "auto"
      #   
      # }   else    {
      
      
      
      heeh  = min(input$dim[1], input$dim[2])*.6*rv$pll
      # if (input$dim[1]/rats >  session$clientData$output_plotl_width*.5*rv$pll) {
      #   
      #   heeh =   session$clientData$output_plotl_width*.5*rv$pll
      #   # heeh =  c(session$clientData$output_plotj_height)/.91
      # } else  {
      #   heeh =  input$dim[1]/rats
      # }
      # }
      div(        
        plotOutput("plotl"
                   ,width = heeh
                   ,height= min(input$dim[2], (input$dim[1])*.8)*.60
                   
                   # ,hover = "plot_hover"
                   # ,click = "plotl_click"
        ) 
      )
      
      #
      
    }  
    )
    
  })
  
  # 
  # observe({
  #   if (rv$autodraw == TRUE) {
  #     
  #     output$plotl<-renderPlot({
  #       sec4()
  #     }
  #     ,height= "1000"
  #     ,width= "1000"
  #     
  #     # function() {
  #     #   session$clientData$output_plotl_width*.5
  #     # }
  #     )
  #   } 
  #   else 
  #   {
  #     output$plotl<-renderPlot({
  #       sec4b()
  #     }
  #     ,height= "1000"
  #     ,width= "1000"
  #       # function() {
  #       #   session$clientData$output_plotl_width*.5
  #       # }
  #     )
  #   }
  # })
  # 
  # output$plotll <- renderUI({
  #   rats = 1.25
  #   # if (rv$pll ==1) {
  #   #   heeh = "auto"
  #   #   
  #   # }   else    {
  #   #   
  #   #   if (input$dim[2]/rats >  session$clientData$output_plotl_width*.5) {
  #   #     
  #   #     heeh =   session$clientData$output_plotl_width*.5
  #   #     # heeh =  c(session$clientData$output_plotj_height)/.91
  #   #   } else  {
  #   #     heeh =  input$dim[2]/rats
  #   #   }
  #   # }
  #   div(        
  #     plotOutput("plotl "
  #                ,width = "1000"
  #                ,height="1000"
  #                
  #                # ,height=heeh
  #                
  #                ,hover = "plot_hover"
  #                ,click = "plotl_click"
  #     ) 
  #   )
  # }  
  # )
  
  # 
  
  #   
  
  
  #   
  #   
  #   output$yearcuic = renderUI({
  #     
  #     if (input$view ==1) {
  #       fluidRow(
  #         style =  "margin-left: 0vw;
  #               margin-top: 0vw; ",
  #         sliderTextInput("yearcl", label="Observation year",choices= seq(minyy, maxyy, 1),
  #                         from_min = rv$ffyear, from_max = rv$lyear
  #                         ,selected=c(2100),
  #                         width="100%"
  #                         , animate=TRUE
  #                         ,grid=TRUE
  #         )
  #       )
  #     }  else if (input$view ==2) {
  #       minyy = mminyy
  #       maxyy = mmaxyy 
  #       fluidRow(
  #         style =  "margin-left: 0vw;
  #               margin-top: -1vw;
  #                       margin-bottom: -1vw;
  # ",
  # 
  # sliderTextInput("yearcl", label="Observation year", choices= seq(minyy, maxyy, 1),
  #                 from_min = rv$ffyear, from_max = rv$lyear
  #                 ,selected=c(2100)
  #                 ,grid=TRUE
  #                 
  #                 ,width="100%"
  #                 , animate=TRUE
  #                 
  # )   )    }
  #     else if (input$view ==3) {
  #       fluidRow(
  #         style =  "margin-left: 0vw;
  #               margin-top: -1vw;
  #                       margin-bottom: -1vw;",
  #         sliderTextInput("yearcl", label="Observation year",choices= seq(mmminyy, mmmaxyy, 1)
  #                         ,from_min = rv$ffyear, from_max = rv$lyear
  #                         
  #                         ,selected=c(2100),
  #                         width="100%"
  #                         , animate=TRUE
  #                         ,grid=TRUE
  #         )
  #       )
  #       
  #     }
  #     
  #     
  #     
  #   })

  # sliderInput("concc",
  #             inf("Convergence of countries emissions", "infoconvergence2"), 
  #             min = .01, max = 1, value = .5, step=.01, ticks=FALSE)
  
  
  
  
  output$buddd =renderUI({
    
    if (rv$lang =="eng") {
    
    div(id="sla",
    selectInput(
      "bud",
      inf("Carbon budget for net CO2 emissions since start of 2020", "infobudget"), 
      c("400Gt (67% likelihood to stay below 1.5°C)" = 400,
        "500Gt (50% likelihood to stay below 1.5°C)" = 500,
        "1150Gt (67% likelihood to stay below 2.0°C)" = 1150,
        "1350Gt (50% likelihood to stay below 2.0°C)" = 1350
        
      ),selected=1150,
      multiple = FALSE,
      selectize = FALSE,
      width = NULL,
      size = NULL
    ))
    

    }  else if (rv$lang =="fin") {
      div(id="sla",
      selectInput(
        "bud",
        inf("Hiilibudjetti CO2-nettopäästöille vuoden 2020 alusta lähtien", "infobudget"),


        c("400Gt (67% todennäköisyys pysyä alle 1,5°C)" = 400,
          "500Gt (50% todennäköisyys pysyä alle 1,5°C)" = 500,
          "1150Gt (67% todennäköisyys pysyä alle 2,0°C)" = 1150,
          "1350Gt (50% todennäköisyys pysyä alle 2,0°C)" = 1350
        ),selected=1150,
        multiple = FALSE,
        selectize = FALSE,
        width = NULL,
        size = NULL
      )
      )


    }
    
  } )
  
  
  
  
  
  
  output$concc = renderUI({
    req(rv$fyear)
    req(rv$lyear)
    if (rv$lang=="eng") {
      
      word ="Convergence of countries emissions:" } else {
        word="Päästöjen yhdentymisen aste:"
      }
    
    # if (input$view ==1) {
    # fluidRow(

      sliderInput("conc",
                  inf(word, "infoconvergence3"), 
                  min = .01, max = 1, value = .5, step=.01, ticks=FALSE)
      
    # )
    
    
    
    
    
    
  })
  
  
  
  output$xxyear = renderUI({
    req(rv$fyear)
    req(rv$lyear)
    if (rv$lang=="eng") {
      
      word ="Show year:" } else {
        word="Näytä tiedot vuodelle:"
      }

    # if (input$view ==1) {
    fluidRow(
      style =  "margin-left: 0vw;
              margin-top: -1vw;
                      margin-bottom: -1vw; ",
      sliderTextInput("xyear", label=word,choices= seq(rv$fyear, rv$lyear, 1),
                      
                      # sliderTextInput("yearc", label=NULL,choices= seq(minyy, maxyy, 1),
                      # from_min = rv$ffyear, from_max = rv$lyear
                      
                      selected=c(2040),
                      width="100%"
                      , animate=TRUE
                      ,grid=FALSE
                      # ticks=FALSE
                      )
    )
    

      
      
   
      
  })
  
  
  
  # 
  # output$xxcen = renderUI({
  #   # if (input$view ==1) {
  #   fluidRow(
  #     style =  "margin-left: 0vw;
  #             margin-top: -1vw;
  #                     margin-bottom: -1vw; ",
  #     
  #      sliderInput("xcen",
  #                   inf("Number of segments", "infopricing"),
  #                   min = 2, max = 218, value = c(218),
  #                  dragRange=FALSE, ticks = FALSE)
  #   )
  # })
  # 
  
  
  
  
  
  
  output$yearcui = renderUI({
    req(rv$ffyear)
    req(rv$lyear)
    if (input$view ==1) {
      fluidRow(
        style =  "margin-left: 0vw;
              margin-top: -1vw;
                      margin-bottom: -1vw; ",
        sliderTextInput("yearc", label=NULL,choices= seq(rv$ffyear, rv$lyear, 1),
                        
                        # sliderTextInput("yearc", label=NULL,choices= seq(minyy, maxyy, 1),
                        from_min = rv$ffyear, from_max = rv$lyear
                        
                        ,selected=c(rv$lyear),
                        width="100%"
                        , animate=FALSE
                        #
                        ,grid=FALSE
                        # ,ticks=FALSE
                        
        )
      )
    }  else if (input$view ==2) {
      minyy = mminyy
      maxyy = mmaxyy 
      
      fluidRow(
        
        style =  "margin-left: 0vw;
              margin-top: -1vw;
                      margin-bottom: -1vw;
",
sliderTextInput("yearc", label=NULL,choices= seq(rv$ffyear, rv$lyear, 1),
                
                # sliderTextInput("yearc", label=NULL, choices= seq(minyy, maxyy, 1),
                from_min = rv$ffyear, from_max = rv$lyear
                # ,step=1
                ,selected=c(rv$lyear)
                 ,grid=FALSE
                # ,ticks=FALSE
                ,width="100%"
                , animate=FALSE
                
)

      )
      
    }
    else if (input$view %in% c(3,4)) {
      fluidRow(
        style =  "margin-left: 0vw;
              margin-top: -1vw;
                      margin-bottom: -1vw;",
        sliderTextInput("yearc", label=NULL,choices= seq(rv$ffyear, rv$lyear, 1)
                        ,from_min = rv$ffyear, from_max = rv$lyear
                        
                        ,selected=c(rv$lyear),
                        width="100%"
                        , animate=FALSE
                         ,grid=FALSE
                        # ticks=FALSE
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
