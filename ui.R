
ui <- dashboardPage(dark=TRUE, fullscreen=TRUE, scrollToTop=TRUE,
                    
                    dashboardHeader(title = "Global carbon taxation simulator"),
                    dashboardSidebar(disable=TRUE),
                    
                    
                    
                    
                    dashboardBody(
                      # plotOutput("plotu", width="auto"), 
                      shinyjs::useShinyjs(),
                      
                      # set input$alert value based on whther value accordion is open or closed
                      tags$script(HTML("$(function() {
                         $('#accordion6')

                         .on('shown.bs.collapse', function() {
                         Shiny.setInputValue('alert',1);
                         })

                             .on('hidden.bs.collapse', function() {
                             Shiny.setInputValue('alert',0);
                             })
                         })



                     ")),
                     
                     
                     
                     
                     
                     tags$script(HTML('
                     

$(document).ready(function(){
    $(".nav li.active a").addClass("active");
});

                    ')),

tags$script(HTML('
                     
       
      var dim = [0, 0];
                                $(document).on("shiny:connected", function(e) {
                                    dim[0] = window.innerWidth;
                                    dim[1] = window.innerHeight;
                                    Shiny.onInputChange("dim", dim);
                                });
                                $(window).resize(function(e) {
                                    dim[0] = window.innerWidth;
                                    dim[1] = window.innerHeight;
                                    Shiny.onInputChange("dim", dim);
                                }); 

                    ')),






tags$style(type = "text/css",
           
           
           HTML(       
             "
   

// tarpeeton


  #tablu .bs4Table {
       border-collapse: collapse;
}
// tarpeeton
.nav.nav-pills li.disabled {
background-color: #aaa 
color: #333 !important;
cursor: not-allowed !important;
border-color: white !important;
}





//
   
//
                #nok {
                color: #f2ede6; 
}

// {}


// tarpeellinen {}

// yläreunan valikko {}
p {
margin-bottom: .1vw;
}

 .card-body {
               padding: .2vw;
    background-color:#454d55 !important;

 }




.accordion {


}

#view {
font-size: .85rem;
              margin-bottom: .1vw;
line-height: 1;

}

#visib {
font-size: .85rem;
line-height: 1;
              margin-bottom: .1vw;

}

#tablu {
color: blue;
font-family: mypolice1;
font-size: .87rem;
line-height: 1.1;
margin -.4vw -.4vw -.4vw -.4vw; padding: -.4vw  -.4vw -.4vw -.4vw;
border-collapse: collapse;
overflow: auto;
}



#tablu2 {
color: blue;
font-family: mypolice1;
font-size: .87rem;
line-height: 1.1;
margin -.4vw -.4vw -.4vw -.4vw; padding: -.4vw  -.4vw -.4vw -.4vw;
border-collapse: collapse;
overflow: auto;
width: 60%;
}


          .table {
              margin-bottom: .1vw;
overflow: scroll;
               }
               
               .table td {
               padding: .1vw;
              margin-bottom: .1vw;

               }

               .td {
               padding: .1vw;
              margin-bottom: .1vw;
display: inline-block;

               }

.form-group {
margin-bottom: .3rem;

}

//
        .nav-tabs .nav-item {
  transform: skew(-22.5deg);
  box-shadow: 5px 5px 15px rgba(0, 0, 0, .2);
        }

#bor {
    
padding: .4vw !important;
margin: .0vw !important;
box-sizing: border-box;
border-width: 0px;
}


#bor2 {
 border-color: #3C8DBC;
border-bottom-color: none;
            border-width: .2vw;
 border-style: outset;
border-color: white !important;
  border-radius: 5px;
padding: .3vw !important;


}
 
#bor3 {
 border-color: hsl(320,46%,77%);
border-bottom-color: none;
            border-width: .5vw;
 border-style: inset;
  border-radius: 5px;
padding: .3vw !important;


}



#bor4 {
 border-color: hsl(145,46%,77%);
border-bottom-color: none;
            border-width: .5vw;
 border-style: outset;
  border-radius: 5px;
padding: .3vw !important;


}

#bor5 {
 border-color: hsl(220,46%,77%);
border-bottom-color: none;
            border-width: .5vw;
 border-style: outset;
  border-radius: 5px;
padding: .0vw !important;


}

   .col-sm-12 {
padding: .0vw;
   } 


   .col-sm-3 {
padding: .01vw;
margin: 1.9.vw;
   } 


   .col-sm-4 {
padding: .01vw;
box-sizing: border-box;

   } 

   .col-sm-8 {
padding: 0.1vw;
margin: 0vw;
    border-left: 0px;
box-sizing: border-box;

    } 


// left panel texts {}

    a {
color: #f2ede6;
padding-left: .0vw;
margin-left: .0vw;
font-size: .9rem;

display: inline;
    } 
       
.row {
    margin-right: -1.5px;
    margin-left: -1.5px;

}


    a.active {
color: #f2ede6;
padding: .0vw;
margin: .0vw;

display: inline;
    background-color:#367e87 !important;
    display:block !important;
    } 

// nav extend left selection field backgrounds to whole lenght
and make vertical instead of horizontal {}             
  
          
         .nav{
flex-wrap: wrap-reverse;
  flex-direction: column;
text-align: left;
padding: .0vw;
margin: .0vw;
line-height: 1.2rem;}

  // left panel general {}


.content-wrapper .nav li {
  color: #46E8CD;

  border-bottom: .1px solid white;
  border-top: .1px solid white;

  }

.content-wrapper .nav li a {
  display: block;


  }


// left panel active
  box-shadow: 5px 5px 15px rgba(0, 0, 0, .2);
{}

.content-wrapper .nav li .active {
    border-top: 2px solid red;
    border-bottom: 2px solid red;
}

// right panel active {}
.tab-pane.active{
    background-color:#367e87;
               margin-left: -0.1vw;
padding: .5vw;
font-size: .85rem;

    border-top: 2px solid red;
    border-bottom: 2px solid red;

}


.control-label {
 
font-size: 1rem;
line-height: 1.2rem;

}

.card-title {
font-size: 1.0rem !important; 

}

.card {
  overflow: hidden;
margin-bottom: .0vw;
}

.card-header {
  overflow: hidden;
padding: .1vw;

}

.card-header {
text-align: center;
  
}

.body {
text-align: center;

}
  
.card-text {
    word-wrap: break-word;
flex-wrap: wrap;
}

#info .text {
display: flex;
flex-wrap: wrap;
        flex-direction: row;
}


               ")
)

,
uiOutput("css_style"),




fluidRow(
  
  column(3, titlePanel(title=h4("Global carbon taxation simulator", align="left"))
  ),
  column(7, style='border-left:1px solid;',height = '34vh',
         tagList(uiOutput("selitys2"))  
  ),
  
  column(2,   style='border-left:1px solid; font-size: 2vw; text-color: cyan;',height = '34vh', 
         # mainPanel(
         tagList(url=a("Link for more info", href="https://villeseppala.wordpress.com/2022/09/14/global-carbon-taxation-simulator-project-page/")), 
         
  )
),

fluidRow(
  
  
  column(12,          id = "bor",
         
         column(12, id = "bor5",
                
                accordion(
                  id = "accordion5",
                  accordionItem(
                    # <i class="material-icons">visibility</i>
                    
                    title = HTML("<font size='5'>","<i class='material-icons'>&#xe925;</i>", as.character(icon("pan_tool")), "  VIEW   ",  as.character( icon("fas fa-caret-square-down")),"</font>"),
                    collapsed =FALSE,
                    fluidRow(
                      column(5,        id = "bor",
                             
                             radioButtons("view", label = "Graph view",
                                          c("Single graph for all scales" = 1,
                                            "Separate graphs for each scale (in progress...)" = 2
                                            
                                          ),selected=1, inline=TRUE
                                          
                                          
                                          
                             )),
                      column(5,   
                             id = "bor",
                             
                             
                             p("Indicator visibility (Manual selection below)"),
                             awesomeCheckbox("visib", label="Automatic, based on VALUES phase", value=TRUE),
                             
                             actionBttn(inputId ="showall", label = "Select all", size="sm", style = "jelly"),
                             actionBttn(inputId ="shownone", label = "Deselect all", size="xs", style = "gradient"),
                             
                      )
                    ) , 
                    
                    div(
                      id="tablu",
                      class="noku",
                      bs4Dash::bs4Table(
                        # "nerf",
                        
                        cardWrap = TRUE,
                        bordered = TRUE,
                        striped = TRUE, 
                        
                        list(
                          
                          headTitles = list(
                            p("Variable:"),
                            
                            chk(fos, "Total emissions", "infofossil"),
                            chk(lul, "Land emissions/sinks", "infolul"),
                            chk(net, "Net emissions", "infonet"),
                            chk(pop, "Population", "infopop"),
                            chk(fpop, "Mean emissions", "infoavgfossil"),
                            chk(tax, "Carbon tax", "infoprice"),
                            chk(avgcost, "Mean carbon costs", "infoavgcost"),
                            chk(dividend, "Carbon dividend", "infodividend"),
                            chk(avgnetcost, "Mean net costs", "infoavgnetcost"),
                            chk(cpop, "User emissions", "infouserfossil"),
                            chk(taxfosindi, "User carbon costs", "infousercost"),
                            chk(netcost, "User net costs", "infonetcost")
                            
                            
                          )
                          ,
                          #         
                          # 
                          list(
                            p("Visibility:"),
                            awesomeCheckbox( "showfossil", label=NULL,  value=TRUE),
                            awesomeCheckbox("showland", label=NULL, value=TRUE),
                            awesomeCheckbox("shownet", label=NULL, value=TRUE),
                            awesomeCheckbox("showpop", label=NULL, value=FALSE),
                            
                            awesomeCheckbox("showavgfossil", label=NULL, value=TRUE),
                            awesomeCheckbox("showprice", label=NULL, value=FALSE),
                            awesomeCheckbox("showavgcost", label=NULL, value=FALSE),
                            awesomeCheckbox("showdividend", label=NULL, value=FALSE),
                            awesomeCheckbox("showavgnetcost", label=NULL, value=FALSE),
                            awesomeCheckbox("showuserfossil", label=NULL, value=FALSE),
                            awesomeCheckbox("showusercost", label=NULL, value=FALSE),
                            awesomeCheckbox("shownetcost", label=NULL, value=FALSE)
                            
                          ) ),
                        
                      ), 
                      
                      
                      div(
                        id="tablu2",
                        class="noku",
                        width ="50%",
                        
                        bs4Dash::bs4Table(
                          cardWrap = TRUE,
                          bordered = TRUE,
                          striped = TRUE,
                          list(
                            headTitles = list(
                              p("Variable:"),
                              chk(countryfossil, "Country per capita emissions", "infocountryfossil"),
                              chk(countrypop, "Country population", "infocountrypop"),
                              chk(countrycost, "Country per capita carbon cost", "countrycost"),
                              chk(countrynetcost, "Country per capita net cost", "countrynetcost")
                            ),
                            list(
                              p("Visibility:"),
                              awesomeCheckbox("showcountryfossil", label=NULL, value=FALSE),
                              awesomeCheckbox("showcountrypop", label=NULL, value=FALSE),
                              awesomeCheckbox("showcountrycost", label=NULL, value=FALSE),
                              awesomeCheckbox("showcountrynetcost", label=NULL, value=FALSE)
                              
                            )   ))
                      ))
                  ))),
         
         box(
           id = "infobox",
           width = NULL,
           title = paste("Info"),
           closable = TRUE,
           closed=TRUE,
           collapsible = FALSE,
           verbatimTextOutput("info"),
           ignore.init=TRUE
         ))),

fluidRow(
  
  column(3, 
         id ="bor",
         column(12, id = "bor4",
                
                # accordion(
                #   id = "accordion6",
                #   accordionItem(
                #     title = HTML("<font size='5'>", as.character( icon("fas fa-edit")), "VALUES",  as.character( icon("fas fa-caret-square-down")),"</font>"),
                #     # status = "teal",
                #     collapsed =FALSE,          
                
                
                div(
                  HTML(
                    "<body style='background-color:aquamarine;'>", 
                    "<font size='5'>", as.character(icon("far fa-hand-paper")), "  VALUES   ","</font>",
                    "</body>"),
                  
                  # h5("Set values:"),
                  navlistPanel(id="nok", 
                               
                               tabPanel( "1. Carbon budget", 
                                         radioButtons("bud", "Carbon budget for net CO2 emissions since start of 2020",
                                                      c("400Gt (67% likelihood to stay below 1,5C)" = 400,
                                                        "500Gt (50% likelihood to stay below 1,5C)" = 500,
                                                        "1150Gt (67% likelihood to stay below 2,0C)" = 1150,
                                                        "1350Gt (50% likelihood to stay below 2,0C)" = 1350
                                                        # "1700Gt (67% likelihood to stay below 2,4C)" = 1700,
                                                        # "2050Gt (50% likelihood to stay below 2,4C)" = 2050
                                                      ),selected=1350
                                         ),
                                         hr(style = "border-top: 1px solid white; margin-bottom:0px; margin-top:0px;;"),
                                         
                                         sliderInput("vuo", label ="Pricing start year and carbon neutrality year", min = 2020, max = 2100, value = c(2024, 2080), dragRange=FALSE),
                                         hr(style = "border-top: 1px solid white; margin-bottom:0px; margin-top:0px;;"),
                                         
                                         numericInput("paa", label="Emissions/sink at the carbon neutrality year",min = 0.1, max = 30,step=.1,value=c(5)),
                                         
                                         hr(style = "border-top: 1px solid white; margin-bottom:0px; margin-top:0px;;"),
                                         
                                         radioButtons("muo", "Shape of the emission curve",
                                                      c("Linear drop" = "linear","Percentual drop" = "percentual"
                                                      )),
                               ),
                               
                               tabPanel("2. Population projection",
                                        radioButtons("popc", "Choose UN population projection",
                                                     c(
                                                       "95% range upper limit" = 5,
                                                       "80% range upper limit" = 4,
                                                       "Medium projection" = 3,
                                                       "80% range lower limit" = 2,
                                                       "95% range lower limit" = 1
                                                     ),selected=3
                                        )
                               ),
                               
                               
                               
                               tabPanel("3. Carbon tax",
                                        numericInput("sprice", label=HTML("Set start year carbon price  ", "<font size='3'>",
                                                                          as.character(actionLink(inputId = "info3", 
                                                                                                  label = "  ", 
                                                                                                  icon = icon("info"))), "</font>"),
                                                     min = 1, max = 1000000,step=1,value=c(40)),
                                        hr(style = "border-top: 1px solid white; margin-bottom:0px; margin-top:0px;;"),
                                        
                                        numericInput("eprice", label=HTML("Set neutrality year carbon price", "<font size='2'>", as.character(actionLink(inputId = "info4", 
                                                                                                                                                         label = "  ", 
                                                                                                                                                         icon = icon("info"))), "</font>"
                                        ),min = 1, max = 1000000,step=1,value=c(400)),
                                        
                                        
                                        hr(style = "border-top: 1px solid white; margin-bottom:0px; margin-top:0px;;"),
                                        
                                        
                                        radioButtons("pri", label ="Shape of the price curve",
                                                     
                                                     c(
                                                       
                                                       "Linear increase" = "linear",
                                                       "Percentual increase" = "percentual",
                                                       "Logarithmic increase" = "logarithmic"
                                                       
                                                     ))
                               ),
                               tabPanel("4. User emissions", 
                                        
                                        numericInput("indi1", label="Start year emissions",min = .01, max = 40,step=.01,value=c(7.77)),
                                        hr(style = "border-top: 1px solid white; margin-bottom:0px; margin-top:0px;;"),
                                        
                                        numericInput("indi2", label="Neutrality year emission",min = .01, max = 40,step=.01,value=c(0.77)),
                                        
                                        hr(style = "border-top: 1px solid white; margin-bottom:0px; margin-top:0px;;"),
                                        
                                        radioButtons("muoindi", "Shape of emission curve",
                                                     c("Linear drop" = "linear","Percentual drop" = "percentual"
                                                       
                                                     )),
                                        hr(style = "border-top: 1px solid white; margin-bottom:0px; margin-top:0px;;"),
                                        
                                        selectInput("indi", label = "ALTERNATIVE: Use country average emission path (this will slower the app considerably)", choices =c("none", paaco$country), selected="none"),
                                        hr(style = "border-top: 1px solid white; margin-bottom:0px; margin-top:0px;;"),
                                        
                                        sliderInput("con", label ="Convergence of countries' emissions", min = .01, max = 1, value = .5, step=.01),
                               ),
                               tabPanel("EXTRA: Country profiles",
                                        pickerInput(
                                          inputId = "countr",
                                          label = "Show countries (this will slower the page considerably):",
                                          choices = c(paaco$country),
                                          selected= NULL,
                                          options = list(
                                            `actions-box` = TRUE),
                                          multiple = TRUE
                                        ),
                                        hr(style = "border-top: 1px solid #62696E; margin-bottom:0px; margin-top:0px;"),
                                        
                                        sliderInput("con", label ="Convergence of countries' emissions", min = .01, max = 1, value = .5, step=.01),
                               ),
                  ),
                  uiOutput("cou"),
                  # ) )
                ) 
         )
  ),
  
  
  column(9, id ="bor",
         
         column(12, id = "bor3",
                conditionalPanel(condition="input.view ==1", 
                                 uiOutput("splot", width="auto"
                                 )
                ),
                conditionalPanel(condition="input.view ==2", 
                                 
                                 uiOutput("splot2", width="auto"
                                 )
                                 
                ),
                
                
                uiOutput("yearc"),
                
                
                
                # DT::dataTableOutput("tablz", width="auto"),
                # 
                # DT::dataTableOutput("tablx", width="100%"),
                # 
                #  DT::dataTableOutput("tably", width="100%"),
                
                
                # ,  width =12 )
                # ,hover = "plot_hover"
                #hover = hoverOpts(id ="plot_hover") )           
         )
  ) )
                    )
)




