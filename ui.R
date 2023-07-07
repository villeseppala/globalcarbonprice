


ui <- dashboardPage(dark=TRUE, fullscreen=TRUE, scrollToTop=TRUE,
                    
                    dashboardHeader(
                      actionButton(inputId = "eng", label = NULL, style = "width: 2.5rem; height: 1.5rem; margin: 3px;
background: url('flag/eng.png');  background-size: cover; background-position: center;"),  
actionButton(inputId = "fin", label = NULL, style = "width: 2.5rem; height: 1.5rem; margin: 3px;
background: url('flag/fin.png');  background-size: cover; background-position: center;"),
                       tagList(uiOutput(style="font-size: 1.3rem !important; margin: 5px; font-weight: 600;
    line-height: 1.1; ","titletext")),       
                      
                    #                 title = " Global carbon price and dividend -simulator (UNFINISHED, may contain errors)"
                    #                 # )
                    # ,
                                    # title="ffff",
# div(style="margin-left: 1px; padding: 1px;",
 div(id="but",
    actionButton(inputId="infodata", label="Data sources",
                 styleclass = "primary", size = "small", block = F,style="line-height: 1.1")),    
    # actionBttn(inputId ="infodata", 
    #            label = "Data sources", size="xs", style = "fill", color="primary"),
div(id="but",
    
  actionButton(inputId ="tutorial", label = "Tutorial",
                 styleclass = "primary", size = "small", block = F,style="line-height: 1.1")
     )
,
                                    tagList(a
                                            # (HTML("<font size='4'>", as.character
                                              (div(style="font-size: 15px !important; margin: 5px;",icon("fab fa-github"))
                                                                              # ))
                                                                              , 
                                              href="https://github.com/villeseppala/globalcarbonprice/")
                                            
                                            # ,title=""
                                            ),
                                    tagList(a
                                            # (HTML("<font size='4'>",as.character
                                            (div(style="font-size: 15px !important;  margin-right: 5px;",icon("fab fa-wordpress"))
                                                    # ))
                                              ,
                                              href="https://villeseppala.wordpress.com/2022/09/14/global-carbon-taxation-simulator-project-page/")
                                           # , title=""
                                            # , class="dropdown"
                                            ),

tagList(a
        # (HTML("<font size='4'>",as.character
        (div(style="font-size: 15px !important;  margin-right: 5px;",p("feedback"))
          # ))
          ,
          href="https://forms.gle/3ognTtwyejCB3XZg6")
        # , title=""
        # , class="dropdown"
)

    # )
                                    
 
                    # )                  

  # actionBttn(inputId ="eng", label = "Terial", size="xs",  color="primary",
#                                style = "width: 50px; height: 50px;
# background: url('flag/eng.png');  background-size: cover; background-position: center;"),
#                     

                    #                     tags$button(
                    #   id = "eng",
                    #   class = "btn action_button",
                    #   img(src = "flag/eng.png",
                    #       height = "30px")
                    # ),
                    # tags$button(
                    #   id = "fin",
                    #   class = "actionBttn",
                    #   img(src = "flag/fin.png",
                    #       height = "30px")
                    # )
                                    # tags$li(a(href = 'http://shinyapps.company.com',
                                    #           icon("power-off"),
                                    #           title = "Back to Apps Home"),
                                    #         class = "dropdown"),
                                    # list(tagList(url=a(HTML("<font size='4'>",
                                    #                      as.character(icon("fab fa-github"))), href="https://github.com/villeseppala/globalcarbonprice/"))),
                                    #   list(tagList(url=a(HTML("<font size='4'>",
                                    #                      as.character(icon("fab fa-github"))), href="https://www.villeseppala.fi"))
                                    #   
                                     # )
                                    
                                    
                                    
                                    
                                    ),
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
                     
                     
                     
                     
                     
                     tags$script(HTML("
                     

$(document).ready(function(){
    $('.nav li.active a').addClass('active');
});




                    ")),


tags$head(tags$script(HTML("$(document).on('click', function () {
                                Shiny.onInputChange('last_btn',this.id);
                             });"))),

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



tags$script(HTML("
                     
       
$(document).on('shiny:connected', function(event) {
        let check = false;
        (function(a){if(/(android|bb\\d+|meego).+mobile|avantgo|bada\\/|blackberry|blazer|compal|elaine|fennec|hiptop|iemobile|ip(hone|od)|iris|kindle|lge |maemo|midp|mmp|mobile.+firefox|netfront|opera m(ob|in)i|palm( os)?|phone|p(ixi|re)\\/|plucker|pocket|psp|series(4|6)0|symbian|treo|up\\.(browser|link)|vodafone|wap|windows ce|xda|xiino/i.test(a)||/1207|6310|6590|3gso|4thp|50[1-6]i|770s|802s|a wa|abac|ac(er|oo|s\\-)|ai(ko|rn)|al(av|ca|co)|amoi|an(ex|ny|yw)|aptu|ar(ch|go)|as(te|us)|attw|au(di|\\-m|r |s )|avan|be(ck|ll|nq)|bi(lb|rd)|bl(ac|az)|br(e|v)w|bumb|bw\\-(n|u)|c55\\/|capi|ccwa|cdm\\-|cell|chtm|cldc|cmd\\-|co(mp|nd)|craw|da(it|ll|ng)|dbte|dc\\-s|devi|dica|dmob|do(c|p)o|ds(12|\\-d)|el(49|ai)|em(l2|ul)|er(ic|k0)|esl8|ez([4-7]0|os|wa|ze)|fetc|fly(\\-|_)|g1 u|g560|gene|gf\\-5|g\\-mo|go(\\.w|od)|gr(ad|un)|haie|hcit|hd\\-(m|p|t)|hei\\-|hi(pt|ta)|hp( i|ip)|hs\\-c|ht(c(\\-| |_|a|g|p|s|t)|tp)|hu(aw|tc)|i\\-(20|go|ma)|i230|iac( |\\-|\\/)|ibro|idea|ig01|ikom|im1k|inno|ipaq|iris|ja(t|v)a|jbro|jemu|jigs|kddi|keji|kgt( |\\/)|klon|kpt |kwc\\-|kyo(c|k)|le(no|xi)|lg( g|\\/(k|l|u)|50|54|\\-[a-w])|libw|lynx|m1\\-w|m3ga|m50\\/|ma(te|ui|xo)|mc(01|21|ca)|m\\-cr|me(rc|ri)|mi(o8|oa|ts)|mmef|mo(01|02|bi|de|do|t(\\-| |o|v)|zz)|mt(50|p1|v )|mwbp|mywa|n10[0-2]|n20[2-3]|n30(0|2)|n50(0|2|5)|n7(0(0|1)|10)|ne((c|m)\\-|on|tf|wf|wg|wt)|nok(6|i)|nzph|o2im|op(ti|wv)|oran|owg1|p800|pan(a|d|t)|pdxg|pg(13|\\-([1-8]|c))|phil|pire|pl(ay|uc)|pn\\-2|po(ck|rt|se)|prox|psio|pt\\-g|qa\\-a|qc(07|12|21|32|60|\\-[2-7]|i\\-)|qtek|r380|r600|raks|rim9|ro(ve|zo)|s55\\/|sa(ge|ma|mm|ms|ny|va)|sc(01|h\\-|oo|p\\-)|sdk\\/|se(c(\\-|0|1)|47|mc|nd|ri)|sgh\\-|shar|sie(\\-|m)|sk\\-0|sl(45|id)|sm(al|ar|b3|it|t5)|so(ft|ny)|sp(01|h\\-|v\\-|v )|sy(01|mb)|t2(18|50)|t6(00|10|18)|ta(gt|lk)|tcl\\-|tdg\\-|tel(i|m)|tim\\-|t\\-mo|to(pl|sh)|ts(70|m\\-|m3|m5)|tx\\-9|up(\\.b|g1|si)|utst|v400|v750|veri|vi(rg|te)|vk(40|5[0-3]|\\-v)|vm40|voda|vulc|vx(52|53|60|61|70|80|81|83|85|98)|w3c(\\-| )|webc|whit|wi(g |nc|nw)|wmlb|wonu|x700|yas\\-|your|zeto|zte\\-/i.test(a.substr(0,4))) check = true;})(navigator.userAgent||navigator.vendor||window.opera);
        Shiny.setInputValue(id = 'isMobile', value = check);
      });

$(document).on('shiny:sessioninitialized', function (e) {
  var mobile = window.matchMedia('only screen and (max-width: 768px)').matches;
  Shiny.onInputChange('is_mobile_device', mobile);
});

                    ")),

# tags$head(tags$script('
#                                 var dimension = [0, 0];
#                                 $(document).on("shiny:connected", function(e) {
#                                     dimension[0] = window.innerWidth;
#                                     dimension[1] = window.innerHeight;
#                                     Shiny.onInputChange("dimension", dimension);
#                                 });
#                                 $(window).resize(function(e) {
#                                     dimension[0] = window.innerWidth;
#                                     dimension[1] = window.innerHeight;
#                                     Shiny.onInputChange("dimension", dimension);
#                                 });
#                             ')),

tags$head(
  tags$link(rel = "stylesheet", type = "text/css", href = "cs.css")
)


,
uiOutput("css_style"),
uiOutput("css_style2"),
uiOutput("css_style3"),
# border-color: rgb(92, 61, 46);



# fluidRow(
#   
#   column(3, titlePanel(title=h4("Global carbon taxation simulator", align="left"))
#   ),
#   column(7, style='border-left:1px solid;',height = '34vh',
#          tagList(uiOutput("selitys2"))  
#   ),
#   
#   column(2,   style='border-left:1px solid; font-size: 2vw; text-color: cyan;',height = '34vh', 
#          # mainPanel(
#          tagList(url=a("Link for more info", href="https://villeseppala.wordpress.com/2022/09/14/global-carbon-taxation-simulator-project-page/")) 
#                
#   )
# ),

fluidRow(
  
  
  column(12,          id = "bor",
         conditionalPanel(condition="input.isMobile==1",   {
           box(
             id = "mobilebox",
             width = NULL,
             title = "If you are using mobile, landscape rotation is recommended",
             closable = TRUE,
             closed=FALSE,
             collapsible = FALSE,
             # verbatimTextOutput("mobileuser"),
             ignore.init=TRUE
           ) }),  
    
         
         # div(id="tuto",
         #     box(
         #       style = 'overflow-x: hidden',
         #       # style = 'overflow-y: scroll',
         #       
         #       id = "infobox",
         #       width = NULL,
         #       title =textOutput("infot"),
         #       closable = TRUE,
         #       closed=TRUE,
         #       collapsible = FALSE,
         #       textOutput("info"),
         #       ignore.init=TRUE
         #       # ))
         #     )))),
         
         
         div(id="tuto",
         box(
           style = 'overflow-x: hidden',
           # style = 'overflow-y: scroll',

           id = "tutorialbox",
           # dots = TRUE,
           width = NULL,
           title ="Tutorial",
           closable = TRUE,
           closed=TRUE,
           collapsible = FALSE,
           # uiOutput("slicks")
           # if (input$isMobile ==T) {
           # slickROutput("slickr1",width = "100%", height="100%")
           # } else {
             slickROutput("slickr1",width = "80%", height="70%")
             
           # }
           # ,ignore.init=TRUE
           # ))
         ) ),
         
        # div( id = "tuto",
             
#              tagList(
              # uiOutput("tutbox",width = "90%", height="90%"),
#                 )
#              
# # )
# ,

        
         column(12, id = "bor5",

                # "</font>",
                # tagList(uiOutput("selitys2")),  
                # "<i class='material-icons'>&#xe925;</i>", as.character(icon("pan_tool")),
                accordion(
                  id = "accordion5",
                  accordionItem(
                    # <i class="material-icons">visibility</i>
                    title = tagList(uiOutput("viewtext")),       
                    
                    # title = HTML("<font size='4'>",
                    #              as.character(icon("far fa-eye")), 
                    #              
                    #         "  VIEWING SETTINGS  ",  
                    #        as.character( icon("fas fa-caret-square-down")),"</font>"),
                    collapsed =FALSE,
                     fluidRow(
                      column(2,        id = "bor5a",
                             
                             radioButtons("view", label = "Graph view",
                                          c("One graph for all scales" = 1,
                                            "Side by side graphs for each scale" = 2,
                                            "Back to back graphs for each scale" = 3
                                            
                                          ),selected=1, inline=TRUE)
                             
                             ,
                             div(style="display:inline-block; font-weight:100;",
                                 awesomeCheckbox("startvalue", label=textOutput("startlabel"), value=TRUE))
                             # ,
                             # 
                             # sliderInput("fonts", label ="Font size", min = 1, max = 10, value = c(5), dragRange=FALSE, ticks = FALSE),
                             # hr(),
                             
                             
                             ),
                      column(10,   
                             id = "bor5a",
                             
                             
                             div(style='font-weight:1000; color: rgba(224, 192, 151) !important;
                                 font-size: calc(.7vw + .7rem);
text-align: left !important;
text-decoration: none;
margin-bottom: .5rem;
margin-top: -.3rem;', textOutput("indicatorvisibility")),
                             div(style="display:inline-block",actionBttn(inputId ="showall", label = textOutput("showall") , size="xs", style = "fill", color="primary")),
                             div(style="display:inline-block",actionBttn(inputId ="shownone", label = textOutput("shownone") , size="xs", style = "fill", color="primary")
                             ),
                             div(style="display:inline-block; font-weight:100;",
                                 awesomeCheckbox("visib", label=textOutput("automatic"), value=TRUE)
                                # prettySwitch
                                # checkboxGroupButtons()
                                 # )
                                 # prettySwitch
                                 # checkboxGroupButtons()
                             ),
                             
 
                             
                       # ),
                   
                    
                    div(
                      id="tablu",
                      class="noku",
                      # tämä renderUIlla?
                       
               
                      
                      bs4Dash::bs4Table(
                        # "nerf",

                        cardWrap = TRUE,
                        bordered = TRUE,
                        striped = TRUE,

                        list(

                          headTitles = list(
                             cuk2(fos, textOutput("labelfossil"), "infofossil", "showfossil", NULL, TRUE),
                            
                            # cuk(fos, "Fossil emissions", "infofossil", "showfossil", NULL, TRUE),
                            cuk2(lul, textOutput("labelland"),  "infolul", "showland", NULL, TRUE),
                            cuk2(net,  textOutput("labelnet"), "infonet", "shownet", NULL, TRUE),
                            cuk2(pop,textOutput("labelpop"),  "infopop", "showpop", NULL, FALSE),
                            cuk2(fpop, textOutput("labelavgfossil"),  "infoavgfossil", "showavgfossil", NULL, TRUE),
                            cuk2(cpop,textOutput("labeluserfossil"), "infouserfossil", "showuserfossil", NULL, FALSE)
                            
                            # cuk(, "", "", "", NULL, TRUE),


                        
                          )
                        )
                      ),
                      bs4Dash::bs4Table(
                        # "nerf",
                        
                        cardWrap = TRUE,
                        bordered = TRUE,
                        striped = TRUE,
                        
                        list(
                          
                          headTitles = list(
                            cuk2(tax,textOutput("labelprice"),  "infoprice", "showprice", NULL, FALSE),
                            
                            cuk2(avgcost, textOutput("labelavgcost"), "infoavgcost", "showavgcost", NULL, FALSE),
                            cuk2(dividend, textOutput("labeldividend"),  "infodividend", "showdividend", NULL, FALSE),
                            cuk2(avgnetcost, textOutput("labelavgnetcost"), "infoavgnetcost", "showavgnetcost", NULL, FALSE),
                            cuk2(taxfosindi, textOutput("labelusercost"),  "infousercost", "showusercost", NULL, FALSE),
                            cuk2(netcost,textOutput("labelnetcost"),  "infonetcost", "shownetcost", NULL, FALSE)
                            
                            
                            
                            
                          )
                          
                        )
                      )
                      


                      )
                    ,
                      

div(
  #table joka näkyy jos averagedividend tai countrydividend relevantteja
  id="tablu4",
  class="noku",
  # width ="50%",
  
  bs4Dash::bs4Table(
    cardWrap = TRUE,
    bordered = TRUE,
    striped = TRUE,
    list(
      headTitles = list(
        cuk2(non, textOutput("labelnonco2"),  "infononco2", "shownonco2", NULL, FALSE),
        
        cuk2(tot, textOutput("labelghg"),  "infoghg", "showghg", NULL, FALSE)
        # cuk(countrycost, "Country per capita cost", "countrycost", "showcountrycost", NULL, FALSE),
        # cuk(countrynetcost, "Country per capita net cost", "countrynetcost", "showcountrynetcost", NULL, FALSE)
        # 
        
      )
      
    )
    
    
  )
  
), 


                    
                    div(
                      #table joka näkyy jos averagedividend tai countrydividend relevantteja
                      id="tablu3",
                      class="noku",
                      # width ="50%",
                      
                      bs4Dash::bs4Table(
                        cardWrap = TRUE,
                        bordered = TRUE,
                        striped = TRUE,
                        list(
                          headTitles = list(
                             cuk2(averagedividend, textOutput("labelaveragedividend"),  "infoaveragedividend", "showaveragedividend", NULL, FALSE),
                            cuk2(countrydividend, textOutput("labelcountrydividend"),  "infocountrydividend", "showcountrydividend", NULL, FALSE)
                            # cuk(countrycost, "Country per capita cost", "countrycost", "showcountrycost", NULL, FALSE),
                            # cuk(countrynetcost, "Country per capita net cost", "countrynetcost", "showcountrynetcost", NULL, FALSE)
                            # 
                            
                          )
                          
                        )
                        
                        
                      )
                      
                    ), 
                    
                  
                      div(
                        id="tablu2",
                        class="noku",
                        # width ="50%",

                        bs4Dash::bs4Table(
                          cardWrap = TRUE,
                          bordered = TRUE,
                          striped = TRUE,
                          list(
                              headTitles = list(
                              cuk2(countryfossil,textOutput("labelcountryfossil"), "infocountryfossil", "showcountryfossil", NULL, FALSE),
                              cuk2(countrypop, textOutput("labelcountrypop"), "infocountrypop", "showcountrypop", NULL, FALSE),
                              cuk2(countrycost, textOutput("labelcountrycost"),  "infocountrycost", "showcountrycost", NULL, FALSE),
                              cuk2(countrynetcost, textOutput("labelcountrynetcost"), "infocountrynetcost", "showcountrynetcost", NULL, FALSE)
                              # 
                           
                             )
                            
                             )
                          
                        
                      )
                      )
                      ) 
                    )
                  ))),
         # (div(style="overflow-y: scroll; overflow-x: hidden;",
       
        div(id="tuto",
          box(
            style = 'overflow-x: hidden',
            # style = 'overflow-y: scroll',
            
           id = "infobox",
           width = NULL,
           title =textOutput("infot"),
           closable = TRUE,
           closed=TRUE,
           collapsible = FALSE,
          textOutput("info"),
           ignore.init=TRUE
         # ))
         )))),

# overflow-x: visible;
# overflow-x: hidden;
# overflow-x: clip;
# overflow-x: scroll;
# overflow-x: auto;


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
                
                # p(HTML(label, "<font size='3'>",
                #        as.character(actionLink(style='color:#ffc107', inputId = id, 
                #                                label = "  ", 
                #                                icon = icon("far fa-hand-paper"))), "</font>")), 
                
                # p(HTML(label, "<font size='3'>",
                #        as.character(actionLink(style='color:#ffc107', 
                #                                label =textOutput("simuset"), 
                #                                icon = icon("far fa-hand-paper"))), "</font>")), 
                
                div(
                  # HTML(
                  #   "<body style='background-color:aquamarine;'>", 
                  #   "<font size='4.5'>", 
                  #    "<b>" ,
                  #   as.character(icon("far fa-hand-paper")), textOutput("simuset"), 
                  #    "</b>",
                  #   "</font>",
                  #   "</body>"),
                # tagList(
                p(uiOutput("simuset")
                ),
                  # ,
                  # p(HTML(textOutput("simuset"), "<font size='3'>", 
                  #        as.character(
                  #                                icon = icon("far fa-hand-paper")), "</font>")), 
                  # h5("Set values:"),
uiOutput("simpan"),
div(id="ress",
    actionButton("reset", "Reset")),

                  uiOutput("cou")
                  # ) )
                )

         )
,checkboxInput("showtab", "Show the simulation results also in table below")

  ),
  
  
  column(9, id ="bor",
         #
         column(12, id = "bor3",  
                
                
             
              uiOutput("yearcui", width="auto"),
              
                                
                conditionalPanel(condition="input.view ==1",
                                  # div( style = 'overflow-y: scroll',
                                 
                                 uiOutput("splot", width="auto"
                                 )
                                 # )
                )
                # , conditionalPanel(condition="input.view ==2",
                #                    # div( style = 'overflow-y: scroll',
                #                    uiOutput("yearcui2", width="auto")
                #                    # )
                # )
#                 
               , conditionalPanel(condition="input.view ==2",
                                  div( style = 'overflow-y: auto',
                                       # uiOutput("yearcui2", width="auto")
                                       # ,
                                 uiOutput("plotjj", width="auto"
                                 )
)
                )


             ,  conditionalPanel(condition="input.view ==3",
                                div( style = 'overflow-y: scroll',
                                     # uiOutput("yearcui2", width="auto"),
                                # uiOutput("ssplot2", width="auto"
                                         uiOutput("plotkk", width="auto"
                                                  
                                          )

               )
               )


                                 
                )
                

                # ,uiOutput("yearcui")
                )
           
         
  )                 ,




conditionalPanel(
  
  condition="input.showtab == 1",
  fluidRow(column(width =12, align = "center",   DT::dataTableOutput("tably", width="100%")
                  # ,style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
                  )),
  
)
# ,
# fluidRow(column(width =12, align = "center",   DT::dataTableOutput("tablx", width="100%")
#                 # ,style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
# ))
# ,conditionalPanel(

  # condition="input.view == 1",
  # ,fluidRow(column(width =12, align = "center",    textOutput("hovers")
  #                 # ,style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
  # )
  # )
  
# )
   )                    
 )




