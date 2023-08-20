


ui <- function(request) {dashboardPage(dark=TRUE, fullscreen=TRUE, scrollToTop=TRUE,help = NULL, 
                    
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
        (div(style="font-size: 15px !important;  margin-right: 5px; color: var(--colink);",p("feedback"))
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
                     
                     tags$head(
                       # taken from accepted answer
                       tags$script(
                         "$(document).on('shiny:inputchanged', function(event) {
          if (event.name != 'changed') {
            Shiny.setInputValue('changed', event.name);
          }
        });"
                       )
        # through to here
                     ),
                     
                     
                     
                     tags$script(HTML("
                     

$(document).ready(function(){
    $('.nav li.active a').addClass('active');
});




                    ")),


# tags$head(tags$script(HTML("$(document).on('click', function () {
#                                 Shiny.onInputChange('last_btn',this.id);
#                              });"))),

tags$head(tags$script(HTML("$(document).on('click', '.needed', function () {
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


# tags$script(HTML("
#                      
#        
# $(document).on('shiny:connected', function(event) {
#         let check = false;
#         (function(a){if(/() check = true;});
#         Shiny.setInputValue(id = 'isMobile', value = check);
#       });
# 
# 
# 
#                     ")),
# 
# 
# tags$script(HTML("
#                      
#        
# $(document).on('shiny:connected', function(event) {
#        Shiny.setInputValue(
# 'dark',
# ('#customSwitch1').checked;
# 
#        );
#       
# })
#                     ")),


tags$script(HTML("
                     
       
$(document).on('shiny:connected', function(event) {

if ($('#customSwitch1').is(':checked')) {

Shiny.setInputValue(id = 'dark', value = true);
}


})
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
uiOutput("stylerota"),

# uiOutput("cssroot"),

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

# fluidRow(
  
  div(  id = "bor",
  fluidRow(   
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
           ) })),  
    
         

         
         # div(id="tuto",


# tabsetPanel(
#   id = "tabset",
#   tabPanel("panel 1", "one"),
#   tabPanel("panel 2", "two"),
#   tabPanel("panel 3", "three")
# ),
#     
         fluidRow(
           
           column(2, style = " padding: .2vw;",  
                  # div(
                  column(12,id = "bor5",
                 
                         tagList( uiOutput(style = "font-size:  calc(.6rem + .9vw); font-weight: 600; text-align: center;","graview")
                         ),
                            div(style="background-color: var(--colbtable); padding: .5vw;",
     
                         
                  radioButtons("view", label = "Graph separation",
                               c("One graph for all scales" = 1,
                                 "Side by side graphs, two in row" = 2,
                                 "Back to back graphs" = 3,
                                 "Side by side graphs, all in one row" = 4
                                 
                               ),selected=4, inline=TRUE)
                  
                  
                  # ,sliderInput("hei", label="Graph height", min=.5, max=1.5, value =1, step =.01 )
                  # ,sliderInput("fonts", label = "Graph tont size",min=.5, max=1.5, value =1, step =.01 )
                  
                  # 
                  # sliderInput("conb",
                  #             inf("Convergence of countries emissions", "infoconvergence2"), 
                  #             min = .01, max = 1, value = .5, step=.01)

                  # ,div(style="display:inline-block; font-weight:100;",
                  #      awesomeCheckbox("startvalue", label=textOutput("startlabel"), value=TRUE))

                  
                  # ,
                  # 
                  # sliderInput("fonts", label ="Font size", min = 1, max = 10, value = c(5), dragRange=FALSE, ticks = FALSE),
                  # hr(),
                  
                  
                  # )
                            )
                  
                  )      
           ),
           
           column(10,style = " padding: .2vw;",  
        
     column(12,id = "bor5",

                # "</font>",
                # tagList(uiOutput("selitys2")),  
                # "<i class='material-icons'>&#xe925;</i>", as.character(icon("pan_tool")),
                # accordion(
                #   id = "accordion5",
                #   accordionItem(
                #     # <i class="material-icons">visibility</i>
                #     title = tagList(uiOutput("viewtext")),       
                    
                    # title = HTML("<font size='4'>",
                    #              as.character(icon("far fa-eye")), 
                    #              
                    #         "  VIEWING SETTINGS  ",  
                    #        as.character( icon("fas fa-caret-square-down")),"</font>"),
                    # collapsed =FALSE,
      fluidRow(
        # column(12,
        # column()
        # style = "align:center; "    ,
      # tagList(uiOutput(style = 'text-align: center', "viewtext"))
      tagList( uiOutput(style = "font-size:  calc(.6rem + .9vw); font-weight: 600; text-align: center; padding-left: 2vw;", "simuset")
         ),
     
     div(id="ress",
         actionButton("reset", "Reset")
         # , bookmarkButton(label="Share")
         
         
         )
     # )
      ),
     
                     fluidRow(
        
                       
                       
                       uiOutput("simpan"),
                       



                    #    ) 
                    # )
                  )
           ))
     

     
     
      ),
         # (div(style="overflow-y: scroll; overflow-x: hidden;",

div(id="tuto",
    box(
      style = 'overflow-y: scroll',
      # style = 'overflow-y: scroll',
      
      id = "tutobox",
      width = NULL,
      title =textOutput("tutorit"),
      closable = TRUE,
      closed=TRUE,
      collapsible = FALSE,
      uiOutput("tutori"),
      ignore.init=TRUE
      # ))
    )),

       
        div(id="tuto",
          box(
            style = 'overflow-x: hidden, font-size: .5vw',
            # style = 'overflow-y: scroll',
            
           id = "infobox",
           width = NULL,
           title =textOutput("infot"),
           closable = TRUE,
           closed=TRUE,
           collapsible = FALSE,
         uiOutput("info"),
           ignore.init=TRUE
         # ))
         )),

# ),

# overflow-x: visible;
# overflow-x: hidden;
# overflow-x: clip;
# overflow-x: scroll;
# overflow-x: auto;


# fluidRow(id="bor5",style = 'padding-top: .5vw !important; padding-bottom: .3vw !important;', 
#   column(2, div(
# 
#          # p(style = 'text-align: right',"Observe specific year:")
#          tagList(uiOutput(style = 'font-weight: 1000 !important; font-size: calc(.6vw + .54rem); text-align: right;', "obsyear")
#          )  
#          )),
#   
#   
#   
#   column(10,         div(
#     uiOutput("yearcui", width="auto")))
# ),


fluidRow(
  
  column(2, style = " padding: .2vw;",  
          # id ="bor",
         
  

         
         column(12, id = "bor4",
                
                
                # bs4Dash::bs4Table(
                #   # "nerf",
                #   
                #   cardWrap = TRUE,
                #   bordered = TRUE,
                #   striped = TRUE,
                #   
                #   list(
                #     
                #     headTitles = list(
                #       cuk2(fos, textOutput("labelfossil"), "infofossil", "showfossil", NULL, TRUE),
                #       cuk2(lul, textOutput("labelland"),  "infolul", "showland", NULL, TRUE),
                #       cuk2(net,  textOutput("labelnet"), "infonet", "shownet", NULL, TRUE)
                # 
                #       
                #       
                #       
                #       
                #     )
                #     
                #   )
                # ),
                
                div(
                # id = "bor5a",
                
                div(style='

text-align: left !important;
text-decoration: none;
margin-bottom: .0rem;
margin-top: -.2rem;',

tagList(uiOutput(style = "font-size:  calc(.6rem + .9vw); font-weight: 600; text-align: center;", "viewtext"))),
# textOutput("indicatorvisibility"),
div(style="text-align: right;",textOutput("yearc")),
                
                div(
                  id="tablu",
                  class="noku",
                  # tämä renderUIlla?
                  
                  
                  
            
                 p(" "),
                bs4Dash::bs4Table(
                  cardWrap = TRUE, bordered = TRUE,striped = FALSE,
                  list( headTitles = list(
                      cuk2(fos, textOutput("labelfossil"), "infofossil", "showfossil", NULL, TRUE)   
                    # ) , list(rv$fossill) )  ), 
                 )  
                 ,   list(uiOutput("fossill") )
                 )), 

                bs4Dash::bs4Table(
                  cardWrap = TRUE, bordered = TRUE,striped = TRUE,
                  list( headTitles = list(
                    cuk2(lul, textOutput("labelland"),  "infolul", "showland", NULL, TRUE) 
                  )  
                  ,   list(uiOutput("landl") )
                  
                  )  ), 
                
                bs4Dash::bs4Table(
                  cardWrap = TRUE, bordered = TRUE,striped = TRUE,
                  list( headTitles = list(
                    cuk2(net,  textOutput("labelnet"), "infonet", "shownet", NULL, TRUE)   
                  ) 
                  ,   list(uiOutput("netl") )
                  
                  )  )   ),
                
                
                div(
                  #table joka näkyy jos averagedividend tai countrydividend relevantteja
                  id="tablu4",
                  class="noku",
                  # width ="50%",
                  
                  bs4Dash::bs4Table(
                    cardWrap = TRUE, bordered = TRUE,striped = TRUE,
                    list( headTitles = list(
                      cuk2(non, textOutput("labelnonco2"),  "infononco2", "shownonco2", NULL, FALSE)
                    )   ,   list(uiOutput("nonco2l") )
                    )  ), 
                  
                  bs4Dash::bs4Table(
                    cardWrap = TRUE, bordered = TRUE,striped = TRUE,
                    list( headTitles = list(
                      cuk2(tot, textOutput("labelghg"),  "infoghg", "showghg", NULL, FALSE)
                    )
                    ,   list(uiOutput("ghgl") )
                    )  )
                  
                ), 
  
                 p(" "),
div(
  id="tablu",
  class="noku",
  # tämä renderUIlla?
  
                bs4Dash::bs4Table(
                  cardWrap = TRUE, bordered = TRUE,striped = TRUE,
                  list( headTitles = list(
                    cuk2(pop,textOutput("labelpop"),  "infopop", "showpop", NULL, FALSE)
                  )    ,   list(uiOutput("popl") ) )  )), 
                
                
                div(
                  id="tablu2",
                  class="noku",
                  # width ="50%",
                  
                  bs4Dash::bs4Table(
                    cardWrap = TRUE, bordered = TRUE,striped = TRUE,
                    list( headTitles = list(
                      cuk2(countrypop, textOutput("labelcountrypop"), "infocountrypop", "showcountrypop", NULL, FALSE)
                    ) 
                    ,   list(uiOutput("countrypopl") )
                    )  )
               
                ),
                
p(" "),

div(
  id="tablu",
  class="noku",
  # tämä renderUIlla?
  
                
                bs4Dash::bs4Table(
                  cardWrap = TRUE, bordered = TRUE,striped = TRUE,
                  list( headTitles = list(
                    cuk2(fpop, textOutput("labelavgfossil"),  "infoavgfossil", "showavgfossil", NULL, TRUE)
                  )
                  ,   list(uiOutput("avgfossill") )
                  )  ), 
                bs4Dash::bs4Table(
                  cardWrap = TRUE, bordered = TRUE,striped = TRUE,
                  list( headTitles = list(
                    cuk2(cpop,textOutput("labeluserfossil"), "infouserfossil", "showuserfossil", NULL, FALSE)
                  )
                  ,   list(uiOutput("userfossill") )
                  )  )),
                
                
                div(
                  id="tablu2",
                  class="noku",
                  # width ="50%",
                  
                  
                  bs4Dash::bs4Table(
                    cardWrap = TRUE, bordered = TRUE,striped = TRUE,
                    list( headTitles = list(
                      cuk2(countryfossil,textOutput("labelcountryfossil"), "infocountryfossil", "showcountryfossil", NULL, FALSE)
                    )
                    ,   list(uiOutput("countryfossill") )
                    )  ), 
                  
              
                  
                ),
                
                p(" "),
div(
  id="tablu",
  class="noku",      
                bs4Dash::bs4Table(
                  cardWrap = TRUE, bordered = TRUE,striped = TRUE,
                  list( headTitles = list(
                    cuk2(tax,textOutput("labelprice"),  "infoprice", "showprice", NULL, FALSE)
                  )
                  
                  ,   list(uiOutput("pricel") )
                  )  ), 
                
                p(" "),
                
                bs4Dash::bs4Table(
                  cardWrap = TRUE, bordered = TRUE,striped = TRUE,
                  list( headTitles = list(
                    cuk2(avgcost, textOutput("labelavgcost"), "infoavgcost", "showavgcost", NULL, FALSE)
                  )
                  ,   list(uiOutput("avgcostl") )
                  )  ), 
                bs4Dash::bs4Table(
                  cardWrap = TRUE, bordered = TRUE,striped = TRUE,
                  list( headTitles = list(
                    cuk2(dividend, textOutput("labeldividend"),  "infodividend", "showdividend", NULL, FALSE)
                  )
                  ,   list(uiOutput("dividendl") )
                  
                  )  ), 
                bs4Dash::bs4Table(
                  cardWrap = TRUE, bordered = TRUE,striped = TRUE,
                  list( headTitles = list(
                    cuk2(avgnetcost, textOutput("labelavgnetcost"), "infoavgnetcost", "showavgnetcost", NULL, FALSE)
                  )
                  ,   list(uiOutput("avgnetcostl") )
                  
                  )  ), 
                bs4Dash::bs4Table(
                  cardWrap = TRUE, bordered = TRUE,striped = TRUE,
                  list( headTitles = list(
                    cuk2(taxfosindi, textOutput("labelusercost"),  "infousercost", "showusercost", NULL, FALSE)
                  )
                  ,   list(uiOutput("usercostl") )
                  
                  )  ), 
                bs4Dash::bs4Table(
                  cardWrap = TRUE, bordered = TRUE,striped = TRUE,
                  list( headTitles = list(
                    cuk2(netcost,textOutput("labelnetcost"),  "infonetcost", "shownetcost", NULL, FALSE)
                  )
                  ,   list(uiOutput("netcostl") )
                  
                  )  )),
                
      
         
         
         
         
         
         
         
div(
  id="tablu2",
  class="noku",
  # width ="50%",
  
  
 
  
  bs4Dash::bs4Table(
    cardWrap = TRUE, bordered = TRUE,striped = TRUE,
    list( headTitles = list(
      cuk2(countrycost, textOutput("labelcountrycost"),  "infocountrycost", "showcountrycost", NULL, FALSE)
    )
    ,   list(uiOutput("countrycostl") )
    
    )  ), 
  
  bs4Dash::bs4Table(
    cardWrap = TRUE, bordered = TRUE,striped = TRUE,
    list( headTitles = list(
      cuk2(countrynetcost, textOutput("labelcountrynetcost"), "infocountrynetcost", "showcountrynetcost", NULL, FALSE)
    )
    ,   list(uiOutput("countrynetcostl") )
    
    )  ), 
  
  
),
         
         
         
         
         
         
         
         

         
         
         
         div(
           #table joka näkyy jos averagedividend tai countrydividend relevantteja
           id="tablu3",
           class="noku",
           # width ="50%",
           
           bs4Dash::bs4Table(
             cardWrap = TRUE, bordered = TRUE,striped = TRUE,
             list( headTitles = list(
               cuk2(averagedividend, textOutput("labelaveragedividend"),  "infoaveragedividend", "showaveragedividend", NULL, FALSE)
             )
             ,   list(uiOutput("averagedividendl") )
             
             )  ), 
           bs4Dash::bs4Table(
             cardWrap = TRUE, bordered = TRUE,striped = TRUE,
             list( headTitles = list(
               cuk2(countrydividend, textOutput("labelcountrydividend"),  "infocountrydividend", "showcountrydividend", NULL, FALSE)
             )
             ,   list(uiOutput("countrydividendl") )
             
             )  ) 
           

           
         )
,
div(style="display:inline-block;",id="ress",actionButton(inputId ="showall", label = textOutput("showall") )),
div(style="display:inline-block;",id="ress",actionButton(inputId ="shownone", label = textOutput("shownone"))
),
div(style="display:inline-block;",id="sla",
    checkboxInput("visib",  label=textOutput("automatic"), value=TRUE)
)
,div(id="sla",
     checkboxInput("startvalue", label=textOutput("startlabel"), value=TRUE))
       
# div(id="ress",
#     actionButton("reset", "Reset"))
# # )
#                 ),
         




         ),
         

                div(
     

                  uiOutput("cou")
                  # ) )
                )

         )

  ),
  
  
  column(10,style = " padding: .2vw;",  
         #
         column(12, id = "bor5",  
                # fluidRow(style = "justify-content: flex-start;",
                #          column(3,
                #                 p(uiOutput(style = "margin-left: 2vw; font-size: calc(.6rem + .9vw); font-weight: 600; text-align: center;", "simuresults")
                #                 )),
                #          column(2,
                #   div(style="margin-left: 1vw; display:inline-block;",id="ress", actionButton("go", "Redraw")),
                #          # ),
                #   # column(1,
                #   div(id="sla",style="margin-top: .4vw; margin-left: .56vw; display:inline-block;",
                #       checkboxInput("autodraw",  label="Auto-redraw", value=FALSE)
                #   )) ,       
                # 
                # 
                # # fluidRow(style = 'padding-top: .5vw !important; padding-bottom: .3vw !important;', 
                # column(7,  
                #        fluidRow(
                #        column(4, div(
                #            
                #            # p(style = 'text-align: right',"Observe specific year:")
                #            tagList(uiOutput(style = 'font-weight: 1000 !important; font-size: calc(.6vw + .54rem); text-align: right;', "obsyear")
                #            )  
                #          )),
                #          
                #          
                #          
                #          column(8,         div(
                #            uiOutput("yearcui", width="auto")))
                #        )
                # # ),
                # )
                # ),
                fluidRow(style = "justify-content: flex-start;",
                         column(4,
                         div(style="display:inline-block;",
                                p(uiOutput(style = "margin-left: 2vw; font-size: calc(.6rem + .9vw); font-weight: 600; text-align: center;", "simuresults")
                                )),
                         div(style="display:inline-block;",
                                div(style="margin-left: 3vw; display:inline-block; margin-top: -10.6vw !important;",id="ress", actionButton(style="margin-top: -.6vw !important;","go", textOutput("redraw"))),
                                # ),
                                # column(1,
                                div(style="margin-top: .5vw; margin-left: .23vw; display:inline-block;",
                                    checkboxInput("autodraw",  
                                                  # label=textOutput(style="display:inline-block;","autoredraw"), 
                                                  label="Auto", 
                                                  
                                                    value=FALSE)
                                )
                             ) 
                         ),       
                         
                         column(8,
                                # fluidRow(
                                  
                         # fluidRow(style = 'padding-top: .5vw !important; padding-bottom: .3vw !important;', 
                         div(
                           # style="display:inline-block;",  
                                 fluidRow(
                                   column(4,
                                         div(
                                    
                                    # p(style = 'text-align: right',"Observe specific year:")
                                    tagList(uiOutput(style = 'font-weight: 1000 !important; font-size: calc(.6vw + .54rem); text-align: right;', "obsyear")
                                    )  
                                   )
                                  ),
                                  
                                  
                                  
                                   column(8,         
                                         div(
                                            style="padding-top: 0.4vw; padding-right: 0.4vw; margin-bottom: 0.9rem;",  
                                    uiOutput("yearcui", width="auto"))
                                     )
                                )
                                # ),
                          )
                         )
                ),
                
                div(id = "rota", class = "rota",
             # div(
             #  uiOutput("yearcui", width="auto")),
              
                                
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

               , conditionalPanel(condition="input.view ==2",
                                  div( style = 'overflow-y: auto',
                                       # uiOutput("yearcui2", width="auto")
                                       # ,
                                 uiOutput("plotjj", width="auto"
                                 )
)
                )

# , conditionalPanel(condition="input.view ==2",
#                    div( style = 'overflow-y: auto',
#                         # uiOutput("yearcui2", width="auto")
#                         # ,
#                         plotOutput("plotj", width="auto"
#                         )
#                    )
# )

             ,  conditionalPanel(condition="input.view ==3",
                                div( style = 'overflow-y: scroll',
                                     # uiOutput("yearcui2", width="auto"),
                                # uiOutput("ssplot2", width="auto"
                                         uiOutput("plotkk"
                                                   , width="auto"
                                          )
               )
               )


,  conditionalPanel(condition="input.view ==4",
                    div( 
                        style = 'overflow-x: scroll',
                         # uiOutput("yearcui2", width="auto"),
                         # uiOutput("ssplot2", width="auto",
                       
                         uiOutput("plotll"
                                   # , width="100%"
                                  , height="auto"
                                  # ,
                                  # height =="auto"
                                  
                         )
                         
                    )
)
                )
# 
,div(style = " background-color:var(--colb)!important; font-size: 1.2vw", p("www.globalcarbonprice.com _____  Data: UN, IPCC 2021, IPCC 2022, Friedlingstein et al. 2022, Gütschow, J.; Pflüger, M. 2022:"))

                                 
                )
,div(id="sla"
     ,checkboxInput("showtab", "Show the simulation results also in table below")
     # ,
     # textOutput("pul")
   
     
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
# ,textOutput("last")
# ,textOutput("lastButtonCliked")
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
 )}




