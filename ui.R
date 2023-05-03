


ui <- dashboardPage(dark=TRUE, fullscreen=TRUE, scrollToTop=TRUE,
                    
                    dashboardHeader(
                      # div(style="padding-left: 4px;",
                                    title = " Global carbon price and dividend -simulator (UNFINISHED, contains errors)"
                                    # )
                    ,
                                    # title="ffff",
                                    tagList(a
                                            # (HTML("<font size='4'>", as.character
                                              (div(style="font-size: 15px !important; margin-right: 5px;",icon("fab fa-github"))
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
                                            )
                                    ,
                                   actionBttn(inputId ="infodata", label = "Data sources", size="xs", style = "fill", color="primary")
                                    
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

hr {border-top: 2px solid white; margin-bottom:3px; margin-top:3px;}

.dark-mode .card {
background-color: rgb(45, 36, 36);

}

.dark-mode .content-wrapper {
background-color: rgb(45, 36, 36);
color: #fff;
}
//
.content-wrapper>.content {
padding: .3rem !important;
}


.custom-control-input { 
visibility: hidden !important;

}

.custom-control-input:checked~.custom-control-label::before {
    color: #fff;
    border-color: #007bff;
    background-color: #007bff;
    box-shadow: none;
}
.dark-mode .custom-control-label::before, .dark-mode .custom-file-label, .dark-mode .custom-file-label::after, .dark-mode .custom-select, .dark-mode .form-control, .dark-mode .input-group-text {
    background-color: #343a40;
    color: #fff;
}
.custom-switch .custom-control-label::before {
visibility: hidden !important;

}

.custom-switch .custom-control-label::after { 
visibility: hidden !important;

}
.custom-switch {
visibility: hidden !important;

}

// yläreunan valikko {}

.label .radio-inline {
font-size: .45rem;

}

p {
margin-bottom: .1vw;
}

 .card-body {
               padding: .2vw;
    background-color:rgb(45, 36, 36) !important;

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
font-family: Roboto !important;
font-size: calc(.7vw + .4rem) !important;

line-height: 1.1;
margin -.4vw -.4vw -.4vw -.4vw; padding: -.4vw  -.4vw -.4vw -.4vw;
border-collapse: collapse;
overflow: auto;
}

#tablu p {
color: blue;
font-family: Roboto !important;
font-size: calc(.7vw + .4rem) !important;
text-decoration: none !important;
margin-bottom: .2rem !important;
margin-top: .1rem !important;

line-height: 1.1;
}

#tablu2 {
color: blue;
font-family: 'Saira' !important;
font-size: calc(.7vw + .4rem) !important;

line-height: 1.1;
margin -.4vw -.4vw -.4vw -.4vw; padding: -.4vw  -.4vw -.4vw -.4vw;
border-collapse: collapse;
overflow: auto;
}

#tablu2 p {
color: blue;
font-family: Roboto !important;
font-size: calc(.7vw + .4rem) !important;
text-decoration: none !important;
margin-bottom: .2rem !important;
margin-top: .1rem !important;
line-height: 1.1;
}

#tablu3 {
color: blue;
font-family: 'Saira' !important;
font-size: calc(.7vw + .4rem) !important;

line-height: 1.1;
margin -.4vw -.4vw -.4vw -.4vw; padding: -.4vw  -.4vw -.4vw -.4vw;
border-collapse: collapse;
overflow: auto;
}

#tablu3 p {
color: blue;
font-family: Roboto !important;
font-size: calc(.7vw + .4rem) !important;
text-decoration: none !important;
margin-bottom: .2rem !important;
margin-top: .1rem !important;
line-height: 1.1;
}



          .table {
              margin-bottom: .1vw;
overflow: scroll;
               }
               
               .table td {
               padding: .1vw;
              margin-bottom: .1vw;
    background-color:rgb(45, 66, 86) !important;
border-color: rgb(224, 192, 151) !important;
               }

               .td {
               padding: .1vw;
              margin-bottom: .1vw;
display: inline-block;
               }


 input[type='awesome-checkbox']{ /* style for checkboxes */
        width: 60px !important; /*Desired width*/
        height: 30px; /*Desired height*/
        line-height: 30px; 
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
 border-color: rgb(92, 61, 46);
border-bottom-color: none;
            border-width: .3vw;
 border-style: solid;
  border-radius: 5px;
padding: .3vw !important;


}



#bor4 {
 border-color: rgb(184, 92, 56);
border-bottom-color: none;
            border-width: .3vw;
 border-style: solid;
  border-radius: 5px;
padding: .3vw !important;


}

#bor5 {
 border-color: rgb(224, 192, 151);
border-bottom-color: none;
            border-width: .3vw;
 border-style: solid; 
  border-radius: 5px;
padding: .0vw !important;

}


#bor5a {
    background-color: rgba(224, 192, 151,.1) !important;
  border-color:rgb(224, 192, 151) !important;
padding: .5vw !important;
margin: .0vw !important;
box-sizing: border-box;
 border-style: solid; 
  border-radius: 5px;
            border-width: .2vw;
}


#bor5a .control-label {
color: rgba(224, 192, 151,.99);
font-size: calc(.6vw + .7rem);
text-align: center !important;
text-decoration: none;
margin-bottom: .7rem;
}

#bor5a p {
color: rgba(224, 192, 151,.99);
font-size: calc(.7vw + .7rem);
text-align: left !important;
text-decoration: none;
margin-bottom: .5rem;
margin-top: -.3rem;
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


// left panel texts    border-color:rgb(45, 36, 36) !important;
 {}

    a {
color: #f2ede6;
padding-top: .2vw;
padding-bottom: .2vw;

padding-left: .0vw;
margin-left: .0vw;
font-size: calc(.5vw + .44rem);
line-height: 1.0rem;
font-weight: 1000 !important;

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
    background-color: rgb(184, 92, 56) !important;
    display:block !important;
    } 

// nav extend left selection field backgrounds to whole lenght
and make vertical instead of horizontal {}             
  .btn {
font-size: .8rem;
}
          
         .nav{
flex-wrap: wrap-reverse;
  flex-direction: column;
text-align: left;
padding: .0vw;
margin: .0vw;
line-height: 1.2rem;

}

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

//
.content-wrapper .nav li .active {
    border-top: 2px solid red;
    border-bottom: 2px solid red;
}

// headlines {}

.control-label {
 
font-size: calc(.5vw + .5rem);
line-height: .99rem;
margin-bottom: 1rem;
margin-top: .3;

}


// right panel active {}


  input[type=\"number\"] {
    width: max(6vw, 52px);
padding: 0.1rem 0.1rem;
font-size: .9rem;
height: calc(.75rem+2px);

  }


.tab-pane.active{
    background-color: rgb(184, 92, 56);
               margin-left: -0.1vw;
padding: .5vw;
font-size: calc(.4vw + .45rem) !important;
font-weight: 100 !important;


}

#sla label{ display: table-cell; text-align: left; vertical-align: middle; 
 width: 40px !important;} 
#sla .form-group { display: table-row; horizontal-align: right;
 width: 40px !important;
max-width: 40px;

}
#sla shiny-input-container:not(.shiny-input-container-inline){
 width: 2rem !important;
max-width: 2rem;
}
#sla .div{
 width: 2rem !important;
}
@media all and (min-device-width: 480px) {

#sla p {width: 8.1vw !important; }
}
@media all and (max-device-width: 480px) {
 #sla p {
   width: 38.1vw !important;
  }
}


.form-check-label {
font-weight: 100 !important;

}
label:not(.form-check-label):not(.custom-file-label) {

}

.radio {

line-height: 1;
margin-bottom: .0rem;
font-weight: 100 !important;

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



.irs-max {
height: 0px !important;
visibility: hidden !important;
}

.irs-min {
height: 0px !important;
visibility: hidden !important;
}

 .irs--shiny .irs-single {
font-size: calc(.6vw + .6rem);
z-index: 2;
 }

.irs--shiny .irs-bar {
border-top: 1px solid rgb(92, 61, 46);
border-bottom: 1px solid rgb(92, 61, 46);
}

 .irs--shiny .irs-handle {
color: #428bca !important;
background-color: #428bca;
 }


#yearc .irs-handle {
color: black !important;


}

.dataTables_scrollBody {
    transform:rotateX(180deg);

}
.dataTables_scrollBody table {
    transform:rotateX(180deg);

}



div.datatables {
color: white;
}

table.dataTable tbody tr {
background-color: rgb(92, 61, 46);
}

table.dataTable.display tbody tr.odd {
background-color: rgb(184, 92, 56);

}


table.dataTable.display tbody tr:hover {
background-color: #1d89ff;

}

button.dt-button {

color: white;
}

tbody {
vertical-align: left;
}

pre {
padding: .1px;
margin: .1px;
overflow-y: scroll; 
}

.irs--shiny .irs-line {
height: 16px

}
fas.ca-info-circle {
color: black;


}

.fa a {
color: black;

}

.btn {
    font-size: 1.8rem;
}

#countrr .btn {
    font-size: .8rem;
}

#vuo .control-label {
margin-bottom: 0.2rem !important;

}


               ")
)

,
uiOutput("css_style"),
uiOutput("css_style2"),

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
        
         column(12, id = "bor5",

                
                accordion(
                  id = "accordion5",
                  accordionItem(
                    # <i class="material-icons">visibility</i>
                    
                    title = HTML("<font size='4'>",
                                 as.character(icon("far fa-eye")), 
                                 # "</font>",
                                 
                                 # "<i class='material-icons'>&#xe925;</i>", as.character(icon("pan_tool")),
                                 "  VIEWING SETTINGS  ",  as.character( icon("fas fa-caret-square-down")),"</font>"),
                    collapsed =TRUE,
                     fluidRow(
                      column(2,        id = "bor5a",
                             
                             radioButtons("view", label = "Graph view",
                                          c("Single graph for all scales" = 1,
                                            "Separate graphs for each scale (in progress...)" = 2
                                            
                                          ),selected=1, inline=TRUE)
                             
                             ,
                             div(style="display:inline-block; font-weight:100;",
                                 awesomeCheckbox("startvalue", label="Start year labels", value=TRUE))
                             # ,
                             # 
                             # sliderInput("fonts", label ="Font size", min = 1, max = 10, value = c(5), dragRange=FALSE, ticks = FALSE),
                             # hr(),
                             
                             
                             ),
                      column(10,   
                             id = "bor5a",
                             
                             
                             p(style='font-weight:1000;', "Indicator visibility"),
                             div(style="display:inline-block",actionBttn(inputId ="showall", label = "Show all", size="xs", style = "fill", color="primary")),
                             div(style="display:inline-block",actionBttn(inputId ="shownone", label = "Hide  all", size="xs", style = "fill", color="primary")
                             ),
                             div(style="display:inline-block; font-weight:100;",
                                 awesomeCheckbox("visib", label="Automatic, based on SIMULATION SETTINGS phase", value=TRUE)
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
                      bs4Dash::bs4Table(
                        # "nerf",
                        
                        cardWrap = TRUE,
                        bordered = TRUE,
                        striped = TRUE, 
                        
                        list(
                          
                          headTitles = list(
                            
                            cuk(fos, "Fossil emissions", "infofossil", "showfossil", NULL, TRUE),
                            cuk(lul, "Land emissions/sinks", "infolul", "showland", NULL, TRUE),
                            cuk(net, "Net emissions", "infonet", "shownet", NULL, TRUE),
                            cuk(pop, "Population", "infopop", "showpop", NULL, FALSE),
                            cuk(fpop, "Mean fossil emissions", "infoavgfossil", "showavgfossil", NULL, TRUE),
                            cuk(tax,"Carbon price", "infoprice", "showprice", NULL, FALSE)
                            # cuk(, "", "", "", NULL, TRUE),
                            
                            
                            # ,
                            # 
                            # div(div(style="display:inline-block", chk(fos, "Total emissions")), 
                            #     div(style="display:inline-block",inf("infofossil")),
                            #     div(style="display:inline-block",awesomeCheckbox( "showfossil", label=NULL,  value=TRUE))),
                            # div(div(style="display:inline-block",  chk(lul, "Land emissions/sinks")), 
                            #     div(style="display:inline-block",awesomeCheckbox("showland", label=NULL, value=TRUE))),
                            # div(div(style="display:inline-block",  chk(net, "Net emissions")), 
                            #     div(style="display:inline-block",awesomeCheckbox("shownet", label=NULL, value=TRUE))),
                            # p("Variable:"),
                            # 
                            #  chk(lul, "Land emissions/sinks", "infolul"),
                            # chk(net, "Net emissions", "infonet"),
                            # chk(pop, "Population", "infopop"),
                            # chk(fpop, "Mean emissions", "infoavgfossil"),
                            # chk(tax, "Carbon tax", "infoprice")
                            
                            
                          # )
                          # ,
                          # #
                          # #
                          # list(
                          #   p("Visibility:"),
                          #   awesomeCheckbox( "showfossil", label=NULL,  value=TRUE),
                          #   awesomeCheckbox("showland", label=NULL, value=TRUE),
                          #   awesomeCheckbox("shownet", label=NULL, value=TRUE),
                          #   awesomeCheckbox("showpop", label=NULL, value=FALSE),
                          # 
                          #   awesomeCheckbox("showavgfossil", label=NULL, value=TRUE),
                          #   awesomeCheckbox("showprice", label=NULL, value=FALSE)

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

                            cuk(avgcost, "Mean carbon costs", "infoavgcost", "showavgcost", NULL, FALSE),
                            cuk(dividend, "Carbon dividend", "infodividend", "showdividend", NULL, FALSE),
                            cuk(avgnetcost, "Mean net costs", "infoavgnetcost", "showavgnetcost", NULL, FALSE),
                            cuk(cpop, "User fossil emissions", "infouserfossil", "showuserfossil", NULL, FALSE),
                            cuk(taxfosindi, "User carbon costs", "infousercost", "showusercost", NULL, FALSE),
                            cuk(netcost, "User net costs", "infonetcost", "shownetcost", NULL, FALSE)
                            
                    


                          )
                     
                          )
                          )

                      )
                    ,
                      
                    
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
                             cuk(averagedividend, "Average national dividend", "infoaveragedividend", "showaveragedividend", NULL, FALSE),
                            cuk(countrydividend, "Dividend for selected country", "infocountrydividend", "showcountrydividend", NULL, FALSE)
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
                              cuk(countryfossil, "Country mean emissions", "infocountryfossil", "showcountryfossil", NULL, FALSE),
                              cuk(countrypop, "Country population", "infocountrypop", "showcountrypop", NULL, FALSE),
                              cuk(countrycost, "Country per capita cost", "infocountrycost", "showcountrycost", NULL, FALSE),
                              cuk(countrynetcost, "Country per capita net cost", "infocountrynetcost", "showcountrynetcost", NULL, FALSE)
                              # 
                           
                             )
                            
                             )
                          
                        
                      )
                      )
                      ) 
                    )
                  ))),
         # (div(style="overflow-y: scroll; overflow-x: hidden;",
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
         ))),

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
                
                
                div(
                  HTML(
                    "<body style='background-color:aquamarine;'>", 
                    "<font size='4.5'>", 
                     "<b>" ,
                    as.character(icon("far fa-hand-paper")), "  SIMULATION SETTINGS   ", 
                     "</b>",
                    "</font>",
                    "</body>"),
                  
                  # h5("Set values:"),
                  navlistPanel(id="nok", 
                               selected = "1. Carbon budget",
                          
                               # HTML("<font size='5'>",
                               #      as.character(icon("far fa-eye")), 
                               #      # "</font>",
                               #      
                               #      # "<i class='material-icons'>&#xe925;</i>", as.character(icon("pan_tool")),
                               #      "  VIEWING SETTINGS  ",  as.character( icon("fas fa-caret-square-down")),"</font>"),
                               # 
                               
                               tabPanel("1. Carbon budget", 
                                        
                                        
                                        #7b396e                                         
                                         
                                         
                                         radioButtons("bud", 
                                                      
                                                     inf("Carbon budget for net CO2 emissions since start of 2020", "infobudget"), 
                                                      # label, size, id
                                                      # 
                                                      # label=p(HTML("Carbon budget for net CO2 emissions since start of 2020" , "<font size='3'>",
                                                      #              as.character(actionLink(inputId = "info3", 
                                                      #                                      label = "  ", 
                                                      #                                      icon = icon("fas fa-info-circle"))), "</font>")),              
                                                      
                                                      # "Carbon budget for net CO2 emissions since start of 2020",
                                                      
                                                      
                                                      c("400Gt (67% likelihood to stay below 1,5C)" = 400,
                                                        "500Gt (50% likelihood to stay below 1,5C)" = 500,
                                                        "1150Gt (67% likelihood to stay below 2,0C)" = 1150,
                                                        "1350Gt (50% likelihood to stay below 2,0C)" = 1350
                                                        # "1700Gt (67% likelihood to stay below 2,4C)" = 1700,
                                                        # "2050Gt (50% likelihood to stay below 2,4C)" = 2050
                                                      ),selected=1150
                                         ),
                                         # hr(),
                                         p("Note: Changing the budget will reset some of the values in this stage and stage 3 to their budget specific defaults"),
                                         hr(),
                                        div(style='margin-bottom:0.2rem important;', sliderInput("vuo", 
                                                     inf("Pricing start year and carbon neutrality year", "infopricing"),
                                                     # label ="Pricing start year and carbon neutrality year", 
                                                     min = 2023, max = 2100, value = c(2024, 2080), dragRange=FALSE, ticks = FALSE)),
                                         hr(),
                                         
                                         tags$div(id="sla",numericInput("paa", 
                                                                        inf("Emissions / sink at the carbon neutrality year", "infoemissionsink"), 
                                                                        
                                                                        # label=p("Emissions / sink at the carbon neutrality year"),
                                                                        min = 0.1, max = 30,step=.1,value=c(6))),
                                         
                                         hr(),
                                         
                                         # uiOutput("muor"),
                                         radioButtons("muo", "Shape of the fossil emission curve",
                                                    choiceNames=   list(
                                                        "Linear" ,
                                                        "Percentual" 
                                                        # ,
                                                        # "Logarithmic drop" = "logarithmic"
                                                      ), 
                                                    choiceValues= list("linear", "percentual"),
                                                    selected = "percentual"
                                                    
                                                    ),
                                         
                                         hr(),
                                         
                                         checkboxInput("advance", "Set emission for year before start year"), 
                                         
                                         conditionalPanel(
                                           
                                           condition="input.advance == 1",
                                           # condition = "(typeof input.df_data_rows_selected !== 'undefined' && input.df_data_rows_selected.length > 0)"
                                           # condition = "(typeof input.indi_rows_selected !== 'undefined' && input.indi_rows_selected.length > 0)",
                                           
                                           
                                           #                           # uiOutput("splot", width="auto"
                                           #                           # )
                                           #          # ,
                                           hr(),
                                           tags$div(id="sla",numericInput("fstart", label=p("Total emissions at year before start year"),min = 0.1, max = 50,step=.1,value=c(37.1))),
                                           
                                            hr(),
                                           tags$div(id="sla",numericInput("lstart", label=p("Land use emissions at year before start year"),min = -5, max = 10,step=.1,value=c(3.9)))
                                           ,p("Note: Emissions from last observed year take a linear trajectory to emissions for year before start year"),
                                           
                                         ),
                                         hr(),
                                         
                                         actionBttn(
                                           inputId = "next1",
                                           label = "   NEXT >",
                                           size="xs",
                                           style = "material-flat", 
                                           color = "primary"
                                         )
                                         
                                         # actionBttn(
                                         #   inputId = "next0",
                                         #   label = "   NEXT >",
                                         #   size="xs",
                                         #   style = "material-flat", 
                                         #   color = "primary"
                                         # )
                                         
                               ),
                               
                               #      
                              
                               
                               tabPanel("2. Population projection",
                                        actionBttn(
                                          inputId = "prev1",
                                          label = "< PREVIOUS",
                                          size="xs",
                                          style = "material-flat", 
                                          color = "primary"
                                        ),
                                        hr(),
                                        
                                        radioButtons("popc", 
                                                     inf("Choose UN population projection", "infopopu"), 
                                                     
                                                     # "Choose UN population projection",
                                                     c(
                                                       "95% range upper limit (12.41B at 2100)" = 5,
                                                       "80% range upper limit (11.65B at 2100)" = 4,
                                                       "Median projection (10.35B at 2100)" = 3,
                                                       "80% range lower limit (9.32B at 2100)" = 2,
                                                       "95% range lower limit (8.84B at 2100)" = 1
                                                     ),selected=3
                                        ),
                                        hr(),
                                        
                                        actionBttn(
                                          inputId = "next2",
                                          label = "NEXT >",
                                          size="xs",
                                          style = "material-flat", 
                                          color = "primary"
                                        )
                               ),
                               
                               
                               
                               tabPanel("3. Carbon price",
                                        actionBttn(
                                          inputId = "prev2",
                                          label = "< PREVIOUS",
                                          size="xs",
                                          style = "material-flat", 
                                          color = "primary"
                                        ),
                                        hr(),
                                        
                                        tags$div(id="sla", numericInput("sprice",
                                                                        inf("Set start year carbon price, $", "infostartprice"), 
                                                                        
                                                               min = 1, max = 1000000,step=1,value=c(40))),
                                        hr(),
                                        
                                        tags$div(id="sla",numericInput("eprice", 
                                                                       inf("Set neutrality year carbon price, $", "infoendprice"), 
                                                                       
      
                                        min = 1, max = 1000000,step=1,value=c(400))),
                                        
                                        
                                        hr(),
                                        
                                        
                                        radioButtons("pri", "Shape of the price curve",
                                                     choiceNames=list(
                                                       "Linear",
                                                       "Percentual"
                                                        ,
                                                        "Logarithmic"
                                                        # ,
                                                        # "Logarithmic drop" = "logarithmic"
                                                     ), 
                                                     choiceValues= list("linear", "percentual", "logarithmic")
                                                     
                                        ),
                                        hr(),
                                        
                                        actionBttn(
                                          inputId = "next3",
                                          label = "NEXT >",
                                          size="xs",
                                          style = "material-flat", 
                                          color = "primary"
                                        )
                               ),
                               tabPanel("4. User emissions", 
                                        actionBttn(
                                          inputId = "prev3",
                                          label = "< PREVIOUS",
                                          size="xs",
                                          style = "material-flat", 
                                          color = "primary"
                                        ),
                                        hr(),
                                        
                                        tags$div(id="sla",numericInput("indi1", label=p("Start year user emissions, t"),min = .01, max = 40,step=.01,value=c(7.77))),
                                        hr(),
                                        
                                        tags$div(id="sla",numericInput("indi2", label=p("Neutrality year user emissions, t"),min = .01, max = 40,step=.01,value=c(0.77))),
                                        
                                        hr(),
                                        
                                        radioButtons("muoindi", "Shape of user emission curve",
                                                     choiceNames=   list(
                                                       "Linear" ,
                                                       "Percentual" 
                                                       # ,
                                                       # "Logarithmic drop" = "logarithmic"
                                                     ), 
                                                     choiceValues= list("linear", "percentual"),
                                                     selected = "percentual"
                                                     
                                        ),
                                        
                                        # radioButtons("muoindi", "Shape of emission curve",
                                        #              c("Linear drop" = "linear","Percentual drop" = "percentual"
                                        #                
                                        #              )),
                                        hr(),
                                        
                                        selectInput("indi",
                                                    inf("ALTERNATIVE: Use country average emission path (this will slower the page considerably) ", "infoconvergence"), 
                                                    
                                                    # label = "ALTERNATIVE: Use country average emission path (this will slower the app considerably)",
                                                    choices =c("none", paaco$country), selected="none"),
                                        # hr(style = "border-top: 1px solid white; margin-bottom:0px; margin-top:0px;;"),
                               
                                        
                                                 
                                        conditionalPanel(

                                condition="input.indi != 'none'",
                               # condition = "(typeof input.df_data_rows_selected !== 'undefined' && input.df_data_rows_selected.length > 0)"
                               # condition = "(typeof input.indi_rows_selected !== 'undefined' && input.indi_rows_selected.length > 0)",


                               #                           # uiOutput("splot", width="auto"
                               #                           # )
                               #          # ,
                                         sliderInput("con",
                                                     
                                                     inf("Convergence of countries emissions", "infoconvergence1"), 
                                                     
                                                     # label ="Convergence of countries' emissions",
                                                     
                                                     min = .01, max = 1, value = .5, step=.01)
                               ),
                               
                               hr(),
                               
                               actionBttn(
                                 inputId = "next4",
                                 label = "EXTRA >",
                                 size="xs",
                                 style = "material-flat", 
                                 color = "primary"
                               )
                               

                               ),
                               tabPanel("EXTRA: Countries",
                                        actionBttn(
                                          inputId = "prev4",
                                          label = "< PREVIOUS",
                                          size="xs",
                                          style = "material-flat", 
                                          color = "primary"
                                        ),
                                        
                                        
                                        hr(),
                                        
                                        sliderInput(
                                          inputId = "national",
                                          inf("Allocate a percentage of collected carbon revenue nationally", "infonationaldiv")
                                          
                                          # label = "Allocate proportion of collected carbon revenue nationally:"
                                          , min = 0, max = 100, value = 0, step=1
                                        ), 
                                        hr(),
                                        conditionalPanel(
                                        condition="input.national != 0",
                                       
                                        
                                        selectInput("nationalcoun", label = "User country of residence for national dividend (This will slower the page considerably)", choices =c("none", paaco$country), selected="none"),
                                        
                               ),
                                        conditionalPanel(
                                          hr(),
                                          
                                          condition="input.countr != '' | input.nationalcoun != 'none'",
                                          # condition = "(typeof input.df_data_rows_selected !== 'undefined' && input.df_data_rows_selected.length > 0)"
                                          # condition = "(typeof input.indi_rows_selected !== 'undefined' && input.indi_rows_selected.length > 0)",
                                          
                                          
                                          #                           # uiOutput("splot", width="auto"
                                          #                           # )
                                          #          # ,
                                          sliderInput("conb",
                                                      inf("Convergence of countries emissions", "infoconvergence2"), 
                                                      
                                                      # label ="Convergence of countries' emissions", 
                                                      min = .01, max = 1, value = .5, step=.01)
                                          
                                        ),
                                        
                                        
                               hr(),
                               tags$div(id ="countrr",
                                        
                                        pickerInput(
                                          inputId = "countr",
                                          label = "Show indicators for specific country/countries (This will slower the page considerably)",
                                          choices = c(paaco$country),
                                          selected= NULL,
                                          options = list(
                                            `actions-box` = TRUE),
                                          multiple = TRUE
                                        )),
                                        # hr(),
                                        # 
                                        # actionBttn(
                                        #   inputId = "next5",
                                        #   label = "more EXTRA >",
                                        #   size="xs",
                                        #   style = "material-flat", 
                                        #   color = "primary"
                                        # )
                                        
                                         ),
                               
                               # tabPanel("EXTRA: National dividend",
                               #          actionBttn(
                               #            inputId = "prev5",
                               #            label = "< PREVIOUS",
                               #            size="xs",
                               #            style = "material-flat", 
                               #            color = "primary"
                               #          ),
                               #          # hr(),
                               #          # 
                               #          # sliderInput(
                               #          #   inputId = "national",
                               #          #   label = "Allocate proportion of collected carbon revenue nationally:"
                               #          #   , min = 0, max = 100, value = 0, step=1
                               #          # ), 
                               #          
                               #          hr(),
                               #          
                               #          selectInput("nationalcoun", label = "User country of residence for national dividend (Note: selecting this will slower the app considerably)", choices =c("none", paaco$country), selected="none"),
                               #          
                               #          
                               #          )      
                               # 
                  ),
                  uiOutput("cou")
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
                
                
                uiOutput("yearc")),
           # DT::dataTableOutput("tablx", width="auto"),
         
                  # DT::dataTableOutput("tablz", width="auto"),
                # 
                # 
                
                
                # ,  width =12 )
                # ,hover = "plot_hover"
                #hover = hoverOpts(id ="plot_hover") )           
         )
  )                 ,

checkboxInput("showtab", "Show the simulation results also in table below"), 

conditionalPanel(
  
  condition="input.showtab == 1",
  fluidRow(column(width =12, align = "center",   DT::dataTableOutput("tably", width="100%")
                  # ,style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
                  )),
  
) 
  )                    
)




