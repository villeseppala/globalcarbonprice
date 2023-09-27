# Created by Ville Seppälä (www.villeseppala.fi)

# read in data files from data folder (data files must be created with datacreation.r if they do not exist)

pop2<-read.csv(file="data/population.csv", header=T,  sep=",", stringsAsFactors = F )
ppaa<-read.csv(file="data/globalemissions.csv", header=T,  sep=",", stringsAsFactors = F )
paac<-read.csv(file="data/countryemissions.csv", header=T,  sep=",", stringsAsFactors = F )



# make sure to have these installed before using them
library(fontawesome) 

library(tableHTML)
library(rootSolve)
# library(ggthemr)
library(ggplot2)
library(data.table)
library(shiny) 
library(DT)
# library(tidyverse)
library(tidyr)
# library(ggbrace)
# library(ggpubr)
library(gridExtra)
library(ggrepel)
library(shinyWidgets)
library(bs4Dash)
# library(ggfx)
library(sass)
library(shinyjs)
library(showtext)
library(slickR)

library(dplyr)

# library(reactlog)

# reactlog::reactlog_enable()
setDTthreads(2)
# shiny::reactlogShow()

# summ =0  

lihh =.99

bgc = "black"
    
  bgc = "darkgrey"
    bgc = rgb(45, 16, 36, maxColorValue = 255)
    bgc = rgb(45, 60, 90, maxColorValue = 255)
    bgc = rgb(45, 66, 80, maxColorValue = 255)
    
    blu = rgb(29, 137, 255, maxColorValue = 255)
    blu ="#428bca"
    
bgc = hsv(.59,.01,.23)    
bgc = "darkgrey"
bgc = hsv(.59,.55,.25)    
bgc = rgb(242, 227, 219, maxColorValue = 255)
bgc = rgb(242, 100, 219, maxColorValue = 255)
bgc = hsv(.7,.0,.84)     

hh = .81
vv = .8



fos = hsv(0.08,0.93,0.89)
lul = hsv(	0.13,1,0.78)

sink = hsv(	0.13,1,0.78)
source = hsv(	0.13,1,0.78)
newsink = hsv(	0.13,1,0.78)


net = hsv(0.10,0.88,0.85)
tot = hsv(0.08,0.8,0.78)
non = hsv(0.11,0.93,0.71)

pop =hsv(0.7,0.47,1)


tax = hsv(0.3,0.93,0.78)


fpop = hsv(0.56,0.86,1)
fosindi =hsv(0.52,0.72,0.81)  
cpop = fosindi
# cpop = fosindi
countryfossil = hsv(0.54,0.78,0.88)  

# avgcost = hsv(0.57,.7,.9)
avgcost = hsv(0.94,0.63,0.89)
dividend =  hsv(0.87,0.639,0.89)
avgnetcost =hsv(0.9,0.6,0.89)
taxfosindi =hsv(0.95,0.59,0.8)
netcost = hsv(	0.91,0.58,0.83)
landcost = hsv(	0.82,0.58,0.83)

averagedividend =hsv(0.86,0.63,0.79)
countrydividend = hsv(0.88,0.73,0.81)


countrycost = hsv(0.94,0.63,0.79)
countrynetcost = hsv(0.9,0.63,0.69)

countrypop = hsv(0.75,0.52,.8)
# dividend = avgcost
# avgnetcost = avgcost
# taxfosindi = avgcost
# netcost = avgcost


# countrycost = avgcost
# countrynetcost = avgcost

# 

# 
# 


hih =.63
prinet = 120

 maxyy=2138
 minyy=1979
 
 mmaxyy = 2147
 mminyy = 1997
 
 
 mmmaxyy=2145
 mmminyy=1974
 
 mmin = 
# font_add_google("Lato", "lato")
# font_add_google("Gochi Hand", "gochi")
  font_add_google("Saira Extra Condensed", "saira")
 font_add_google("Saira Condensed", "saira")
 
 font_add_google("Roboto Condensed", "roboto")
 
 font_add_google("Archivo Narrow", "jsans")
 font_add_google("IBM Plex Sans Condensed", "jsans")
 font_add_google("IBM Plex Sans Condensed", "jsans")
 font_add_google("Cabin Condensed", "jsans")
 font_add_google("PT Sans", "jsans")
 
 fam = "jsans"
 
#  font_add_google("Asap", "asap")

# dir.create('~/.fonts')
# file.copy("www/IndieFlower-Regular.ttf", "~/.fonts")
# system('fc-cache -f ~/.fonts')

 showtext_auto()

 # fam = "saira"
 
# showtext_auto()

css_content <- "
#tablu2 {
color: red;
font-family: mypolice1;
font-size: .87rem;
line-height: 1.1;
margin -.4vw -.4vw -.4vw -.4vw; padding: -.4vw  -.4vw -.4vw -.4vw;
border-collapse: collapse;
overflow: auto;
width: 60%;
visibility: collapse;
}
"
css_content2 <- "
#tablu3 {
color: red;
font-family: mypolice1;
font-size: .87rem;
line-height: 1.1;
margin -.4vw -.4vw -.4vw -.4vw; padding: -.4vw  -.4vw -.4vw -.4vw;
border-collapse: collapse;
overflow: auto;
width: 60%;
visibility: collapse;
}
"

css_content3 <- "
#tablu4 {
color: red;
font-family: mypolice1;
font-size: .87rem;
line-height: 1.1;
margin -.4vw -.4vw -.4vw -.4vw; padding: -.4vw  -.4vw -.4vw -.4vw;
border-collapse: collapse;
overflow: auto;
width: 60%;
visibility: collapse;
}
"
# font_add_google("Karla", "lato")
# showtext_auto()
# shinyWidgets::shinyWidgetsGallery()

sliderInput2 <- function(inputId, label, min, max, value, step, from_min, from_max, width){
  x <- sliderInput(inputId, label, min, max, value, step, width)
  x$children[[2]]$attribs <- c(x$children[[2]]$attribs,
                               "data-from-min" = from_min,
                               "data-from-max" = from_max,
                               "data-from-shadow" = TRUE)
  x
}




llist =c("paa", "muo", "sprice", "eprice","pri" ,"indi1" , "indi2", "muoindi", "indi","popc","con")
# lllist =c("paa", "muo", "sprice", "eprice","pri" ,"indi1" , "indi2", "muoindi", "indi","popc","con")

# bgc = hsv(	0.59,0.05,0.25)







sec=c("fossil", "land", "net", "ghg","nonco2","source", "sink", "newsink",  "price", "avgcost","landcost", "avgfossil", "userfossil", "netcost","usercost",
      "pop","dividend", "avgnetcost", "countryfossil", "countrypop", "countrycost", "countrynetcost", "averagedividend", "countrydividend")

lu = data.frame(sec)
lu = as.data.table(lu)

# not used currently, for positioning average values in graphs
lu$pos = 0
lu[sec == "fossil", pos:=1]
lu[sec == "land", pos:=2]
lu[sec == "net", pos:=3]
lu[sec == "pop", pos:=4]
lu[sec == "avgfossil", pos:=5]
lu[sec == "price", pos:=6]
lu[sec == "avgcost", pos:=7]
lu[sec == "dividend", pos:=8]
lu[sec == "avgnetcost", pos:=9]
lu[sec == "userfossil", pos:=10]
lu[sec == "usercost", pos:=11]
lu[sec == "netcost", pos:=12]
lu[sec == "countryfossil", pos:=13]
lu[sec == "countrycost", pos:=14]
lu[sec == "countrynetcost", pos:=15]
lu[sec == "countrypop", pos:=16]
lu[sec == "countrydividend", pos:=17]
lu[sec == "averagedividend", pos:=18]
lu[sec == "ghg", pos:=19]
lu[sec == "nonco2", pos:=20]
lu[sec == "source", pos:=21]
lu[sec == "sink", pos:=22]
lu[sec == "newsink", pos:=23]
lu[sec == "landcost", pos:=24]


# decimals in numbers
lu$le = 0
lu[sec == "fossil", le:=1]
lu[sec == "land",le:=1]
lu[sec == "net",le:=1]
lu[sec == "ghg",le:=1]
lu[sec == "nonco2",le:=1]
lu[sec == "pop", le:=2]
lu[sec == "avgfossil",le:=2]
lu[sec == "price", le:=0]
lu[sec == "avgcost", le:=0]
lu[sec == "dividend", le:=0]
lu[sec == "avgnetcost", le:=0]
lu[sec == "userfossil", le:=2]
lu[sec == "usercost", le:=0]
lu[sec == "netcost", le:=0]
lu[sec == "countryfossil", le:=2]
lu[sec == "countrycost", le:=0]
lu[sec == "countrynetcost", le:=0]
lu[sec == "countrypop", le:=0]
lu[sec == "countrydividend", le:=0]
lu[sec == "averagedividend", le:=0]
lu[sec == "source",le:=1]
lu[sec == "sink",le:=1]
lu[sec == "newsink",le:=1]
lu[sec == "landcost",le:=0]

# marks
lu$mark = "t"
lu[sec == "fossil", mark:="Gt"]
lu[sec == "land", mark:="Gt"]
lu[sec == "net", mark:="Gt"]
lu[sec == "ghg", mark:="Gt"]
lu[sec == "nonco2", mark:="Gt"]
lu[sec == "price", mark:="$/t"]
lu[sec == "avgcost", mark:="$"]
lu[sec == "avgfossil", mark:="t"]
lu[sec == "userfossil", mark:="t"]
lu[sec == "netcost", mark:="$"]
lu[sec == "usercost",mark:="$"]
lu[sec == "pop",mark:="B"]
lu[sec == "dividend",mark:="$"]
lu[sec == "avgnetcost", mark:="$"]
lu[sec == "countrycost", mark:="$"]
lu[sec == "countrynetcost", mark:="$"]
lu[sec == "countryfossil", mark:="t"]
lu[sec == "countrypop", mark:="M"]
lu[sec == "countrydividend", mark:="$"]
lu[sec == "averagedividend", mark:="$"]
lu[sec == "source", mark:="Gt"]
lu[sec == "sink", mark:="Gt"]
lu[sec == "newsink", mark:="Gt"]
lu[sec == "landcost", mark:="$"]

# labels, English
# lu[sec == "fossil", label:="Total emissions"]
# lu[sec == "land", label:="Land use / sinks"]
# lu[sec == "net", label:="Net emissions"]
# lu[sec == "price", label:="Carbon tax"]
# lu[sec == "avgcost", label:="Mean carbon costs"]
# lu[sec == "avgfossil", label:="Mean emissions"]
# lu[sec == "userfossil", label:="User emissions"]
# lu[sec == "netcost", label:="User net costs"]
# lu[sec == "usercost", label:="User carbon costs"]
# lu[sec == "pop", label:="Population"]
# lu[sec == "dividend",label:="Carbon dividend"]
# lu[sec == "avgnetcost", label:="Mean net costs"]
# lu[sec == "countrycost", label:="costs"]
# lu[sec == "countrynetcost", label:="net costs"]
# lu[sec == "countryfossil", label:="mean emissions"]
# lu[sec == "countrypop", label:="population"]
# lu[sec == "countrydividend", label:="Dividend, average national"]
# lu[sec == "averagedividend", label:="dividend"]



# colors
lu$col =fos
lu[sec == "fossil", col:=fos]
lu[sec == "land", col:=lul]
lu[sec == "net", col:=net]
lu[sec == "ghg", col:=tot]
lu[sec == "nonco2", col:=non]

lu[sec == "price", col:=tax]
lu[sec == "avgcost", col:=avgcost]
lu[sec == "avgfossil", col:=fpop]
lu[sec == "userfossil", col:=fosindi]
lu[sec == "netcost", col:=netcost]
lu[sec == "usercost", col:=taxfosindi]
lu[sec == "pop", col:=pop]
lu[sec == "dividend",col:=dividend]
lu[sec == "avgnetcost", col:=avgnetcost]
lu[sec == "countrycost", col:=countrycost]
lu[sec == "countrynetcost", col:=countrynetcost]
lu[sec == "countryfossil", col:=countryfossil]
lu[sec == "countrypop", col:=countrypop]
lu[sec == "countrydividend", col:=dividend]
lu[sec == "averagedividend", col:=dividend]
lu[sec == "source",col:=source]
lu[sec == "sink", col:=sink]
lu[sec == "newsink", col:=newsink]
lu[sec == "landcost", col:=landcost]

#  alpha?
alas=.9
lu$ala = 0
lu[sec == "fossil", ala:=1]
lu[sec == "land", ala:=1]
lu[sec == "net", ala:=1]
lu[sec == "ghg", ala:=1]
lu[sec == "nonco2", ala:=1]

lu[sec == "price", ala:=alas]
lu[sec == "avgcost", ala:=alas]
lu[sec == "avgfossil", ala:=1]
lu[sec == "userfossil", ala:=alas]
lu[sec == "netcost", ala:=alas]
lu[sec == "usercost", ala:=alas]
lu[sec == "pop", ala:=alas]
lu[sec == "dividend", ala:=alas]
lu[sec == "avgnetcost", ala:=alas]
lu[sec == "countrycost",  ala:=alas]
lu[sec == "countrynetcost",  ala:=alas]
lu[sec == "countryfossil",  ala:=alas]
lu[sec == "countrypop",  ala:=alas]
lu[sec == "countrydividend", ala:=alas]
lu[sec == "averagedividend", ala:=alas]
lu[sec == "source", ala:=alas]
lu[sec == "sink", ala:=alas]
lu[sec == "newsink", ala:=alas]
lu[sec == "landcost", ala:=alas]

lu$visi = 1
lu[sec =="pop", visi :=0]


lu[sec == "fossil", label:="CO2 emissions"]
lu[sec == "land", label:="Land+CCS CO2"]
lu[sec == "net", label:="Net CO2 emissions"]
lu[sec == "ghg", label:="Total emissions"]
lu[sec == "nonco2", label:="Non-CO2 emissions"]

lu[sec == "price", label:="Carbon price"]
lu[sec == "avgcost", label:="Mean costs"]
lu[sec == "avgfossil", label:="Mean emissions"]
lu[sec == "userfossil", label:="User emissions"]
lu[sec == "netcost", label:="User net costs"]
lu[sec == "usercost", label:="User costs"]
lu[sec == "pop", label:="World population"]
lu[sec == "dividend",label:="Carbon dividend"]
lu[sec == "avgnetcost", label:="Mean net costs"]
lu[sec == "countrycost", label:="Country costs"]
lu[sec == "countrynetcost", label:="Country net costs"]
lu[sec == "countryfossil", label:="Country emissions"]
lu[sec == "countrypop", label:="Country population"]
lu[sec == "countrydividend", label:="National dividend"]
lu[sec == "averagedividend", label:="Mean dividend"]
lu[sec == "source", label:="Land emissions"]
lu[sec == "sink", label:="Land sinks + CCS"]
lu[sec == "newsink", label:="New land sinks + CCS"]
lu[sec == "landcost", label:="New sink+CCS costs"]


lu[sec == "fossil", labbi:="Global emissions"]
lu[sec == "land",labbi:="Global emissions"]
lu[sec == "net", labbi:="Global emissions"]
lu[sec == "ghg", labbi:="Global emissions"]
lu[sec == "nonco2", labbi:="Global emissions"]

lu[sec == "price", labbi:="Carbon price"]
lu[sec == "avgcost", labbi:="Emissions costs and benefits"]
lu[sec == "avgfossil", labbi:="Individual emissions"]
lu[sec == "userfossil", labbi:="Individual emissions"]
lu[sec == "netcost", labbi:="Emissions costs and benefits"]
lu[sec == "usercost", labbi:="Emissions costs and benefits"]
lu[sec == "pop", labbi:="Population projection"]
lu[sec == "dividend",labbi:="Emissions costs and benefits"]
lu[sec == "avgnetcost", labbi:="Emissions costs and benefits"]
lu[sec == "countrycost", labbi:="Emissions costs and benefits"]
lu[sec == "countrynetcost",labbi:="Emissions costs and benefits"]
lu[sec == "countryfossil", labbi:="Individual emissions"]
lu[sec == "countrypop", labbi:="Population projection"]
lu[sec == "countrydividend", labbi:="Emissions costs and benefits"]
lu[sec == "averagedividend", labbi:="Emissions costs and benefits"]
lu[sec == "source", labbi:="Global emissions"]
lu[sec == "sink", labbi:="Global emissions"]
lu[sec == "newsink", labbi:="Global emissions"]
lu[sec == "landcost", labbi:="Emissions costs and benefits"]


# lalist = c("Fossil emissions", "Land emissions / sinks", "Net emissions","World population",
#   "Average emissions",  "Carbon price", "Average carbon costs",
#   "Carbon dividend","Mean net costs", "User emissions", "User carbon costs", "User net costs"
#   )


lu2 = copy(lu)
lu2[sec == "fossil", labbi:="Globaalit päästöt"]
lu2[sec == "land", labbi:="Globaalit päästöt"]
lu2[sec == "net", labbi:="Globaalit päästöt"]
lu2[sec == "ghg", labbi:="Globaalit päästöt"]
lu2[sec == "nonco2", labbi:="Globaalit päästöt"]

lu2[sec == "price", labbi:="Päästöjen hinta"]
lu2[sec == "avgcost", labbi:="Menot ja tulot päästöistä"]
lu2[sec == "avgfossil", labbi:="Yksilökohtaiset päästöt"]
lu2[sec == "userfossil", labbi:="Yksilökohtaiset päästöt"]
lu2[sec == "netcost", labbi:="Menot ja tulot päästöistä"]
lu2[sec == "usercost", labbi:="Menot ja tulot päästöistä"]
lu2[sec == "pop", labbi:="Väestökehitys"]
lu2[sec == "dividend",labbi:="Menot ja tulot päästöistä"]
lu2[sec == "avgnetcost", labbi:="Menot ja tulot päästöistä"]
lu2[sec == "countrycost", labbi:="Menot ja tulot päästöistä"]
lu2[sec == "countrynetcost", labbi:="Menot ja tulot päästöistä"]
lu2[sec == "countryfossil", labbi:="Yksilökohtaiset päästöt"]
lu2[sec == "countrypop", labbi:="Väestökehitys"]
lu2[sec == "countrydividend", labbi:="Menot ja tulot päästöistä"]
lu2[sec == "averagedividend", labbi:="Menot ja tulot päästöistä"]
lu2[sec == "source", labbi:="Globaalit päästöt"]
lu2[sec == "sink", labbi:="Globaalit päästöt"]
lu2[sec == "newsink",labbi:= "Globaalit päästöt"]
lu2[sec == "landcost",labbi:= "Menot ja tulot päästöistä"]





lu2[sec == "fossil", label:="CO2-päästöt"]
lu2[sec == "land", label:="Maankäyttö+CCS CO2"]
lu2[sec == "net", label:="CO2-nettopäästöt"]
lu2[sec == "ghg", label:="Kokonaispäästöt"]
lu2[sec == "nonco2", label:="Muut päästöt"]
lu2[sec == "price", label:="Hiilen hinta"]
lu2[sec == "avgcost", label:="Keskipäästömenot"]
lu2[sec == "avgfossil", label:="Keskipäästöt"]
lu2[sec == "userfossil", label:="Käyttäjän päästöt"]
lu2[sec == "netcost", label:="Käyttäjän nettomenot"]
lu2[sec == "usercost", label:="Käyttäjän päästömenot"]
lu2[sec == "pop", label:="Maailman väestö"]
lu2[sec == "dividend",label:="Hiiliosinko"]
lu2[sec == "avgnetcost", label:="Keskinettomenot"]
lu2[sec == "countrycost", label:="menot"]
lu2[sec == "countrynetcost", label:="Maan nettokeskimenot"]
lu2[sec == "countryfossil", label:="Maan keskipäästöt"]
lu2[sec == "countrypop", label:="Maan väestö"]
lu2[sec == "countrydividend", label:="Kansallinen osinko"]
lu2[sec == "averagedividend", label:="Keskiosinko"]
lu2[sec == "source", label:="Maankäytön päästöt"]
lu2[sec == "sink", label:="Maanielu + CCS"]
lu2[sec == "newsink", label:="Uusi maanielu + CCS"]
lu2[sec == "landcost", label:="Uuden nielun + CCS menot"]


## function for calculating country per capita emissions to match world per capita emissions
# start = world start year emissions
# end = world neutrality year emissions
# convergence = convergence factor
# coustart= country start year emissions
# result is country neutrality year emissions

arg = function(start, end, convergence, coustart) {
  
  end - (start-coustart)*(1-convergence)*(end/start)
  
}


# carbon budget scenario default values
{
  
  nams = c("vuo", "vuo", "yearc", "paa", "muo", "pri", "sprice", "eprice")
  
  vuo = c(2024, 2055)
  yearc = 2055
  paa = 3
  muo = "percentual"
  pri = "linear"
  sprice = 200
  eprice = 1200
  
  sken= rep(400,8)
  vals = c(vuo, yearc, paa, muo, pri, sprice, eprice)
  skenb = data.frame(nams, sken, vals)
  skenb1 = copy(skenb)
  
  vuo = c(2024, 2060)
  yearc= 2060
  paa = 4
  muo = "percentual"
  pri = "linear"
  sprice = 150
  eprice = 1000
  
  sken= rep(500,8)
  vals = c(vuo, yearc, paa, muo, pri, sprice, eprice)
  skenb = data.frame(nams, sken, vals)
  skenb2 = copy(skenb)
  
  vuo = c(2024, 2080)
  yearc = 2080
  paa = 6
  muo = "percentual"
  pri = "linear"
  sprice = 50
  eprice = 500
  
  sken= rep(1150,8)
  vals = c(vuo, yearc, paa, muo, pri, sprice, eprice)
  skenb = data.frame(nams, sken, vals)
  skenb3 = copy(skenb)
  
  vuo = c(2024, 2100)
  yearc = 2100
  paa = 5
  muo = "percentual"
  pri = "linear"
  sprice = 40
  eprice = 400
  
  sken= rep(1350,8)
  vals = c(vuo, yearc, paa, muo, pri, sprice, eprice)
  skenb = data.frame(nams, sken, vals)
  skenb4 = copy(skenb)
  
  
  skenbs = rbind(skenb1, skenb2, skenb3, skenb4)
  
  skenbs = as.data.table(skenbs)
  # 13
  
}

l = 1
r= 1
da = data.frame(l,r)
# as.numeric(2023)


# first year to show in graphs?


# population projections
paac = as.data.table(paac)

paac[country =="China, Taiwan Province of China", country:="Taiwan"]

pack = copy(paac)
# pack = paac[year >=pastyear, ]



paaco = paac[year==2021 & var ==3,]
paaco = paaco[order(country)]
# paaco = paaco[order(gdpcap)]

ll = as.character(paaco$country)

paaco = paaco[!(country =="World"),]
ll2 = as.character(paaco$country)

#



pop2 = as.data.table(pop2)
# pop2[variant=="Lower 95 PI", var:=1]
# pop2[variant=="Lower 80 PI", var:=2]
# 
# pop2[variant=="Medium", var:=3]
# pop2[variant=="Upper 80 PI", var:=4]
# pop2[variant=="Upper 95 PI", var:=5]

# pop2[var=="med", var:=2]
# pop2[var=="high", var:=3]


ppaa = as.data.table(ppaa)
# ppaa = ppaa[year >=pastyear,]


lastyear = as.numeric(ppaa[, max(year)])

budgetyear = 2020
# 

# 
# populaatio = populaatio[populaatio]
# 
# 
# pop = populaatio$PopTotal
# pop
#time = pituus
#end = tasapainopaasto
#start = alkupaasto 
#rate = laskutahti 
#fyear = alkuvuosi
#lyear = loppuvuosi
#fossil  = yearly fossil emissions
#total = kokonaispaastot / total fossil emissions
#lstart = alkunielu



paaa = 5


chk = function(col, label, info) {
  p(style=paste0("color:",col,";"),
    HTML(label, "<font size='2'>",
         as.character(actionLink(inputId = info,
                                 label = "  ",
                                 icon = icon("info"))), "</font>")

  )
}



inf = function(label, id) {
  # div(style="display:inline-block",
  label=p(label,HTML("<font size='1'>",
               as.character(actionLink( inputId = id, 
                                       label = " ", 
                                       icon = icon("fas fa-info-circle"))), "</font>"))
  # )  
}





inf2 = function( id) {
  div(style="display:inline-block",
  label=p(HTML("<font size='2'>",
               as.character(actionLink(inputId = id, 
                                      label = " ", 
                                      icon = icon("fas fa-info-circle"))), "</font>")))
  }

cuk = function(col, label, info, show, lab, value) {
  div(div(style="display:inline-block",   p(style=paste0("color:",col,";"),
                                            HTML(label)
  )), 
      div(style="display:inline-block",HTML( "<font size='2'>", 
                                             as.character(actionLink(style='color:#ffc107',inputId = info, 
                                                                     label = "  ", 
                                                                     icon = icon("fas fa-info-circle"))), "</font>") ),
      div(style="display:inline-block; ",awesomeCheckbox(show, label=lab,  value=FALSE)))  
  
}




cuk2 = function(col, label, info, show, lab, value) {
  div(div(style="display:inline-block", div(style=paste0("color:",col,";","font-weight:1000; "),
                                              label
  )),
  div(style="display:inline-block",HTML( "<font size='2'>",
                                         as.character(actionLink(inputId = info,
                                                                 label = "  ",
                                                                 icon = icon("fas fa-info-circle"))), "</font>") ),
  # div(style="display:inline-block; ",awesomeCheckbox(show, label=lab,  value=FALSE)))
  div(style="display:inline-block; ",checkboxInput(show, label=lab,  value=FALSE)))

}



cuk3 = function(col, label, info, show, lab, value) {
  div(style="display:inline-block; ",
    div(style="display:inline-block; ",awesomeCheckbox(show, label=lab,  value=FALSE)),
    
    div(style="display:inline-block; ", div(  style=paste0("color:",col,";"),
                                              label
  )),
  div(style="display:inline-block; ",HTML( "<font size='2'>",
                                         as.character(actionLink(inputId = info,
                                                                 label = "  ",
                                                                 icon = icon("fas fa-info-circle"))), "</font>") )
    )
  
}

enableBookmarking(store = "url")
