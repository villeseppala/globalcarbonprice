# Ville Seppälä (www.villeseppala.fi)

# nww
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
library(ggbrace)
# library(ggpubr)
library(gridExtra)
library(ggrepel)
library(shinyWidgets)
library(bs4Dash)
# library(ggfx)
library(sass)
library(shinyjs)
# library(showtext)


css_content <- "
#tablu2 {
color: blue;
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


# sliderInput2 <- function(inputId, label, min, max, value, step=NULL, from_min, from_max, width, animate){
#   x <- sliderInput(inputId, label, min, max, value, step, width, animate)
#   x$children[[2]]$attribs <- c(x$children[[2]]$attribs, 
#                                "data-from-min" = from_min, 
#                                "data-from-max" = from_max, 
#                                "data-from-shadow" = TRUE)
#   x
# }


llist =c("paa", "muo", "sprice", "eprice","pri" ,"indi1" , "indi2", "muoindi", "indi","popc","con")
# lllist =c("paa", "muo", "sprice", "eprice","pri" ,"indi1" , "indi2", "muoindi", "indi","popc","con")

bgc = hsv(	0.59,0.05,0.25)

fos = hsv(0.09,0.6,0.99)
lul = hsv(0.06,0.9,0.92)
net = hsv(0.09,0.99,0.99)

pop = hsv(0.17,0.99,0.85)


tax = hsv(0.27,0.9,0.72)


fosindi = hsv(.96,.8,.99)
fpop = fosindi
cpop = fosindi
countryfossil = fosindi 


avgcost = hsv(0.48,0.6,0.99)
dividend = hsv(0.56,0.99,0.90)
avgnetcost = hsv(0.53,0.99,0.99)


taxfosindi = avgcost
netcost = avgnetcost


countrycost = avgcost
countrynetcost = avgnetcost

countrypop = pop




sec=c("fossil", "land", "net", "price", "avgcost", "avgfossil", "userfossil", "netcost","usercost",
      "pop","dividend", "avgnetcost", "countryfossil", "countrypop", "countrycost", "countrynetcost")

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
lu[sec == "countryfossil", pos:=12]
lu[sec == "countrycost", pos:=12]
lu[sec == "countrynetcost", pos:=12]
lu[sec == "countrypop", pos:=12]


# decimals in numbers
lu$le = 0
lu[sec == "fossil", le:=1]
lu[sec == "land",le:=1]
lu[sec == "net",le:=1]
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

# marks
lu$mark = "t"
lu[sec == "fossil", mark:="Gt"]
lu[sec == "land", mark:="Gt"]
lu[sec == "net", mark:="Gt"]
lu[sec == "price", mark:="€/t"]
lu[sec == "avgcost", mark:="€"]
lu[sec == "avgfossil", mark:="t"]
lu[sec == "userfossil", mark:="t"]
lu[sec == "netcost", mark:="€"]
lu[sec == "usercost",mark:="€"]
lu[sec == "pop",mark:="B"]
lu[sec == "dividend",mark:="€"]
lu[sec == "avgnetcost", mark:="€"]
lu[sec == "countrycost", mark:="€"]
lu[sec == "countrynetcost", mark:="€"]
lu[sec == "countryfossil", mark:="t"]
lu[sec == "countrypop", mark:="M"]


# labels, English
lu[sec == "fossil", label:="Total emissions"]
lu[sec == "land", label:="Land use / sinks"]
lu[sec == "net", label:="Net emissions"]
lu[sec == "price", label:="Carbon tax"]
lu[sec == "avgcost", label:="Mean carbon costs"]
lu[sec == "avgfossil", label:="Mean emissions"]
lu[sec == "userfossil", label:="User emissions"]
lu[sec == "netcost", label:="User net costs"]
lu[sec == "usercost", label:="User carbon costs"]
lu[sec == "pop", label:="Population"]
lu[sec == "dividend",label:="Carbon dividend"]
lu[sec == "avgnetcost", label:="Mean net costs"]
lu[sec == "countrycost", label:="costs"]
lu[sec == "countrynetcost", label:="net costs"]
lu[sec == "countryfossil", label:="mean emissions"]
lu[sec == "countrypop", label:="population"]

# colors
lu$col =fos
lu[sec == "fossil", col:=fos]
lu[sec == "land", col:=lul]
lu[sec == "net", col:=net]
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


#  alpha?
alas=.9
lu$ala = 0
lu[sec == "fossil", ala:=1]
lu[sec == "land", ala:=1]
lu[sec == "net", ala:=1]
lu[sec == "price", ala:=alas]
lu[sec == "avgcost", ala:=alas]
lu[sec == "avgfossil", ala:=1]
lu[sec == "userfossil", ala:=alas]
lu[sec == "netcost", ala:=alas]
lu[sec == "usercost", ala:=alas]
lu[sec == "pop", ala:=alas]
lu[sec == "dividend", ala:=alas]
lu[sec == "avgnetcost", ala:=alas]

lu$visi = 1
lu[sec =="pop", visi :=0]






## function for calculating country per capita emissions to match world per capita emissions
# start = world start year emissions
# avg = world neutrality year emissions
# con = convergence factor
# cou = country start year emissions
# result is country neutrality year emissions

arg = function(start, end, convergence, coustart) {
  
  end - (start-coustart)*(1-convergence)*(end/start)
  
}


# carbon budget scenario default values
{
  
  nams = c("vuo", "vuo", "yearc", "paa", "muo", "pri", "eprice")
  
  vuo = c(2023, 2050)
  yearc = 2050
  paa = 8
  muo = "percentual"
  pri = "linear"
  eprice = 1000
  
  sken= rep(400,7)
  vals = c(vuo, yearc, paa, muo, pri, eprice)
  skenb = data.frame(nams, sken, vals)
  skenb1 = copy(skenb)
  
  vuo = c(2023, 2055)
  yearc= 2055
  paa = 7
  muo = "percentual"
  pri = "linear"
  eprice = 800
  
  sken= rep(500,7)
  vals = c(vuo, yearc, paa, muo, pri, eprice)
  skenb = data.frame(nams, sken, vals)
  skenb2 = copy(skenb)
  
  vuo = c(2023, 2080)
  yearc = 2080
  paa = 5
  muo = "linear"
  pri = "linear"
  eprice = 400
  
  sken= rep(1150,7)
  vals = c(vuo, yearc, paa, muo, pri, eprice)
  skenb = data.frame(nams, sken, vals)
  skenb3 = copy(skenb)
  
  vuo = c(2023, 2100)
  yearc = 2100
  paa = 5
  muo = "percentual"
  pri = "linear"
  eprice = 300
  
  sken= rep(1350,7)
  vals = c(vuo, yearc, paa, muo, pri, eprice)
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
pastyear = 1970


# population projections
paac = as.data.table(paac)
paac = paac[year >=pastyear,]
paac[variant=="Lower 95 PI", var:=1]
paac[variant=="Lower 80 PI", var:=2]

paac[variant=="Medium", var:=3]
paac[variant=="Upper 80 PI", var:=4]
paac[variant=="Upper 95 PI", var:=5]


pack = paac[year >=pastyear, ]
paaco = paac[year==2021 & variant =="Medium",]
paaco = paaco[order(country)]

ll = as.character(paaco$country)

paaco = paaco[!(country =="World"),]
ll2 = as.character(paaco$country)

#



pop2 = as.data.table(pop2)
pop2[variant=="Lower 95 PI", var:=1]
pop2[variant=="Lower 80 PI", var:=2]

pop2[variant=="Medium", var:=3]
pop2[variant=="Upper 80 PI", var:=4]
pop2[variant=="Upper 95 PI", var:=5]

# pop2[var=="med", var:=2]
# pop2[var=="high", var:=3]

pop2$var = as.numeric(pop2$var)

ppaa = as.data.table(ppaa)
ppaa = ppaa[year >=pastyear,]
ppaa$land=ppaa$lulucf
ppaa$lulucf = NULL

ppaa = ppaa[pop2[var==3,],pop:=i.pop/1000000000, on=c("year")]
ppaa$avgfossil = ppaa$fossil/ppaa$pop


# year, lyear, budget, lbudget,rate, end, lend, fossil, land,  pop, total, price, userfossil, totalindi
# net, avgfossil, avgcost, usercost, netcost, zero
# 



ppaa$budget = NA
ppaa$rate=NA
ppaa$total = NA
ppaa$price=NA
ppaa$userfossil=NA
ppaa$avgcost=NA
ppaa$usercost=NA
ppaa$dividend =NA
ppaa$avgnetcost=NA
ppaa$netcost=NA


ppaa <- gather(ppaa, sec, yy, "fossil":"netcost")

ppaa=as.data.table(ppaa)
lastyear = as.numeric(ppaa[, max(year)])
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
