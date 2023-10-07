
#first install these packages if they are not installed already

library(readxl)
library(data.table)
library(countrycode)

library(wbstats)







# go to https://www.climatewatchdata.org/data-explorer/
# click Download bulk data from top right corner and choose ghg emissions
# put the PIK file on your file location

# emu<-read.csv(file="primap2023march.csv", header=T,  sep="," )[,]
# emu = as.data.table(emu)
# emu[area..ISO3.=="FIN" & entity =="CO2" & category..IPCC2006_PRIMAP.=="M.0.EL" & scenario..PRIMAP.hist. == "HISTCR", X2021]
# 
# 
# 
# emu<-read.csv(file="primapnor.csv", header=T,  sep="," )[,]
# emu = as.data.table(emu)
# emu[area..ISO3.=="EARTH" & entity =="KYOTOGHG (AR4GWP100)"  & category..IPCC2006_PRIMAP.=="M.LULUCF" & scenario..PRIMAP.hist. == "HISTCR",X2019]
# emu[area..ISO3.=="EARTH" & entity =="KYOTOGHG (AR4GWP100)"  & category..IPCC2006_PRIMAP.=="M.0.EL" & scenario..PRIMAP.hist. == "HISTCR",X2019]
# emu[area..ISO3.=="EARTH" & entity =="CO2" & category..IPCC2006_PRIMAP.=="M.0.EL" & scenario..PRIMAP.hist. == "HISTTP",X2019]
# 
# emu[area..ISO3.=="QAT" & entity =="KYOTOGHG (AR4GWP100)"  & category..IPCC2006_PRIMAP.=="M.0.EL" & scenario..PRIMAP.hist. == "HISTTP",X2021]
# 



em<-read.csv(file="CW_HistoricalEmissions_PIK.csv", header=T,  sep="," )[,]

em = as.data.table(em)

em[, 4] = NULL
em = em[gas %in%  c("KYOTOGHG", "CO2"), ]
em = em[sector=="Total excluding LULUCF", ]


em[, 2] = NULL


colnames(em)[1] <- "cou"
colnames(em)[2] <- "gas"



# em = mutate_all(em, as.character)

emo <- gather(em, year, emission, "X1850":"X2021")


emo$year= gsub('X', '', emo$year)
emo$year = as.numeric(emo$year)
emo$emission = as.numeric(emo$emission)


emo = as.data.frame(emo)

emo$country <- factor(countrycode(sourcevar=emo[, "cou"], 
                                  origin="iso3c", 
                                  destination="country.name"))

emo=as.data.table(emo)


emo[cou=="EUU", country:="European Union"]
emo[cou=="WORLD", country:="World"]

colnames(emo)[1] = "iso3c"

emo[iso3c=="WORLD", iso3c:="WLD"]


emo[gas =="KYOTOGHG", gas:="ghg"]
emo[gas =="CO2", gas :="co2"]

# emo[,  , by=c("year", "country")]
emo = emo[year > 1979,]

emc = emo[gas=="co2",]
emt = emo[gas=="ghg"]
emt = emt[, nonco2:= emission - emc[, emission], on=c("year", "iso3c")]

emt[emc, nonco2 := emission - i.emission, on=c("year", "iso3c")]



# emuk = emo[gas=="total",]
emt[, emission :=nonco2]
emt[, gas :="nonco2"]

emt$nonco2=NULL

emo = rbind(emo, emt)

pik = copy(emo)

(unique(pik$iso3c))



# Emissions from Friedlingstein et al. 2021

#2021
#https://data.icos-cp.eu/licence_accept?ids=%5B%22rmU_viZcddCV7LdflaFGN-My%22%5D

#2022
#https://data.icos-cp.eu/licence_accept?ids=%5B%22zL1wtJrG7Q5xdvF39Ylg3lUw%22%5D

tempdl <- tempfile()
download.file("https://data.icos-cp.eu/licence_accept?ids=%5B%22zL1wtJrG7Q5xdvF39Ylg3lUw%22%5D",tempdl, mode="wb") 

tempfile()
mine <- read_excel(tempdl, sheet = "Territorial Emissions", skip=11)


paasi1 = copy(mine)
setnames(paasi1, 1, "year")



paasi2 <- gather(paasi1, country, co2, "Afghanistan":"World")

paasi2 = as.data.table(paasi2)

paasi2[(country %in% c("USA")), ':='(country =c("United States"))]



paasi2$co2= gsub('NaN', 'NA', paasi2$co2)
paasi2$co2= gsub(',', '.', paasi2$co2)

paasi2$co2 = as.numeric(paasi2$co2)

#convert carbon measure to carbon dioxide
paasi2$co2 = paasi2$co2*3.664
paasi2$co2 = paasi2$co2*1000000

paasi2 = as.data.frame(paasi2)

paasi2$iso3c <- factor(countrycode(sourcevar=paasi2[, "country"], 
                                   origin="country.name", 
                                   destination="iso3c"))

paasi2=as.data.table(paasi2)

paasi2 = paasi2[year >1949,]
paasi2[country =="World", iso3c := "WLD"]


paasi2 = paasi2[paasi2[country =="Bunkers"], bunkers :=i.co2, on=c("year")]

packo = paasi2[year==2021,]
packo = packo[!(iso3c %in% NA),]

ll = as.character(packo$iso3c)





tempdl <- tempfile()
download.file("https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_TotalPopulationBySex.zip",tempdl, mode="wb") 
unzip(tempdl, "WPP2022_TotalPopulationBySex.csv") 
# data <- read.table("WPP2022_TotalPopulationBySex.csv", sep=",")
data <- read.csv("WPP2022_TotalPopulationBySex.csv", sep=",")


popred = as.data.table(data)
popred$year = as.numeric(popred$Time)
popred$country = as.factor(popred$Location)
popred$iso3c = as.character(popred$ISO3_code)
popred[country =="World", iso3c := "WLD"]


variantlist=c("Upper 80 PI", "Lower 80 PI", "Upper 95 PI", "Lower 95 PI","Medium")


populaatio= popred[iso3c %in% ll & Variant %in% variantlist &year %in% 1950:2100, c("PopTotal", "year", "Variant","iso3c", "country")]

populaatio$PopTotal = as.numeric(populaatio$PopTotal)*1000


popb= populaatio[year %in% c(1950:2021),]

popu80=copy(popb)
popu95=copy(popb)
popl80=copy(popb)
popl95=copy(popb)

popu80 = popu80[,Variant :="Upper 80 PI"]
popl80 = popl80[,Variant :="Lower 80 PI"]
popu95 = popu95[,Variant :="Upper 95 PI"]
popl95 = popl95[,Variant :="Lower 95 PI"]


pops = rbind(populaatio, popu80, popl80, popu95, popl95)

pops$pop = pops$PopTotal

pops$variant = pops$Variant
pops$PopTotal = NULL
pops$Variant = NULL

pops[variant=="Lower 95 PI", var:=1]
pops[variant=="Lower 80 PI", var:=2]

pops[variant=="Medium", var:=3]
pops[variant=="Upper 80 PI", var:=4]
pops[variant=="Upper 95 PI", var:=5]
pops$var = as.numeric(pops$var)


popsw = pops[country=="World",]

paa = pops[paasi2, co2:=i.co2, on =c("year", "iso3c")]
paa = paa[paasi2, bunkers:=i.bunkers, on =c("year")]



paa$co2cap = paa$co2/paa$pop
paac=copy(paa)




pastyear = 1980

paac = paac[year >=pastyear,]
paac[variant=="Lower 95 PI", var:=1]
paac[variant=="Lower 80 PI", var:=2]

paac[variant=="Medium", var:=3]
paac[variant=="Upper 80 PI", var:=4]
paac[variant=="Upper 95 PI", var:=5]


paac = paac[pik[gas=="nonco2"], nonco2 := i.emission*1000000 , on=c("year", "iso3c")]


## approximating non-co2 emissions for countries that do not have nonco2 emissions. 

# first find out how big emitter these countries are relative to each other by using co2
paac[is.na(nonco2) & year < 2022, kel := sum(co2), by=c("year", "var")]
paac[is.na(nonco2) & year < 2022, sha := co2/kel]

# next calculate how much ghg emissions are accounted for
paac[!(iso3c=="WLD"), sumt := sum(nonco2, na.rm=T), by=c("year", "var")]

# compare to total ghg emissions
paac = paac[paac[iso3c =="WLD",], wor:=i.nonco2, on=c("year", "var")]

# distribute the remainer to countries without ghg emissions by using their relative sizes
paac[is.na(nonco2) & year < 2022, nonco2 := 1]

paac[is.na(nonco2) & year < 2022, nonco2 := (wor-sum)*sha]

paac$ghg = paac$co2 + paac$nonco2

paac$ghgcap = paac$ghg/paac$pop



paac = paac[, !(c("variant", "kel", "sha", "sumt", "wor"))]






gdpdata <- wb(indicator = "NY.GDP.MKTP.KD", startdate = 1959, enddate = 2022)

gdpdata=as.data.table(gdpdata)
gdpdata = gdpdata[indicator =="GDP (constant 2015 US$)",]
# gdpdata$country
gdpdata$year = as.numeric(gdpdata$date)



paac = paac[gdpdata, gdp :=i.value, on=c("year", "iso3c")]
paac$gdpcap = paac$gdp/paac$pop



paacw = paac[iso3c =="WLD",]
# paacw[]

# sum = 
# paac[paac[iso3c =="WLD"], rem :=]

# rem = pik[year ==2021 & gas =="co2",]
  
# paacoo= paac[year ==2021 & var==3,]
# pikoo= pik[year ==2021 & gas =="co2",]
# pikoo= pikoo[!(iso3c %in% c("ANNEXI", "BASIC", "ANT", "AOSIS", "NONANNEXI", 	
#                             "UMBRELLA", "EUU", "WLD", "LDC")),]
# 
# pikoo[,sum(emission)]



## old emissions

#2021
# https://data.icos-cp.eu/licence_accept?ids=%5B%22Ayyw1HeihXdTUoO000dGcxrP%22%5D

#2022
# https://data.icos-cp.eu/licence_accept?ids=%5B%221umMtgeUlhS2Y1YW_Qp94bu3%22%5D


tempdl <- tempfile()
download.file("https://data.icos-cp.eu/licence_accept?ids=%5B%221umMtgeUlhS2Y1YW_Qp94bu3%22%5D",tempdl, mode="wb") 

tempfile()
min <- read_excel(tempdl, sheet = "Global Carbon Budget", skip=20)

paasi1 = copy(min)
setnames(paasi1, 1, "year")

paa = as.data.table(paasi1)


setnames(paa, 2, "fossil")
setnames(paa, 3, "lulucf")

paa = paa[,1:3]


paa$net = paa$fossil + paa$lulucf

paa$fossil = paa$fossil*3.664
paa$lulucf = paa$lulucf*3.664
paa$net= paa$net*3.664


paa = paa[paacw[var==3], ghg := i.ghg/1000000000 , on="year"]
paa = paa[paacw[var==3], nonco2 := i.nonco2/1000000000 , on="year"]
# paa = paa[pik, nonco2 := i.nonco2 , on="year"]


paa = paa[year >=pastyear,]

paa$land=paa$lulucf
paa$lulucf = NULL
# total ghg emissions and non-co2 emissions from Climate Watch

ppaa = copy(paa)

ppaa = ppaa[popsw[var==3,],pops:=i.pop/1000000000, on=c("year")]
ppaa$avgfossil = ppaa$fossil/ppaa$pop
ppaa$avgghg= ppaa$ghg/ppaa$pop




tempdl <- tempfile()
download.file("https://data.icos-cp.eu/licence_accept?ids=%5B%221umMtgeUlhS2Y1YW_Qp94bu3%22%5D",tempdl, mode="wb") 

tempfile()
min <- read_excel(tempdl, sheet = "Land-Use Change Emissions", skip=29)

paasi1 = copy(min)
setnames(paasi1, 1, "year")

paa = as.data.table(paasi1)


setnames(paa, 3, "sink")
setnames(paa, 4, "source")


paa$sink = paa$sink*3.664
paa$source = paa$source*3.664

paa = paa[,c(1,3,4)]

paa = paa[year >=pastyear,]

ppaa$sink =paa$sink
ppaa$source = paa$source

# paa$net = paa$fossil + paa$lulucf

# here new 


# year, lyear, budget, lbudget,rate, end, lend, fossil, land,  pop, total, price, userfossil, totalindi
# net, avgfossil, avgcost, usercost, netcost, zero
#  


ppaa$newsink = NA

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











# replace directory path to save

write.csv(paac,"data/countryemissions.csv", row.names = FALSE)

write.csv(popsw,"data/population.csv", row.names = FALSE)

write.csv(ppaa,"data/globalemissions.csv", row.names = FALSE)









