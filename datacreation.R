
#first install these packages if they are not installed already

library(readxl)
library(data.table)
library(countrycode)


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

popsw = pops[country=="World",]

paa = pops[paasi2, co2:=i.co2, on =c("year", "iso3c")]
paa = paa[paasi2, bunkers:=i.bunkers, on =c("year")]



paa$co2cap = paa$co2/paa$pop
paac=copy(paa)








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



# replace directory path to save


write.csv(paac,"data/countryemissions.csv", row.names = FALSE)

write.csv(popsw,"data/population.csv", row.names = FALSE)

write.csv(paa,"data/globalemissions.csv", row.names = FALSE)









