library(dplyr)


lm_eqn = function(m) {
    
    l <- list(a = format(coef(m)[1], digits = 2),
    b = format(abs(coef(m)[2]), digits = 2),
    r2 = format(summary(m)$r.squared, digits = 3));
    
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
    
    
    as.character(as.expression(eq));
}


#allzips <- readRDS("data/superzip.rds")
superzip <- read.csv(file="data/superzip.csv")
zipcodes <- read.csv(file="data/zip_codes_states.csv")
healthcaredata <- read.csv(file="data/Healthcare.csv")
cities <- read.csv(file="data/Municipalities.csv")
unemployment <- read.csv(file="data/Unemployment.csv")
congresscontact <- read.csv(file="data/CongressionalContact.csv")
congresselections <- read.csv(file="data/CongressElections.csv")


allzips <- merge(x=superzip, y=zipcodes, by.x="zipcode", by.y="zip_code")
allzips <- merge(x=allzips, y=unemployment, by.x="zipcode", by.y="Zip")
allzips <- merge(x=allzips, y=healthcaredata, by.x="zipcode", by.y="Id")
allzips <- merge(x=allzips, y=cities, by.x="zipcode", by.y="zipcode")
allzips <- merge(x=allzips, y=congresscontact, by.x="districtcode", by.y="districtwide")
allzips <- merge(x=allzips, y=congresselections, by.x="districtcode", by.y="districtcode")



allzips$representative <- paste(allzips$firstname, allzips$lastname, sep=" ")
allzips$latitude <- jitter(allzips$latitude)
allzips$longitude <- jitter(allzips$longitude)
allzips$college <- allzips$college * 100
allzips$unemployment <- allzips$Unemp..Rate * 100
allzips$pubcov <- as.numeric(as.vector(allzips$Percent.Public.Coverage..Estimate..COVERAGE.ALONE...Public.health.insurance.alone))
allzips$medicare <- as.numeric(as.vector(allzips$Percent.Public.Coverage..Margin.of.Error..COVERAGE.ALONE...Public.health.insurance.alone...Medicare.coverage.alone))
allzips$va <- as.numeric(as.vector(allzips$Percent.Public.Coverage..Margin.of.Error..COVERAGE.ALONE...Public.health.insurance.alone...VA.health.care.coverage.alone))
allzips$medicaidexpansion <- as.numeric(as.vector(allzips$Percent.Public.Coverage..Margin.of.Error..PUBLIC.HEALTH.INSURANCE.ALONE.OR.IN.COMBINATION...Below.138.percent.of.the.poverty.threshold))
allzips$zipcode <- formatC(allzips$zipcode, width=5, format="d", flag="0")
