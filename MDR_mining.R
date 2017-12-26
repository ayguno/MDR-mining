###################
# MDR mining
###################

# MDR master file

MDR <- read.delim("mdrfoi.txt", sep = "|", nrows = 50)
device <- read.delim("foidev.txt", sep = "|", nrows = 50)
patient <- read.delim("patient.txt", sep = "|", nrows = 50)

library(jsonlite)
x <- fromJSON("https://api.fda.gov/device/event.json?search=HERCEPTEST&count=event_type.exact")

x <- fromJSON("https://api.fda.gov/device/event.json?search=HERCEPTEST&count=date_of_event")

y <- fromJSON('https://api.fda.gov/device/event.json?search="PATHVYSION"&count=device.openfda.device_name')

y$results

x$results

z <- fromJSON('https://api.fda.gov/device/event.json?search=device.openfda.regulation_number:864.4400&count=device.brand_name.exact')

VYSIS CLL FISH PROBE KIT 

k <- fromJSON('https://api.fda.gov/device/event.json?search=IVD&count=device.brand_name.exact')

t <- fromJSON('https://api.fda.gov/device/event.json?search=device.brand_name:"Dx"&count=event_type.exact')
t1 <- fromJSON('https://api.fda.gov/device/event.json?search=device.brand_name:"Dx"&count=device.brand_name.exact')


#####################################################
# Table of companion diagnostics parsed by pyhton
#####################################################

essential <- read.csv("essential_table.csv", stringsAsFactors = F)
essential <- essential[!(essential$PMA == ""),]

# Companion diagnostics associated with Herceptin
herceptin <- essential[grepl("Herceptin",essential$Drug.Trade.Name.Generic.Name.),]

# All drug reactions for the specified date window
all_reactions <- fromJSON('https://api.fda.gov/drug/event.json?search=receivedate:[19980101+TO+20171222]&count=patient.reaction.reactionmeddrapt.exact')$results

# All drug reactions asociated with Herceptin for the specified date window
herceptin_reactions <- fromJSON('https://api.fda.gov/drug/event.json?search=receivedate:[19980101+TO+20171222]+AND+patient.drug.openfda.brand_name.exact:(%22HERCEPTIN%22)&count=patient.reaction.reactionmeddrapt.exact')$results

# Calculate PRR for each year FOR a GIVEN EVENT, FOR Herceptin

# PRR = (m/n)/( (M-m)/(N-n) )
# Where; 
#         m = #reports with drug and event
#         n = #reports with drug
#         M = #reports with event in database
#         N = #reports in database


# Let's exercise this for DIARRHOEA, the most common event associated with Herceptin
yearlyPRR <- function(event,drug){
        require(jsonlite)
        require(lubridate)
        require(dplyr)
        
        today <- gsub("-","",as.character(Sys.Date()))
        drug <- toupper(drug)
        
        temp <- data.frame(receipt_year = year, 
                           reports_with_drug_and_event = 0,
                           reports_with_drug = 0,
                           reports_with_event_in_database = 0,
                           reports_in_database = 0,
                           PRR = 0
                           )
        #######################################################
        #This requires internet connection!
        #Perform openFDA API inquiries to obtain relevant data
        ########################################################
        reports_with_drug_and_event <- fromJSON(paste0('https://api.fda.gov/drug/event.json?search=receivedate:[19980101+TO+',today,']+AND+patient.drug.openfda.brand_name.exact:(%22',drug,'%22)+AND+patient.reaction.reactionmeddrapt.exact=',event,'&count=receivedate'))$results
        reports_with_drug_and_event$year <- substr(as.character(reports_with_drug_and_event$time), 1,4)
        reports_with_drug_and_event <- reports_with_drug_and_event %>% group_by(year) %>% summarise(reports_with_drug_and_event = sum(count))
        
        reports_with_drug <- fromJSON(paste0('https://api.fda.gov/drug/event.json?search=receivedate:[19980101+TO+',today,']+AND+patient.drug.openfda.brand_name.exact:(%22',drug,'%22)&count=receivedate'))$results
        reports_with_drug$year <- substr(as.character(reports_with_drug$time), 1,4)
        reports_with_drug <- reports_with_drug %>% group_by(year) %>% summarise(reports_with_drug = sum(count)) 
        
        reports_with_event_in_database <- fromJSON(paste0('https://api.fda.gov/drug/event.json?search=receivedate:[19980101+TO+',today,']+AND+patient.reaction.reactionmeddrapt.exact=',event,'&count=receivedate'))$results
        reports_with_event_in_database$year <- substr(as.character(reports_with_event_in_database$time), 1,4)
        reports_with_event_in_database <- reports_with_event_in_database %>% group_by(year) %>% summarise(reports_with_event_in_database = sum(count))
        
        reports_in_database <- fromJSON(paste0('https://api.fda.gov/drug/event.json?search=receivedate:[19980101+TO+',today,']&count=receivedate'))$results
        reports_in_database$year <- substr(as.character(reports_in_database$time), 1,4)
        reports_in_database <- reports_in_database %>% group_by(year) %>% summarise(reports_in_database = sum(count))
        
        ### CONTINUE HERE ###
        
        # Merge the above reports based on years 
        
        # Add a "total reports" column to give the N (this should be cumulative version of current reports_in_database variable, which is just per year)
        # Similarly, correct the "reports_with_drug" column to give n (this should also be cumulative)
        
        # Calculate PRR for each year
        
        # Return a nice dataframe with results
}

