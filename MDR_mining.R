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
yearlyPRR <- function(event,drug, start.date = "1998-01-01", end.date = as.character(Sys.Date())){
        
        # Returns yearly PRR scores for a given drug for a given event
        
        
        require(jsonlite)
        require(lubridate)
        require(dplyr)
        
        end.date <- gsub("-","", end.date)
        start.date <- gsub("-","", start.date)
        
        drug <- toupper(drug)
        event <- toupper(event)
        
        #######################################################
        #This requires internet connection!
        #Perform openFDA API inquiries to obtain relevant data
        ########################################################
        reports_with_drug_and_event <- fromJSON(paste0('https://api.fda.gov/drug/event.json?search=receivedate:[',start.date,'+TO+',end.date,']+AND+patient.drug.openfda.brand_name.exact:(%22',drug,'%22)+AND+patient.reaction.reactionmeddrapt.exact=',event,'&count=receivedate'))$results
        reports_with_drug_and_event$year <- substr(as.character(reports_with_drug_and_event$time), 1,4)
        reports_with_drug_and_event <- reports_with_drug_and_event %>% group_by(year) %>% summarise(reports_with_drug_and_event = sum(count))
        
        reports_with_drug <- fromJSON(paste0('https://api.fda.gov/drug/event.json?search=receivedate:[',start.date,'+TO+',end.date,']+AND+patient.drug.openfda.brand_name.exact:(%22',drug,'%22)&count=receivedate'))$results
        reports_with_drug$year <- substr(as.character(reports_with_drug$time), 1,4)
        reports_with_drug <- reports_with_drug %>% group_by(year) %>% summarise(reports_with_drug = sum(count)) 
        
        reports_with_event_in_database <- fromJSON(paste0('https://api.fda.gov/drug/event.json?search=receivedate:[',start.date,'+TO+',end.date,']+AND+patient.reaction.reactionmeddrapt.exact=',event,'&count=receivedate'))$results
        reports_with_event_in_database$year <- substr(as.character(reports_with_event_in_database$time), 1,4)
        reports_with_event_in_database <- reports_with_event_in_database %>% group_by(year) %>% summarise(reports_with_event_in_database = sum(count))
        
        reports_in_database <- fromJSON(paste0('https://api.fda.gov/drug/event.json?search=receivedate:[',start.date,'+TO+',end.date,']&count=receivedate'))$results
        reports_in_database$year <- substr(as.character(reports_in_database$time), 1,4)
        reports_in_database <- reports_in_database %>% group_by(year) %>% summarise(reports_in_database = sum(count))
        
        # Merge the above reports based on years 
        temp <- merge(reports_with_drug_and_event,reports_with_drug,by = "year", all.x = T, all.y = F, sort = F)
        temp <- merge(temp,reports_with_event_in_database,by = "year", all.x = T, all.y = F, sort = F)
        temp <- merge(temp,reports_in_database,by = "year", all.x = T, all.y = F, sort = F)
        
        # Calculate PRR for each year:
        # PRR = (m/n)/( (M-m)/(N-n) )
        # Where; 
                 m = temp$reports_with_drug_and_event #reports with drug and event
                 n = temp$reports_with_drug #reports with drug
                 M = temp$reports_with_event_in_database #reports with event in database
                 N = temp$reports_in_database #reports in database
        
                 temp$PRR <- (m/n)/( (M-m)/(N-n) )
                 
                 temp$drug <- drug
                 temp$event <- event
        # Return a nice dataframe with 
        return(temp)         
}


# Let's try to parse information systematically for all drugs in our list
essential <- read.csv("essential_table.csv", stringsAsFactors = F)
essential <- essential[!(essential$PMA == ""),]

# Need to perform some cleaning
essential$Drug.Trade.Name.Generic.Name. <- gsub("\\?","",essential$Drug.Trade.Name.Generic.Name.)


# Clean for Oncomine cases
x <- essential[6,]

x$Drug.Trade.Name.Generic.Name. <- gsub("*\\(.*?\\) *","",x$Drug.Trade.Name.Generic.Name.)

x.drugs <- unlist(strsplit(x$Drug.Trade.Name.Generic.Name.," "))

for(i in seq_along(x.drugs)){
        x <- rbind(x,x)   
}

x$Drug.Trade.Name.Generic.Name.[1:length(x.drugs)] <- x.drugs

x <- x[1:length(x.drugs),]

essential <- essential[-6,]
essential<- rbind(essential, x) # Oncomine added

# Clean for Idhifa (enasidenib),50 and 100 mg tablets

x <- essential[4,]

x$Drug.Trade.Name.Generic.Name.

x.drugs <- c("Idhifa 50mg", "Idhifa 100mg")

for(i in seq_along(x.drugs)){
        x <- rbind(x,x)   
}

x$Drug.Trade.Name.Generic.Name.[1:length(x.drugs)] <- x.drugs
x <- x[1:length(x.drugs),]

essential <- essential[-4,]
essential<- rbind(essential, x) # Idhifa added


# Cleaning for The cobas? KRAS Mutation Test

x <- essential[which(essential$Device.Trade.Name == "The cobas? KRAS Mutation Test"),]

x$Drug.Trade.Name.Generic.Name. <- gsub("*\\(.*?\\) *|;","",x$Drug.Trade.Name.Generic.Name.)

x.drugs <- unlist(strsplit(x$Drug.Trade.Name.Generic.Name.," "))[c(1,3)]

for(i in seq_along(x.drugs)){
        x <- rbind(x,x)   
}

x$Drug.Trade.Name.Generic.Name.[1:length(x.drugs)] <- x.drugs
x <- x[1:length(x.drugs),]

essential <- essential[-17,]
essential<- rbind(essential, x) #  added

# therascreen KRAS RGQ PCR Kit

x <- essential[which(essential$Device.Trade.Name == "therascreen KRAS RGQ PCR Kit"),]

x$Drug.Trade.Name.Generic.Name. <- gsub("*\\(.*?\\) *|;","",x$Drug.Trade.Name.Generic.Name.)

x.drugs <- unlist(strsplit(x$Drug.Trade.Name.Generic.Name.," "))[c(1,3)]

for(i in seq_along(x.drugs)){
        x <- rbind(x,x)   
}

x$Drug.Trade.Name.Generic.Name.[1:length(x.drugs)] <- x.drugs
x <- x[1:length(x.drugs),]

essential <- essential[-18,]
essential<- rbind(essential, x) #  added

# DAKO EGFR PharmDx Kit

