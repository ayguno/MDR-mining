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
        event <- gsub(" ","+",toupper(event))
        
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


#####################################
# Write a function that calculates PRR from all reports associated with all events for a given drug
# Use the yearlyPRR function described above

drugPRR <- function(drug,start.date = "1998-01-01", end.date = as.character(Sys.Date())){
        
        require(jsonlite)
        require(lubridate)
        require(dplyr)
        
        end.date <- gsub("-","", end.date)
        start.date <- gsub("-","", start.date)
        
        drug <- toupper(drug)
        # Get the names of events associated with a drug in a given time window
        
        events.list <- unique(fromJSON(paste0('https://api.fda.gov/drug/event.json?search=receivedate:[',start.date,'+TO+',end.date,']+AND+patient.drug.openfda.brand_name.exact:(%22',drug,'%22)&count=patient.reaction.reactionmeddrapt.exact'))$results$term)
        
        store.temp <- yearlyPRR(drug=drug,event=events.list[1])
        
        if(length(events.list) > 50){
                ## it will time out if it takes too long
                for(i in 2:50){
                        store.temp <- rbind(store.temp,yearlyPRR(drug=drug,event=events.list[i]))  
                } 
                ## it will time out if it takes too long
                for(i in 51:length(events.list)){
                        store.temp <- rbind(store.temp,yearlyPRR(drug=drug,event=events.list[i]))  
                } 
        }else
        {
                for(i in 2:length(events.list)){
                        store.temp <- rbind(store.temp,yearlyPRR(drug=drug,event=events.list[i]))  
                }     
        }
        
}


Herceptin.Data <- drugPRR("Herceptin")











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

x <- essential[which(essential$Device.Trade.Name == "DAKO EGFR PharmDx Kit"),]

x$Drug.Trade.Name.Generic.Name. <- gsub("*\\(.*?\\) *|;","",x$Drug.Trade.Name.Generic.Name.)

x.drugs <- unlist(strsplit(x$Drug.Trade.Name.Generic.Name.," "))

for(i in seq_along(x.drugs)){
        x <- rbind(x,x)   
}

x$Drug.Trade.Name.Generic.Name.[1:length(x.drugs)] <- x.drugs
x <- x[1:length(x.drugs),]

essential <- essential[-18,]
essential<- rbind(essential, x) #  added


# HERCEPTEST

x <- essential[which(essential$Device.Trade.Name == "HERCEPTEST"),]

x$Drug.Trade.Name.Generic.Name. <- gsub("*\\(.*?\\) *|;","",x$Drug.Trade.Name.Generic.Name.)

x.drugs <- c("Herceptin","Perjeta", "Kadcyla")

for(i in seq_along(x.drugs)){
        x <- rbind(x,x)   
}

x$Drug.Trade.Name.Generic.Name.[1:length(x.drugs)] <- x.drugs
x <- x[1:length(x.drugs),]

essential <- essential[-29,]
essential<- rbind(essential, x) #  added


# HER2 FISH PharmDx Kit

x <- essential[which(essential$Device.Trade.Name == "HER2 FISH PharmDx Kit"),]

x$Drug.Trade.Name.Generic.Name. <- gsub("*\\(.*?\\) *|;","",x$Drug.Trade.Name.Generic.Name.)

x.drugs <- c("Herceptin","Perjeta", "Kadcyla")

for(i in seq_along(x.drugs)){
        x <- rbind(x,x)   
}

x$Drug.Trade.Name.Generic.Name.[1:length(x.drugs)] <- x.drugs
x <- x[1:length(x.drugs),]

essential <- essential[-29,]
essential<- rbind(essential, x) #  added

# THxID? BRAF Kit

x <- essential[which(essential$Device.Trade.Name == "THxID? BRAF Kit"),]

x$Drug.Trade.Name.Generic.Name. <- gsub("*\\(.*?\\) *|;","",x$Drug.Trade.Name.Generic.Name.)

x.drugs <- unlist(strsplit(x$Drug.Trade.Name.Generic.Name.," "))[c(1,3)]

for(i in seq_along(x.drugs)){
        x <- rbind(x,x)   
}

x$Drug.Trade.Name.Generic.Name.[1:length(x.drugs)] <- x.drugs
x <- x[1:length(x.drugs),]

essential <- essential[-29,]
essential<- rbind(essential, x) #  added


# DAKO C-KIT PharmDx


x <- essential[which(essential$Device.Trade.Name == "DAKO C-KIT PharmDx"),]

x$Drug.Trade.Name.Generic.Name. <- gsub("*\\(.*?\\) *|;","",x$Drug.Trade.Name.Generic.Name.)

x.drugs <- c("Gleevec","Glivec")

for(i in seq_along(x.drugs)){
        x <- rbind(x,x)   
}

x$Drug.Trade.Name.Generic.Name.[1:length(x.drugs)] <- x.drugs
x <- x[1:length(x.drugs),]

essential <- essential[-20,]
essential<- rbind(essential, x) #  added


essential$Drug.Trade.Name.Generic.Name. <- gsub("*\\(.*?\\) *","",essential$Drug.Trade.Name.Generic.Name.)

# Save this relatively clean version of the essential table
write.csv(essential,"essential_clean.csv")

###############################################################
# Read files back to start developing a prototype application
###############################################################

essential.clean <- read.csv("essential_clean.csv")
Herceptin.Data <- readRDS("herceptin_data.rds")

# List of drugs
drug.list <- stringr::str_trim(unique(as.character(essential.clean$Drug.Trade.Name.Generic.Name.)))

# Save to use in the app later:
saveRDS(drug.list,"/Users/OZANAYGUN/Desktop/2016/Data_science/companion/drug_list.rds")

# General query to bring events list for a given drug
events.list <- unique(fromJSON(paste0('https://api.fda.gov/drug/event.json?search=receivedate:[',start.date,'+TO+',end.date,']+AND+patient.drug.openfda.brand_name.exact:(%22',drug,'%22)&count=patient.reaction.reactionmeddrapt.exact'))$results$term)


start.date = "1998-01-01"; end.date = as.character(Sys.Date());drug = "HERCEPTIN"
Herceptin.events.list <- unique(fromJSON(paste0('https://api.fda.gov/drug/event.json?search=receivedate:[',start.date,'+TO+',end.date,']+AND+patient.drug.openfda.brand_name.exact:(%22',drug,'%22)&count=patient.reaction.reactionmeddrapt.exact'))$results$term)

# Save to use in the app later:
saveRDS(Herceptin.events.list,"/Users/OZANAYGUN/Desktop/2016/Data_science/companion/Herceptin.events.list.rds")

######################################################
# Prepare time-series plots using the drug PPR data
######################################################

drug.data <- Herceptin.Data[Herceptin.Data$event == "FATIGUE",] # Use for the example, generalize later

##################################################################################
# Input: data frame generated by yearlyPRR

# Infer and Set the date range from the drug table:
year.range <- range(drug.data$year)

start_date <- date(format(paste(year.range[1],"01","01", sep = "-")))
end_date <- date(format(paste(year.range[2],"01","01", sep = "-")))

# Set the yearly points as the end of the year
drug.data$end.year <- date(format(paste(drug.data$year,"12","30",sep = "-")))

p <- ggplot(data = drug.data , aes(x = end.year , y = PRR))+
     geom_line(colour = "purple", size = 2)+
     geom_point(colour = "navy", size = 4)+
     labs(x="Date", y = "PRR", title = paste0("Yearly PRR of adverse event ",unique(drug.data$event)," for drug ",unique(drug.data$drug)))+
        theme(panel.background=element_rect(fill = "white",colour = "black"),
              panel.border=element_rect(colour = "black", fill = NA),
              plot.title = element_text(hjust = 0.5, face = "bold",size = 15),
              axis.title = element_text(size = 13, face = "bold"),
              axis.text = element_text(size = 13, face = "bold"))

return(p)
##################################################################################

