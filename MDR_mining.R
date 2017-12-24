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
