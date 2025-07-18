#Welche Abstimmung?
abstimmung_date <- "September2025"
voting_date <- "2025-09-28"
date_voting <- "20250928"

#Save texts? Simulation? Default FALSE
simulation <- FALSE
save_texts <- FALSE

#Output for special Einzugsgebiete? Default FALSE
SPECIAL_AREAS <- FALSE

#Mail
DEFAULT_MAILS <- "contentdevelopment@keystone-sda.ch, robot-notification@awp.ch"
#DEFAULT_MAILS <- "robot-notification@awp.ch"

#JSON Feeds
FEED_NATIONAL <- "https://app-prod-static-voteinfo.s3.eu-central-1.amazonaws.com/v1/ogd/sd-t-17-02-20250928-eidgAbstimmung.json"
FEED_CANTONAL <- "https://app-prod-static-voteinfo.s3.eu-central-1.amazonaws.com/v1/ogd/sd-t-17-02-20250928-kantAbstimmung.json"

#Spezialfälle?
other_check <- FALSE


sprachen <- c("de","fr","it")

monate_de <- c("Januar", "Februar", "März", 
               "April", "Mai", "Juni", "July", 
               "August", "September", "Oktober",
               "November", "Dezember")

monate_fr <- c("janvier","février","mars",
               "avril","mai","juin","juillet",
               "août","septembre","octobre",
               "novembre","décembre")

monate_it <- c("gennaio","febbraio","marzo",
               "aprile","maggio","giugno",
               "luglio","agosto","settembre",
               "ottobre","novembre","dicembre")