library(readxl)
library(readr)
library(dplyr)
library(lubridate)
library(purrr)
library(stringr)
#Appointees -----
Secretaries <- read_excel("~/Documents/Cabinet Positions.xlsx", sheet = "Cabinet Positions")
Secretaries$EndDate <- mdy(Secretaries$EndDate)
Secretaries$StartDate <- mdy(Secretaries$StartDate)
Secretaries$ID <- 1:nrow(Secretaries)

Appointments <- filter(Secretaries, Position %in% c("Secretary of State", "Secretary of the Treasury", "Secretary of the Interior", "Postmaster General", "Attorney General", "Secretary of Transportation", "Secretary of Commerce", "Secretary of Commerce & Labor", "Secretary of Defense", "Secretary of Education", "Secretary of Energy", "Secretary of Health and Human Services", "Secretary of Health, Education, and Welfare", "Secretary of Homeland Security", "Secretary of Housing and Urban Development", "Secretary of Labor", "Secretary of Agriculture", "Secretary of Veterans Affairs", "Secretary of the Navy", "Secretary of War"))
AppointConf <- filter(Appointments, Acting == 0)

AppointConf$TermLength <- as.numeric(difftime(AppointConf$EndDate, AppointConf$StartDate, units = "days"))
AppointConf$PresName <- paste(AppointConf$AdministrationFirst, AppointConf$Administration)
summary(AppointConf$TermLength)

AppointConf$PresName<- str_replace(AppointConf$PresName, "William Clinton", "Bill Clinton")
AppointConf$PresName<- str_replace(AppointConf$PresName, "James Garfield", "James A. Garfield")
AppointConf$PresName<- str_replace(AppointConf$PresName, "William H. Taft", "William Howard Taft")
AppointConf$PresName<- str_replace(AppointConf$PresName, "Richard M. Nixon", "Richard Nixon")
AppointConf$PresName<- str_replace(AppointConf$PresName, "William Clinton", "Bill Clinton")

write_csv(AppointConf, path = "~/Desktop/Cabinet Positions Clean.csv")
summary(AppointConf$TermLength)
head(arrange(AppointConf, desc(TermLength)), n = 20)

#Presidents -----
Presidents <-read_csv("~/Desktop/USPresidentsKaggle.txt")
Presidents <- Presidents[, -1]
Presidents$prior <- str_replace_all(Presidents$prior, pattern = "â€“", replacement = "-")

Presidents$party <- str_replace_all(Presidents$party, pattern = "\\[.\\]", replacement = "")
Presidents$party <- str_replace_all(Presidents$party, pattern = "\\[..\\]", replacement = "")
Presidents$party <- str_replace_all(Presidents$party, pattern = "National Union", replacement = "")
Presidents$party <- str_replace_all(Presidents$party, pattern = "\\( | \\)", replacement = "")
Presidents$party <- str_replace_all(Presidents$party, fixed(pattern = "  "), replacement = "")
Presidents$party <- str_replace_all(Presidents$party, pattern = " .*", replacement = "")

Presidents[44, "end"] <- "January 20, 2017"
Presidents[32, "end"] <- "April 12, 1945"
Presidents$start <- mdy(Presidents$start)
Presidents$end <- mdy(Presidents$end)
Presidents$TermLength <- as.numeric(difftime(Presidents$end, Presidents$start, units = "days"))
summary(Presidents$TermLength)

write.csv(Presidents, "~/Desktop/Presidents.csv")





test <- select(Presidents, president, TermLength)
print(test, n= 45)
