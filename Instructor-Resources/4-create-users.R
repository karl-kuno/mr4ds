library(tidyverse)

marvel <- read_csv("Instructor-Resources/marvel-wikia-data.csv")
students <- read_csv("Instructor-Resources/Registrations_MRS_SVMT_03082017_registrants.csv")

create_creds <- function() {
  
  library(stringr)
  library(tidyverse)
  
  chars <- marvel %>% 
    slice(1:nrow(students)) %>% 
    select(name)
  
  chars <- chars %>% 
    mutate(char_name = substr(name, 1, str_locate(name, "\\(") - 2), 
           character = str_replace_all(str_to_lower(char_name), " ", "-")) %>% 
    select(-char_name, -name)
  
  names(students) <- str_to_lower(str_replace_all(names(students), " ", "_"))
  
  creds <- bind_cols(chars, students %>% select(first_name, last_name))
  
  return(creds)
  
}



install.packages('gmailr', repos = 'https://mran.revolutionanalytics.com/snapshot/2016-05-01')
library(gmailr)
use_secret_file("gmailr-mr4ds.json")

# find key here
# https://console.developers.google.com/apis/credentials?project=gmailr-mr4ds


# Import ------------------------------------------------------------------

library(readr)
creds <- read_csv("sampa-creds.csv")
names(creds) <- c("first", "last", "email")
library(dplyr)
library(stringr)


my_dat <- creds %>% mutate(username = str_to_lower(str_replace_all(first, " |-", ""))) %>% 
  mutate(username = paste0(username, str_to_lower(substr(last, 1, 1))),
         password = paste0(username, "1234!"))



# test --------------------------------------------------------------------


test_email <- mime(
  To = "alizaidi@microsoft.com",
  From = "learn.mr4ds@gmail.com",
  Subject = "this is just a gmailr test",
  body = "Can you hear me now?")

send_message(test_email)

## verify that the email arrives succesfully!



# create usernames --------------------------------------------------------

library(data.table)
library(dplyr)
library(stringr)

comics <- fread("https://raw.githubusercontent.com/fivethirtyeight/data/master/comic-characters/marvel-wikia-data.csv")
comics <- comics %>% tbl_df

heros <- comics %>% 
  arrange(desc(APPEARANCES)) %>% select(name) %>% 
  mutate(first = str_locate(pattern = "[(]", name)[, "start"], 
         cool_name = tolower(str_replace(substr(name, 1, first - 2), " |-", ""))) %>% 
  filter(nchar(cool_name) < 10, !str_detect(cool_name, " "))

credentials <- heros %>% select(cool_name) %>% mutate(password = paste0(cool_name, "123!"))

credentials %>% slice(1:50) %>% write.csv(file = "credentials.csv", row.names = F, quote = FALSE)

logins <- credentials %>% slice(1:50)


# credentials -------------------------------------------------------------

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(purrr))
library(readr)
library(stringr)

my_dat <- read_csv("Instructor-Resources/toronto.csv")
names(my_dat) <- c("first", "last", "email")

my_dat <- my_dat %>% 
  mutate(username = str_to_lower(str_replace_all(first, " |-", ""))) %>% 
  mutate(username = paste0(username, str_to_lower(substr(last, 1, 1))),
         password = paste0(username, "1234!"))


subject <- "Login Credentials for Microsoft R Workshop"
email_sender <- 'Ali Zaidi <learn.mr4ds@gmail.com>'
optional_bcc <- 'Ali Zaidi <alizaidi@microsoft.com>'
body <- "Hi, %s.

Your username for the workshop is %s, and your password is %s.

Please login at http://sampads.eastus2.cloudapp.azure.com:8787/ for RStudio, and \n
https://sampads.eastus2.cloudapp.azure.com:8000/ for JupyterHub.


Thanks for participating in this workshop!

Ali Zaidi
"


names(my_dat) <- str_replace_all(names(my_dat), " ", ".")

edat <- my_dat
mutate(
  To = sprintf('%s <%s>', First.Name, Email),
  Bcc = optional_bcc,
  From = email_sender,
  Subject = subject,
  body = sprintf(body, first, username, password)) %>%
  select(To, Bcc, From, Subject, body)
edat
write_csv(edat[, ], "composed-emails.csv")

edat <- my_dat %>% 
  mutate(
    To = sprintf('%s <%s>', first, email),
    Bcc = optional_bcc,
    From = email_sender,
    Subject = subject,
    body = sprintf(body, first, username, password)) %>%
  select(To, Bcc, From, Subject, body)
edat


emails <- edat %>%
  pmap(mime)


safe_send_message <- safely(send_message)
sent_mail <- emails %>%
  map(safe_send_message)

saveRDS(sent_mail,
        paste(gsub("\\s+", "_", subject), "sent-emails.rds", sep = "_"))

errors <- sent_mail %>%
  transpose() %>%
  .$error %>%
  map_lgl(Negate(is.null))
sent_mail[errors]

