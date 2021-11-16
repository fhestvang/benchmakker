### ADDITIONAL
google_app <- httr::oauth_app(
  "benchmakker",
  key = "313741018459-s1s3tal0btkdnakm9abmvhr21ka91uq4.apps.googleusercontent.com",
  secret = "-QxIuHh1MAoear5ijvZZaJLS"
)
google_key <- "AIzaSyCEsKOnqScydQyQWrp3DChm1wlyNlgQXqs"
gs4_auth_configure(app = google_app, api_key = google_key)


## SENDING NOTIFICATIONS THROUGH GMAIL
gm_auth_configure(key = "307192950962-e6e2sofmh3r4ddaku3n6cc8q430b68oq.apps.googleusercontent.com",
                  secret = "HaTPtJx3PiKgmeNLVklPsgsf")
text_msg <- gm_mime() %>%
  gm_to("mrvc@telmore.dk") %>%
  gm_from("update@telmorepricecomp.com") %>%
  gm_subject("[TPC] New sheet available: Telia_2021-02-15") %>% 
  gm_text_body("Date: 2021-02-12 \n Operator: Telia \n Price scraping: Done \n Upload: Done \n Comment: Fixed typo in subscriptionName \n Data available here: https://docs.google.com/spreadsheets/d/1z1o9qTJ6M4TZg6xi0YuBbqz7rcb6icNzJPHzVQAMHOs/edit?usp=sharing")
strwrap(as.character(text_msg))

gm_send_message(text_msg)

## NOTIFICATIONS: TEAMS/SLACK
login <- list(
  text = paste0("Hello! \n The following scraping job is completed succesfully: \n", 
                "*", operatorName, "*", 
                "\n", 
                "Time: ", Sys.time(),
                "\n")
)


res <- POST("https://tdc.webhook.office.com/webhookb2/d847dfa7-c7b6-4081-bcee-bd2768e6af2d@e8dcf6e6-3acc-4af9-9cb2-77f688cb688b/IncomingWebhook/e197f0450f5c443180b9eaccc227ee59/590c8e08-2c5a-4db1-aeed-7c0186789fdc", body = login, encode = "json", verbose())
