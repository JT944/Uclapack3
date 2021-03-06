#' Resturant
#'@description simply typing the city name you want to check and your yelp api key, the function will give you all information (address,rate, etc) of 50 resturants in that city.
#' @author Jiatong Li

#'
#' @title Get information of resturants from yelp using your own yelp api key
#' @return
#' @export
#' @usage Resturant(api1="your client id ",api_key1= "your client_secret",i="City,State")
#'
#' @examples Resturant(api1="q-f2lLVGZkys5AVkVPWsuQ",api_key1 =  "tLcN4rX8BzXZTSE5fYKHntHgSbjHilyGQV8VoXq0zGgiwP7RM8rS0PCOuRyRpn6UkMsEE9gnXWqEs2V32IreXfwVjao7iiAgKeJQ85u2n_mbuZn3-Tuq3R7qs4CPX3Yx",i="Irvine,CA")

Resturant=function(api1,api_key1,i){
  library(httr)
client_id <- api1
client_secret <- api_key1
res <- POST("https://api.yelp.com/oauth2/token",
            body = list(grant_type = "client_credentials",
                        client_id = client_id,
                        client_secret = client_secret))

token <- content(res)$access_token

yelp <- "https://api.yelp.com"
term <- "Restaurant"
location <- i
categories <- NULL
limit <- 50
radius <- 8000
url <- modify_url(yelp, path = c("v3", "businesses", "search"),
                  query = list(term = term, location = location,
                               limit = limit,
                               radius = radius))
res <- GET(url, add_headers('Authorization' = paste("bearer", client_secret)))

results <- content(res)

yelp_httr_parse <- function(x) {

  parse_list <- list(
    name = x$name,
    rating = x$rating,

    address1 = x$location$address1
  )

  parse_list <- lapply(parse_list, FUN = function(x) ifelse(is.null(x), "", x))

  df <- data.frame(
    name=parse_list$name,
    rating = parse_list$rating,
    address1 = parse_list$address1
  )
  df
}

results_list <- lapply(results$businesses, FUN = yelp_httr_parse)

resturant <- do.call("rbind", results_list)
resturant
}


