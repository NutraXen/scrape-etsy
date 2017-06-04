library(dplyr) # Data management
library(rvest) # Web scrapping

# My connection is rather unstable, so I had to create some fixes in case the
# connection dropped (and this is good practice anyway)

# When I time out, I print the error and pause the code for 10 seconds (enough
# time to recover the connection.
err <- function(e) {
    print(e)
    # Sys.sleep(10)
}

load("output.RData") # Use this if you've already have started the scraping.
# out <- data.frame() # Use this if this is the first iteration.
for (i in 1:1241) {
    
    # Reading the page i of the search results. 
    go_1 <- 0
    while (go_1 == 0) {
        tryCatch({
            url_page <- paste("https://www.etsy.com/au/search/shops?",
                              "order=most_recent&page=",
                              i, sep = "")
            sellers <- read_html(url_page) %>%
                html_nodes(".shopname") %>%
                html_nodes("a") %>%
                html_attrs() %>%
                unlist()
            go_1 <- 1
        }, error = err, warning = err) 
        # In case of error (timeout), run the function 'err'
    }
    
    links <- substr(sellers, 1, nchar(sellers) - 15)
    # Remove links that are already in the database:
    links <- links[!links %in% out$url] 
    
    # Looping over the 10 results of the page:
    for (url in links) {
        
        go_2 <- 0
        while (go_2 == 0) {
            tryCatch({
                page <- read_html(url)
                go_2 <- 1
            }, error = err, warning = err)
            # In case of error (timeout), run the function 'err'
        }
        
        shop_number <- which(url == links)
        
        shop_name <- sub("https://www.etsy.com/au/shop/", "", url)
        
        # Every time, we specify the 'node' at which the information is, and
        # functions from the 'rvest' package extract the information.
        
        name <- page %>% 
            html_node(".larger-name") %>% 
            html_text()
        
        location <- page %>%
            html_node(".shop-location") %>%
            html_text()
        
        since <- page %>%
            html_node(".etsy-since") %>%
            html_text() 
        since <- as.numeric(gsub("[^0-9]", "", since))
        
        nb_sales <- page %>%
            html_node(".shop-sales") %>%
            html_text()
        nb_sales <- as.numeric(gsub("[^0-9]", "", nb_sales))
        
        admirers <- page %>%
            html_nodes(".mt-lg-5") %>%
            html_nodes("a") %>%
            html_text() %>%
            tail(1)
        admirers <- as.numeric(gsub("[^0-9]", "", admirers))
        if (length(admirers) == 0) admirers <- NA # In case of no admirer
        
        reviews <- page %>%
            html_node(".total-rating-count") %>%
            html_text()
        reviews <- as.numeric(gsub("[^0-9]", "", reviews))
        
        if (is.na(reviews)) {
            score <- NA # If no review, no score.
        } else {
            score <- page %>%
                html_node(".reviews-link-shop-info") %>%
                html_node(".screen-reader-only") %>%
                html_text()
            score <- as.numeric(sub(" out of 5 stars", "", score))
        }
        
        if (is.na(reviews)) {
            last_review <- NA # If no review, no "last review".
        } else {
            last_review <- page %>%
                html_node(".review-item") %>%
                html_node("p") %>%
                html_text()
            last_review <- gsub("  |\n", "", last_review)
            last_review <- paste(tail(strsplit(last_review, " ")[[1]], 3),
                                 collapse = " ")
            last_review <- as.Date(last_review, format = "%b %d, %Y")
        }
            
        out <- rbind(out, 
                     data.frame(shop_name, 
                                url, 
                                name,
                                location,
                                since, 
                                nb_sales,
                                admirers,
                                reviews,
                                score,
                                last_review))
        print(paste("Page", i, "Shop number", shop_number))
    }
    # Saving at each page iteraction to avoid losing everything (power outage...)
    save(out, file = "output.RData")
    
    # Useful when connection dropping. R opens "files" or "connections" but
    # doesn't close them in case of error. At some point, it runs into an error
    # 'too many connections are opened". This function prevents this:
    closeAllConnections()
}

out <- unique(out)
save(out, file = "output.RData")

write.csv(out, file = "output.csv", row.names = F)










