library(rtweet)
library(dplyr)
library(tidyr)
library(reactable)
library(glue)

#https://youtu.be/O0gTv9VGRig?t=24
# bonus video for Shiny :  https://www.infoworld.com/video/100759/how-to-code-an-interactive-shiny-app-to-search-twitter-do-more-with-r-bonus-video

tweet_df = search_tweets("VirusStalker", n=500, include_rts = T)
saveRDS(tweet_df, "tweet_df.rds")
tweet_df = readRDS("tweet_df.rds")


res = tweet_df %>% select(user_id, status_id, created_at, screen_name, text, favorite_count, retweet_count, urls_expanded_url) %>%
  mutate(
    Tweet = glue::glue("{text} <a href='https://twitter.com/{screen_name}/status/{status_id}'>>> </a>") 
  )%>%
  select(DateTime = created_at, User = screen_name, Tweet, Likes = favorite_count, RTs = retweet_count, URLs = urls_expanded_url)


# show unique URLs
res %>%select(URLs) %>% unnest(cols=c(URLs)) %>% filter (!is.na(URLs)) %>% unique() %>% pull()


make_url_html <- function(url) {
  if(length(url) < 2) {
    if(!is.na(url)) {
      as.character(glue("<a title = {url} target = '_new' href = '{url}'>{url}</a>") )
    } else {
      ""
    }
  } else {
    paste0(purrr::map_chr(url, ~ paste0("<a title = '", .x, "' target = '_new' href = '", .x, "'>", .x, "</a>", collapse = ", ")), collapse = ", ")
  }
}

# convert URLs to html links
res$URLs <- purrr::map_chr(res$URLs, make_url_html)

reactable(res, filterable = TRUE, searchable = TRUE, bordered = TRUE, 
          striped = TRUE, highlight = TRUE,
          defaultPageSize = 25, showPageSizeOptions = TRUE, 
          showSortable = TRUE, pageSizeOptions = c(25, 50, 75, 100, 200), defaultSortOrder = "desc",
          columns = list(
            DateTime = colDef(defaultSortOrder = "asc"),
            User = colDef(defaultSortOrder = "asc"),
            Tweet = colDef(html = TRUE, minWidth = 190, resizable = TRUE),
            Likes = colDef(filterable = FALSE),
            RTs = colDef(filterable =  FALSE),
            URLs = colDef(html = TRUE)
          )
) 


