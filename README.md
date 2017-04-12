jeopboty
================

My \#jeopboty bot
=================

Reading data and the \[correct\] Twitter token
----------------------------------------------

load rtweet

``` r
library(rtweet)
```

If you haven't created and saved a personal access Twitter API token, do it now following these instructions:

``` r
vignette("auth", "rtweet")
```

Make sure you've given the token "write" access (permission) which you can check through your Twitter account. Uncomment next line to open browser to Twitter's app page. browseURL("<https://apps.twitter.com>")

If you already saved your token as an environment variable, you can fetch it using the get\_tokens() function.

``` r
token <- get_tokens()
```

I have a token I use exclusively for my \#jeopboty statuses, so I've saved that in my data folder (note: token won't appear in my Github repository; nice try!)

``` r
token <- readRDS("/Users/mwk/rtw.rds")
```

Get the screen name associated with your token.

``` r
my_screen_name <- token$credentials$screen_name
```

This should print out YOUR screen name:

``` r
my_screen_name
```

Read jeopardy data.

``` r
jeop <- readRDS("../data/joepardy.rds")
```

Posting the CLUE status on Twitter
----------------------------------

Randomly select a question.

``` r
i <- sample(seq_len(NROW(jeop)), 1L)
```

Compose CLUE status.

``` r
clue <- paste0("Clue: ", jeop$clue[i], " #jeopboty")
```

Post CLUE status to Twitter.

``` r
post_tweet(clue, token = token)
```

Keeping track of questions you've tweeted over time
---------------------------------------------------

Either extract and update or create "used" data object.

``` r
if ("used" %in% names(attributes(jeop))) {
    ## Extract used data attribute
    used <- attr(jeop, "used")

    ## Determine clue [number] n post.
    n <- NROW(used[["id"]]) + 1L

    ## Update (append) used object
    used[["id"]][n] <- n
    used[["datetime"]][n] <- Sys.time()
    used[["clue"]][n] <- jeop$clue[i]
    used[["answer"]][n] <- jeop$answer[i]

} else {
    ## Create used object.
    used <- data.frame(
        id = n,
        datetime = Sys.time(),
        clue = jeop$clue[i],
        answer = jeop$answer[i])
}
```

Add "used" data attribute to "jeop" data.

``` r
attr(jeop, "used") <- used
```

Save data (dropping the row that was just posted.

``` r
saveRDS(jeop[-i, ], "/Users/mwk/r/joepardy.rds")
```

Posting the ANSWER status on Twitter
------------------------------------

Get recent timeline data for your account.

``` r
tw <- get_timeline(my_screen_name)
```

Find and return status ID of most recent clue.

``` r
status_id <- tw$status_id[grep("^Clue", tw$text)[1]]
```

Use the status ID to create quote link.

``` r
quote <- paste0("https://twitter.com/",
                my_screen_name,
                "/status/",
                status_id)
```

Compose ANSWER status.

``` r
answer <- paste0(
    "Answer: ", jeop$answer[i], " #jeopboty ", quote)
```

Post ANSWER status to Twitter.

``` r
post_tweet(answer, token = token)
```
