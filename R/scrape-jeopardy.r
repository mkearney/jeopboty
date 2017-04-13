

## This function scrapes and returns clues and answers from
## a comprehensive Jeopary archive.
get_jeopardy <- function(gid) {
    require(rvest)
    get_value <- function(x) {
        value <- x %>%
            html_nodes("td.clue_value_daily_double") %>%
            html_text()
        if (!isTRUE(length(value) > 0L)) {
            value <- x %>%
                html_nodes("td.clue_value") %>%
                html_text()
        }
        value
    }
    get_answer <- function(x) {
        x %>%
            html_attr("onmouseover") %>%
            read_html() %>%
            html_nodes("em.correct_response") %>%
            html_text()
    }
    ## send GET request (and store response)
    r <- httr::GET(paste0(
        "http://www.j-archive.com/showgame.php?game_id=", gid))
    ## read as HTML
    jeop.html <- r[["content"]] %>%
        rawToChar() %>%
        read_html()
    ## parse clues
    clue <- jeop.html %>%
        html_nodes("table td.clue_text") %>%
        html_text() %>%
        lapply(paste, collapse = " ") %>%
        unlist(use.names = FALSE)
    ## parse values
    value <- jeop.html %>%
        html_nodes("table.clue_header") %>%
        lapply(get_value) %>%
        lapply(paste, collapse = " ") %>%
        unlist(use.names = FALSE)
    ## parse answers
    answer <- jeop.html %>%
        html_nodes("table tr td div") %>%
        lapply(get_answer) %>%
        lapply(paste, collapse = " ") %>%
        unlist(use.names = FALSE)
    ## parse categories
    category <- jeop.html %>%
        html_nodes("table td.category_name") %>%
        html_text() %>%
        lapply(paste, collapse = " ") %>%
        unlist(use.names = FALSE) %>%
        .[c(rep(1:6, 5), rep(1:6, 5))]
    ## create data frame
    data.frame(
        category = category[1:NROW(value)],
        value = value[1:NROW(value)],
        answer = answer[1:NROW(value)],
        clue = clue[1:NROW(value)],
        stringsAsFactors = FALSE
    )
}

## Get Jeopardy data or if error return empty data frame
get_jeop <- function(x) {
    dat <- tryCatch(
        get_jeopardy(x),
        error = function(e) return(NULL))
    if (is.null(dat)) {
        dat <- data.frame(
            category = character(),
            value = character(),
            answer = character(),
            clue = character(),
            stringsAsFactors = FALSE)
    }
    dat
}

## I only selected a subset of episodes for the sake of
## time but this could be: 1:5602 (as of 04/12/2017)
jeop <- lapply(3500:4000, get_jeop)
## collapse list into single data frame
jeop <- do.call("rbind", jeop)
## preview wdata
head(jeop)
## count rows
nrow(jeop)

## Shuffle question order (for the thrill).
jeop <- jeop[sample(seq_len(NROW(jeop))), ]

## Save jeop data into data directory.
saveRDS(jeop, "../data/jeopardy.rds")

