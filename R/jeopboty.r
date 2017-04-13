

#' R code from Rmarkdown file
#'
#' Converts Rmarkdown to R script file
#'
#' @param x File name of Rmd file
#' @param verbose Logical indicating whether to print (cat) the
#'   script code. Defaults to TRUE.
#' @return String with script syntax.
rRMD <- function(x, verbose = TRUE) {
    ## read Rmd file
    x <- readLines(x)
    ## remove meta info
    if ("---" == x[[1]]) x <- x[-c(1:(which(x[-1] == "---") + 1))]
    ## remove chunk specs
    x <- lapply(x, function(.) gsub("```.*", "```", .))
    ## collapse
    x <- paste(x, collapse = "\n")
    ## split by tick marks
    x <- strsplit(x, "```")[[1]]
    ## select the first in each pair of tick marks
    comnt <- x[seq(1, NROW(x), 2)]
    ## select the second in each pair of tick marks
    rcode <- x[seq(2, NROW(x), 2)]
    ## if uneven provide blanks
    if (length(comnt) > length(rcode)) {
        rcode <- c(rcode, rep("", length(comnt) - length(rcode)))
    }
    ## combine each pair into vector and create a list
    x <- mapply(function(a, b) list(c(a, b)), comnt, rcode,
                USE.NAMES = FALSE)
    ## format comments and r code, return string
    formatrcode <- function(x) {
        if (grepl("^---", x[[1]])) return("")
        if (grepl("^\nknitr::opts_chunk", x[[2]])) return("")
        cmmnt <- gsub("#{2,} ", "", x[[1]])
        cmmnt <- strsplit(cmmnt, "\n")[[1]]
        cmmnt <- grep("^<", cmmnt, value = TRUE, invert = TRUE)
        cmmnt <- paste("##", cmmnt)
        cmmnt <- cmmnt[cmmnt != "## "]
        rcode <- gsub("^\n|\n$", "", x[[2]])
        paste(c(cmmnt, rcode), collapse = "\n")
    }
    ## apply formatrcode
    x <- lapply(x, formatrcode)
    ## collapse with line breaks
    x <- paste(x, collapse = "\n\n")
    ## print if verbose
    if (verbose) cat(x, fill = TRUE)
    ## return code string
    return(invisible(x))
}
