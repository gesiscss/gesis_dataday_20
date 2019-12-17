library(data.table)


# The read.SentiWS function read the SentiWS files and a data.table.
# Code adapted from PolMine/sentiws.R (by Andreas Blätte)
# https://gist.github.com/PolMine/70eeb095328070c18bd00ee087272adf

read.SentiWS <- function(){

    .unfold <- function(.SD){
    pos <- gsub("^([A-Z]+)\\s+.*$", "\\1", .SD[["data"]][1])
    weight <- as.numeric(gsub("^[A-Z]+\\s+(-?\\d\\.\\d+).*$", "\\1", .SD[["data"]][1]))
    words <- gsub("^[A-Z]+\\s+-?\\d\\.\\d+\\s*(.*?)\\s*$", "\\1", .SD[["data"]][1])
    words <- if (!grepl("^\\s*$", words)) strsplit(x = words, split = ",")[[1]] else NULL
    list(
      word = c(.SD[["word"]][1], words),
      lemma = .SD[["word"]][1],
      weight = weight
    )
  }

    dts <- lapply(
    c(positive = "SentiWS_v1.8c/SentiWS_v1.8c_Positive.txt", negative = "SentiWS_v1.8c/SentiWS_v1.8c_Negative.txt"),
    function(filename){
      dt <- fread(filename)
      colnames(dt) <- c("word", "data")
      dt[, "id" :=  1L:nrow(dt)]
      dt[, .unfold(.SD), by = c("id")]
    }
  )

    as.data.table(rbindlist(dts)[,c("word","lemma","weight")])
}
