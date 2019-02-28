#########################################################################
#                            DEDUPE
#########################################################################

dedupe <- function(dataset, identity, exclude, blockfld){

    con <- dbConnect(odbc(),
                     Driver = "SQL Server",
                     Server = "mandc-sql-01",
                     Database = "SEE_REP",
                     Trusted_Connection = "True")

    rpairs <- RLBigDataDedup(dataset
                             , identity = identity
                             , exclude = exclude
                             , blockfld = blockfld
                             , strcmp = TRUE
                             , strcmpfun = "levenshtein"
                             # , phonetic = c(5:6), phonfun = "pho_h"
                            )


    # EPI WEIGHTS (Simpler version of EM, much faster)
    rpairs <- epiWeights(rpairs)
    #summary(rpairs)

    # EVALUATION
    evaluation_1 <- hist(rpairs@Wdata)
    #return(evaluation_1)
    #threshold <- getParetoThreshold(rpairs,interval=c(0.6,0.8))

    # CLERICAL REVIEW
    #tail(getPairs(rpairs, 0.9, 0.89))

    # THRESHOLD SETTING (DISCERNMENT BETWEEN MATCHES AND NON-MATCHES). MODEL SUMMARY
    result <- epiClassify(rpairs, 0.5)

    # This evaluation is based on the identity parameter
    #summary(result)
    #getTable(result)
    #getErrorMeasures(result)
    #getFalseNeg(result)

    # GETTING LINKS
    matchedPairs <- getPairs(result,min.weight=0.5, filter.link="link", single.rows=T)

    # DEDUPE
    matchedPairs  <-  matchedPairs[order(matchedPairs$id.1),]
    matchedPairs  <-  matchedPairs[!duplicated(matchedPairs$id.2),]

    evaluation_2 <- hist(matchedPairs$Weight[matchedPairs$is_match==0], freq = FALSE
                         , xlab="Weight", main="False Positives")

    # FINAL FORMAT
    colnames(matchedPairs) <- gsub("\\.", "_", colnames(matchedPairs))
    matchedPairs$Class <- NULL
    matchedPairs$is_match <- ifelse(grepl("TRUE", matchedPairs$is_match), "1", "0")
    names(matchedPairs)[names(matchedPairs) == 'id_1']  <- 'goldenID'

    input_table <- readline(prompt = "Table to write: ")
    dbWriteTable(con, input_table, matchedPairs)

    return(list(evaluation_1, evaluation_2))
}
