#########################################################################
#                            LINKAGE
#########################################################################

####################################
## STRING COMPARISON
####################################

linkage <- function(dataset1, dataset2, identity1, identity2, exclude, blockfld){

    con <- dbConnect(odbc(),
                     Driver = "SQL Server",
                     Server = "mandc-sql-01",
                     Database = "SEE_REP",
                     Trusted_Connection = "True")

    rpairs <- RLBigDataLinkage(dataset1 = dataset1, dataset2 = dataset2
                               , identity1 = identity1, identity2 = identity2
                               , exclude = exclude
                               , blockfld = blockfld
                               , strcmp = TRUE
                               , strcmpfun = "levenshtein"
                               # , phonetic = ..., phonfun = "pho_h"
    )


    # EPI WEIGHTS (Simpler version of EM, much faster)
    rpairs <- epiWeights(rpairs)

    # EVALUATION
    evaluation_1 <- hist(rpairs@Wdata)
    #threshold <- getParetoThreshold(rpairs,interval=c(0.6,0.8))

    # CLERICAL REVIEW
    #tail(getPairs(rpairs, 0.5, 0.4373932))

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

    ####################################
    ## OUTPUT
    ####################################
    # 4.1 FINAL FORMAT
    colnames(matchedPairs) <- gsub("\\.", "_", colnames(matchedPairs))
    matchedPairs$Class <- NULL
    matchedPairs$is_match <- ifelse(grepl("TRUE", matchedPairs$is_match), "1", "0")
    names(matchedPairs)[names(matchedPairs) == 'id_1']  <- 'goldenID'

    input_table <- readline(prompt = "Table to write: ")
    dbWriteTable(con, input_table, matchedPairs)

    return(list(evaluation_1, evaluation_2))
}
