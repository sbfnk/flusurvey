##' Question codes of the FluSurvey questionnaires
##'
##' Readable names for the question codes, by season and questionnaire.
##' \code{\link{read_data}} uses them to rename the columns of the files it
##' reads.
##'
##' @format A list with one element per season (named by the year in which the
##'   season ends), each a list with one named character vector per
##'   questionnaire that maps question codes to readable names
##' @source \code{R/define_questions.r}
"questions"

##' Answer codes of the FluSurvey questionnaires
##'
##' Labels for the coded answers, by question. \code{\link{read_data}} uses
##' them to turn coded answers into factors.
##'
##' @format A list with one named character vector per question that maps
##'   answer codes to labels
##' @source \code{R/define_questions.r}
"options"

##' Regions of UK postcode districts
##'
##' @format A \code{data.table} with columns \code{postcode} (the postcode
##'   district) and \code{region}
##' @source \code{inst/extdata/postcodes.csv}, read by
##'   \code{scripts/geography.r}
"regions"

##' Country and settlement type of UK postcode districts
##'
##' @format A \code{data.table} with columns \code{postcode} (the postcode
##'   district), \code{country} and \code{settlement.type}
##' @source \code{inst/extdata/urban_rural.csv}, read by
##'   \code{scripts/geography.r}
"urban_rural"
