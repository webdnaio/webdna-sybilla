#' Available classes.
#' 
#' Important!
#' This vector MUST BE synchronized with class values in PHP application!
#' 
#' @name Available classes
CLASSES <- c(
    'positive' = 1,
    'negative' = 2,
    'unknown' = 999
)

#' Load current model from disk
#'
#' @export
#' @return RF model
load_current_model <- function() {
    readRDS(file.path(system.file(package = "webdnaSybilla"), "extdata", "models", "current.rds"))
}

#' Check for malware detection
#'
#' @param metrics
#' @return logical
is_malware <- function(metrics) {
    as.logical(metrics$document_security_google_safe_browsing != 0)
}

#' Check for non-ok http_code
#'
#' @param metrics
#' @return logical
is_invalid_http_code <- function(metrics) {
    http_code <- as.integer(levels(metrics$http_code))[metrics$http_code]
    
    as.logical(http_code < 200 || http_code >= 400)
}

#' Compute all violation on return logical vector
#'
#' @param metrics
#' @return logical
is_violation <- function(metrics) {
    c(
        malware = is_malware(metrics),
        invalid_http_code = is_invalid_http_code(metrics)
    )
}

#' Classify link by metrics, for single link
#' 
#' @param values
#' @param label
#' @export
#' @return Classification info about metrics
classify_metrics <- function(values, labels) {
    classification <- tryCatch({
        values <- sapply(values, as.numeric)
        names(values) <- labels
        
        data <- data.frame(as.list(values))
        data <- clean_raw_data(data)
        
        # Prediction with RF model should be done if and only if
        # when there are no basic known violations like
        # invalid http_code or detected malware.
        if (any(is_violation(as.list(data)))) {
            # Synthetic result when violation has been detected
            data.frame(negative = 1.0, positive = 0.0)
        } else {
            predict(model, data, type = "prob")
        }
    }, error = function(e) {
        'unknown'
    })
    
    classification
}