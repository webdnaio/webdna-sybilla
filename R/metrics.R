#' Fields definition
#'
#' @name Available metrics
AVAILABLE_METRICS <- c(
    'class' = 'factor',
    'domain' = 'character',
    'url' = 'character',
    'http_code' = 'factor',
    'domain_whois_created' = 'character',
    'domain_whois_changed' = 'character',
    'domain_whois_expires' = 'character',
    'domain_internet_archive_created' = 'character',
    'domain_moz_domain_authority' = 'numeric',
    'domain_moz_mozrank' = 'numeric',
    'links' = 'numeric',
    'external_equity_links' = 'numeric',
    'document_html_length' = 'numeric',
    'document_text_length' = 'numeric',
    'document_text_to_html_ratio' = 'numeric',
    'document_html_meta_title_length' = 'numeric',
    'document_html_meta_description_length' = 'numeric',
    'document_html_meta_keyword_length' = 'numeric',
    'document_html_meta_description_length' = 'numeric',
    'document_html_meta_keyword_length' = 'numeric',
    'document_html_tag_h1' = 'numeric',
    'document_html_tag_h2' = 'numeric',
    'document_html_tag_h3' = 'numeric',
    'document_html_tag_h4' = 'numeric',
    'document_html_tag_h5' = 'numeric',
    'document_html_tag_meta' = 'numeric',
    'document_html_tag_section' = 'numeric',
    'document_html_tag_nav' = 'numeric',
    'document_html_tag_form' = 'numeric',
    'document_html_tag_br' = 'numeric',
    'document_html_tag_p' = 'numeric',
    'document_html_tag_img' = 'numeric',
    'document_html_tag_script' = 'numeric',
    'document_html_tag_link' = 'numeric',
    'document_html_tag_table' = 'numeric',
    'document_links_attr_follow_count' = 'numeric',
    'document_links_attr_nofollow_count' = 'numeric',
    'document_links_count' = 'numeric',
    'document_website_links_attr_follow_count' = 'numeric',
    'document_website_links_attr_nofollow_count' = 'numeric',
    'document_link_distribution_range_1' = 'numeric',
    'document_link_distribution_range_2' = 'numeric',
    'document_link_distribution_range_3' = 'numeric',
    'document_link_distribution_range_4' = 'numeric',
    'document_link_distribution_range_5' = 'numeric',
    'document_website_link_distribution_range_1' = 'numeric',
    'document_website_link_distribution_range_2' = 'numeric',
    'document_website_link_distribution_range_3' = 'numeric',
    'document_website_link_distribution_range_4' = 'numeric',
    'document_website_link_distribution_range_5' = 'numeric',
    'document_external_link_distribution_range_1' = 'numeric',
    'document_external_link_distribution_range_2' = 'numeric',
    'document_external_link_distribution_range_3' = 'numeric',
    'document_external_link_distribution_range_4' = 'numeric',
    'document_external_link_distribution_range_5' = 'numeric',
    'document_performance_total_time' = 'numeric',
    'document_performance_speed_download' = 'numeric',
    'document_security_google_safe_browsing' = 'numeric'
)

#' Fields supported by model.
#'
#' @name Supported metrics
SUPPORTED_METRICS <- c(
    'class',
    'http_code',
    'domain_moz_domain_authority',
    'domain_moz_mozrank',
    'document_html_length',
    'document_text_length',
    'document_text_to_html_ratio',
    'document_html_meta_title_length',
    'document_html_meta_description_length',
    'document_html_meta_keyword_length',
    'document_html_meta_description_length',
    'document_html_meta_keyword_length',
#     'document_html_tag_meta',
#     'document_html_tag_section',
#     'document_html_tag_nav',
#     'document_html_tag_form',
#     'document_html_tag_br',
#     'document_html_tag_p',
#     'document_html_tag_img',
#     'document_html_tag_script',
#     'document_html_tag_link',
#     'document_html_tag_table',
    'document_links_attr_follow_count',
    'document_links_attr_nofollow_count',
    'document_links_count',
#     'document_website_links_attr_follow_count',
#     'document_website_links_attr_nofollow_count',
    'document_link_distribution_range_1',
    'document_link_distribution_range_2',
    'document_link_distribution_range_3',
    'document_link_distribution_range_4',
    'document_link_distribution_range_5',
    'document_external_link_distribution_range_1',
    'document_external_link_distribution_range_2',
    'document_external_link_distribution_range_3',
    'document_external_link_distribution_range_4',
    'document_external_link_distribution_range_5',
    'document_performance_total_time',
    'document_performance_speed_download',
    'document_security_google_safe_browsing'
)

#' Load raw data from file.
#' 
#' @param path Path to metrics file
#' @return Data frame with column classes
load_raw_data <- function(path) {
    # Read first row from file
    data <- read.csv(path, sep = '\t', nrows = 1)
    
    # Create vector of classes to further processing.
    # Proper class associated with given column should be found in vector of all metrics defintions.
    columnClasses <- sapply(colnames(data), function(name) { AVAILABLE_METRICS[[name]] })
    
    # Read entire data with valid column classes
    data <- read.csv(path, sep = '\t', colClasses = columnClasses, na.strings = 'NULL')
    
    # Sort columns
    data[, sort(names(data))]
}

#' Clean data frame from unused columns and NA's values.
#' 
#' @param data Data frame with metrics
#' @return Clean data frame
clean_raw_data <- function(data) {
    # Remove unsupported columns from input data.
    columnsToRemove <- setdiff(names(AVAILABLE_METRICS), SUPPORTED_METRICS)
    
    data <- data[, !names(data) %in% columnsToRemove]
    
    # Replace NAs with 0 in numeric columns
    numericColumns <- colnames(data[sapply(data, is.numeric)])
    
    # Normalization
    data[, 'document_links_attr_follow_count'] <- data[, 'document_links_attr_follow_count'] / data[, 'document_links_count']
    data[, 'document_links_attr_nofollow_count'] <- data[, 'document_links_attr_nofollow_count'] / data[, 'document_links_count']
    data[, 'document_link_distribution_range_1'] <- data[, 'document_link_distribution_range_1'] / data[, 'document_links_count']
    data[, 'document_link_distribution_range_2'] <- data[, 'document_link_distribution_range_2'] / data[, 'document_links_count']
    data[, 'document_link_distribution_range_3'] <- data[, 'document_link_distribution_range_3'] / data[, 'document_links_count']
    data[, 'document_link_distribution_range_4'] <- data[, 'document_link_distribution_range_4'] / data[, 'document_links_count']
    data[, 'document_link_distribution_range_5'] <- data[, 'document_link_distribution_range_5'] / data[, 'document_links_count']
    data[, 'document_external_link_distribution_range_1'] <- data[, 'document_external_link_distribution_range_1'] / data[, 'document_links_count']
    data[, 'document_external_link_distribution_range_2'] <- data[, 'document_external_link_distribution_range_2'] / data[, 'document_links_count']
    data[, 'document_external_link_distribution_range_3'] <- data[, 'document_external_link_distribution_range_3'] / data[, 'document_links_count']
    data[, 'document_external_link_distribution_range_4'] <- data[, 'document_external_link_distribution_range_4'] / data[, 'document_links_count']
    data[, 'document_external_link_distribution_range_5'] <- data[, 'document_external_link_distribution_range_5'] / data[, 'document_links_count']
    
    data[numericColumns][is.na(data[numericColumns])] <- 0
    
    # Set all possible levels for http_code column.
    # Based on list available on page http://www.iana.org/assignments/http-status-codes/http-status-codes.xhtml
    data[, 'http_code'] <- factor(data[, 'http_code'], c(
        "200", "201", "202", "203", "204", "205", "206", "207", "208",
        "300", "301", "302", "303", "304", "305", "307", "308",
        "400", "401", "402", "403", "404", "405", "406", "407", "408", "409",
        "410", "411", "412", "413", "414", "415", "416", "417",
        "422", "423", "424", "425", "426", "427", "428", "429",
        "500", "501", "502", "503", "504", "505", "506", "507", "508", "509"
    ))
    
    data
}

#' Load url classification data
#' 
#' @param path
#' @return Data frame with url classification
#' @export
load_classification <- function(path) {
    read.csv(
        path,
        sep = '\t',
        col.name = c('url', 'class'),
        colClasses = c('character', 'factor')
    )
}

#' Prepare labeled metrics from file.
#' 
#' @param path
#' @param path
#' @return Data frame with cleaned and labeled metrics
#' @export
prepare_metrics <- function(metricsPath, classificationPath) {
    raw <- load_raw_data(metricsPath)
    classification <- load_classification(classificationPath)
    
    merged <- merge(raw, classification, by = 'url')
    
    clean_raw_data(merged)
}