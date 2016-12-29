merge_results <- function(results_x, results_y) {
    results_x <- results_x[, c('url', 'class')]
    results_x[, 'class'] <- as.character(results_x[, 'class'])
    
    results_y <- results_y[, c('url', 'class')]
    results_y[, 'class'] <- as.character(results_y[, 'class'])
    
    merge(results_x, results_y, c('url'))
}