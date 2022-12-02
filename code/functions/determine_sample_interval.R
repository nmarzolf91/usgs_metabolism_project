determine_sample_interval <- function(d, dt_col){

    #d: data.frame. all columns other than dt_col are assumed to contain numeric data
    #dt_col: character. the name of the datetime column

    #calculates the mode interval of dt_col in minutes

    time_diffs <- diff(d[[dt_col]])
    units(time_diffs) = 'mins'
    interval_mode <- Mode(time_diffs)

    return(interval_mode)
}

