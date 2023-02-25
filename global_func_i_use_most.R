# a function that 
#convert categorical variables to factor
is_fact_func <- function(dat) {
  listdat <- list()
  for (i in seq_along(dat)) {
    if (is.character(dat[[i]])) {
      listdat[[i]] <- factor(dat[[i]])
    } else{
      listdat[[i]] <- dat[[i]]
    }
  }
  newdat <- data.frame(listdat)
  colnames(newdat) <- colnames(dat)
  return(as_tibble(newdat))
}

# a function that
# samples the data variables and come
# up with a new sampled data
sample_data_function <- function(dat) {
  lis_dat <- list()
  for (var in seq_along(dat)) {
    lis_dat[[var]] <- sample(x = dat[[var]],
                             size = length(dat[[var]]),
                             replace = TRUE)
  }
  new_dat <- data.frame(lis_dat)
  colnames(new_dat) <- colnames(dat)
  return(as_tibble(new_dat))
}