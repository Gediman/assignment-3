#Generate many simulated (predicted) data sets
#from two hypotheses. Under the first, there is no difference in the means of the distributions between the two
#groups.
generate_data_sets <- function(lang_good, lang_bad, different_means, replacing){
  good <- dplyr::summarise(lang_good, total=n(), mean=mean(rating), sd=sd(rating)) 
  bad  <- dplyr::summarise(lang_bad, total=n(), mean=mean(rating), sd=sd(rating))
  coefs <- rep(0, 9999) 
  for (i in 1:9999){
    
    if (different_means){    
      mean_good <- sample(0:100, 1, replace=TRUE)  
      mean_bad  <- mean_good + (good$mean - bad$mean)
      good_list <- as.integer(rnorm(good$total, 
                                    mean=mean_good, 
                                    sd=good$sd))
      bad_list  <- as.integer(rnorm(bad$total, 
                                    mean=mean_bad, 
                                    sd=bad$sd))
    }else{
      x <- sample(1:2, 1, replace=T)
      means     <- sample(0:100, 1, replace=TRUE)
      good_list <- as.integer(rnorm(good$total, 
                                    mean=means, 
                                    sd=good$sd))
      bad_list  <- as.integer(rnorm(bad$total, 
                                    mean=means, 
                                    sd=bad$sd))
    }
    if (replacing){
      good_list <- replace(good_list, good_list < 0, 0)
      good_list <- replace(good_list, good_list > 100, 100)
      bad_list  <- replace(bad_list, bad_list < 0, 0)
      bad_list  <- replace(bad_list, bad_list > 100, 100)
    }
    
    dataframe <- tibble::tibble(language = c(rep("adger-good", 
                                                 good$total), 
                                             rep("adger-bad",
                                                 bad$total)),
                                rating = c(good_list, bad_list))
    
    coefs[i] <- coef(lm(rating ~ language, data = dataframe))[2]
  }
  coefs <- data.frame(coef=coefs)
  return(coefs)
}

###NOT USED
generate_data_sets_not_used <- function(lang_good, lang_bad, different_means, replacing){
  good <- dplyr::summarise(lang_good, total=n(), mean=mean(rating), sd=sd(rating)) 
  bad  <- dplyr::summarise(lang_bad, total=n(), mean=mean(rating), sd=sd(rating))
  coefs <- rep(0, 9999) 
  for (i in 1:9999){
    
    if (different_means){
      mean_good <- sample(lang_good$rating, 1, replace=TRUE)  
      mean_bad <- sample(lang_bad$rating, 1, replace=TRUE) 
      #mean_bad  <- mean_good + (good$mean - bad$mean)
      good_list <- as.integer(rnorm(good$total, 
                                    mean=mean_good, 
                                    sd=good$sd))
      bad_list  <- as.integer(rnorm(bad$total, 
                                    mean=mean_bad, 
                                    sd=bad$sd))
    }else{
      x <- sample(1:2, 1, replace=T)
      if (x ==1){
        means     <- sample(lang_good$rating, 1, replace=TRUE)
      } else {
        means     <- sample(lang_bad$rating, 1, replace=TRUE)
      }
      good_list <- as.integer(rnorm(good$total, 
                                   mean=means, 
                                   sd=good$sd))
      bad_list  <- as.integer(rnorm(bad$total, 
                                      mean=means, 
                                      sd=bad$sd))
    }
    if (replacing){
      good_list <- replace(good_list, good_list < 0, 0)
      good_list <- replace(good_list, good_list > 100, 100)
      bad_list  <- replace(bad_list, bad_list < 0, 0)
      bad_list  <- replace(bad_list, bad_list > 100, 100)
    }
    
    dataframe <- tibble::tibble(language = c(rep("adger-good", 
                                                 good$total), 
                                             rep("adger-bad",
                                                 bad$total)),
                                rating = c(good_list, bad_list))
    
    coefs[i] <- coef(lm(rating ~ language, data = dataframe))[2]
  }
  coefs <- data.frame(coef=coefs)
  return(coefs)
}
