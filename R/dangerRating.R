# Compute the Danger Rating of a set of crimes given in the data frame, DATA

dangerRating = function(data) {
  crimes <- data %>%
    group_by(severity) %>%
    summarise(count = n())
  crimes$severity <- as.numeric(crimes$severity)
  rating <- sum(crimes[,1] * crimes[,2]) / (nrow(data) * 5)
  return(rating)
}