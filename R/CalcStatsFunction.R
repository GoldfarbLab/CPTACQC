#create a function that can preform the calculations on an experiment such as "Experiment 02"
#then figure out how to add the efficiency stats to a table
# input : calc_stats(my.data.5, "Experiment 02") which is the dataset and the column name(s)

calc_stats <- function(filtered.data)

{
  experiments <- grep("Experiment ", colnames(filtered.data), ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE, value = TRUE)

  exp_stats <- calc_labeling_efficiency(filtered.data)
  exp_stats$Experiment <- "All"

  for (i in experiments)
    {
      calc_stats_exp <- subset(filtered.data, filtered.data[[i]] > 0)
      efficiency_for_exp <- calc_labeling_efficiency(calc_stats_exp)
      efficiency_for_exp$Experiment <- i
      exp_stats <- rbind(exp_stats, efficiency_for_exp)

  }
  exp_stats[, c(ncol(exp_stats),(seq(2, ncol(exp_stats)-1)))]
}

