#create a function that can preform the calculations on an experiment such as "Experiment 02"
# input : calc_stats(my.data.5, "Experiment 02") which is the dataset and the column name(s)

calc_stats <- function(filtered.data)

{
  experiments <- grep("Experiment ", colnames(filtered.data), ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE, value = TRUE)
  print(experiments)

  exp_stats <- calc_labeling_efficiency(filtered.data)
  print(exp_stats)

  for (i in experiments)
    {
      calc_stats_exp <- subset(filtered.data, filtered.data[[i]] > 0)
      efficiency_for_exp <- calc_labeling_efficiency(calc_stats_exp)
      bind_rows
      #cols_in_effciency <- colnames(exp_stats)
     # add_row(exp_stats, colnames(calc_labeling_efficiency(calc_stats_exp)))
    }

}
