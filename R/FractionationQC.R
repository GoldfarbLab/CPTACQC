# PROCESSING  ----------------------------------------------------------------------------------------------------------


#find the total number and the unique number of peptides in each fraction (experiments where that one has a number, but all the others are NA)
#data <- calc_fractionation_efficiency("~/Box/CellBio-GoldfarbLab/Users/Ria Jasuja/modificationSpecificPeptides.txt")
calc_fractionation_efficiency <- function(path)
{
  data <- read_tsv(path)
  experiments <- grep("Experiment ", colnames(data), ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE, value = TRUE)
  filtered.data <- select(data, "Sequence","Modifications", experiments)

  fractionation_efficiency <- tibble("Experiment" = character(), "Total" = character(), "Unique" = character())

  for (i in experiments)
  {
    present_in_exp <- subset(filtered.data, filtered.data[[i]] > 0)
    total.present <- nrow(present_in_exp)
    unique.tests <- rowSums(is.na(present_in_exp))
    print(unique.tests)
    unique.present <- unique.tests == length(experiments)-1
    total.unique <- sum(unique.present)

    fractionation_efficiency <- add_row(fractionation_efficiency, Experiment = i, Total = total.present, Unique = total.unique)
  }
  fractionation_efficiency
}


# VISUALIZATION  -------------------------------------------------------------------------------------------------------


#
plot_fractionation_efficiency <- function()
{

}
