# PROCESSING  ----------------------------------------------------------------------------------------------------------


# INPUT: internal peptide-level formatted data
#
# OUTPUT: tibble with the following columns:
#           experiment,
#           overall_labeling_efficiency, lys_labeling_efficiency, nterm_labeling_efficiency,
#           num_fully_labeled, num_partially_labeled, num_unlabeled, num_no_sites_available, num_overlabeled,
#           percent_fully_labeled, percent_partially_labeled, percent_unlabeled, percent_no_sites_available, percent_overlabeled,
#           num_lys_labeled, num_lys_unlabeled, num_lys_blocked,
#           num_nterm_labeled, num_nterm_unlabeled, num_nterm_blocked
#
#         and the following rows:
#           The first row should be the values for all experiments/fractions combined.
#           After that, there should be a row for each experiment/fraction.
#
calc_labeling_efficiency <- function(filtered.data)
{

  #calculate overall label efficiency
  calculate.labeling.efficiency <- sum(filtered.data$detected_tags)/sum(filtered.data$expected_tags)


  #calculate K label efficiency
  lysine.labeling.efficiency <- sum(filtered.data$detected_lysine)/sum(filtered.data$expected_lysine)


  #calculate N term label efficiency
  nterm.labeling.efficiency <- sum(filtered.data$detected_nterm)/sum(filtered.data$expected_nterm)


  # num_fully_labeled, num_partially_labeled, num_unlabeled, num_no_sites_available, num_overlabeled,
  num.fully.labeled <- sum(filtered.data$labeling_efficiency == "Fully Labeled")
  num.partially.labeled <- sum(filtered.data$labeling_efficiency == "Partially Labeled")
  num.unlabeled <- sum(filtered.data$labeling_efficiency == "Unlabeled")
  num.no.sites.available <- sum(filtered.data$labeling_efficiency == "No Sites Available")
  num.overlabeled <- sum(filtered.data$labeling_efficiency == "Overlabeled")

  #percent_fully_labeled, percent_partially_labeled, percent_unlabeled, percent_no_sites_available, percent_overlabeled,
  per.fully.labeled <- (num.fully.labeled/nrow(filtered.data))*100
  per.partially.labeled <- (num.partially.labeled/nrow(filtered.data))*100
  per.unlabeled <- (num.unlabeled/nrow(filtered.data))*100
  per.no.sites.available <- (num.no.sites.available/nrow(filtered.data))*100
  per.overlabeled <- (num.overlabeled/nrow(filtered.data))*100

  #num_lys_labeled, num_lys_unlabeled, num_lys_blocked, num_nterm_labeled, num_nterm_unlabeled, num_nterm_blocked
  num.lys.labeled <- sum(filtered.data$detected_lysine)
  num.lys.unlabeled <- sum(filtered.data$expected_lysine) - sum(filtered.data$detected_lysine)

  num.nterm.labeled <- sum(filtered.data$detected_nterm)
  num.nterm.unlabeled <- sum(filtered.data$expected_nterm) - sum(filtered.data$detected_nterm)

  #create a tibble with these three values inside of it
  efficiency.calculations <- tibble("Labeling Efficiency" = calculate.labeling.efficiency,
                                    "Lysine Labeling Efficiency" = lysine.labeling.efficiency,
                                    "N term Labeling Efficiency" = nterm.labeling.efficiency,
                                    "Number Fully Labeled" = num.fully.labeled,
                                    "Number Partially Labeled" = num.partially.labeled,
                                    "Number Unlabeled" = num.unlabeled,
                                    "Number No Sites Available" = num.no.sites.available,
                                    "Number Overlabeled" = num.overlabeled,
                                    "Percent Fully Labeled" = per.fully.labeled,
                                    "Percent Partially Labeled" = per.partially.labeled,
                                    "Percent Unlabeled" = per.unlabeled,
                                    "Percent No Sites Available" = per.no.sites.available,
                                    "Percent Overlabeled" = per.overlabeled)

  efficiency.calculations
}


#
calc_mixing_correction <- function()
{

}

# VISUALIZATION  -------------------------------------------------------------------------------------------------------


#
plot_labeling_efficiency <- function()
{

}
