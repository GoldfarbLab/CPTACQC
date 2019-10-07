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

  #calculate overal label efficiency
  calculate.labelling.efficiency <- sum(filtered.data$detected_tags)/sum(filtered.data$expected_tags)


  #calculate K label efficiency
  lysine.labelling.efficiency <- sum(filtered.data$detected_lysine)/sum(filtered.data$expected_lysine)


  #calculate N term label efficiency
  nterm.labelling.efficiency <- sum(filtered.data$detected_nterm)/sum(filtered.data$expected_nterm)

  #create a tibble with these three values inside of it
  efficiency.calculations <- tibble("Labelling Efficiency" = calculate.labelling.efficiency, "Lysine Labelling Efficiency" = lysine.labelling.efficiency, "N term Labelling Efficiency" = nterm.labelling.efficiency)
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
