# PROCESSING  ----------------------------------------------------------------------------------------------------------


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
  tibble("Labelling Efficiency" = calculate.labelling.efficiency, "Lysine Labelling Efficiency" = lysine.labelling.efficiency, "N term Labelling Efficiency" = nterm.labelling.efficiency)

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
