# PROCESSING  ----------------------------------------------------------------------------------------------------------


#
calc_labeling_efficiency <- function(filtered.data)
{
  #compute expected tags (amount of K and N-term) and observed tags (the number of hits in the TMT columns)

  expected.tags <- 0
  detected.tags <- 0
  expected.lysine <- 0
  detected.lysine <- 0
  expected.nterm <- 0
  detected.nterm <- 0

  for (row in 1:nrow(filtered.data))
    #for (row in 1:1)
  {
    expected.tags <- expected.tags + str_count(filtered.data[row, "Sequence"], "K") + str_count(filtered.data[row, nterm], "0")
    detected.tags <- detected.tags + filtered.data[row, "TMT10-K"] + filtered.data[row, "TMT10-Nterm"]

    expected.lysine <- expected.lysine + str_count(filtered.data[row, "Sequence"], "K")
    detected.lysine <- detected.lysine + filtered.data[row, "TMT10-K"]

    expected.nterm <- expected.nterm + str_count(filtered.data[row, nterm], "0")
    detected.nterm <- detected.nterm + filtered.data[row, "TMT10-Nterm"]

  }

  # expected.tags
  # detected.tags

  filtered.data$expected_tags <- str_count(filtered.data$Sequence, "K") + str_count(filtered.data$nterm, "0")
  filtered.data$detected_tags <- filtered.data$"TMT10-K" + filtered.data$"TMT10-Nterm"

  filtered.data$labelling_efficiency <- filtered.data$expected_tags - filtered.data$detected_tags

  filtered.data$labelling_efficiency[filtered.data$expected_tags - filtered.data$detected_tags == 0 & filtered.data$expected_tags > 0] <- "Fully Labelled"
  filtered.data$labelling_efficiency[filtered.data$expected_tags - filtered.data$detected_tags > 0] <- "Partially Labelled"
  filtered.data$labelling_efficiency[filtered.data$expected_tags > 0 & filtered.data$detected_tags == 0] <- "Unlabelled"
  filtered.data$labelling_efficiency[filtered.data$expected_tags == 0] <- "No Sites Available"

  for (row in 1:nrow(filtered.data))
  {
    #put a warning for "overlabeling" - not sure about this
    if(filtered.data[row, "detected_tags"] - filtered.data[row, "expected_tags"] > 0)
    {
      warning('This row has more detected tags than expected tags (overlabeled)')
    }
  }

  #calculate overal label efficiency
  calculate.labelling.efficiency <- detected.tags/expected.tags
  print(as.numeric(calculate.labelling.efficiency))

  #calculate K label efficiency
  lysine.labelling.efficiency <- detected.lysine/expected.lysine
  print(lysine.labelling.efficiency)

  #calculate N term label efficiency
  nterm.labelling.efficiency <- detected.nterm/expected.nterm
  print(nterm.labelling.efficiency)

  filtered.data
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
