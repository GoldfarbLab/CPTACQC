# READ AND FORMAT DATA  ------------------------------------------------------------------------------------------------

## yooo heres the path: my.data <- read_maxquant("~/Box/CellBio-GoldfarbLab/Users/Ria Jasuja/evidence.txt", "TMT10-K", "TMT10-Nterm", c("Acetyl (Protein N-term)"))

# INPUT: path to MaxQuant's evidence.txt file and modification names
#
# OUTPUT: internal peptide-level formatted data
#
# Reads MaxQuant's "Evidence.txt" file and converts it into our internal QC format.
#
# Returns a tibble with columns for the number of possible and observed TMT labels.
#
# If TMT.N.mod and TMT.K.mod are NA, then assume that this was not searched with variable TMT mods.
# The values of the internal QC columns for those label shouldalso be NA.
#
# If only one of the TMT.N and TMT.K columns are NA, then throw a warning.
# The values of the internal QC column for the NA label should also be NA.
#
# Removes Reverses and Potential contaminants
#
read_maxquant <- function(path,
                           TMT_K_mod = "TMT10 (K)",
                           TMT_N_mod = "TMT10 (N-term)",
                           N_term_blocking_mods = c("Acetyl (Protein N-term)"),
                           K_blocking_mods = c(),
                           phospho_mod = "Phospho (STY)")
{
  data <- read_tsv(path)

  #organize table a bit to make it easier to work with

  filtered.data <- select(data, "Sequence","Length", "Modifications", "Missed cleavages", TMT_N_mod, TMT_K_mod, N_term_blocking_mods,)
  filtered.data$Total_Nterm_Mods <- rowSums(select(data, N_term_blocking_mods))
  filtered.data <- filtered.data %>% rename("TMT10-K" = `TMT_K_mod`, "TMT10-Nterm" = `TMT_N_mod`, "N-term Modifications" = `Total_Nterm_Mods`)

  #compute expected tags (amount of K and N-term) and observed tags (the number of hits in the TMT columns) for each row and add them to their own columns

  filtered.data$expected_lysine <- str_count(filtered.data$Sequence, "K")
  filtered.data$detected_lysine <- filtered.data$"TMT10-K"

  filtered.data$expected_nterm <- str_count(filtered.data$"N-term Modifications", "0")
  filtered.data$detected_nterm <- filtered.data$"TMT10-Nterm"

  # calculate total expected.tags and detected.tags for each column either this way or by adding the values of the columns calculated above

  filtered.data$expected_tags <- str_count(filtered.data$Sequence, "K") + str_count(filtered.data$"N-term Modifications", "0")
  filtered.data$detected_tags <- filtered.data$"TMT10-K" + filtered.data$"TMT10-Nterm"

  #create a column that quantifies the efficiency of the labels
  #if the expected = detected, it is fully labelled,
  #if expected > detected, its partially labelled
  #if expected = 0, its completely unlabeled

  filtered.data$labelling_efficiency <- filtered.data$expected_tags - filtered.data$detected_tags

  filtered.data$labelling_efficiency[filtered.data$expected_tags - filtered.data$detected_tags == 0 & filtered.data$expected_tags > 0] <- "Fully Labelled"
  filtered.data$labelling_efficiency[filtered.data$expected_tags - filtered.data$detected_tags > 0] <- "Partially Labelled"
  filtered.data$labelling_efficiency[filtered.data$expected_tags > 0 & filtered.data$detected_tags == 0] <- "Unlabelled"
  filtered.data$labelling_efficiency[filtered.data$expected_tags == 0] <- "No Sites Available"

  #if there are more detected than expected, throw a warning to user

  for (row in 1:nrow(filtered.data))
  {
    #put a warning for "overlabeling" - not sure about this
    if(filtered.data[row, "detected_tags"] - filtered.data[row, "expected_tags"] > 0)
    {
      warning('This row has more detected tags than expected tags (overlabeled)')
    }

  }
  return(filtered.data)
}

#
format_spectrum_mill <- function(path,
                                 TMT_N_mod,
                                 TMT_K_mod,
                                 N_term_blocking_mods,
                                 K_blocking_mods,
                                 phospho_mod)
{

}

#
format_mz_tab <- function(path,
                          TMT_N_mod,
                          TMT_K_mod,
                          N_term_blocking_mods,
                          K_blocking_mods,
                          phospho_mod)
{

}

#
read_qc <- function(path)
{

}

