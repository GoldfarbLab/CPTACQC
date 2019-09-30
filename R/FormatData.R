# READ AND FORMAT DATA  ------------------------------------------------------------------------------------------------

## yooo heres the path: my.data <- read_maxquant("~/Box/CellBio-GoldfarbLab/Users/Ria Jasuja/evidence.txt", "TMT10-Nterm")

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

# This is how I would name the function and parameters
#read_maxquant <- function(path,
#                            TMT_N_mod = "TMT10 (N-term)",
#                            TMT_K_mod = "TMT10 (K)",
#                            N_term_blocking_mods = c("Acetyl (N-term)"),
#                            K_blocking_mods = c(),
#                            phospho_mod = "Phospho (STY)")

read_maxquant <- function(path, TMT.k, TMT.nterm, Nterm.mods)
{
  data <- read_tsv(path)
  # colnames(data)$`TMT.nterm`

  #nterm <- unite(Nterm.mods, col="total", sep = "")
  for (row in 1:nrow(Nterm.mods)) {
    Nterm.mods$total <- rowSums(Nterm.mods)
  }
 #  Nterm.mods$total <-
  filtered.data <- select(data, "Sequence","Length", "Modifications", Nterm.mods$total, TMT.k, TMT.nterm, "Missed cleavages")
  filtered.data <- filtered.data %>% rename("TMT10-K" = `TMT.k`, "TMT10-Nterm" = `TMT.nterm`, "N-term Modifications" = Nterm.mods$total)
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

