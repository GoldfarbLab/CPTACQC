# READ AND FORMAT DATA  ------------------------------------------------------------------------------------------------

## yooo heres the path: my.data <- read_maxquant("~/Box/CellBio-GoldfarbLab/Users/Ria Jasuja/evidence.txt", "TMT-K", "TMT10-Nterm", c("Acetyl (Protein N-Term)"))

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
read_maxquant <- function(path,
                           TMT_N_mod = "TMT10 (N-term)",
                           TMT_K_mod = "TMT10 (K)",
                           N_term_blocking_mods = c("Acetyl (Protein N-term)"),
                           K_blocking_mods = c(),
                           phospho_mod = "Phospho (STY)")
{
  print(N_term_blocking_mods)
  data <- read_tsv(path)
  # colnames(data)$`TMT.nterm`
  #as.data.frame(N_term_blocking_mods)
  Total_Nterm_mods <- rowSums(select(data, N_term_blocking_mods))

  filtered.data <- select(data, "Sequence","Length", "Modifications", "Missed cleavages", TMT_K_mod, TMT_N_mod, N_term_blocking_mods,)
  filtered.data$Total_Nterm_Mods <- Total_Nterm_mods
  filtered.data <- filtered.data %>% rename("TMT10-K" = `TMT_K_mod`, "TMT10-Nterm" = `TMT_N_mod`, "N-term Modifications" = `Total_Nterm_Mods`)
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

