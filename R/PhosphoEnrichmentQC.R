# PROCESSING  ----------------------------------------------------------------------------------------------------------


#
calc_phospho_enrichment_efficiency <- function(filtered.data)
  {
  total.tags <- sum(filtered.data$"Phospho (STY)")
  at.least.one.tag <- sum(filtered.data$"Phospho (STY)" >= 1)
  one.tag <- sum(filtered.data$"Phospho (STY)" == 1)
  two.tags <- sum(filtered.data$"Phospho (STY)" == 2)
  three.tags <- sum(filtered.data$"Phospho (STY)" == 3)
  four.or.more.tags <- sum(filtered.data$"Phospho (STY)" >= 4)
  no.sites <- sum(filtered.data$"Phospho (STY)" == 0)

  multiple.tags <- total.tags - at.least.one.tag

phospho.enrichment.calculations <- tibble("Total Phospho Sites" = total.tags,
                                          "At Least 1 Site" = at.least.one.tag,
                                          "1 Site" = one.tag,
                                          "2 Sites" = two.tags,
                                          "3 Sites" = three.tags,
                                          "4 Sites or More" = four.or.more.tags,
                                          "No Sites" = no.sites,
                                          "Tagged Multiple Sites" = multiple.tags)

#
phospho.enrichment.calculations
}

calc_phospho_localized <- function(filtered.data)
{
  total <- nrow(filtered.data)
  localized.data <- subset(filtered.data, filtered.data$"Localization prob" >= .75)
  serine <- sum(localized.data$"Amino acid" == "S")
  threonine <- sum(localized.data$"Amino acid" == "T")
  tyrosine <- sum(localized.data$"Amino acid" == "Y")
  totalabovepercent <- nrow(localized.data)

  localized_phosph_data <- tibble("Total" = total,
                                  "Total Phospho Sites Above 75% Localization" = totalabovepercent,
                                  "Serine" = serine,
                                  "% S" = (serine/totalabovepercent)*100,
                                  "Threonine" = threonine,
                                  "% T" = (threonine/totalabovepercent)*100,
                                  "Tyrosine" = tyrosine,
                                  "% Y" = (tyrosine/totalabovepercent)*100)

  fractions <- grep("Localization prob ", colnames(filtered.data), ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE, value = TRUE)

  localized_phosph_data$Fraction <- "All"

  for (i in fractions)
  {
    calc_stats_fraction <- subset(filtered.data, filtered.data[[i]] != "NA")
    fraction_data <- calc_phospho_localized(calc_stats_fraction)
    fraction_data$Fraction <- i
    fraction_stats <- rbind(localized_phosph_data, fraction_data)
  }

  fraction_stats
}


# VISUALIZATION  -------------------------------------------------------------------------------------------------------


#
plot_phospho_enrichment_efficiency <- function()
{

}
