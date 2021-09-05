

# Load all PeriodicTables
PeriodicTable <- LoadPeriodicTable()

# Load all Nomenclature
Nomenclature <- LoadNomenclature()

InteligentSelection <- LoadInteligentSelection()

PageFamilyOptions <- LoadPageFamilyOptions()

PageHelperLevel <- LoadPageHelperLevel()


all_app_language <- c("en", "es", "fr")
Elements_Info <- NewCombination(all_app_language = all_app_language)


# My step
my_step_options <- c(1, 2, 3)
names(my_step_options) <- c("Beginning", "Final Equation", "Nomenclature")

# Information for reactive() in server
my_valence <- strsplit(PeriodicTable[["en"]][,10], ";")
