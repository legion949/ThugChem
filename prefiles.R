

# Load all PeriodicTables
PeriodicTable <- LoadPeriodicTable()

# Load all Nomenclature
Nomenclature <- LoadNomenclature()

# Chemestry Family (Initial in english langueage)
my_family_chem <- c("Oxide", "Hydroxide", "Oxacid",
                    "Hydride", "Hydracid", "Oxosalt",
                    "Salt")

# Information for reactive() in server
my_atomic_numbers <- PeriodicTable[["en"]][,1]
my_symbols <- PeriodicTable[["en"]][,2]
my_names <- PeriodicTable[["en"]][,3]
my_valence <- strsplit(PeriodicTable[["en"]][,10], ";")
combinated_options <- my_atomic_numbers
names(combinated_options) <- paste0(my_names, " - ", my_symbols) 