
# options(encoding = "UTF-8")


# 2) Periodic Tables by language
PeriodicTable <- list()


# # English...
suppressWarnings(PeriodicTable[[1]] <- read.csv(file = "data/PeriodicTables/PeriodicTable_en.csv", sep=",", dec=".", header = T))
names(PeriodicTable)[1] <- "en"
      

# # Spanish...
suppressWarnings(PeriodicTable[[2]] <- read.csv(file = "data/PeriodicTables/PeriodicTable_es.csv", sep=",", dec=".", header = T))
names(PeriodicTable)[2] <- "es"


# # French...
suppressWarnings(PeriodicTable[[3]] <- read.csv("data/PeriodicTables/PeriodicTable_fr.csv", sep=",", dec=".", header = T))
names(PeriodicTable)[3] <- "fr"
################################################################################






my_family_chem <- c("Oxide", "Hydroxide", "Oxacid",
                    "Hydride", "Hydracid", "Oxosalt",
                    "Salt")


my_atomic_numbers <- PeriodicTable[["en"]][,1]
my_symbols <- PeriodicTable[["en"]][,2]
my_names <- PeriodicTable[["en"]][,3]
my_valence <- strsplit(PeriodicTable[["en"]][,10], ";")

combinated_options <- my_atomic_numbers
names(combinated_options) <- paste0(my_names, " - ", my_symbols) 