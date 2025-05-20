library(tidyverse)

# Schema
# food_name <- food_ID -> Compound_id -> compound_name;
#                                     -> InChi
#                                     -> ChEBI
#                                     -> KEGG cpd

foodb.content <- read.delim("raw/foodb_2020_4_7_csv/Content.csv", sep = ",")

# Link compounds to foods
FDB2FOOD <- foodb.content %>%
    filter(source_type == "Compound") %>%
    dplyr::select("FDB" = "source_id", "FOOD" = "food_id") %>%
    mutate(
        FOOD = paste0("FOOD", formatC(FOOD, width = 5, flag = "0")),
        FDB = paste0("FDB", formatC(FDB, width = 6, flag = "0"))
    )


# Name the compounds
FDB2name <- read.delim(
    "raw/foodb_2020_4_7_csv/Compound.csv",
    sep = ",",
    row.names = 1
) %>%
    dplyr::select(
        "FDB" = "public_id",
        "name",
        "CAS" = "description",
        "InChIKey" = "moldb_smiles",
        "InChI" = "moldb_inchikey"
    )

FDB2external <- read.delim(
    "raw/foodb_2020_4_7_csv/CompoundExternalDescriptor.csv",
    sep = ","
) %>%
    dplyr::select("FDB" = compound_id, external_id) %>%
    mutate(
        FDB = paste0("FDB", formatC(FDB, width = 6, flag = "0")),
        source_db = case_when(
            grepl(x = external_id, "^CHEBI:[0-9]{+}$") ~ "ChEBI",
            grepl(x = external_id, "^LM[A-Z]{+}[0-9]{+}$") ~ "lmsd",
            grepl(x = external_id, "^C[0-9]{5}$") ~ "KEGG",
            .default = "MetaCyc"
        )
    )

FDB2cpd <- FDB2external %>%
    filter(source_db == "KEGG") %>%
    select("FDB", "cpd" = "external_id")

FDB2cpd$cpd %>% unique() %>% length()
anansi::kegg_link()$ec2cpd$cpd %>% unique %>% length

FDB2ChEBI <- FDB2external %>%
    filter(source_db == "ChEBI") %>%
    select("FDB", "ChEBI" = "external_id")

FDB2name$InChI %>% unique() %>% length()

#
# foodb_df %>%
#
#     ggplot() +
#     aes(x = Food.Cat, y = value, fill = Food.Cat) +
#
#     geom_boxplot(aes(group = Food.Cat)) +
#     geom_point(shape = 21) +
#
#     xlab(NULL) +
#     ylab("mg/100g") +
#
#     facet_wrap(~name) +
#     theme_bw() +
#     theme(axis.text.x = element_blank())
