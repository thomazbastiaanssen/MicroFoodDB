library(tidyverse)
# dat <- xml2::read_xml("raw/expasy/enzyme.rdf")
# read.delim("http://ftp.microbio.me/pub/wol2/function/metacyc/reaction-to-ec.map", header = F, quote = "") %>% write.csv("raw/WoL/reaction-to-ec.map")
# read.delim("http://ftp.microbio.me/pub/wol2/function/metacyc/reaction-to-left_compound.map",  header = F, quote = "") %>% write.csv("raw/WoL/reaction-to-left_compound.map")
# read.delim("http://ftp.microbio.me/pub/wol2/function/metacyc/reaction-to-right_compound.map", header = F, quote = "") %>% write.csv("raw/WoL/reaction-to-right_compound.map")
# read.delim("http://ftp.microbio.me/pub/wol2/function/metacyc/compound_name.txt",    header = F, quote = "") %>% write.csv("raw/WoL/compound_name.txt")

# ko2p <- KEGGREST::keggLink("ko", "pathway")
# data.frame(ko = ko2p, pathway = names(ko2p)) %>%
#   write.csv("raw/WoL/ko2pathway.csv")
# ko2b <- KEGGREST::keggLink("ko", "brite")
# data.frame(ko = ko2b, brite = names(ko2b)) %>%
#   write.csv("raw/WoL/ko2brite.csv")

# pathway_names <- KEGGREST::keggList("pathway")
# data.frame(pathway_names = pathway_names, pathway = paste0("path:", names(pathway_names)), row.names = NULL) %>%
#   write.csv("raw/WoL/pathway_names.csv")

# cpd_names <- KEGGREST::keggList("cpd")
# data.frame(cpd = names(cpd_names), cpd_names = cpd_names, row.names = NULL) %>%
#   write.csv("raw/WoL/cpd_names.csv")

# ko_names <- KEGGREST::keggList("ko")
# data.frame(ko = names(ko_names), ko_names = ko_names, row.names = NULL) %>%
#   write.csv("raw/WoL/ko_names.csv")


homopolysach <- c("glucan", "fructan", "galactan", "arabinan", "xylan")
hexoses <- c("D-glucose", "D-galactofuranose", "D-galactose", "L-galactose",
             "D-mannose", "D-allose", "L-altrose", "D-gulose", "L-idose", "D-talose")
pentoses <- c("D-ribose", "D-arabinose", "L-arabinose", "D-xylose", "D-lyxose")

pathway_names <- read.delim("raw/WoL/pathway_names.csv", sep = ",", row.names = 1)
cpd_names <- read.delim("raw/WoL/cpd_names.csv", sep = ",", row.names = 1)
ko_names  <- read.delim("raw/WoL/ko_names.csv", sep = ",", row.names = 1)

compound_name <- read.delim("raw/WoL/compound_name.txt", sep = ",", row.names = 1) %>%
  dplyr::rename("compound" = V1, "name" = V2)

reaction_ec     <- read.delim("raw/WoL/reaction-to-ec.map", sep = ",", row.names = 1) %>%
  rename("RXN" = V1, "EC" = V2) %>%
  mutate(EC = case_when(str_detect(RXN, "EC-") ~ RXN, .default = EC),
         RXN = case_when(str_detect(RXN, "EC-") ~ NA, .default = RXN),
         ec = str_remove(EC, "EC-")) %>%
  filter(grepl(x = ec, "^4.2.2.") | grepl(x = ec, "^3.2.1."))

#Bridges
reaction_left_compound   <-  read.delim("raw/WoL/reaction-to-left_compound.map", sep = ",", row.names = 1) %>%
  rename("RXN" = V1) %>%
  pivot_longer(!RXN) %>%
  filter(value != "") %>%
  dplyr::select(RXN, compound = value) %>%
  mutate(side = "left")
reaction_right_compound  <-  read.delim("raw/WoL/reaction-to-right_compound.map", sep = ",", row.names = 1) %>%
  rename("RXN" = V1) %>%
  pivot_longer(!RXN) %>%
  filter(value != "") %>%
  dplyr::select(RXN, compound = value) %>%
  mutate(side = "right")

ec2compounds <- left_join(
  reaction_ec,
  rbind(reaction_left_compound, reaction_right_compound),
  by = "RXN") %>%
  left_join(., compound_name, by = "compound")

dat <- read.delim("raw/expasy/enzyme.dat", skip = 24, header = FALSE) %>%
  separate_wider_delim(V1, delim = "   ",
                       names = c("name", "value"),
                       too_few = "align_start", too_many = "merge")

ko2pathway <- read.delim("raw/WoL/ko2pathway.csv", sep = ",", row.names = 1) %>%
  mutate(ko = str_remove(ko, "ko:"))
ko2brite <- read.delim("raw/WoL/ko2brite.csv", sep = ",", row.names = 1) %>%
  mutate(ko = str_remove(ko, "ko:"))


ec2kegg <- anansi::kegg_link()$ec2ko
ec2cpd <- anansi::kegg_link()$ec2cpd



super_cat <- c("cellulose", "hemicellulose", "pectin")
sub_cat <- c("a", "b")

plot_df <-
  dat %>%
  mutate(ec = case_when(name == "ID" ~ value)) %>%
  filter(name != "//") %>%
  fill(ec) %>%

  filter(!name %in% c("DR", "AN")) %>%

  filter(grepl(x = ec, "^4.2.2.") | grepl(x = ec, "^3.2.1."))  %>%

  group_by(ec) %>%

  reframe(
    name, ec,
    value = case_when(
      name == "CA" ~ paste0(value[name == "CA"], collapse = " "),
      name == "DE" ~ paste0(value[name == "DE"], collapse = " "),
      .default = value)
  ) %>%

  distinct() %>%
  ungroup() %>%

  filter(name %in% c("DE", "CA")) %>%
  # dplyr::reframe(n = dplyr::n(), .by = c(ec, name), value) |>
  # dplyr::filter(n > 1L)

  pivot_wider(names_from = name, values_from = value) %>%
  left_join(., ec2kegg, by = "ec") %>%
  left_join(., ko_names, by = "ko", relationship = "many-to-many") %>%
  left_join(., ec2compounds, by = "ec", relationship = "many-to-many") %>%

  # left_join(., ko2pathway, by = "ko", relationship = "many-to-many") %>%
  # left_join(., ko2brite, by = "ko", relationship = "many-to-many") %>%
  # left_join(., pathway_names, by = "pathway") %>%

  left_join(., ec2cpd, by = "ec", relationship = "many-to-many") %>%
  left_join(., cpd_names, by = "cpd")




 plot_ecs <-
plot_df %>%

  filter(str_detect(DE, "endoplasmic reticulum", negate = TRUE)) %>%
  filter(str_detect(DE, "lysozyme", negate = TRUE)) %>%
  filter(str_detect(DE, "Deleted", negate = TRUE)) %>%
  filter(str_detect(DE, "limit ", negate = TRUE)) %>%
  filter(str_detect(DE, "Transferred ", negate = TRUE)) %>%
  filter(str_detect(DE, "blood", negate = TRUE)) %>%
  filter(str_detect(DE, "amygdalin", negate = TRUE)) %>%
  filter(str_detect(DE, "UDP|GDP", negate = TRUE)) %>%



  unite("all_metabolite_text", c(name, DE, CA, cpd_names, ko_names), sep = "_", remove = FALSE) %>%

  mutate(
    glucose   = str_detect(all_metabolite_text, pattern = regex("gluc",  ignore_case = TRUE)),
    galactose = str_detect(all_metabolite_text, pattern = regex("galac", ignore_case = TRUE)),
    mannose   = str_detect(all_metabolite_text, pattern = regex("mann",  ignore_case = TRUE)),
    arabinose = str_detect(all_metabolite_text, pattern = regex("arab",  ignore_case = TRUE)),
    xylose    = str_detect(all_metabolite_text, pattern = regex("xyl",   ignore_case = TRUE)),
    fucose    = str_detect(all_metabolite_text, pattern = regex("fuc",   ignore_case = TRUE)),
    gulose    = str_detect(all_metabolite_text, pattern = regex("gul",   ignore_case = TRUE)),
    rhamnose  = str_detect(all_metabolite_text, pattern = regex("rham",  ignore_case = TRUE))
  ) %>%
  # mutate(ec = factor(ec, levels = ec %>% unique() %>% str_sort(numeric = TRUE) )) %>%


  # mutate(subtype = gsub(DE, pattern = " .*", replacement = "")) %>%
  # mutate(subtype = case_when(str_detect(DE, "heparin") ~ "heparin",
  #                             str_detect(DE, "chondroitin") ~ "chondroitin",
  #                             str_detect(DE, "alginate") ~ "alginate",
  #                             str_detect(DE, "xylanase") ~ "xylan",
  #                             str_detect(DE, "arabi") ~ "arabinan",
  #
  #
  #                             .default = subtype),
  #        supertype = subtype
  #        ) %>%
  #
  # mutate(supertype = case_when(str_detect(DE, "rhamnogalacturonan|pectin|pectate") ~ "pectin",
  #                              str_detect(DE, "inulin|levan")  ~ "inulin",
  #                              .default = supertype
  #                              )) %>%
  #
  dplyr::select(
    DE,
    glucose, galactose, rhamnose, mannose, gulose, arabinose, xylose, fucose
  ) %>%

  group_by(DE) %>%
  mutate(
    across(c(glucose, galactose, rhamnose, mannose, gulose, arabinose, xylose, fucose),
           function(x) {as.logical(max(x))})
  ) %>%
  ungroup() %>%
  distinct() %>%

  #filter(side == "left") %>%


  # filter(grepl(x = compound, "[1-9],[1-9]|[1-9]->[1-9]") |
  #          grepl(x = compound, "oligosacharide") |
  #          grepl(x = ec, "^4.2.2")
  #        )

  #  dplyr::select(DE, glucose, galactose, mannose, arabinose, xylose, fucose, rhamnose) %>%
  #filter(!is.na(glucose)) %>%

  filter(any(glucose, galactose, rhamnose, mannose, gulose, arabinose, xylose, fucose)) %>%

  mutate(subtype = case_when(str_detect(DE, "heparin") ~ "heparin",
                             str_detect(DE, "chondroitin") ~ "chondroitin",
                             str_detect(DE, "alginate") ~ "alginate",

                             str_detect(DE, regex("^xylan|-xylan")) ~ "xylan",
                             str_detect(DE, regex("^arabinan|-arabinan")) ~ "arabinan",
                             str_detect(DE, regex("^fructan|-fructan")) ~ "fructan",
                             str_detect(DE, regex("^glucan|-glucan")) ~ "glucan",
                             str_detect(DE, regex("^xanthan|-xanthan")) ~ "xanthan",
                             str_detect(DE, regex("^mannan|-mannan")) ~ "mannan",
                             str_detect(DE, regex("^funoran|-funoran")) ~ "funoran",

                             str_detect(DE, regex("^galactan|-galactan")) ~ "galactan",
                             str_detect(DE, regex("^gellan")) ~ "gellan",

                             str_detect(DE, "xylogluc") ~ "xyloglucan",
                             str_detect(DE, "arabinogal") ~ "arabinogalactan",



                             str_detect(DE, "inulin|levan") ~ "inulin",
                             str_detect(DE, "galacturonan|pectin|pectate") ~ "pectin",
                             str_detect(DE, "peptidoglycan") ~ "peptidoglycan",
                             str_detect(DE, "pullulan") ~ "pullulan",
                             str_detect(DE, "hemicel") ~ "hemicellulose",
                             str_detect(DE, "cellul") ~ "cellulose",
                             str_detect(DE, "chiti|chito") ~ "chitin",
                             str_detect(DE, "mannosyl-") ~ "mannosyl-oligosaccharides",
                             str_detect(DE, "ginse") ~ "ginsenosides",


                             .default = NA)
  ) %>%


#   filter(is.na(subtype)) %>%
#filter(subtype == "unsorted saccharides") %>%



  pivot_longer(!c(DE, subtype)) %>%

  mutate(name = factor(name, levels = c("glucose", "galactose", "rhamnose", "mannose", "gulose", "arabinose","xylose", "fucose"))) %>%


  #filter(name %in% c("gulose", "arabinose")) %>%

  ggplot() +

  aes(x = name, y = DE, fill = value) +

  geom_tile() +

  facet_grid(subtype~ name, scales = "free", switch = "y", space = "free_y") +

  scale_fill_manual(values = c("TRUE" = "dodgerblue", "FALSE" = "lightgray"), na.value = "lightgray" ) +
  scale_y_discrete(position = "right") +

  xlab(NULL) +
  ylab(NULL) +

  theme_bw() +
  theme(text = element_text(size = 12),
        strip.text.y.left = element_text(angle = 0, hjust = 0))


plot_ecs

