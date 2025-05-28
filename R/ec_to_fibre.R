library(tidyverse)
# dat <- xml2::read_xml("raw/expasy/enzyme.rdf")
# read.delim("http://ftp.microbio.me/pub/wol2/function/metacyc/reaction-to-ec.map", header = F, quote = "") %>% write.csv("raw/WoL/reaction-to-ec.map")
# read.delim("http://ftp.microbio.me/pub/wol2/function/metacyc/reaction-to-left_compound.map",  header = F, quote = "") %>% write.csv("raw/WoL/reaction-to-left_compound.map")
# read.delim("http://ftp.microbio.me/pub/wol2/function/metacyc/reaction-to-right_compound.map", header = F, quote = "") %>% write.csv("raw/WoL/reaction-to-right_compound.map")
# read.delim("http://ftp.microbio.me/pub/wol2/function/metacyc/compound_name.txt",    header = F, quote = "") %>% write.csv("raw/WoL/compound_name.txt")

# ko2p <- KEGGREST::keggLink("ko", "pathway") ; data.frame(ko = ko2p, pathway = names(ko2p)) %>% write.csv("raw/WoL/ko2pathway.csv")
# ko2b <- KEGGREST::keggLink("ko", "brite") ; data.frame(ko = ko2b, brite = names(ko2b))%>% write.csv("raw/WoL/ko2brite.csv")

# pathway_names <- KEGGREST::keggList("pathway")
# pathway_names <- data.frame(pathway_names = pathway_names, pathway = paste0("path:", names(pathway_names)), row.names = NULL) %>% write.csv("raw/WoL/pathway_names.csv")

# cpd_names <- KEGGREST::keggList("cpd")
# cpd_names <- data.frame(cpd = names(cpd_names), cpd_names = cpd_names, row.names = NULL) %>% write.csv("raw/WoL/cpd_names.csv")

homopolysach <- c("glucan", "fructan", "galactan", "arabinan", "xylan")
hexoses <- c("D-glucose", "D-galactofuranose", "D-galactose", "L-galactose",
            "D-mannose", "D-allose", "L-altrose", "D-gulose", "L-idose", "D-talose")
pentoses <- c("D-ribose", "D-arabinose", "L-arabinose", "D-xylose", "D-lyxose")

pathway_names <- read.delim("raw/WoL/pathway_names.csv", sep = ",", row.names = 1)
cpd_names <- read.delim("raw/WoL/cpd_names.csv", sep = ",", row.names = 1)


compound_name <- read.delim("raw/WoL/compound_name.txt", sep = ",", row.names = 1) %>%
  rename(V1 = "name", V2 = "compound")

reaction_ec     <- read.delim("raw/WoL/reaction-to-ec.map", sep = ",", row.names = 1) %>%
  rename(V1 = "RXN", V2 = "EC") %>%
  mutate(EC = case_when(str_detect(RXN, "EC-") ~ RXN, .default = EC),
         RXN = case_when(str_detect(RXN, "EC-") ~ NA, .default = RXN),
         ec = str_remove(EC, "EC-")) %>%
  filter(grepl(x = ec, "^4.2.2.") | grepl(x = ec, "^3.2.1."))

#Bridges
reaction_left_compound   <-  read.delim("raw/WoL/reaction-to-left_compound.map", sep = ",", row.names = 1) %>%
  rename(V1 = "RXN") %>%
  pivot_longer(!RXN) %>%
  filter(value != "") %>%
  dplyr::select(RXN, compound = value) %>%
  mutate(side = "left")
reaction_right_compound  <-  read.delim("raw/WoL/reaction-to-right_compound.map", sep = ",", row.names = 1) %>%
  rename(V1 = "RXN") %>%
  pivot_longer(!RXN) %>%
  filter(value != "") %>%
  dplyr::select(RXN, compound = value) %>%
  mutate(side = "right")

ec2compounds <- left_join(
  reaction_ec,
  rbind(reaction_left_compound, reaction_right_compound),
  by = "RXN") %>%
  left_join(., compound_name, by = "compound")

dat <- read.delim("raw/expasy/enzyme.dat", skip = 24, header = FALSE)

ko2pathway <- read.delim("raw/WoL/ko2pathway.csv", sep = ",", row.names = 1) %>%
  mutate(ko = str_remove(ko, "ko:"))
ko2brite <- read.delim("raw/WoL/ko2brite.csv", sep = ",", row.names = 1) %>%
  mutate(ko = str_remove(ko, "ko:"))


ec2kegg <- anansi::kegg_link()$ec2ko
ec2cpd <- anansi::kegg_link()$ec2cpd

dat <- separate_wider_delim(dat, V1, delim = "   ",
                            names = c("name", "value"),
                            too_few = "align_start", too_many = "merge")

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
  left_join(., ec2compounds, by = "ec", relationship = "many-to-many") %>%
  left_join(., ko2pathway, by = "ko", relationship = "many-to-many") %>%
  left_join(., ko2brite, by = "ko", relationship = "many-to-many") %>%
  left_join(., ec2cpd, by = "ec", relationship = "many-to-many") %>%
  left_join(., cpd_names, by = "cpd") %>%
  # left_join(., pathway_names, by = "pathway") %>%


  select(!CA)


plot_ecs <- plot_df %>%


  mutate(glucose    = str_detect(name, "gluc")  | str_detect(DE, "gluc")   | str_detect(cpd_names, "gluc"),
         galactose  = str_detect(name, "galact")| str_detect(DE, "galact") | str_detect(cpd_names, "galact"),
         mannose    = str_detect(name, "mann")  | str_detect(DE, "mann")   | str_detect(cpd_names, "mann"),
         arabinose  = str_detect(name, "arab")  | str_detect(DE, "arab")   | str_detect(cpd_names, "arab"),
         xylose     = str_detect(name, "xyl")   | str_detect(DE, "xyl")    | str_detect(cpd_names, "xyl"),
         fucose     = str_detect(name, "fuc")   | str_detect(DE, "fuc")    | str_detect(cpd_names, "fuc"),
         gulose     = str_detect(name, "gul")   | str_detect(DE, "gul")    | str_detect(cpd_names, "gul"),
         rhamnose   = str_detect(name, "rham")  | str_detect(DE, "rham")   | str_detect(cpd_names, "rham")
         ) %>% replace_na(
           replace = list(
             glucose = FALSE, galactose = FALSE, manose = FALSE, gulose = FALSE,
             arabinose = FALSE, xylose = FALSE, fucose = FALSE, rhamnose = FALSE)
           ) %>%

  mutate(ec = factor(ec, levels = ec %>% unique() %>% str_sort(numeric = TRUE) )) %>%
  mutate(category = gsub(DE, pattern = " .*", replacement = "")) %>%
  mutate(category = case_when(str_detect(DE, "heparin") ~ "heparin",
                              str_detect(DE, "chondroitin") ~ "chondroitin",
                              str_detect(DE, "alginate") ~ "alginate",
                              str_detect(DE, "xylanase") ~ "xylan",
                              str_detect(DE, "arabi") ~ "arabinan",


                              .default = category)
         ) %>%
  mutate(substrate = str_replace(category, pattern = "rhamnogalacturonan", "pectin")) %>%
  filter(str_detect(DE, "endoplasmic reticulum", negate = TRUE)) %>%
  filter(str_detect(DE, "lysozyme", negate = TRUE)) %>%
  filter(str_detect(DE, "Deleted", negate = TRUE)) %>%
  filter(str_detect(DE, "limit ", negate = TRUE)) %>%
  filter(str_detect(DE, "Transferred ", negate = TRUE)) %>%
  filter(str_detect(DE, "blood-group", negate = TRUE)) %>%
  #filter(side == "left") %>%


  filter(grepl(x = compound, "[1-9],[1-9]|[1-9]->[1-9]") |
           grepl(x = compound, "oligosacharide") |
           grepl(x = ec, "^4.2.2")
         ) %>%

#  dplyr::select(DE, glucose, galactose, mannose, arabinose, xylose, fucose, rhamnose) %>%
  #filter(!is.na(glucose)) %>%

  reframe(DE,
          metabolite = name,
          cpd_names,
          category,
          glucose  = any(glucose), galactose = any(galactose),
          mannose  = any(mannose), arabinose = any(arabinose),
          xylose   = any(xylose),  fucose    = any(fucose),
          rhamnose = any(rhamnose), gulose = any(gulose), .by = DE) %>%
  distinct() %>%
  mutate(NONE = !any(glucose, galactose, mannose, arabinose, xylose, fucose, rhamnose, gulose)) %>%
    pivot_longer(!c(DE, metabolite, category, cpd_names)) %>%
  pivot_longer(c(metabolite, cpd_names), names_to = "v", values_to = "metabolite") %>%

  mutate(name = factor(name, levels = c("glucose", "galactose", "rhamnose", "mannose", "gulose", "arabinose","xylose", "fucose", "NONE"))) %>%

 # filter(name %in% c("galactose", "glucose", "mannose", "rhamnose")) %>%

  ggplot() +

  aes(x = metabolite, y = DE, fill = value) +
  geom_tile() +
  facet_grid(category~name, scales = "free", switch = "y", space = "free_y") +
  scale_fill_manual(values = c("TRUE" = "dodgerblue", "FALSE" = "lightgray"), na.value = "lightgray" ) +
  scale_y_discrete(position = "right") +
  theme_bw() +
  theme(strip.text.y.left = element_text(angle = 0, hjust = 0))
